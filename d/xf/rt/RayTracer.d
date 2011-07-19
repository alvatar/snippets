module xf.rt.RayTracer;

private {
	import xf.rt.KDTree;
	import xf.rt.TriBox;
	import xf.rt.Misc;
	import xf.rt.Camera;
	import xf.rt.Photon : Photon, LightingSettings, PMap = PhotonMap;
	import xf.rt.Color;
	import xf.rt.IrradCache : IrradianceCache = IrradCache;
	import xf.rt.Spectrum : xyz2rgb, Spectrum, FullSpectrum;
	import xf.rt.Sampling;
	import xf.rt.Light;
	import xf.rt.TaskMngr;
	
	import xf.rt.Math;
	import xf.omg.geom.AABB;
	import xf.omg.rt.TriAccel;
	import xf.utils.data.BitSet : DynamicBitSet;
	
	import tango.text.convert.Format;
	import tango.time.StopWatch;
	import tango.math.random.Kiss;
	import tango.math.Math : isNaN, isInfinity, log, rndint;
	import tango.core.Array : lbound;
	import tango.stdc.stdlib : alloca;
	import tango.core.sync.ReadWriteMutex;
	
	import tango.util.log.Trace;
}




struct IrradValue {
	vec3d		light = {x: 0, y: 0, z:0};
	/+const float	waveLenMin = 380.f;
	const float waveLenMax = 700.f;
	const float waveLenStep = (waveLenMax - waveLenMin) / light.length;+/
	
	void opMulAssign(float mult) {
		//foreach (ref l; light) l *= mult;
		light *= mult;
	}

	void opAddAssign(ref IrradValue rhs) {
		//foreach (i, ref l; light) l += rhs.light[i];
		light += rhs.light;
	}
	
	float magnitude() {
		/+float sum = 0.f;
		foreach (ref l; light) sum += l * l;
		return sqrt(sum);+/
		return light.x + light.y + light.z;
	}
}

alias IrradianceCache!(IrradValue) IrradCache;



int intIterSqrt(int num) {
	for (int i = 0; i <= num; ++i) {
		if (i * i >= num) return i;
	}
	return 0;
}


struct ScenePrim {
	vec3	normal;
	int	materialId;
}



class RayTracer {
	alias KDTree!(TriAccel)					Scene;
	alias PMap!(Spectrum, Scene)			PhotonMap;
	alias PMap!(FullSpectrum, Scene)	RadiancePhotonMap;

	Scene						scene;
	PhotonMap 				photonMap;
	RadiancePhotonMap	radiancePhotonMap;
	IrradCache				cache;
	Light[]						lights;
	ScenePrim[]				prims;
	FullSpectrum[]			materials;
	
	ReadWriteMutex			cacheMutex;
	
	
	int		maxPhotonCount		= 1_000_000;
	double	lightPowerMult			= 2_800_000;

	float		photonSearchRadius = 200.f;
	int		photonsPerGather = 400;
	bool		doFinalGather = true;
	bool		doDirect = true;
	bool		doIndirect = true;
	bool		storeFirstPhoton = true;
	int		shadowSamples = 4;		// total
	float		irradAccuracy = 20.f;

	// calculated in shootPhotons
	double photonPowerMult = 0.0;

	// for stats only
	uint		numIrradCachePoints = 0;

	
	void		finalGatherSamples(int n) {
		_samplesN = cast(int)(intIterSqrt(cast(int)(n / pi)) * pi);
		_samplesM = (n + _samplesN / 2) / _samplesN;
		_finalGatherSamples = _samplesN * _samplesM;
	}
	
	this() {
		finalGatherSamples = 3000;
	}
	
	private {
		int					_finalGatherSamples;
		int					_samplesN;
		int					_samplesM;
		LightingSettings	settings;
	}


	static void generateHemisphericalSampling(vec3 tangent, vec3 bitangent, vec3 normal, int _samplesN, int _samplesM, vec3[] res) {
		float invM = 1.f / _samplesM;
		float invN = 1.f / _samplesN;
		
		assert (_samplesN * _samplesM == res.length);
		 for (int j = 0; j < _samplesM; ++j) {
			for (int k = 0; k < _samplesN; ++k) {
				float phi = asin(sqrt((j + frand) * invM));
				float theta = 2 * pi * invN * (k + frand);
				
				vec3 v = vec3(sin(phi) * cos(theta), sin(phi) * sin(theta), cos(phi));
				v = v.x * tangent + v.y * bitangent + v.z * normal;
				//Trace.formatln(`v={} normal={} dot={} phi={}`, v.toString, normal.toString, v.dot(normal), phi);
				/+if (dot(v, normal) !>= 0.f) {
					throw new Exception("dot(v, normal) !>= 0.f  in generateHemisphericalSampling");
				}+/
				res[j * _samplesN + k] = v;
			}
		}
		
		/+foreach (ref r; res) {
			r = vrand;
			if (dot(r, normal) < 0.f) {
				r = -r;
			}
		}+/
	}


	vec3 hemisphericalSample(vec3 tangent, vec3 bitangent, int k, float thetaOffset) {
		float invN = 1.f / _samplesN;
		float theta = 2 * pi * invN * k + thetaOffset;
		return cos(theta) * tangent + sin(theta) * bitangent;
	}
	
	
	void prepare() {
		settings.maxPhotonCount = this.maxPhotonCount;
		photonMap = new PhotonMap(settings);
		cache = new IrradCache(scene.box);
		cacheMutex = new ReadWriteMutex;
	}
	
	
	// not threadsafe
	void shootPhotons() {
		if (doIndirect) {
			Trace.formatln(`Shooting photons ...`);
			scope sw = new StopWatch;
			sw.start();
			
			float totalLightIntens = 0.f;
			int[] photonsPerLight = new int[lights.length]; {
				foreach (light; lights) {
					totalLightIntens += light.intensity;
				}
				uint totalPhotons = 0;
				foreach (i, ref pht; photonsPerLight) {
					pht = rndint(lights[i].intensity / totalLightIntens * maxPhotonCount);
					totalPhotons += pht;
				}
				while (totalPhotons > maxPhotonCount) {
					int x = rng.toInt() % lights.length;
					if (photonsPerLight[x] > 0) {
						--photonsPerLight[x];
						--totalPhotons;
					}
				}
			}
			
			double energyShot = 0;

			int prevPercent;
			
			foreach (lightId, photonsToShoot; photonsPerLight) {
				auto light = lights[lightId];
				
				for (int pnum = 0; pnum < photonsToShoot/+settings.maxPhotonCount+/;) {
					{
						int currentPercent = pnum * 1000 / photonsToShoot;
						if (currentPercent != prevPercent) {
							Trace.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
							prevPercent = currentPercent;
						}
					}
					
					Ray r = light.sampleRay;
					auto energy = light.sampleSpectrum(r.dir);

					//r.orig += vec3(0, -0.001f, 0);
					
					energyShot += 1.0;
					
					for (int inum = 0; inum < 50; ++inum) {		// limit to some intersections for sanity's sake
						Hit hit = Hit(10000.f);
						if (scene.intersect(r, hit)) {
							vec3 poi = r.orig + r.dir * hit.dist;
							vec3 pnorm = prims[hit.prim].normal;
							
							if (storeFirstPhoton || inum > 0) {
								photonMap.store(energy, poi, r.dir, pnorm);
								++pnum;
							}
							
							auto materialId = prims[hit.prim].materialId;
							//Trace.formatln("material:{}", materialId);
							float reflectance = energy.filter(materials[materialId]);
							reflectance *= energy.y;
							assert (reflectance <= 1.f);
							energy.normalize();
							
							vec3 reflv = vrand;
							if (dot(reflv, pnorm) < 0.f) {
								reflv = -reflv;
							}
							
							reflectance *= orenNayarBRDF(pnorm, -r.dir, reflv);
							
							if (reflectance >= frand) {
								r.orig = poi + pnorm * 0.001f;
								r.dir = reflv;
							} else break;
						}
					}
				}
			}

			photonPowerMult = totalLightIntens / energyShot;
			Trace.formatln(\n`Energy shot: {}. Done in {} sec`, energyShot, sw.stop());
		}
		
		if (doIndirect) {
			Trace.formatln(`Balancing photons ...`);
			scope sw = new StopWatch;
			sw.start();
			photonMap.balance();
			Trace.formatln(`Done in {} sec`, sw.stop());
		}
	}
	
	
	void precalcRadiancePhotons(TaskMngr taskMngr) {
		if (!doIndirect) {
			return;
		}
		
		//int numPrecalc = maxPhotonCount / 5;//rndint(maxPhotonCount / (log(maxPhotonCount) / 2));
		const float probab = .2f;
		
		scope included = new DynamicBitSet;
		included.resize(maxPhotonCount);
		
		int numPrecalc = 0;
		for (int i = 0; i < maxPhotonCount; ++i) {
			if (probab > frand()) {
				included.set(i);
				++numPrecalc;
			}
		}
		
		auto settings2 = settings;
		settings2.maxPhotonCount = numPrecalc;
		settings2.minPhotonsInEstimate = 1;
		auto pm2 = new RadiancePhotonMap(settings2);
		auto photons = photonMap.getPhotons();

		Trace.formatln("Precomputing {} radiance photons", numPrecalc);

		int prevPercent;
		int numDone = 0;
		
		taskMngr.parallel((int taskId) {
			auto slice = taskMngr.getSlice(taskId, maxPhotonCount);
			
			for (int i = slice._0; i < slice._1; ++i) {
				if (!included.isSet(i)) {
					continue;
				}
				
				int targetIdx;
				synchronized (taskMngr) {
					targetIdx = numDone++;
					int currentPercent = numDone * 1000 / numPrecalc;
					if (currentPercent != prevPercent) {
						Trace.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
						prevPercent = currentPercent;
					}
				}

				vec3 pos = photons[i].pos;
				vec3 norm;
				photonMap.photonDir(norm, &photons[i]);
				
				FullSpectrum spect;
				float maxDist2;
				vec3 normal = vec3.zero;
				photonMap.irradianceEstimate(pos, photons[i].surfNormal, photonSearchRadius, photonsPerGather, true, scene, (Spectrum s, vec3 pdir) {
					spect.add(s, 1.f);
					normal += pdir;
					//Trace.formatln("adding {}", s.toXYZ);
				}, maxDist2);

				if (maxDist2 > 0.0) {
					spect *= ((1.f / 3.14159265) / maxDist2);
				} else {
					spect = FullSpectrum.init;
				}
				
				//Trace.formatln("Spectrum at point: {}  maxDist2: {}", spect.toXYZ, maxDist2);
				
				pm2.store(spect, pos, normal.normalized, photons[i].surfNormal, targetIdx);
			}
		});
		
		pm2.balance();
		radiancePhotonMap = pm2;
		Trace.format("\n");
	}
	
	
	vec3d irradianceEstimate(vec3 sampleDir, vec3 pnorm, vec3 gpoi, vec3 gpnorm, vec3 toEye, Spectrum.FullSpectrum mat) {
		vec3d gxyz = vec3d.zero;
		
		float maxDist2;

		if (radiancePhotonMap) {
			radiancePhotonMap.irradianceEstimate(gpoi, gpnorm, photonSearchRadius, 1, true, scene, (FullSpectrum s, vec3 pdir) {
				FullSpectrum scaled = s;
				scaled *= mat;
				//scaled *= gmat;
				gxyz += vec3d.from(scaled.toXYZ * (orenNayarBRDF(gpnorm, -pdir, -sampleDir) / _finalGatherSamples));
			}, maxDist2);
			
			if (maxDist2 > 0.0) maxDist2 = (1.0 / pi);
		} else {
			photonMap.irradianceEstimate(gpoi, gpnorm, photonSearchRadius, photonsPerGather, true, scene, (Spectrum s, vec3 pdir) {
				//gxyz += vec3d.from(s.toXYZ) * (mat(s) * gmat(s) / _finalGatherSamples);
				float scale = orenNayarBRDF(gpnorm, -pdir, -sampleDir);
				Spectrum scaled = s;
				scale *= scaled.filter(mat);
				//scale *= scaled.filter(gmat);
				scale /= _finalGatherSamples;
				gxyz += vec3d.from(scaled.toXYZ * scale);
				//vec3 localXyz = s.toXYZ * mat
			}, maxDist2);
		}
		
		if (maxDist2 > 0.0) {
			gxyz *= ((1.0 / pi) / maxDist2);
			
			float refl = orenNayarBRDF(pnorm, sampleDir, toEye);
			if (refl > 0) {
				// the lambertian term is inherent in the hemispherical sampling. remove it.
				gxyz *= (refl / lambertianBRDF(pnorm, sampleDir, toEye));
			}
		}
		
		return gxyz;
	}
	
	
	void finalGathering(
		vec3 poi, vec3 pnorm, vec3 toEye, Spectrum.FullSpectrum mat,
		out int successfulSamples, ref vec3d xyz, bool doSecondary, int numGatherSamples, int samplesN, int samplesM,
		void delegate(vec3 basis1, vec3 basis2, vec3[] sampleDirs, float[] sampleDists, vec3d[] sampleRadiances) sampleDg = null
	) {
		vec3 basis1, basis2;
		pnorm.formBasis(&basis1, &basis2);
		
		static char[] allocaArrayFGS(char[] type, char[] name) {
			return type~"[] "~name~" = (cast("~type~"*)alloca(numGatherSamples * "~type~".sizeof))[0..numGatherSamples];";
		}
		
		mixin(allocaArrayFGS("vec3", "sampleDirs"));
		
		// rotation to a new basis may be faster than regeneration
		//generateSunflowerHemisphereSampling(basis1, basis2, pnorm, sampleDirs);
		generateHemisphericalSampling(basis1, basis2, pnorm, samplesN, samplesM, sampleDirs);
		
		mixin(allocaArrayFGS("float", "sampleDists"));
		mixin(allocaArrayFGS("vec3d", "sampleRadiances"));
		
		const float minSampleDist = 1f;
		const float rejectSampleDist = 0.001f;
		
		foreach (sampleI, ref sampleDir; sampleDirs) {
			Ray r = Ray(poi + pnorm * 0.01f + sampleDir * 0.01f, sampleDir);
			Hit ghit = Hit(10000.f);
			
			if (scene.intersect(r, ghit)) {
				sampleDists[sampleI] = ghit.dist;

				vec3 gpoi = r.orig + r.dir * ghit.dist;
				vec3 gpnorm = prims[ghit.prim].normal;
				auto gmat = materials[prims[ghit.prim].materialId];
				
				if (ghit.dist < rejectSampleDist) {
					// bad, bad sample
				}
				else if (ghit.dist < minSampleDist) {
					if (doSecondary) {
						int successful;
						vec3d gxyz = vec3d.zero;
						const int secondaryGatherSamples = 30;
						int N = cast(int)(intIterSqrt(cast(int)(secondaryGatherSamples / pi)) * pi);
						int M = (secondaryGatherSamples + N / 2) / N;
						finalGathering(gpoi, gpnorm, -sampleDir, mat*gmat, successful, gxyz, false, secondaryGatherSamples, N, M);
						if (successful < secondaryGatherSamples / 2) {
							// bad, bad sample!
						} else {
							++successfulSamples;
							sampleRadiances[sampleI] = gxyz;
							xyz += gxyz / secondaryGatherSamples;
						}
					}
				} 
				else {
					++successfulSamples;
					auto gxyz = irradianceEstimate(sampleDir, pnorm, gpoi, gpnorm, toEye, mat*gmat);
					sampleRadiances[sampleI] = gxyz * numGatherSamples;
					xyz += gxyz;
				}
			} else {
				++successfulSamples;
				sampleRadiances[sampleI] = vec3d.zero;
				sampleDists[sampleI] = float.init;
			}
		}
		
		if (successfulSamples > 0) {
			xyz *= cast(float)sampleDirs.length / successfulSamples;
		}

		{
			const float minCacheDist = .5f;
			//const float minCacheDist = 0f;
			float missDist = scene.box.size.length * 2f;
			foreach (ref sd; sampleDists) {
				if (sd < minCacheDist/+ && sd > rejectSampleDist+/) {
					// it would have an incredibly low weight in the cache and an errorneous value
					//return vec3d(.001, 0, 0);
					return xyz;
					//sd = minCacheDist;
				} else if (!(sd >= minSampleDist)) {
					sd = missDist;
				}
			}
		}
		
		if (sampleDg) {
			sampleDg(basis1, basis2, sampleDirs, sampleDists, sampleRadiances);
		}
		
		return xyz;
	}
	
	
	vec3d computeIlluminance(/+Hit hit, +/vec3 poi, vec3 pnorm, vec3 toEye, Spectrum.FullSpectrum mat)
	in {
		/+assert (hit.dist <>= 0);
		assert (hit.u <>= 0);
		assert (hit.v <>= 0);+/
		assert (poi.ok);
		assert (pnorm.ok);
	} body {
		vec3d xyz = vec3d.zero;
		//double power = 0.f;
		
		int successfulSamples = 0;
		
		// indirect lighting
		if (doIndirect) {
			if (doFinalGather) {
				IrradValue val;
				bool inCache;
				synchronized (cacheMutex.reader()) inCache = cache.query(poi, pnorm, irradAccuracy, &val);
				if (inCache) {
					//xyz += val.light;
					//return val.light;
					return vec3d.init;
				} else {
					//irradComputed[x][y] = true;
					synchronized(this) ++numIrradCachePoints;
					
					finalGathering(poi, pnorm, toEye, mat, successfulSamples, xyz, true, _finalGatherSamples, _samplesN, _samplesM,
					(vec3 basis1, vec3 basis2, vec3[] sampleDirs, float[] sampleDists, vec3d[] sampleRadiances) {
						//precalc[x][y] = xyz;
						
						// bad value, don't add it to the irradiance cache
						/+if (_finalGatherSamples * 2 > successfulSamples * 3) {
							//continue;
							return xyz;
						}+/
						
						float R0 = 0.f; {
							foreach (sd; sampleDists) {
								R0 += 1.f / sd;
							}
							
							R0 = sampleDists.length / R0;
						}
						
						float rjk(int j, int k) {
							j += _samplesM;
							j %= _samplesM;
							k += _samplesN;
							k %= _samplesN;
							return sampleDists[j * _samplesN + k];
						}

						vec3d ljk(int j, int k) {
							j += _samplesM;
							j %= _samplesM;
							k += _samplesN;
							k %= _samplesN;
							return sampleRadiances[j * _samplesN + k];
						}
						
						vec3 translGradients[3] = vec3.zero;
						
						gradientComp: for (int c = 0; c < 3; ++c) {
							for (int k = 0; k < _samplesN; ++k) {
								vec3 sdir = vec3.zero;
								
								float jsum = 0.f;
								for (int j = 1; j < _samplesM; ++j) {
									sdir += sampleDirs[j * _samplesN + k];
									
									float Ljk = ljk(j, k).cell[c];
									float Lj1k = ljk(j-1, k).cell[c];
									
									if (Ljk !<>= 0 || Lj1k !<>= 0) {
										// we don't want gradients for dubious points
										translGradients[] = vec3.zero;
										break gradientComp;
									}
									
									float sin2t = cast(float)j / _samplesM;
									float sint = sqrt(sin2t);
									float cos2t = 1.f - sin2t;
									
									float minR = min(rjk(j, k), rjk(j-1, k));
									float mult = sint * cos2t / minR * (Ljk - Lj1k);

									if (!isInfinity(mult) && !isNaN(mult)) {
										jsum += mult;
									}
								}
								
								sdir -= pnorm * dot(sdir, pnorm);
								vec3 uk;
								
								if (sdir.sqLength > 0.001f) {
									uk = sdir.normalized;
								} else {
									uk = hemisphericalSample(basis1, basis2, k, 0.f);
								}
								
								vec3 lpart = uk * (jsum * 2 * pi / _samplesN);
								
								jsum = 0.f;
								for (int j = 0; j < _samplesM; ++j) {
									float Ljk = ljk(j, k).cell[c];
									float Ljk1 = ljk(j, k-1).cell[c];

									if (Ljk !<>= 0 || Ljk1 !<>= 0) {
										// we don't want gradients for dubious points
										translGradients[] = vec3.zero;
										break gradientComp;
									}
									
									float sint1 = sqrt(cast(float)(j+1) / _samplesM);
									float sint2 = sqrt(cast(float)j / _samplesM);
									float minR = min(rjk(j, k), rjk(j, k-1));
									float mult = (sint1 - sint2) / minR * (Ljk - Ljk1);
									if (!isInfinity(mult) && !isNaN(mult)) {
										jsum += mult;
									}
								}
								
								assert (!isInfinity(jsum));
								assert (!isNaN(jsum));
								assert (lpart.ok);

								vec3 vk = hemisphericalSample(basis1, basis2, k, pi / 2);
								assert (vk.ok);

								translGradients[c] += lpart;
								translGradients[c] += vk * jsum;
							}
						}
						
						//Trace.formatln(`transl gradients: {} {} {}`, translGradients[0].toString, translGradients[1].toString, translGradients[2].toString);
						synchronized (cacheMutex.writer()) cache.store(poi, pnorm, R0, IrradValue(xyz), translGradients);
					});
				}
			}
		}
		
		return xyz;
	}


	vec3d lightingAtPoint(vec3 poi, vec3 pnorm, vec3 toEye, int materialId) {
		vec3d xyz = vec3d.zero;
		auto mat = materials[materialId];
		
			// indirect lighting
			if (doIndirect) {
				bool cacheFailed = false;
				
				if (doFinalGather) {
					/+if (irradComputed[x][y]) {
						xyz = vec3d(1e5, 1e5, 1e5);
					} else +/{
						IrradValue val;
						bool inCache;
						synchronized (cacheMutex.reader()) inCache = cache.query(poi, pnorm, irradAccuracy, &val);
						if (inCache) {
							xyz = val.light;
						}/+ else {
							xyz = precalc[x][y];
						}+/
						
						if (xyz == vec3d.zero || xyz.x !<>= 0 || xyz.y !<>= 0 || xyz.z !<>= 0) {
							xyz = computeIlluminance(/+hit, +/poi, pnorm, toEye, mat);
							if (xyz.x !<>= 0) {
								cacheFailed = true;
								//xyz = vec3d.zero;
							}
						}
					}
				}
				
				/+if (!doFinalGather || cacheFailed) {
					xyz = vec3d.zero;
					vec3d gxyz = vec3d.zero;
					
					float maxDist2;

					if (radiancePhotonMap) {
						radiancePhotonMap.irradianceEstimate(poi, pnorm, photonSearchRadius, 1, false, scene, (FullSpectrum s, vec3 pdir) {
							FullSpectrum scaled = s;
							scaled *= mat;
							gxyz += vec3d.from(scaled.toXYZ * orenNayarBRDF(pnorm, -pdir, toEye));
						}, maxDist2);

						if (maxDist2 > 0) maxDist2 = (1.0 / pi);
					} else {
						photonMap.irradianceEstimate(poi, pnorm, photonSearchRadius, photonsPerGather, true, scene, (Spectrum s, vec3 pdir) {
							float p = s.filter(mat);
							gxyz += vec3d.from(s.toXYZ * orenNayarBRDF(pnorm, -pdir, toEye)) * p;
						}, maxDist2);
					}

					if (maxDist2 > 0.0) {
						auto incr = gxyz / (pi * maxDist2);
						if (incr.ok) {
							xyz += incr;
						}
					}
				}+/
			}

			xyz *= photonPowerMult * lightPowerMult;


			// direct lighting
			if (doDirect) {
				foreach (light; lights) {
					light.samplePointsStratified(shadowSamples, (vec3 lp, vec3 ltnorm) {
					/+for (int u = 0; u < shadowSamplesU; ++u) {
						for (int v = 0; v < shadowSamplesV; ++v) {
							vec3 lp = ltstrat(vec2i(u, v), vec2i(shadowSamplesU, shadowSamplesV));+/
							Hit hit2 = Hit(10000.f);
							
							// offset the origin to avoid numerical instability
							auto lpOff = lp + ltnorm * 0.001f;
							Ray r2 = Ray(lpOff, poi - lpOff);
							
							if (dot(r2.dir, ltnorm) > 0.0001f && scene.intersect(r2, hit2) && hit2.dist >= 0.999f) {
								vec3 rdnorm = r2.dir.normalized;
								double p = orenNayarBRDF(pnorm, -rdnorm, toEye) * dot(ltnorm, rdnorm) / r2.dir.sqLength;
								p *= lightPowerMult * light.intensity;
								if (p > 0) {
									auto spect = light.getSpectrum(rdnorm);
									spect *= mat;
									vec3d contrib = vec3d.from(spect.toXYZ);
									
									// divide by the projected solid angle
									p /= shadowSamples * pi * 2;
									xyz += p * contrib;
								}
							}
						//}
					});
				}
			}
			
		return xyz;
	}
	
	
	static float lambertianBRDF(vec3 norm, vec3 toLight, vec3 toEye) {
		return max(0, dot(toLight, norm));
	}
	//alias lambertianBRDF orenNayarBRDF;


	static float orenNayarBRDF(vec3 norm, vec3 toLight, vec3 toEye) {
		float lambertianTerm = dot(toLight, norm);
		
		if (lambertianTerm <= 0) {
			return 0;
		} else {
			const float roughness = .5;
			const float A = 1 - .5 * (roughness * roughness) / (roughness * roughness + 0.33);
			const float B = .45 * (roughness * roughness) / (roughness * roughness + 0.09);
			
			vec3 toLightProj = toLight - norm * dot(norm, toLight);
			vec3 toEyeProj = toEye - norm * dot(norm, toEye);
			
			float aniso = max(0, dot(toLightProj.normalized, toEyeProj.normalized));		
			
			float thetaI = acos(lambertianTerm);
			float thetaR = acos(dot(toEye, norm));
			
			float alpha = max(thetaI, thetaR);
			float beta = min(thetaI, thetaR);
			
			return lambertianTerm * (A + B * aniso * sin(alpha) * tan(beta));
		}
	}


	vec3d singleSample(Ray r) {
		vec3d xyz = vec3d.zero;

		Hit hit = Hit(10000.f);
		if (scene.intersect(r, hit)) {
			vec3 poi = r.orig + r.dir * hit.dist;
			vec3 pnorm = prims[hit.prim].normal;
			xyz = lightingAtPoint(poi, pnorm, -r.dir.normalized, prims[hit.prim].materialId);
		}
			
		return xyz;
	}
}
