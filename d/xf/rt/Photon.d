module xf.rt.Photon;

private {
	import xf.rt.Math;
	import xf.rt.Color;
	import xf.omg.geom.AABB;
	
	import tango.stdc.stdlib : free, malloc, realloc, alloca;
	import tango.math.Math;
}



struct LightingSettings {
	uint		maxPhotonCount;//				= 1_000_000;
	float		maxPhotonSearchRadius	= 15.f;
	uint		maxPhotonsInEstimate		= 50;
	bool		useLeakPrevention				= false;
	bool		discBasedEstimate				= true;
	float		capDistanceMult					= 0.075f;
	uint		minPhotonsInEstimate		= 12;
	float		simpleCheckMaxRadius		= 0.5;
}


struct Photon(Spectrum) {
	vec3			pos;
	Spectrum	energy;
	short			plane;
	ubyte		theta, phi;
	vec3			surfNormal;
}


struct NearestPhotons(Spectrum) {
	alias .Photon!(Spectrum) Photon;

	int			max;
	int			found;
	bool			gotHeap;
	vec3			pos;
	vec3			normal;
	vec3			pivot;
	float			maxPivotLook;
	float			D;		// from the plane equation
	float*		dist2;
	Photon**	index;
}


class PhotonMap(Spectrum, SceneAccel) {
	alias .Photon!(Spectrum) Photon;
	
	private {
		Photon*	photons;
		int			storedPhotons;
		int			halfStoredPhotons;
		int			maxPhotons;
		float[256]	cosTheta;
		float[256]	sinTheta;
		float[256]	cosPhi;
		float[256]	sinPhi;
		
		AABB		bbox;
		
		LightingSettings settings;
	}
	
	
	this(LightingSettings settings) {
		this.settings = settings;
		storedPhotons = 0;
		maxPhotons = settings.maxPhotonCount;
		
		photons = cast(Photon*)malloc(Photon.sizeof * (maxPhotons + 1));
		if (photons is null) {
			throw new Exception("Cannot allocate memory for the photon map");
		}
		
		for (uint i = 0; i < 256; ++i) {
			double angle = cast(double)i * (1.0 / 256.0) * 3.14159265;
			cosTheta[i]	= cos(angle);
			sinTheta[i]		= sin(angle);
			cosPhi[i]		= cos(2.0 * angle);
			sinPhi[i]		= sin(2.0 * angle);
		}
	}
	
	
	// wtf, somehow this crashes :/
	/+~this() {
		free(photons);
		photons = null;
	}+/
	
	
	final void photonDir(inout vec3 dir, Photon* p) {
		dir.x = sinTheta[p.theta] * cosPhi[p.phi];
		dir.y = sinTheta[p.theta] * sinPhi[p.phi];
		dir.z = cosTheta[p.theta];
	}
	
	
	final Photon[] getPhotons() {
		return photons[0..storedPhotons];
	}
	
	
	/+vec3 findPhotonCheckPivot(in vec3 pos, in vec3 normal, float maxDist, SceneAccel world) {
		float staringDistance = maxDist * 0.5f;
		float t = staringDistance;
		
		vec3 primNormal;
		vec4 primColor;
		vec2 uv;
		PrimitiveHit primHit;
		
		pos += normal * 0.01f;
		
		if (!world.intersect(pos, normal, t, primNormal, primColor, uv, primHit)) {
			t = staringDistance;
		} else {
			if (t <= 0) return vec3.zero;  // invalid lumel
			if (t > staringDistance) t = staringDistance;
		}
		
		t *= 0.95f;
		assert (t < staringDistance);
		
		return pos + normal * t;
	}+/
	
	
	// returns the color in CIE XYZ
	final void irradianceEstimate(in vec3 pos, in vec3 normal, float maxDist, uint nPhotons, bool opaque, SceneAccel world, void delegate(Spectrum, vec3 pdir) gatherFunc, out float maxDist2) {
		/+power = 0.0;
		xyz = vec3d.zero;+/
		
		NearestPhotons!(Spectrum) np;
		np.dist2 = cast(float*)alloca(float.sizeof * (nPhotons + 1));
		np.index = cast(Photon**)alloca((Photon*).sizeof * (nPhotons + 1));
		
		np.pos = pos;
		np.normal = normal;
		np.D = -dot(pos, normal);
		np.max = nPhotons;
		np.found = 0;
		np.gotHeap = false;
		np.dist2[0] = maxDist * maxDist;
		
		/+if (settings.useLeakPrevention) {
			np.pivot = findPhotonCheckPivot(pos, normal, maxDist, world);
			np.maxPivotLook = maxDist * 3.f;
		}+/
		
		if (1 == nPhotons) {
			locatePhoton(np, 1, world);
		} else {
			locatePhotons(np, 1, world);
		}
		
		if (np.found < settings.minPhotonsInEstimate) {
			maxDist2 = 0.f;
			return;
		}
		
		vec3 pdir;
		
		for (uint i = 1; i <= np.found; ++i) {
			Photon* p = np.index[i];
			
			photonDir(pdir, p);
			
			if (!opaque || dot(pdir, normal) < 0.f) {
				gatherFunc(p.energy, pdir);
			}
		}
		
		maxDist2 = np.dist2[0];
		/+
		if (power > 0.0) {
			xyz /= (xyz.x + xyz.y + xyz.z);
			power *= (1.0 / 3.14159265) / np.dist2[0];
		}+/
	}
		
		
	final void locatePhotons(inout NearestPhotons!(Spectrum) np, uint index, SceneAccel world) {
		Photon* p = &photons[index];
		
		if (index < halfStoredPhotons) {
			float dist1 = np.pos.cell[p.plane] - p.pos.cell[p.plane];
			
			if (dist1 > 0.f) {
				locatePhotons(np, 2 * index + 1, world);
				if (dist1*dist1 < np.dist2[0]) {
					locatePhotons(np, 2 * index, world);
				}
			} else {
				locatePhotons(np, 2 * index, world);
				if (dist1*dist1 < np.dist2[0]) {
					locatePhotons(np, 2 * index + 1, world);
				}
			}
		}
		
		bool	cappedSphere = settings.discBasedEstimate;
		float	capDistanceMult = settings.capDistanceMult;
		
		float dist2 = void;
		{
			float xd = p.pos.x - np.pos.x;
			float yd = p.pos.y - np.pos.y;
			float zd = p.pos.z - np.pos.z;
			dist2 = xd * xd + yd * yd + zd * zd;
		}
		
		float xnp = void, ynp = void, znp = void;
		if (dist2 < np.dist2[0] && (!cappedSphere || abs(dot(np.normal, p.pos) + np.D) <= np.dist2[0] * capDistanceMult)) {
			// found a photon
			
			if (np.found < np.max) {
				++np.found;
				np.dist2[np.found] = dist2;
				np.index[np.found] = p;
			} else {
				uint j, parent;
				
				if (!np.gotHeap) {
					float dst2;
					Photon* phot;
					uint halfFound = np.found >> 1;
					
					for (uint k = halfFound; k >= 1; --k) {
						parent = k;
						phot = np.index[k];
						dst2 = np.dist2[k];
						
						while (parent <= halfFound) {
							j = parent + parent;
							
							if (j < np.found && np.dist2[j] < np.dist2[j+1]) {
								++j;
							}
							
							if (dst2 >= np.dist2[j]) {
								break;
							}
							
							np.dist2[parent] = np.dist2[j];
							np.index[parent] = np.index[j];
							parent = j;
						}
							
						np.dist2[parent] = dst2;
						np.index[parent] = phot;
					}
					
					np.gotHeap = true;
				}
				
				parent = 1;
				j = 2;
				
				while (j <= np.found) {
					if (j < np.found && np.dist2[j] < np.dist2[j+1]) {
						++j;
					}
					
					if (dist2 > np.dist2[j]) {
						break;
					}
					
					np.dist2[parent] = np.dist2[j];
					np.index[parent] = np.index[j];
					parent = j;
					j += j;
				}
				
				np.index[parent] = p;
				np.dist2[parent] = dist2;
				
				np.dist2[0] = np.dist2[1];
			}
		}
	}
	

	final void locatePhoton(inout NearestPhotons!(Spectrum) np, uint index, SceneAccel world) {
		Photon* p = &photons[index];
		
		if (index < halfStoredPhotons) {
			float dist1 = np.pos.cell[p.plane] - p.pos.cell[p.plane];
			
			if (dist1 > 0.f) {
				locatePhoton(np, 2 * index + 1, world);
				if (dist1*dist1 < np.dist2[0]) {
					locatePhoton(np, 2 * index, world);
				}
			} else {
				locatePhoton(np, 2 * index, world);
				if (dist1*dist1 < np.dist2[0]) {
					locatePhoton(np, 2 * index + 1, world);
				}
			}
		}
		
		bool	cappedSphere = settings.discBasedEstimate;
		float	capDistanceMult = settings.capDistanceMult;
		
		float dist2 = void;
		{
			float xd = p.pos.x - np.pos.x;
			float yd = p.pos.y - np.pos.y;
			float zd = p.pos.z - np.pos.z;
			dist2 = xd * xd + yd * yd + zd * zd;
		}
		
		float xnp = void, ynp = void, znp = void;
		if (dist2 < np.dist2[0] && (!cappedSphere || abs(dot(np.normal, p.pos) + np.D) <= np.dist2[0] * capDistanceMult)) {
			// found a photon
			
			if (np.found < 1) {
				++np.found;
			}
			
			np.dist2[0] = np.dist2[1] = dist2;
			np.index[1] = p;
		}
	}

	
	final void store(Spectrum energy, vec3 pos, vec3 dir, vec3 surfNormal, int idx = -1) {
		if (storedPhotons >= maxPhotons && -1 == idx) {
			return;
		}
		
		Photon* node;
		if (-1 == idx) {
			++storedPhotons;		
			node = &photons[storedPhotons];
		} else {
			storedPhotons = maxPhotons;
			node = &photons[idx];
		}
		
		bbox.expand(node.pos);
		node.energy = energy;
		node.pos = pos;
		node.surfNormal = surfNormal;
		
		int theta = cast(int)(acos(dir.z) * (256.0 / 3.14159265));
		if (theta > 255) {
			node.theta = 255;
		} else {
			node.theta = cast(ubyte)theta;
		}
		
		int phi = cast(int)(atan2(dir.y, dir.x) * (256.0 / (2.0 * 3.14159265)));
		if (phi > 255) {
			node.phi = 255;
		} else if (phi < 0) {
			node.phi = cast(ubyte)(phi + 256);
		} else {
			node.phi = cast(ubyte)phi;
		}
	}
	
	
	/+void scalePhotonPower(float scale) {
		for (int i = 1; i < storedPhotons; ++i) {
			photons[i].power *= scale;
		}
		//prevScale = storedPhotons;
	}+/
	
	
	final void balance() {
		if (storedPhotons > 1) {
			Photon** pa1 = cast(Photon**)malloc((Photon*).sizeof * (storedPhotons + 1));
			Photon** pa2 = cast(Photon**)malloc((Photon*).sizeof * (storedPhotons + 1));
			
			for (uint i = 0; i <= storedPhotons; ++i) {
				pa2[i] = &photons[i];
			}
			
			balanceSegment(pa1, pa2, 1, 1, storedPhotons);
			free(pa2);
			
			int d, j = 1, foo = 1;
			Photon fooPhoton = photons[j];
			
			for (uint i = 1; i <= storedPhotons; ++i) {
				d = pa1[j] - photons;
				pa1[j] = null;
				
				if (d != foo) {
					photons[j] = photons[d];
				} else {
					photons[j] = fooPhoton;
					
					if (i < storedPhotons) {
						for (; foo <= storedPhotons; ++foo) {
							if (pa1[foo] != null) {
								break;
							}
						}
							
						fooPhoton = photons[foo];
						j = foo;
					}
						
					continue;
				}
				
				j = d;
			}
			
			free(pa1);
		}
		
		halfStoredPhotons = storedPhotons / 2 - 1;
	}
	
	
	final void swap(Photon** ph, int a, int b) {
		Photon *ph2 = ph[a];
		ph[a] = ph[b];
		ph[b] = ph2;
	}
	
	
	final void medianSplit(Photon** p, int start, int end, int median, int axis) {
		int left = start;
		int right = end;
		
		while (right > left) {
			float v = p[right].pos.cell[axis];
			int i = left - 1;
			int j = right;
			
			while (true) {
				while (p[++i].pos.cell[axis] < v) {};
				while (p[--j].pos.cell[axis] > v && j > left) {};
				
				if (i >= j) {
					break;
				}
				
				swap(p, i, j);
			}
			
			swap(p, i, right);
			if (i >= median) {
				right = i - 1;
			}
			
			if (i <= median) {
				left = i + 1;
			}
		}
	}
	
	
	final void balanceSegment(Photon** pbal, Photon** porg, int index, int start, int end) {
		int median = 1;
		while ((4 * median) <= (end - start + 1)) {
			median += median;
		}
		
		if ((3 * median) <= (end - start + 1)) {
			median += median;
			median += start - 1;
		} else {
			median = end - median + 1;
		}
		
		int axis = 2;
		
		if (	(bbox.max.x - bbox.min.x) > (bbox.max.y - bbox.min.y) &&
				(bbox.max.x - bbox.min.x) > (bbox.max.z - bbox.min.z)) {
			axis = 0;
		} else if ((bbox.max.y - bbox.min.y) > (bbox.max.z - bbox.min.z)) {
			axis = 1;
		}
		
		medianSplit(porg, start, end, median, axis);
		
		pbal[index] = porg[median];
		pbal[index].plane = axis;
		
		if (median > start) {
			if (start < median - 1) {
				float tmp = bbox.max.cell[axis];
				bbox.max.cell[axis] = pbal[index].pos.cell[axis];
				balanceSegment(pbal, porg, 2 * index, start, median - 1);
				bbox.max.cell[axis] = tmp;
			} else {
				pbal[2 * index] = porg[start];
			}
		}


		if (median < end) {
			if (median + 1 < end) {
				float tmp = bbox.min.cell[axis];
				bbox.min.cell[axis] = pbal[index].pos.cell[axis];
				balanceSegment(pbal, porg, 2 * index + 1, median + 1, end);
				bbox.min.cell[axis] = tmp;
			} else {
				pbal[2 * index + 1] = porg[end];
			}
		}
	}
}

