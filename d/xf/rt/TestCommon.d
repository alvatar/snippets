module xf.rt.TestCommon;

//version = ShowIrradCache;

private {
	import xf.rt.Misc;
	import xf.rt.Camera;
	import xf.rt.Color;
	import xf.rt.Spectrum : xyz2rgb, Spectrum, FullSpectrum;
	import xf.rt.Sampling;
	import xf.rt.RayTracer;
	import xf.rt.TaskMngr;
	import xf.rt.Math;
	import xf.rt.TriBox;
	
	import xf.omg.geom.AABB;
	import tango.time.StopWatch;
	
	import tango.util.log.Trace;
	import tango.stdc.stdio : fopen, fclose;
	import xf.rt.RGBE;
}



struct SrcPrim {
	vec3[3]	v;
	vec3		n;
	uint		idx;
	
	
	void calcRange(ref Plane p, float* min, float* max) {
		*min = *max = p.dist(v[0]);
		
		foreach (v; this.v[1..$]) {
			float d = p.dist(v);
			if (d > *max) *max = d;
			if (d < *min) *min = d;
		}
	}
	
	
	vec3 calcNormal() {
		return n = cross(v[1] - v[0], v[2] - v[0]).normalized;
	}
	
	
	bool intersect(AABB box) {
		vec3 boxcenter = box.center;
		vec3 boxhalfsize = box.halfSize;
		return triBoxOverlap(*cast(float[3]*)&boxcenter, *cast(float[3]*)&boxhalfsize, *cast(float[3][3]*)&this.v);
	}
	
	
	AABB calcAABB() {
		AABB res;
		res.expand(v[0]);
		res.expand(v[1]);
		res.expand(v[2]);
		return res;
	}
}



void generateImage(int width, int height, int aaSamples, TaskMngr taskMngr, RayTracer tracer, Camera cam) {
	tracer.prepare();
	
	vec3ub[][]	pix = new vec3ub[][](width, height);
	
	rng.seed(12345);
	
	tracer.shootPhotons();
	tracer.precalcRadiancePhotons(taskMngr);


	Ray makeRay(float x, float y) {
		return cam.ray(cast(float)(x - width / 2) / width, cast(float)(height / 2 - y) / height);
	}
	
	
	version (ShowIrradCache) {
	} else {
		if (tracer.doIndirect) {
			Trace.formatln(`Computing indirect illumination ...`);		
			scope sw = new StopWatch;
			sw.start();

			uint totalPixels = width * height;
			uint currentPixel = 0;
			int prevPercent = 0;
			Trace.format(`0.0 %`).flush;

			taskMngr.parallel((int taskId) {
				auto yslice = taskMngr.getSlice(taskId, height);
				for (int y = yslice._0; y < yslice._1; ++y) {
					xIter: for (int x = 0; x < width; ++x) {
						synchronized(taskMngr) {
							++currentPixel;
							int currentPercent = currentPixel * 1000 / totalPixels;
							if (currentPercent != prevPercent) {
								Trace.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
								prevPercent = currentPercent;
							}
						}
						
						vec3ub col = vec3ub.zero;

						Ray r = makeRay(x, y);
						Hit hit = Hit(10000.f);
						if (tracer.scene.intersect(r, hit)) {
							vec3 poi = r.orig + r.dir * hit.dist;
							vec3 pnorm = tracer.prims[hit.prim].normal;
							auto mat = tracer.materials[tracer. prims[hit.prim].materialId];
							tracer.computeIlluminance(/+hit, +/poi, pnorm, -r.dir, mat);
						}
					}
				}
			});
			
			Trace.formatln(\n`Done in {} sec ; Irradiance cache size: {}`, sw.stop(), tracer.numIrradCachePoints);
		}
	}

	{
		Trace.formatln(`Computing direct illumination ...`);
		scope sw = new StopWatch;
		sw.start();

		uint totalPixels = width * height;
		uint currentPixel = 0;
		int prevPercent = 0;
		Trace.format(`0.0 %`).flush;
		
		auto xyzData = new vec3d[totalPixels];
		auto rgbData = new vec3d[totalPixels];

		taskMngr.parallel((int taskId) {
			auto yslice = taskMngr.getSlice(taskId, height);
			for (int y = yslice._0; y < yslice._1; ++y) {
				xIter: for (int x = 0; x < width; ++x) {
					synchronized(taskMngr) {
						++currentPixel;
						int currentPercent = currentPixel * 1000 / totalPixels;
						if (currentPercent != prevPercent) {
							Trace.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
							prevPercent = currentPercent;
						}
					}

					//vec3ub col = vec3ub.zero;


					//single sample lol wut haha


					vec3d xyz = vec3d.zero;
					
					for (int u = 0; u < aaSamples; ++u) {
						for (int v = 0; v < aaSamples; ++v) {
							float rx = x -.5f + cast(float)(u+1) / (aaSamples+1);
							float ry = y -.5f + cast(float)(v+1) / (aaSamples+1);
							
							Ray r = makeRay(rx, ry);
							version (ShowIrradCache) {
								Hit hit = Hit(10000.f);
								if (tracer.scene.intersect(r, hit)) {
									vec3 poi = r.orig + r.dir * hit.dist;
									vec3 pnorm = tracer.prims[hit.prim].normal;
									auto mat = tracer.materials[tracer. prims[hit.prim].materialId];
									xyz += tracer.computeIlluminance(/+hit, +/poi, pnorm, -r.dir, mat);
								}
							} else {
								xyz += tracer.singleSample(r);
							}
						}
					}
					
					xyz *= 1.0 / (aaSamples * aaSamples);
					xyzData[y*width+x] = xyz;
				}
			}
		});
		
		foreach (i, xyz; xyzData) {
			vec3d rgb = xyz2srgb(xyz);
			rgbData[i] = rgb;
		}
		
		try {
			auto data = new float[width * height * 3];
			int off = 0;
			foreach (c; rgbData) {
				data[off++] = c.x;
				data[off++] = c.y;
				data[off++] = c.z;
			}
			
			auto file = fopen("test.hdr", "wb");
			scope(exit) {
				fclose(file);
				delete data;
			}
			RGBE_WriteHeader(file, width, height, null);
			RGBE_WritePixels(file, data.ptr, width * height);
		} catch (Object o) {
			Trace.formatln("Exception while writing the RGBE file: {}", o);
		}

		writeTGA((int x, int y) {
			auto rgb = rgbData[y*width+x];
			
			ubyte f2ub(double f) {
				if (f >= 1.0) return 255;
				if (f <= 0.0) return 0;
				return cast(ubyte)(255 * f);
			}
			
			rgb = gammaCorrectSRGB(rgb);
			return vec3ub(f2ub(rgb.r), f2ub(rgb.g), f2ub(rgb.b));
			//return col;
		}, vec2i.zero, vec2i(width, height), "test.tga");

		Trace.formatln(\n`Done in {} sec ; Irradiance cache size: {}`, sw.stop(), tracer.numIrradCachePoints);
	}
}