module rt.test;

private {
	import rt.KDTree;
	import rt.TriAccel;
	import rt.TriBox;
	import rt.Misc;
	import rt.Camera;
	import rt.Photon : Photon, LightingSettings, PMap = PhotonMap;
	import rt.Color;
	
	import maths.Vec;
	import maths.Quat;
	import maths.Plane;
	import maths.AABB;
	import maths.Misc;
	import maths.Stats;

	import tango.io.Stdout;
	import tango.util.time.StopWatch;
	import tango.math.Random;
	import tango.core.Array : lbound;
	
	import rt.BunnyData;
	import Cornell = rt.CornellBoxData;
}



alias KDTree!(TriAccel)	Scene;


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
	
	
	void calcNormal() {
		n = vec3.cross(v[1] - v[0], v[2] - v[0]).normalized;
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


Random rand;
float frand() {
	return (1.0 / uint.max) * rand.next;
}
vec3 vrand() {
	float phi	= frand() * 2 * pi;
	float u		= (frand() - .5f) * 2.f;
	float u2	= u*u;
	float rt		= sqrt(1.f - u2);
	
	vec3 res = vec3(rt * cos(phi), rt * sin(phi), u);
	return res;
}
vec3 ltrand() {		// randomly sample the light
	alias Cornell.light lt;
	vec2 ls = vec2(frand, frand);
	return (lt.v[0] + ((lt.v[1] - lt.v[0]) * ls.x)) * ls.y + (lt.v[3] + ((lt.v[2] - lt.v[3]) * ls.x)) * (1.f - ls.y);
}	


SrcPrim[] prims;

const int width = 512;
const int height = 512;
const double lightPowerMult = 2_000_000;


void main() {
	Scene sc	= new Scene;
	rand			= new Random;
	
	foreach (i, surf; Cornell.surfaces) {
		{
			SrcPrim p;
			p.idx = i;
			p.v[] = surf.v[0..3];
			p.calcNormal;
			prims ~= p;
		}

		{
			SrcPrim p;
			p.idx = i;
			p.v[0] = surf.v[0];
			p.v[1] = surf.v[2];
			p.v[2] = surf.v[3];
			p.calcNormal;
			prims ~= p;
		}
	}
	
	auto builder = sc.build(prims[], (SrcPrim s){
		TriAccel res = TriAccel(s.v[0], s.v[1], s.v[2]);
		res.data = s.idx;
		return res;
	});
	
	{
		Stdout.formatln(`Building a kD-Tree...`);
		scope sw = new StopWatch;
		sw.start();
		builder.build();	
		Stdout.formatln(`Built in {} sec`, sw.stop());
	}
	
	Camera cam = new Camera(Cornell.Camera.position, Cornell.Camera.upDirection, Cornell.Camera.direction);
	cam.aspect = cast(float)width / height;
	cam.focalLenMul = 1.4f;
	
	vec3ub[][] pix = new vec3ub[][](width, height);
	
	{
		Stdout.formatln(`Tracing ...`);		
		scope sw = new StopWatch;
		sw.start();

		uint totalPixels = width * height;
		uint currentPixel = 0;
		int prevPercent = 0;
		Stdout.format(`0.0 %`).flush;
		
		writeTGA((int x, int y) {
			{
				++currentPixel;
				int currentPercent = currentPixel * 1000 / totalPixels;
				if (currentPercent != prevPercent) {
					Stdout.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
					prevPercent = currentPercent;
				}
			}
			
			vec3ub col = vec3ub.zero;
			Ray r = cam.ray(cast(float)(x - width / 2) / width, cast(float)(height / 2 - y) / height);
			
			Hit hit = Hit(10000.f);
			if (sc.intersect(r, hit)) {
				vec3 poi = r.orig + r.dir * hit.dist;
				
				// ----
				vec3 pnorm = prims[hit.prim * 2].n;
				auto mat = Cornell.surfaces[hit.prim].material();
				vec3d xyz = vec3d.zero;
				
				{
					int samples = 1500;
					for (int i = 0; i < samples; ++i) {
						xyz += trace(poi, pnorm, (float w){ return mat(w); }, sc);
						xyz += getRadiance(poi, pnorm, (float w){ return mat(w); }, sc);
					}
					xyz *= 1.0 / samples;
				}
					
				double power = xyz.x + xyz.y + xyz.z;
				xyz *= 1.f / power;
				
				// from E to d65 white
				/+
						0.9568   -0.0141    0.0226
					   -0.0190    0.9941    0.0080
						0.0038   -0.0058    1.0885
				+/
				{
					vec3d xyz2;
					xyz2.x = xyz.x * 0.9568 + xyz.y * -0.0141 + xyz.z * 0.0226;
					xyz2.y = xyz.x * -0.0190 + xyz.y * 0.9941 + xyz.z * 0.0080;
					xyz2.z = xyz.x * 0.0038 + xyz.y * -0.0058 + xyz.z * 1.0885;
					xyz = xyz2;
				}
				
				vec3d rgb = vec3d.zero;
				xyz_to_rgb(&CIEsystem, xyz.x, xyz.y, xyz.z, &rgb.r, &rgb.g, &rgb.b);
				constrain_rgb(&rgb.r, &rgb.g, &rgb.b);
				norm_rgb(&rgb.r, &rgb.g, &rgb.b);
				rgb *= power;
				
				float tos(float l) {
					if (l < 0.0031308f) {
						return l * 12.92f;
					} else {
						const float a = 0.055f;
						return (1 + a) * pow(l, 1.f / 2.4f) - a;
					}
				}
				
				ubyte f2ub(double f) {
					if (f >= 1.0) return 255;
					if (f <= 0.0) return 0;
					return cast(ubyte)(255 * f);
				}
				
				col = vec3ub(f2ub(tos(rgb.r)), f2ub(tos(rgb.g)), f2ub(tos(rgb.b)));
				// ----
			}
			
			return col;
		}, vec2i.zero, vec2i(width, height), "foo.tga");

		Stdout.formatln(\n`Done in {} sec`, sw.stop());
	}
}


vec3d getRadiance(vec3 pos, vec3 norm, float delegate(float wlen) mat, Scene sc) {
	vec3 ltnorm = -vec3.unitY;
	vec3 lp = ltrand();
	
	// offset the origin to avoid numerical instability
	Ray r2 = Ray(lp + ltnorm * 0.001f, pos - lp);
	
	Hit hit2 = Hit(10000.f);
	if (r2.dir.dot(ltnorm) > 0.0001f && sc.intersect(r2, hit2) && hit2.dist >= 0.9999f) {
		double p = -r2.dir.normalized.dot(norm) / r2.dir.sqLen;
		p *= lightPowerMult;
		
		vec3d contrib = spectrumToXYZ((double w) {
			int i = Cornell.emissionWavelengths.lbound(w);
			if (Cornell.emissionWavelengths.length == i) --i;
			float emIns = Cornell.emissionIntens[i] / 50.f;
			return cast(double)(mat(w) * emIns);
		});
		
		p /= Cornell.emissionWavelengths.length;
		return p * contrib;
	}
	
	return vec3d.zero;
}


vec3d trace(vec3 pos, vec3 norm, float delegate(float wlen) mat, Scene sc) {
	Ray r = Ray(pos + norm * 0.001f, nextDirection(norm));
	Hit hit = Hit(10000.f);
	
	if (sc.intersect(r, hit)) {
		vec3 poi = r.orig + r.dir * hit.dist;
		vec3 pnorm = prims[hit.prim * 2].n;
		
		auto mat2 = Cornell.surfaces[hit.prim].material();
		auto localRefl = (float wlen) {
			return mat(wlen) * mat2(wlen);
		};

		vec3d xyz = getRadiance(pos, norm, localRefl, sc);
		
		int reflSamples = 0;
		int reflFrom = 380;
		int reflTo = 700;
		float totalRefl = 0.f;
		for (int w = reflFrom; w < reflTo; w += (reflTo - reflFrom) / 10, ++reflSamples) {
			totalRefl += localRefl(w);
		}
		float meanRefl = totalRefl / reflSamples;
		
		if (frand < meanRefl) {
			xyz += trace(poi, pnorm, (float wlen) { return localRefl(wlen) / meanRefl; }, sc);
		}
		
		return xyz;
	}
	
	return vec3d.zero;
}


vec3 nextDirection(vec3 normal) {
	while (true) {
		vec3 d = vrand();
		if (d.dot(normal) < 0.f) d = -d;
		float dot = d.dot(normal);
		if (dot >= frand) return d;
	}
}
