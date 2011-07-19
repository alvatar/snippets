module xf.rt.Light;

private {
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.Misc;
	import xf.rt.Spectrum;
	import xf.rt.Sampling;
	import xf.rt.Misc;
	
	import tango.io.Stdout;
}



abstract class Light {
	float							intensity;	
	abstract Ray				sampleRay();
	abstract void				samplePointsStratified(int total, void delegate(vec3, vec3) dg);
	abstract Spectrum		sampleSpectrum(vec3 direction);
	abstract FullSpectrum	getSpectrum(vec3 direction);
}



// the default orientation is [i, j, k], thus a light that points up
final class UniformQuadLight : Light {
	mat3				orientation	= mat3.identity;
	vec3				origin		= vec3.zero;
	vec2				size			= {x: 1, y: 1};
	FullSpectrum	spectrum;
	
	
	
	// TODO: optimize me
	// TODO: add random
	override void samplePointsStratified(int total, void delegate(vec3, vec3) dg) {
		float cnt = sqrt(cast(real)total / (size.x * size.y));
		int nx = rndint(size.x * cnt);
		int ny = rndint(size.y * cnt);
		if (nx <= 0) nx = 1;
		if (ny <= 0) ny = 1;
		
		//Stdout.formatln(`total:{}, cnt:{}, x:{}, y:{}`, total, cnt, nx, ny);
		
		for (int x = 1; x <= nx; ++x) {
			float xf = cast(float)x / (nx+1) - .5f;
			float xr = 1.f / (nx+1);
			for (int y = 1; y <= ny; ++y) {
				float yf = cast(float)y / (ny+1) - .5f;
				float yr = 1.f / (ny+1);
				
				vec3 pt;
				pt = vec3((xf + (frand - .5f) * xr) * size.x, 0, (yf + (frand - .5f) * yr) * size.y);
				pt = orientation * pt;
				pt += origin;
				dg(pt, orientation.col[1].vec);
			}
		}
	}
	
	
	override Ray sampleRay() {
		Ray ray	= void;
		ray.orig	= origin + orientation * vec3((frand - .5f) * size.x, 0, (frand - .5f) * size.y);
		ray.dir	= cosineHemisphericalSample(orientation.col[1].vec);
		return ray;
	}
	
	
	override Spectrum sampleSpectrum(vec3 direction) {
		return spectrum.sample;
	}
	
	
	override FullSpectrum getSpectrum(vec3 direction) {
		return spectrum;
	}
}
