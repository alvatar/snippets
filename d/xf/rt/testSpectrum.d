module xf.rt.testSpectrum;

private {
	import xf.rt.Misc;
	import xf.rt.Math;
	import xf.rt.Spectrum;
	import xf.rt.Color;
	import Cornell = xf.rt.CornellBoxData;
	
	import tango.text.convert.Format;
	import tango.io.Stdout;
}



void main() {
	const int width	= 150;
	const int height	= 558;
	const int minW	= 400;
	const int maxW	= 800;
	
	float wlenSample(float wlen) {
		static float[] wlens = [400.f, 500, 600, 700];
		static float[] intens = [0.f, 8, 15.6, 18];
		
		if (wlen < wlens[0]) {
			return 0.f;
		}

		if (wlen > wlens[$-1]) {
			return 0.f;
		}
		
		for (int i = 0; i+1 < wlens.length; ++i) {
			if (wlen >= wlens[i] && wlen <= wlens[i+1]) {
				float step = (wlen - wlens[i]) / (wlens[i+1] - wlens[i]);
				float v1 = intens[i];
				float v2 = intens[i+1];
				return (1.f - step) * v1 + step * v2;
			}
		}
		assert (false, Format("{}", wlen));
	}
	
	/+int sample = 0;
	for (	float wlen = spectrum.minWavelength; sample < spectrum.intensity.length;
			wlen += (spectrum.maxWavelength - spectrum.minWavelength) / spectrum.numSamples) {
			spectrum.intensity[sample++] = wlenSample(wlen) / 10;
	}+/
	
	
	writeTGA((int x, int y) {
		auto spectrum = WavelenSpectrum(cast(float)(height - y - 1) / height * (maxW - minW) + minW);
		//auto spectrum = Cornell.lightSpectrum;
		vec3d xyz = vec3d.from(spectrum.toXYZ);
		//vec3d rgb = vec3d(5.453, 3.766, 1.266) * 0.15;
		vec3d rgb = xyz2srgb(xyz);
		rgb = gammaCorrectSRGB(rgb);
		rgb *= 1.2;
				
		ubyte f2ub(double f) {
			if (f >= 1.0) return 255;
			if (f <= 0.0) return 0;
			return cast(ubyte)(255 * f);
		}
		
		return vec3ub(f2ub(rgb.r), f2ub(rgb.g), f2ub(rgb.b));
		//return col;
	}, vec2i.zero, vec2i(width, height), "spectrum.tga");
}
