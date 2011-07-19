module xf.rt.Spectrum;

private {
	import xf.rt.Color;
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.Misc;
	import tango.math.random.Kiss;
	extern (C) int printf(char*, ...);
}


private Kiss* rand;		// HACK
static this() {
	rand = new Kiss;
}



struct WavelenSpectrum {
	float	wavelength;
	
	struct FullSpectrum {
		const int		numSamples = 30;
		const float		minWavelength = 400.f;
		const float		maxWavelength = 700.f;
		
		float[numSamples]	intensity = 0.f;
		float						maxIntens = 0.f;
	
	
		/+static FullSpectrum fromRGB(vec3 rgb) {
			assert (false);
			return FullSpectrum.init;
		}+/
		
		
		static FullSpectrum fromWavelengths(float[] wavelen, float[] wavelenIntens) {
			FullSpectrum res;
			res.intensity[] = 0.f;
			
			int[numSamples] sampleCnt;
			
			float total = 0.f;
			
			foreach (i, wlen; wavelen) {
				float intens = wavelenIntens[i];
				assert (intens <>= 0.f);
				assert (wlen <>= 0.f);
				if (wlen >= minWavelength && wlen <= maxWavelength) {
					int snum = wavelengthSample(wlen);
					res.intensity[snum] += intens;
					++sampleCnt[snum];
					total += intens;
				}
			}
			
			printf("Spectrum:\n");
			foreach (i, ref intens; res.intensity) {
				if (sampleCnt[i] != 0) {
					intens /= sampleCnt[i];
				}
				printf("w:%f i:%f\n", sampleWavelength(i), intens);
			}
			printf("\n");
			
			/+if (total != 0.f) {
				float norm = 1.f / total;			
				foreach (ref intens; res.intensity) {
					intens *= total;
				}
			}+/
			
			return res;
		}
		
		/+float magnitude() {
			return max(rgb.r, rgb.g, rgb.b);
		}+/
		
		float y() {
			vec3 xyz = toXYZ;
			return xyz.x + xyz.y + xyz.z;
		}
		
		float max() {
			float m = 0.f;
			foreach (i; intensity) {
				if (i > m) m = i;
			}
			return m;
		}


		void normalize() {
			/+float magn = 0.f;
			foreach (intens; intensity) {
				magn += intens;
			}
			magn *= 1.f / numSamples;+/
			
			float magn = this.max;
			
			if (magn != 0.f) {
				//rgb *= 1.f / magn;
				float norm = 1.f / magn;
				foreach (ref intens; intensity) {
					intens *= norm;
				}
			}
		}
		

		void normalizeLight() {
			float magn = this.y;
			
			if (magn != 0.f) {
				//rgb *= 1.f / magn;
				float norm = 1.f / magn;
				foreach (ref intens; intensity) {
					intens *= norm;
				}
			}
		}

		
		static int wavelengthSample(float wlen) {
			assert (wlen <>= 0.f);
			if (wlen < minWavelength) return 0;
			if (wlen > maxWavelength) return numSamples - 1;
			const float offset = (maxWavelength - minWavelength) / (numSamples * 2);
			int res = cast(int)((wlen - minWavelength - offset) / (maxWavelength - minWavelength) * numSamples);
			if (res >= numSamples) res = numSamples - 1;
			if (res < 0) res = 0;
			return res;
		}
		
		static float sampleWavelength(int i) {
			if (i <= 0) return minWavelength;
			if (i >= numSamples) return maxWavelength;
			const float offset = (maxWavelength - minWavelength) / (numSamples * 2);
			return minWavelength + (maxWavelength - minWavelength) / numSamples * i + offset;
		}


		// not threadsafe
		WavelenSpectrum sample() {
			WavelenSpectrum res;
			res.wavelength = float.nan;
			
			// TODO: optimize me
			float total = 0.f;
			foreach (intens; intensity) {
				total += intens;
			}
			
			float frand() {
				return (1.0 / uint.max) * rand.toInt;
			}

			float r = frand() * total;
			foreach (i, intens; intensity) {
				r -= intens;
				if (r <= 0.f) {
					res.wavelength = sampleWavelength(i);
					return res;
				}
			}
			
			res.wavelength = maxWavelength;
			
			/+for (int i = 0; i < 10_000; ++i) {
				int snum = rand() % numSamples;
				if (intensity[snum] > frand()) {
					res.wavelength = sampleWavelength(snum);
					return res;
				}
			}+/
			
			return res;
		}
		
		FullSpectrum opMul(float m) {
			FullSpectrum result;
			foreach (i, ref s; result.intensity) {
				s = intensity[i] * m;
				assert (s <>= 0.f);
			}
			return result;
		}
		
		FullSpectrum opMul(FullSpectrum s) {
			auto res = *this;
			res *= s;
			return res;
		}
		
		void opMulAssign(float m) {
			foreach (i, ref s; intensity) {
				s *= m;
				assert (s <>= 0.f);
			}
		}

		void opMulAssign(FullSpectrum m) {
			foreach (i, ref s; intensity) {
				s *= m.intensity[i];
				assert (s <>= 0.f);
			}
		}
		
		void opAddAssign(FullSpectrum m) {
			foreach (i, ref s; intensity) {
				s += m.intensity[i];
				assert (s <>= 0.f);
			}
		}
		
		void add(WavelenSpectrum spec, float scale) {
			intensity[wavelengthSample(spec.wavelength)] += scale * numSamples;
		}

		vec3 toXYZ() {
			vec3d res = vec3d.zero;

			foreach (i, s; intensity) {
				vec3d c = spectrumToXYZ(sampleWavelength(i));
				res += c * s;
			}
			res *= 1.f / numSamples;
			
			/+for (int i = 0; i < 100; ++i) {
				res += sample.toXYZ * 0.01f;
			}+/

			return vec3.from(res);
		}
	}

	/+static RGBSpectrum sampleFromRGB(vec3 rgb) {
		return RGBSpectrum(rgb);
	}+/
	
	vec3 toXYZ() {
		if (wavelength !<>= 0.f) {
			return vec3.zero;
		}
		vec3d c = spectrumToXYZ(wavelength);
		return vec3.from(c);
	}

	/+float reflLuminance(FullSpectrum rs) {
		vec3 m = this.rgb;
		m.r *= rs.rgb.r;
		m.g *= rs.rgb.g;
		m.b *= rs.rgb.b;
		return max(m.r, m.g, m.b) * rs.scale;
	}+/

	/+float magnitude() {
		return max(rgb.r, rgb.g, rgb.b);
	}+/
	
	float y() {
		return 1.f;
	}
	
	float filter(FullSpectrum fs)
	out (res) {
		assert (res <>= 0.f);
	}
	body {
		return fs.intensity[fs.wavelengthSample(wavelength)];
	}

	void normalize() {
		// nothing to do here
	}
}


struct RGBSpectrum {
	vec3	rgb = vec3.zero;
	
	struct FullSpectrum {
		vec3	rgb = vec3.zero;
	
		static FullSpectrum fromRGB(vec3 rgb) {
			return FullSpectrum(rgb);
		}
		
		static FullSpectrum fromWavelengths(float[] wavelen, float[] wavelenIntens) {
			vec3 res = vec3.zero;
			//float maxI = 0.f;
			
			float maxIntens = 0.f;
			foreach (idx, i; wavelenIntens) {
				//if (i > maxI) maxI = i;
				vec3 part = vec3.from(spectrumToXYZ(wavelen[idx]) * i);
				//vec3 r = xyz2rgb(part);
				//if ((r.r + r.g + r.b) / 3 > maxIntens) maxIntens = (r.r + r.g + r.b) / 3;
				if (part.y > maxIntens) maxIntens = part.y;
				res += part;
			}
			//res *= 1.f / wavelenIntens.length;
			
			vec3 rgb = xyz2rgb(res);
			rgb = constrainRGB(rgb);
			//constrain_rgb(&rgb.r, &rgb.g, &rgb.b);
			
			float mult = maxIntens / res.y;//.max(rgb.r, rgb.g, rgb.b);
			//float mult = 1.f;//WavelenSpectrum.FullSpectrum.fromWavelengths(wavelen, wavelenIntens).max / FullSpectrum(rgb).max;
			
			//norm_rgb(&rgb.r, &rgb.g, &rgb.b);
			return FullSpectrum(rgb * mult);
		}
		
		float y() {
			vec3 xyz = toXYZ;
			return xyz.x + xyz.y + xyz.z;
			//return 0.212671f * rgb.r + 0.715160f * rgb.g + 0.072169f * rgb.b;
			//return (rgb.r + rgb.g + rgb.b) / 3;
		}

		float max() {
			//return 0.212671f * rgb.r + 0.715160f * rgb.g + 0.072169f * rgb.b;
			//return (rgb.r + rgb.g + rgb.b) / 3;
			return .max(rgb.r, rgb.g, rgb.b);
		}

		void normalize() {
			float magn = max;
			if (magn != 0.f) {
				rgb *= 1.f / magn;
			}
		}
		
		void normalizeLight() {
			float magn = y;
			if (magn != 0.f) {
				rgb *= 1.f / magn;
			}
		}

		RGBSpectrum sample() {
			vec3 res = this.rgb;
			//norm_rgb(&res.r, &res.g, &res.b);
			return RGBSpectrum(res);
		}
		
		FullSpectrum opMul(float m) {
			return FullSpectrum(rgb * m);
		}

		void opMulAssign(float m) {
			rgb *= m;
		}
		
		void opMulAssign(FullSpectrum m) {
			rgb.r *= m.rgb.r;
			rgb.g *= m.rgb.g;
			rgb.b *= m.rgb.b;
		}
		
		void opAddAssign(FullSpectrum m) {
			rgb.r += m.rgb.r;
			rgb.g += m.rgb.g;
			rgb.b += m.rgb.b;
		}

		void add(RGBSpectrum spec, float scale) {
			rgb += spec.rgb * scale;
		}

		vec3 toXYZ() {
			return rgb2xyz(rgb);
		}
	}

	/+static RGBSpectrum sampleFromRGB(vec3 rgb) {
		return RGBSpectrum(rgb);
	}+/
	
	vec3 toXYZ() {
		return rgb2xyz(rgb);
	}

	/+float reflLuminance(FullSpectrum rs) {
		vec3 m = this.rgb;
		m.r *= rs.rgb.r;
		m.g *= rs.rgb.g;
		m.b *= rs.rgb.b;
		return max(m.r, m.g, m.b) * rs.scale;
	}+/

	/+float magnitude() {
		return max(rgb.r, rgb.g, rgb.b);
	}+/
	
	float y() {
		//return 0.212671f * rgb.r + 0.715160f * rgb.g + 0.072169f * rgb.b;
		//return max(rgb.r, rgb.g, rgb.b);
		//return (rgb.r + rgb.g + rgb.b) / 3;

		vec3 xyz = toXYZ;
		return xyz.x + xyz.y + xyz.z;
	}
	
	float filter(FullSpectrum fs) {
		rgb.r *= fs.rgb.r;
		rgb.g *= fs.rgb.g;
		rgb.b *= fs.rgb.b;
		return 1.f;
	}

	void normalize() {
		float magn = y;
		if (magn != 0.f) {
			rgb *= 1.f / magn;
		}
	}
}


vec3 xyz2rgb(vec3 xyz) {
	vec3 rgb = void;
	rgb.r =  3.240479f * xyz.x + -1.537150f * xyz.y + -0.498535f * xyz.z;
	rgb.g = -0.969256f * xyz.x +  1.875991f * xyz.y +  0.041556f * xyz.z;
	rgb.b =  0.055648f * xyz.x + -0.204043f * xyz.y +  1.057311f * xyz.z;
	return rgb;
}


vec3 rgb2xyz(vec3 rgb) {
	vec3 xyz = void;
	xyz.x = 0.412453f * rgb.r + 0.357580f * rgb.g + 0.180423f * rgb.b;
	xyz.y = 0.212671f * rgb.r + 0.715160f * rgb.g + 0.072169f * rgb.b;
	xyz.z = 0.019334f * rgb.r + 0.119193f * rgb.g + 0.950227f * rgb.b;
	return xyz;
}






//version = RGBPhotonMapping;

version (RGBPhotonMapping) {
	alias RGBSpectrum Spectrum;
} else {
	alias WavelenSpectrum Spectrum;
}

alias Spectrum.FullSpectrum FullSpectrum;
