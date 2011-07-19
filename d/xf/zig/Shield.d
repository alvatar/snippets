module xf.zig.Shield;

private {
	import xf.dog.OpenGL;
	import xf.dog.GLWrap;
	import xf.maths.Misc;
	import xf.maths.Vec;

	extern (C) int printf(char* format, ...);
}



class Shield {
	this(float radius) {
		this.radius = radius;
	}
	
	
	void render(GL gl) {
		const int segments = 16;
		float[segments+1] precalcMult;
		
		foreach (i, ref m; precalcMult) {
			float a = i * 360.f / segments;
			m = calcMult(a);
		}
		
		auto iter = delegate int(int delegate(inout float a, inout float mult, inout float rmult) dg) {
			for (int i = 0; i <= segments; ++i) {
				float mult = precalcMult[i];
				float alpha = color.a * mult * (energy / maxEnergy);
				gl.Color4f(color.r, color.g, color.b, alpha);
				float rmult = renderThicknessFunc(mult);
				float a = i * 360.f / segments;
				if (auto res = dg(a, mult, rmult)) return res;
			}
			return 0;
		};

		gl.withState(GL_BLEND, GL_LINE_SMOOTH) in {
			gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

			// outer line
			gl.immediate(GL_LINE_STRIP, {
				foreach (a, mult, rmult; iter) {
					float d = this.radius + rmult;
					float arad = deg2rad * a;
					gl.Vertex2f(-sin(arad) * d, cos(arad) * d);
				}
			});

			// inner line
			gl.immediate(GL_LINE_STRIP, {
				foreach (a, mult, rmult; iter) {
					float d = this.radius;
					float arad = deg2rad * a;
					gl.Vertex2f(-sin(arad) * d, cos(arad) * d);
				}
			});
		};
	}
	
	
	void update() {
		energy += rechargeRate;
		if (energy > maxEnergy) energy = maxEnergy;
	}
	
	
	float takeDamage(float dmg) {
		energy -= dmg;
		
		if (energy < 0.f) {
			float res = -energy;
			energy = 0.f;
			return res;
		} else {
			return 0.f;
		}
	}
	
	
	float calcMult(float angle) {
		int idx1 = genIdx(angle);
		int idx0 = (idx1 + numGens - 1) % numGens;
		int idx2 = (idx1 + 1) % numGens;
		
		float i01 = circleAbsDiff!(360.f)(idx0 * genSpacing, angle) / genSpacing;
		float i12 = circleAbsDiff!(360.f)(idx2 * genSpacing, angle) / genSpacing;
		float i02 = circleAbsDiff!(360.f)(idx0 * genSpacing, angle) / (genSpacing * 2);
		
		void wrap01(inout float f) {
			if (f < 0.f) f = 0.f;
			if (f > 1.f) f = 1.f;
		}
		
		wrap01(i01);
		wrap01(i12);
		wrap01(i02);
		
		// Bezier-like interpolation
		float v01, v12, res;
		linearInterp(i01, focus[idx0], focus[idx1], v01);
		linearInterp(i12, focus[idx2], focus[idx1], v12);
		
		linearInterp(i02, v01, v12, res);
		return res;
	}
	
	
	void focusAtAngle(float angle) {
		focus[genIdx(angle)]							+= focusIncr;
		focus[idxOffset(genIdx(angle), -1)]		+= focusIncr * focusNeighIncr;
		focus[idxOffset(genIdx(angle), 1)]		+= focusIncr * focusNeighIncr;
		normalizeFocus();
	}
	
	
	private {
		int genIdx(float angle) {
			return rndint(angle / 45.f) % numGens;
		}
		
		
		int idxOffset(int idx, int offset) {
			assert (-offset <= numGens);
			return (idx + numGens + offset) % numGens;
		}
		
		
		void resetFocus() {
			focus[] = 1.f;
		}
		
		
		void normalizeFocus() {
			foreach (inout f; focus) {
				if (f > maxFocus) f = maxFocus;
				if (f < minFocus) f = minFocus;
			}
			float sum = 0.f;
			foreach (f; focus) sum += f;
			float norm = (1.f * numGens) / sum;
			foreach (inout f; focus) f *= norm;
		}
		
		
		float renderThicknessFunc(float th) {
			return thicknessMult * th;
		}
	}
	
	
	const int	numGens = 8;
	const float	genSpacing = 360.f / numGens;
	
	float[8]	focus = 1.f;	// angle = index * 45
	int		focusAngle = 0;

	float	energy = 100.f;
	float	maxEnergy = 100.f;
	float	rechargeRate = 10.f * .01f;		// 10 per second
	
	float	maxFocus = 10.f;
	float	minFocus = .1f;
	
	float	focusIncr = 0.08f;
	float	focusNeighIncr = 0.25;
	
	float	radius = 0.f;
	float	thicknessMult = 0.125f;
	
	vec4	color = {r: 0.4f, g: 0.7f, b:1.f, a:0.4f};
}
