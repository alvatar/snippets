module xf.maths.Vec;

private {
	import xf.maths.Fixed;
	import Integer = tango.text.convert.Integer;
	import Float = tango.text.convert.Float;
}

public {
	import tango.math.Math;
	alias tango.math.Math.fabs fabs;
}




const int X		= 0;
const int Y		= 1;
const int Z		= 2;
const int W	= 3;



bool isNaN(T)(T a) {
	static if (is(typeof(T.init.isNaN()) : bool)) {
		return a.isNaN();
	} else static if (is(T : fixed)) {
		return false;
	} else {
		return a !<>= 0;
	}
}


struct vec(flt_, int dim_, int xpad, int ypad, int zpad, int wpad)
{
	static assert (dim_ >= 2 && dim_ <= 4);
	
	alias flt_ flt;
	
	const static bool	swizzled	= xpad != 0 || ypad != flt.sizeof || zpad != flt.sizeof*2 || wpad != flt.sizeof*3;
	const static int	dim			= dim_;

	static if (!swizzled) {
		static assert (vec.sizeof == flt.sizeof * dim_);
	}
	
	union {
		static if (!swizzled) {
			flt[dim] cell;
		}
		
		static if (dim >= 1) {
			struct {
				byte[xpad]	xpad_;
				union {
					flt	x, r;
				}
			}
		}
		
		static if (dim >= 2) {
			struct {
				byte[ypad]	ypad_;
				union {
					flt	y, g;
				}
			}
		}

		static if (dim >= 3) {
			struct {
				byte[zpad]	zpad_;
				union {
					flt	z, b;
				}
			}
		}

		static if (dim >= 4) {
			struct {
				byte[wpad]	wpad_;
				union {
					flt	w, a;
				}
			}
		}
	}


	bool ok() {
		static if (dim >= 1) if (isNaN(x)) return false;
		static if (dim >= 2) if (isNaN(y)) return false;
		static if (dim >= 3) if (isNaN(z)) return false;
		static if (dim >= 4) if (isNaN(w)) return false;
		return true;
	}
	
	// :P
	alias ok isOK;
	alias ok isCorrect;

	
	static if (!swizzled) {
		static if (2 == dim) const static vec zero = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 0) };
		static if (3 == dim) const static vec zero = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 0), z : .cscalar!(flt, 0) };
		static if (4 == dim) const static vec zero = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 0), z : .cscalar!(flt, 0), w : .cscalar!(flt, 0) };

		static if (2 == dim) const static vec one = { x : .cscalar!(flt, 1), y : .cscalar!(flt, 1) };
		static if (3 == dim) const static vec one = { x : .cscalar!(flt, 1), y : .cscalar!(flt, 1), z : .cscalar!(flt, 1) };
		static if (4 == dim) const static vec one = { x : .cscalar!(flt, 1), y : .cscalar!(flt, 1), z : .cscalar!(flt, 1), w : .cscalar!(flt, 1) };

		static if (2 == dim) const static vec unitX = { x : .cscalar!(flt, 1), y : .cscalar!(flt, 0) };
		static if (3 == dim) const static vec unitX = { x : .cscalar!(flt, 1), y : .cscalar!(flt, 0), z : .cscalar!(flt, 0) };
		static if (4 == dim) const static vec unitX = { x : .cscalar!(flt, 1), y : .cscalar!(flt, 0), z : .cscalar!(flt, 0), w : .cscalar!(flt, 0) };

		static if (2 == dim) const static vec unitY = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 1) };
		static if (3 == dim) const static vec unitY = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 1), z : .cscalar!(flt, 0) };
		static if (4 == dim) const static vec unitY = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 1), z : .cscalar!(flt, 0), w : .cscalar!(flt, 0) };

		static if (3 == dim) const static vec unitZ = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 0), z : .cscalar!(flt, 1) };
		static if (4 == dim) const static vec unitZ = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 0), z : .cscalar!(flt, 1), w : .cscalar!(flt, 0) };

		static if (4 == dim) const static vec unitW = { x : .cscalar!(flt, 0), y : .cscalar!(flt, 0), z : .cscalar!(flt, 0), w : .cscalar!(flt, 1) };
	}
	
	
	
	template swizzleDim(int xs, int ys, int zs, int ws) {
				static if (-1 == xs)	const int swizzleDim = swizzleDim!(0, ys, zs, ws) - 1;
		else	static if (-1 == ys)	const int swizzleDim = swizzleDim!(xs, 0, zs, ws) - 1;
		else	static if (-1 == zs)	const int swizzleDim = swizzleDim!(xs, ys, 0, ws) - 1;
		else	static if (-1 == ws)	const int swizzleDim = swizzleDim!(xs, ys, zs, 0) - 1;
		else	const int swizzleDim = 4;
	}
	
	template swizzlePad(int pos) {
				static if (0 == pos) const int swizzlePad = xpad;
		else	static if (1 == pos) const int swizzlePad = ypad;
		else	static if (2 == pos) const int swizzlePad = zpad;
		else	static if (3 == pos) const int swizzlePad = wpad;
		else	const int swizzlePad = 0;
	}
	
	template swizzleT(int xs, int ys, int zs, int ws) {
		alias .vec!(flt, swizzleDim!(xs, ys, zs, ws), swizzlePad!(xs), swizzlePad!(ys), swizzlePad!(zs), swizzlePad!(ws)) swizzleT;
	}
	
	swizzleT!(xs, ys, zs, ws)* swizzle(int xs, int ys, int zs=-1, int ws=-1)() {
		return cast(swizzleT!(xs, ys, zs, ws)*)this;
	}
	

	static if (swizzled) {
		.vec!(flt, dim, 0, flt.sizeof, flt.sizeof*2, flt.sizeof*3) unswizzle() {
			.vec!(flt, dim, 0, flt.sizeof, flt.sizeof*2, flt.sizeof*3) res;
			static if (dim >= 1) res.x = x;
			static if (dim >= 2) res.y = y;
			static if (dim >= 3) res.z = z;
			static if (dim >= 4) res.w = w;
			return res;
		}
	}
	
	
	static vec convert(T)(T v) {
		vec res;
		static if (dim >= 1) res.x = scalar!(flt)(v.x);
		static if (dim >= 2) res.y = scalar!(flt)(v.y);
		static if (dim >= 3) res.z = scalar!(flt)(v.z);
		static if (dim >= 4) res.w = scalar!(flt)(v.w);
		return res;
	}
	alias convert from;
	

	void set(T)(T v) {
		static if (dim >= 1) x = scalar!(flt)(v.x);
		static if (dim >= 2) y = scalar!(flt)(v.y);
		static if (dim >= 3) z = scalar!(flt)(v.z);
		static if (dim >= 4) w = scalar!(flt)(v.w);
	}

	
	static if (dim == 2) {
		static vec opCall(flt x, flt y) {
			vec res = void;
			res.x = x;
			res.y = y;
			return res;
		}
	}
	
	
	static if (dim == 3) {
		static vec opCall(flt x, flt y, flt z) {
			vec res = void;
			res.x = x;
			res.y = y;
			res.z = z;
			return res;
		}
	}
	
	
	static if (dim == 4) {
		static vec opCall(flt x, flt y, flt z, flt w) {
			vec res = void;
			res.x = x;
			res.y = y;
			res.z = z;
			res.w = w;
			return res;
		}
	}
	

	static if (dim == 2) {
		static vec opIndex(real x, real y) {
			vec res = void;
			res.x = scalar!(flt)(x);
			res.y = scalar!(flt)(y);
			return res;
		}
	}
	
	
	static if (dim == 3) {
		static vec opIndex(real x, real y, real z) {
			vec res = void;
			res.x = scalar!(flt)(x);
			res.y = scalar!(flt)(y);
			res.z = scalar!(flt)(z);
			return res;
		}
	}
	
	
	static if (dim == 4) {
		static vec opIndex(real x, real y, real z, real w) {
			vec res = void;
			res.x = scalar!(flt)(x);
			res.y = scalar!(flt)(y);
			res.z = scalar!(flt)(z);
			res.w = scalar!(flt)(w);
			return res;
		}
	}

	
	char[] toString() {
		char[] res = "[";
		static if (is(typeof(x.toString))) {
			res ~= x.toString();
			static if (dim >= 2) res ~= ", " ~ y.toString();
			static if (dim >= 3) res ~= ", " ~ z.toString();
			static if (dim >= 4) res ~= ", " ~ w.toString();
		} else {
			static if (is(flt : long)) {
				alias Integer.toString str;
			} else {
				alias Float.toString str;
			}
			res ~= str(x);
			static if (dim >= 2) res ~= ", " ~ str(y);
			static if (dim >= 3) res ~= ", " ~ str(z);
			static if (dim >= 4) res ~= ", " ~ str(w);
		}
		return res ~ "]";
	}
	
	
	flt dot(T)(inout T v) {
		static assert (v.dim == dim);
		static if (2 == dim) return x * v.x + y * v.y;
		else static if (3 == dim) return x * v.x + y * v.y + z * v.z;
		else static if (4 == dim) return x * v.x + y * v.y + z * v.z + w * v.w;
		else static assert (false);
	}
	
	
	static if(dim==3) {
		static vec cross(vec a, vec b) {
			static assert (a.dim == 3 && b.dim == 3);
			
			return opCall
			(
				a.y * b.z - b.y * a.z,
				a.z * b.x - b.z * a.x,
				a.x * b.y - b.x * a.y
			);
		}
		
		
		static if (is(typeof(zero)) && !(is(flt == fixed))) {
			/// assumes that the current vector is normalized
			void formBasis(vec* v1, vec* v2) {
				float anx = fabs(x);
				float any = fabs(y);
				float anz = fabs(z);
				
				int k;
				if (anx > any) {
					if (anx > anz) k = 1; else k = 0;
				} else {
					if (any > anz) k = 2; else k = 0;
				}
				
				*v1 = zero;
				v1.cell[k] = 1;
				
				*v2 = cross(*this, *v1);
				*v1 = cross(*v2, *this);
			}
		}
	}
	
	
	static if (dim==2) {
		vec rotatedHalfPi() {
			vec res = void;
			res.x = -this.y;
			res.y = this.x;
			return res;
		}
	}
	

	flt sqLen() {
				static if (2 == dim) return x * x + y * y;
		else	static if (3 == dim) return x * x + y * y + z * z;
		else	static if (4 == dim) return x * x + y * y + z * z + w * w;
		else	static assert (false);
	}

	
	flt len() {
		flt sq = sqLen();
		if (sq != cscalar!(flt, 0)) {
			return scalar!(flt)(sqrt(cast(real)sq));
		}
		return sq;
	}
	
	
	void normalize() {
		// TODO: optimize for floating point types
		flt l = len();
		if (l != cscalar!(flt, 0)) {
			static if (is(flt == float) || is(flt == double) || is(flt == real)) {
				flt inv = cscalar!(flt, 1) / l;
				*this *= inv;
			} else {
				*this /= l;
			}
		}
	}
	
	
	vec normalized() {
		vec res = *this;
		res.normalize();
		return res;
	}
	
	
	void opSubAssign(inout vec rhs) {
		static if (dim >= 1) x -= rhs.x;
		static if (dim >= 2) y -= rhs.y;
		static if (dim >= 3) z -= rhs.z;
		static if (dim >= 4) w -= rhs.w;
	}

	
	void opAddAssign(inout vec rhs) {
		static if (dim >= 1) x += rhs.x;
		static if (dim >= 2) y += rhs.y;
		static if (dim >= 3) z += rhs.z;
		static if (dim >= 4) w += rhs.w;
	}
	
	
	void opMulAssign(flt_)(flt_ rhs) {
		static if (dim >= 1) x *= rhs;
		static if (dim >= 2) y *= rhs;
		static if (dim >= 3) z *= rhs;
		static if (dim >= 4) w *= rhs;
	}
	
	
	void opDivAssign(flt rhs) {
		static if (dim >= 1) x /= rhs;
		static if (dim >= 2) y /= rhs;
		static if (dim >= 3) z /= rhs;
		static if (dim >= 4) w /= rhs;
	}
	
	
	vec opSub(inout vec rhs) {
		static if (2 == dim) return vec(x - rhs.x, y - rhs.y);
		static if (3 == dim) return vec(x - rhs.x, y - rhs.y, z - rhs.z);
		static if (4 == dim) return vec(x - rhs.x, y - rhs.y, z - rhs.z, w - rhs.w);
	}


	vec opAdd(inout vec rhs) {
		static if (2 == dim) return opCall(x + rhs.x, y + rhs.y);
		static if (3 == dim) return opCall(x + rhs.x, y + rhs.y, z + rhs.z);
		static if (4 == dim) return opCall(x + rhs.x, y + rhs.y, z + rhs.z, w + rhs.w);
	}
	
	
	vec opMul(flt_)(flt_ rhs) {
		vec res = *this;
		res *= rhs;
		return res;
	}
	

	vec opDiv(flt rhs) {
		vec res = *this;
		res /= rhs;
		return res;
	}

	
	vec opNeg() {
		static if (2 == dim) return opCall(-x, -y);
		static if (3 == dim) return opCall(-x, -y, -z);
		static if (4 == dim) return opCall(-x, -y, -z, -w);
	}
	
	
	static if (!swizzled) {
		int leastAxis() {
			real m;
			int mi = -1;
			foreach (i, c; cell) {
				if (abs(cast(real)c) < m || -1 == mi) {
					m = abs(cast(real)c);
					mi = i;
				}
			}
			assert (mi != -1);
			return mi;
		}
	}
}


alias vec!(float, 2, 0, float.sizeof, float.sizeof*2, float.sizeof*3)	vec2;
alias vec!(float, 3, 0, float.sizeof, float.sizeof*2, float.sizeof*3)	vec3;
alias vec!(float, 4, 0, float.sizeof, float.sizeof*2, float.sizeof*3)	vec4;

alias vec!(fixed, 2, 0, fixed.sizeof, fixed.sizeof*2, fixed.sizeof*3)	vec2fi;
alias vec!(fixed, 3, 0, fixed.sizeof, fixed.sizeof*2, fixed.sizeof*3)	vec3fi;
alias vec!(fixed, 4, 0, fixed.sizeof, fixed.sizeof*2, fixed.sizeof*3)	vec4fi;

alias vec!(int, 2, 0, int.sizeof, int.sizeof*2, int.sizeof*3)	vec2i;
alias vec!(int, 3, 0, int.sizeof, int.sizeof*2, int.sizeof*3)	vec3i;
alias vec!(int, 4, 0, int.sizeof, int.sizeof*2, int.sizeof*3)	vec4i;

alias vec!(ubyte, 2, 0, 1, 2, 3) vec2ub;
alias vec!(ubyte, 3, 0, 1, 2, 3) vec3ub;
alias vec!(ubyte, 4, 0, 1, 2, 3) vec4ub;

alias vec!(double, 2, 0, double.sizeof, double.sizeof*2, double.sizeof*3)	vec2d;
alias vec!(double, 3, 0, double.sizeof, double.sizeof*2, double.sizeof*3)	vec3d;
alias vec!(double, 4, 0, double.sizeof, double.sizeof*2, double.sizeof*3)	vec4d;


unittest {
	// floats
	{
		vec4 a = vec4(0f, 1f, 2f, 3f);
		
		auto b = a.swizzle!(Y, X).unswizzle();
		assert(b.dim == 2);
		assert(b == vec2(1f, 0f));
		
		assert(a.swizzle!(W, Z).unswizzle() == vec2(3f, 2f));
		assert(a.swizzle!(Y, Z).x == 1.f);
		
		assert(a.swizzle!(Z, Y, X).unswizzle() == vec3(2f, 1f, 0f));
		assert(a.swizzle!(Z, Y, X)().swizzle!(Y, X)().unswizzle.cell == [1f, 2f]);
	
		vec3 i = vec3(1f, 0f, 0f);
		vec3 j = vec3(0f, 1f, 0f);
		vec3 k = vec3.cross(i, j);
		assert (k == vec3(0f, 0f, 1f));
	}


	// fixeds
	{
		vec4fi a = vec4fi[0f, 1f, 2f, 3f];
		
		auto b = a.swizzle!(Y, X).unswizzle();
		assert(b.dim == 2);
		assert(b == vec2fi[1f, 0f]);
		
		assert(a.swizzle!(W, Z).unswizzle() == vec2fi[3f, 2f]);
		assert(a.swizzle!(Y, Z).x == scalar!(fixed)(1.f));
		
		assert(a.swizzle!(Z, Y, X).unswizzle() == vec3fi[2f, 1f, 0f]);
		assert(a.swizzle!(Z, Y, X)().swizzle!(Y, X)().unswizzle.cell == [cscalar!(fixed, 1), cscalar!(fixed, 2)]);
	
		vec3fi i = vec3fi[1f, 0f, 0f];
		vec3fi j = vec3fi[0f, 1f, 0f];
		vec3fi k = vec3fi.cross(i, j);
		assert (k == vec3fi[0f, 0f, 1f]);
	}

	{
		vec3fi	a = vec3fi[1, 2, 3];
		
		auto		b = a.swizzle!(X, Y, Y, Z)();
		assert (b.unswizzle() == vec4fi[1, 2, 2, 3]);
		
		b.y = fixed.fromInt(4);
		assert (a == vec3fi[1, 4, 3]);
		
		b.z = fixed.fromInt(5);
		assert (a == vec3fi[1, 5, 3]);
		
		auto		c = a.swizzle!(Z, Y, X)();
		assert (c.unswizzle() == vec3fi[3, 5, 1]);
		
		assert (c.swizzle!(Z, Y, X).dot(a) == a.sqLen);
	}
	
	{
		vec3		a = vec3(1, 2, 3);
		//vec3fi	b = vec3fi.convert(a);		// bug in dmd.172
		vec3fi	b = vec3fi.init.convert(a);
	}
}
