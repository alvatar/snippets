module xf.maths.Fixed;

import tango.math.Math : rndint;
import tango.text.convert.Float : toString;



version (NoFixedMath)
{
	struct fixedT {
		private	double store;	
		
		
		static fixedT fromFlt_(flt)(flt val) {
			fixedT res;
			res.store = val;
			return res;
		}
		alias fromFlt_!(float)		fromFloat;
		alias fromFlt_!(double)	fromDouble;
		alias fromFlt_!(real)		fromReal;
		
	
		static fixedT fromInt(int val) {
			fixedT res;
			res.store = val;
			return res;
		}
		
		
		void opAddAssign(fixedT rhs) {
			store += rhs.store;
		}
		
		
		void opSubAssign(fixedT rhs) {
			store -= rhs.store;
		}
		
		
		void opMulAssign(fixedT rhs) {
			store *= rhs.store;
		}
		
		
		void opDivAssign(fixedT rhs) {
			store /= rhs.store;
		}
		
		
		fixedT opAdd(fixedT rhs) {
			fixedT res = *this;
			res += rhs;
			return res;
		}
	
	
		fixedT opSub(fixedT rhs) {
			fixedT res = *this;
			res -= rhs;
			return res;
		}
	
	
		fixedT opMul(fixedT rhs) {
			fixedT res = *this;
			res *= rhs;
			return res;
		}
	
	
		fixedT opDiv(fixedT rhs) {
			fixedT res = *this;
			res /= rhs;
			return res;
		}
		
	
		fixedT opNeg() {
			fixedT res;
			res.store = -store;
			return res;
		}
	
		
		int opCmp(fixedT rhs) {
			if (store > rhs.store) return 1;
			if (store < rhs.store) return -1;
			return 0;
		}
	
	
		bool opEquals(fixedT rhs) {
			return store == rhs.store;
		}
	
	
		char[] toString() {
			return .toString(cast(real)*this);
		}
		
		
		real opCast() {
			return store;
		}
	}
	
	alias fixedT fixed;
	
	
	
	
	public {
		template cscalar(T, real val) {
			const T cscalar = cast(T)val;
		}
		
		template cscalar(T : fixed, real val) {
			const T cscalar = { store: val };
		}
		
		protected template scalarT(T) {
			static if (is(T : fixed)) {
				T scalar(real val) {
					return fixed.fromReal(val);
				}
	
				T scalar(fixed val) {
					return val;
				}
			} else {
				T scalar(real val) {
					return cast(T)val;
				}
			
				T scalar(fixed val) {
					return cast(T)cast(real)val;
				}
			}
		}
		
		template scalar(T) {
			alias scalarT!(T).scalar scalar;
		}
	}
}

else		// use fixed point math
{
	struct fixedT(int resolution_) {
		static const	int resolution = resolution_;
		private			int store = 0;
		
		
		static fixedT fromFlt_(flt)(flt val) {
			fixedT res;
			res.store = rndint(val * resolution);
			return res;
		}
		alias fromFlt_!(float)		fromFloat;
		alias fromFlt_!(double)	fromDouble;
		alias fromFlt_!(real)		fromReal;
		
	
		static fixedT fromInt(int val) {
			val *= resolution;
			fixedT res;
			res.store = cast(int)val;
			return res;
		}
		
		
		void opAddAssign(fixedT rhs) {
			store += rhs.store;
		}
		
		
		void opSubAssign(fixedT rhs) {
			store -= rhs.store;
		}
		
		
		void opMulAssign(fixedT rhs) {
			store = cast(int)((cast(long)rhs.store * store) / resolution);
		}


		// we'll allow scaling by real as scaling is often used in various interpolation functions
		void opMulAssign(real rhs) {
			opMulAssign(fixedT.fromReal(rhs));
		}
		
		
		void opDivAssign(fixedT rhs) {
			long st = store;
			st *= resolution;
			st /= rhs.store;
			store = cast(int)st;
		}
		
		
		fixedT opAdd(fixedT rhs) {
			fixedT res = *this;
			res += rhs;
			return res;
		}
	
	
		fixedT opSub(fixedT rhs) {
			fixedT res = *this;
			res -= rhs;
			return res;
		}
	
	
		fixedT opMul(fixedT rhs) {
			fixedT res = *this;
			res *= rhs;
			return res;
		}
	

		fixedT opMul(real rhs) {
			fixedT res = *this;
			res *= rhs;
			return res;
		}

	
		fixedT opDiv(fixedT rhs) {
			fixedT res = *this;
			res /= rhs;
			return res;
		}
		
	
		fixedT opNeg() {
			fixedT res;
			res.store = -store;
			return res;
		}
	
		
		int opCmp(fixedT rhs) {
			return store - rhs.store;
		}
	
	
		bool opEquals(fixedT rhs) {
			return store == rhs.store;
		}
	
	
		char[] toString() {
			return .toString(cast(real)*this);
		}
		
		
		real opCast() {
			return (1.0 / resolution) * store;
		}
	}
	
	alias fixedT!(1 << 12) fixed;
	
	
	
	
	public {
		template cscalar(T, real val) {
			const T cscalar = cast(T)val;
		}
		
		template cscalar(T : fixed, real val) {
			const T cscalar = { store: cast(int)(val * T.resolution + .5f) };
		}
		
		protected template scalarT(T) {
			static if (is(T : fixed)) {
				T scalar(real val) {
					return fixed.fromReal(val);
				}
	
				T scalar(fixed val) {
					return val;
				}
			} else {
				T scalar(real val) {
					return cast(T)val;
				}
			
				T scalar(fixed val) {
					return cast(T)cast(real)val;
				}
			}
		}
		
		template scalar(T) {
			alias scalarT!(T).scalar scalar;
		}
	}
	
	
	
	
	/+unittest {
		fixed a = fixed.fromFloat(3.1415f);
		assert (a.store == 3217);
	
		a *= fixed.fromInt(2);
		assert (a.store == 3217 * 2);
		
		a /= fixed.fromInt(3);
		assert (a.store == 3217 * 2 / 3);
	
		a *= fixed.fromInt(-1);
		assert (a.store == 3217 * 2 / 3 * -1);
		
		a += fixed.fromInt(2_000_000);
		assert (a.store == 3217 * 2 / 3 * -1 + 2048000000);
	
		a = fixed.fromReal(0.001);
		assert (a.store == 1);
		
		assert (cast(real)fixed.fromReal(0.5) == 0.5);
		assert (fixed.fromReal(0.125).toString() == "0.125");
		
		a *= 0.5;
	}+/
}