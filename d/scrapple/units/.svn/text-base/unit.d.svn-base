module unit;

private import std.math;

import rational;

struct Unit(
			int LengthN,  int LengthD,
			int MassN,    int MassD,
			int TimeN,    int TimeD,
			int TempN,    int TempD,
			int CurrentN, int CurrentD
			)
{
	void _rawSet(real v){value = v;}
	real _rawGet(){return value;}

	static const LenN = LengthN,  LenD = LengthD;
	static const MasN = MassN,    MasD = MassD;
	static const TimN = TimeN,    TimD = TimeD;
	static const TmpN = TempN,    TmpD = TempD;
	static const CurN = CurrentN, CurD = CurrentD;
	
	static assert (Reduce!(LengthN,  LengthD).Reduced, "Length must be in reduced form");
	static assert (Reduce!(MassN,    MassD).Reduced,   "Mass must be in reduced form");
	static assert (Reduce!(TimeN,    TimeD).Reduced,   "Time must be in reduced form");
	static assert (Reduce!(TempN,    TempD).Reduced,   "Temp must be in reduced form");
	static assert (Reduce!(CurrentN, CurrentD).Reduced,"Current must be in reduced form");

	private real value;

	private alias Unit!(LengthN, LengthD, MassN, MassD, TimeN, TimeD, TempN, TempD, CurrentN, CurrentD) This;

	private template Mul(T)
	{
		static if(is(T : real))
			alias This Mul;
		else
			alias Unit!
					(
						Reduce!(LengthN*T.LenD  + LengthD*T.LenN,  LengthD * T.LenD).V,
						Reduce!(MassN*T.MasD    + MassD*T.MasN,    MassD*T.MasD).V,
						Reduce!(TimeN*T.TimD    + TimeD*T.TimN,    TimeD*T.TimD).V,
						Reduce!(TempN*T.TmpD    + TempD*T.TmpN,    TempD*T.TmpD).V,
						Reduce!(CurrentN*T.CurD + CurrentD*T.CurN, CurrentD*T.CurD).V
					) Mul;
	}
	public template Div(T)
	{
		static if(is(T : real))
			alias This Div;
		else
			alias Unit!
					(
						Reduce!(LengthN*T.LenD  - LengthD*T.LenN,  LengthD * T.LenD).V,
						Reduce!(MassN*T.MasD    - MassD*T.MasN,    MassD*T.MasD).V,
						Reduce!(TimeN*T.TimD    - TimeD*T.TimN,    TimeD*T.TimD).V,
						Reduce!(TempN*T.TmpD    - TempD*T.TmpN,    TempD*T.TmpD).V,
						Reduce!(CurrentN*T.CurD - CurrentD*T.CurN, CurrentD*T.CurD).V
					) Div;
	}

	static public template RootT(int i)
	{
		alias Unit!(
			Reduce!(LenN, LenD*i).V,
			Reduce!(MasN, MasD*i).V,
			Reduce!(TimN, TimD*i).V,
			Reduce!(TmpN, TmpD*i).V,
			Reduce!(CurN, CurD*i).V
			) RootT;
	}

	public template Power(int i)
	{
		alias Unit!(
			Reduce!(LenN*i, LenD).V,
			Reduce!(MasN*i, MasD).V,
			Reduce!(TimN*i, TimD).V,
			Reduce!(TmpN*i, TmpD).V,
			Reduce!(CurN*i, CurD).V
			) Power;
	}

	This opCall(This ret) { return ret; }

	This opNeg() { This ret; ret.value = - this.value; return ret; }
	This opPos() { return *this; }

	This opAdd(This that) { This ret; ret.value = this.value + that.value; return ret; }
	This opSub(This that) { This ret; ret.value = this.value - that.value; return ret; }
	This opAddAssign(This that) { this.value += that.value; return *this; }
	This opSubAssign(This that) { this.value -= that.value; return *this; }

	int opCmp(This that) 
	{
		if(this.value < that.value) return -1;
		if(this.value > that.value) return +1;
		return 0;
	}

	int opCmp(real that) 
	{
		if(this.value < that) return -1;
		if(this.value > that) return +1;
		return 0;
	}

	static if(LengthN == 0 && MassN == 0 && TimeN == 0 && TempN == 0 && CurrentN == 0)
	{
		static This opCall(real v) { This ret; ret.value = v; return ret; }

		This opAdd(real that) { This ret; ret.value = this.value + that; return ret; }
		This opSub(real that) { This ret; ret.value = this.value - that; return ret; }
		This opAddAssign(real that) { this.value += that; return *this; }
		This opSubAssign(real that) { this.value -= that; return *this; }
		This opAdd_r(real that) { This ret; ret.value = that + this.value; return ret; }
		This opSub_r(real that) { This ret; ret.value = that - this.value; return ret; }
		real opCast() { return value; }

		This exp()
		{
			This ret;
			ret.value = std.math.exp(this.value);
			return ret;
		}
		This log()
		{
			This ret;
			ret.value = std.math.log(this.value);
			return ret;
		}
		This pow(real p)
		{
			This ret;
			ret.value = std.math.pow(this.value,p);
			return ret;
		}
	}
	
	bool Near(This that, int count = 5)
	{
		return std.math.feqrel!(real)(this.value,that.value) + count >= real.mant_dig;
	}

	//static if(is(This == Unit!(1,1, 0,1, 0,1, 0,1, 0,1))) pragma(msg, Mul!(Unit!(1,1, 0,1, 0,1, 0,1, 0,1)).stringof);
	
	Mul!(T) opMul(T)(T that) { Mul!(T) ret; static if(is(T : real)) ret.value = this.value * that; else ret.value = this.value * that.value; return ret; }
	Div!(T) opDiv(T)(T that) { Div!(T) ret; static if(is(T : real)) ret.value = this.value / that; else ret.value = this.value / that.value; return ret; }

	This opMulAssign(real r) { this.value *= r; return *this; }
	This opDivAssign(real r) { this.value /= r; return *this; }
	
	This opMul_r(real that) { This ret; ret.value = this.value * that; return ret; }
	Power!(-1) opDiv_r(real that) { Power!(-1) ret; ret.value = that / this.value; return ret; }
	
	RootT!(i) Root(int i)(){RootT!(i) ret; ret.value = std.math.sqrt(this.value); return ret;} 
	Power!(i) Pow(int i)(){Power!(i) ret; ret.value = std.math.pow(this.value,i); return ret;} 
	This Abs() { This ret; ret.value = std.math.abs(this.value); return ret; }	
}

template Product(T1, T...)
{
	static if(T.length == 0)
		alias T1 Product;
	else
		alias T1.Mul!(Product!(T)) Product;
}

unittest
{
	Unit!(1,1, 0,1, 0,1, 0,1, 0,1) v;  //pragma(msg,"v:\t"~typeof(v).stringof);

	auto v2 = v * v;  //pragma(msg,"v2:\t"~typeof(v2).stringof);
	auto v3 = v2 / v; //pragma(msg,"v3:\t"~typeof(v3).stringof);
	auto v4 = v / v;  //pragma(msg,"v4:\t"~typeof(v4).stringof);
	auto v5 = v4 / v; //pragma(msg,"v5:\t"~typeof(v5).stringof);
	//auto v6a = v2 + v;
	auto v6b = v3 + v;
	auto v7 = v.Root!(2)(); //pragma(msg,"v7:\t"~typeof(v7).stringof);
	auto v8 = v7.Pow!(4)(); //pragma(msg,"v8:\t"~typeof(v8).stringof);
}

