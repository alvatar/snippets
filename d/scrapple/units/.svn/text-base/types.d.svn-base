module types;

import unit;

public template Lit(int l, int m, int t, int T, int I)
{
	alias Unit!(l,1,m,1,t,1,T,1,I,1) Lit;
}


///
public struct Types
{
	alias Lit!( 0, 0, 0, 0, 0) Value;		///
	alias Lit!( 1, 0, 0, 0, 0) Distance;	///
	alias Lit!( 0, 1, 0, 0, 0) Mass;		///
	alias Lit!( 0, 0, 1, 0, 0) Time;		///
	alias Lit!( 0, 0, 0, 1, 0) Temp;		///
	alias Lit!( 0, 0, 0, 0, 1) Current;		///

	alias Lit!(-3, 1, 0, 0, 0) MassDensity;	///

	alias Lit!(-2,-1, 3, 0, 2) Resistance;	///
	alias Lit!(-2,-1, 4, 0, 2) Capacitance;	///

	alias Lit!(-1, 1,-2, 0, 0) Pressure;	///
	alias Lit!(-1, 1,-1, 0, 0) Viscosity;	///

	alias Lit!( 0, 0,-1, 0, 0) Frequency;	///
	alias Lit!( 0, 0, 1, 0, 1) Charge;		///
	alias Lit!( 0, 1,-1, 0, 0) MassFlow;	///

	alias Lit!( 1, 1,-2, 0, 0) Force;		///
	alias Lit!( 1, 0,-2, 0, 0) Acceleration;///
	alias Lit!( 1, 0,-1, 0, 0) Speed;		///

	alias Lit!( 2, 1,-3, 0,-1) Voltage;		///
	alias Lit!( 2, 1,-3, 0, 0) Power;		///
	alias Lit!( 2, 1,-2, 0,-2) Inductance;	///
	alias Lit!( 2, 1,-2, 0, 0) Energy;		///
	alias Lit!( 2, 0, 0, 0, 0) Area;		///
	alias Lit!( 2, 0,-1, 0, 0) KViscosity;	///

	alias Lit!( 3, 0,-1, 0, 0) VolumeFlow;	///
	alias Lit!( 3, 0, 0, 0, 0) Volume;		///
}

public struct Engr
{
	public struct Thermal
	{
		alias Lit!( 2, 0,-2,-1, 0) ThermalCap;		///
		alias Lit!( 1, 1,-3,-1, 0) ThermalCond;		///
		alias Lit!( 0, 1,-3,-1, 0) ConvectionCoeff;	///
	}
}
