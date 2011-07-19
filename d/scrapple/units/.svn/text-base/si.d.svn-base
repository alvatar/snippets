/**
 * Set of real world units to work with
 */
module si;

import std.string;

import unit;
import rational;

/// generate an encoder and decoder
template Option(char[] name, real v, int l, int m, int t, int T, int i)
{
	const char[] ls = l.stringof;
	const char[] ms = m.stringof;
	const char[] ts = t.stringof;
	const char[] Ts = T.stringof;
	const char[] Is = i.stringof;
	const char[] type = "Unit!("~ls~",1,"~ms~",1,"~ts~",1,"~Ts~",1,"~Is~",1)";
	
	/// unit encoder: stuff the value in the correct type with convertions 
	const char[] Make =
		"static "~type~name~"(real val)"
		"{"~
			type~" ret;"
			"ret._rawSet(val*"~v.stringof~");"
			"return ret;"
		"}"
		"static "~type~name~"()"
		"{"~
			type~" ret;"
			"ret._rawSet("~v.stringof~");"
			"return ret;"
		"}"
		\n;
	
	/// unit decoder: if the unit classes match, create a function that extracts, converts and returns the value.
	const char[] From = 
		"static if(is(T == "~type~"))"
		"{"
			"real "~name~"()"
			"{"
				"return t._rawGet()/"~v.stringof~";"
			"}"
		"}"
		\n;
	//pragma(msg,From);
}

/// unwinds a set of unit defs into an implemntation of encoders or decoders
template BuildSet(char[] suf, T...)
{
	static if(T.length == 0)
		const BuildSet = "";
	else static if (T.length == 1)
		const BuildSet = (mixin(`Option!(`~T[0]~`).`~suf));
	else
		const BuildSet = BuildSet!(suf,T[0..$/2]) ~ BuildSet!(suf,T[$/2..$]);
}

/// The set of unit defs
alias Tpl!( 
	`"value",    1.0, 0,0,0,0,0`,
	`"meter",    1.0, 1,0,0,0,0`,
	`"kilogram", 1.0, 0,1,0,0,0`,
	`"second",   1.0, 0,0,1,0,0`,
	`"kelvin",   1.0, 0,0,0,1,0`,
	`"ampere",   1.0, 0,0,0,0,1`,
	
	/// Distance Unit Types
	`"Angstrom", 1e-10,             1,0,0,0,0`,
	`"Micron",   1e-6,              1,0,0,0,0`,
	`"mm",       1e-3,              1,0,0,0,0`,
	`"cm",       1e-2,              1,0,0,0,0`,
	`"km",       1e3,               1,0,0,0,0`,
	`"inch",     2.54e-2,           1,0,0,0,0`,
	`"foot",     3.04799835e-1,     1,0,0,0,0`,
	`"yard",     9.14399506e-1,     1,0,0,0,0`,
	`"mile",     1.609343130e3,     1,0,0,0,0`,
	`"parsec",   3.085677473598e13, 1,0,0,0,0`,
	
	/// Mass Unit Types
	`"gram",  1e-3,        0,1,0,0,0`,
	`"lb",    4.5359237e-1,0,1,0,0,0`,
	`"Ounce", 2.834952e-2, 0,1,0,0,0`,
	
	/// Time Unit Types
	`"minute", 60.0,    0,0,1,0,0`,
	`"hour",   3600.0,  0,0,1,0,0`,
	`"day",    86400.0, 0,0,1,0,0`,
	
	/// Temperature Unit Types
	`"R", 0.5555555555, 0,0,0,1,0`,
	
	/// Force  Unit Types
	`"Newton", 1.0,           1,1,-2,0,0`,
	`"dyne",   1e-5,          1,1,-2,0,0`,
	`"lbf",    4.44822246806, 1,1,-2,0,0`,
	
	/// Charge Unit Types
	`"Coulomb", 1.0, 0,0,1,0,1`,
	
	/// Magnetic flux density
	`"T", 1.0, 0,1,-2,0,-1`,

	/// magnetic flux
	`"Wb", 1.0, 2,1,-2,0,1`,
	
	/// inductance
	`"H", 1.0, 2,1,-2,0,-2`,
	
	/// Energy Unit Types
	`"Joule", 1.0,              2,1,-2,0,0`,
	`"Erg",   1e-7L,            2,1,-2,0,0`,
	`"cal",   4.1868L,          2,1,-2,0,0`,
	`"eV",    1.602176462e-19L, 2,1,-2,0,0`,
	`"BTU",   1.0550558526e3L,  2,1,-2,0,0`,
	
	/// Voltage Unit Types
	`"Volt", 1.0, 2,1,-3,0,-1`,
	
	/// Frequency Unit Types
	`"Hz", 1.0, 0,0,-1,0,0`,
	
	/// Resistance Unit Types
	`"Ohm", 1.0, -2,-1,3,0,2`,
	
	/// Pressure Unit Types
	`"Pa",  1.0,         -1,1,-2,0,0`,
	`"Bar", 1e5L,        -1,1,-2,0,0`,
	`"Atm", 1.01325e5L,  -1,1,-2,0,0`,
	`"psi", 6.89475729e3,-1,1,-2,0,0`,
	
	/// Viscosity
	//
	`"Poise",  0.1,    -1,1,-1,0,0`,
	//
	`"stokes", 0.0001,  2,0,-1,0,0`,
	
	/// Power Unit Types
	`"Watt", 1.0,          2,1,-3,0,0`,
	`"kWatt", 1000,        2,1,-3,0,0`,
	`"Hp",   745.69987158L,2,1,-3,0,0`,
	
	/// Volume Unit Types
	`"Steres", 1.0,          3,0,0,0,0`,
	`"Liter",  0.001L,       3,0,0,0,0`,
	`"Gal",    0.0037854120L,3,0,0,0,0`,
	`"Cup",    0.0002365883L,3,0,0,0,0`,

	/// volume rate
	`"gpm",    6.30901995e-5L, 3,0,-1,0,0`,
	`"cfm",    4.71947443e-4L, 3,0,-1,0,0`,

	/// Speed Unit Types
	`"mps",   1L,       1,0,-1,0,0`,
	`"Knots", 1.51444L, 1,0,-1,0,0`,
	`"mph",   0.44704L, 1,0,-1,0,0`,
	`"kph",   0.27778L, 1,0,-1,0,0`,
	`"fps",   0.3048L,  1,0,-1,0,0`,
	
	/// Capacitance Unit Types
	`"Farad", 1.0, -2,-1,4,0,2`,
	
	/// Area Unit Types
	`"Acre", 4046.856421L, 2,0,0,0,0`,
	
	/// Acceleration Unit Types
	`"G", 9.80665L, 1,0,-2,0,0`

	) unitsSet;


static if(false)
{
	pragma(msg,BuildSet!("Make", unitsSet));
	pragma(msg,BuildSet!("From", unitsSet));
}


/// Name space to carry Unit encoders 
struct SI
{
	mixin(BuildSet!("Make", unitsSet));

	static Unit!( 0,1, 0,1, 0,1, 1,1, 0,1) degF(real val)
	{
		Unit!( 0,1, 0,1, 0,1, 1,1, 0,1) ret;
		ret._rawSet(val*0.5555555555 + 273.15);
		return ret;
	}

	static Unit!( 0,1, 0,1, 0,1, 1,1, 0,1) degC(real val)
	{
		Unit!( 0,1, 0,1, 0,1, 1,1, 0,1) ret;
		ret._rawSet(val + 273.15);
		return ret;
	}

}

/// Type to build decoders on.
struct To_back(T)
{
	T t;
	mixin(BuildSet!("From", unitsSet));

	static if(is(T == Unit!( 0,1, 0,1, 0,1, 1,1, 0,1)))
	{
		real degF() { return (t._rawGet()-273.15)/0.5555555555; }
		real degC() { return  t._rawGet()-273.15; }
	}

}

/// Wrap a unit in a To_Back and retun it 
To_back!(T) From(T)(T t)
{
	To_back!(T) ret;
	ret.t = t;
	return ret;
}

static if(!false)
{
import std.stdio;
unittest
{
	//pragma(msg, Product!(SI.Meter,SI.Second).Div!(SI.Kilogram).stringof);
	writef("1 meter is %s in\n",From(SI.meter(1)).inch());
	/+
	1 meter is 39.3701 in
	+/
	
	auto force =    SI.lbf(10);
	auto distance = SI.foot(1);
	auto time =     SI.second(10);
	auto energy = force * distance;
	auto power = energy / time;
	auto pressure = force/ distance.Pow!(2);

	writef("force %s N\n",      From(force).Newton);
	writef("energy %s Joule\n", From(energy).Joule);
	writef("power %s Watt\n",   From(power).Watt);
	writef("pressure %s Pa\n",   From(pressure).Pa);
	/+
	force 44.4822 N
	energy 13.5582 Joule
	power 135.582 Watt
	pressure 4.13253 Pa
	+/
}
void main(){}
}