/**
 * Constant values with units
 */
module constants;

import si;
import unit;
import types;

/// math constants
struct Math
{
	/// Euler's constant
	static const Types.Value gamma = { value : 0.577215664901532 };
	/// good old pi
	static const Types.Value pi    = { value : 3.14159265359};
	/// natural log base
	static const Types.Value e     = { value : 2.718281828459 };
	/// golden ratio
	static const Types.Value phi   = { value : 1.6180339887498948482045868 };
}

/// atomic physics constants
struct Atomic
{
	/// unified atomic mass
	static const Types.Mass   u  = { value : 1.66053873e-27 };
	/// electron mass
	static const Types.Mass   me = { value : 9.109373815276e-31 };
	/// electron charge
	static const Types.Charge e  = { value : 1.602176462e-16 };
	/// Proton mass
	static const Types.Mass   mp = { value : 1.67262158e-27 };
	/// Neutron mass
	static const Types.Mass   mn = { value : 1.67492716e-27 };
	
}

/// bulk physics constants
struct Physical
{
	/// standard gravity
	static const Types.Acceleration g    = { value : 9.81 };
	/// Boltzmann constant
	static const Lit!(2,1,-2,-1,0) k     = { value : 1.3806503e-23 };
	/// electric field constant / permittivity of free space
	static const Lit!(-3,-1,4,0,2) e     = { value : 8.854187817e-12};
	/// Gravitational Constant
	static const Lit!(3,-1,-2,0,0) G     = { value : 6.67259 };
	/// Impedance of free space
	static const Lit!(2,1,-3,0,-2) Z     = { value : 376.731};
	/// speed of light
	static const Types.Speed       c     = { value : 2.99792458e8 };
	/// magnetic field constant / Permeability of a vacuum
	static const Lit!(1,1,-2,0,-1) mu    = { value : 1.2566370614 };
	/// Planck constant
	static const Lit!(2,1,-1,0,0)  h     = { value : 6.62606876e-34 };
	/// Stefan-Boltzmann constant
	static const Lit!(4,-3,1,-4,0) sigma = { value : 5.670400e-8 };
}

/// astronomy constants
struct Astronomical
{
	/// Astronomical unit
	static const Types.Distance AU = { value :149.59787e11 };
}

///
struct Chemistry
{
	/// MassDensity of water (used for specific gravity)
	static const Types.MassDensity rhoH2O = { value : 1000 };
	/// Viscosity of water
	static const Types.Viscosity muH2O = { value : 0.89e-3 };
	/// Vapor pressure of Water at 80F
	static const Types.Pressure PvH2O_80F = { value : 2827 };
	
}