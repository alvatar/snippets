module xf.maths.Misc;


public {
	import tango.math.Math;
}

private {
	static import std.intrinsic;
}



const real deg2rad	= 0.0174532925199432957692369076848861;
const real rad2deg	= 57.2957795130823208767981548141052;
const real pi			= 3.1415926535897932384626433832795;



/**
	Convert the specfied [x y z] coordinates to spherical [pi theta r]
*/
void cartesianToSpherical(real x, real y, real z, real* phi, real* theta, real* r)
out {
	assert (*phi >= -pi - 0.001 && *phi <= pi + 0.001);
	assert (*theta >= -0.001 && *theta <= pi + 0.001);
}
body {
	*r			= sqrt(x*x+y*y+z*z);
	*phi		= atan2(y, x);
	*theta	= acos(z / *r);
}


/**
	Convert the specfied [x y z] coordinates to spherical [pi theta]
*/
void cartesianToSpherical(real x, real y, real z, real* phi, real* theta)
in {
	real r = sqrt(x*x+y*y+z*z);
	assert (r >= 0.999 && r <= 1.001);
}
out {
	assert (*phi >= -pi - 0.001 && *phi <= pi + 0.001);
	assert (*theta >= -0.001 && *theta <= pi + 0.001);
}
body {
	*phi		= atan2(y, x);
	*theta	= acos(z);
}


/**
	'Nearest' interpolation function
	
	Params:
	t == time, a float from [0, 1]
	a == first value
	b == second value
	res == a place to put the result in
	
	Remarks:
	---
	nearestInterp(< 0.5, a, b, res), res == a
	nearestInterp(>= 0.5, a, b, res), res == b
	---
*/
void nearestInterp(T)(float t, T a, T b, inout T res) {
	if (t >= 0.5f) res = b;
	else res = a;
}


/**
	'Linear' interpolation function
	
	Params:
	t == time, a float from [0, 1]
	a == first value
	b == second value
	res == a place to put the result in
	
	Remarks:
	---
	nearestInterp(0, a, b, res), res == a
	nearestInterp(1, a, b, res), res == b
	---
*/
void linearInterp(T)(float t, T a, T b, inout T res) {
	res = a * (1.f - t) + b * t;
}


/**
	Catmull-rom interpolation function
	
	Params:
	t == time, a float from [0, 1]
	a == first value
	b == second value
	c == third value
	d == fourth value
	res == a place to put the result in
*/
void catmullRomInterp(T)(float t, T a, T b, T c, T d, inout T res) {
	res = .5f * (	(b * 2.f) +
						(c - a) * t +
						(a * 2.f - b * 5.f + c * 4.f - d) * t * t +
						(b * 3.f - c * 3.f + d - a) * t * t * t);
}



void hermiteInterp(T)(float t, T a, T ta, T b, T tb, inout T res) {
	float t2 = t * t;
	float t3 = t2 * t;
	float h1 = 2 * t3 - 3 * t2 + 1;
	float h2 = -2* t3 + 3 * t2;
	float h3 = t3 - 2 * t2 + t;
	float h4 = t3 - t2;
	res = h1 * a + h3 * ta + h2 * b + h4 * tb;
}



uint ceilPowerOf2(uint num) {
	if (0 == num) return 1;
	int msb = std.intrinsic.bsr(num);
	if ((1 << msb) == num) return num;
	return 2 << msb;
}



unittest {
	assert (ceilPowerOf2(2) == 2);
	assert (ceilPowerOf2(3) == 4);
	assert (ceilPowerOf2(4) == 4);
	assert (ceilPowerOf2(123) == 128);
	assert (ceilPowerOf2(1000) == 1024);
	assert (ceilPowerOf2(2048) == 2048);
}



T clamp(T)(T val, T min, T max) {
	if (val >= max) return max;
	if (val <= min) return min;
	return val;
}



float circleAbsDiff(float fullCircle)(float a, float b) {
	while (a < 0.f) a += fullCircle;
	while (b < 0.f) b += fullCircle;
	while (a >= fullCircle) a -= fullCircle;
	while (b >= fullCircle) b -= fullCircle;
	float diff = a - b;
	if (diff < 0.f) diff = -diff;
	
	if (fullCircle - diff < diff) {
		return fullCircle - diff;
	}
	else return diff;
}



float circleDiff(float fullCircle)(float from, float to) {
	float diff = to - from;
	while (diff > fullCircle/2) diff -= fullCircle;
	while (diff < -fullCircle/2) diff += fullCircle;
	return diff;
}


float invSqrt(float x) {
    float xhalf = 0.5f * x;
    int i = *cast(int*)&x;
    i = 0x5f3759df - (i >> 1);
    x = *cast(float*)&i;
    x = x*(1.5f - xhalf * x * x);
    return x;
}
