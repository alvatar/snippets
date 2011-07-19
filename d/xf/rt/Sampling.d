module xf.rt.Sampling;

private {
	import xf.omg.core.LinearAlgebra;
	import tango.math.random.Kiss;
	import tango.core.Thread;
}


ThreadLocal!(Kiss*) _rng;
static this() {
	_rng = new typeof(_rng);
}

Kiss* rng() {
	auto val = _rng.val();
	if (val is null) {
		val = new Kiss;
		_rng.val = val;
	}
	return val;
}



float frand() {
	return (1.0 / uint.max) * rng.toInt;
}


vec3 vrand() {
	float phi	= frand() * 2 * pi;
	float u		= (frand() - .5f) * 2.f;
	float u2		= u*u;
	float rt		= sqrt(1.f - u2);
	
	return vec3(rt * cos(phi), u, rt * sin(phi));
}


vec3 cosineHemisphericalSample(vec3 normal) {
	while (true) {
		vec3 d = vrand();
		float dot = dot(d, normal);
		if (dot >= frand) return d;
	}
}


/+vec3 ltrand() {		// randomly sample the light
	alias Cornell.light lt;
	vec2 ls = vec2(frand, frand);
	return (lt.v[0] + ((lt.v[1] - lt.v[0]) * ls.x)) * ls.y + (lt.v[3] + ((lt.v[2] - lt.v[3]) * ls.x)) * (1.f - ls.y);
}	+/

