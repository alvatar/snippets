module maths.Stats;

private {
	import xf.maths.Vec;
	import xf.maths.Misc;
	import tango.math.Math;
}



/**
	Generates the uniform sunflower sampling of points on a unit hemisphere
	axis1, axis2, axis3 should be orthonormal axes in R3
	axis3 is the position of the hemisphere's tip
	
	http://www.8ung.at/halcyon/povray/custom-radiosity.html
*/
void generateSunflowerHemisphereSampling(vec3 axis1, vec3 axis2, vec3 axis3, vec3[] samples) {
	float gsa = 2.0 * pi * ((sqrt(5.0) - 1.0) / 2.0);
	float phi = gsa;
	
	real invSC = cast(real)1.0 / samples.length;
	samples[0] = sqrt(invSC) * axis2 + sqrt(1.0 - invSC) * axis3;
	assert (samples[0].x <>= 0);
	assert (samples[0].y <>= 0);
	assert (samples[0].z <>= 0);

	for (uint i = 1; i < samples.length; ++i, phi += gsa) {
		float pr = sqrt(cast(real)i * invSC);
		samples[i] = axis1 * (pr * sin(phi)) + axis2 * (pr * cos(phi)) + axis3 * sqrt(1.0 - pr * pr);
		assert (samples[i].x <>= 0);
		assert (samples[i].y <>= 0);
		assert (samples[i].z <>= 0);
	}
}
