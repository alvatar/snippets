module rt.Surface;

private {
	import maths.Vec;
}



abstract class Surface(Spectrum) {
	// ratio of exiting radiance to incident radiance
	abstract float BRDF(vec3 exiting, vec3 incident);
	
	// 0..1 reflectance at a given spectrum
	abstract float reflectance(Spectrum);
}



abstract class LambertianSurface(Spectrum) : Surface {
	/// exiting and incident vectors are assumed to be unit
	final float BRDF(vec3 exiting, vec3 incident) {
		return -exiting.dot(incident);
	}

	
	abstract float reflectance(Spectrum);
}
