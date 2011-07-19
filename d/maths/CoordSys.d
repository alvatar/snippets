module xf.maths.CoordSys;

private {
	import xf.maths.Vec;
	import xf.maths.Quat;
	import xf.maths.Matrix;
}



/**
	The usage of vec3fi (fixed - based) instead of vec3 (float - based) yields two advantages:
	1. uniform resolution across the whole space
	2. higher precission even than using doubles because CoordSys only uses 'fixed-safe' operations which don't overflow under normal circumstances
*/
struct CoordSys {
	vec3fi	origin;
	quat		rotation;
	
	static const CoordSys identity = { origin: vec3fi.zero, rotation: quat.identity };
	
	
	static CoordSys opCall(vec3fi origin, quat rotation) {
		CoordSys res;
		res.origin = origin;
		res.rotation = rotation;
		return res;
	}
	
	
	CoordSys opIn(CoordSys reference) {
		CoordSys res;
		res.origin = reference.origin;
		res.origin += reference.rotation.xform(this.origin);
		res.rotation = reference.rotation * this.rotation;
		res.rotation.normalize();
		return res;
	}


	CoordSys quickIn(CoordSys reference) {
		CoordSys res;
		res.origin = reference.origin;
		res.origin += reference.rotation.xform(this.origin);
		res.rotation = reference.rotation * this.rotation;
		return res;
	}


	vec3 opIn_r(vec3 v) {
		return rotation.xform(v) + vec3.convert(origin);
	}


	vec3fi opIn_r(vec3fi v) {
		return rotation.xform(v) + origin;
	}
	
	
	quat opIn_r(quat q) {
		return rotation * q;
	}
	
	
	CoordSys worldToLocal(CoordSys global) {
		CoordSys inv = *this;
		inv.invert();
		return global in inv;
	}


	void invert() {
		rotation.invert();
		origin = rotation.xform(-origin);
	}
	
	
	CoordSys inverse() {
		CoordSys res = *this;
		res.invert();
		return res;
	}


	mat4 getMatrix() {
		mat4 res = rotation.getMatrix();
		res.setTranslation(vec3.init.convert(this.origin));		// .init is here to circumvent a bug in dmd .174
		return res;
	}
}
