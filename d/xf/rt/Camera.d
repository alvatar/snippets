module xf.rt.Camera;

private {
	import xf.omg.core.LinearAlgebra;
	import xf.omg.rt.Common;
}



class Camera {
	this (vec3 pos, vec3 up, vec3 fwd) {
		this.pos = pos;
		this.up = up.normalized;
		this.fwd = fwd.normalized;
		this.right = cross(this.fwd, this.up);
	}
	
	
	// screen center at 0,0
	Ray ray(float x, float y) {
		return Ray(this.pos, right * x * aspect + up * y + fwd * focalLenMul);
	}
	
	
	void rotate(quat q) {
		up = q.xform(up);
		right = q.xform(right);
		fwd = q.xform(fwd);
	}
	
	
	vec3 pos, up, right, fwd;
	float aspect = 1.33333333f;
	float focalLenMul = 1.f;
}
