module xf.zig.BVol;

private {
	import xf.maths.Vec : vec2;
}



abstract class BVol {
	public void*	userData;
	vec2				origin;
}



class BSphere : BVol {
	this(float r) {
		this.radius = r;
	}
	
	float	radius;
}


bool collide(BVol a, BVol b) {
	if (auto sa = cast(BSphere)a) {
		if (auto sb = cast(BSphere)b) {
			return collideSphereSphere(sa, sb);
		}
	}
	
	assert (false);
}


bool collideSphereSphere(BSphere a, BSphere b) {
	float dist = (a.origin - b.origin).len;
	if (dist - a.radius - b.radius < 0.f) return true;
	return false;
}
