module xf.maths.Plane;

private {
	import xf.maths.Vec;
}



struct PlaneT(flt) {
	alias vec!(flt, 3, 0, flt.sizeof, flt.sizeof*2, flt.sizeof*3)	vec3;
	
	union {
		struct {
			flt a, b, c;
		}
		
		vec3 normal;
	}
	
	flt d;
	
	
	static PlaneT opCall(flt a, flt b, flt c, flt d) {
		PlaneT res;
		res.a = a;
		res.b = b;
		res.c = c;
		res.d = d;
		return res;
	}


	static PlaneT opCall(vec3 normal, flt d) {
		PlaneT res;
		res.a = normal.x;
		res.b = normal.y;
		res.c = normal.z;
		res.d = d;
		return res;
	}
	
	
	static PlaneT fromNormalPoint(vec3 normal, vec3 point) {
		PlaneT res;
		res.normal = normal;
		res.d = -normal.dot(point);
		return res;
	}
	
	
	float dist(ref vec3 pt) {
		return pt.dot(normal) + d;
	}
	
	
	vec3 point() {
		return normal * -d;
	}
}


alias PlaneT!(float)		Plane;
alias PlaneT!(double)	Planed;
