module xf.maths.AABB;

private {
	import xf.maths.Vec;
}



/// Axis Aligned Bounding Box
struct AABBT(flt) {
	alias vec!(flt, 3, 0, flt.sizeof, flt.sizeof*2, flt.sizeof*3)	vec3;

	vec3 min	= { x: flt.max, y: flt.max, z: flt.max };
	vec3 max	= { x: -flt.max, y: -flt.max, z: -flt.max };
	
	flt width()	{ return max.x - min.x; }
	flt height()	{ return max.y - min.y; }
	flt depth()	{ return max.z - min.z; }
	
	
	static AABBT opCall(vec3 min, vec3 max) {
		AABBT result;
		result.min = min;
		result.max = max;
		return result;
	}
	
	
	static AABBT opCall() {
		AABBT result;
		return result;
	}
	
	
	void expand(AABBT b) {
		if (min.x > b.min.x) min.x = b.min.x;
		if (min.y > b.min.y) min.y = b.min.y;
		if (min.z > b.min.z) min.z = b.min.z;
		if (max.x < b.max.x) max.x = b.max.x;
		if (max.y < b.max.y) max.y = b.max.y;
		if (max.z < b.max.z) max.z = b.max.z;
	}
	

	void expand(vec3 v) {
		if (v.x > max.x) max.x = v.x;
		if (v.y > max.y) max.y = v.y;
		if (v.z > max.z) max.z = v.z;
		if (v.x < min.x) min.x = v.x;
		if (v.y < min.y) min.y = v.y;
		if (v.z < min.z) min.z = v.z;
	}
	

	bool intersect(AABBT box) {
		return	(min.x < box.max.x && max.x > box.min.x) &&
					(min.y < box.max.y && max.y > box.min.y) &&
					(min.z < box.max.z && max.z > box.min.z);
	}
	
	
	vec3 halfSize() {
		return (max - min) * 0.5;
	}
	
	
	vec3 size() {
		return max - min;
	}
	
	
	flt area() {
		vec3 s = max - min;
		return 2.0 * (s.x * s.y + s.x * s.z + s.y * s.z);
	}
	
	
	vec3 center() {
		return (max + min) * 0.5;
	}
	
	
	void getVertices(vec3[] verts) {
		verts[0] = vec3(min.x, min.y, min.z);
		verts[1] = vec3(min.x, min.y, max.z);
		verts[2] = vec3(min.x, max.y, min.z);
		verts[3] = vec3(min.x, max.y, max.z);
		verts[4] = vec3(max.x, min.y, min.z);
		verts[5] = vec3(max.x, min.y, max.z);
		verts[6] = vec3(max.x, max.y, min.z);
		verts[7] = vec3(max.x, max.y, max.z);
	}
}

alias AABBT!(float)		AABB;
alias AABBT!(double)	AABBd;
