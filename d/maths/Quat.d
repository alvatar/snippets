// TODO: remove unnecessary type conversions


module xf.maths.Quat;


private {
	import xf.maths.Vec;
	import xf.maths.Misc;
	import xf.maths.Matrix;
}



struct quatT(flt) {
	union {
		struct {
			vec4.flt x, y, z, w;
		}
		vec4 xyzw;
	}


	const static quatT identity = { x: 0, y: 0, z: 0, w: 1 };


	static quatT opCall(flt x, flt y, flt z, flt w) {
		quatT q;
		q.x = x;
		q.y = y;
		q.z = z;
		q.w = w;
		return q;
	}


	static quatT opCall(vec4 v) {
		quatT q;
		q.xyzw = v;
		return q;
	}


	quatT dup() {
		return quatT(x, y, z, w);
	}
	
	
	flt magnitude() {
		return cast(flt)sqrt(cast(real)x * x + y * y + z * z + w * w);
	}
	
	
	void normalize() {
		flt det = magnitude();
		det = cast(flt)(1.0 / det);
		x *= det;
		y *= det;
		z *= det;
		w *= det;
	}
	
	
	void conjugate() {
		x = -x;
		y = -y;
		z = -z;
	}
	alias conjugate invert;
	

	// Right-handed rotation around vector [1.0; 0.0; 0.0]
	static quatT xRot(real angle) {
		quatT res;
		
		angle *= deg2rad * 0.5;
		res.w = cast(flt)cos(angle);
		res.x = cast(flt)sin(angle);
		res.y = 0;
		res.z = 0;
		
		return res;
	}
	
	
	// Right-handed rotation around vector [0.0; 1.0; 0.0]
	static quatT yRot(real angle) {
		quatT res;
		
		angle *= deg2rad * 0.5;
		res.w = cast(flt)cos(angle);
		res.x = 0;
		res.y = cast(flt)sin(angle);
		res.z = 0;
		
		return res;
	}
	
	// Right-handed rotation around vector [0.0; 0.0; 1.0]
	static quatT zRot(real angle) {
		quatT res;
		
		angle *= deg2rad * 0.5;
		res.w = cast(flt)cos(angle);
		res.x = 0;
		res.y = 0;
		res.z = cast(flt)sin(angle);
		
		return res;
	}
	

	// the axis must be normalized
	static quatT rotAxisAngle(vec3 axis, real angle) {
		debug assert (abs(axis.sqLen - 1.f) < 0.1f);
		
		flt	sinA	= cast(flt)(sin(angle * 0.5 ));
		flt	cosA	= cast(flt)(cos(angle * 0.5 ));
		return quatT(axis.x * sinA, axis.y * sinA, axis.z * sinA, cosA);
	}


	mat4 getMatrix() {
		mat4 res;
		
		flt x2 = x + x;
		flt y2 = y + y; 
		flt z2 = z + z;
		
		flt xx = x * x2;	flt xy = x * y2;	flt xz = x * z2;
		flt yy = y * y2;	flt yz = y * z2;	flt zz = z * z2;
		flt wx = w * x2;	flt wy = w * y2;	flt wz = w * z2;
		
		res[0]  = cast(flt)(1.0 - yy - zz);
		res[1]  = xy + wz;
		res[2]  = xz - wy;
		
		res[4]  = xy - wz;
		res[5]  = cast(flt)(1.0 - xx - zz);
		res[6]  = yz + wx;
		
		res[8]  = xz + wy;
		res[9]  = yz - wx;
		res[10] = cast(flt)(1.0 - xx - yy);
		
		res[3] = res[7] = res[11] = res[12] = res[13] = res[14] = 0;
		res[15] = 1;
		
		return res;
	}
	

	void getAxisAngle(inout vec3 axis, inout flt angle) {
		normalize();
		
		flt cos_a = w;
		angle = cast(flt)(acos( cos_a ) * 2.0);
		flt sin_a = cast(flt)sqrt( 1.0 - cos_a * cos_a );
		
		if ( sin_a < cast(flt)0.0005 && sin_a > cast(flt)-0.0005 ) sin_a = cast(flt)1.0;
		
		axis.x = cast(flt)(x / sin_a);
		axis.y = cast(flt)(y / sin_a);
		axis.z = cast(flt)(z / sin_a);
	}


	// BUG: <h3> doesnt seem to work... gruby ?
	vec3 getDirection() {
		vec3 dir;
		dir.x = cast(flt)( ((x * z) + (w * y)) * 2.f );
		dir.y = cast(flt)( ((y * z) - (w * x)) * 2.f );
		dir.z = cast(flt)( 1.f - ((x * x) + (y * y)) * 2.f);
		return dir;
	}
	

	quatT opMul(quatT rhs) {
		quatT res;
		res.x = w * rhs.x + x * rhs.w + y * rhs.z - z * rhs.y;
		res.y = w * rhs.y + y * rhs.w + z * rhs.x - x * rhs.z;
		res.z = w * rhs.z + z * rhs.w + x * rhs.y - y * rhs.x;
		res.w = w * rhs.w - x * rhs.x - y * rhs.y - z * rhs.z;
		return res;
	}
	

	quatT inverseMult(quatT rhs) {
		return rhs * (*this);
	}


	quatT opMul(float t) {
		quatT res = slerp(quatT.identity, *this, t);
		return res;
	}
	
	
	static quatT slerp(quatT q0, quatT q1, real time) {
		quatT res;
		
		flt scale1;
		flt scale2;
		quatT A = q0;
		quatT B = q1;
		
		// compute dot product, aka cos(theta):
		flt cosTheta = A.x*B.x + A.y*B.y + A.z*B.z + A.w*B.w;
		
		if (cosTheta < 0) {
			// flip 'start' quatT
			A.x = -A.x; A.y = -A.y; A.z = -A.z; A.w = -A.w;
			cosTheta = -cosTheta;
		}
		
		if ((cosTheta + 1) > 0.05) {
			// If the quats are close, use linear interploation
			if ((1.0 - cosTheta) < 0.05) {
				scale1 = cast(flt)(1.0 - time);
				scale2 = cast(flt)time;
			}
			else { 
				// Otherwise, do spherical interpolation
				real theta   = acos(cosTheta);
				real sinTheta = sin(theta);
				scale1 = cast(flt)(sin(theta * (1.0-time)) / sinTheta);
				scale2 = cast(flt)(sin(theta * time) / sinTheta);
			}
		}
		else {
			B.x = -A.y;
			B.y =  A.x;
			B.z = -A.w;
			B.w =  A.z;
			scale1 = cast(flt)(sin(pi * (0.5 - time)));
			scale2 = cast(flt)(sin(pi * time));
		}
		
		res.x = scale1 * A.x + scale2 * B.x;
		res.y = scale1 * A.y + scale2 * B.y;
		res.z = scale1 * A.z + scale2 * B.z;
		res.w = scale1 * A.w + scale2 * B.w;
		
		return res;
	}
	
	
	quatT getDeltaFrom(quatT other) {
		other.invert();
		return other * *this;
	}


	/+char[] toString() {
		return format("[%3.3f, %3.3f, %3.3f, %3.3f]", cast(float)x, cast(float)y, cast(float)z, cast(float)w);
	}+/


	vecT xform(vecT)(vecT rhs) {
		flt a = w * w - (x * x + y * y + z * z);
		flt b = cast(flt)(2.0 * (x * cast(real)rhs.x + y * cast(real)rhs.y + z * cast(real)rhs.z));
		flt c = cast(flt)(2.0 * w);
		
		flt cross[3];
		cross[0] = y * cast(real)rhs.z - z * cast(real)rhs.y;
		cross[1] = z * cast(real)rhs.x - x * cast(real)rhs.z;
		cross[2] = x * cast(real)rhs.y - y * cast(real)rhs.x;
		
		return vecT[
			a * cast(real)rhs.x + b * x + c * cross[0],
			a * cast(real)rhs.y + b * y + c * cross[1],
			a * cast(real)rhs.z + b * z + c * cross[2]
		];
	}


	bool isSameAs(quatT v, float epsilon) {
		foreach (uint i, flt c; xyzw.cell) {
			if (abs(c - v.xyzw.cell[i]) > epsilon) {
				return false;
			}		}
		
		return true;
	}
}



alias quatT!(float) quat;

