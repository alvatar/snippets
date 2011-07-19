module xf.zig.Phys;

private {
	import xf.utils.Singleton;
	import xf.utils.Array : remove, findMin;
	
	import mintl.deque;
	import mintl.mem : Mallocator = Malloc;
	
	import xf.maths.Misc;
	import xf.maths.Vec : vec2;

	extern (C) int printf(char* format, ...);
}



bool extractCollision(T)(Body a, Body b, void delegate(T, Body) h) {
	if (auto t = cast(T)cast(Object)a.userData) {
		h(t, b);
		return true;
	}
	
	if (auto t = cast(T)cast(Object)b.userData) {
		h(t, a);
		return true;
	}
	
	return false;
}


bool extractCollision(T, U)(Body a, Body b, void delegate(T, U) h) {
	if (auto t = cast(T)cast(Object)a.userData) {
		if (auto u = cast(U)cast(Object)b.userData) {
			h(t, u);
			return true;
		}
	}
	
	if (auto t = cast(T)cast(Object)b.userData) {
		if (auto u = cast(U)cast(Object)a.userData) {
			h(t, u);
			return true;
		}
	}
	
	return false;
}


// a circle-based physical body
final class Body {
	public {
		void*	userData;
		float		bounciness = .2f;
		float		mass;
		bool		playerControlled;
		bool		ghostGroup;
	}
	
	
	this(float radius, float mass = 1.f) {
		this.radius_ = radius;
		this.mass = mass;
	}
	
	
	void accelerate(vec2 v) {
		velocity_ += v;
	}
	
	
	void overrideOrigin(vec2 o) {
		origin_ = o;
	}
	
	
	void overrideVelocity(vec2 v) {
		velocity_ = v;
	}
	

	void dampen(float amnt) {
		velocity_ *= 1.f - amnt;
	}
	

	void limitVelocity(float max) {
		float len = velocity_.len;
		if (len > max) {
			velocity_ *= max / len;
		}
	}

	
	vec2	origin()		{ return origin_; }
	float	radius()		{ return radius_; }
	vec2	velocity()	{ return velocity_; }
	
	
	private {
		vec2	origin_;
		float	radius_;
		vec2	velocity_ = {x: 0, y: 0};
		float	timeLeft = 0.f;
	}
}



// sorry, I'm too lazy to make it parallel :P
private final class Phys {
	synchronized void register(Body b) {
		bodies ~= b;
	}
	
	
	synchronized void unregister(Body b) {
		bodies.remove(b, false);
	}
	
	
	synchronized void addStaticLineSegment(vec2 p0, vec2 p1, float thickness) {
		vec2	n = vec2(p0.y - p1.y, p1.x - p0.x).normalized;
		float	d = -n.dot(p0);
		staticLineSegments ~= LineSegment(p0, p1, n, d, (p1 - p0).sqLen, thickness);
	}
	
	
	private synchronized Body updateBody(Body b) {
		auto circle = Circle(b.origin, b.radius, b.radius * b.radius);
		vec2	v = b.velocity;
		
		float	poiDist = float.max;
		vec2	poiNorm = void;
		Body iObj = null;
		
		// find the closest point and its normal
		foreach (ref lseg; staticLineSegments) {
			float pd = void;
			vec2 pn = void;
			
			if (sweepCircleLineSegment(circle, v, lseg, pd, pn)) {
				if (pd < poiDist) {
					poiDist = pd;
					poiNorm = pn;
				}
			}
		}
		
		foreach (b2; bodies) {
			if (b.ghostGroup && b2.ghostGroup) continue;
			
			Ray r = Ray(b.origin, v);
			float rsum = b.radius + b2.radius;
			float vlen = r.d.len;
			if (vlen == 0.f) continue;
			r.d *= 1.f / vlen;
			
			Circle c = Circle(b2.origin, rsum, rsum * rsum);
			float pd = intersectRayCircle(r, c);
			if (float.max == pd) continue;
			
			pd /= vlen;
			
			if (pd < poiDist) {
				poiDist = pd;
				poiNorm = (b.origin + v * pd - b2.origin).normalized;
				iObj = b2;
			}
		}
		
		// no collision
		if (poiDist > b.timeLeft) {
			//printf(`No collision. Closest dist = %f ; v = %f %f`\n, poiDist, v.x, v.y);
			
			b.origin_ += v * b.timeLeft;
			b.timeLeft = 0.f;
			return null;
		} else {
			vec2 incident = v.normalized;

			b.origin_ += v * poiDist;
			b.timeLeft -= poiDist;
			
			if (iObj !is null) {
				// printf(`before: o1 v: %f %f  o2 v: %f %f`\n, b.velocity_.x, b.velocity_.y, iObj.velocity_.x, iObj.velocity_.y);
				
				vec2 push = poiNorm * poiNorm.dot(v);
				b.velocity_ -= push;

				float u1 = push.len;
				
				float m1 = b.mass;
				float m2 = iObj.mass;
				
				vec2 v1 = -poiNorm * (u1 * (m1 - m2) / (m1 + m2));
				vec2 v2 = -poiNorm * (2 * m1 * u1 / (m1 + m2));
				
				b.velocity_ += v1;
				iObj.velocity_ += v2;
				
				foreach (ch; this.collisionHandlers) {
					ch(b, iObj);
				}

				// printf(`after: o1 v: %f %f  o2 v: %f %f`\n, b.velocity_.x, b.velocity_.y, iObj.velocity_.x, iObj.velocity_.y);
			} else {
				v -= poiNorm * poiNorm.dot(v) * (1.f + b.bounciness);		// the slide vector
				v.normalize();
				
				float dampen;
				linearInterp!(float)(abs(incident.dot(poiNorm)), .1f, .3f, dampen);
				
				b.velocity_ = v * b.velocity_.len * (1.f - dampen);

				//printf(`Obj-World collision @ %f`\n, poiDist);
			}
			
			return iObj;
		}
	}
	
	
	synchronized void update() {
		foreach (b; bodies) {
			b.timeLeft = 1.f;
			collQueue.addTail(b);
		}
		
		while (!collQueue.isEmpty) {
			//printf(`processing a body`\n);
			const float	moveEps = 0.0001f;

			Body b = collQueue.takeHead;
			
			if (b.timeLeft > moveEps && b.velocity.sqLen > moveEps) {
				Body b2 = updateBody(b);
				if (b2 && b2.timeLeft > moveEps) {
					collQueue.addHead(b2);
				}
				
				if (b.timeLeft > moveEps) {
					collQueue.addTail(b);
				}
			}
		}
	}
	
	
	/+synchronized void calcCollisions(void delegate(Body, Body) h) {
		foreach (i, bv1; objs) {
			for (int j = i+1; j < objs.length; ++j) {		// yea, shitty O(n*n)  :P
				auto bv2 = objs[j];
				
				if (collide(bv1, bv2)) {
					CollidingPair p;
					p.bv1 = bv1;
					p.bv2 = bv2;
					collPairs.addTail(p);
				}
			}
		}
		
		while (!collPairs.isEmpty) {
			auto first = collPairs.takeHead();
			h(first.bv1, first.bv2);
		}
	}
	

	synchronized void calcCollisions(Body bv1, void delegate(Body) h) {
		foreach (i, bv2; objs) {
			if (collide(bv1, bv2)) {
				h(bv2);
			}
		}
	}+/
	
	
	void addCollisionHandler(void delegate(Body, Body) dg) {
		collisionHandlers ~= dg;
	}

	
	private {
		/+struct CollidingPair {
			Body	bv1;
			Body bv2;
		}+/
		
		//Deque!(CollidingPair, false, Mallocator) collPairs;
		Deque!(Body) collQueue;
		
		Body[]				bodies;
		LineSegment[]	staticLineSegments;
		
		void delegate(Body, Body)[] collisionHandlers;
	}
}


// ------------------------------------
alias Singleton!(Phys)	phys;
// ------------------------------------



private {
	const float colEps = 0.001f;

	struct Ray {
		vec2	o, d;
	}


	struct Circle {
		vec2	o;
		float	r;
		float	r2;
	}
	
	
	struct LineSegment {
		vec2	p0;
		vec2	p1;
		vec2	n;
		float	d;
		float	len2;
		float	thick;
	}
}


float intersectRayCircle(ref Ray ray, ref Circle cir) {
	auto dst = ray.o - cir.o;
	float B = dst.dot(ray.d);
	//if (B > 0.f) return float.max;
	float C = dst.dot(dst) - cir.r2;
	float D = B*B - C;
	float res = D > 0 ? -B - sqrt(D) : float.max;
	return res < -colEps ? float.max : res;
}


float sweepCirclePlane(ref Circle cir, ref vec2 d, ref vec2 planeN, float planeD) {
	planeD -= cir.r;		// move towards the circle by its radius
	// we can now just trace the ray against the plane
	
	float facing = d.dot(planeN);
	if (facing >= 0.f) return float.max;		// moving away from or alongside the plane
	
	float dist = planeN.dot(cir.o) + planeD;
	dist /= -facing;
	
	return dist < 0.f ? float.max : dist;
}


bool sweepCircleLineSegment(ref Circle cir, vec2 d, LineSegment lseg, out float poiDist, out vec2 poiNorm) {
	float extR = cir.r + lseg.thick;
	float extR2 = extR * extR;
	
	vec2 origD = d;
	
	float dlen = d.len;
	if (dlen == 0.f) return false;
	d *= 1.f / dlen;
	
	float[4]	dist;		// first two are vertex/circle intersections
	dist[0] = intersectRayCircle(Ray(cir.o, d), Circle(lseg.p0, cir.r, extR2));
	dist[1] = intersectRayCircle(Ray(cir.o, d), Circle(lseg.p1, cir.r, extR2));
	
	if (dist[0] != float.max) dist[0] /= dlen;
	if (dist[1] != float.max) dist[1] /= dlen;
	
	vec2	planeN		= lseg.n;
	float	planeD		= lseg.d;
	vec2	nplaneN	= -planeN;
	float	nplaneD	= -planeD;
	
	// front facing plane intersection
	dist[2] = sweepCirclePlane(Circle(cir.o, extR, extR2), origD, planeN, planeD);
	dist[3] = sweepCirclePlane(Circle(cir.o, extR, extR2), origD, nplaneN, nplaneD);	

	void pruneLSegCol(inout float dist, vec2 norm) {
		if (float.max == dist) return;
		
		vec2 poi = cir.o + d * dist - norm * extR;
		vec2 v0 = poi - lseg.p0;
		vec2 v1 = lseg.p1 - lseg.p0;
		
		float dot = v0.dot(v1);
		if (dot < 0.f || dot > lseg.len2) {
			dist = float.max;
		}
	}
	
	pruneLSegCol(dist[2], planeN);
	pruneLSegCol(dist[3], nplaneN);
	
	int minI = dist[].findMin();
	float min = dist[minI];

	if (float.max == min) return false;

	poiDist = min;
	
	switch (minI) {
		case 0:		// point 0
			poiNorm = (cir.o + d * min - lseg.p0) / extR;
			return true;

		case 1:		// point 1
			poiNorm = (cir.o + d * min - lseg.p1) / extR;
			return true;
			
		case 2:		// plane front
			poiNorm = planeN;
			return true;
			
		case 3:		// plane back
			poiNorm = nplaneN;
			return true;
			
		default: break;
	}
	
	assert (false, `uh, mem corruption? :S`);
}
