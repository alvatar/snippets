module xf.zig.GameObj;

private {
	import xf.zig.Renderer;
	import xf.zig.Phys;

	import xf.net.NetObj;
	import xf.maths.Vec : vec2;
	import xf.maths.Misc : circleAbsDiff, abs, deg2rad, rad2deg, sin, cos;
	import xf.dog.OpenGL;
}



struct PosVelState {
	vec2	pos	= vec2.zero;
	vec2	vel	= vec2.zero;
	
	float compare(ref PosVelState rhs) {
		return (pos - rhs.pos).sqLen + (vel - rhs.vel).sqLen;
	}
}


struct RotState {
	const float importance = 0.5f;		// hint for the server's prioritized snapshots
	
	// shall be in degrees. [0; 360)
	float rot			= 0.f;
	float angVel	= 0.f;	// degrees per second

	float compare(ref RotState rhs) {
		return (circleAbsDiff!(360.f)(rot, rhs.rot) + abs(angVel - rhs.angVel)) / 180.f;
	}
}


abstract class GameObj : NetObj!(PosVelState, RotState), Renderable {
	this() {
		renderer.register(this);
	}
	
	
	final vec2 position() {
		return body_.origin;
	}
	
	
	final vec2 velocity() {
		return body_.velocity;
	}
	
	
	final float rotation() {
		return rotation_;
	}
	
	
	final float angVelocity() {
		return angVelocity_;
	}
	
	
	vec2 direction() {
		float r = deg2rad * rotation;
		return vec2(-sin(r), cos(r));
	}
	
	
	void setState(PosVelState st, tick) {
		body_.overrideOrigin(st.pos);
		body_.overrideVelocity(st.vel);
		//updateBVol();
	}
	
	
	void getState(PosVelState* st) {
		st.pos = body_.origin;
		st.vel = body_.velocity;
	}
	

	void setState(RotState st, tick) {
		this.rotation_ = st.rot;
		this.angVelocity_ = st.angVel;
	}
	
	
	void getState(RotState* st) {
		st.rot = this.rotation_;
		st.angVel = this.angVelocity_;
	}

	
	void accelerate(vec2 v) {
		float cosr = cos(deg2rad * this.rotation);
		float sinr = sin(deg2rad * this.rotation);
		body_.accelerate(vec2(cosr * v.x, sinr * v.x) + vec2(-sinr * v.y, cosr * v.y));
	}
	
	
	void rotate(float angV) {
		angVelocity_ += angV;
	}
	
	
	void update() {
		rotation_ += angVelocity_;
	}
	
	
	void dampen(float amnt) {
		body_.dampen(amnt);
	}
	

	void dampenRot(float amnt) {
		angVelocity_ *= 1.f - amnt;
	}

	
	void limitVelocity(float max) {
		body_.limitVelocity(max);
	}
	
	
	abstract void render(GL gl);
	
	
	void dispose() {
		renderer.unregister(this);

		this.body_.userData = null;
		phys.unregister(this.body_);

		delete this;
	}
	
	
	protected {
		float	rotation_		= 0.f;
		float	angVelocity_	= 0.f;
		Body	body_;
	}
}
