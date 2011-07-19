module xf.zig.Ship;

private {
	import xf.zig.GameObj;
	import xf.zig.Phys;
	import xf.zig.Shield;
	import xf.net.NetObj;
	import xf.dog.OpenGL;
	import xf.dog.GLWrap;
	import xf.maths.Misc;
	
	extern (C) int printf(char* format, ...);
}



private class ShipGameObj : GameObj {
	mixin MNetObj;
}


struct ShieldState {
	const float importance = 0.25f;		// hint for the server's prioritized snapshots
	const bool critical = false;
	
	float		energy = 100.f;
	float[8]	focus = 1.f;

	float compare(ref ShieldState rhs) {
		float sum = 0.f;
		foreach (i, f; focus) sum += abs(f - rhs.focus[i]) / 10.f;
		return sum + abs(energy - rhs.energy) / 100.f;
	}
}


class Ship : ShipGameObj, NetObj!(ShieldState) {
	mixin MNetObj;
	

	protected {
		alias ShipGameObj.setState setState;
		void setState(ShieldState st, tick) {
			this.shield.energy = st.energy;
			this.shield.focus[] = st.focus[];
		}
		
		
		alias ShipGameObj.getState getState;
		void getState(ShieldState* st) {
			st.energy = this.shield.energy;
			st.focus[] = this.shield.focus[];
		}
	}

	
	override void render(GL gl) {
		gl.localMatrix() in {
			gl.Translatef(position.x, position.y, 0);
			gl.Rotatef(rotation, 0, 0, 1);
			
			gl.withState(GL_LINE_SMOOTH, GL_BLEND) in {
				gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
				gl.LineWidth(1.f);
				
				gl.immediate(GL_LINE_LOOP, {
					gl.Color3f(1, 1, 1);
					gl.Vertex2f(-1, -1.5);
					gl.Vertex2f(1, -1.5);
					gl.Vertex2f(0, 1.5);
				});
			};

			shield.render(gl);
		};
	}
	
	
	override void update() {
		rotation_ += angVelocity_;
		limitVelocity(0.7f);
		dampen(0.01f);
		dampenRot(0.04f);
		shield.update();
	}
	
	
	void hit(float dmg, vec2 pos) {
		vec2 off = pos - this.position;
		float angle = rad2deg * atan2(off.y, off.x) - 90;
		angle -= this.rotation;
		while (angle > 360.f) angle -= 360.f;
		while (angle < 0.f) angle += 360.f;
		printf(`Hit at %1.1f deg; `, angle);
		printf(`dmg: %f`, dmg);
		float shieldDmgMult = (1.f / shield.calcMult(angle));
		dmg = shield.takeDamage(shieldDmgMult * dmg);
		dmg /= shieldDmgMult;
		printf(`, hull should take %f pts of damage`\n, dmg);
	}
	
	
	this() {
		float radius = 2.f;
		this.shield = new Shield(radius);
		
		this.body_ = new Body(radius);
		this.body_.bounciness = .4f;
		this.body_.mass = 1.f;
		this.body_.userData = cast(void*)this;
		this.body_.playerControlled = true;
		phys.register(this.body_);

		this.body_.overrideOrigin(vec2(0, -40));
	}


	public {
		Shield	shield;
	}
}
