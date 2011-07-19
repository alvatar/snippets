module xf.zig.Beacon;

private {
	import xf.zig.GameObj;
	import xf.zig.Phys;

	import xf.net.NetObj;
	import xf.dog.OpenGL;
	import xf.dog.GLWrap;
	import xf.maths.Vec : vec2;
	import xf.maths.Misc : sin, cos;
}



class Beacon : GameObj {
	mixin MNetObj;
	
	
	override void render(GL gl) {
		gl.localMatrix() in {
			gl.Translatef(position.x, position.y, 0);
			
			gl.withState(GL_LINE_SMOOTH, GL_BLEND) in {
				gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
				gl.LineWidth(1.f);

				gl.immediate(GL_LINE_LOOP, {
					gl.Color3f(1, 1, 1);
					gl.Vertex2f(-1, 1);
					gl.Vertex2f(-1, -1);
					gl.Vertex2f(1, -1);
					gl.Vertex2f(1, 1);
				});
			};
		};
	}
	
	
	void spin() {
		/+curRot += rotSpeed;
		float s = sin(curRot);
		float c = cos(curRot);
		this.position_ = this.centerPos + vec2[s, c] * 7f;
		this.updateBVol();+/
	}
	
	
	override void update() {
		//centerPos += velocity_;
		//this.updateBVol();
		//dampen(0.02);
	}
	

	this() {
		this.body_ = new Body(1.42);
		this.body_.userData = cast(void*)this;
		phys.register(this.body_);
	}


	public {
		vec2	centerPos = vec2.zero;
		float	rotSpeed = 0.f;
		
		float	curRot = 0.f;
	}
}
