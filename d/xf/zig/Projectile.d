module xf.zig.Projectile;

private {
	import xf.zig.GameObj;
	import xf.zig.Phys;
	import xf.net.NetObj;
	import xf.dog.OpenGL;
	import xf.dog.GLWrap;
}



class Projectile : GameObj {
	mixin MNetObj;		// just to impl the interfaces
	

	override void render(GL gl) {
		gl.localMatrix() in {
			gl.Translatef(position.x, position.y, 0);

			gl.withState(GL_LINE_SMOOTH, GL_BLEND) in {
				gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
				gl.LineWidth(1.f);

				gl.immediate(GL_LINE_LOOP, {
					float l = .2f + lifeRemaining / 100.f;
					gl.Color3f(l, l, l);
					float dim = 0.5f;
					
					gl.Vertex2f(-dim, dim);
					gl.Vertex2f(-dim, -dim);
					gl.Vertex2f(dim, -dim);
					gl.Vertex2f(dim, dim);
				});
			};
		};
	}


	this(vec2 pos, vec2 vel, ulong hash, int life) {
		debug printf(`projectile hash: %llu`\n, hash);
		
		this.body_ = new Body(0.7);
		this.body_.userData = cast(void*)this;
		this.body_.mass = 0.1f;
		phys.register(this.body_);

		body_.overrideOrigin(pos);
		body_.overrideVelocity(vel);
		body_.ghostGroup = true;
		this.projectileHash = hash;
		this.lifeRemaining = life;
	}
	
	
	ulong	projectileHash;
	int		lifeRemaining;
}
