module xf.zig.Camera;

private {
	import xf.zig.GameObj;
	import xf.dog.OpenGL;
	import xf.core.JobHub;
	import xf.maths.Misc : circleDiff;
}



class Camera {
	this() {
		jobHub.addRepeatableJob(&this.update, 30);
	}
	
	
	void update() {
		if (followed is null) return;		
		rotation += circleDiff!(360.f)(this.rotation, followed.rotation) * 0.1f;
	}
	
	
	void follow(GameObj o) {
		followed = o;
	}
	
	
	void setView(GL gl) {
		gl.LoadIdentity();
		gl.Translatef(0, -20, 0);
		gl.Rotatef(-this.rotation, 0, 0, 1);
		gl.Translatef(-followed.position.x, -followed.position.y, 0);
	}
	
	
	GameObj	followed;
	float			rotation = 0.f;
}
