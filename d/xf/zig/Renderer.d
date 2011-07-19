module xf.zig.Renderer;

private {
	import xf.zig.Camera;
	import xf.core.JobHub;
	
	import xf.core.GraphicsHub;
	import xf.dog.Dog;
	
	import xf.utils.Singleton;
	import xf.utils.Array : remove;
}



private class Renderer {
	void initialize() {
		jobHub.addPostFrameJob(&this.render);
	}
	
	
	void register(Renderable r) {
		renderables ~= r;
	}
	
	
	void unregister(Renderable r) {
		renderables.remove(r);
	}
	
	
	void render() {
		if (!camera) return;
		
		use (graphicsHub.context) in (GL gl) {
			gl.Clear(GL_COLOR_BUFFER_BIT);

			camera.setView(gl);
			foreach (r; renderables) {
				r.render(gl);
			}
		};
		
		graphicsHub.context.show();
	}
	
	
	public {
		Camera		camera;
		Renderable[]	renderables;
	}
}


interface Renderable {
	void render(GL);
}



alias Singleton!(Renderer) renderer;
