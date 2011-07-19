module xf.utils.SimpleCamera;

private {
	import xf.input.Input;
	import xf.omg.core.LinearAlgebra : mat4, vec3, vec2, quat;
}



class SimpleCamera {
	this(vec3 pos, float pitch, float yaw, InputChannel ch) {
		keyboard = new SimpleKeyboardReader(ch);
		auto mouse = this.new MouseReader;
		ch.addReader(mouse);
		this.pos = pos;
		this.pitch = pitch;
		this.yaw = yaw;
	}
	
	
	void update(float seconds) {
		vec3 move = vec3.zero;
		
		if (keyboard.keyDown(KeySym.w)) {
			move.z -= 1.f * movementSpeed.z;
		}
		if (keyboard.keyDown(KeySym.s)) {
			move.z += 1.f * movementSpeed.z;
		}
		if (keyboard.keyDown(KeySym.a)) {
			move.x -= 1.f * movementSpeed.x;
		}
		if (keyboard.keyDown(KeySym.d)) {
			move.x += 1.f * movementSpeed.x;
		}
		
		pos += rot.xform(move) * seconds;
	}
	
	
	mat4 getMatrix() {
		quat r = rot.inverse;
		vec3 p = r.xform(-pos);
		auto res = r.toMatrix!(4, 4)();
		res.setTranslation(p);
		return res;
	}
	
	
	vec3 position() {
		return pos;
	}
	
	
	quat orientation() {
		return rot;
	}
	

	class MouseReader : InputReader {
		void handle(MouseInput* i) {
			this.outer.pitch -= i.move.y * mouseSensitivity.y;
			this.outer.yaw -= i.move.x * mouseSensitivity.x;
			this.outer.rot = quat.yRotation(this.outer.yaw) * quat.xRotation(this.outer.pitch);
		}
	   
		this() {
			registerReader!(MouseInput)(&this.handle);
		}
	}
	
	
	public {
		vec2	mouseSensitivity = { x: 0.2f, y: 0.2f };
		vec3	movementSpeed = vec3.one;
	}
	
	private {
		float	pitch = 0.f;
		float	yaw = 0.f;
		vec3	pos = vec3.zero;
		quat	rot = quat.identity;
		
		SimpleKeyboardReader	keyboard;
	}
}
