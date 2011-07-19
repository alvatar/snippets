module test.TestClasses;

private {
	import maths.Vec;
	import maths.Quat;
	import utils.BitStream;
	import game.Misc;

	extern (C) int printf(char* format, ...);
}

public {
	import net.NetObj;
}



// network streaming functions generated automatically
struct PositionState {
	vec3	pos = vec3.zero;
	quat	rot = quat.identity;
	
	float compare(ref typeof(*this) rhs) {
		return (pos - rhs.pos).sqLen + rot.xyzw.sqLen;
	}
}


// network streaming functions generated automatically
struct AnimState {
	const float importance = 0.5f;
	
	float time;

	float compare(ref typeof(*this) rhs) {
		return abs(time - rhs.time) * 0.1f;
	}
}


class Player : NetObj!(PositionState, AnimState) {
	mixin MNetObj;		// state queues and stuff
	
	
	void getState(PositionState* ps) {
		printf(`get PositionState / pos = %f %f %f`\n, pos.x, pos.y, pos.z);
		ps.pos = this.pos;
	}
	
	void setState(PositionState ps, tick tck) {
		this.pos = ps.pos;
		printf(`set PositionState / pos = %f %f %f`\n, pos.x, pos.y, pos.z);
	}


	void getState(AnimState* ps) {
	}
	
	void setState(AnimState ps, tick tck) {
	}
	
	
	void dispose() {
		delete this;
	}
	
	
	vec3 pos = vec3.zero;
}
