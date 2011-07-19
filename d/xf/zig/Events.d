module xf.zig.Events;

private {
	import xf.game.Event;
	import xf.game.Misc;
	import xf.maths.Vec : vec2;
}



class LoginRequest : Wish {
	char[]	nick;
	mixin exposeEvent!(`nick`);
}


class JoinGame : Wish {
	mixin exposeEvent;
}


class LoginAccepted : Order {
	playerId	pid;
	char[]		nick;
	objId		shipId;
	mixin exposeEvent!(`pid|nick|shipId`);

	override bool strictTiming() {
		return true;
	}
}


class LoginRejected : Order {
	char[]	reason;
	mixin exposeEvent!(`reason`);
}


class PlayerLogin : Order {
	playerId	pid;
	char[]		nick;
	objId		shipId;
	mixin exposeEvent!(`pid|nick|shipId`);

	override bool strictTiming() {
		return true;
	}
}


class PlayerLogout : Order {
	playerId	pid;
	mixin exposeEvent!(`pid`);
}


class InputWish : Wish {
	enum : ubyte {
		Shoot = 0b1
	}
	
	byte		thrust;
	byte		strafe;
	byte		rot;
	ubyte	action;
	mixin	exposeEvent!(`thrust|strafe|rot|action`);


	override bool logged() {
		return true;
	}
	
	override bool replayed() {
		return true;
	}
}


class ShieldFocusWish : Wish {
	byte		angle8;
	mixin	exposeEvent!(`angle8`);
}


class CreateBeacon : Order {
	objId	id;
	mixin	exposeEvent!(`id`);
}


class CreateProjectile : Order {
	vec2			position;
	vec2			velocity;
	playerId	owner;
	mixin		exposeEvent!(`position|velocity|owner`);
}


class DestroyProjectile : Order {
	ulong	projectileHash;
	mixin	exposeEvent!(`projectileHash`);
}


class CreateLocalProjectile : Local {
	vec2			position;
	vec2			velocity;
	playerId	owner;
	mixin		exposeEvent!(`position|velocity|owner`);
	
	override void rollback() {
		if (destroyProjectileDg !is null) {
			destroyProjectileDg(createdHash);
		}
	}
	
	void delegate(ulong hash)	destroyProjectileDg;
	ulong								createdHash;
}


class CreateExplosion : Order {
	vec2		center;
	float		radius;
	float		strength;
	mixin	exposeEvent!(`center|radius|strength`);
}
