module xf.net.ControlEvents;

private {
	import xf.game.Event;
	import xf.game.Misc;
}


abstract class ImmediateEvent : Order {}


class AdjustTick : ImmediateEvent {
	tick		serverTick;
	mixin	MEvent;//!(`serverTick`);
}


class KickPlayer : Local {
	playerId	pid;
	mixin		MEvent;//!(`pid`);
	
	override void rollback() {};
}


class DestroyObject : Order {
	objId	id;
	mixin	MEvent;//!(`id`);
}


class TuneClientTiming : Local {
	playerId	pid;
	float			tickOffset;		// the client should be running this many ticks more forward
	mixin		MEvent;//!(`pid|tickOffset`);
	
	override void rollback() { assert(false); };		// should only be created on server side
}
