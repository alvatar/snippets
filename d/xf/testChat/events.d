private import game.Event;


class LoginRequest : Wish {
	char[]	nick;
	mixin	exposeEvent!(`nick`);
}


class LoginRequestAccepted : Order {
	ubyte	pid;
	mixin	exposeEvent!(`pid`);
}


class LoginRequestRejected : Order {
	char[]	reason;
	mixin	exposeEvent!(`reason`);
}


class PlayerLogin : Order {
	ubyte	pid;
	char[]	nick;
	mixin	exposeEvent!(`pid|nick`);
}


class PlayerDisconnection : Order {
	ubyte	pid;
	mixin	exposeEvent!(`pid`);
}


class OutgoingMsg : Wish {
	char[]	text;
	mixin	exposeEvent!(`text`);
}


class IncomingMsg : Order {
	ubyte	pid;
	char[]	text;
	mixin	exposeEvent!(`pid|text`);
}
