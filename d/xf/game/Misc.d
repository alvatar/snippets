module xf.game.Misc;


typedef uint		tick;
typedef uint		objId;
typedef ubyte	playerId;


enum : playerId {
	NoAuthority		= playerId.max-1,
	NoPlayer			= NoAuthority,
	ServerAuthority	= playerId.max
}
