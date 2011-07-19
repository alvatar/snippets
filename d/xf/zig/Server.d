module xf.zig.Server;

private {
	import xf.zig.MainProcess;
	import xf.zig.Events;
	import xf.zig.Game : Game;
	
	import xf.game.Event;
	import xf.game.GameInterface;
	
	import xf.net.GameServer;
	import xf.net.RaknetServer;
	import xf.net.ControlEvents;
	
	import xf.core.JobHub;
	import tango.core.Thread;
}


GameServer		server;



void updateGame() {
	server.advanceTick();
	debug printf(`tick: %d`\n, timeHub.currentTick);
}


void main() {
	auto gameInterface = new GameInterface;
	auto game = new Game!(true)(gameInterface);
	server = new GameServer((new RaknetServer(32)).start(`0.0.0.0`, 8000));
	gameInterface.addObserver(server);

	server.setDefaultWishMask((Wish w) { return cast(LoginRequest)w !is null; });
	server.relevanceCalcFunc = &game.relevanceCalcFunc;
	
	game.setLevel(`levels/ctf3.txt`);
	game.start();
	
	LoginAccepted.addHandler((LoginAccepted e) {
		server.setWishMask(e.pid, null);
	});
	
	JoinGame.addHandler((JoinGame e) {
		server.setStateMask(e.wishOrigin, true);
	});
	
	KickPlayer.addHandler((KickPlayer e) {
		server.kickPlayer(e.pid);
	});
	
	server.registerDisconnectionHandler((playerId pid) {
		PlayerLogout(pid).atTick(0);
	});
	
	jobHub.addRepeatableJob(&updateGame, timeHub.ticksPerSecond);
	jobHub.addPostFrameJob({ Thread.getThis.yield(); });

	jobHub.exec(new MainProcess);
}
