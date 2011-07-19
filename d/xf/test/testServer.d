import game.Event;
import game.GameInterface;
import net.GameServer;
import net.RaknetServer;

import test.TestEvents;
import test.TestClasses;
import tango.core.Thread;

import maths.Vec;

extern (C) int printf(char* format, ...);



void main() {
	auto game = new GameInterface;
	auto server = new GameServer((new RaknetServer(32)).start(`0.0.0.0`, 666));
	game.addObserver(server);
	
	Player player;

	TestWish.addHandler((TestWish e) {
		printf(`server handling a TestWish (%d %f %.*s)`\n, e.a, e.b, e.c);
		
		server.setStateMask(e.wishOrigin, true);
		TestOrder(`zomg gimmeh all your monies!`).immediate();
		CreatePlayer(game.genObjId()).immediate;
	});
	
	CreatePlayer.addHandler((CreatePlayer e) {
		game.register(player = new Player, e.id);
		player.pos = vec3.zero;
	});
	
	while (true) {
		server.advanceTick();
		Thread.getThis.sleep(0.1);
		
		if (player !is null) player.pos += vec3(1.f, 2.f, 3.f);
	}
}
