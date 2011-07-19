import game.Event;
import game.GameInterface;
import net.GameClient;
import net.RaknetClient;

import test.TestEvents;
import test.TestClasses;
import tango.core.Thread;

extern (C) int printf(char* format, ...);



void main() {
	auto game = new GameInterface;
	auto client = new GameClient((new RaknetClient).connect(0, `localhost`, 666));
	game.addObserver(client);
	
	Player player;

	TestWish.addHandler((TestWish e) {
		printf(`client handling a TestWish`\n);
	});
	
	TestOrder.addHandler((TestOrder e) {
		printf(`client handling a TestOrder(%.*s)`\n, e.str);
	});
	
	CreatePlayer.addHandler((CreatePlayer e) {
		game.register(player = new Player, e.id);
	});
	
	client.registerConnectionHandler({
		RegisterEventOrder(1, `TestWish`).immediate();
		TestWish(2, 3.3f, `lolz another one`).immediate();
	});
	
	while (true) {
		client.receiveData();
		timeHub.advanceTick();
		if (player !is null) printf(`* player.pos = %f %f %f`\n, player.pos.x, player.pos.y, player.pos.z);
		Thread.getThis.sleep(0.1);
	}
}
