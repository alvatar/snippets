import game.Event;
import game.GameInterface;
import net.GameServer;
import net.RaknetServer;
import game.TimeHub;

import events;
import std.c.time : usleep;



void main() {
	auto game = new GameInterface;
	auto server = new GameServer((new RaknetServer(32)).start(`0.0.0.0`, 8000));
	
	char[][ubyte]	playersConnected;
	
	bool nickConnected(char[] nick) {
		foreach (pid, n; playersConnected) {
			if (nick == n) return true;
		}
		return false;
	}
	
	LoginRequest.addHandler((LoginRequest e) {
		if (nickConnected(e.nick)) {
			LoginRequestRejected(`nickname in use`).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
		} else {
			printf(`Login request accepted ! *****************************`\n);
			
			LoginRequestAccepted(e.wishOrigin).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
			foreach (rpid, rnick; playersConnected) {
				PlayerLogin(rpid, rnick).filter((playerId pid) { return pid == e.wishOrigin; }).immediate;
			}

			playersConnected[e.wishOrigin] = e.nick;
			PlayerLogin(e.wishOrigin, e.nick).filter((playerId pid) { return pid != e.wishOrigin; }).immediate;
		}
	});

	OutgoingMsg.addHandler((OutgoingMsg e) {
		printf(`server handling an OutgoingMsg (%.*s)`\n, e.text);
		IncomingMsg(e.wishOrigin, e.text).immediate();
	});
	
	server.registerDisconnectionHandler((playerId pid) {
		playersConnected.remove(pid);
		PlayerDisconnection(pid).immediate;
	});
	
	while (true) {
		server.advanceTick();
		printf(`Tick: %d`\n, timeHub.currentTick);
		usleep(100000);
	}
}
