module xf.net.LowLevelServer;

private {
	import xf.core.Registry;
	import xf.utils.BitStream;
	import xf.game.Misc;
	import xf.net.Misc;
}



abstract class LowLevelServer {
	mixin(CtorParams = "int");		// max player count
	
	LowLevelServer start(char[] addr, int port);
	LowLevelServer	stop();
	
	bool recvPacket(StreamFate delegate(playerId, BitStreamReader));
	
	void send(void delegate(BitStreamWriter), playerId target);
	void broadcast(void delegate(BitStreamWriter), bool delegate(playerId) filter);
	void unreliableSend(void delegate(BitStreamWriter), playerId target);
	void unreliableBroadcast(void delegate(BitStreamWriter), bool delegate(playerId) filter);
	
	void setPlayerTimeTuning(playerId pid, float val);

	void kickPlayer(playerId pid);
	
	void registerConnectionHandler(void delegate(playerId));
	void registerDisconnectionHandler(void delegate(playerId));
}
