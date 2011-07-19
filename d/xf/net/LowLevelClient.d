module xf.net.LowLevelClient;

private {
	import xf.utils.BitStream;
	import xf.net.Misc;
}



abstract class LowLevelClient {
	LowLevelClient connect(int clientPort, char[] address, int port);
	LowLevelClient disconnect();

	bool recvPacket(StreamFate delegate(BitStreamReader));
	void send(void delegate(BitStreamWriter));
	void unreliableSend(void delegate(BitStreamWriter));
	
	float timeTuning();

	uint averageRTTMillis();

	void registerConnectionHandler(void delegate());
}
