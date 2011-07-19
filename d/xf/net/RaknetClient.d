module xf.net.RaknetClient;

private {
	import xf.core.Registry;
	
	import xf.net.RaknetCommon;
	import xf.net.LowLevelClient;
	import xf.net.Misc;
	
	import xf.utils.BitStream;

	import xf.utils.ScopedResource;
	import xf.utils.ThreadsafePool;

	import xf.net.raknet.RakCore;
	import xf.net.raknet.PacketEnumerations;
	import xf.net.raknet.PacketPriority;

	import tango.stdc.stdio : printf;
}



class RaknetClient : LowLevelClient {
	mixin(Implements("LowLevelClient"));
	
	this() {
		if (rnfGetRakClientInterface is null) {
			bindRaknetFunctions();
		}
	}
	
	
	RaknetClient connect(int clientPort, char[] serverAddress, int serverPort) {
		if (impl !is null) {
			rnfDestroyRakClientInterface(impl);
			impl = null;
		}
		
		impl = rnfGetRakClientInterface();
        this.clientPort = clientPort;
        this.serverPort = serverPort;
        this.serverAddress = serverAddress ~ \0;
		
		rciSetTimeoutTime(impl, 5000);
		
		if (!rciConnect(impl, this.serverAddress.ptr, serverPort, clientPort, 0, 0)) {
            printf(`Connection failed`\n);
			rnfDestroyRakClientInterface(impl);
			impl = null;
        }
        else {
            printf(`Attempting to connect to: %s on port: %d`\n, this.serverAddress.ptr, serverPort);
        }

		return this;
	}
	
	
	RaknetClient disconnect() {
		return this;
	}
	
	
	mixin MRecvPacket!(StreamFate delegate(BitStreamReader));
	
		
	void send(void delegate(BitStreamWriter) dg) {
		getStreamWriter((RakStreamWriter bsw) {
			assert (bsw !is null);
			(cast(BitStreamWriter)bsw)(cast(ubyte)PacketEnumerations.ID_USER_PACKET_ENUM);
			dg(bsw);
			debug printf(`client sending a packet`\n);
			rciSendBS(impl, bsw.bs, PacketPriority.MEDIUM_PRIORITY, PacketReliability.RELIABLE_ORDERED, 0);
		});
	}


	void unreliableSend(void delegate(BitStreamWriter) dg) {
		getStreamWriter((RakStreamWriter bsw) {
			assert (bsw !is null);
			(cast(BitStreamWriter)bsw)(cast(ubyte)PacketEnumerations.ID_USER_PACKET_ENUM);
			dg(bsw);
			debug printf(`client sending a packet`\n);
			rciSendBS(impl, bsw.bs, PacketPriority.HIGH_PRIORITY, PacketReliability.UNRELIABLE, 0);
		});
	}


	void registerConnectionHandler(void delegate() h) {
		connectionHandlers ~= h;
	}


	uint averageRTTMillis() {
		return 0;
	}
	

	private StreamFate handlePacket(Packet p, ubyte packetId, BitStream clientData, StreamFate delegate(BitStreamReader) handler) {
		switch(packetId) {
			case PacketEnumerations.ID_DISCONNECTION_NOTIFICATION:
				// Connection lost normally
				//printf("ID_DISCONNECTION_NOTIFICATION\n");
				//onPlayerDisconnected(cast(peerid) ntPacketGetPlayerIndex(p));
				break;	
			case PacketEnumerations.ID_CONNECTION_REQUEST_ACCEPTED:
				// Somebody connected.  We have their IP now
				//printf("ID_CONNECTION_REQUEST_ACCEPTED\n");
				if (impl !is null) rciStartOccasionalPing(impl);
				foreach (h; connectionHandlers) {
					h();
				}
				//clientID=p->playerId; // Record the player ID of the client
				break;
			case PacketEnumerations.ID_CONNECTION_LOST:
				// Couldn't deliver a reliable packet - i.e. the other system was abnormally terminated
				printf(`ID_CONNECTION_LOST`\n);
				//onPlayerDisconnected(cast(peerid) ntPacketGetPlayerIndex(p));
				break;
			case PacketEnumerations.ID_USER_PACKET_ENUM:
				//printf("ID_USER_PACKET_ENUM\n");
				
				StreamFate res = void;
				getStreamReader(clientData, (RakStreamReader reader) {
					res = handler(reader);
				});
				return res;
				
				break;
			case PacketEnumerations.ID_TUNE_CLIENT_TIMING:
				float val = 0.f;
				bsReadFloat(clientData, &val);
				_timeTuning = val;
				break;
			default:
				//printf("Unrecognized packet id: %d\n", cast(int)packetId);
				break;
		}
		
		return StreamFate.Dispose;
	}
	
	
	override float timeTuning() {
		return _timeTuning;
	}


	mixin MStreamGetters;	
	
	
	private {
		RakClientInterface impl;
		
		int		clientPort;
		int		serverPort;
		char[]	serverAddress;
		
		float		_timeTuning = 0.f;

		void delegate()[]	connectionHandlers;
	}
}
