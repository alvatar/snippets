module xf.net.RaknetServer;

private {
	import xf.core.Registry;
	
	import xf.net.RaknetCommon;
	import xf.net.LowLevelServer;
	import xf.net.Misc;
	
	import xf.utils.BitStream;
	import xf.game.Misc;
	
	import xf.utils.ScopedResource;
	import xf.utils.ThreadsafePool;
	import xf.utils.Memory : alloc, free;

	import xf.net.raknet.RakCore;
	import xf.net.raknet.PacketEnumerations;
	import xf.net.raknet.PacketPriority;

	import tango.stdc.stdio : printf;
}



class RaknetServer : LowLevelServer {
	mixin(Implements("LowLevelServer"));
	
	this(int maxPlayers) {
		this.maxPlayers = maxPlayers;
		this.playersConnected.alloc(maxPlayers);
		
		if (rnfGetRakServerInterface is null) {
			bindRaknetFunctions();
		}
	}
	
	
	~this() {
		this.playersConnected.free();
	}

	
	RaknetServer start(char[] addr, int port) {
		impl = rnfGetRakServerInterface();

		if (!rsiStart(impl, maxPlayers, 0, 30, port, (addr ~ \0).ptr)) {
			printf(`RaknetServer failed to start`\n);
			rnfDestroyRakServerInterface(impl);
		} else {
			printf(`RakServer on port %d.`\n, port);
		}
		
		rsiStartOccasionalPing(impl);
		
		return this;
	}
	
	
	RaknetServer stop() {
		return this;
	}
	
	
	mixin MRecvPacket!(StreamFate delegate(playerId, BitStreamReader));


	private void sendImpl(void delegate(BitStreamWriter) dg, playerId target, PacketPriority priority, PacketReliability reliability) {
		getStreamWriter((RakStreamWriter bsw) {
			(cast(BitStreamWriter)bsw)(cast(ubyte)PacketEnumerations.ID_USER_PACKET_ENUM);
			dg(bsw);
			PlayerID player = rsiGetPlayerIDFromIndex(impl, target);
			rsiSendBS(impl, bsw.bs, priority, reliability, 0, player, false);
		});
	}


	private void broadcastImpl(void delegate(BitStreamWriter) dg, bool delegate(playerId) filter, PacketPriority priority, PacketReliability reliability) {
		getStreamWriter((RakStreamWriter bsw) {
			(cast(BitStreamWriter)bsw)(cast(ubyte)PacketEnumerations.ID_USER_PACKET_ENUM);
			dg(bsw);
			
			foreach (i, con; playersConnected) {
				if (con && filter(cast(playerId)i)) {
					PlayerID player = rsiGetPlayerIDFromIndex(impl, i);
					rsiSendBS(impl, bsw.bs, priority, reliability, 0, player, false);
				}
			}
		});
	}
	
	
	void send(void delegate(BitStreamWriter) dg, playerId target) {
		sendImpl(dg, target, PacketPriority.MEDIUM_PRIORITY, PacketReliability.RELIABLE_ORDERED);
	}
	void broadcast(void delegate(BitStreamWriter) dg, bool delegate(playerId) filter) {
		broadcastImpl(dg, filter, PacketPriority.MEDIUM_PRIORITY, PacketReliability.RELIABLE_ORDERED);
	}
	
	void unreliableSend(void delegate(BitStreamWriter) dg, playerId target) {
		sendImpl(dg, target, PacketPriority.HIGH_PRIORITY, PacketReliability.UNRELIABLE);
	}
	void unreliableBroadcast(void delegate(BitStreamWriter) dg, bool delegate(playerId) filter) {
		broadcastImpl(dg, filter, PacketPriority.HIGH_PRIORITY, PacketReliability.UNRELIABLE);
	}
	
	void setPlayerTimeTuning(playerId target, float val) {
		getStreamWriter((RakStreamWriter bsw) {
			(cast(BitStreamWriter)bsw)(cast(ubyte)PacketEnumerations.ID_TUNE_CLIENT_TIMING)(val);
			PlayerID player = rsiGetPlayerIDFromIndex(impl, target);
			rsiSendBS(impl, bsw.bs, PacketPriority.SYSTEM_PRIORITY, PacketReliability.UNRELIABLE_SEQUENCED, 0, player, false);
		});
	}

	
	void kickPlayer(playerId pid) {
		PlayerID player = rsiGetPlayerIDFromIndex(impl, pid);
		rsiKick(impl, player);
	}
	
	
	void registerConnectionHandler(void delegate(playerId) h) {
		connectionHandlers ~= h;
	}
	
	void registerDisconnectionHandler(void delegate(playerId) h) {
		disconnectionHandlers ~= h;
	}


	private StreamFate handlePacket(Packet p, ubyte packetId, BitStream clientData, StreamFate delegate(playerId, BitStreamReader) handler) {
		PlayerID player = ntPacketGetPlayerId(p);
		
		switch(packetId) {
			case PacketEnumerations.ID_DISCONNECTION_NOTIFICATION:
				// Connection lost normally
				printf("ID_DISCONNECTION_NOTIFICATION\n");
				uint index = ntPacketGetPlayerIndex(p);
				playersConnected[index] = false;
				foreach (h; disconnectionHandlers) {
					h(cast(playerId)index);
				}
				//onPlayerDisconnected(cast(peerid) ntPacketGetPlayerIndex(p));
				break;	
			case PacketEnumerations.ID_NEW_INCOMING_CONNECTION:
				// Somebody connected.  We have their IP now
				printf("ID_NEW_INCOMING_CONNECTION\n");
				uint index = rsiGetIndexFromPlayerID(impl, player);
				playersConnected[index] = true;
				foreach (h; connectionHandlers) {
					h(cast(playerId)index);
				}
				//clientID=p->playerId; // Record the player ID of the client
				break;
			case PacketEnumerations.ID_CONNECTION_LOST:
				// Couldn't deliver a reliable packet - i.e. the other system was abnormally terminated
				printf("ID_CONNECTION_LOST\n");
				uint index = ntPacketGetPlayerIndex(p);
				playersConnected[index] = false;
				foreach (h; disconnectionHandlers) {
					h(cast(playerId)index);
				}
				//onPlayerDisconnected(cast(peerid) ntPacketGetPlayerIndex(p));
				break;
			case PacketEnumerations.ID_USER_PACKET_ENUM:
				ubyte playerIndex = rsiGetIndexFromPlayerID(impl, player);
				//printf("ID_USER_PACKET_ENUM\n");
				
				StreamFate res = void;
				getStreamReader(clientData, (RakStreamReader reader) {
					res = handler(cast(playerId)playerIndex, reader);
				});
				return res;
				
				break;
			default:
				printf("Unrecognized packet id: %d\n", cast(int)packetId);
				break;
		}
		
		return StreamFate.Dispose;
	}
	
	
	mixin MStreamGetters;
	
	
	invariant {
		assert (maxPlayers == playersConnected.length);
	}

	private {
		RakServerInterface			impl;
		
		void delegate(playerId)[]	connectionHandlers;
		void delegate(playerId)[]	disconnectionHandlers;
		
		int									maxPlayers;
		bool[]								playersConnected;
	}
}
