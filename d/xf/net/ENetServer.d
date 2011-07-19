module xf.net.ENetServer;

private {
	import xf.core.Registry;
	
	import tango.stdc.stringz;
	import tango.stdc.stdlib;

	import xf.utils.BitStream;
	import xf.utils.GenericBitStream;
	import xf.game.Misc;
	import xf.net.Misc;
	import xf.net.LowLevelServer;

	import enet;

	import tango.stdc.stdio : printf;
}

class ENetServer : LowLevelServer {
	mixin(Implements("LowLevelServer"));
	
	enum {
		DataChannel = 0,
		TimeTuningChannel = 1
	}


	this(size_t maxPlayers) {
		this.maxPlayers = maxPlayers;
		playersConnected.length = maxPlayers;
		for(size_t i = 0; i < maxPlayers; i++) {
			playersConnected[i] = false;
		}
	}

	~this() {
		if(running) {
			stop();
		}
	}

	
	ENetServer start(char[] addr, int port) {
		if(running) {
			printf("Attempted to start an already-running server.\n");
			return this;
		}

		ENetAddress bindTo;

		enet_address_set_host(&bindTo, toStringz(addr));
		bindTo.port = port;
		server = enet_host_create(&bindTo,
								  maxPlayers,
								  0, // Incoming bandwidth limit
								  0  // Outgoing bandwidth limit
			);
		if(!server) {
			printf("enet failed to start.\n");
		} else {
			printf("Server started.\n");
			running = true;
		}
		
		return this;
	}
	
	ENetServer stop() {
		if(!running) {
			printf("Attempted to stop a server which was not running.\n");
			return this;
		}
		running = false;
		enet_host_destroy(server);
		return this;
	}
	

	// Handle zero or one packet(s).  Will not necessarily call dg,
	// irrespective of return value.  Returns true if a packet was
	// handled, false if no packets available.
	bool recvPacket(StreamFate delegate(playerId, BitStreamReader) dg) {
		bool handlePacket(playerId id, BitStreamReader bsr, ENetPacket* pkt) {
			auto fate = dg(id, bsr);
			if(fate == StreamFate.Dispose) {
				enet_packet_destroy(pkt);
				delete bsr;
				return true;
			} else {
				assert(retainedBsr is null, "Cannot retain multiple packets.");
				
				retainedId = id;
				retainedBsr = bsr;
				retainedPkt = pkt;
				return false;
			}
		}

		if(retainedBsr !is null) {
			auto recvId = retainedId;
			auto recvBsr = retainedBsr;
			auto recvPkt = retainedPkt;
			retainedId = 0;
			retainedBsr = null;
			retainedPkt = null;
			if(!handlePacket(recvId, recvBsr, recvPkt)) {
				return false;
			}
		}
		
		ENetEvent ev;
		if(enet_host_service(server, &ev, 0)) {
			//printf("Got event.\n");
			// We got an event!
			switch(ev.type) {
			case ENetEventType.ENET_EVENT_TYPE_CONNECT:
				ev.peer.data = malloc(playerId.sizeof);
				auto id = getFreeID();
				*(cast(playerId*)(ev.peer.data)) = id;
				playersConnected[id] = true;
				peers[*(cast(playerId*)(ev.peer.data))] = ev.peer;

				printf("%d connected\n", *(cast(playerId*)(ev.peer.data)));
				
				foreach(handler; connHandlers) {
					handler(*(cast(playerId*)(ev.peer.data)));
				}
				break;

			case ENetEventType.ENET_EVENT_TYPE_DISCONNECT:
				printf("%d disconnected\n", *(cast(playerId*)(ev.peer.data)));
				
				foreach(handler; disconnHandlers) {
					handler(*(cast(playerId*)(ev.peer.data)));
				}

				peers.remove(*(cast(playerId*)(ev.peer.data)));
				free(ev.peer.data);
				ev.peer.data = null;
				break;

			case ENetEventType.ENET_EVENT_TYPE_RECEIVE:
				//printf("Recieved a packet from %d.\n", *(cast(playerId*)(ev.peer.data)));
				auto bsr = new GenericBitStreamReader(ev.packet.data[0..ev.packet.dataLength]);
				if(!handlePacket(*(cast(playerId*)(ev.peer.data)), bsr, ev.packet)) {
					return false;
				}
				break;

			default:
				printf("WARNING: Received unsupported event.\n");
				break;
			}
			return true;
		}
		return false;
	}

	
	void send(void delegate(BitStreamWriter) writer, playerId target) {
		sendImpl(writer, target, ENetPacketFlag.ENET_PACKET_FLAG_RELIABLE);
	}
	void broadcast(void delegate(BitStreamWriter) writer, bool delegate(playerId) filter) {
		broadcastImpl(writer, filter, ENetPacketFlag.ENET_PACKET_FLAG_RELIABLE);
	}
	void unreliableSend(void delegate(BitStreamWriter) writer, playerId target) {
		sendImpl(writer, target, ENetPacketFlag.ENET_PACKET_FLAG_UNSEQUENCED);
	}
	void unreliableBroadcast(void delegate(BitStreamWriter)writer , bool delegate(playerId) filter) {
		broadcastImpl(writer, filter, ENetPacketFlag.ENET_PACKET_FLAG_UNSEQUENCED);
	}

	
	void setPlayerTimeTuning(playerId pid, float val) {
		auto pkt = enet_packet_create(&val, float.sizeof, 0 /* unreliable sequenced */);
		enet_peer_send(peers[pid], TimeTuningChannel, pkt);
	}

	
	void kickPlayer(playerId pid) {
		enet_peer_disconnect(peers[pid], 0);
	}

	
	void registerConnectionHandler(void delegate(playerId) dg) {
		connHandlers ~= dg;
	}
	
	void registerDisconnectionHandler(void delegate(playerId) dg) {
		disconnHandlers ~= dg;
	}

protected:
	ENetHost* server;
	size_t maxPlayers;
	bool running = false;

	void delegate(playerId)[] connHandlers;
	void delegate(playerId)[] disconnHandlers;

	bool[] playersConnected;
	ENetPeer*[playerId] peers;

	playerId getFreeID() {
		foreach(id, connected; playersConnected) {
			if(!connected) {
				return cast(playerId)id;
			}
		}
		printf("Failed to find a free ID.\n");
		return 0;
	}

	void sendImpl(void delegate(BitStreamWriter) writer, playerId target, uint flags) {
		auto bsw = new GenericBitStreamWriter;
		writer(bsw);
		auto buf = bsw.data;
		// TODO: Confirm if this flag is what we want
		auto pkt = enet_packet_create(buf.ptr, buf.length, flags);
		//printf("Sending a packet.\n");
		enet_peer_send(peers[target], DataChannel, pkt);
	}

	void broadcastImpl(void delegate(BitStreamWriter) writer, bool delegate(playerId) filter, uint flags) {
		auto bsw = new GenericBitStreamWriter;
		writer(bsw);
		auto buf = bsw.data;
		// TODO: Confirm if this flag is what we want
		auto pkt = enet_packet_create(buf.ptr, buf.length, flags);
		foreach(id, peer; peers) {
			if(filter(id)) {
				//printf("Sending a packet.\n");
				enet_peer_send(peer, DataChannel, pkt);
			}
		}
	}

private:
	ENetPacket* retainedPkt;
	BitStreamReader retainedBsr;
	playerId retainedId;
}
