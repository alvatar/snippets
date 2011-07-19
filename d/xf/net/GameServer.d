module xf.net.GameServer;

private {
	import xf.net.LowLevelServer;
	import xf.net.NetObj;
	import xf.net.ControlEvents : AdjustTick, KickPlayer, TuneClientTiming;
	import xf.net.Misc;
	import xf.net.Common;
	
	import xf.utils.BitStream;

	import xf.game.EventConsumer;
	import xf.game.Event;
	import xf.game.Misc : tick, objId, playerId;	
	import xf.game.TimeHub;
	import xf.core.JobHub;
	
	import xf.utils.HardwareTimer : hardwareTimer;

	import tango.stdc.stdio : printf;
}



class GameServer : EventConsumer, NetObjObserver {
	public {
		float delegate(playerId, NetObjBase) relevanceCalcFunc;
	}
	
	
	mixin MNetComm!(true);
	
	
	this(LowLevelServer lls) {
		this.impl = lls;

		Order.addSubmitHandler(&this.consume);
		
		lls.registerConnectionHandler(&this.onPlayerConnected);
		lls.registerDisconnectionHandler(&this.onPlayerDisconnected);
		
		TuneClientTiming.addHandler(&onTuneClientTiming);
		
		//jobHub.addRepeatableJob(&broadcastStates, snapshotsPerSecond);
	}
	

	void advanceTick() {
		//printf("GameServer: advance tick @ %d"\n, cast(int)timeHub.currentTick);
		receiveData();
		timeHub.advanceTick();
		receiveData();
		broadcastStates();
	}


	void setDefaultOrderMask(bool delegate(Order) m) {
		defaultOrderMask = m;
	}
	

	void setDefaultWishMask(bool delegate(Wish) m) {
		defaultWishMask = m;
	}
	

	void setOrderMask(playerId pid, bool delegate(Order) m) {
		if (m is null) m = (Order) { return true; };
		players[pid].orderMask = m;
	}
	

	void setWishMask(playerId pid, bool delegate(Wish) m) {
		if (m is null) m = (Wish) { return true; };
		players[pid].wishMask = m;
	}
	
	
	void setStateMask(playerId pid, bool m) {
		players[pid].stateMask = m;
	}

	
	void kickPlayer(playerId pid) {
		impl.kickPlayer(pid);
	}


	void registerConnectionHandler(void delegate(playerId) h) {
		return impl.registerConnectionHandler(h);
	}
	
	
	void registerDisconnectionHandler(void delegate(playerId) h) {
		return impl.registerDisconnectionHandler(h);
	}
	
	
	//private {
		synchronized void broadcastStates() {
			//printf("Server: broadcastStates @ %d"\n, cast(int)timeHub.currentTick);
			removePendingNetObjects();
			
			foreach (obj; &iterNetObjects) {
				/**
					If a client has authority over the object, set it to the client's state
				*/
				if (obj.authOwner != NoAuthority && obj.authOwner != ServerAuthority) {
					for (int i = 0; i < obj.numStateTypes; ++i) {
						if (obj.setToStoredState(obj.authOwner, i, cast(tick)(timeHub.currentTick+1))) {
							debug printf("object %d set to client's state; curTick: %d"\n, obj.netObjId, timeHub.currentTick);
						}
					}
				}
			}
			
			foreach (i, player; players) {
				if (player is null) continue;
				if (false == player.stateMask) continue;
				
				// TODO: turn this into a possibly-threaded task			
				updatePlayer(cast(playerId)i);
			}
		}

		
		void onPlayerConnected(playerId id) {
			assert (id >= players.length || players[id] is null);
			allocPlayers(id+1);
			with (players[id] = new RemotePlayer) {
				if (defaultWishMask !is null) {
					wishMask = defaultWishMask;
				} else {
					wishMask = (Wish) { return true; };
				}

				if (defaultOrderMask !is null) {
					orderMask = defaultOrderMask;
				} else {
					orderMask = (Order) { return true; };
				}
			}
			
			foreach (o, ref d; netObjData) {
				d.importances[id].realloc(o.numStateTypes, false);		// dont init to float.init (NaN)
				d.importances[id][] = 0.f;
			}

			// tell the client which tick it's at the server side
			// send our currentTick + 1, to make the event tell about the tick that will happen in just a moment, when orders are dispatched
			// 	this is sort of a HACK, but I couldnt come up with anything else that would do the trick
			// 	the +1 might be simply removed and client's catch-up could handle the difference, but would be slightly less efficient
			this.consume(AdjustTick(cast(tick)(timeHub.currentTick+1)).filter((playerId pid) { return id == pid; }), 0);
		}
		
		
		void onTuneClientTiming(TuneClientTiming e) {
			//players[e.pid].tickOffsetTuning = e.tickOffset;
			impl.setPlayerTimeTuning(e.pid, e.tickOffset);
		}
		
		
		void allocPlayers(uint nr) {
			if (players.length < nr) {
				players.length = nr;
				playerBudgetWriters.length = nr;
				foreach (ref w; playerBudgetWriters) {
					if (w is null) w = new BudgetWriter;
				}
				
				foreach (_dummy, ref d; netObjData) {
					d.importances.realloc(nr);
				}
			}
		}


		void onPlayerDisconnected(playerId id) {
			// BUG: need to do this in a thread-safe manner. a snapshot task might be running.
			assert (id < players.length);
			assert (players[id] !is null);
			players[id] = null;
		}

	
		LowLevelServer					impl;
		
		RemotePlayer[]					players;
		BudgetWriter[]					playerBudgetWriters;
		
		bool delegate(Order)			defaultOrderMask;
		bool delegate(Wish)			defaultWishMask;
		
		const int snapshotsPerSecond = 30;
	//}
}


private class RemotePlayer {
	// HACK!!! TODO: fix object prioritization, do some compression
	int							bitBudgetPerSecond = 16 * 1024;
	bool	delegate(Order)	orderMask;
	bool	delegate(Wish)	wishMask;
	bool							stateMask;
	float							tickOffsetTuning = 0.f;
	tick							lastTickRecvd;
}
