module xf.net.NetObj;

private {
	import xf.game.Misc : tick, objId, playerId;
	import xf.utils.BitStream;
	import xf.utils.data.BitSet;
	
	import tango.io.Stdout;
}




const uint maxStates = 256;

static if (maxStates <= 256) {
	alias ubyte StateIdx;
} else
static if (maxStates <= 65536) {
	alias ushort StateIdx;
} else {
	alias ushort StateIdx;		// just so the compiler doesn't complain anywhere else
	static assert(false, `Are you nuts?!`);
}

alias BitSet!(maxStates)	StateSet;


static bool _netObj_clientSideChecks = false;

interface NetObjObserver {
	void	onNetObjCreated(NetObjBase o);
	void	onNetObjDestroyed(NetObjBase o);
}


enum StateOverrideMethod {
	Replace,
	ApplyDiff
}


/+interface NetObjState {
	void	writeToStream(uint state, BitStreamWriter);
	float	readFromStream(uint state, tick, tick, BitStreamReader, StateOverrideMethod);		// returns an error level
	bool	setToStored(uint state, tick);
	void	storeCurrent(tick);
	float	compareCurrentWithStored(tick tck);	
	void	dropNewerThan(tick);
}


interface NetObjBase2 {
	objId netObjId();
	void	overrideNetObjId(objId);
	
	uint	numStateTypes();
	float	getStateImportance(uint state);
	bool	getStateCriticalness(uint state);
	
	NetObjState getStateForPlayer(playerId);
	
	bool	predicted();
	
	void	netObjUnref();
	bool	netObjRef();
	bool	netObjScheduledForDeletion();
	void	netObjScheduleForDeletion();
	
	void	dispose();

	playerId netObjAuthOwner();
	void netObjAuthOwner(playerId);
}+/


interface NetObjBase {
	objId netObjId();
	void	overrideNetObjId(objId);
	
	uint	numStateTypes();
	float	getStateImportance(playerId pid, uint state);
	bool	getStateCriticalness(uint state);
	
	void	writeStateToStream(playerId pid, uint state, BitStreamWriter, bool store);
	float	readStateFromStream(playerId pid, uint state, tick, tick, BitStreamReader, StateOverrideMethod);		// returns an error level
	bool	setToStoredState(playerId pid, uint state, tick);
	void	storeCurrentStates(playerId pid, tick);
	float	compareCurrentStateWithStored(playerId pid, tick tck);
	void	dropNewerStates(playerId pid, tick);
	void	getCurrentStates();
	
	bool	predicted();
	void	overridePredicted(bool);
	
	void	dumpStates(void delegate(char[]));
	void	dumpState(uint stateI, void delegate(char[]));
	
	void	netObjUnref();
	bool	netObjRef();
	bool	netObjScheduledForDeletion();
	void	netObjScheduleForDeletion();
	
	void	dispose();

/+	playerId netObjAuthOwner();
	void netObjAuthOwner(playerId);+/
	
	playerId	authOwner();
	void			setAuthOwner(playerId pid);
	playerId	realOwner();
	void			setRealOwner(playerId pid);
	
	/**
		A client-side NetObj may return true from keepServerUpdated, in such a case the client will keep updating
		the server with object data even though it doesn't own the object. This might be useful e.g. when the server
		takes control temporarily over the object but the client wants it back
	*/
	bool keepServerUpdated();
}



void delegate(NetObjBase)[]	netObjScheduleForDeletionHandlers;


interface NetObjSingle(State) : NetObjBase {
	void getState(State* ps);
	void setState(State ps, tick tck);
}


template NetObjMixImpl(States ...) {
	private import tango.stdc.stdio : printf;
	private import xf.utils.BitStream;
	private import xf.game.Misc : tick;
	
	private alias typeof(super) SuperType;
	//pragma (msg, typeof(this).stringof ~ `:` ~ SuperType.stringof);
	private const bool superNetObj = (is(typeof(SuperType._netObjPredicted)));


	static if (!superNetObj) {
		objId netObjId() {
			return this.netObjId_;
		}


		void overrideNetObjId(objId id) {
			this.netObjId_ = id;
		}
	}


	uint numStateTypes() {
		static if (superNetObj) {
			return super.numStateTypes + States.length;
		}
		else {
			return States.length;
		}
	}
	
	
	private {
		static if (superNetObj) {
			T superDo(T)(ref uint stateI, T delegate() sup, T delegate() notsup) {
				uint supStates = super.numStateTypes;
				if (stateI < supStates) return sup();
				else {
					stateI -= supStates;
					return notsup();
				}
			}
		} else {
			T superDo(T)(ref uint stateI, void delegate() sup, T delegate() notsup) {
				return notsup();
			}
		}
	}
	

	float	getStateImportance(playerId pid, uint stateI) {
		return superDo(stateI, {
			static if (superNetObj) {
				return super.getStateImportance(pid, stateI);
			}
		}, {
			float mult = 10.f;
			if (auto lws = pid in _lastWrittenStates) {
				foreach (i, dummy; States) {
					if (i == stateI) {
						if ((*lws).present[i]) {
							auto prevState = (*lws).states[i];
							typeof(prevState) curState = _currentlySetStates[i];
							//this.getState(&curState);
							mult = xf.game.StateUtils.compareStates(prevState, curState);
							//printf("* State importance mult = %f"\n, mult);
						}
					}
				}
			}
			return importances[stateI] * mult;
		});
	}
	

	void getCurrentStates() {
		static if (superNetObj) {
			super.getCurrentStates();
		}
		
		foreach (i, State; States) {
			State st;
			this.getState(&st);
			//this.setState(st, tick.init);
			_currentlySetStates[i] = st;
		}
	}

	
	void dumpStates(void delegate(char[]) dg) {
		static if (superNetObj) {
			super.dumpStates(dg);
		}
		
		foreach (i, State; States) {
			State st = _currentlySetStates[i];
			//this.getState(&st);
			st.dump(dg);
		}
	}


	void dumpState(uint stateI, void delegate(char[]) dg) {
		static if (superNetObj) {
			super.dumpState(stateI, dg);
		}
		
		foreach (i, dummy; States) {
			if (i == stateI) {
				States[i] state = _currentlySetStates[i];
				//this.getState(&state);
				state.dump(dg);
			}
		}
	}


	bool	getStateCriticalness(uint stateI) {
		return superDo(stateI, {
			static if (superNetObj) {
				return super.getStateCriticalness(stateI);
			}
		}, {
			return criticalness[stateI];
		});
	}


	void	writeStateToStream(playerId pid, uint stateI, BitStreamWriter bs, bool store) {
		if (_netObj_clientSideChecks) assert (0 == pid);
		
		return superDo(stateI, {
			static if (superNetObj) {
				return super.writeStateToStream(pid, stateI, bs, store);
			}
		}, {
			foreach (i, dummy; States) {
				if (i == stateI) {
					States[i] state = _currentlySetStates[i];
					//this.getState(&state);
					
					xf.game.StateUtils.writeState(state, bs);
					if (!(pid in _lastWrittenStates)) {
						_lastWrittenStates[pid] = StateSTuple.init;
					}
					_lastWrittenStates[pid].states[i] = state;
					_lastWrittenStates[pid].present[i] = true;
				}
			}
		});
	}
	
	
	float	readStateFromStream(playerId pid, uint stateI, tick tck, tick dropEarlierThan, BitStreamReader bs, StateOverrideMethod som)	{	// returns an error level
		if (_netObj_clientSideChecks) assert (0 == pid);
	
		return superDo(stateI, {
			static if (superNetObj) {
				return super.readStateFromStream(pid, stateI, tck, dropEarlierThan, bs, som);
			}
		}, {
			foreach (i, dummy; States) {
				if (i == stateI) {
					synchronized (queueMutex) {
						States[i]*	localState = getTimedState!(i)(pid, tck);
						States[i]	auth = States[i].init;
						
						xf.game.StateUtils.readState(auth, bs);
						/+printf("Received state:"\n);
						auth.dump((char[] txt) {
							printf("%.*s", txt);
						});
						printf("State dump end"\n);+/
						
						if (localState !is null) {
							/+printf("Local state:"\n);
							localState.dump((char[] txt) {
								printf("%.*s", txt);
							});
							printf("State dump end"\n);+/

							int numToRemove = 0;
							scope (exit) {
								while (numToRemove--) {
									stateQueues(pid).queues[i].removeHead();
								}
							}
							
							foreach (itemIdx, ref item; stateQueues(pid).queues[i]) {
								if (&item.state is localState) {
									break;
								} else {
									++numToRemove;
								}
							}

							float diff = xf.game.StateUtils.compareStates(*localState, auth);
							if (diff > 0.f) {
								switch (som) {
									case StateOverrideMethod.Replace:
										*localState = auth;		// store the state, for a future override
										break;
										
									case StateOverrideMethod.ApplyDiff:
										if (diff > 5.f) {
											*localState = auth;
											this.setState(auth, tck);
											_currentlySetStates[i] = auth;
											printf(\n"State difference too huge. Hard snapping"\n);
											break;
										}
									
										synchronized (queueMutex) {
											auto q = stateQueues(pid).queues[i];

											int qlen = q.length;
											
											const bool canApplyDiff = is(typeof(this.applyStateDiff(States[i].init, States[i].init, tick.init)));
											
											static if (canApplyDiff) {
												States[i] prev = *localState;
												this.setState(auth, tck);
												_currentlySetStates[i] = auth;
												*localState = auth;
												
												foreach (itemIdx, ref item; q) {
													if (item.tck > tck) {
														States[i] foo;
														this.getState(&foo);
														this.applyStateDiff(prev, item.state, tck);
														States[i] bar;
														this.getState(&bar);
														
														static if (is(typeof(bar.pos))) {
															auto diff = bar.pos - foo.pos;
															Stdout.formatln("moved {}; wanted {}", diff, item.state.pos - prev.pos);
														}
														
														prev = item.state;
														item.state = bar;
														//this.getState(&item.state);
														_currentlySetStates[i] = item.state;
													}
												}												
											} else {
												States[i]	prev = *localState;
												States[i]*	prevAuth = &auth;
												const constMult = 0.96f;
												const multMult = 0.98f;
												float mult = 0.97f;
												foreach (itemIdx, ref item; q) {
													if (item.tck > tck) {
														auto backup = item.state;
														item.state = *prevAuth;
														item.state.applyDiff(prev, backup, mult * constMult);
														mult *= multMult;
														prev = backup;
														prevAuth = &item.state;
													}
													
													if (qlen-1 == itemIdx) {
														this.setState(item.state, item.tck);
														_currentlySetStates[i] = item.state;
														/+printf("Set delta'd state:"\n);
														item.state.dump((char[] txt) {
															printf("%.*s", txt);
														});
														printf("State dump end"\n);+/
														break;
													}
												}

												*localState = auth;
											}

											
											/+foreach (itemIdx, ref item; q) {
												if (item.tck > tck) {
													item.state.applyDiff(*localState, auth);
												}
												
												if (qlen-1 == itemIdx) {
													this.setState(item.state, item.tck);
												}
											}+/
											
											if (this.predicted) {
												//printf("* applied a state diff to a predicted object"\n);
											}
											
											diff = 0.f;
										}
										break;
								}
							}
							
							return diff;
						} else {
							//printf(`state not found!!!`\n);
							auto q = &stateQueues(pid).queues[i];

							while (!q.isEmpty && (*q)[0].tck < dropEarlierThan) {
								q.removeHead();
							}

							q.addTail(StateQueueItem!(States[i])(auth, tck));		// store the state, for a future override
							int qlen = q.length;
							const int maxQLen = 200;
							if (qlen > maxQLen) {
								//printf("1state queue len: %d :S removing some"\n, qlen);
								for (int j = maxQLen; j < qlen; ++j) {
									q.removeHead;
								}
							}
							return 1.f;
						}
					}
				}
			}

			assert (false);	// should never get here
		});
	}


	float	compareCurrentStateWithStored(playerId pid, tick tck) {
		if (_netObj_clientSideChecks) assert (0 == pid);
		
		float res = 0.f;
		
		static if (superNetObj) {
			res += super.compareCurrentStateWithStored(pid, tck);
		}
		
		foreach (i, State; States) {
			States[i]* localState = getLatestTimedState!(i)(pid, tck);
			if (localState !is null){
				State st = _currentlySetStates[i];
				//this.getState(&st);
				float diff = xf.game.StateUtils.compareStates(*localState, st);
				res += diff;
				/+localState.dump((char[] localTxt) {
					st.dump((char[] stTxt) {
						printf("comparing states\n\tstored=[%.*s]\n\tset=[%.*s]"\n, localTxt, stTxt);
					});
				});
				printf("state %.*s diff: %f"\n, typeid(State).toString, diff);+/
			} else {
				printf("state not found: %.*s"\n, typeid(State).toString);
				res += 99999f;
			}
		}
		
		return res;
	}
	
	
	bool	setToStoredState(playerId pid, uint stateI, tick tck){
		if (_netObj_clientSideChecks) assert (0 == pid);
		
		return superDo(stateI, {
			static if (superNetObj) {
				return super.setToStoredState(pid, stateI, tck);
			}
		}, {
			foreach (i, dummy; States) {
				if (i == stateI) {
					synchronized (queueMutex) {
						States[i]*	localState = getTimedState!(i)(pid, tck);
						if (localState !is null) {
							this.setState(*localState, tck);
							_currentlySetStates[i] = *localState;
							return true;
						}
					}
					return false;
				}
			}
			
			return true;
		});
	}
	
	
	void	storeCurrentStates(playerId pid, tick tck) {
		if (_netObj_clientSideChecks) assert (0 == pid);
		
		static if (superNetObj) {
			super.storeCurrentStates(pid, tck);
		}
		
		foreach (i, State; States) {
			State st = _currentlySetStates[i];
			//this.getState(&st);
			
			synchronized (queueMutex) {
				auto q = &stateQueues(pid).queues[i];
				q.addTail(StateQueueItem!(State)(st, tck));
				int qlen = q.length;
				const int maxQLen = 200;
				if (qlen > maxQLen) {
					//printf("2state queue len: %d :S removing some"\n, qlen);
					for (int j = maxQLen; j < qlen; ++j) {
						q.removeHead;
					}
				}
			}
		}
	}
	
	
	void	dropNewerStates(playerId pid, tick ti) {
		if (_netObj_clientSideChecks) assert (0 == pid);
		
		static if (superNetObj) {
			super.dropNewerStates(pid, ti);
		}

		foreach (i, dummy_; stateQueues(pid).queues) {
			while (!stateQueues(pid).queues[i].isEmpty && stateQueues(pid).queues[i][stateQueues(pid).queues[i].length-1].tck > ti) {
				stateQueues(pid).queues[i].removeTail();
			}
		}
	}
	
	
	static if (!superNetObj) {
		bool	predicted() {
			return _netObjPredicted;
		}
		
		
		void	overridePredicted(bool p) {
			_netObjPredicted = p;
		}
		

		void	netObjUnref() {
			bool d = false;
			
			synchronized (this) {
				if (0 >= --netObjRefCnt_ && netObjScheduledForDeletion_) {
					d = true;
				}
			}
			
			if (d) this.dispose();
		}


		bool	netObjRef() {
			synchronized (this) {
				if (netObjScheduledForDeletion_) {
					return false;
				} else {
					++netObjRefCnt_;
					return true;
				}
			}
		}
		
		
		bool	netObjScheduledForDeletion() {
			return netObjScheduledForDeletion_;
		}


		void	netObjScheduleForDeletion() {
			synchronized (this) {
				if (!netObjScheduledForDeletion_) {
					netObjScheduledForDeletion_ = true;
					foreach (h; netObjScheduleForDeletionHandlers) {
						h(this);
					}
				}
			}
		}
	}
	
	
	/+playerId netObjAuthOwner() {
		return _netObjAuthOwner;
	}
	

	void netObjAuthOwner(playerId id) {
		_netObjAuthOwner = id;
	}+/

	
	// ----
	
	
	private {
		import mintl.deque : Deque;		// TODO: replace it with a list-based queue that's safe for multithreaded code.
		import xf.utils.Bind : Tuple;
		import mintl.mem : Mallocator = Malloc;
		static import xf.game.StateUtils;
		

		template QueueTuple(int i = 0) {
			static if (States.length > i+1) {
				alias Tuple!(Deque!(StateQueueItem!(States[i]), false, Mallocator), QueueTuple!(i+1)).type QueueTuple;
			} else {
				alias Tuple!(Deque!(StateQueueItem!(States[i]), false, Mallocator)).type QueueTuple;
			}
		}
		
		struct StateQueueItem(State) {
			State	state;
			tick		tck;

			static StateQueueItem opCall(State st, tick tck){
				StateQueueItem qi;
				qi.state = st;
				qi.tck = tck;
				return qi;
			}
		}
		

		// no need to synchronize, only called in critical sections.
		/+void	dropEarlierStates(playerId pid, tick ti) {
			foreach (i, dummy_; stateQueues(pid).queues) {
				while (!stateQueues(pid).queues[i].isEmpty && stateQueues(pid).queues[i][0].tck < ti) {
					stateQueues(pid).queues[i].removeHead();
				}
			}
		}+/

		
		States[stateI]* getTimedState(int stateI)(playerId pid, tick tck) {
			synchronized (queueMutex) {
				//printf(`browsing through %d states`\n, stateQueues(pid).queues[stateI].length);
				foreach (i, ref item; stateQueues(pid).queues[stateI]) {
					if (item.tck == tck) {
						return &item.state;
					}
				}
			}
			
			return null;
		}


		States[stateI]* getLatestTimedState(int stateI)(playerId pid, tick tck) {
			synchronized (queueMutex) {
				int best = -1;
				
				//printf(`browsing through %d states`\n, stateQueues(pid).queues[stateI].length);
				foreach (i, ref item; stateQueues(pid).queues[stateI]) {
					if (item.tck <= tck) {
						best = i;
					}
				}
				
				if (best != -1) {
					foreach (i, ref item; stateQueues(pid).queues[stateI]) {
						if (i == best) {
							return &item.state;
						}
					}
				}
			}
			
			return null;
		}


		// BUG
		Object queueMutex() {
			volatile {
				if (queueMutexObj__ !is null) {
					return queueMutexObj__;
				}
			}
			
			synchronized (this) {
				if (queueMutexObj__ is null) {
					queueMutexObj__ = new Object;
				}
			}
			
			return queueMutexObj__;
		}


		objId								netObjId_;
		
		//playerId							_netObjAuthOwner = playerId.max;
	
		static float[States.length]	importances;
		static bool[States.length]	criticalness;
		

		struct QueueSTuple {
			QueueTuple!() queues;
		}
		
		struct StateSTuple {
			States						states;
			bool[States.length]		present;
		}

		QueueSTuple*					stateQueues(playerId pid) {
			if (0 == pid) {
				return &_defaultStateQueues;
			} else {
				if (auto sq = pid in _playerStateQueues) {
					return *sq;
				} else {
					_playerStateQueues[pid] = new QueueSTuple;
					return _playerStateQueues[pid];
				}				
			}
		}
		
		QueueSTuple						_defaultStateQueues;
		QueueSTuple*[playerId]		_playerStateQueues;
		
		StateSTuple[playerId]		_lastWrittenStates;
		States								_currentlySetStates;
		
		Object								queueMutexObj__;
		
		int									netObjRefCnt_;
		bool									netObjScheduledForDeletion_;
		
		bool									netObjCanBeGivenBack;
	}
	
	public {
		static if (!superNetObj) {
			bool	_netObjPredicted;
		}
	}
	
	static this() {
		foreach (i, state; States) {
			static if (is(typeof(state.importance))) {
				importances[i] = state.importance;
			} else {
				importances[i] = 1.f;
			}
			
			static if (is(typeof(state.critical))) {
				criticalness[i] = state.critical;
			} else {
				criticalness[i] = true;
			}
		}
	}
}



//  internal stuff -----------------------------------------------------------------------------

interface NetObj(States ...) : NetObjSingle!(States[0]), NetObj!(States[1..$]) {
	alias States NetObjStateTuple__;
}
interface NetObj() {}


template MNetObj() {
	static if (is(typeof(this) Bases == super)) {
		mixin NetObjMix_!(Bases);
	}
}


template NetObjMix_(Bases ...) {
	private alias Bases[0] BasesFirst;
	
	static if (is(BasesFirst.NetObjStateTuple__)) {
		mixin NetObjMixImpl!(BasesFirst.NetObjStateTuple__);
	} else {
		mixin NetObjMix_!(Bases[1..$]);
	}
}
template NetObjMix_() {}
