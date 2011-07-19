module xf.net.GameClient;

private {
	import xf.net.LowLevelClient;
	import xf.net.NetObj;
	import xf.net.ControlEvents;
	import xf.net.Misc;
	import xf.net.Common;
	
	import xf.utils.BitStream;
	import xf.utils.HardwareTimer : HardwareTimer;
	import xf.utils.Array;

	import xf.game.EventConsumer;
	import xf.game.Event;
	import xf.game.Misc : tick, objId;
	import xf.game.TimeHub;
	import xf.game.TickTracker;
	
	import xf.omg.core.Misc;

	import tango.stdc.stdio : printf, fflush, stdout;
}



class GameClient : EventConsumer, NetObjObserver, TickTracker {
	mixin MNetComm!(false);
	
	
	this(LowLevelClient impl) {
		this.impl = impl;
		serverBudgetWriter = new BudgetWriter;
		Wish.addSubmitHandler(&this.consume);
		AdjustTick.addHandler(&this.adjustTick);
		DestroyObject.addHandler(&this.onDestroyObjectOrder);
		registerConnectionHandler(&this.onConnectedToServer);
		timeHub.addTracker(this);
	}


	void advanceTick(uint ticks) {
		//printf("GameClient: advance tick @ %d"\n, cast(int)timeHub.currentTick);
		removePendingNetObjects();
		
		/**
			Store current states to be compared with future server snapshots
		*/
		//printf("GameClient: store states @ %d"\n, cast(int)timeHub.currentTick+1);
		foreach (id, netObj; netObjects) {
			if (netObj.keepServerUpdated) {
				netObj.storeCurrentStates(0, cast(tick)(timeHub.currentTick+1));
			}
			/+printf("obj %d:"\n, cast(int)id);
			netObj.dumpStates((char[] txt) {
				printf("%.*s", txt);
			});+/
		}
		//printf("State dump end"\n);
		updateServer();
	}


	void registerConnectionHandler(void delegate() h) {
		return impl.registerConnectionHandler(h);
	}
	
	
	tick lastTickReceived() {
		return lastTickRecvd;
	}
	
	
	float serverTickOffset() {
		/+if (lastTickRecvd != lastTickRecvd.init) {
			//return cast(long)lastTickReceived - cast(long)timeHub.inputTick;
			return this.tickOffsetTuning;
		}
		else return 0;+/
		return impl.timeTuning;
	}
	

	protected void trimHistory(uint ticks) {
	}
	
	
	protected void rollback(uint ticks) {
	}

	
	protected void onDestroyObjectOrder(DestroyObject e) {
		printf(`Handling a DestroyObject order`\n);
		getNetObj(e.id).netObjScheduleForDeletion();
	}
	
	
	// TODO: make this automatic
	void setLocalPlayerId(playerId id) {
		this.localPlayerId = id;
	}

	
	//private {
		void adjustTick(AdjustTick e) {
			printf(`Adjusting tick to %d`\n, e.serverTick);
			
			assert (!tickAdjusted);
			tickAdjusted = true;
			
			timeHub.overrideCurrentTick(e.serverTick);
		}

		
		void onConnectedToServer() {
			connectedToServer = true;
		}


		void rollbackTo(tick tck) {
			timeHub.rollback(timeHub.currentTick - tck);

			foreach (id, netObj; netObjects) {
				netObj.dropNewerStates(0, tck);
				
				int states = netObj.numStateTypes;
				for (int i = 0; i < states; ++i) {
					netObj.setToStoredState(0, i, tck);
				}
			}
		}
	//}
		


	private {
		BudgetWriter			serverBudgetWriter;
		LowLevelClient		impl;
		tick						lastTickRecvd;
		bool						tickAdjusted = false;
		bool						connectedToServer = false;
		bool						streamRetained = false;
		float						tickOffsetTuning = 0.f;
		playerId				localPlayerId = playerId.max;
	}
	
	const int serverBitBudgetPerSecond = 16_000;//56 * 1024;
}


/**
	Precise tick synchronization. Makes the client run as closely as possible to the perfect ahead-of-server time
	
	client.serverTickOffset specifies server-defined tick tuning feedback.
	It specifies how many ticks earlier the last event should've arrived.
	If it's a small negative number, then it's ok. If it's positive, the server has received an event out of time.
*/
void synchronizeNetworkTicks(GameClient client) {
	static int totalSampleCount = 0;
	++totalSampleCount;
	
	static float[200] offsetTable = 0.f;
	static int offsetPtr = 0;
	
	static float[200]	offsetTbl2 = void;
	static int			offsetPtr2 = 0;
	
	float serverOffset = client.serverTickOffset;
	offsetTable[offsetPtr++ % offsetTable.length] = serverOffset;
	//float offset = offsetTable.fold(0.f, (float a, float b){ return a + b; }) / offsetTable.length;
	
	static float deviation = -1.f;
	static float amortizedDeviation = 0.f;
	
	offsetTbl2[offsetPtr2++] = serverOffset;
	if (offsetTbl2.length == offsetPtr2) {
		offsetPtr2 = 0;
	}// {
		float mean = 0.f;
		foreach (x; offsetTbl2) mean += x;
		mean /= offsetTbl2.length;
		
		deviation = 0.f;
		foreach (x; offsetTbl2) deviation += (mean - x) * (mean - x);
		deviation /= offsetTbl2.length;
		deviation = sqrt(deviation);
	//}

	amortizedDeviation = (deviation + amortizedDeviation) / 2.f;

	// not enough samples to determine reliably
	if (totalSampleCount < 50) {
		amortizedDeviation = 2.f;
	}
	
		static float[offsetTable.length] sortedOffsets;
		sortedOffsets[] = offsetTable;
		sortedOffsets.sort;
		
		float errorThresh = .4f;
		float offset = sortedOffsets[rndint((1.f - errorThresh) * (offsetTable.length - 1))];
		
	offset += amortizedDeviation * 3f;
	
	// vary the game speed to match with the server
	float timeMult = 1.f;
	if (offset > 0.0f) {
		timeMult = 1.f + 0.005f * offset;
	} else if (offset < -1.0f) {
		timeMult = 1.f + 0.005f * offset;
	}
	
	if (timeMult > 1.2f) {
		timeMult = 1.2f;
	}
	if (timeMult < 0.8f) {
		timeMult = 0.8f;
	}
	HardwareTimer.multiplier = timeMult;

	printf(\r`offset [srv: %1.1f, calc: %1.1f; dev: %1.2f] ; time mult: %3.3f`, serverOffset, offset, amortizedDeviation, cast(float)HardwareTimer.multiplier);
	fflush(stdout);
}


void standardClientUpdate(GameClient client, void delegate() inputSampler) {
	const int maxCatchUp = 2;
	
	timeHub.incTargetInputTickOffset();
	
	while (timeHub.targetInputTickOffset > 0) {
		timeHub.incInputTick();
		inputSampler();
	}
	
	if (timeHub.currentTick >= timeHub.inputTick) {		// if we've simulated up to the budget, only receive data.
		client.receiveData();
	} else {
		for (int i = 0; i < maxCatchUp && timeHub.currentTick < timeHub.inputTick; ++i) {
			client.receiveData();
			timeHub.advanceTick();
			debug printf(`tick: %d , inputTick: %d`\n, timeHub.currentTick, timeHub.inputTick);
		}
	}
}
