module xf.game.GameInterface;

private {
	import xf.game.Event;
	import xf.game.EventQueue;
	import xf.game.EventConsumer;
	import xf.game.TickTracker;
	import xf.game.TimeHub;
	import xf.game.Misc;

	import xf.net.NetObj;

	import xf.utils.UidPool;

	import tango.stdc.stdio : printf;
}



class GameInterface : EventConsumer, TickTracker {
	this() {
		createEventQueue();
		objIdPool = new UidPool!(objId);
		timeHub.addTracker(this);
		
		Order.addSubmitHandler(&this.consume);
		Wish.addSubmitHandler(&this.consume);
		Local.addSubmitHandler(&this.consume);
		
		netObjScheduleForDeletionHandlers ~= &this.onNetObjScheduleForDeletion;
	}
	
	
	protected void createEventQueue() {
		eventQueue = new EventQueue!();
	}


	void consume(Event ev, tick target) {
		eventQueue.addEvent(ev, cast(int)target - timeHub.currentTick);
	}
	
	
	void advanceTick(uint num) {
		//printf("GameInterface: advance tick @ %d"\n, cast(int)timeHub.currentTick);
		eventQueue.advanceTick(num);

		while (eventQueue.moreEvents) {
			Event ev = eventQueue.nextEvent;
			ev.handle();
		}
	}
	
	
	// nothing to do here ----
	void trimHistory(uint ticks) {
	}
	void rollback(uint ticks) {
	}
	// ----
	
		
	void	addObserver(NetObjObserver noo) {
		foreach (o; netObjObservers) {
			if (o is noo) return;
		}
		
		netObjObservers ~= noo;
	}
	
	
	void register(NetObjBase obj, objId id) {
		obj.overrideNetObjId(id);
		
		foreach (o; netObjObservers) {
			o.onNetObjCreated(obj);
		}
	}
	
	
	void onNetObjScheduleForDeletion(NetObjBase obj) {
		foreach (o; netObjObservers) {
			o.onNetObjDestroyed(obj);
		}
	}
	
	
	objId genObjId() {
		return objIdPool.get();
	}
	
	
	void disposeObjId(objId id) {
		objIdPool.dispose(id);
	}

	

	protected {
		EventQueue!()		eventQueue;
	}	
	private {
		UidPool!(objId)		objIdPool;
		NetObjObserver[]	netObjObservers;
	}
}



class LoggingGameInterface : GameInterface {
	protected override void createEventQueue() {
		eventQueue = new LoggingEventQueue!();
	}


	override void trimHistory(uint ticks) {
		eventQueue.trimHistory(ticks);
	}
	
	
	override void rollback(uint ticks) {
		printf(`Performing a time rollback`\n);
		
		eventQueue.rollback(ticks, (Event ev) {
			if (auto local = cast(Local)ev) {
				local.rollback();
			}
		});
	}
}



void createNetworkedGame(Game, Net)(lazy Game game, lazy Net net) {
	auto gameInterface = new LoggingGameInterface;
	game().setGameInterface(gameInterface);
	gameInterface.addObserver(net());
}
