module xf.game.EventQueue;

private {
	import xf.game.Event;
	import xf.utils.FreeList;
}



/**
	this FIFO queue should keep waiting events and event history.
	when events are added, they go to the waiting section
	when the tick is advanced, appropriate events should be marked as active
	when an active event is processed, it should be shifted to the history
	the history can be trimmed to a specific number of ticks
*/
class EventQueue(alias CustomMix = emptyMix) {
	struct Item {
		Event	event;
		int		ticksToGo;
		mixin	CustomMix;
	}
	

	protected void removeMatching_(ref FreeListQueue!(Item) q, bool delegate(ref Item) dg) {
		for (auto it = q.begin(); !it.done; it.next()) {
			if (dg(*it())) {
				q.erase(it.node);
			}
		}
	}

	
	void removeMatching(bool delegate(ref Item) dg) {
		removeMatching_(queue, dg);
	}
	
	
	protected int iterQueues(int delegate(ref FreeListQueue!(Item).Node) dg) {
		foreach (ref node; queue) {
			if (auto r = dg(node)) return r;
		}
		return 0;
	}
	
	
	final void advanceTick(uint ticks) {
		foreach (ref node; &iterQueues) {
			node().ticksToGo -= ticks;
		}
	}
	
	
	bool moreEvents() {
		return !queue.empty && queue.front.ticksToGo <= 0;
	}
	
	
	Event nextEvent() {
		assert (moreEvents());
		Item item = *queue.front();
		queue.popFront;
		
		return item.event;
	}
	
	
	void addEvent(Event e, int ticksToGo) {
		Item item; {
			item.event = e;
			item.ticksToGo = ticksToGo;
		}
		
		if (queue.empty) {
			queue ~= item;
		} else {
			for (auto it = queue.revBegin(); !it.done(); it.prev()) {
				if (it().ticksToGo <= ticksToGo) {
					queue.insertAfter(it.node, item);
					return;
				}
			}
			
			queue.pushFront(item);
		}
	}


	void rollback(uint ticks, void delegate(Event) handler) {
		assert (false);
	}
	
	
	void trimHistory(uint ticks) {
		assert (false);
	}
	
	
	protected {
		FreeListQueue!(Item)		queue;
	}
}


class LoggingEventQueue(alias CustomMix = emptyMix) : EventQueue!(CustomMix) {
	override void removeMatching(bool delegate(ref Item) dg) {
		super.removeMatching(dg);
		removeMatching_(historyQueue, dg);
	}


	protected override int iterQueues(int delegate(ref FreeListQueue!(Item).Node) dg) {
		if (auto r = super.iterQueues(dg)) return r;
		foreach (ref node; historyQueue) {
			if (auto r = dg(node)) return r;
		}
		return 0;
	}


	override Event nextEvent() {
		assert (moreEvents());
		Item item = *queue.front();
		queue.popFront;
		
		if (item.event.logged) {
			historyQueue.pushBack(item);
		}
		
		return item.event;
	}


	void rollback(uint ticks, void delegate(Event) handler) {
		foreach (ref node; &iterQueues) {
			node().ticksToGo += ticks;
		}
		
		for (auto it = historyQueue.revBegin(); !it.done(); it.prev()) {
			if (it().ticksToGo > 0) {
				handler(it().event);
				Item item = *historyQueue.back();
				historyQueue.popBack();
				
				if (it().event.replayed) {
					queue.pushFront(item);
				}
			}
		}
	}
	
	
	void trimHistory(uint ticks) {
		// only leave events with ticksToGo > -'ticks'
		int thresh = -cast(int)ticks;
		while (!historyQueue.empty && historyQueue.front.ticksToGo <= thresh) {
			historyQueue.popFront();
		}
	}


	private {
		FreeListQueue!(Item)	historyQueue;
	}
}


template emptyMix() {
}