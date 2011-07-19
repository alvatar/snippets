module xf.game.Event;

public {
	import xf.game.Misc : playerId, tick;
	import xf.game.TimeHub;
	import xf.utils.BitStream : BitStreamReader, BitStreamWriter, bsWrite, bsRead;
	import xf.utils.Singleton;

	import xf.xpose2.Expose;
	//import xf.xpose2.MiniD;
	
	import tango.stdc.stdio : printf;
}



abstract class Event {
	abstract void handle();
	
	/**
		These should add the event to local queues and also send it through the network
	*/
	final void delayed(float seconds) {
		this.atTick(cast(tick)(timeHub.currentTick + timeHub.secondsToTicks(seconds)));
	}
	abstract void atTick(tick);
	abstract void immediate();
	
	abstract void readFromStream(BitStreamReader);
	abstract void writeToStream(BitStreamWriter);
	
	abstract ushort getEventId();
	
	private {
		static ushort	eventId = ushort.max;
	}

	bool logged() {
		return false;
	}
	
	bool replayed() {
		return false;
	}
	
	ushort			instanceId = ushort.max;
	tick				eventTargetTick;

	//mixin(xpose2(`instanceId|eventTargetTick|logged|replayed|atTick|immediate|delayed|getEventId`));
	//mixin xposeMiniDNoSubclass;
}


template EventExpose() {
	private static char[] argListCodegen(bool types = true) {
		char[] res = "";
		foreach (i, field; xposeFields) {
			static if (field.isData) {
				const char[] name = field.name;
				if (i > 0) {
					res ~= ",";
				}
				if (types) {
					res ~= "typeof(this." ~ name ~ ")" ~ name;
				} else {
					res ~= name;
				}
			}
		}
		return res;
	}
	
	private static char[] opCallCodegen() {
		char[] res = "static typeof(this) opCall(" ~ argListCodegen ~ "){";
		res ~= "return new _ThisType(" ~ argListCodegen(false) ~ ");";
		res ~= "}";
		return res;
	}
	
	mixin(opCallCodegen());
	
	
	public void writeToStream(BitStreamWriter bs) {
		foreach (i, field; xposeFields) {
			static if (field.isData) {
				const char[] _fieldName = field.name;
				mixin(`
				static if(is(typeof(this.`~_fieldName~`) T__ == typedef)) {
					bs(cast(T__)this.`~_fieldName~`);
				} else {
					bsWrite(bs, this.`~_fieldName~`);
				}`);
			}
		}
	}
	
	public void readFromStream(BitStreamReader bs) {
		foreach (i, field; xposeFields) {
			static if (field.isData) {
				const char[] _fieldName = field.name;
				mixin(`
				static if(is(typeof(this.`~_fieldName~`) T__ == typedef)) {
					bs(cast(T__*)&this.`~_fieldName~`);
				} else {
					bsRead(bs, &this.`~_fieldName~`);
				}`);
			}
		}
	}
}


template _exposeEvent(_ThisType) {
	static void addHandler(void delegate(typeof(this)) h) {
		assert (h.ptr !is null);
		assert (h.funcptr !is null);
		handlers ~= cast(void delegate(Event))h;
	}

	override void handle() {
		foreach (h; handlers) {
			h(this);
		}
	}

	static typeof(this) createUninitialized() {
		return new typeof(this);
		//return cast(typeof(this))typeof(this).classinfo.init.dup.ptr;
	}
	
	override ushort getEventId() {
		return eventId;
	}

	
	static this() {
		xf.game.Event.registerEvent!(typeof(this));
	}

	override void atTick(tick t) {
		if (eventTargetTick == eventTargetTick.init) {
			eventTargetTick = t;
		}
		super.atTick(t);
	}
	
	// ----

	/+private import xf.xpose2.Expose;
	private import xf.xpose2.MiniD;+/
	private import xf.game.Event : EventExpose;
	
	this (typeof(_ThisType.tupleof) args) {
		foreach (i, a; args) {
			this.tupleof[i] = a;
		}
		static if (is(typeof(this) : Wish)) {
			wishOrigin = Wish.defaultWishOrigin;
		}
	}
	
	static if (typeof(_ThisType.tupleof).length > 0) {
		mixin(xpose2(`.*|_ctor`));
		mixin EventExpose;

		this() {
			// for createUninitialized
		}
	} else {
		mixin(xpose2(`_ctor`));
		
		static typeof(this) opCall() {
			return new typeof(this);
		}
		
		void writeToStream(BitStreamWriter bs) {}
		void readFromStream(BitStreamReader bs) {}
	}


	//mixin xposeMiniDNoSubclass;

	static if (is(typeof(this) : Order)) {
		typeof(this) filter(bool delegate(playerId) destinationFilter) {
			this.destinationFilter = destinationFilter;
			return this;
		}
	}

	static void delegate(Event)[]	handlers;
	private {
		static ushort	eventId = ushort.max;
	}
}


template MEvent() {
	mixin _exposeEvent!(typeof(this));
}



private template MBaseEvent() {
	static {
		private void delegate(Event, tick)[] submitHandlers;
	
		void addSubmitHandler(void delegate(Event, tick) h) {
			submitHandlers ~= h;
		}
	}
	

	override void atTick(tick t) {
		foreach (sh; submitHandlers) {
			sh(this, t);
		}
	}

	override void immediate() {
		return this.atTick(timeHub.currentTick);
	}
}


/**
	initiated by the server
	executed by the server and the client 
*/
class Order : Event {
	mixin MBaseEvent;
	
	
	abstract void	writeToStream(BitStreamWriter);
	bool				strictTiming() { return false; }
	
	final override bool logged() {		// not sure about it yet, but it makes sense, unless there's a separate queue for orders at client side
													// even then, orders would have to be reversible, and it's hardly ever feasible
		return false;
	}
	
	bool delegate(playerId) destinationFilter;
	
	//mixin(xpose2(`strictTiming`));
	//mixin xposeMiniDNoSubclass;
}


/**
	initiated by a client
	executed by the server in a safe manner and by client’s prediction modules 
*/
class Wish : Event {
	mixin MBaseEvent;
	
	
	abstract void writeToStream(BitStreamWriter);
	
	playerId			wishOrigin;
	static playerId	defaultWishOrigin;
	uint					receptionTimeMillis;

	/+mixin(xpose2(`wishOrigin|receptionTimeMillis`));
	mixin xposeMiniDNoSubclass;+/
}


/**
	initiated by the server and/or clients
	not sent through the network, executed locally 
*/
class Local : Event {
	mixin MBaseEvent;
	
	
	abstract void rollback();
	override bool logged() {
		return true;
	}
	
	// handling a Local event should also add it to an undo-queue

	/+mixin(xpose2(`rollback`));
	mixin xposeMiniDNoSubclass;+/
}



enum EventType {
	Order	= 0b1,
	Wish		= 0b10,
	Local		= 0b100,
	
	Any		= Order | Wish | Local
}


// not multithread-safe, but it should only be called from static ctors.
void registerEvent(T)() {
	static assert (is(T : Event));
	assert ((T.classinfo in registeredEvents) is null);
	
	debug printf(`Registering event %.*s to id %d`\n, T.classinfo.name, cast(int)lastFreeEventId);
	
	eventFactories[T.eventId = lastFreeEventId++] = cast(Event function())&T.createUninitialized;
	eventTypes[T.eventId] = is(T : Order) ? EventType.Order : is(T : Wish) ? EventType.Wish : EventType.Local;
	registeredEvents[T.classinfo] = T.eventId;
}


bool checkEventType(typeof(Event.eventId) id, EventType typeMask) {
	debug printf(`event type: %d(%d) ; type mask: %d`\n,
		id,
		id in eventTypes ? cast(int)eventTypes[id] : -1,
		cast(int)typeMask
	);
	
	// BUG, TODO: crash-proof me
	return (eventTypes[id] & typeMask) != 0;
}


Event createEvent(typeof(Event.eventId) id) {
	return eventFactories[id]();
}


Event readEventOr(BitStreamReader bs, EventType typeMask, void delegate() error) {
	typeof(Event.eventId) eventId;
	bs(&eventId);
	debug printf(`read event id: %d`\n, cast(int)eventId);
	
	if (!checkEventType(eventId, typeMask)) {
		error();
		return null;
	} else {
		Event event = createEvent(eventId);
		assert (event !is null);
		debug printf(`created a %.*s`\n, event.classinfo.name);
		
		bs(&event.instanceId);
		debug printf(`read event instance id: %d`\n, cast(int)event.instanceId);
		
		// read the number of dependencies - some packed int  -  TODO
		// read the dependency list  -  TODO
		
		event.readFromStream(bs);
		
		debug printf(`event data unserialized`\n);
		return event;
	}
}


void writeEvent(BitStreamWriter bs, Event event) {
	debug printf(`writing event id: %d, instance id %d`\n, cast(int)event.getEventId(), cast(int)event.instanceId);
	
	bs(event.getEventId())(event.instanceId);
	
	// write the number of dependencies - some packed int  -  TODO
	// write the dependency list  -  TODO
	
	event.writeToStream(bs);
}


private {
	typeof(Event.eventId)							lastFreeEventId = 1;
	typeof(Event.eventId)[ClassInfo]			registeredEvents;
	Event function()[typeof(Event.eventId)]	eventFactories;
	EventType[typeof(Event.eventId)]			eventTypes;
}
