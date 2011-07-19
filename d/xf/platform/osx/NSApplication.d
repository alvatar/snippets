module xf.platform.osx.NSApplication;

import xf.platform.osx.objc_class;
import xf.platform.osx.objc_runtime;
import xf.platform.osx.NSObject;
import xf.platform.osx.NSEvent;

import xf.utils.Singleton;

extern (C) {
	extern id *NSDefaultRunLoopMode;
	extern id NSApp;
}

class NSApplication {

	void terminate(id sender = nil) {
		send(obj, sel("terminate:"), sender);
		destroyPool(pool);
	}

	void run() {
		send(obj, sel("run"));
	}

    void update() {
		id updatePool = createPool();
		
		event.obj = send(obj, selNextEvent, 0xffffffffU, distantPast, NSDefaultRunLoopMode, YES);
		while(event.obj) {
			if (_eventHandler !is null) _eventHandler(&event);

			send(obj, selSendEvent, event.obj);

			event.obj = send(obj, selNextEvent, 0xffffffffU, distantPast, NSDefaultRunLoopMode, YES);
        }

		destroyPool(updatePool);
	}


	id mainMenu() {
		return send(_obj, selMainMenu);
	}


	bool loadNib(char[] nibName) {
		synchronized(this) {
			if (!loaded) {
				if (YES == cast (byte) send(cast(id)lookUp("NSBundle"), sel("loadNibNamed:owner:"), NSString(nibName), obj)) {
					loaded = true;
				}
			}
			return loaded;
		}
	}

	void finishLaunching() {
		synchronized(this) {
			if (!launched) {
				send(obj, sel("finishLaunching"));
				launched = true;
			}
		}
	}

	void eventHandler(void delegate(NSEvent *) h) {
		_eventHandler = h;
	}

	this() {
		meta = "NSApplication";
		pool = createPool();
		_obj = send(cast(id)meta, sel("sharedApplication"));

		selMainMenu = sel("mainMenu");
		selSendEvent = sel("sendEvent:");
		selNextEvent = sel("nextEventMatchingMask:untilDate:inMode:dequeue:");

		distantPast = send(cast(id) lookUp("NSDate"), sel("distantPast"));
	}

	protected {
		static id pool;
		id distantPast;

		SEL selMainMenu;
		SEL selNextEvent;
		SEL selSendEvent;

		NSEvent event; // current event

		bool launched;
		bool loaded;

		void delegate(NSEvent *) _eventHandler;
	}
	mixin NSObject;
}

alias Singleton!(NSApplication) Application;
