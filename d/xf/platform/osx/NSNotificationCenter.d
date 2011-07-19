module xf.platform.osx.NSNotificationCenter;

import xf.platform.osx.objc_class;
import xf.platform.osx.objc_runtime;

import xf.platform.osx.NSObject;

class NSNotificationCenter {

	static NSNotificationCenter defaultCenter() {
		if (_default is null) {
			_default = new NSNotificationCenter(false);
			_default._obj = send(cast(id) _default.meta, sel("defaultCenter"));
		}
		return _default;
	}

	void addObserverO(id observer, SEL s, id object) {
		send(obj, selAddObserver, observer, s, nil, object);
	}

	void addObserverN(id observer, SEL s, id *name) {
		send(obj, selAddObserver, observer, s, name, nil);
	}

	void addObserverNO(id observer, SEL s, id *name, id object) {
		send(obj, selAddObserver, observer, s, name, object);
	}

	this(bool create = true) {
		meta = "NSNotificationCenter";
	
		selAddObserver = sel("addObserver:selector:name:object:");
	
		if (create) {
			_obj = createInstance(meta);
		}
	}

	private {
		static NSNotificationCenter _default;

		SEL selAddObserver;
	}

	mixin NSObject;
}
