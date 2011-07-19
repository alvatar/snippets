module xf.platform.osx.NSObject;

import xf.platform.osx.objc_class;
import xf.platform.osx.objc_runtime;

template NSObject() {

	Class meta() {
		return _meta;
	}

	void meta(char[] name) {
		if (_meta is null) _meta = lookUp(name);
	}

	id obj() {
		return _obj;
	}

	protected {
		Class _meta;
		id _obj;
	}
}
