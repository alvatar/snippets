module xf.utils.ThreadsafePool;

private {
	import xf.utils.FreeList : FreeListQueue;
	import xf.utils.Bind : ParameterTypeTuple, ReturnType;
}



struct ThreadsafePool(alias create__, alias reuse__) {
	alias ParameterTypeTuple!(create__)	Params;
	alias ReturnType!(create__)				T;
	
	
	static if (is(T == class) || is(T == interface)) {
		alias T Res;
		
		Res create(Params p) {
			return create__(p);
		}
	} else {
		alias T* Res;
		
		Res create(Params p) {
			Res r = new T;
			*r = create__(p);
			return r;
		}
	}
	
	
	Res get(Params p) {
		Res r;
		
		synchronized (myMutex()) {
			if (freeList.empty) {
				return create(p);
			} else {
				r = *freeList.back();
				freeList.popBack;
			}
		}

		reuse__(r, p);
		return r;
	}
	
	
	void dispose(Res r) {
		synchronized (myMutex()) {
			freeList ~= r;
		}
	}
	
	
	private Object myMutex() {
		// double-checked locking is broken; TODO: invent something better here
		//if (mutex__ !is null) return mutex__;
		
		synchronized (poolInitMutex) {
			if (mutex__ is null) mutex__ = new Object;
		}
		return mutex__;
	}
	
	
	private {
		Object						mutex__;
		FreeListQueue!(Res)	freeList;
	}
}


private Object poolInitMutex;
static this() {
	poolInitMutex = new Object;
}
