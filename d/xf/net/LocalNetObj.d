module xf.net.LocalNetObj;

private {
	import xf.utils.Memory : alloc;
	import xf.game.Misc : playerId;

	alias void delegate(Object) DisposeEvt;
	extern (C) void rt_attachDisposeEvent(Object obj, DisposeEvt evt);
	extern (C) void rt_detachDisposeEvent(Object obj, DisposeEvt evt);
}


typedef uint localNetObjId;


struct LocalNetObjHandle {
	private LocalNetObjHandle*	next;
	Object									obj;
	localNetObjId							serverId;
	localNetObjId							creatorId;
	playerId								creator;
	LocalNetObjMngr					mngr;
	
	
	void onDispose(Object o) {
		mngr.onDispose(this);
		rt_detachDisposeEvent(o, &onDispose);
		mngr.releaseHandle(this);
	}
}


class LocalNetObjMngr {
	void registerDisposeHandler(void delegate(LocalNetObjHandle*) dh) {
		disposeHandlers ~= dh;
	}
	
	
	LocalNetObjHandle* register(Object obj) {
		assert (obj !is null);
		auto handle = acquireHandle();
		rt_attachDisposeEvent(obj, &handle.onDispose);
		handle.obj = obj;
		handle.mngr = this;
		return handle;
	}
	
	
	void onDispose(LocalNetObjHandle* h) {
		foreach (dh; disposeHandlers) {
			dh(h);
		}
	}
	

	private {
		const int allocChunk = 64;
		LocalNetObjHandle* unused;
		void delegate(LocalNetObjHandle*)[] disposeHandlers;
		
		
		LocalNetObjHandle* acquireHandle() {
			if (unused is null) {
				allocMore();
			}
			auto res = unused;
			unused = res.next;
			return res;
		}
		
		
		void releaseHandle(LocalNetObjHandle* h) {
			h.next = unused;
			unused = h;
		}
		
		
		void allocMore() {
			LocalNetObjHandle[] arr;
			arr.alloc(allocChunk);
			for (int i = 0; i+1 < arr.length; ++i) {
				arr[i].next = &arr[i+1];
			}
			arr[$-1].next = this.unused;
			this.unused = &arr[0];
		}
	}
}
