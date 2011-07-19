module xf.net.Misc;



enum StreamFate {
	Dispose,
	Retain
}


template MRemovePendingNetObjects() {
	protected void removePendingNetObjects() {
		// BUG: should use some malloc'd container
		static NetObjBase[] delObjects;
		
		foreach (o; &iterNetObjects) {
			if (o.netObjScheduledForDeletion) {
				delObjects ~= o;
			}
		}
		
		foreach (o; delObjects) {
			removeNetObject(o);
		}
		
		delObjects.length = 0;
	}
}
