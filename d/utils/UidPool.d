module xf.utils.UidPool;

private {
	import xf.utils.Memory : append;
}



class UidPool(T) {
	synchronized T get() {
		if (numRecycled > 0) {
			return recycledIds[--numRecycled];
		} else {
			return nextId++;
		}
	}
	
	
	synchronized void dispose(T id) {
		if (nextId == id + 1) {
			--nextId;
		} else {
			recycledIds.append(id, &numRecycled);
		}
	}
	
	
	private {
		T		nextId = 0;
		T[]	recycledIds;
		uint	numRecycled;
	}
}
