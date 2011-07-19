module xf.utils.MemPool;

private {
	import xf.utils.Memory;
}



private template MemPool(uint blockSize) {
	void* alloc() {
		return cMalloc(blockSize);		// TODO
	}
	
	
	void dispose(void* ptr) {
		free(ptr);								// TODO
	}
	
	
	struct Block {
		ulong						usage;
		ubyte[blockSize][64]	buffer;
	}
}



T* palloc(T)() {
	T* result = cast(T*)MemPool!(T.sizeof).alloc();
	*result = T.init;
}

void pfree(T : T*)(T* ptr) {
	MemPool!(T.sizeof).dispose(ptr);
}
