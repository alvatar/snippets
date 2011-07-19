module xf.utils.data.ComboArray;

private {
	import xf.utils.Memory;
}



struct ComboArray(T, int staticSize, bool mallocd = false) {
	static assert (staticSize > 0);
	
	private {
		union {
			T[staticSize]	stat;
			uint				dynLength;
		}
		T[]					dyn;
	}
	
	
	bool onStack() {
		return dyn.length <= staticSize;
	}
	

	void opCatAssign(T[] x) {
		foreach (t; x) {
			this.opCatAssign(t);
		}
	}

	
	void opCatAssign(T x) {
		if (dyn.length < staticSize) {
			stat[dyn.length] = x;
			dyn = stat[0..dyn.length+1];
		} else if (staticSize == dyn.length) {
			dyn = null;
			static if (mallocd) {
				dyn.alloc(staticSize*2, false);
			} else {
				dyn = new T[staticSize*2];
			}
			dyn[0..staticSize] = stat;
			dyn[staticSize] = x;
			dynLength = staticSize+1;
		} else {
			static if (mallocd) {
				dyn.append(x, &dynLength);
			} else {
				if (dynLength >= dyn.length) {
					dyn.length = dyn.length * 2;
				}
				dyn[dynLength++] = x;
			}
		}
	}
	
	
	void free() {
		if (onStack) {
			dyn = null;
		} else {
			static if (mallocd) {
				xf.utils.Memory.free(dyn);
			} else {
				delete dyn;
			}
		}
		
		assert (dyn is null);
	}
	
	
	T opIndex(int i) {
		if (onStack) {
			return dyn[i];
		} else {
			return dyn.ptr[0..dynLength][i];
		}
	}
	
	
	void opIndexAssign(T x, int i) {
		if (onStack) {
			dyn[i] = x;
		} else {
			dyn.ptr[0..dynLength][i] = x;
		}
	}
	
	
	T[] opCall() {
		if (onStack) {
			return dyn;
		} else {
			return dyn.ptr[0..dynLength];
		}
	}
	
	
	size_t length() {
		if (onStack) {
			return dyn.length;
		} else {
			return dynLength;
		}
	}
}


unittest {
	ComboArray!(int, 4, true) arr;
	scope(exit) arr.free;
	
	arr ~= 1;
	assert (arr.onStack);
	assert (arr() == [1]);
	arr ~= 2;
	assert (arr.onStack);
	assert (arr() == [1, 2]);
	arr ~= 3;
	assert (arr.onStack);
	assert (arr() == [1, 2, 3]);
	arr ~= 4;
	assert (arr.onStack);
	assert (arr() == [1, 2, 3, 4]);
	arr ~= 5;
	assert (!arr.onStack);
	assert (arr() == [1, 2, 3, 4, 5]);
	arr ~= 6;
	assert (!arr.onStack);
	assert (arr() == [1, 2, 3, 4, 5, 6]);
}
