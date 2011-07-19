module xf.utils.data.BitSet;

private {
	import xf.utils.Memory;
	import std.intrinsic;
}



struct BitSet(int minBits) {
	void opIndexAssign(bool b, int idx) {		// TODO: maybe there are some intrinsics for this...
		if (b) {
			bts(bits.ptr, idx);
		} else {
			btr(bits.ptr, idx);
		}
	}
	
	
	bool opIndex(int idx) {
		//return (bits[idx / Tbits] & (1 << (idx % Tbits))) != 0;
		return bt(bits.ptr, idx) != 0;
	}
	
	
	// opApply is evil, because it forces an inout index :F
	void iter(void delegate(uint i) dg) {
		uint off = 0;
		
		foreach (chunk; bits) {
			while (chunk != 0) {
				int idx = bsf(chunk);
				dg(off + idx);
				btr(&chunk, idx);
			}			
			
			off += Tbits;
		}
	}


	private {
		alias uint T;
		
		const uint									Tbits = T.sizeof * 8;
		T[(minBits + Tbits - 1) / Tbits]		bits;
	}
}


class DynamicBitSet {
	~this() {
		bits.free();
	}
	

	void resize(size_t count) {
		size_t size = (count+31) / 32;
		if (bits.length != size) {
			if (bits !is null) {
				bits.realloc(size);
			} else {
				bits.alloc(size);
			}
		}
	}
	
	
	size_t length() {
		return bits.length * 32;
	}


	void set(int i) {
		bits[i>>5] |= (1 << (i & 31));
	}


	bool isSet(int i) {
		return (bits[i>>5] & (1 << (i & 31))) != 0;
	}


	void clear(int i) {
		bits[i>>5] &= ~(1 << (i & 31));
	}


	void clearAll() {
		bits[] = 0;
	}


	private {
		uint[] bits;
	}
}



unittest {
	BitSet!(128) bs;
	static const int[] indices = [1, 3, 31, 55, 127];
	
	foreach (i; indices) bs[i] = true;
	foreach (i; indices) assert (bs[i] == true);
	foreach (i; indices) bs[i] = false;
	foreach (i; indices) assert (bs[i] == false);

	foreach (i; indices) bs[i] = true;
	{
		int i = 0;
		bs.iter((uint bi) {
			assert (indices[i++] == bi);
		});
	}

	foreach (i; indices) bs[i] = false;
	bs.iter((uint bi) {
		assert (false);
	});
}
