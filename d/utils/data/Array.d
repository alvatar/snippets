module xf.utils.data.Array;

private static import tango.stdc.stdlib;


interface ArrayExpandPolicy {
	template FixedAmount(int count) {
		template FixedAmount(int _count = count) {
			void _expand(size_t num) {
				if (num < _count) {
					_capacity += _count;
				} else {
					_capacity += (num + _count - 1) / _count * _count;
				}
				
				_reallocate();
			}
		}
	}

	template Exponential(int _mult) {
		static assert (false, `TODO`);
	}
}


interface ArrayAllocator {
	template Malloc() {
		void _reallocate() {
			_ptr = cast(T*)tango.stdc.stdlib.realloc(_ptr, _capacity * T.sizeof);
		}
		
		void _dispose() {
			tango.stdc.stdlib.free(_ptr);
			_length = _capacity = 0;
		}
	}
}


struct Array(
		T,
		alias ExpandPolicy = ArrayExpandPolicy.FixedAmount!(64),
		alias Allocator = ArrayAllocator.Malloc
) {
	mixin Allocator;
	mixin ExpandPolicy;
	
	
	void append(T x) {
		if (_length < _capacity) {
			_ptr[_length++] = x;
		} else {
			_expand(1U);
			_ptr[_length++] = x;
		}
	}
	alias append opCatAssign;
	
	
	void reserve(size_t num) {
		if (num > _capacity) {
			_expand(num - _capacity);
		}
	}
	
	
	void resize(size_t num) {
		reserve(num);
		_length = num;
	}
	
	
	void clear() {
		_length = 0;
	}
	
	
	void opIndexAssign(T x, int i) {
		assert (i < _length);
		_ptr[i] = x;
	}
	

	T* opIndex(int i) {
		assert (i < _length);
		return _ptr + i;
	}

	
	void dispose() {
		_dispose();
	}
	
	
	int opApply(int delegate(ref T) dg) {
		if (_ptr is null) {
			return 0;
		}
		
		T* end = _ptr + _length;
		for (T* it = _ptr; it < end; ++it) {
			if (auto r = dg(*it)) {
				return r;
			}
		}
		
		return 0;
	}
	

	int opApply(int delegate(ref int i, ref T) dg) {
		if (_ptr is null) {
			return 0;
		}
		
		T* end = _ptr + _length;
		int i = 0;
		for (T* it = _ptr; it < end; ++it, ++i) {
			if (auto r = dg(i, *it)) {
				return r;
			}
		}
		
		return 0;
	}

	
	private {
		T*		_ptr = null;
		size_t	_length = 0;
		size_t	_capacity = 0;
	}
}
