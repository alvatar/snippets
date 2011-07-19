/** Python-like (yield) iterators for Digital Mars D
 * Version: 1.0
 * Author: Witold Baryluk <baryluk@smp.if.uj.edu.pl>
 * Copyright 2007
 * Licence: BSD

This package may be redistributed under the terms of the UCB BSD
license:

Copyright (c) Witold Baryluk
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
4. Neither the name of the Witold Baryluk nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

 */

/**
 * TODO:
 *	 - Index variables shouldn't be inout.
 *	 - Shorter syntax (s.squers(), insted s.squers.ic())
 *	 - Neseting?
 *   - Remove unnacasary types: (mixin mainiter!(iter),
 *     insted mixin mainiter!(int, iter));
 *   - Reuse mainiter procedures in inneriter, becouse
 *     they are very similar.
 *   - Mayby void iter() , and return 0; is not needed really
 */
module iterators;


/*
class IterNoMore : Exception {
	this() { super(); }
}
*/

/** Exception rised when user defined function in
 *  foreach breaks, which cancel iteration
 *	process, catched in opApply. */
class IterBreak : Exception {
	int ret;
	this(int ret0) {
		super("Iterator Break");
		ret = ret0;
	}
}
/** Similar to IterBreak, used to make last iteration. */
class IterLast : Exception {
	int ret;
	this(int ret0) {
		super("Iterator Last");
		ret = ret0;
	}
}

/** Mixin this template to use your class as
 * iterator.
 * Params:
 *    V = type of iterator (ie. double)
 *    f = name of virtual method implementing iteration.
 * Example:
 * -------
 *  class Naturals(int n) {
 *    int iter() {
 * 	    for (int i = 1; i <= n; i++) yield(i); // use yield to process next item
 * 	    return 0;        // indicate correct end of iterator
 *    }
 *    mixin mainiter!(int, iter);   // mixin iterator's stuff
 *  }
 *  ...
 *  foreach (int x; new Naturals!(10)()) {
 *      ...
 *  }
 * -------
 */
template mainiter (V, alias f) {
	/* Curent context of iterator, user's delegate */
	private int delegate(inout V) _iter_curctx;
	private int delegate(inout int, inout V) _iter_curctx2;
	private int _iter_count = 0;

	int opApply(int delegate(inout V) fn) {
		assert(_iter_curctx is null && _iter_curctx2 is null, "Another iteration on this object in progress");
		_iter_curctx = fn;
		try {
			return f();
		} catch (IterBreak ib) {
			return ib.ret;
		} catch (IterLast il) {
			return il.ret;
		} finally {
			_iter_curctx = null;
		}
	}

	int opApply(int delegate(inout int, inout V) fn) {
		assert(_iter_curctx is null && _iter_curctx2 is null, "Another iteration on this object in progress");
		_iter_curctx2 = fn;
		_iter_count = 0;
		try {
			return f();
		} catch (IterBreak ib) {
			return ib.ret;
		} catch (IterLast il) {
			return il.ret;
		} finally {
			_iter_curctx2 = null;
		}
	}


	/** Used to process next iteration. */
	void yield(V v) {
		int ret;
		if (_iter_curctx !is null) {
			ret = _iter_curctx(v);
		} else if (_iter_curctx2 !is null) {
			int i = ++_iter_count;
			ret = _iter_curctx2(i, v);
			assert(i == _iter_count, "Inout index error");
		} else {
			assert(false, "Iterator context not initialized");
		}
		if (ret != 0) {
			throw new IterBreak(ret);
		}
	}

	/** Used to process last iteration. */
	void last(V v) {
		yield(v);
		throw new IterLast(0);
	}
}

/** V type Iterator for method metoda0 */
class Iterator (V) {
	private int delegate(Iterator!(V)) metoda;
	/** */
	this(int delegate(Iterator!(V)) metoda0) {
		metoda = metoda0;
	}

	private int delegate(inout V) _iter_curctx;
	private int delegate(inout int, inout V) _iter_curctx2;
	private int _iter_count;

	int opApply(int delegate(inout V) fn) {
		assert(_iter_curctx is null && _iter_curctx2 is null, "Another iteration on this object in progress");
		_iter_curctx = fn;
		try {
			return metoda(this);
		} catch (IterBreak ib) {
			return ib.ret;
		} catch (IterLast il) {
			return il.ret;
		} finally {
			_iter_curctx = null;
		}
	}

	int opApply(int delegate(inout int, inout V) fn) {
		assert(_iter_curctx is null && _iter_curctx2 is null, "Another iteration on this object in progress");
		_iter_curctx2 = fn;
		_iter_count = 0;
		try {
			return metoda(this);
		} catch (IterBreak ib) {
			return ib.ret;
		} catch (IterLast il) {
			return il.ret;
		} finally {
			_iter_curctx2 = null;
		}
	}

	/** Used to process next iteration. */
	void yield(V v) {
		int ret;
		if (_iter_curctx !is null) {
			ret = _iter_curctx(v);
		} else if (_iter_curctx2 !is null) {
			int i = ++_iter_count;
			ret = _iter_curctx2(i, v);
			assert(i == _iter_count, "Inout index error");
		} else {
			assert(false, "Iterator context not initialized");
		}
		if (ret != 0) {
			throw new IterBreak(ret);
		}
	}

	/** Used to process last iteration. */
	void last(V v) {
		yield(v);
		throw new IterLast(0);
	}
}

/** Mixin this template to use subclass as iterator.
 * Use this when you want multiple iterators.
 * Example:
 * -------
 *  class MNaturals(int n) {
 *   int iter2(Iterator!(double) x) {
 * 	   for (int i = 1; i <= n; i++) x.yield(i*i);
 * 	   return 0;
 *   }
 *   mixin inneriter!(double, iter2) squers;
 *   int iter3(Iterator!(double) x) {
 * 	   for (int i = 1; i <= n; i++) x.yield(i*i*i);
 * 	   return 0;
 *   }
 *   mixin inneriter!(double, iter3) qubes;
 * }
 *  ...
 *  auto s = new MNaturals!(10)();
 *  foreach (int x; s.squers.ic()) {
 *      ...
 *  }
 *  foreach (int x; s.qubes.ic()) {
 *      ...
 *  }
 * -------
 */

template inneriter (V, alias metoda) {
	Iterator!(V) ic() {
		return new Iterator!(V)(&metoda);
	}
}
