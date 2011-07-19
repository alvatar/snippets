/**
	Fast dependency graph
	
	Examples:
	---
	Node	n1; n1.init();
	Node	n2; n2.init();
	Node	n3; n3.init();
	
	n3.dependsOn(n1);
	n3.dependsOn(n2);
	
	foreach (inout n; n1.iterNext) {
		assert (&n is &n3);
	}

	foreach (inout n; n1.iterRemNext) {
		assert (false);	// n3 also depens on n1
	}

	foreach (inout n; n2.iterRemNext) {
		assert (&n is &n3);
	}
	---
*/
module xf.utils.QuickDep;

import xf.utils.Tricks : opHackedApply;
import xf.utils.Memory;




private {
	struct Nothing {}
	Nothing nothing;
}


/**
	Aliased by DepGraph for convenience
*/
template DepGraph_(DataType = void, alias DataAlias = nothing) {
	struct Link {
		void setDst(Node* n, inout Link inLink)
		in {
			assert (n !is null);
			assert (inLink._dstNext !is null);
			assert (inLink._dstPrev !is null);
		}
		out {
			assert (_dstNext !is null);
			assert (_dstPrev !is null);
			assert (this is _dstNext._dstPrev);
			assert (this is _dstPrev._dstNext);
		}
		body {
			_dst = n;
			
			Link*	n0 = &inLink;
			Link*	n1 = inLink._dstNext;
			
			_dstPrev = n0;
			_dstNext = n1;
			
			n0._dstNext = n1._dstPrev = this;
		}
		
		
		Node* dst() {
			return _dst;
		}
		
		
		void unlinkDst()
		in {
			assert (_dstNext !is null);
			assert (_dstPrev !is null);
		}
		out {
			assert (_dstNext is null);
			assert (_dstPrev is null);
		}
		body {
			_dstNext._dstPrev = _dstPrev;
			_dstPrev._dstNext = _dstNext;
			
			_dstNext = _dstPrev = null;
		}
		
		
		void dispose() {
			_srcNext = nextFree;
			nextFree = this;
		}
		
	
		static Link* create() {
			if (nextFree is null) {
				allocBlock();
			}
	
			Link* res = nextFree;
			nextFree = nextFree._srcNext;
			return res;
		}
		
		
		private {
			static Link* nextFree = null;
			
			static void allocBlock() {
				const int blockSize = 512;
				
				Link[] block;
				block.alloc(blockSize);
				
				for (int i = block.length-1; i >= 1; --i) {
					block[i]._srcNext = &block[i-1];
				}
				
				nextFree = &block[$-1];
			}
		}
	
		
		private {
			Link*	_srcNext;		// reused at disposed-time to point at the next free item
			Link*	_dstNext;
			Link*	_dstPrev;
			Node*	_dst;
		}
	}
	
	
	
	/**
		A single dependency node. It may depend on numerous other nodes and nodes may depend on it.
		There are two kinds of dependencies:
		$(UL
			$(LI dynamic - removed during iterRemNext)
			$(LI static - these 'stick' between iterations and thus are more effective)
		)
		
		The typical usage case for the graph is:
		$(OL
			$(LI create some nodes)
			$(LI initialize these nodes, by calling init())
			$(LI set static and dynamic dependencies)
			$(LI traverse thru the graph using a queue)
		)
	*/
	struct Node {
		static if (!is(typeof(DataAlias) == Nothing)) {
			
			static assert (!is(typeof(DataAlias!().init)));
			static assert (!is(typeof(DataAlias!().dependsOn)));
			static assert (!is(typeof(DataAlias!().staticallyDependsOn)));
			static assert (!is(typeof(DataAlias!().iterNext_impl)));
			static assert (!is(typeof(DataAlias!().iterNext_mix)));
			static assert (!is(typeof(DataAlias!().iterNext)));
			static assert (!is(typeof(DataAlias!().iterRemNext_impl)));
			static assert (!is(typeof(DataAlias!().iterRemNext_mix)));
			static assert (!is(typeof(DataAlias!().iterRemNext)));
			static assert (!is(typeof(DataAlias!().process)));
			static assert (!is(typeof(DataAlias!().allInboundDone)));
			static assert (!is(typeof(DataAlias!().outHead)));
			static assert (!is(typeof(DataAlias!().staticOutHead)));
			static assert (!is(typeof(DataAlias!().inLink)));
			static assert (!is(typeof(DataAlias!().staticInLink)));
			static assert (!is(typeof(DataAlias!().staticDegree)));
			static assert (!is(typeof(DataAlias!().staticDone)));
			
			mixin DataAlias;
		} else static if (!is(DataType == void)) {
			DataType data;
		}
		
		
		/**
			Initialize the node. Call it before creating the node. Must be called in-place as it works on pointers.
		*/
		void init() {
			inLink._dstNext	= &inLink;
			inLink._dstPrev	= &inLink;
			inLink._dst			= this;
	
			staticInLink._dstNext	= &staticInLink;
			staticInLink._dstPrev	= &staticInLink;
			staticInLink._dst			= this;
		}
		
		
		/**
			Add a dynamic dependency.
		*/
		void dependsOn(inout Node n) {
			Link* l = Link.create();
			l.setDst(this, this.inLink);
			l._srcNext = n.outHead;
			n.outHead = l;
		}
		
	
		/**
			Add a static dependency
		*/
		void staticallyDependsOn(inout Node n) {
			Link* l = Link.create();
			l.setDst(this, this.staticInLink);
			l._srcNext = n.staticOutHead;
			n.staticOutHead = l;
			++staticDegree;
		}
		
		
		/**
			Iterates thru node's outgoing connections in the graph (nodes that depend on it)
		*/
		int iterNext_impl(int delegate(inout Node) dg) {
			int res = 0;
	
			for (Link* it = staticOutHead; it !is null; it = it._srcNext) {
				res = dg(*it.dst);
				if (res) return res;
			}
	
			for (Link* it = outHead; it !is null; it = it._srcNext) {
				res = dg(*it.dst);
				if (res) return res;
			}
			
			return res;
		}
		mixin opHackedApply!(iterNext_impl) iterNext_mix;
		alias iterNext_mix.apply iterNext;
	
	
		/**
			Iterates thru node's $(B resolved) outgoing connections in the graph (nodes that depend on it), $(B removing them as it goes)
		*/
		int iterRemNext_impl(void delegate(inout Node) cleanupDg, int delegate(inout Node) dg) {
			int res = 0;
	
			for (Link* it = staticOutHead; it !is null; it = it._srcNext) {
				++it.dst.staticDone;
				if (it.dst.allInboundDone) {
					it.dst.process();
					res = dg(*it.dst);
					if (res) return res;
				} else {
					cleanupDg(*it.dst);
				}
			}
	
			while (outHead !is null) {
				Node*	cur	= outHead.dst;
				Link*	next	= outHead._srcNext;
				
				outHead.unlinkDst();
				outHead.dispose();
				outHead = next;
	
				if (cur.allInboundDone) {
					cur.process();
					res = dg(*cur);
					if (res) return res;
				}
			}
			
			return res;
		}
		mixin opHackedApply!(iterRemNext_impl) iterRemNext_mix;
		alias iterRemNext_mix.apply iterRemNext;
		
		
		void cleanup() {
			staticDone = 0;
		}
	
	
		private {
			void process() {
				staticDone = 0;
			}
		
			
			bool allInboundDone() {
				return inLink._dstNext is &inLink && staticDone >= staticDegree;
			}
	
	
			Link*	outHead;
			Link*	staticOutHead;
			Link		inLink;
			Link		staticInLink;
			int		staticDegree;
			int		staticDone;
		}
	}
}


/**
	Dependency graph with a custom data type
	
	Params:
	DataType = the user type whose instance will be called 'data' and sit within Nodes
*/
template DepGraph(DataType = void) {
	alias DepGraph_!(DataType, nothing) DepGraph;
}


/**
	Dependency graph with a custom data type
	
	Params:
	DataAlias = the user data type template, which will be mixed into Nodes
*/
template DepGraph(alias DataAlias) {
	alias DepGraph_!(void, DataAlias) DepGraph;
}


unittest {
	{
		alias DepGraph!().Node Node;
		
		Node	n1; n1.init();
		Node	n2; n2.init();
		Node	n3; n3.init();
		
		n3.dependsOn(n1);
		n3.dependsOn(n2);
		
		foreach (inout n; n1.iterNext) {
			assert (&n is &n3);
		}

		foreach (inout n; n1.iterRemNext((inout Node n) {})) {
			assert (false);	// n3 also depens on n1
		}
	
		foreach (inout n; n2.iterRemNext((inout Node n) {})) {
			assert (&n is &n3);
		}
		
		assert (n2.outHead is null);
	}


	{
		alias DepGraph!(char[]).Node Node;
		
		Node	n1; n1.init();
		Node	n2; n2.init();

		n1.data = "n1";
		n2.data = "n2";
		
		n2.dependsOn(n1);
		
		foreach (inout n; n1.iterRemNext((inout Node n) {})) {
			assert (&n is &n2);
			assert ("n2" == n2.data);
		}
	}}


version (Unittest) {
	private template CustomData() {
		char[]	name;
		int		index;
	}
}

unittest {
	alias DepGraph!(CustomData).Node Node;
	
	Node	n1; n1.init();
	Node	n2; n2.init();

	n1.name = "n1";	n1.index = 1;
	n2.name = "n2";	n2.index = 2;
	
	n2.dependsOn(n1);
	
	foreach (inout n; n1.iterRemNext((inout Node n) {})) {
		assert (&n is &n2);
		assert ("n2" == n2.name);
		assert (2 == n2.index);
	}
}
