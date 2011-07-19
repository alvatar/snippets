module xf.utils.FreeList;



template MFreeList(bool GCd = true, uint blockSize = 8) {
	private import xf.utils.Memory;


	private {
		typeof(this)			freeListNext;
		
		static typeof(this)	freeListFirst;

		// HACK: it should probably use thread-local storage
		static Object			freeListMutex;
		
		static this() {
			freeListMutex = new Object;
		}
	}


	static typeof(this) freeListAlloc() {
		if (freeListFirst is null) {
			typeof(*this)[] arr;
			static if (GCd) {
				arr.length = blockSize;
			} else {
				arr.alloc(blockSize);
			}
			foreach (i, ref t; arr[0..$-1]) {
				t.freeListNext = &arr[i+1];
			}
			synchronized (freeListMutex) {
				arr[$-1].freeListNext = freeListFirst;
				freeListFirst = &arr[0];
			}
		}
		
		typeof(this) res = void;
		
		synchronized (freeListMutex) {
			res = freeListFirst;
			freeListFirst = res.freeListNext;
		}
		
		*res = typeof(*this).init;
		return res;
	}
	
	
	final void freeListDispose() {
		memset(this, 0, typeof(this).sizeof);
		synchronized (freeListMutex) {
			freeListNext = freeListFirst;
			freeListFirst = this;
		}
	}
}




struct FreeListQueue(T, bool GCd = true, uint blockSize = 8) {
	struct Node {
		private mixin MFreeList!(GCd, blockSize);
		
		T* opCall() {
			return &data;
		}

		public {
			T			data;
			
			alias freeListNext	next;
			Node*					prev;
		}
	}
	
	private {
		Node* head, tail;
		
		// just a handy helper func
		Node* alloc(ref T x)
		out (result)
		{
			assert(!(result is null));
		}
		body
		{
			Node* n = Node.freeListAlloc;
			n.data = x;
			return n;
		}
		
		void dispose(Node* n) {
			n.freeListDispose();
		}
	}
	
	
	// check if the list is empty
	bool empty() {
		return head is null;
	}
	
	
	uint length() {
		uint res = 0;
		for (Node *n = head; n; n = n.next) ++res;
		return res;
	}
	
	
	void pushBack(T x) {
		if (head is null) {
			head = tail = alloc(x);
			return;
		}
		
		Node* n = alloc(x);
		tail.next = n;
		n.prev = tail;
		tail = n;
	}


	void pushFront(T x) {
		if (head is null) {
			head = tail = alloc(x);
			return;
		}
		
		Node* n = alloc(x);
		head.prev = n;
		n.next = head;
		head = n;
	}
	
	
	void popBack()
	in {
		assert (!empty);
	}
	body {
		Node* n = tail;
		if (n.prev)
		{
			n.prev.next = null;
			tail = n.prev;
		} else head = tail = null;
		dispose(n);
	}
	
	
	void popFront()
	in {
		assert (!empty);
	}
	body {
		Node* n = head;
		if (n.next)
		{
			n.next.prev = null;
			head = n.next;
		} else head = tail = null;
		dispose(n);
	}
	
	
	void erase(Node* n)
	in {
		assert (n !is null);
	}
	body {
		unlink(n);
		dispose(n);
	}
	
	
	Node* opIndex(uint i)
	in {
		assert (!empty);
	}
	body {
		Node *n = head;
		while (i--) {
			n = n.next;
			assert (!(n is null));
		}
		return n;
	}
	

	T* front()
	in {
		assert (!empty);
	}
	body {
		return &head.data;
	}

	
	T* back()
	in {
		assert (!empty);
	}
	body {
		return &tail.data;
	}
	
	
	// returns -1 on failure
	int indexOf(T x) {
		int i = 0;
		for (Node *n = head; n; n = n.next)
		{
			if (n.data == x) return i;
			++i;
		}
		return -1;
	}
	

	bool contains(Node *node) {
		for (Node *n = head; n; n = n.next) if (node is n) return true;
		return false;
	}

	
	void reverse() {
		void swap(ref Node* a, ref Node* b) {
			Node *tmp = a;
			a = b;
			b = tmp;
		}
		
		Node *n = head;
		while (n) {
			Node *n2 = n;
			n = n.next;
			swap(n2.prev, n2.next);
		}

		swap(head, tail);
	}
	
	
	void opCatAssign(T x) {
		pushBack(x);
	}
	

	typeof(*this) opCat(T x) {
		typeof(*this) res = this.dup;
		res ~= x;
		return res;
	}


	typeof(*this) dup()	{
		typeof(*this) res;
		foreach (ref n; *this) res ~= n.data;
		return res;
	}
	
	
	void insertAfter(Node* after, Node* n)
	in {
		assert (!(after is null));
		assert (!(n is null));
		assert (!empty);
	}
	body {
		n.next = after.next;
		n.prev = after;
		if (after.next) after.next.prev = n;
		after.next = n;
		if (after is tail) tail = n;
	}


	void insertAfter(Node* after, T x) {
		insertAfter(after, alloc(x));
	}
	
		
	void insertBefore(Node* before, Node* n)
	in {
		assert (!(before is null));
		assert (!(n is null));
		assert (!empty);
	}
	body {
		n.next = before;
		n.prev = before.prev;
		if (before.prev) before.prev.next = n;
		before.prev = n;
		if (before is head) head = n;
	}


	void insertBefore(Node* before, T x) {
		insertBefore(before, alloc(x));
	}
	
	
	void unlink(Node* n)
	in {
		assert (!(n is null));
	}
	out {
		assert (n.next is null);
		assert (n.prev is null);
	}
	body {
		if (head == n) head = n.next;
		if (tail == n) tail = n.prev;
		if (n.prev)	n.prev.next = n.next;
		if (n.next)	n.next.prev = n.prev;
		n.next = null;
		n.prev = null;
	}
	
	
	int opApply(int delegate(ref Node) dg)
	in {
		assert (!(dg is null));
	}
	body {
		int result = 0;
		
		for (Node *n = head; n; n = n.next)
		{
			result = dg(*n);
			if (result) break;
		}
		
		return result;
	}
	

	int opApply(int delegate(ref uint index, ref Node) dg)
	in {
		assert (!(dg is null));
	}
	body {
		int result = 0;
		
		uint i_ = 0;
		for (Node *n = head; n; n = n.next)
		{
			result = dg(i_, *n);
			++i_;
			if (result) break;
		}
		
		return result;
	}
	
	
		typedef Node AfterNode;
		AfterNode* after(uint i) { return cast(AfterNode*)(*this)[i]; }
		
		typedef Node BeforeNode;
		BeforeNode* before(uint i) { return cast(BeforeNode*)(*this)[i]; }
	
		
		void move(uint what, AfterNode* after)
		in {
			assert (contains(cast(Node*)after));
		}
		body {
			Node* n = (*this)[what];
			assert (!(n is cast(Node*)after));
			unlink(n);
			insertAfter(cast(Node*)after, n);
		}
		

		void move(uint what, BeforeNode* before)
		in {
			assert (contains(cast(Node*)before));
		}
		body {
			Node* n = (*this)[what];
			assert (!(n is cast(Node*)before));
			unlink(n);
			insertBefore(cast(Node*)before, n);
		}

	
	void destroy() {
		while (!empty) popBack();
	}
	
	
	Iterator begin() {
		Iterator res;
		res.n = head;
		return res;
	}
	

	Iterator revBegin() {
		Iterator res;
		res.n = tail;
		res.getSiblings();
		return res;
	}

	
	struct Iterator {
		private {
			Node* n, next_, prev_;
			
			void getSiblings() {
				if (n) {
					next_ = n.next;
					prev_ = n.prev;
				} else {
					next_ = prev_ = null;
				}
			}
		}
		
		Node* node() {
			return n;
		}
		
		bool done() {
			return n is null;
		}
		
		bool hasNext() {
			return next_ !is null;
		}
		
		void next() {
			n = next_;
			getSiblings;
		}
		
		bool hasPrev() {
			return prev_ !is null;
		}
		
		void prev() {
			n = prev_;
			getSiblings;
		}

		T* get() {
			return &n.data;
		}
		
		alias get opCall;
	}
}



unittest {
	FreeListQueue!(int) l1;
	scope(exit) l1.destroy();
	
	l1.pushBack(0);
	l1.pushBack(1);
	l1.pushBack(2);
	l1.pushBack(3);
	l1.pushBack(4);
	
	assert (l1[0].data == 0);
	assert (l1[1].data == 1);
	assert (l1[2].data == 2);
	assert (l1[3].data == 3);
	assert (l1[4].data == 4);
	
	l1.popBack();
	assert (*l1.front == 0);
	assert (*l1.back == 3);

	l1.popFront();
	assert (*l1.front == 1);
	assert (*l1.back == 3);

	l1.popBack();
	assert (*l1.front == 1);
	assert (*l1.back == 2);

	l1.popBack();
	assert (*l1.front == 1);
	assert (*l1.back == 1);

	l1.popFront();
	assert (l1.empty);
}
