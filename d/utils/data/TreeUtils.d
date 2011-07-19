module xf.utils.data.TreeUtils;



template MParentChildManagement() {
	private alias typeof(this) Node;
	
	
	private {
		Node[]			_children;
		Node				_parent;
	}



	private const static bool doRefCounting = is(typeof(typeof(this).addRef)) && is(typeof(typeof(this).unref));

	void parent(Node n) {
		if (_parent !is n) {
			static if (doRefCounting) { this.addRef(); } {
				if (_parent !is null) {
					
					bool removed = false;
					foreach (i, inout c; _parent._children) {
						if (this is c) {
							c = _parent._children[$-1];
							
							static if (doRefCounting) { this.unref(); }

							_parent._children = _parent._children[0 .. $-1];
							
							removed = true;
							break;
						}
					}					
					assert (removed);
				}
	
				_parent = n;
				if (is(typeof(typeof(this).onParentSet))) this.onParentSet();

				if (_parent !is null) {
					_parent._children ~= this;
					static if (doRefCounting) { this.addRef(); }
				}
			} static if (doRefCounting) { this.unref(); }
		}
	}


	final void attachChild(Node n)
	in {
		assert (n !is null);
	}
	body {
		n.parent = this;
	}

	
	final void detachChild(Node n, bool setChildParent = true)
	in {
		assert (n !is null);
	}
	body {
		n.parent = null;
	}
	
	
	final Node[] children() {
		return _children;
	}
	
	
	final Node parent() {
		return _parent;
	}
	
	
	Node root() {
		Node it = this;
		
		while (it._parent !is null) {
			it = it._parent;
		}
		
		//assert (it.validRootType());
		return it;
	}}