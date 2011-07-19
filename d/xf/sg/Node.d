module xf.sg.Node;

private {
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.CoordSys;

	import xf.utils.data.TreeUtils : MParentChildManagement;
	import tango.stdc.stdlib : alloca;
	import tango.stdc.stdio : printf;
}



struct NodeCallback(Params ...) {
	alias void delegate(SgNode n, Params)	DgType1;
	alias void delegate(Params) 					DgType2;
	
	
	void add(DgType1 dg) {
		registered1 ~= dg;
		types ~= 1;
	}
	
	void add(DgType2 dg) {
		registered2 ~= dg;
		types ~= 2;
	}

	int numRegistered() {
		return registered1.length;
	}

	void rem(DgType1 dg) {
		foreach (i, inout r; registered1) {
			if (r is dg) {
				r = registered1[$-1];
				types[i] = types[$-1];
				registered1 = registered1[0..$-1];
				types = types[0..$-1];
				return;
			}
		}
		
		assert (false);		// no such dg...
	}
	
	void rem(DgType2 dg) {
		foreach (i, inout r; registered2) {
			if (r is dg) {
				r = registered2[$-1];
				types[i] = types[$-1];
				registered2 = registered2[0..$-1];
				types = types[0..$-1];
				return;
			}
		}
		
		assert (false);		// no such dg...
	}

	void call(SgNode n, Params params) {
		ubyte[]		types = (cast(ubyte*)alloca(this.types.length * ubyte.sizeof))[0..this.types.length];
		DgType1[]	registered1 = (cast(DgType1*)alloca(this.registered1.length * DgType1.sizeof))[0..this.registered1.length];
		DgType2[] registered2 = (cast(DgType2*)registered1.ptr)[0..this.registered2.length];
		
		types[] = this.types;
		registered1[] = this.registered1;
		registered2[] = this.registered2;
		
		foreach (i, type; types) {
			if (1 == type) registered1[i](n, params);
			else registered2[i](params);
		}
	}
	
	
	void callAndClear(SgNode n, Params params) {
		auto reg1 = registered1;
		auto reg2 = registered2;
		auto types_ = types;
		registered1 = null;
		registered2 = null;
		types = null;
		
		foreach (i, type; types_) {
			if (1 == type) reg1[i](n, params);
			else reg2[i](params);
		}
	}


	protected {
		union {
			DgType1[]	registered1;
			DgType2[]	registered2;
		}
		
		ubyte[] types;
	}	
}



/**
	Central, atomic unit of the scenegraph. A node with a position in the game world, a name and an optional corresponding dynamics node.
*/
class SgNode {
	mixin 		MParentChildManagement;
	char[]		name;			/// an optional textual name
	
	
	/**
		A callback for the occasion of a node movement or direct position change.
		The registered function will be called with the global translation and local rotation of the node, relatively to the previous state
	*/
	NodeCallback!(vec3fi, quat)	moveCallback;

	/**
		Called upon a position/rotation change. Like moveCallback, but it gives the world position and rotation instead of transformation deltas
	*/
	NodeCallback!(vec3fi, quat)	transformChangeCallback;

	/**
		Called just before destroying the node
	*/
	NodeCallback!()						destroyCallback;
	
	
	
	void onParentSet() {
		recalculateLocalCS();
	}
	
	
	void setParentNoTransform(SgNode n) {
		auto cs = localCS;
		parent = n;
		setTransform(cs, worldCS);
	}
	
	
	void recalculateLocalCS() {
		if (parent) parent.recalculateLocalCS();
		auto newCS = _parentOffset in parentCS;
		setTransform(newCS.origin, newCS.rotation, worldCS);
	}
	
	
	void updateChildTransforms() {
		foreach (c; children) {
			CoordSys ccs = c._parentOffset in this.worldOffset;
			c.setTransform(ccs.origin, ccs.rotation, c.worldCS);
			
			c.updateChildTransforms();
		}
	}
	
	
	// yields self-explanatory code in transformation functions
	static const CoordSys worldCS = CoordSys.identity;
	
	
	CoordSys parentCS() {
		if (parent is null) return CoordSys.identity;
		return parent.localCS;
	}
	
	
	// this one is quite confusing. it's a coordsys which is an offset from the global reference frame. The 'local' part doesn't mean that the
	// transformation is local. It means a transformation from the world to the local scope.
	CoordSys localCS() {
		return worldOffset;
	}
	

	/**
		Translate the node about a vector in a given coordinate system.
		
		Params:
		vec - the translation offset
		cs - a coordinate system in which the translation should be carried out. Can be e.g. a worldCS, parentCS or localCS of any Node in the same graph.
	*/
	void translate(vec3fi vec, CoordSys cs) {
		return transform(vec, quat.identity, cs);
	}

	
	/**
		Translate the node in its local coordsys
	*/
	void translate(vec3fi vec) {
		vec3fi worldVec = vec in localCS;
		worldOffset.origin += worldVec;
		_parentOffset.origin += _parentOffset.rotation.xform(vec);

		updateChildTransforms();
		moveCallback.call(this, worldVec, quat.identity);
		if (transformChangeCallback.numRegistered) transformChangeCallback.call(this, localCS.origin, localCS.rotation);
	}
	
	
	/**
		Rotate the node about a quaternion in a given coordinate system.
		
		Params:
		rot = the rotation to be performed
		cs = a coordinate system in which the rotation should be carried out. Can be e.g. a worldCS, parentCS or localCS of any Node in the same graph.
	*/
	void rotate(quat rot, CoordSys cs) {
		return transform(vec3fi.zero, rot, cs);
	}


	/**
		Rotate the node in its local coordsys
	*/
	void rotate(quat rot) {
		_parentOffset.rotation = _parentOffset.rotation * rot;
		worldOffset.rotation = worldOffset.rotation * rot;

		updateChildTransforms();
		moveCallback.call(this, vec3fi.zero, rot);
		if (transformChangeCallback.numRegistered) transformChangeCallback.call(this, localCS.origin, localCS.rotation);
	}
	
	
	/**
		Translate and rotate the node in a given coordinate system - in this order.
		
		Params:
		vec = the translation offset
		rot = the rotation to be performed
		cs = a coordinate system in which the translation should be carried out. Can be e.g. a worldCS, parentCS or localCS of any Node in the same graph.
		
		Remarks:
		Using this function is faster than performing a translation followed by a rotation
	*/
	void transform(vec3fi vec, quat rot, CoordSys cs) {
		// our offset from 'cs'
		CoordSys relativeCS = cs.worldToLocal(localCS);
		
		// our new offset from 'cs'
		CoordSys relativeCS2 = relativeCS in CoordSys(vec, rot);
		
		// new worldOffset
		CoordSys newCS = relativeCS2 in cs;
		
		vec3fi worldMove = newCS.origin - localCS.origin;
		quat  worldRot = newCS.rotation.deltaFrom(localCS.rotation);
		
		this._parentOffset = parentCS.worldToLocal(newCS);
		this.worldOffset = newCS;
		
		updateChildTransforms();
		moveCallback.call(this, worldMove, worldRot);
		if (transformChangeCallback.numRegistered) transformChangeCallback.call(this, localCS.origin, localCS.rotation);
	}
	

	// in localCS
	void transform(vec3fi vec, quat rot) {
		return transform(vec, rot, localCS);
	}

	
	/**
		Translate and rotate the node in its local coordsys - in this order
	*/
	void setTransform(vec3fi pos, quat rot) {
		setTransform(pos, rot, parentCS);
	}
	
	
	/**
		Set a node's transformation relatively to a coord sys
		
		Params:
		pos = the new position for the node
		rot = the new rotation for the node
		cs = a coordinate system in which the pos and rot are given
	*/
	void setTransform(vec3fi pos, quat rot, CoordSys cs) {
		CoordSys newCS = CoordSys(pos, rot) in cs;
		
		vec3fi worldMove = newCS.origin - localCS.origin;
		quat  worldRot = newCS.rotation.deltaFrom(localCS.rotation);
		
		this._parentOffset = parentCS.worldToLocal(newCS);
		this.worldOffset = newCS;
		
		updateChildTransforms();
		moveCallback.call(this, worldMove, worldRot);
		if (transformChangeCallback.numRegistered) transformChangeCallback.call(this, localCS.origin, localCS.rotation);
	}
	

	///
	void setTransform(CoordSys cs, CoordSys refFrame) {
		this.setTransform(cs.origin, cs.rotation, refFrame);
	}


	///
	void setTransform(CoordSys cs) {
		this.setTransform(cs.origin, cs.rotation, parentCS);
	}

	
	/**
		Get the rotation of the node within its SceneRoot
	*/
	quat worldRotation() {
		return worldOffset.rotation;
	}
	
	
	/**
		Get the position of the node within its SceneRoot
	*/
	vec3fi worldPosition() {
		return worldOffset.origin;
	}
	
	/**
		Set the position of the node within its SceneRoot
	*/
	void worldPosition(vec3fi pos) {
		setTransform(pos, quat.identity, worldCS);
	}

	/**
		Set the position of the node within its parrents position
	*/	
	void localPosition(vec3fi pos){
		setTransform(pos, quat.identity, localCS);
	}
	
	/**
		Set the rotation of the node within its parrent position
	*/
	void localRotation(quat q){
		setTransform(vec3fi.zero, q , localCS);
	}
	
	/**
		Get the rotation of the node relative to its parent
	*/
	quat localRotation() {
		return _parentOffset.rotation;
	}
	
	
	///
	CoordSys parentOffset() {
		return _parentOffset;
	}
	
	
	/**
		Get the position of the node relative to its parent
	*/
	vec3fi localPosition() {
		return _parentOffset.origin;
	}
	

	/**
		Destroy any data this node may have in the engine - such as its physical representation
	*/
	final void destroy() {
		if (destroyed) return;
		destroyed = true;
		
		while (children.length > 0) {
			if (children[0].unref) {	// will delay the detachChild to the child's destroy.parent = null
				children[0].addRef;
				detachChild(children[0]);
			}	// else the child has destroyed itself
		}
		
		destroyCallback.callAndClear(this);
		
		this.parent = null;		// this has to be done after children, they might still need the parent.
	}
	

	this() {
	}
	
	
	~this() {
		if (!destroyed) {
			printf(`warning: node deleted without an earlier call to destroy()`\n);
		}
	}
	
	
	final void addRef() {
		++numRefs;
	}
	
	
	final bool unref() {
		if (--numRefs <= 0) {
			destroy();
			return false;
		}
		
		else return true;
	}
	
	
	protected {
		CoordSys	_parentOffset	= CoordSys.identity;
		CoordSys	worldOffset	= CoordSys.identity;
		bool			destroyed = false;
		int			numRefs = 0;
	}
}
