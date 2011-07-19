module xf.sg.Follower;


/**
	When mixed into an sg node, it will add functions that make it easy for an sg node to directly follow another sg node
*/
template MFollower() {
	private {
		import xf.sg.Node : NodeCallback, SgNode;
		import xf.omg.core.LinearAlgebra : vec3fi, quat;
		import xf.omg.core.CoordSys;
	}


	NodeCallback!()	detachCallback;
	
	
	typeof(this) attachTo(SgNode n) {
		assert (n !is null);
		n.transformChangeCallback.add(&onTargetTransformChange);
		n.destroyCallback.add(&onTargetDestroyed);
		return this;
	}
	
	
	void onTargetDestroyed() {
		detachCallback.call(this);
	}
	
	
	void onTargetTransformChange(vec3fi pos, quat rot) {
		auto cs = CoordSys(pos, rot) in targetOffset;
		this.setTransform(cs.origin, cs.rotation, this.worldCS);
	}
	
	CoordSys targetOffset = CoordSys.identity;
}
