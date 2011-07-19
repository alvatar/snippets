module xf.loader.scene.model.Animation;

private {
	import xf.loader.scene.model.Entity;
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.CoordSys;
	import xf.utils.Memory;
}



struct KeyFrame {
	float	 		time;			// [0 .. 1]
	CoordSys	coordSys;
}


struct Bone {
	int parentId = -1;


	KeyFrame[] keyframes() {
		return keyframes_;
	}
	
	
	void allocKeyframes(int n) {
		keyframes_.alloc(n);
	}
	
	void destroy() {
		name = null;
		parentId = -1;
		keyframes_.free();
	}
	
	
	char[] name() {
		return name_;
	}
	
	void name(char[] n) {
		name_ = n.dup;
	}
	
	
	private {
		char[]			name_;
		KeyFrame[]	keyframes_;	// transforms are parent-relative
	}
}


struct AnimVertex {
	final int[] bones() {
		return bones_;
	}
	
	final float[] weights() {
		return weights_;
	}
	
	final vec3[] offsets() {
		return offsets_;
	}
	
	
	AnimVertex clone() {
		AnimVertex res;
		res.bones_ = xf.utils.Memory.clone(bones_);
		res.weights_ = xf.utils.Memory.clone(weights_);
		res.offsets_ = xf.utils.Memory.clone(offsets_);
		return res;
	}
	
	void allocBones(int n) {
		bones_.alloc(n);
		weights_.alloc(n);
		offsets_.alloc(n);
	}
	
	void destroy() {
		bones_.free();
		weights_.free();
		offsets_.free();
	}


	public {
		int[]		bones_;
		float[]	weights_;
		vec3[]	offsets_;
	}
}


class Animation : Entity {
	mixin MEntity;
}


class SkeletalAnimation : Animation {
	final Bone[] bones() {
		return bones_;
	}
	
	
	final AnimVertex[] vertices() {
		return vertices_;
	}
	
	
	void allocBones(int n) {
		bones_.alloc(n);
	}
	
	
	void allocVertices(int n) {
		vertices_.alloc(n);
	}
	
	
	/**
		'av' should be malloc'd. Use at your own risk.
	*/
	void overrideVertices_(AnimVertex[] av) {
		foreach (inout v; vertices_) {
			v.destroy();
		}
		vertices_.free();
		vertices_ = av;
	}
	
	
	~this() {
		foreach (inout b; bones_) {
			b.destroy();
		}
		
		foreach (inout v; vertices_) {
			v.destroy();
		}
		
		bones_.free();
		vertices_.free();
	}
	
	
	public {
		Bone[]			bones_;
		AnimVertex[]	vertices_;
	}
}
