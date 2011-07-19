module xf.loader.scene.model.Node;

private {
	import xf.loader.scene.model.WorldEntity;
	import xf.loader.scene.model.Mesh;
	import xf.loader.scene.model.Light;
	import xf.loader.scene.model.Camera;
	import xf.loader.scene.model.Animation;
	import xf.omg.core.CoordSys : CoordSys;
}



class Node : WorldEntity {
	int filterChildren(Type)(int delegate(ref Type) dg) {
		foreach (ch; children) {
			if (auto t = cast(Type)ch) {
				if (auto res = dg(t)) {
					return res;
				}
			}
		}
		
		return 0;
	}

	Animation	animation;	
	

	int iterWorldEntityTree(int delegate(ref WorldEntity, ref CoordSys) dg) {
		int worker(Node node) {
			foreach (ch_; node.children) {
				if (auto ch = cast(WorldEntity)ch_) {
					CoordSys cs = ch.localCS;
					if (auto res = dg(ch, cs)) {
						return res;
					}
					if (auto node2 = cast(Node)ch) {
						if (auto res = worker(node2)) {
							return res;
						}
					}
				}
			}
			
			return 0;
		}
		
		
		return worker(this);
	}
	
}
