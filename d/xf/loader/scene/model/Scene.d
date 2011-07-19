// might disappear soon; not sure if i like it
module xf.loader.scene.model.Scene;

private {
	import xf.loader.scene.model.Node;
	import xf.loader.scene.model.WorldEntity;
	import xf.loader.scene.model.Material;
	import xf.omg.core.CoordSys;
}



class Scene {
	Node[]		nodes;
	Material[]	materials;
}


struct iterSceneNodes {
	Scene scene;
	
	int iter(WorldEntity we, int delegate(ref WorldEntity, ref CoordSys) dg) {
		auto cs = we.localCS;
		if (auto r = dg(we, cs)) {
			return r;
		}
		
		if (auto node = cast(Node)we) {
			foreach (n; &node.filterChildren!(WorldEntity)) {
				if (auto r = iter(n, dg)) {
					return r;
				}
			}
		}
		
		return 0;
	}
	
	int opApply(int delegate(ref WorldEntity, ref CoordSys) dg) {
		if (scene) foreach (n; scene.nodes) {
			if (auto r = iter(n, dg)) {
				return r;
			}
		}
		return 0;
	}
}
