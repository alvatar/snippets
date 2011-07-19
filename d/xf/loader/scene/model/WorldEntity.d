module xf.loader.scene.model.WorldEntity;

private {
	import xf.loader.scene.model.Entity;
	import xf.omg.core.LinearAlgebra : vec3;
	import xf.omg.core.CoordSys;
	import xf.omg.rt.Common : Ray, Hit;
	import xf.omg.geom.Sphere;
	import xf.sg.Node : SgNode;
	import xf.sg.Follower;
}



class WorldEntity : SgNode, Entity {
	float intersectionSphereRadius = .3f;
	
	bool intersect(Ray r, Hit* hit) {
		auto sph = Sphere(vec3.zero, intersectionSphereRadius);
		Hit hit_;
		if (hit is null) {
			hit = &hit_;
		}
		return sph.intersect(r, *hit);
	}
	
	mixin MEntity;
	mixin MFollower;
}
