module xf.rt.test;

//version = Sponza;

private {
	import xf.rt.KDTree;
	import xf.rt.Misc;
	import xf.rt.Camera;
	import xf.rt.Photon : Photon, LightingSettings, PMap = PhotonMap;
	import xf.rt.Color;
	import xf.rt.IrradCache : IrradianceCache = IrradCache;
	import xf.rt.Spectrum : xyz2rgb, Spectrum, FullSpectrum;
	import xf.rt.Sampling;
	import xf.rt.Light;
	import xf.rt.RayTracer;
	import xf.rt.TaskMngr;
	
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.CoordSys;
	import xf.omg.core.Misc;
	import xf.omg.rt.TriAccel;

	import xf.loader.scene.Hme;
	import xf.loader.scene.model.Node;
	import xf.loader.scene.model.Mesh;
	import xf.loader.scene.model.Scene;

	import xf.rt.Math;
	import xf.omg.geom.AABB;
	
	import tango.text.convert.Format;
	import tango.time.StopWatch;
	import tango.math.Math : isNaN, isInfinity;
	import tango.core.Array : lbound;
	import tango.stdc.stdlib : alloca;
	
	import xf.rt.BunnyData;
	import Cornell = xf.rt.CornellBoxData;

	import tango.util.log.Trace;
	import tango.stdc.stdio : fopen, fclose;
	import xf.rt.RGBE;
	
	import xf.rt.TestCommon;
}





void main() {
	auto taskMngr = new TaskMngr;

	const int width			= 512;
	const int height			= 512;
	
	auto tracer	= new RayTracer;
	tracer.scene	= new RayTracer.Scene;
	
	tracer.maxPhotonCount		= 1_000_000;
	tracer.photonSearchRadius	= 100.f;
	tracer.photonsPerGather		= 300;
	tracer.doFinalGather			= true;
	tracer.doDirect					= true;
	tracer.doIndirect				= true;
	tracer.storeFirstPhoton		= true;
	tracer.shadowSamples		= 4;		// total
	const int aaSamples			= 3;		// sqrt
	tracer.irradAccuracy			= 20.f;
	tracer.finalGatherSamples	= 500;

	
	SrcPrim[] prims;
	void addPrim(SrcPrim p) {
		prims ~= p;
		auto norm = p.calcNormal;
		p.v.reverse;
		foreach (ref v; p.v) {
			v -= norm * .5f;
		}
		p.calcNormal;
		prims ~= p;
	}
	
	void spawnCornellBox() {
		foreach (i, surf; Cornell.surfaces) {
			{
				SrcPrim p;
				p.idx = i;
				p.v[] = surf.v[0..3];
				p.calcNormal;
				addPrim(p);
			}

			{
				SrcPrim p;
				p.idx = i;
				p.v[0] = surf.v[0];
				p.v[1] = surf.v[2];
				p.v[2] = surf.v[3];
				p.calcNormal;
				addPrim(p);
			}
		}
	}
	
	void spawnBunny(vec3 off) {
		for (int tri = 0; tri < bunnyTriangles.length; tri += 3) {
			mat3	rot = mat3.yRotation(90);
			float	scl = 100.f;
			
			vec3 v0 = (cast(vec3[])bunnyVertices)[bunnyTriangles[tri+0]] * rot * scl + off;
			vec3 v1 = (cast(vec3[])bunnyVertices)[bunnyTriangles[tri+1]] * rot * scl + off;
			vec3 v2 = (cast(vec3[])bunnyVertices)[bunnyTriangles[tri+2]] * rot * scl + off;

			SrcPrim p;
			p.idx = 0;
			p.v[] = [v0, v1, v2];
			p.calcNormal;
			addPrim(p);
		}
	}
	
	void spawnHME(char[] name, vec3 origin, quat rot, vec3 scale) {
		origin.x /= scale.x;
		origin.y /= scale.y;
		origin.z /= scale.z;
		
		vec3[]	positions;
		int[]		indices;
		
		{
			scope loader = new HmeLoader;
			loader.load(`scenes/`~name~`/scene.hme`);
			
			auto objCS = CoordSys(vec3fi.from(origin), rot);

			void loadMesh(Mesh mesh, CoordSys cs) {
				uint idxOffset = positions.length;

				foreach (p; mesh.positions) {
					p = cs.rotation.xform(p) + vec3.from(cs.origin);
					p.x *= scale.x;
					p.y *= scale.y;
					p.z *= scale.z;
					positions ~= p;
				}
				foreach (i; mesh.indices) {
					indices ~= i + idxOffset;
				}
			}

			void loadNode(Node node, CoordSys cs) {
				foreach (n; node.children) {
					loadNode(n, n.coordSys in cs);
				}

				foreach (m; node.meshes) {
					loadMesh(m, m.coordSys in cs);
				}
			}

			foreach (node; loader.scene.nodes) {
				loadNode(node, node.coordSys in objCS);
			}
			
			for (int tri = 0; tri < indices.length; tri += 3) {
				vec3 v0 = positions[indices[tri+0]];
				vec3 v1 = positions[indices[tri+1]];
				vec3 v2 = positions[indices[tri+2]];

				SrcPrim p;
				p.idx = 0;
				p.v[] = [v0, v1, v2];
				p.calcNormal;
				addPrim(p);
			}
		}
	}

	const float sponzaScale = 40.f;

	version (Sponza) {
		spawnHME(`sponza`, vec3.zero, quat.identity, vec3(1, 1, 1) * sponzaScale);
	} else {
		spawnCornellBox();
		//spawnBunny(vec3(200, 230, 200));
		spawnHME(`tank`, vec3(250, 100, 200), quat.yRotation(30), vec3(1, 1, 1) * 3.5f);
	}
	/+spawnHME(`teapot`, vec3(368, 330, 406), quat.yRotation(45), vec3(1, 1, 1) * 150.f);
	spawnHME(`mp5PickUp`, vec3(430, 170, 176), quat.yRotation(45), vec3(1, 1, 1) * 300.f);+/
	
	/+spawnBunny(vec3(400, 100, 400));
	spawnBunny(vec3(400, 450, 400));
	spawnBunny(vec3(100, 450, 300));+/


	tracer.materials = [ Cornell.Reflectances.white, Cornell.Reflectances.green, Cornell.Reflectances.red ];
	tracer.prims = new ScenePrim[prims.length];
	foreach (i, ref p; tracer.prims) {
		p.materialId = Cornell.surfaces[prims[i].idx].material_;
		p.normal = prims[i].n;
		prims[i].idx = i;
	}

	auto builder = tracer.scene.build(prims[], (SrcPrim s){
		bool valid = true;
		TriAccel res = TriAccel(s.v[0], s.v[1], s.v[2], &valid);
		if (valid) {
			res.data = s.idx;
			return res;
		} else {
			return TriAccel.init;
		}
	});
	
	{
		Trace.formatln(`Building a kD-Tree...`);
		scope sw = new StopWatch;
		sw.start();
		builder.build();	
		Trace.formatln(`Built in {} sec`, sw.stop());
	}

	Camera cam;
	version (Sponza) {
		vec3 sponzaCamPos = vec3(0.150078, 1.55762, -1.3782) * sponzaScale;
		quat sponzaCamRot = quat.yRotation(135);//quat(-0.546291, -0.38315, 0.677787, 0.308816);
		with (cam = new Camera(sponzaCamPos, sponzaCamRot.xform(vec3.unitY), sponzaCamRot.xform(-vec3.unitZ))) {
			aspect = cast(float)width / height;
			focalLenMul = 0.4f;
		}
	} else {
		with (cam = new Camera(Cornell.Camera.position, Cornell.Camera.upDirection, Cornell.Camera.direction)) {
			aspect = cast(float)width / height;
			focalLenMul = 1.4f;
		}
	}	
	
	
	// sponza
	version (Sponza) {{
		auto light = new UniformQuadLight;
		light.intensity = 1f;
		light.orientation	= mat3.xRotation(180);
		light.origin			= vec3(0, 16, 0) * sponzaScale;
		light.size			= vec2(6, 23) * sponzaScale;
		light.spectrum	= Cornell.lightSpectrum;
		
		tracer.lights ~= light;
	}} else {
		// orig cornell
		/+{
			auto light = new UniformQuadLight;
			light.intensity = .7f;
			light.orientation	= mat3.xRotation(180);
			light.origin			= vec3.zero;
			foreach (pt; Cornell.light.v) {
				light.origin += pt;
			}
			light.origin *= 0.25f;
			
			light.size			= vec2(343 - 213, 332 - 227);
			light.spectrum	= Cornell.lightSpectrum;
			
			tracer.lights ~= light;
		}+/
		
		// 2 light cornell
		{
			auto light = new UniformQuadLight;
			light.intensity = .1f;
			//light.orientation	= mat3.xRotation(180);
			//light.orientation	= mat3.zRotation(-90);
			light.origin			= vec3.zero;
			foreach (pt; Cornell.light.v) {
				light.origin += pt;
			}
			light.origin *= 0.25f;
			
			//light.origin			+= vec3(200, -150, 0);
			light.origin += vec3(220, -200, 50);
			
			light.size			= vec2(343 - 213, 332 - 227) * .5f;
			light.spectrum	= Cornell.lightSpectrum;
			
			tracer.lights ~= light;
		}
		{
			auto light = new UniformQuadLight;
			light.intensity = .6f;
			light.orientation	= mat3.xRotation(180);
			//light.orientation	= mat3.zRotation(-90);
			light.origin			= vec3.zero;
			foreach (pt; Cornell.light.v) {
				light.origin += pt;
			}
			light.origin *= 0.25f;
			light.origin -= vec3(170, 0, 0);
			
			//light.origin			+= vec3(200, -150, 0);
			
			light.size			= vec2(343 - 213, 332 - 227);
			light.spectrum	= Cornell.lightSpectrum;
			
			tracer.lights ~= light;
		}
	}
	
	generateImage(width, height, aaSamples, taskMngr, tracer, cam);
}
