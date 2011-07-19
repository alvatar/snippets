module xf.rt.testBSP;

private {
	import xf.rt.KDTree;
	import xf.rt.TriBox;
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

	import xf.loader.bsp.BSP;
	
	import xf.rt.Math;
	import xf.omg.geom.AABB;
	import xf.omg.rt.TriAccel;
	
	import tango.text.convert.Format;
	import tango.time.StopWatch;
	import tango.math.random.Kiss;
	import tango.math.Math : isNaN, isInfinity;
	import tango.core.Array : lbound;
	import tango.stdc.stdlib : alloca;
	import tango.util.Convert;
	
	import xf.rt.BunnyData;
	import Cornell = xf.rt.CornellBoxData;

	import tango.util.log.Trace;
	import xf.rt.TestCommon;
}




alias xf.loader.bsp.BSP.BSPLevel!(int) BSPLevel;


int getTexture(char[] path, int flags, int type){
	return 0;
}

int getLightmapTexture(void[] data, int w, int h, int bits) {
	return 0;
}




void main() {
	auto taskMngr = new TaskMngr;

	const int width			= 512;
	const int height			= 512;
	
	auto tracer	= new RayTracer;
	tracer.scene	= new RayTracer.Scene;
	
	tracer.maxPhotonCount		= 1_000_000;
	tracer.photonSearchRadius	= 150.f;
	tracer.photonsPerGather		= 500;
	tracer.doFinalGather			= true;
	tracer.doDirect					= true;
	tracer.doIndirect				= true;
	tracer.storeFirstPhoton		= true;
	tracer.shadowSamples		= 9;		// total
	const int aaSamples			= 2;		// sqrt
	tracer.irradAccuracy			= 15.f;
	tracer.finalGatherSamples	= 500;


	
	SrcPrim[] prims;

	auto level = new BSPLevel;
	level.textureCallback(&getTexture);
	level.lightmapCallback(&getLightmapTexture);
	level.loadShaders("shaders");
	level.loadMap("charon3dm13.bsp");
	
	foreach (face; level.faces) {
		SrcPrim p;

		vec3 v(int i) {
			return level.vertices[face.startVertIndex+level.indices[face.startIndex+i]].position;
		}
		
		for (int i = 0; i+2 < face.indicesNumber; i += 3) {
			p.idx = 0;
			p.v[] = [v(i), v(i+1), v(i+2)];
			p.n = face.normal;
			prims ~= p;
		}
	}

	foreach (face, material; level) {
		if (material.isShader) {
			auto shader = material.shader;
			if (shader.hasSurfaceParam(SurfaceParam.NodLight)) {
				Trace.formatln("Light face({}), with shader {} , intensity: {} , lightimage: {}", face.indicesNumber, shader.name, shader.param("q3map_surfacelight"), shader.param("q3map_lightimage"));

				vec3 v(int i) {
					return level.vertices[face.startVertIndex+level.indices[face.startIndex+i]].position;
				}
				
				vec3 center = vec3.zero;
				center += v(0);
				center += v(1);
				center += v(2);
				center += v(4);
				center *= .25f;

				auto light			= new UniformQuadLight;
				light.intensity		= to!(float)(shader.param("q3map_surfacelight")) / 100000.f;
				light.orientation	= mat3.fromVectors(face.imapVectors[0], face.normal, face.imapVectors[1]);
				light.origin			= center;
				light.size			= vec2(2, 2);
				light.spectrum	= Cornell.lightSpectrum;
				
				tracer.lights ~= light;
			}
		}
	}
	
	tracer.materials = [ Cornell.Reflectances.white, Cornell.Reflectances.green, Cornell.Reflectances.red ];
	tracer.prims = new ScenePrim[prims.length];
	foreach (i, ref p; tracer.prims) {
		p.materialId = 0;
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
	
	vec3 camPos = vec3(916.36, 69.34, -1087.38);
	vec3 camDir = (vec3(916.36, 69.34, -1087.38) - vec3(172.86, 63.66, -598.14)).normalized;
	camDir = quat.yRotation(-130).xform(camDir);
	vec3 camRight = cross(camDir, vec3.unitY);
	//camPos -= camRight * 80;
	with (cam = new Camera(camPos, vec3.unitY, camDir)) {
		aspect = cast(float)width / height;
		focalLenMul = 0.7f;
	}
	
	{
		auto light = new UniformQuadLight;
		light.intensity = 1f;
		//light.orientation	= mat3.xRotation(180);
		light.orientation	= mat3.zRotation(180);
		light.origin			= camPos - (vec3(916.36, 69.34, -1087.38) - vec3(172.86, 63.66, -598.14)).normalized * 400 + vec3.unitY * 700;
		light.size			= vec2(343 - 213, 332 - 227) * .5f;
		light.spectrum	= Cornell.lightSpectrum;
		
		tracer.lights ~= light;
	}

	generateImage(width, height, aaSamples, taskMngr, tracer, cam);
}
