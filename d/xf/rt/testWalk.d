module xf.rt.testWalk;

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
	import xf.loader.scene.model.Mesh : HMEMesh = Mesh;
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
	
	
	import xf.omg.mesh.Mesh;
	import xf.omg.mesh.Subdivision;
	import xf.rt.OCTree;
	import xf.omg.mesh.Logging;
	import xf.dog.Dog;
	import xf.utils.Memory;
}





void main() {
	auto taskMngr = new TaskMngr;

	const int width			= 512;
	const int height			= 512;
	
	auto tracer	= new RayTracer;
	tracer.scene	= new RayTracer.Scene;
	
	tracer.maxPhotonCount		= 200_000;
	tracer.photonSearchRadius	= 50.f;
	tracer.photonsPerGather		= 400;
	tracer.doFinalGather			= true;
	tracer.doDirect					= true;
	tracer.doIndirect				= true;
	tracer.storeFirstPhoton		= true;
	tracer.shadowSamples		= 36;		// total
	tracer.irradAccuracy			= 5.f;
	tracer.finalGatherSamples	= 500;

	
	SrcPrim[] prims;
	foreach (i, surf; Cornell.surfaces) {
		{
			SrcPrim p;
			p.idx = i;
			p.v[] = surf.v[0..3];
			p.calcNormal;
			prims ~= p;
		}

		{
			SrcPrim p;
			p.idx = i;
			p.v[0] = surf.v[0];
			p.v[1] = surf.v[2];
			p.v[2] = surf.v[3];
			p.calcNormal;
			prims ~= p;
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
			prims ~= p;
		}
	}
	
	void spawnHME(char[] name, vec3 origin, quat rot, vec3 scale) {
		vec3[]	positions;
		int[]		indices;
		
		{
			scope loader = new HmeLoader;
			loader.load(`scenes/`~name~`/scene.hme`);
			
			auto objCS = CoordSys(vec3fi.from(origin), rot);

			void loadMesh(HMEMesh mesh, CoordSys cs) {
				uint idxOffset = positions.length;

				foreach (p; mesh.positions) {
					p.x *= scale.x;
					p.y *= scale.y;
					p.z *= scale.z;
					positions ~= cs.rotation.xform(p) + vec3.from(cs.origin);
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
				prims ~= p;
			}
		}
	}

	spawnBunny(vec3(200, 230, 200));

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

	{
		auto light = new UniformQuadLight;
		light.intensity = .1f;
		light.origin			= vec3.zero;
		foreach (pt; Cornell.light.v) {
			light.origin += pt;
		}
		light.origin *= 0.25f;
		
		light.origin += vec3(220, -200, 50);
		
		light.size			= vec2(343 - 213, 332 - 227) * .5f;
		light.spectrum	= Cornell.lightSpectrum;
		
		tracer.lights ~= light;
	}
	{
		auto light = new UniformQuadLight;
		light.intensity = .6f;
		light.orientation	= mat3.xRotation(180);
		light.origin			= vec3.zero;
		foreach (pt; Cornell.light.v) {
			light.origin += pt;
		}
		light.origin *= 0.25f;
		light.origin -= vec3(170, 0, 0);
		
		light.size			= vec2(343 - 213, 332 - 227);
		light.spectrum	= Cornell.lightSpectrum;
		
		tracer.lights ~= light;
	}
	
	vec3[]	meshPos;
	uint		meshPosLen;
	vec3[]	meshCol;
	vec3[]	meshNorm;
	int[]		meshMat;
	int[]		meshIndices;
	
	meshPos.alloc(tracer.prims.length * 3);
	meshPosLen = meshPos.length;
	meshCol.alloc(tracer.prims.length * 3);
	meshNorm.alloc(tracer.prims.length * 3);
	meshMat.alloc(tracer.prims.length * 3);
	meshIndices.alloc(tracer.prims.length * 3);
	
	meshCol[] = vec3.one;
	foreach (i, p; prims) {
		int i0 = i*3+0;
		int i1 = i*3+1;
		int i2 = i*3+2;
		meshIndices[i0] = i0;
		meshIndices[i1] = i1;
		meshIndices[i2] = i2;
		meshPos[i0] = p.v[0];
		meshPos[i1] = p.v[1];
		meshPos[i2] = p.v[2];
		meshNorm[i0] = p.n;
		meshNorm[i1] = p.n;
		meshNorm[i2] = p.n;
		meshMat[i0] = tracer.prims[i].materialId;
		meshMat[i1] = tracer.prims[i].materialId;
		meshMat[i2] = tracer.prims[i].materialId;
	}
	
	struct VHashData {
		vec3	position;
		uint	idx;
	}
	alias OCTree!(VHashData) VHashOCTree;

	scope vhash = new VHashOCTree.Tree(AABB(meshPos));
	foreach (i, p; meshPos) {
		vhash.addData(VHashData(p, i));
	}

	meshLog.msg("Computing adjacency ...");
	auto mesh = Mesh.fromTriList(meshIndices);
	mesh.computeAdjacency((vertexI idx, void delegate(vertexI) adjIter) {
		auto v0 = meshPos[idx];
		foreach (data; vhash.findData(v0, 0.01f)) {
			if ((data.position - v0).length < 0.01f) {
				if (idx != data.idx) {
					meshLog.trace("{} is adjacent to {}", idx, data.idx);
					adjIter(cast(vertexI)data.idx);
				}
			}
		}
	});
	meshLog.msg("Done.");

	const float maxEdgeLen = 15.f;

	auto sub = new Subdivider;
	sub.shouldSubdivideEdge = (vertexI a, vertexI b) {
		float dist = (meshPos[a] - meshPos[b]).length;
		meshLog.trace("dist = {}", dist);
		return dist > maxEdgeLen;
	};
	sub.interpAndAdd = (vertexI a, vertexI b, float t) {
		auto va = meshPos[a];
		auto vb = meshPos[b];
		auto vi = va * (1.f - t) + vb * t;
		meshPos.append(vi, &meshPosLen);
		meshNorm.realloc(meshPos.length);
		meshMat.realloc(meshPos.length);
		meshNorm[meshPosLen-1] = (meshNorm[a] * (1.f - t) + meshNorm[b] * t).normalized;
		meshMat[meshPosLen-1] = meshMat[a];
		meshCol.realloc(meshPos.length);
		meshLog.info("subdiv {}, {} -> {}", a, b, meshPosLen-1);
		return cast(vertexI)(meshPosLen-1);
	};
	sub.subdivide(mesh);


	tracer.prepare();
	rng.seed(12345);
	tracer.shootPhotons();
	tracer.precalcRadiancePhotons(taskMngr);
	
	vec3 faceCenter(Face* f) {
		vec3 c = vec3.zero;
		uint num = 0;
		foreach (h; mesh.faceHEdges(f)) {
			c += meshPos[h.vi];
			++num;
		}
		return c * (1f / num);
	}
	
	vec3 hedgeSamplingPos(HEdge* h) {
		uint	num = 0;
		vec3	sum = vec3.zero;
		meshLog.trace("hedge vi: {}", h.vi);
		meshLog.tab;
		foreach (rh; mesh.hedgeRing(h)) {
			sum += faceCenter(rh.face(mesh));
			//sum += meshPos[rh.nextFaceHEdge(mesh).vi];
			//sum += meshPos[rh.vi];
			++num;
			meshLog.trace("ring hedge vi: {} = {}", rh.vi, meshPos[rh.nextFaceHEdge(mesh).vi]);
		}
		meshLog.utab;
		meshLog.trace("ring size: {}", num);
		if ((meshPos[h.vi] - sum * (1f / num)).sqLength !< .01) {
			meshLog.trace("hedge pos: {}, sampling pos: {}", meshPos[h.vi], sum * (1f / num));
		}
		return sum * (1f / num) * 0.5f + meshPos[h.vi] * 0.5f;
	}

	if (tracer.doIndirect) {
		Trace.formatln(`Computing indirect illumination ...`);		
		scope sw = new StopWatch;
		sw.start();

		uint totalPoints = mesh.numHEdges;
		uint currentPoint = 0;
		int prevPercent = 0;
		Trace.format(`0.0 %`).flush;
		
		bool[] posDone;
		posDone.alloc(meshPos.length);
		scope (exit) posDone.free();

		taskMngr.parallel((int taskId) {
			auto slice = taskMngr.getSlice(taskId, totalPoints);
			for (int ptI = slice._0; ptI < slice._1; ++ptI) {
			//for (int ptI = 0; ptI < totalPoints; ++ptI) {
				synchronized(taskMngr) {
					++currentPoint;
					int currentPercent = currentPoint * 1000 / totalPoints;
					if (currentPercent != prevPercent) {
						Trace.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
						prevPercent = currentPercent;
					}
				}

				auto hedge = mesh.hedge(cast(hedgeI)ptI);
				auto vi = hedge.vi;
				if (posDone[vi]) {
					continue;
				} else {
					posDone[vi] = true;
				}
				
				vec3 pos = hedgeSamplingPos(hedge);//meshPos[vi];
				vec3 norm = vec3.unitY;
				auto mat = tracer.materials[meshMat[vi]];
				tracer.computeIlluminance(pos, meshNorm[vi], mat);
			}
		});
		
		Trace.formatln(\n`Done in {} sec ; Irradiance cache size: {}`, sw.stop(), tracer.numIrradCachePoints);
	}

	{
		Trace.formatln(`Computing direct illumination ...`);
		scope sw = new StopWatch;
		sw.start();

		uint totalPoints = mesh.numHEdges;
		uint currentPoint = 0;
		int prevPercent = 0;
		Trace.format(`0.0 %`).flush;
		
		auto xyzData = new vec3d[meshPos.length];
		auto rgbData = meshCol;//new vec3d[totalPoints];
		
		bool[] posDone;
		posDone.alloc(meshPos.length);
		scope (exit) posDone.free();

		taskMngr.parallel((int taskId) {
			auto slice = taskMngr.getSlice(taskId, totalPoints);
			for (int ptI = slice._0; ptI < slice._1; ++ptI) {
			//for (int ptI = 0; ptI < totalPoints; ++ptI) {
				synchronized(taskMngr) {
					++currentPoint;
					int currentPercent = currentPoint * 1000 / totalPoints;
					if (currentPercent != prevPercent) {
						Trace.format(\r`{}.{} %`, currentPercent / 10, currentPercent % 10).flush;
						prevPercent = currentPercent;
					}
				}

				auto hedge = mesh.hedge(cast(hedgeI)ptI);
				auto vi = hedge.vi;
				if (posDone[vi]) {
					continue;
				} else {
					posDone[vi] = true;
				}
				
				vec3 pos = hedgeSamplingPos(hedge);//meshPos[vi];
				vec3 norm = vec3.unitY;
				vec3d xyz = tracer.lightingAtPoint(pos, meshNorm[vi], meshMat[vi]);
				xyzData[vi] = xyz;
			}
		});
		
		foreach (i, xyz; xyzData) {
			if (xyz.ok) {
				vec3d rgb = gammaCorrectSRGB(xyz2srgb(xyz));
				rgbData[i] = vec3.from(rgb);
			}
		}
		
		/+for (uint i = 0; i < totalPoints; ++i) {
			meshCol[mesh.hedge(cast(hedgeI)i).vi] = vec3.from(rgbData[i]);
		}+/

		Trace.formatln(\n`Done in {} sec ; Irradiance cache size: {}`, sw.stop(), tracer.numIrradCachePoints);
	}

	
	auto context = GLWindow();
	context
		.title("Photon mapping test")
		.width(640)
		.height(480)
	.create();
	
	use (context) in (GL gl) {
		gl.MatrixMode(GL_PROJECTION);
		gl.LoadIdentity();
		gl.gluPerspective(90.f, 1.333f, 1f, 10000.f);
		gl.MatrixMode(GL_MODELVIEW);
	};
	
	float yRot = 140.f;
	while (context.created) {
		use(context) in (GL gl) {
			gl.Clear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			gl.LoadIdentity();

			gl.Enable(GL_LINE_SMOOTH);
			gl.Enable(GL_POINT_SMOOTH);
			//gl.Enable(GL_BLEND);
			gl.Enable(GL_DEPTH_TEST);
			gl.Enable(GL_CULL_FACE);
			//gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			gl.LineWidth(1);
			gl.PointSize(4);
			
			yRot += 0.2f;
			mat4 transform = mat4.translation(vec3(0, 0, -500)) * mat4.yRotation(yRot) * mat4.translation(vec3(-280, -200, -280));
			gl.MultMatrixf(transform.ptr);
			
			gl.Begin(GL_TRIANGLES); {
				uint faces = mesh.numFaces;
				for (int i = 0; i < faces; ++i) {
					auto face = mesh.face(cast(faceI)i);
					foreach (fh; mesh.faceHEdges(face)) {
						auto v0 = fh.vi;
						auto v1 = fh.nextFaceHEdge(mesh).vi;
						
						gl.Color3fv(meshCol[v0].ptr);
						gl.Vertex3fv(meshPos[v0].ptr);
						/+gl.Color3fv(meshCol[v1].ptr);
						gl.Vertex3fv(meshPos[v1].ptr);+/
					}
				}
			} gl.End();
		};
		
		context.update().show();
	}
}
