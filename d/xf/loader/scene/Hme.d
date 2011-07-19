module xf.loader.scene.Hme;

private {
	import xf.utils.OldCfg;
	
	import xf.loader.scene.model.all;
	import xf.loader.scene.hme.HmeMaterial;
	
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.CoordSys;
	
	import xf.utils.Memory;
	
	import xf.omg.geom.NormalQuantizer;
	
	import xf.utils.log.Log;
	import xf.utils.log.ConsoleAppender;
	
	import tango.io.FilePath;
	import tango.io.device.File;
	
	import xf.omg.mesh.TangentSpaceCalc;
}

mixin LibraryLog!("hmeLog", "HMEDebug", LogLevel.Trace, LogLevel.Info, LogLevel.Warn, LogLevel.Error, LogLevel.Fatal);

HmeLoader hmeLoader(char[] filename)
{
	auto cache = hmeCache();
	return cache.get(filename);
}

private HmeCache hmeCache()
{
	if ( HmeCache.instance is null )  HmeCache.instance = new HmeCache();
	return HmeCache.instance;
}

private class HmeCache {
	static HmeCache instance;
	HmeLoader[char[] ]  cached;
	
	HmeLoader get(char[] filename){
		if ( (filename in cached ) is null ) {			
			auto loader = new HmeLoader;
			loader.load(filename);
			cached[filename] = loader;
		} else {
			hmeLog.info("HME file from cache: {}", filename);
		}
		
		return cached[filename];
	}
}


class HmeLoader {
	void load(char[] filename) {
		hmeLog.info("loading HME file: {}", filename);
		
		scope cfgLdr = new CfgLoader;
		auto data = cast(char[])File.get(filename);
		auto hme = cfgLdr.loadFromMem(data).result();
		delete data;
		
		hmeLog.info("  parsed");
		
		hmeDir = FilePath(filename).path;
		scene = new Scene;
		
		for (int i = 0; i < hme.count(`material`); ++i) {
			auto mat = loadMaterial(hme.child(`material`, i));
			materials[hme.child(`material`, i).string_(`name`)] = mat;
			scene.materials ~= mat;
		}
		
		for (int i = 0; i < hme.count(`node`); ++i) {
			scene.nodes ~= loadNodeTree(hme.child(`node`, i));
		}

		hmeLog.info("  loaded");
		
		foreach (node; scene.nodes) {
			checkMesh(node);
		}
	}
	
	void checkMesh(Node node) {
		foreach (n; &node.filterChildren!(Node)) {
			checkMesh( n);
		}
		
		foreach (m; &node.filterChildren!(Mesh)) {
			checkMesh(m);
		}
	}
	
	void checkMesh(Mesh mesh) {
		version (HmeComputeTangents) {
			if(mesh._normals.length > mesh._tangents.length && mesh._texCoords.length > 0 && mesh._texCoords[0].length > 0)
			{
				mesh.allocTangents(mesh._normals.length);
				mesh.allocBitangents(mesh._normals.length);
				
				hmeLog.info(" Recalculating tangents and bitangents");
				computeMeshTangents ( 
					mesh._indices,
					mesh._positions,
					mesh._normals,
					mesh._texCoords[0],
					mesh._tangents,
					mesh._bitangents
				);
				hmeLog.info(" done ");
			}
		}
	}
	
	
	float		scale = 1.f;
	Scene	scene;
	
	
protected:


	Material loadMaterial(Array hme) {
		switch (hme.string_(`type`)) {
			case `standard`: {
				return loadStandardMaterial(hme);
			} break;
			
			default: return null;
		}
	}
	
	
	Material loadStandardMaterial(Array hme) {
		auto res = new Standard3dsMaxMaterial;
		
		void loadMap(char[] name, inout Map map) {
			if (hme.hasChild(name)) with (hme.child(name)) {
				switch (string_(`type`)) {
					case "bitmap": {
						map.texture = translatePath(string_(`bitmap`));
					} break;
					
					default: {
						assert (false, "TODO");
					}
				}
				
				if (count(name ~ `Amount`) > 0) {
					map.amount = float_(name ~ `Amount`) * 0.01f;
				} else {
					map.amount = 0.f;
				}
				
				map.enabled = true;
			} else {
				map.enabled = false;
			}
		}
		
		loadMap(`diffuseMap`, res.diffuseMap);
		loadMap(`ambientMap`, res.ambientMap);
		
		res.opacity = hme.float_(`opacity`) * 0.01f;
		res.diffuse = parseColor(hme, `diffuse`);
		res.specular = parseColor(hme, `specular`);
		res.ambient = parseColor(hme, `ambient`);
		
		// TODO: load the rest of 3dsmax' stuff
		
		return res;
	}
	
	
	vec3 parseColor(Array hme, char[] name) {
		return vec3(hme.float_(name, 0, 0), hme.float_(name, 0, 1), hme.float_(name, 0, 2)) * (1.f / 255);
	}


	Node loadNodeTree(Array hme) {
		Node n = loadNode(hme);
		
		for (int i = 0; i < hme.count(`node`); ++i) {
			auto ch = loadNodeTree(hme.child(`node`, i));
			auto cs = ch.localCS;
			n.attachChild(ch);
			ch.setTransform(cs, n.localCS);
		}
		
		return n;
	}
	
	
	// BUG: colors can't have alpha channels with the way Hme currently works.
	void loadNodeMesh(Mesh mesh, Array hme, vec3 meshScale) {
		with (hme) {
			vec3[] vertices;
			scope (exit) vertices.free();

			with (child(`verts`, 0, 1)) {
				int numVerts = count();
				hmeLog.trace("    {} vertices", numVerts);
				
				vertices.alloc(numVerts);
				vec3_(vertices);
				
				foreach (inout v; vertices) {
					v *= scale;
					v.x *= meshScale.x;
					v.y *= meshScale.y;
					v.z *= meshScale.z;
				}
			}
			
			
			vec3i[]	faces;
			scope (exit) faces.free();
			
			vec3[]	normals;
			scope (exit) normals.free();
			

			with (child(`faces`, 0, 1)) {
				int numFaces = count();
				hmeLog.trace("    {} faces", numFaces);
	
				faces.alloc(numFaces);
				vec3i_(faces);
			}

			with (child(`faceVertexNormals`)) {
				normals.alloc(faces.length * 3);
				vec3_(normals);
			}

			
			uint[] indices = (cast(uint*)faces.ptr)[0 .. faces.length * 3];
			foreach (i; indices) assert (i < vertices.length);

			
			uint[]	normalIndices;
			scope (exit) normalIndices.free();
			
			{
				uint[QuantizedNormal]	normalHash;
				uint	numNormals = 0;
				
				normalIndices.alloc(indices.length);
				
				vec3[]	quantizedNormals;
				quantizedNormals.alloc(indices.length);
				
				foreach (int ni, vec3 n; normals) {
					QuantizedNormal qn = QuantizedNormal(n);
					uint* fnd = qn in normalHash;
					
					if (fnd !is null) {
						normalIndices[ni] = *fnd;
					} else {
						normalIndices[ni] = numNormals;
						normalHash[qn] = numNormals;
						quantizedNormals[numNormals] = n;
						++numNormals;
					}
				}
				
				quantizedNormals.realloc(numNormals);
				
				
				float maxError = 0f;
				foreach (i, ni; normalIndices) {
					float error = (normals[i] - quantizedNormals[ni]).length;
					if (error > maxError) maxError = error;
				}
				
				normals.free();
				normals = quantizedNormals;
			}
			
			
			assert (normalIndices.length == indices.length);
			
			
			int numMaps = int_(`maps`);
			hmeLog.trace("    {} maps", numMaps);
			
			
			vec3[][]	texCoords;
			uint[][]		texCoordIndices;
			texCoords.alloc(numMaps);
			texCoordIndices.alloc(numMaps);

			scope (exit) {
				foreach (inout tci; texCoordIndices) {
					if (tci !is null) tci.free();
				}
				texCoordIndices.free();

				foreach (inout tc; texCoords) {
					if (tc !is null) tc.free();
				}
				texCoords.free();
			}
			
			
			uint numNonEmptyMaps = 0;
			
			for (int mapId = 0; mapId < numMaps; ++mapId)  {
				if (hasChild(`map`, mapId)) with (child(`map`, mapId)) {
					++numNonEmptyMaps;
					
					hmeLog.trace("    {} map", mapId);
					with (child(`verts`, 0, 1)) {
						int numVerts = count();
						hmeLog.trace("      {} vertices", numVerts);

						texCoords[mapId].alloc(numVerts);
						vec3_(texCoords[mapId]);
					}
					
					
					with (child(`faces`, 0, 1)) {
						int numFaces = count();
						hmeLog.trace("      {} faces", numFaces);

						assert (numFaces == faces.length);
						
						texCoordIndices[mapId].alloc(numFaces * 3);
						vec3i_((cast(vec3i*)texCoordIndices[mapId].ptr)[0 .. numFaces]);
					}
				} else {
					hmeLog.trace("    map {} is empty", mapId);
					assert (texCoords[mapId] is null);		// sanity check. we will use this property later
				}
			}

			
			// now fix indices
			
			uint[][]	propertyIndices;
			
			propertyIndices.alloc(2 + numNonEmptyMaps);
			scope (exit) propertyIndices.free();
			
			propertyIndices[0] = indices;
			propertyIndices[1] = normalIndices;
			
			// find non-empty index lists and put them to the propertyIndex array
			{
				uint dst = 0;
				for (int mapId = 0; mapId < numMaps; ++mapId)  {
					if (texCoords[mapId] !is null) {
						propertyIndices[2+dst] = texCoordIndices[mapId];
						++dst;
					}
				}
				assert (dst == numNonEmptyMaps);
			}
			
			uint[]	finalIndices;
			finalIndices.alloc(indices.length);

			remapIndices(propertyIndices, finalIndices);
			uint maxIndex = 0; {
				foreach (i; finalIndices) if (i > maxIndex) maxIndex = i;
			}
			
			hmeLog.trace("    {} unique vertices", maxIndex+1);

			vec3[] positions;
			positions.alloc(maxIndex+1);
			
			vec3[] vnormals;
			vnormals.alloc(maxIndex+1);

			vec3[] colors;
			if (texCoords.length > 0 && texCoords[0] !is null) {
				colors.alloc	(maxIndex+1);
			}
			
			vec3[][] coords;
			if (texCoords.length > 1) {
				coords.alloc(texCoords.length-1);
				foreach (ti, t; texCoords[1..$]) {
					if (t !is null) {
						coords[ti].alloc(maxIndex+1);
					}
				}
			}
			scope (exit) coords.free();		// they will be converted to an array of streams.
			

			foreach (src, dst; finalIndices) {
				vec3 v = vertices[indices[src]];
				positions[dst] = v;

				vec3 n = normals[normalIndices[src]];
				vnormals[dst] = n;
			}

			if (texCoords.length > 0 && texCoords[0] !is null) {
				foreach (src, dst; finalIndices) {
					vec3 v = texCoords[0][texCoordIndices[0][src]];
					colors[dst] = v;
				}
			}

			if (texCoords.length > 1) {
				foreach (ti, t; texCoords[1..$]) {
					if (t !is null) {
						foreach (src, dst; finalIndices) {
							vec3 v = t[texCoordIndices[ti+1][src]];
							coords[ti][dst] = v;
						}
					}
				}
			}
			
			
			mesh.allocIndices(finalIndices.length);
			mesh.indices()[] = finalIndices[];
			
			mesh.allocPositions(positions.length);
			mesh.positions()[] = positions[];
			
			mesh.allocNormals(vnormals.length);
			mesh.normals()[] = vnormals[];
			
			if (colors.length) {
				mesh.allocColors(colors.length);
				foreach (i, inout c; mesh.colors()) c=colors[i].swizzle!("xyz1");
			}
			
			foreach (i, tc; coords) {
				mesh.allocTexCoords(i, tc.length);
				mesh.texCoords(i)[] = tc[];
			}
		}
	}
	
	
	void remapIndices(uint[][] propertyIndices, uint[] finalIndices)
	in {
		assert (finalIndices.length > 0);
		
		foreach (pi; propertyIndices) {
			assert (pi.length == finalIndices.length);
		}
	}
	out {
		// to be written
	}
	body {
		if (1 == propertyIndices.length) {
			finalIndices[] = propertyIndices[0][];
			return;
		}
		
		uint[] buffer;
		buffer.alloc(finalIndices.length);
		buffer[] = propertyIndices[0][];
		
		foreach (uint[] prop; propertyIndices[1..$]) {
			remapIndicesWorker(buffer, prop, finalIndices);
			
			if (prop is propertyIndices[$-1]) break;
			buffer[] = finalIndices[];
		}
		
		buffer.free();
	}
	
	
	void remapIndicesWorker(uint[] srcA, uint[] srcB, uint[] dst)
	in {
		assert (srcA.length == srcB.length);
		assert (srcB.length == dst.length);
		assert (srcA !is srcB && srcA !is dst && srcB !is dst);
	}
	body {
		struct IndexUnion {
			union {
				long cmpVal;
			}
			struct {
				uint a, b;
			}
			
			static IndexUnion opCall(uint a, uint b) {
				IndexUnion res;
				res.a = a;
				res.b = b;
				return res;
			}
			
			uint toHash() {
				return (a << 16) + (a & 0xffff0000) + b;
			}
			
			int opCmp(IndexUnion* rhs) {
				if (a > rhs.a) return 1;
				if (a < rhs.a) return -1;
				if (b > rhs.b) return 1;
				if (b < rhs.b) return -1;
				return 0;
			}
		}
		
		
		uint[IndexUnion] newIndices;
		uint newI = 0;
		
		for (uint i = 0; i < srcA.length; ++i) {
			auto iu = IndexUnion(srcA[i], srcB[i]);
			uint* indexP = iu in newIndices;
			
			if (indexP is null) {
				newIndices[iu] = newI;
				dst[i] = newI;
				++newI;
			} else {
				dst[i] = *indexP;
			}
		}
	}
	
	
	CoordSys loadCoordSys(Array hme) {
		vec4 v = hme.vec4_(`rot`);
		quat q = quat(v.x, -v.z, v.y, v.w);
		vec3 axis;
		real angle;

		q.getAxisAngle(axis, angle);
		
		axis.normalize();
		
		return CoordSys(vec3fi.from(hme.vec3_(`pos`) * scale), quat.axisRotation(vec3(axis.x, axis.z, -axis.y), -angle));
	}
	
	
	Node loadNode(Array hme) {
		Node node = new Node;

		node.name = hme.string_(`name`);
		node.type = hme.string_(`type`);

		hmeLog.trace("    node {} ({})", node.name, node.type);

		vec3 meshScale = vec3(1, 1, 1);
		CoordSys meshCS = CoordSys.identity;
		
		if (hme.hasChild(`transform`)) with (hme.child(`transform`)) {
			node.setTransform(loadCoordSys(hme.child(`transform`)));
			meshScale = vec3_(`scl`);
			/+rgNode.position			= vec3fi.init.convert(vec3_(`pos`));		// BUG: precission loss	// .init is used to sidestep a bug in dmd.172
			rgNode.rotation.xyzw	= vec4_(`rot`);
			rgNode.scale				= vec3_(`scl`);+/
		}

		
		Mesh firstMesh;
		
		if (hme.hasChild(`mesh`)) {
			Mesh mesh = new Mesh;
			loadNodeMesh(mesh, hme.child(`mesh`), meshScale);
			
			//node.meshes ~= mesh;
			node.attachChild(mesh);
			if (firstMesh is null) {
				firstMesh = mesh;
			}

			char[] materialName = "undefined";
			
			if (hme.hasField(`material`)) materialName = hme.string_(`material`);
			hmeLog.trace("    material {}", materialName);
		
			if (materialName != "undefined" && materialName in materials) {
				mesh.material = materials[materialName];
			}
		}

		node.animation = loadAnimation(hme, firstMesh);
		
		int childCount = hme.int_(`children`);
		hmeLog.trace("    {} children", childCount);
		
		return node;
	}
	
	
	Animation loadAnimation(Array hme, Mesh mesh) {
		if (hme.hasChild(`physique`)) {
			assert (hme.hasChild(`mesh`));	// physique needs the original indices to vertex positions.
			
			return loadPhysique(hme.child(`physique`), hme.child(`mesh`), mesh);
		}
		
		return null;
	}
	
	
	SkeletalAnimation loadPhysique(Array physique, Array rawMesh, Mesh mesh) {
		hmeLog.trace("    physique {");
		
		auto res = new SkeletalAnimation;
		
		int numBones = physique.tupleLength(`bones`);
		res.allocBones(numBones);
		Bone[] bones = res.bones;
		
		hmeLog.trace("      {} bones", numBones);
		
		foreach (i, inout bone; bones) {
			{
				bone.name = physique.string_(`bones`, 0, i);
				bone.parentId = physique.int_(`boneParents`, 0, i);
			}
			
			
			{
				auto motion = physique.child(`boneMotion`, i);
				int numKeys = motion.count(`key`);
				
				bone.allocKeyframes(numKeys);
				
				foreach (keyi, inout key; bone.keyframes) {
					key.time = motion.float_(`key`, keyi);
					key.coordSys = loadCoordSys(motion.child(`key`, keyi, 1));
				}
			}
		}
		
		int numVerts = physique.int_(`weights`);
		assert (numVerts == physique.int_(`offsets`));
		res.allocVertices(numVerts);

		hmeLog.trace("      {} vertices", numVerts);
		
		double avgBonesPerVertex = 0.0;
		
		AnimVertex[]	vertices = res.vertices; {
			auto vWeights	= physique.child(`weights`, 0, 1);
			auto vOffsets		= physique.child(`offsets`, 0, 1);
			
			foreach (i, inout v; vertices) {
				int vbones = vWeights.tupleLength(i) / 2;				// they are [id weight] tuples
				assert (vbones == vOffsets.tupleLength(i) / 4);	// they are [id x y z] tuples
				
				v.allocBones(vbones);
				
				avgBonesPerVertex += cast(double)vbones / numVerts;
				
				for (int j = 0; j < vbones; ++j) {
					v.bones[j] = vWeights.int_(i, j*2);
					v.weights[j] = vWeights.float_(i, j*2+1);
					v.offsets[j] = vOffsets.vec3_(i, j*4+1);
				}
			}
		}
		
		hmeLog.trace("      {} bones/vertex avg",  avgBonesPerVertex);		
		hmeLog.trace("    }");
		
		physiqueDuplicateVertices(res, rawMesh, mesh);
		
		return res;
	}


	/**
		The reasoning behind this function is that the mesh loading process may duplicate some vertices in order to build a HW-friendly
		representation. The vertices in Physique on the other hand, correspond to the original positions. Solution: duplicate some physique vertices.
	*/
	private void physiqueDuplicateVertices(SkeletalAnimation anim, Array rawMesh, Mesh mesh) 
	in {
		assert (anim !is null);
		assert (mesh !is null);
	}
	out {
		assert (anim.vertices.length == mesh.positions.length);
	}
	body {
		vec3i[]	faces;
		scope (exit) faces.free();
		
		with (rawMesh.child(`faces`, 0, 1)) {
			int numFaces = count();
			faces.alloc(numFaces);
			vec3i_(faces);
		}
		
		AnimVertex[]	av;
		av.alloc(mesh.positions.length);
		
		foreach (vin, int vi; (cast(int*)faces.ptr)[0..faces.length*3]) {
			int newvi = mesh.indices[vin];
			av[newvi] = anim.vertices[vi].clone();
		}
		
		anim.overrideVertices_(av);
	}
	

	char[] translatePath(char[] filename) {
		/+ TODO
		if (std.file.isabs(filename)) return filename;
		else return std.path.join(hmeDir, filename);+/
		return filename;
	}
	
	
	char[]				hmeDir;
	Material[char[]]	materials;
}
