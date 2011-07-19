module xf.terrain.ChunkedTerrain;

private {
	import xf.terrain.Chunk;
	import xf.terrain.ChunkLoader;
	import xf.omg.core.LinearAlgebra : vec3, vec2, vec2i;
}



class ChunkedTerrain {
	this (ChunkLoader loader) {
		_loader = loader;
	}
	
	
	void optimize(vec3 viewer, float maxError) {
		viewer -= this.position;
		viewer.x /= scale.x;
		viewer.y /= scale.y;
		viewer.z /= scale.z;
		root.optimize(viewer, vec2(.5f, .5f), .5f, maxError);
		_loader.loadPendingChunks;
	}
	
	
	int getIndex(Chunk* ch) {
		return _loader.getIndex(ch);
	}
	
	
	Chunk* root() {
		return _loader.root;
	}
	
	
	vec2i fullResHeightmapSize() {
		return _loader.fullResHeightmapSize;
	}
	
	
	void getFullResHeightmap(ushort[] heights) {
		_loader.getFullResHeightmap(heights);
	}
	

	void addChunkHandler(IChunkHandler h) {
		_loader.addChunkHandler(h);
	}
	
	
	void iterLeafChunks(void delegate(Chunk*, vec3) dg) {
		void iter(Chunk* ch, vec3 pos, float halfSize) {
			if (ch !is null) {
				if (!ch.split) {
					dg(ch, pos);
				} else {
					vec2[4] chpos;
					ch.getChildPositions(vec2(pos.x, pos.z), halfSize, &chpos);
					
					foreach (i, c; ch.children) {
						iter(c, vec3(chpos[i].x, 0, chpos[i].y), halfSize * .5f);
					}
				}
			}
		}
		
		iter(root, vec3(.5f, 0.f, .5f), .5f);
	}

	
	public {
		vec3	scale = vec3.one;
		vec3	position = vec3.one;
	}
	
	private {
		ChunkLoader	_loader;
	}
}
