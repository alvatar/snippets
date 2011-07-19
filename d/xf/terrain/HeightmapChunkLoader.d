module xf.terrain.HeightmapChunkLoader;

private {
	import xf.terrain.Chunk;
	import xf.terrain.ChunkLoader;
	import xf.terrain.ChunkData;
	
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.Misc;
	import xf.omg.geom.AABB;
	
	import xf.image.Image;
	import xf.image.DevilLoader;
	
	import xf.utils.Memory;
	
	import tango.text.convert.Format;
	import tango.io.Stdout;
}



class HeightmapChunkLoader : ChunkLoader {
	void unloadAll() {
	}
	
	
	void load(char[] heightMapPath, int maxChunkSize) {
		unloadAll();
		
		_maxChunkSize = maxChunkSize;
		
		auto ldr = new DevilLoader;
		auto img = _heightMap = ldr.load(heightMapPath);
		if (img is null) {
			throw new Exception("Could not load heightmap image: '" ~ heightMapPath ~ "'");
		}
		assert (ImageFormat.Grayscale == img.imageFormat);
		
		Stdout.formatln("HeightmapChunkLoader: got a {}x{} {} {} img",
			img.planes[0].width, img.planes[0].height,
			(["grayscale", "rgb", "rgba"])[img.imageFormat],
			(["ubyte", "byte", "ushort", "float"])[img.dataFormat]
		);
		
		load();
	}
	
	
	override Chunk* root() {
		return &_chunks[0];
	}
	
	
	override int getIndex(Chunk* ch) {
		return ch - _chunks.ptr;
	}
	
	
	// BUG: the terrain should be centered in the middle of the heightmap. right now the pivot is on its far left edge
	override void loadPendingChunks() {
		void worker(Chunk* chunk, vec2i from, vec2i isize, vec2 pos, float halfSize) {
			if (chunk.split) {
				vec2[4] chpos;
				chunk.getChildPositions(pos, halfSize, &chpos);

				vec2i csize = isize / 2;
				worker(chunk.children[0], vec2i(from.x, from.y), csize+vec2i.one, chpos[0], halfSize * .5f);
				worker(chunk.children[1], vec2i(from.x+csize.x, from.y), csize+vec2i.one, chpos[1], halfSize * .5f);
				worker(chunk.children[2], vec2i(from.x+csize.x, from.y+csize.y), csize+vec2i.one, chpos[2], halfSize * .5f);
				worker(chunk.children[3], vec2i(from.x, from.y+csize.y), csize+vec2i.one, chpos[3], halfSize * .5f);
			} else {
				int idx = chunk - _chunks.ptr;
				
				bool needLoad = false;
				foreach (h; _chunkHandlers) {
					if (!h.loaded(idx)) {
						needLoad = true;
						break;
					}
				}
				
				if (needLoad) {
					getChunkData(from, isize, pos, halfSize, (ChunkData data) {
						foreach (h; _chunkHandlers) {
							if (!h.loaded(idx)) {
								h.load(idx, chunk, data);
							}
						}
					});
				}
			}
		}
		
		worker(root, vec2i.zero, vec2i(this.width, this.depth), vec2(.5f, .5f), .5f);
	}
	
	
	override vec2i fullResHeightmapSize() {
		return vec2i(width, depth);
	}
	
	
	override void getFullResHeightmap(ushort[] heights) {
		auto data =_heightMap.planes[0].data;

		switch (_heightMap.dataFormat) {
			case DataFormat.Short: {
				heights[] = cast(ushort[])data;
			} break;

			case DataFormat.Byte: {
				foreach (i, ref h; heights) {
					h = cast(ushort)data[i] * 256;
				}
			} break;
		}
	}
	
	
	private {
		class HeightChunkData(T) : ChunkData {
			vec3[]		_positions;
			ushort[]	_indices;
			
			
			private float getSkirtHeight(Chunk2DSlice!(T) data, vec2i pos_, int dir, int skip) {
				float res = cast(float)data[pos_] / T.max;
				
				const int maxNeighborLevelDifference = 2;
				
				skip *= (1 << maxNeighborLevelDifference);
				for (int steps_ = 1, diff = -maxNeighborLevelDifference; diff <= maxNeighborLevelDifference; steps_ *= 2, ++diff) {
					if (diff != 0) {
						int steps = max(1, steps_ >> maxNeighborLevelDifference);
						for (int i = -steps; i <= steps; ++i) {
							if (0 == i) continue;

							vec2i pos = pos_;
							pos.cell[dir] += skip * i;
							
							if (auto h = pos in data) {
								res = min(res, cast(float)*h / T.max);
							}
						}
					}
					
					if (abs(skip) > 1) {
						skip /= 2;
					}
				}
				
				return res;
			}
			
			
			this (Chunk2DSlice!(T) data, vec2 center, float halfSize) {
				int wskip = (data.xl-1) / _maxChunkSize;
				int dskip = (data.yl-1) / _maxChunkSize;
				int numPts = (_maxChunkSize+1) * (_maxChunkSize+1);
				int numTris = _maxChunkSize * _maxChunkSize * 2;
				
				int numSkirtPts = _maxChunkSize * 4;
				int numSkirtTris = numSkirtPts * 2;
				
				//Stdout.formatln("xl={} yl={} wskip={} dskip={} pts={} tris={}", data.xl, data.yl, wskip, dskip, numPts, numTris);
				
				assert (numPts < ushort.max);
				
				_positions.alloc(numPts + numSkirtPts);
				_indices.alloc((numTris + numSkirtTris) * 3);
				
				float xinc = halfSize * 2 / _maxChunkSize;
				float zinc = -xinc;
				vec2	p0 = center + vec2(-halfSize, halfSize);

				int posIt = 0; {
					int zsrc = 0;
					for (int zi = 0; zi <= _maxChunkSize; ++zi, zsrc += dskip) {
						float z = p0.y + zi * zinc;
						
						auto row = data.row(zsrc);
						
						int xsrc = 0;
						for (int xi = 0; xi <= _maxChunkSize; ++xi, xsrc += wskip) {
							float x = p0.x + xi * xinc;
							
							auto yraw = row[xsrc];
							float y = cast(float)yraw / T.max;
							
							_positions[posIt++] = vec3(x, y, z);
						}
					}
				}
				
				int firstSkirtIdx = posIt;
				
				void findSkirtPositions(vec2i start, int dir, int skip) {
					vec3 getPos(float h) {
						return vec3(p0.x + start.x * 2 * halfSize / (data.xl-1), h, p0.y - start.y * 2 * halfSize / (data.yl-1));
					}
					
					for (int i = 0; i < _maxChunkSize; ++i, start.cell[dir] += skip) {
						float h = getSkirtHeight(data, start, dir, skip);
						if (0 == i || _maxChunkSize-1 == i) {
							h = min(h, getSkirtHeight(data, start, dir^1, skip));
						}
						_positions[posIt++] = getPos(h);
					}
				}
				
				findSkirtPositions(vec2i.zero, 0, wskip);
				findSkirtPositions(vec2i(data.xl-1, 0), 1, dskip);
				findSkirtPositions(vec2i(data.xl-1, data.yl-1), 0, -wskip);
				findSkirtPositions(vec2i(0, data.yl-1), 1, -dskip);

				assert (posIt == _positions.length);
				
				int idxIt = 0; {
					int idx = 0;
					for (int zi = 0; zi < _maxChunkSize; ++zi) {
						for (int xi = 0; xi < _maxChunkSize; ++xi, ++idx) {
							int i0 = idx;
							int i1 = idx+1;
							int i2 = idx+_maxChunkSize+1+1;
							int i3 = idx+_maxChunkSize+1;
							_indices[idxIt++] = i0;
							_indices[idxIt++] = i1;
							_indices[idxIt++] = i2;
							_indices[idxIt++] = i2;
							_indices[idxIt++] = i3;
							_indices[idxIt++] = i0;
						}
						++idx;
					}
				}
				
				void findSkirtIndices(int skirtStart, int start, int stride) {
					void add(int i0, int i1) {
						int i2 = start+stride;
						int i3 = start;
						assert (i0 < _indices.length);
						assert (i1 < _indices.length);
						assert (i2 < _indices.length);
						assert (i3 < _indices.length);
						_indices[idxIt++] = i0;
						_indices[idxIt++] = i1;
						_indices[idxIt++] = i2;
						_indices[idxIt++] = i2;
						_indices[idxIt++] = i3;
						_indices[idxIt++] = i0;
					}
					
					for (int i = 0; i < _maxChunkSize-1; ++i, ++skirtStart, start += stride) {
						int i0 = skirtStart;
						int i1 = skirtStart+1;
						add(i0, i1);
					}

					int i0 = skirtStart;
					int i1 = skirtStart+1;
					if (i1 == firstSkirtIdx+4*_maxChunkSize) {
						i1 = firstSkirtIdx;
					}
					add(i0, i1);
				}
				
				findSkirtIndices(firstSkirtIdx+0*_maxChunkSize, 0, 1);
				findSkirtIndices(firstSkirtIdx+1*_maxChunkSize, _maxChunkSize, _maxChunkSize+1);
				findSkirtIndices(firstSkirtIdx+2*_maxChunkSize, numPts-1, -1);
				findSkirtIndices(firstSkirtIdx+3*_maxChunkSize, _maxChunkSize * (_maxChunkSize+1), -(_maxChunkSize+1));
				
				assert (idxIt == _indices.length);
			}
			
			
			~this() {
				_positions.free();
				_indices.free();
			}
			
			
			override bool hasExplicitIndices() {
				return true;
			}
			
			override uint numIndices() {
				return _indices.length;
			}
			
			override IndexType nativeIndexType() {
				assert (numIndices <= ushort.max);
				return IndexType.Ushort;
			}
			
			override void getIndices(ushort[] ind) {
				ind[] = _indices;
			}
			
			override void getIndices(uint[]) {
				assert (false, `TODO`);
			}
			
			override HeightType nativeHeightType() {
				static if (is(T == ushort)) {
					return HeightType.Ushort;
				} else static if (is(T == ubyte)) {
					return HeightType.Ubyte;
				} else static assert (false);
			}
			
			override void getHeights(ChunkSlice, ubyte[]) {
				assert (false, `TODO`);
			}
			
			override void getHeights(ChunkSlice, ushort[]) {
				assert (false, `TODO`);
			}
			
			override void getHeights(ChunkSlice, float[]) {
				assert (false, `TODO`);
			}
			
			override float heightAtPoint(vec2) {
				assert (false, `TODO`);
				return 0.f;
			}
			
			override float heightAtPoint(vec2us) {
				assert (false, `TODO`);
				return 0.f;
			}

			override bool hasExplicitPositions() {
				return true;
			}
			
			override uint numPositions() {
				return _positions.length;
			}
			
			override void getPositions(vec3ub[]) {
				assert (false, `TODO`);
			}
			
			override void getPositions(vec3us[]) {
				assert (false, `TODO`);
			}
			
			override void getPositions(vec3[] pos) {
				pos[] = _positions;
			}
			
			override bool hasExplicitTexCoords() {
				assert (false, `TODO`);
				return false;
			}
			
			override void getTexCoords(ChunkSlice, vec2[]) {
				assert (false, `TODO`);
			}
		}
		
		
		void getChunkData(vec2i pos, vec2i size, vec2 center, float halfSize, void delegate(ChunkData) dg) {
			switch (_heightMap.dataFormat) {
				case DataFormat.Short: {
					scope data = new HeightChunkData!(ushort)(slice2D!(ushort)(pos, size), center, halfSize);
					dg(data);
				} break;

				case DataFormat.Byte: {
					scope data = new HeightChunkData!(ubyte)(slice2D!(ubyte)(pos, size), center, halfSize);
					dg(data);
				} break;
			}
		}
		
		
		void load() {
			allocChunks();
			Stdout.formatln("Allocated {} chunks for terrain", _chunks.length);
			
			createChunks(0, vec2i.zero, vec2i(width, depth));			
			Stdout.formatln("Terrain height bounds: [{}; {}]", _chunks[0].minH, _chunks[0].maxH);
		}
		
		
		int width() {
			return _heightMap.planes[0].width;
		}
		

		int depth() {
			return _heightMap.planes[0].height;
		}
		
		
		private static struct Chunk2DSlice(T) {
			private {
				T[]	data;
				uint	width;
				uint	height;
				uint	x, y, xl, yl;
			}
			
			T[] row(uint r) {
				assert (r < yl);
				assert (r+y < height);
				int off = (y+r) * width + x;
				return data[off .. off + xl];
			}
			
			T opIndex(vec2i pos) {
				auto res = opIn_r(pos);
				assert (res !is null);
				return *res;
			}
			
			char[] toString() {
				return Format("{}x{} slice of {}x{} terrain, starting at {}x{}", xl, yl, width, height, x, y);
			}
			
			T* opIn_r(vec2i pos) {
				if (		pos.x < xl
					&&	pos.y < yl
					&&	pos.x >= 0
					&&	pos.y >= 0
				) {
					return &data[(y+pos.y) * width + x+pos.x];
				} else {
					return null;
				}
			}
			
			int opApply(int delegate(ref T) dg) {
				final T* endRow = &data.ptr[(y+yl)*width + x];
				for (T* row = &data[y*width+x]; row < endRow; row += width) {
					final T* colEnd = row + width;
					for (T* col = row; col < colEnd; ++col) {
						if (auto res = dg(*col)) {
							return res;
						}
					}
				}
				return 0;
			}
		}
		
		
		Chunk2DSlice!(T) slice2D(T)(vec2i from, vec2i size) {
			assert (from.x >= 0);
			assert (from.y >= 0);
			assert (size.x > 0);
			assert (size.y > 0);
			assert (from.x + size.x <= width);
			assert (from.y + size.y <= depth);
			return Chunk2DSlice!(T)(cast(T[])_heightMap.planes[0].data, width, depth, from.x, from.y, size.x, size.y);
		}
		
		
		protected override void addChunkHandler(IChunkHandler h) {
			super.addChunkHandler(h);
			h.alloc(_chunks.length);
		}

		
		void allocChunks() {
			int splits = 0;
			for (int w = width-1, d = depth-1; w > _maxChunkSize || d > _maxChunkSize; w /= 2, d /= 2) {
				++splits;
				if (1 == (w & 1) || 1 == (d & 1)) {
					throw new Exception("Heightmap side's size must be maxChunkSize * 2^k + 1");
				}
				if (w < 1 || d < 1) {
					throw new Exception("Bad terrain proportions. Unable to subdivide chunks within the maxChunkSize bounds");
				}
			}
			
			_quadTreeDepth = splits+1;
			
			int totalChunks = 1;
			for (int s = 0; s < splits; ++s) {
				totalChunks += 4 * totalChunks;
			}
			
			_chunks.alloc(totalChunks);
			foreach (h; _chunkHandlers) {
				h.alloc(totalChunks);
			}
		}
		
		
		// if this turns out a bottlenec, it can be memoized
		void chunksAtLevel(int level, int* from, int* num) {
			*from = 0;
			*num = 1;
			while (--level) {
				*from += *num;
				*num <<= 2;
			}
		}
		

		void findBox(vec2i from, vec2i size, float* minH, float* maxH) {
			*minH = float.max;
			*maxH = -float.max;
			
			void process(float h) {
				if (h > *maxH) *maxH = h;
				if (h < *minH) *minH = h;
			}
			
			switch (_heightMap.dataFormat) {
				case DataFormat.Short: {
					foreach (h; slice2D!(ushort)(from, size)) {
						process(cast(float)h / ushort.max);
					}
				} break;

				case DataFormat.Byte: {
					foreach (h; slice2D!(ubyte)(from, size)) {
						process(cast(float)h / ubyte.max);
					}
				} break;
			}
		}

		
		void createChunk(int idx, Chunk* parent, vec2i from, vec2i size, int level) {
			auto chunk = &_chunks[idx];
			chunk.parent = parent;
			
			if (hasChildren(idx)) {
				chunk.minH = float.max;
				chunk.maxH = -float.max;
				foreach (chidx; chunkChildren(idx)) {
					auto ch = &_chunks[chidx];
					chunk.minH = min(chunk.minH, ch.minH);
					chunk.maxH = max(chunk.maxH, ch.maxH);
				}
			} else {
				findBox(from, size, &chunk.minH, &chunk.maxH);
			}

			// HACK: some more elaborate error calculation would be useful
			{	int depth = _quadTreeDepth - level - 1;
				chunk.error = 0 == depth ? 0 : (.5f * cast(float)size.x / width);
			}
		}
		
		
		Chunk* createChunks(int chunk, vec2i from, vec2i size, int level = 0) {
			auto ch = &_chunks[chunk];

			int child = firstChild(chunk);
			if (child < _chunks.length) {
				auto csize = size / 2;
				ch.children[0] = createChunks(child+0, vec2i(from.x, from.y), csize+vec2i.one, level+1);
				ch.children[1] = createChunks(child+1, vec2i(from.x+csize.x, from.y), csize+vec2i.one, level+1);
				ch.children[2] = createChunks(child+2, vec2i(from.x+csize.x, from.y+csize.y), csize+vec2i.one, level+1);
				ch.children[3] = createChunks(child+3, vec2i(from.x, from.y+csize.y), csize+vec2i.one, level+1);
			}

			createChunk(chunk, ch, from, size, level);
			return ch;
		}
		
		
		// subsequent children go ccw from the one at [0, 0] offset
		static int firstChild(int chunkIdx) {
			return chunkIdx * 4 + 1;
		}
		
		
		ChunkChildIndexIter chunkChildren(int idx) {
			return ChunkChildIndexIter(idx);
		}
		
		
		bool hasChildren(int idx) {
			return idx * 4 + 1 < _chunks.length;
		}
		
		
		struct ChunkChildIndexIter {
			int chunkIdx;
			int opApply(int delegate(ref int) dg) {
				int x = firstChild(chunkIdx);
				if (auto res = dg(x)) return res; else ++x;
				if (auto res = dg(x)) return res; else ++x;
				if (auto res = dg(x)) return res; else ++x;
				return dg(x);
			}
		}
	}
	
	
	private {
		int			_maxChunkSize;	// grid squares on a side
		Image		_heightMap;
		Chunk[]	_chunks;
		int			_quadTreeDepth;
	}
}
