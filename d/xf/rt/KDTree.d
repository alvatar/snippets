module xf.rt.KDTree;

private {
	import xf.rt.Math;
	import xf.rt.Misc;
	import xf.omg.geom.AABB;
	
	import tango.stdc.stdlib : malloc;
	import tango.core.Traits : ParameterTupleOf;
	import xf.utils.Memory;
	
	import tango.io.Stdout;
}



struct KDTreeData {
	uint[] indices;
	static assert(KDTreeData.sizeof == 8);
}



struct KDTreeNode(Prim) {
	final void splitPos(float pos) {
		splitPos_ = pos;
	}
		
	final float splitPos() {
		return splitPos_;
	}
	
	final void left(typeof(this) c) {
		data_ = cast(uint)c + (data_ & 7);
	}
	
	final typeof(this) left() {
		return cast(typeof(this))(data_ & 0xfffffff8);
	}
	
	final typeof(this) right() {
		return left + 1;
	}
	
	final void axis(int a) {
		data_ = (data_ & 0xfffffffc) + a;
	}
	
	final uint axis() {
		return data_ & 3;
	}
	
	final bool leaf() {
		return ((data_ & 4) > 0) != 0 ? true : false;
	}
	
	final void leaf(bool isLeaf) {
		data_ = (isLeaf) ? (data_ | 4) : (data_ & 0xfffffffb);
	}
		
	final KDTreeData* leafData() {
		return cast(KDTreeData*)cast(void*)left();
	}
	
	final void leafData(KDTreeData* ptr) {
		left(cast(typeof(this))cast(void*)ptr);
	}
	
	final AABB leftChildBox(AABB thisBox) {
		thisBox.max.cell[axis] = splitPos;
		return thisBox;
	}
	
	final AABB rightChildBox(AABB thisBox) {
		thisBox.min.cell[axis] = splitPos;
		return thisBox;
	}

package:
	float	splitPos_;
	uint	data_;
}


class KDTree(Prim) {
	alias KDTreeNode!(Prim) NodeType;



	Builder_!(typeof(SrcPrims[0])) build(SrcPrims, StupidIFTI)(SrcPrims srcPrims, StupidIFTI primConv) {
		static assert (is(StupidIFTI == Prim delegate(typeof(SrcPrims[0]))));
		return new Builder_!(typeof(SrcPrims[0]))(srcPrims, primConv);
	}
	

	class Builder_(SrcPrim) {
		private {
			NodeType[]		nodePool;
			KDTreeData[]	leafPool;
			
			NodeType[]		allNodes;
			KDTreeData[]	allLeaves;
			
			SrcPrim[]			srcPrims;
			
			Prim delegate(SrcPrim) primConv;

			
			this(SrcPrim[] srcPrims, Prim delegate(SrcPrim) primConv) {
				this.primConv = primConv;
				this.srcPrims = srcPrims;
				this.outer.prims.alloc(srcPrims.length);
				
				int prims = 0;
				foreach (i, prim; srcPrims) {
					auto conv = primConv(prim);
					if (conv != Prim.init) {
						this.outer.prims[prims++] = conv;
					}
				}
				this.outer.prims.realloc(prims);
				/+foreach(i, ref prim; this.outer.prims) {
				}+/
				
				ubyte* alignedAlloc(uint allocSize) {
					ubyte* memPool = cast(ubyte*)malloc(allocSize);
					
					for (ubyte* p = memPool; p != memPool + allocSize; ++p) {
						*p = 0;
					}
					
					ubyte* res = cast(ubyte*)((cast(uint)memPool + 32) & (0xffffffff - 31));
					assert (0 == (cast(uint)res & 31));
					return res;
				}
				
				uint allocSize = NodeType.sizeof * (2 << maxTreeDepth);
				ubyte* memPool = alignedAlloc(allocSize);
				allNodes = nodePool = (cast(NodeType*)memPool)[0 .. (2 << maxTreeDepth) - 1];
				
				//writefln("Node pool allocated");


				allocSize = KDTreeData.sizeof * (2 << (maxTreeDepth - 1));
				memPool = alignedAlloc(allocSize);
				allLeaves = leafPool = (cast(KDTreeData*)memPool)[0 .. (2 << (maxTreeDepth - 1)) - 1];
				
				//writefln("Leaf pool allocated");
			}
			

			NodeType* newNodePair() {
				NodeType* ret = &nodePool[0];
				nodePool = nodePool[2 .. $];
				assert (ret.data_ == 0);
				assert ((ret+1).data_ == 0);
				assert (((cast(uint)cast(void*)ret) & 7) == 0);
				return ret;
			}


			KDTreeData* newLeaf() {
				KDTreeData* ret = &leafPool[0];
				leafPool = leafPool[1 .. $];
				assert(ret.indices is null);
				assert(ret.indices.length == 0);
				assert (((cast(uint)cast(void*)ret) & 7) == 0);
				return ret;
			}
		}

		public {
			bool	testBoxIntersections	= false;		// setting this to true seems to cause nightmares
			float	intersectCost				= 1.0f;
			float	traversalCost				= 0.3f;
			uint	maxTreeDepth			= 20;
			uint	numPrimsToCheck		= 1000;
			uint	minTrisPerLeaf			= 3;
		}
		
		
		void build() {
			assert (prims.length > 0);
			
			this.outer.root = newNodePair();
			AABB box;
			foreach (p; srcPrims) {
				box.expand(p.calcAABB);
			}
				
			//writefln("extending the kDTree box");
			
			/+vec3 boxExpand = vec3(1, 1, 1) * 100_000;
			box.min	-= boxExpand;
			box.max	+= boxExpand;+/
				
			this.outer.box = box;
			uint[] indices;
			indices.alloc(prims.length);
			
			for (uint i = 0; i < indices.length; ++i) {
				indices[i] = i;
			}
			
			build(root, indices, box, 1);
			if (indices) {
				//writefln("kD-Tree :: deleting indices");
				indices.free();
			}
		}
		
		
		struct SplitPlane {
			float	pos;
			int	axis;
		}
		
		
		void build(NodeType* res, ref uint[] indices, AABB box, uint depth, SplitPlane[] splitsUsed ...) {
			//Stdout.formatln("KDTree.build called; box={}<->{}, depth={}, indices.length={}", box.min, box.max, depth, indices.length);
			
			debug {
				foreach (i; indices) {
					assert (i < srcPrims.length);
				}
			}
						
			void makeLeaf() {
				Stdout.formatln(`Putting {} indices into the leaf`, indices.length);
				
				assert (indices.length > 0);
				
				KDTreeData* leafData = newLeaf();
				leafData.indices = indices;
				
				// it actually stops the calling function from deleting indices
				indices = null; assert (0 == indices.length);
				
				assert (leafData.indices.length > 0);
				res.leafData = leafData;
				res.leaf = true;
				return;
			}

			if (depth >= maxTreeDepth || indices.length <= minTrisPerLeaf) {
				Stdout.formatln("depth={} indices.length={}; making a single leaf.", depth, indices.length);
				return makeLeaf();
			}

			// Calculate the split axis
			int axis = 2; {
				vec3 s = box.halfSize;
				if ((s.x >= s.y) && (s.x >= s.z)) {
					axis = 0;
				} else if ((s.y >= s.x) && (s.y >= s.z)) {
					axis = 1;
				}
			}
			
			//Stdout.formatln("split axis: {}", axis);
			
			res.axis = axis;
			Plane axisPlane = Plane(0, 0, 0, 0);
			axisPlane.normal.cell[axis] = 1;
			
			uint numPrimsToCheck = this.numPrimsToCheck;
			if (indices.length < numPrimsToCheck) {
				numPrimsToCheck = indices.length;
			}
			
			struct PrimRange {
				float	min;
				float	max;
			}
			PrimRange[] primRanges;
			primRanges.alloc(indices.length, false);

			uint checkIndex(int i) {
				return cast(uint)(i * (cast(double)numPrimsToCheck / indices.length));
			}
			
			// Find all possible split positions, sort them
			float[] splitPositions;
			splitPositions.alloc(numPrimsToCheck * 2, false);
			{
				uint tmp = 0;
				int lastIndex = -1;

				foreach (i, pindex; indices) {
					PrimRange range;
					srcPrims[pindex].calcRange(axisPlane, &range.min, &range.max);
					primRanges[i] = range;
					
					uint index = checkIndex(i);
					if (lastIndex != index) {
						splitPositions[tmp++] = range.min;
						splitPositions[tmp++] = range.max;
						lastIndex = index;
					}
				}
				
				splitPositions.realloc(tmp);
				splitPositions.sort;
			}
			
			//Stdout.formatln("split positions: {}", splitPositions);
			
			//writefln("Finding the optimal split plane");
			
			float nonSplitCost = intersectCost * indices.length;
			float invSA = 1.f / box.area;
			
			float bestCost	= float.max;
			float bestPos		= float.nan;

			{
				// Exploit the property that split positions are sorted, no need to check srcPrims that have been marked as not intersecting the right box
				bool[] skipPrims;
				skipPrims.alloc(indices.length);
				
				// boxes for the new left and right nodes
				AABB leftBox = box;
				AABB rightBox = box;
					
				float lastPos = float.max;
				foreach (i, splitPos; splitPositions) {
					// this split pos has already been considered, skip it
					if (splitPos == lastPos) continue;
					else lastPos = splitPos;
					
					// srcPrims in the left and right boxes
					uint leftPrims = 0, rightPrims = 0;
					
					//if (i % 10 == 0) writefln("%1.1f%%", 100.f * cast(float)i / splitPositions.length);

					// calculate the left and right boxes
					rightBox.min.cell[axis] = leftBox.max.cell[axis] = splitPos;
					
					// calculate the number of srcPrims in left and right boxes
					foreach (uint pindex, uint i_; indices) {
						if (!skipPrims[pindex]) {
							auto p = &srcPrims[i_];
							if (primRanges[pindex].min <= leftBox.max.cell[axis]) {// && (!testBoxIntersections || p.intersect(leftBox))) {
								++leftPrims;
							}
							
							if (primRanges[pindex].max >= rightBox.min.cell[axis]) {// && (!testBoxIntersections || p.intersect(rightBox))) {
								++rightPrims;
							} else {
								skipPrims[pindex] = true;
							}
						} else {
							++leftPrims;
						}
					}

					// surface area of left and right boxes
					float leftSA	= leftBox.area();
					float rightSA	= rightBox.area();
					
					// finally calculate the split cost
					float cost = traversalCost + intersectCost * invSA * (leftSA * leftPrims + rightSA * rightPrims);
					//Stdout.formatln("split cost: {} @ {}", cost, splitPos);

					if (cost < bestCost) {
						bool alreadyUsed = false;
						foreach (spl; splitsUsed) {
							if (spl.axis == axis && spl.pos == splitPos) {
								alreadyUsed = true;
								break;
							}
						}
						
						if (!alreadyUsed) {
							bestCost = cost;
							bestPos = splitPos;
						}
					}
				}
				
				skipPrims.free();
			}
			
			//Stdout.formatln("bestCost: {} @ {}", bestCost, bestPos);
			
			splitPositions.free();
			

			if (!(bestPos <>= 0) || bestCost >= nonSplitCost) {
				primRanges.free();
				KDTreeData* leafData = newLeaf();
				leafData.indices = indices;
				
				// it actually stops the calling function from deleting indices
				indices = null;
				
				assert (0 == indices.length);
				assert (0 != leafData.indices.length);
				res.leafData = leafData;
				res.leaf = true;
				return;
			} else {
				res.leaf = false;
			}
			
			float splitPos = bestPos;
			res.splitPos = splitPos;
			
			uint[] indicesLeft;
			uint[] indicesRight;
			
			// calculate the left and right boxes
			AABB leftBox		= box;
			AABB rightBox	= box;
			rightBox.min.cell[axis] = leftBox.max.cell[axis] = splitPos;

			AABB newLeftBox, newRightBox;

			foreach (uint index, uint i_; indices) {
				auto range = &primRanges[index];
				auto p = &srcPrims[i_];
				
				if (	range.max >= leftBox.min.cell[axis] && range.min <= leftBox.max.cell[axis]
						&& (!testBoxIntersections || p.intersect(leftBox)))
				{
					indicesLeft.append(i_);
					newLeftBox.expand(p.calcAABB);
				}

				if (	range.max >= rightBox.min.cell[axis] && range.min <= rightBox.max.cell[axis]
						&& (!testBoxIntersections || p.intersect(rightBox)))
				{
					indicesRight.append(i_);
					newRightBox.expand(p.calcAABB);
				}
			}
			
			//Stdout.formatln("leftBox: {}<->{} ; rightBox: {}<->{}", leftBox.min, leftBox.max, rightBox.min, rightBox.max);

			if (0 == indicesLeft.length || 0 == indicesRight.length) {
				indicesLeft.free();
				indicesRight.free();
				Stdout.formatln("extreme splitting plane chosen; making a single leaf.");
				return makeLeaf();
			} else {
				Stdout.formatln("split into {} and {} indices", indicesLeft.length, indicesRight.length);
			}

			NodeType* children = newNodePair();
			res.left = children;
			
			primRanges.free();
			
			build((children+0), indicesLeft, leftBox, depth+1, splitsUsed ~ SplitPlane(splitPos, axis));
			if (indicesLeft) indicesLeft.free();
			
			build((children+1), indicesRight, rightBox, depth+1, splitsUsed ~ SplitPlane(splitPos, axis));
			if (indicesRight) indicesRight.free();
			
			return res;
		}
	}


	
	struct KDStack {
		NodeType* node;
		float t;
		vec3 pb;
		int prev, pad1, pad2;
	}
	static assert (KDStack.sizeof == 32);
	
	//KDStack[64]	travStack;
	int[5]			mod3 = [0, 1, 2, 0, 1];
	const float		intersectionEpsilon = .001f;
	
	
	// make sure dist is the largest distance in which you want to find the intersection
	// at the same time, dist should be as small as possible to avoid precission problems
	final bool intersect(ref Ray ray, inout Hit hit)
	in {
		assert (hit.dist <>= 0);  // check if dist is not a NaN
		assert (hit.dist <= float.max * 0.5f);
	}
	body {
		KDStack[64] travStack;

		float tnear = 0, tfar = hit.dist, t;
		vec3 p1 = box.min;
		vec3 p2 = box.max;
		
		// early out
		for (int i = 0; i < 3; ++i) {
			if (ray.dir.cell[i] < 0) {
				if (ray.orig.cell[i] < p1.cell[i]) {
					return false;
				}
			}
			else if (ray.orig.cell[i] > p2.cell[i]) {
				return false;
			}
		}
			
		// clip ray segment to box
		for (int i = 0; i < 3; ++i) {
			float pos = ray.orig.cell[i] + tfar * ray.dir.cell[i];
			if (!(pos <= float.max)) return false;
				
			if (ray.dir.cell[i] < 0) {
				// clip end point
				if (pos < p1.cell[i]) tfar = tnear + (tfar - tnear) * ((ray.orig.cell[i] - p1.cell[i]) / (ray.orig.cell[i] - pos));
				// clip start point
				if (ray.orig.cell[i] > p2.cell[i]) { //tnear += (tfar - tnear) * ((ray.orig.cell[i] - p2.cell[i]) / (tfar * ray.dir.cell[i]));
					float tnear2 = (ray.orig.cell[i] - p2.cell[i]) / ray.dir.cell[i];
					if (tnear2 > tnear) tnear = tnear2;
				}
			} else {
				// clip end point
				if (pos > p2.cell[i]) tfar = tnear + (tfar - tnear) * ((p2.cell[i] - ray.orig.cell[i]) / (pos - ray.orig.cell[i]));
				// clip start point
				if (ray.orig.cell[i] < p1.cell[i]) { //tnear += (tfar - tnear) * ((p1.cell[i] - ray.orig.cell[i]) / (tfar * ray.dir.cell[i]));
					float tnear2 = (p1.cell[i] - ray.orig.cell[i]) / ray.dir.cell[i];
					if (tnear2 > tnear) tnear = tnear2;
				}
			}
			
			if (tnear > tfar) {
				return false;
			}
		}
		
		// init traversal
		NodeType* farchild, currnode;
		currnode = root;

		int entrypoint = 0;
		travStack[entrypoint].t = tnear;
		
		// distinguish between internal and external origin
		if (tnear > 0.0f) {
			travStack[entrypoint].pb = ray.orig + ray.dir * tnear;
		} else {
			travStack[entrypoint].pb = ray.orig;
		}
		
		int exitpoint = 1;
		travStack[exitpoint].t = tfar;
		travStack[exitpoint].pb = ray.orig + ray.dir * tfar;
		travStack[exitpoint].node = null;
		
		// traverse kd-tree
		while (currnode !is null) {
			while (!currnode.leaf) {
				float splitpos = currnode.splitPos;
				int axis = currnode.axis;
				
				if (travStack[entrypoint].pb.cell[axis] <= splitpos) {
					if (travStack[exitpoint].pb.cell[axis] <= splitpos) {
						// case N1, N2, N3, P5, Z2 and Z3
						currnode = currnode.left;
						continue;
					}
					
					if (travStack[exitpoint].pb.cell[axis] == splitpos) {
						// case Z1
						currnode = currnode.right;
						continue;
					}
					
					// case N4
					farchild = currnode.right;
					currnode = currnode.left;
				}
				else {		// travStack[entrypoint].pb.cell[axis] > splitpos
					if (travStack[exitpoint].pb.cell[axis] > splitpos) {
						// case P1, P2, P3 and N5
						currnode = currnode.right;
						continue;
					}
					
					// case P4
					farchild = currnode.left;
					currnode = currnode.right;
				}
				// case P4 or N4 ... traverse both children
				
				t = (splitpos - ray.orig.cell[axis]) / ray.dir.cell[axis];

				int tmp = exitpoint++;
				if (exitpoint == entrypoint) {
					++exitpoint;
				}
				
				travStack[exitpoint].prev = tmp;
				travStack[exitpoint].t = t;
				travStack[exitpoint].node = farchild;
				travStack[exitpoint].pb.cell[axis] = splitpos;
				
				int nextaxis = mod3[axis + 1];
				int prevaxis = mod3[axis + 2];
				
				travStack[exitpoint].pb.cell[nextaxis] = ray.orig.cell[nextaxis] + t * ray.dir.cell[nextaxis];
				travStack[exitpoint].pb.cell[prevaxis] = ray.orig.cell[prevaxis] + t * ray.dir.cell[prevaxis];
			}
			
			// current node is the leaf ... emtpy or full
			// intersect ray with each object in the object list, discarding
			// those lying before travStack[entrypoint].t or father than travStack[exitpoint].t
			
			KDTreeData* list = currnode.leafData;
			assert (currnode.leaf);
			assert (((cast(uint)list) & 7) == 0);
			
			Hit h;
			h.dist = travStack[exitpoint].t + intersectionEpsilon;
			
			bool anyIntersection = false;

			foreach (uint i; list.indices) {
				//Stdout.formatln(`indexing {} prims at {}`, prims.length, i);
				auto prim = &prims[i];
				
				//float d = bestDist;
				//Stdout.formatln(`prim normal: {} ; bestHit.dist: {:3} maxDist: {:3}`, prim.calcNormal.toString, bestHit.dist, h.dist);

				//h.dist = bestDist;
				if (prim.intersect(ray, h)) {
					//Stdout.formatln(`intersection with prim`);
					anyIntersection = true;
					//bestDist = d;
					//bestHit = h;
				} else {
					//Stdout.formatln(`no intersection with prim`);
				}
			}
			
			if (anyIntersection) {
				//dist = bestDist;
				hit = h;
				return true;
			}
			
			// pop from the stack
			entrypoint = exitpoint;
			
			// retrieve the pointer to the next node, it is possible that ray traversal terminates
			currnode = travStack[exitpoint].node;
			exitpoint = travStack[entrypoint].prev;
		}
		
		return false;
	}


	NodeType*	root;
	AABB			box;
	Prim[]			prims;


	/+this(Prim[] prims) {
		/+this();

		//uint start = SDL_GetTicks();
		
		this.prims = prims;

		root = MemoryManager.newNodePair();
		AABB box;
		foreach (Prim prim; prims)
			box.fit(prim.calcAABB);
			
		writefln("extending the kDTree box");
		box.min	-= vec3(1000, 1000, 1000);			
		box.max	+= vec3(1000, 1000, 1000);			
			
		this.box = box;
		uint[] indices;
		Alloc!(uint)(indices, prims.length);
		for (uint i = 0; i < indices.length; ++i)
			indices[i] = i;
		root.build(prims, indices, box, 1);
		if (indices) {
			writefln("kD-Tree :: deleting indices");
			Free!(uint)(indices);
		}
		
		debug verify();

		//uint stop = SDL_GetTicks();
		//writefln("kD-Tree construction time: %2.2f s", cast(double)(stop - start) / 1000);+/
	}+/
	
	/+debug {
		void verify() {
			writefln("Verifying the kD-Tree");
			
			/+if (&MemoryManager.allLeaves[0] is &MemoryManager.leafPool[0]) {
				throw new Exception("No leaf node has been allocated for the kD-Tree !!!");
			}
			
			for (KDTreeData *d = &MemoryManager.allLeaves[0]; d != &MemoryManager.leafPool[0]; ++d) {
				foreach (uint index; d.indices) {
					if (index > prims.length) throw new Exception("kD-Tree corrupt");
				}
			}+/
			
			writefln("kD-Tree verified !");
		}
	}+/
	
	static assert((void*).sizeof == 4);
	static assert(NodeType.sizeof == 8);
}
