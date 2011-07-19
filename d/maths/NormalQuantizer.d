module xf.maths.NormalQuantizer;

private {
	import xf.maths.Vec : vec3;
}


private const numAxisCells = 64;
static assert (numAxisCells*numAxisCells*numAxisCells-1 <= uint.max);


struct QuantizedNormal {
	vec3	vec;
	
	
	static QuantizedNormal opCall(vec3 vec) {
		QuantizedNormal res;
		res.vec = vec;
		return res;
	}
	
	
	uint toHash() {
		uint res;
		
		uint cell(float f) {
			if (f <= -1f) return 0;
			if (f >= 1f) return numAxisCells - 1;
			uint res = cast(uint)(cast(float)numAxisCells * (f * 0.5f + 0.5f));
			
			assert (res <= numAxisCells);
			
			if (res == numAxisCells) return res - 1;
			return res;
		}
		
		res = cell(vec.x);
		res *= numAxisCells;
		res += cell(vec.y);
		res *= numAxisCells;
		res += cell(vec.z);
		return res;
	}
	
	
	// we want to quantize normals, not hash them exactly. trick the AA :P
	int opCmp(QuantizedNormal* rhs) {
		return toHash - rhs.toHash;
	}
}

