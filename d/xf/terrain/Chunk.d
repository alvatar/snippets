module xf.terrain.Chunk;

private {
	import xf.omg.core.LinearAlgebra;
	import xf.omg.core.Misc;
}



struct Chunk {
	union Neighbor {
		Chunk*	chunk;
		uint		label;
	}
	
	enum Flags {
		Split = 1,
	}

	Chunk*			parent;
	Chunk*[4]		children;
	Neighbor[4]	neighbors;
	float				error;
	float				minH;
	float				maxH;
	Flags				flags;


	bool split() {
		return (flags & Flags.Split) != 0;
	}
	
	void split(bool b) {
		if (b) {
			flags |= Flags.Split;
		} else {
			flags &= ~Flags.Split;
		}
	}
	

	bool isLeaf() {
		return children[0] is null;
	}
	
	
	void getChildPositions(vec2 pos, float halfSize, vec2[4]* chpos) {
		(*chpos)[] = pos;
		float chSiz = halfSize * .5f;
		(*chpos)[0].x -= chSiz;
		(*chpos)[0].y += chSiz;
		(*chpos)[1].x += chSiz;
		(*chpos)[1].y += chSiz;
		(*chpos)[2].x += chSiz;
		(*chpos)[2].y -= chSiz;
		(*chpos)[3].x -= chSiz;
		(*chpos)[3].y -= chSiz;
	}
	
	
	void optimize(vec3 viewer, vec2 pos, float halfSize, float maxError) {
		if (isLeaf) {
			split = false;
			return;
		}
		
		float verticalDist = 0.f;
		if (viewer.y > maxH) {
			verticalDist = viewer.y - maxH;
		} else if (viewer.y < minH) {
			verticalDist = minH - viewer.y;
		}
		
		vec3 distv = vec3(max(0, abs(pos.x - viewer.x) - halfSize), verticalDist, max(0, abs(pos.y - viewer.z) - halfSize));
		float dist = distv.length + float.epsilon;
		
		float err = this.error / dist;
		if (err > maxError) {
			vec2[4] chpos;
			getChildPositions(pos, halfSize, &chpos);
			
			foreach (chi, ch; children) {
				assert (ch !is null);
				ch.optimize(viewer, chpos[chi], halfSize * .5f, maxError);
			}
			
			split = true;
		} else {
			split = false;
		}
	}
}
