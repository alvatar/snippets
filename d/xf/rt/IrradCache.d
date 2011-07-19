module xf.rt.IrradCache;


private {
	import xf.omg.geom.OCTree;
	import xf.omg.core.LinearAlgebra;
	import xf.omg.geom.AABB;
	import xf.omg.core.Misc;
	import tango.io.Stdout;
}



class IrradCache(Value) {
	static assert (is(Value == struct));
	static assert (is(typeof(Value.opMulAssign(1.f))));
	static assert (is(typeof(Value.opAddAssign(Value.init))));
	static assert (is(typeof(Value.magnitude()) == float));
	
	
	this(AABB box) {
		spatialHash = new SpatialHash(box);
	}
	
	
	bool query(vec3 pos, vec3 normal, float irradAccuracy, Value* res) {
		*res = Value.init;
		float div = 0.f;
		
		const float maxDist = 50.f;
		
		foreach (ref data; spatialHash.findData(pos, maxDist)) {
			float di = dot(pos - data.position, normal + data.normal);
			if (di < maxDist * -.1f) continue;		// the cached value is in front of us
			
			float eps = calcEpsilon(data, pos, normal);
			//Stdout.formatln(`epsilon = {}`, eps);
			
			if (0 == eps) {
				*res = data.value;
				return true;
			}
			
			if (eps <= 1.f / irradAccuracy && eps <>= 0) {
				vec3 off = pos - data.position;
				float w = 1.f / (off.sqLength / data.R0 + sqrt(max(0, 1.f - dot(normal, data.normal))));
				div += w;
				
				// ----
				
				auto tmp = data.value;
				//tmp.light = vec3d.zero;
				
				/+tmp.light.x += off.dot(data.translGradient[0]);
				tmp.light.y += off.dot(data.translGradient[1]);
				tmp.light.z += off.dot(data.translGradient[2]);+/

				tmp *= w;
				*res += tmp;
			}
		}
		
		if (0.f == div) {
			return false;
		} else {
			*res *= 1.f / div;
			return true;
		}
	}
	
	
	void store(vec3 pos, vec3 normal, float R0, Value value, vec3[3] translGradient) {
		assert (R0 > 0);
		float illum = value.magnitude();
		assert (R0 <>= 0);
		spatialHash.addData(CacheItem(pos, normal, value, illum, R0, translGradient));
	}
	
	
	float calcEpsilon(CacheItem cached, vec3 pos, vec3 normal) {
		float res = //cached.illuminance * (
			4 / pi * (pos - cached.position).length / cached.R0
			+ sqrt(max(0, 2.0 - 2 * dot(normal, cached.normal)))
		;//);
		
		if (res !<>= 0) {
			Stdout.formatln(`pos:{} normal:{} | cached: pos:{} normal:{} illum:{} R0:{}, normdot:{}`,
				pos.toString, normal.toString, cached.position.toString, cached.normal.toString, cached.illuminance, cached.R0, dot(normal, cached.normal));
			assert (false);
		}
		
		return res;
	}	
	

	private {
		struct CacheItem {
			vec3		position;
			vec3		normal;
			Value	value;
			float		illuminance;
			float		R0;		// harmonic mean of distances from the position at normal
			vec3[3]	translGradient;
		}
		
		
		alias OCTree!(CacheItem).Tree	SpatialHash;
		SpatialHash								spatialHash;
	}
}
