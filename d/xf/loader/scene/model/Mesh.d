module xf.loader.scene.model.Mesh;

private {
	import xf.loader.scene.model.WorldEntity;
	import xf.loader.scene.model.Material;
	import xf.omg.core.LinearAlgebra;
	import xf.omg.rt.Common : Ray, Hit;
	import xf.omg.geom.Triangle : intersectTriangle;
	import xf.utils.Memory;
}



class Mesh : WorldEntity {
	Material		material;		
	
	// vertex indices and params
	final uint[]		indices() {
		return _indices;
	}
	
	final vec3[]		positions() {
		return _positions;
	}
	
	final vec3[]		normals() {
		return _normals;
	}
	final vec3[]		tangents() {
		return _tangents;
	}
	final vec3[]		bitangents() {
		return _bitangents;
	}
	
	final vec3[]	texCoords(int i) {
		return _texCoords[i];
	}
	
	final int numTexCoords() {
		return _texCoords.length;
	}
	
	final vec4[]		colors() {
		return _colors;
	}
	


	void allocIndices(int n) {
		_indices.alloc(n);
	}
	
	void allocPositions(int n) {
		_positions.alloc(n);
	}
	
	void allocNormals(int n) {
		_normals.alloc(n);
	}
	
	void allocTangents(int n) {
		_tangents.alloc(n);
	}
	
	void allocBitangents(int n) {
		_bitangents.alloc(n);
	}
	
	void allocTexCoords(int i, int n) {
		if (_texCoords.length <= i) _texCoords.alloc(i+1);
		_texCoords[i].alloc(n);
	}
	
	void allocColors(int n) {
		_colors.alloc(n);
	}
	

	override bool intersect(Ray r, Hit* hit) {
		float dist = float.max;
		if (hit !is null) {
			dist = hit.distance;
		}

		for (int i = 0; i+2 < indices.length; i += 3) {
			vec3[3] verts = void;
			verts[0] = positions[indices[i+0]];
			verts[1] = positions[indices[i+1]];
			verts[2] = positions[indices[i+2]];
			
			if (intersectTriangle(verts[], r.origin, r.direction, dist)) {
				if (hit !is null) {
					hit.distance = dist;
				}
				return true;
			}
		}
		
		return false;
	}
	
	
	~this() {
		_indices.free();
		_positions.free();
		_normals.free();
		_tangents.free();
		_bitangents.free();
		foreach (inout vec3[] tc; _texCoords) tc.free();
		_texCoords.free();
		_colors.free();
	}

	public {
		uint[]		_indices;
		
		vec3[]		_positions;
		
		vec3[]		_normals;
		vec3[]		_tangents;
		vec3[]		_bitangents;
		
		vec3[][]	_texCoords;
		
		vec4[]		_colors;
	}
}
