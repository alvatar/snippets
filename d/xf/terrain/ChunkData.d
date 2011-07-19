module xf.terrain.ChunkData;

private {
	import xf.omg.core.LinearAlgebra;
	
	alias Vector!(ushort, 2)	vec2us;
	alias Vector!(ushort, 3)	vec3us;
}



struct ChunkSlice {
	uint	x, length, y;
}


abstract class ChunkData {
	enum IndexType {
		Ushort, Uint
	}
	
	enum HeightType {
		Ubyte, Ushort, Float
	}	
	
	bool				hasExplicitIndices();
	uint				numIndices();
	IndexType		nativeIndexType();
	void				getIndices(ushort[]);
	void				getIndices(uint[]);
	
	HeightType	nativeHeightType();
	void				getHeights(ChunkSlice, ubyte[]);
	void				getHeights(ChunkSlice, ushort[]);
	void				getHeights(ChunkSlice, float[]);
	
	float				heightAtPoint(vec2);
	float				heightAtPoint(vec2us);

	bool	hasExplicitPositions();
	uint	numPositions();
	void	getPositions(vec3ub[]);
	void	getPositions(vec3us[]);
	void	getPositions(vec3[]);
	
	bool	hasExplicitTexCoords();
	void	getTexCoords(ChunkSlice, vec2[]);
	
	// TODO: textures
}
