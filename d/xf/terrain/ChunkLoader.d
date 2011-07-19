module xf.terrain.ChunkLoader;

private {
	import xf.terrain.Chunk;
	import xf.terrain.ChunkData;
	import xf.omg.core.LinearAlgebra;
}


interface IChunkHandler {
	void	alloc(int);
	bool	loaded(int);
	void	load(int, Chunk*, ChunkData);
	void	unload(int);
	void	free();
}


class ChunkLoader {
	abstract Chunk*	root();
	abstract void			loadPendingChunks();
	abstract vec2i		fullResHeightmapSize();
	abstract void			getFullResHeightmap(ushort[] heights);
	abstract int			getIndex(Chunk* ch);
	
	void addChunkHandler(IChunkHandler h) {
		_chunkHandlers ~= h;
	}

	protected {
		IChunkHandler[]	_chunkHandlers;
	}
}
