module xf.image.CachedLoader;

private {
	import xf.image.Image;
	import xf.image.Loader;
	import xf.image.Log;
	import xf.utils.StructClass;
	import tango.io.vfs.model.Vfs;
}



class CachedLoader : Loader {
	this (Loader cached) {
		assert (cached !is null);
		this.cached = cached;
	}
	
	
	override Image load(char[] filename, ImageRequest* req = null) {
		CacheKey key = CacheKey(filename, req is null ? ImageRequest.init : *req);
		
		{
			Image* c = key in cache;
			if (c !is null) {
				assert (*c !is null);
				imageLogger.debug4("Giving {} from cache", filename);
				return *c;
			}
		}
		
		Image im = cached.load(filename, req);
		if (im !is null) {
			cache[key] = im;
			imageLogger.info("{} loaded and cached", filename);
		} else {
			imageLogger.warn("Cannot find file: {} " , filename);
		}
		
		return im;
	}
	
	
	override void useVfs(VfsFolder vfs) {
		cached.useVfs(vfs);
	}
	
	
	protected {
		Loader					cached;
		Image[CacheKey]	cache;
	}
}


private struct CacheKey {
	char[]				source;
	ImageRequest	req;
	mixin simpleStructCtor;
	
	hash_t toHash() {
		uint hash;
		foreach (char c; source) hash = (hash * 9) + c;
		return hash + req.toHash;
	}
	
	bool opEquals(CacheKey* other) {
		return source == other.source && req == other.req;
	}

	int opCmp(CacheKey* rhs) {
		if (auto strCmp = typeid(char[]).compare(cast(void*)&source, cast(void*)&rhs.source)) {
			return strCmp;
		}
		return req.opCmp(&rhs.req);
	}	
}
