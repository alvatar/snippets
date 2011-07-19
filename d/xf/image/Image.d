module xf.image.Image;

private {
	import xf.utils.Memory;
	version(OldMath) import xf.maths.Vec : vec2i;
	else import xf.omg.core.LinearAlgebra : vec2i;
}



enum ImageFormat {
	Grayscale,
	RGB,
	RGBA
}


enum DataFormat {
	Byte,
	SignedByte,
	Short,
	Float
}


struct ImageRequest {
	ImageFormat	imageFormat = ImageFormat.RGBA;
	DataFormat	dataFormat = DataFormat.Byte;
	
	hash_t toHash() {
		uint hash;
		hash += imageFormat * 4;
		hash += dataFormat;
		return hash;
	}

	int opCmp(ImageRequest* rhs) {
		int cmp = imageFormat - rhs.imageFormat;
		if (0 != cmp) return cmp;
		return dataFormat - rhs.dataFormat;
	}
}


class ImagePlane {
	ubyte[]	data;
	char[]	source;

	int	width;
	int	height;
	int	depth;
	bool	opaque = true;
	
	
	ImagePlane dup() {
		auto res = new ImagePlane;
		res.data.alloc(data.length);
		res.data[] = data[];
		res.source = source;
		res.width = width;
		res.height = height;
		res.depth = depth;
		res.opaque = opaque;
		return res;
	}
	
	
	vec2i size() {
		return vec2i(width, height);
	}
	
	
	this() {}
	
	
	this(ubyte[] data, char[] source, int width, int height, int depth) {
		this.data = data;
		this.source = source;
		this.width = width;
		this.height = height;
		this.depth = depth;
	}
	
	
	~this() {
		data.free();
	}
}


class Image {
	ImagePlane[]	planes;
	ImageFormat		imageFormat	= ImageFormat.RGB;
	DataFormat		dataFormat	= DataFormat.Byte;
	
	Image dup() {
		Image res = new Image;
		foreach (p; planes) {
			res.planes ~= p.dup;
		}
		res.imageFormat = imageFormat;
		res.dataFormat = dataFormat;
		return res;
	}
}
