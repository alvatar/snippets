module xf.image.DevilLoader;

private {
	import xf.image.Image;
	import xf.image.Loader;
	
	import derelict.devil.il;
	import derelict.util.exception; 
	
	import xf.utils.Memory;
	
	import tango.io.device.File;
	import tango.io.FilePath;
	import tango.io.vfs.model.Vfs;
	import tango.core.Exception : VfsException;
	import tango.text.convert.Format;
}



// handle missing DevIL functions 
bool handleMissingDevIL(char[] libname, char[] procName) {
	static char[][] ignoreMethods = ["ilDeleteImage", "ilGenImage", "ilDetermineTypeF", "ilModAlpha", "ilSetAlpha"];
			
	foreach (m; ignoreMethods) {
		if (procName == m) {
			return true;
		}
	}
	
	return false;
}


class DevilLoader : Loader {
	
	// TODO: 1d, 3d and cube textures
	// TODO: convert to fileWrapper
	
	override Image load(char[] filename, ImageRequest* req = null) {
		// TODO: generalize this
		ilEnable(IL_ORIGIN_SET);
		ilOriginFunc(IL_ORIGIN_LOWER_LEFT);
		
		Image result = new Image;
		
		ILuint ilId;
		{
			void[] rawData = loadDataFromFile(filename);
			if (rawData is null) {
				// TODO: log
				return null;
			}
			
			ilGenImages(1, &ilId);
			ilBindImage(ilId);
			ilLoadL(IL_TYPE_UNKNOWN, &rawData[0], rawData.length);
			delete rawData;
		}
		
		result.dataFormat = DataFormat.Byte;
		
		uint w			= ilGetInteger(IL_IMAGE_WIDTH);
		uint h			= ilGetInteger(IL_IMAGE_HEIGHT);
		uint channels	= 0;
		uint bytesPerChannel = 1;

		switch (ilGetInteger(IL_IMAGE_FORMAT)) {
			case IL_BGRA:
			case IL_RGBA:
				channels = 4;
				break;
				
			case IL_BGR:
			case IL_RGB:
				channels = 3;
				break;
				
			case IL_LUMINANCE:
				channels = 1;
				break;
		}
		
		switch (ilGetInteger(IL_IMAGE_TYPE)) {
			case IL_UNSIGNED_BYTE:
				break;
				
			case IL_UNSIGNED_SHORT:
				bytesPerChannel = 2;
				result.dataFormat = DataFormat.Short;
				break;
		}
		
		if (req !is null) {
			auto ilImgType = IL_UNSIGNED_BYTE;
			
			switch (req.dataFormat) {
				case DataFormat.Byte:
				case DataFormat.SignedByte:
					bytesPerChannel = 1;
					break;

				case DataFormat.Short:
					bytesPerChannel = 2;
					ilImgType = IL_UNSIGNED_SHORT;
					break;
					
				case DataFormat.Float:
					assert (false, "TODO: DataFormat.Float");
			}
			
			result.dataFormat = req.dataFormat;
			
			switch (req.imageFormat) {
				case ImageFormat.RGBA:
					channels = 4;
					break;
				case ImageFormat.RGB:
					channels = 3;
					break;
				case ImageFormat.Grayscale:
					channels = 1;
					break;
			}
			
			if (4 == channels) {
				ilConvertImage(IL_RGBA, ilImgType);				
			} else if (1 == channels) {
				ilConvertImage(IL_LUMINANCE, ilImgType);
			} else {
				ilConvertImage(IL_RGB, ilImgType);
			}
		}
		
		switch (channels) {
			case 4:
				result.imageFormat = ImageFormat.RGBA;
				break;
			case 3:
				result.imageFormat = ImageFormat.RGB;
				break;
			case 1:
				result.imageFormat = ImageFormat.Grayscale;
				break;
			default:
				throw new Exception(Format("Unsupported image channel count: {}", channels));
		}

		ImagePlane imgPlane = new ImagePlane;

		{
			ubyte[] tmp = (cast(ubyte*)ilGetData())[0 .. w * h * channels * bytesPerChannel];
			imgPlane.data.alloc(tmp.length);
			imgPlane.data[] = tmp[];
		}
		ilDeleteImages(1, &ilId);

		
		imgPlane.opaque	= true;
		imgPlane.source	= filename;
		imgPlane.width		= w;
		imgPlane.height		= h;
		imgPlane.depth		= 1;
		
		// check if the image is opaque
		if (4 == channels) {
			assert (imgPlane.data.length);
			assert (1 == bytesPerChannel, "TODO: opaque check for 16 bit-per-channel images");
			
			foreach (uint i, ubyte a; cast(ubyte[])imgPlane.data) {
				if (i & 3 != 3) continue;		// only check alpha
				if (a != 255) {
					imgPlane.opaque = false;
					break;
				}
			}
		}
		
		result.planes ~= imgPlane;
		return result;
	}


	void useVfs(VfsFolder vfs) {
		this.vfs = vfs;
	}
	
	
	this() {
		if (ilInit is null) {
			Derelict_SetMissingProcCallback(&handleMissingDevIL); 
			DerelictIL.load();
			assert (ilInit !is null);
			ilInit();
		}
	}
	
	
	protected {
		VfsFolder	vfs;
		
		
		// TODO: log
		void[] loadDataFromFile(char[] name) {
			if (vfs) {
				try {
					auto file = vfs.file(name);
					if (file.exists) {
						auto stream = file.input();
						if (stream) {
							scope (exit) stream.close();
							return stream.load();
						}
					}
				} catch (VfsException e) {}

				// TODO: log
				return null;
			} else {
				if (!(new FilePath(name)).exists) {
					//version (Silent) {} else writefln("File not found: '%s'", filename);
					return null;
				}
				return File.get(name);
			}
		}
	}
}