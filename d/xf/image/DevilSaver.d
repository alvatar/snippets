module xf.image.DevilSaver;

private {
	import xf.image.Image;
	import derelict.devil.il;
	import derelict.devil.ilu;
	
	import xf.utils.Memory;
	
	import tango.io.device.File;
	import tango.io.FilePath;
	import tango.stdc.stringz;
}



class DevilSaver {
	void overwrite(bool b) {
		if (b) {
			ilEnable(IL_FILE_OVERWRITE);
		} else {
			ilDisable(IL_FILE_OVERWRITE);
		}
	}
	
	
	void save(Image img, char[] filename, float quality = 1.f) {
		save(img, filename, vec2i.zero, quality);
	}
	

	void save(Image img, char[] filename, vec2i scale, float quality = 1.f) {
		// TODO: generalize this
		ilEnable(IL_ORIGIN_SET);
		ilOriginFunc(IL_ORIGIN_LOWER_LEFT);

		uint bytes;
		uint fmt;
		switch (img.imageFormat) {
			case ImageFormat.Grayscale:	bytes = 1; fmt = IL_LUMINANCE; break;
			case ImageFormat.RGB:			bytes = 3; fmt = IL_RGB; break;
			case ImageFormat.RGBA:		bytes = 4; fmt = IL_RGBA; break;
		}

		ILuint ilId;
		{
			ilGenImages(1, &ilId);
			ilBindImage(ilId);
			ilTexImage(img.planes[0].width, img.planes[0].height, 1, bytes, fmt, IL_UNSIGNED_BYTE, img.planes[0].data.ptr);
		}
		
		if (scale != vec2i.zero) {
			iluImageParameter(ILU_FILTER, ILU_SCALE_LANCZOS3);
			iluScale(scale.x, scale.y, 1);
		}
		
		if (quality < 0.f) {
			quality = 0.f;
		}
		if (quality > 1.f) {
			quality = 1.f;
		}
		
		int prevJpgQuality = ilGetInteger(IL_JPG_QUALITY);
		ilSetInteger(IL_JPG_QUALITY, cast(int)(99 * quality));
			ilSaveImage(toStringz(filename));
		ilSetInteger(IL_JPG_QUALITY, prevJpgQuality);
		
		ilDeleteImages(1, &ilId);
	}
	
	
	this() {
		if (ilInit is null) {
			DerelictIL.load();
			ilInit();
		}

		if (iluInit is null) {
			DerelictILU.load();
			iluInit();
		}
	}

}
