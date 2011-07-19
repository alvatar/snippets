module xf.image.Loader;

private {
	import xf.image.Image;
	import tango.io.vfs.model.Vfs;
}



abstract class Loader {
	abstract void		useVfs(VfsFolder vfs);
	abstract Image	load(char[] filename, ImageRequest* req = null);
}
