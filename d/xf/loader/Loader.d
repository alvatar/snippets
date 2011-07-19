module xf.loader.Loader;


/+private {
	import rg.Node : RgNode = Node;
}+/



abstract class Loader {
	abstract void			load(char[] filename);
	
	/+
	// I must've been drunk while coding this ... :S it obviously needs to be changed
	// because the generic 'Loader' should not know about 'RgNodes'..........
	
	RgNode[]	rgNodes()	{ assert (false); return null; }
	//abstract SgNode[]	sgNodes();+/
}
