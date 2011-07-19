module xf.dcuda.cufft;

private {
	import tango.sys.SharedLib;
	import tango.util.log.Trace;
	import xf.dcuda.CufftFunctions;
}

public {
	import xf.dcuda.CufftConsts;
	import xf.dcuda.CufftFunctionPtrs;
}

version (Windows) {
	version (fftRelease) {
		char[] cufftDynLibFileName = "cufft.dll";
	} else version(fftEmu){
		char[] cufftDynLibFileName = "cufftemu.dll";
	} else {
		static assert(false, "Define version=fftEmu or version=fftRelease");
	}
} else {
	static assert(false, "TODO");
}

private {
	SharedLib cufftDynLib;
}

void initDcufft() {
	if (cufftDynLib !is null)
		cufftDynLib.unload();

	try {
		cufftDynLib = SharedLib.load(cufftDynLibFileName);
		Trace.formatln("Loading: {} completed.", cufftDynLibFileName);
	}
	catch (SharedLibException e) {
		Trace.formatln("Warning: {}.", e.msg);
	}

	assert(cufftDynLib !is null, "Could't load cufft library.");

	loadCufftFunctions_(function void*(char* n) {
		return cufftDynLib.getSymbol(n);
	});
}

