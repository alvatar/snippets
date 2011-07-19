module xf.dcuda.cutil;

private {
	import tango.sys.SharedLib;
	import tango.util.log.Trace;
	import xf.dcuda.CutFunctions;
}

public {
	import xf.dcuda.CutConsts;
	import xf.dcuda.CufftConsts;
	import xf.dcuda.CutFunctionPtrs;
}

version (Windows) {
	version (Release) {
		char[] cutDynLibFileName = "cutil32.dll";
	} else version(Debug){
		char[] cutDynLibFileName = "cutil32D.dll";
	} else {
		static assert(false, "Define version=Debug or version=Release");
	}
} else {
	static assert(false, "TODO");
}

private {
	SharedLib cutDynLib;
}

void initDcutil() {
	if (cutDynLib !is null)
		cutDynLib.unload();

	try {
		cutDynLib = SharedLib.load(cutDynLibFileName);
		Trace.formatln("Loading: {} completed.", cutDynLibFileName);
	}
	catch (SharedLibException e) {
		Trace.formatln("Warning: {}.", e.msg);
	}

	assert(cutDynLib !is null, "Could't load cutil32 library.");

	loadCutFunctions_(function void*(char* n) {
		return cutDynLib.getSymbol(n);
	});
}


void cutSafeCall(T)(lazy T call) {
	CUTBoolean result;
	result = call;
	assert(CUTTrue == result, "Cut error.\n");
}

void cufftSafeCall(T)(lazy T call) {
	cufftResult result;
	result = call;
	assert(CUFFT_SUCCESS == result, "Cufft error.\n");
}

