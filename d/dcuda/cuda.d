module xf.dcuda.cuda;

private {
	import tango.sys.SharedLib;
	import tango.util.log.Trace;
	import xf.dcuda.CuFunctions;
}

public {
	import xf.dcuda.CuConsts;
	import xf.dcuda.CuFunctionPtrs;
}

version (Windows) {
	char[] cudaDynLibFileName = "nvcuda.dll";
} else {
	static assert(false, "TODO");
}

private {
	SharedLib cudaDynLib;
}

void initDcuda() {
	if (cudaDynLib !is null)
		cudaDynLib.unload();

	try {
		cudaDynLib = SharedLib.load(cudaDynLibFileName);
		Trace.formatln("Loading: {} completed.", cudaDynLibFileName);
	}
	catch (SharedLibException e) {
		Trace.formatln("Warning: {}.", e.msg);
	}

	assert(cudaDynLib !is null, "Could't load nvcuda.dll");

	loadCudaFunctions_(function void*(char* n) {
		return cudaDynLib.getSymbol(n);
	});
}

void cuSafeCallNoSync(T)(lazy T call) {
	CUresult result;
	result = call;
	assert(CUDA_SUCCESS == result, "Cuda driver error.\n");
}

void cuSafeCall(T)(lazy T call) {
	cuSafeCallNoSync(call);
	CUresult result;
	result = cuCtxSynchronize();
	assert(CUDA_SUCCESS == result, "Cuda driver error.\n");
}
