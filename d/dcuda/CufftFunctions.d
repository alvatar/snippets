module xf.dcuda.CufftFunctions;

private {
	import xf.dcuda.CufftConsts;
	import xf.dcuda.CufftFunctionPtrs;
}

void loadCufftFunctions_(void* function(char*) loadFuncFromLib) {
	*cast(void**)&cufftPlan1d = loadFuncFromLib("cufftPlan1d");
	*cast(void**)&cufftPlan2d = loadFuncFromLib("cufftPlan2d");
	*cast(void**)&cufftPlan3d = loadFuncFromLib("cufftPlan3d");
	*cast(void**)&cufftDestroy = loadFuncFromLib("cufftDestroy");
	*cast(void**)&cufftExecC2C = loadFuncFromLib("cufftExecC2C");
	*cast(void**)&cufftExecR2C = loadFuncFromLib("cufftExecR2C");
	*cast(void**)&cufftExecC2R = loadFuncFromLib("cufftExecC2R");
	*cast(void**)&cufftExecZ2Z = loadFuncFromLib("cufftExecZ2Z");
	*cast(void**)&cufftExecD2Z = loadFuncFromLib("cufftExecD2Z");
	*cast(void**)&cufftExecZ2D = loadFuncFromLib("cufftExecZ2D");
//	*cast(void**)&cufftSetStream = loadFuncFromLib("cufftSetStream");
}

