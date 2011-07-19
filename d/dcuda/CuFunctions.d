module xf.dcuda.CuFunctions;

private {
	import xf.dcuda.CuConsts;
	import xf.dcuda.CuFunctionPtrs;
}

void loadCudaFunctions_(void* function(char*) loadFuncFromLib) {
	*cast(void**)&cuInit = loadFuncFromLib("cuInit");
	*cast(void**)&cuDeviceGet = loadFuncFromLib("cuDeviceGet");
	*cast(void**)&cuDeviceGetCount = loadFuncFromLib("cuDeviceGetCount");
	*cast(void**)&cuDeviceGetName = loadFuncFromLib("cuDeviceGetName");
	*cast(void**)&cuDeviceComputeCapability = loadFuncFromLib("cuDeviceComputeCapability");
	*cast(void**)&cuDeviceTotalMem = loadFuncFromLib("cuDeviceTotalMem");
	*cast(void**)&cuDeviceGetProperties = loadFuncFromLib("cuDeviceGetProperties");
	*cast(void**)&cuDeviceGetAttribute = loadFuncFromLib("cuDeviceGetAttribute");
	*cast(void**)&cuCtxCreate = loadFuncFromLib("cuCtxCreate");
	*cast(void**)&cuCtxDestroy = loadFuncFromLib("cuCtxDestroy");
	*cast(void**)&cuCtxAttach = loadFuncFromLib("cuCtxAttach");
	*cast(void**)&cuCtxDetach = loadFuncFromLib("cuCtxDetach");
	*cast(void**)&cuCtxPushCurrent = loadFuncFromLib("cuCtxPushCurrent");
	*cast(void**)&cuCtxPopCurrent = loadFuncFromLib("cuCtxPopCurrent");
	*cast(void**)&cuCtxGetDevice = loadFuncFromLib("cuCtxGetDevice");
	*cast(void**)&cuCtxSynchronize = loadFuncFromLib("cuCtxSynchronize");
	*cast(void**)&cuModuleLoad = loadFuncFromLib("cuModuleLoad");
	*cast(void**)&cuModuleLoadData = loadFuncFromLib("cuModuleLoadData");
	*cast(void**)&cuModuleLoadDataEx = loadFuncFromLib("cuModuleLoadDataEx");
	*cast(void**)&cuModuleLoadFatBinary = loadFuncFromLib("cuModuleLoadFatBinary");
	*cast(void**)&cuModuleUnload = loadFuncFromLib("cuModuleUnload");
	*cast(void**)&cuModuleGetFunction = loadFuncFromLib("cuModuleGetFunction");
	*cast(void**)&cuModuleGetGlobal = loadFuncFromLib("cuModuleGetGlobal");
	*cast(void**)&cuModuleGetTexRef = loadFuncFromLib("cuModuleGetTexRef");
	*cast(void**)&cuMemGetInfo = loadFuncFromLib("cuMemGetInfo");
	*cast(void**)&cuMemAlloc = loadFuncFromLib("cuMemAlloc");
	*cast(void**)&cuMemAllocPitch = loadFuncFromLib("cuMemAllocPitch");
	*cast(void**)&cuMemFree = loadFuncFromLib("cuMemFree");
	*cast(void**)&cuMemGetAddressRange = loadFuncFromLib("cuMemGetAddressRange");
	*cast(void**)&cuMemAllocHost = loadFuncFromLib("cuMemAllocHost");
	*cast(void**)&cuMemFreeHost = loadFuncFromLib("cuMemFreeHost");
	*cast(void**)&cuMemHostAlloc = loadFuncFromLib("cuMemHostAlloc");
	*cast(void**)&cuMemHostGetDevicePointer = loadFuncFromLib("cuMemHostGetDevicePointer");
	*cast(void**)&cuMemHostGetFlags = loadFuncFromLib("cuMemHostGetFlags");
	*cast(void**)&cuMemcpyHtoD = loadFuncFromLib("cuMemcpyHtoD");
	*cast(void**)&cuMemcpyDtoH = loadFuncFromLib("cuMemcpyDtoH");
	*cast(void**)&cuMemcpyDtoD = loadFuncFromLib("cuMemcpyDtoD");
	*cast(void**)&cuMemcpyDtoA = loadFuncFromLib("cuMemcpyDtoA");
	*cast(void**)&cuMemcpyAtoD = loadFuncFromLib("cuMemcpyAtoD");
	*cast(void**)&cuMemcpyHtoA = loadFuncFromLib("cuMemcpyHtoA");
	*cast(void**)&cuMemcpyAtoH = loadFuncFromLib("cuMemcpyAtoH");
	*cast(void**)&cuMemcpyAtoA = loadFuncFromLib("cuMemcpyAtoA");
	*cast(void**)&cuMemcpy2D = loadFuncFromLib("cuMemcpy2D");
	*cast(void**)&cuMemcpy2DUnaligned = loadFuncFromLib("cuMemcpy2DUnaligned");
	*cast(void**)&cuMemcpy3D = loadFuncFromLib("cuMemcpy3D");
	*cast(void**)&cuMemcpyHtoDAsync = loadFuncFromLib("cuMemcpyHtoDAsync");
	*cast(void**)&cuMemcpyDtoHAsync = loadFuncFromLib("cuMemcpyDtoHAsync");
	*cast(void**)&cuMemcpyHtoAAsync = loadFuncFromLib("cuMemcpyHtoAAsync");
	*cast(void**)&cuMemcpyAtoHAsync = loadFuncFromLib("cuMemcpyAtoHAsync");
	*cast(void**)&cuMemcpy2DAsync = loadFuncFromLib("cuMemcpy2DAsync");
	*cast(void**)&cuMemcpy3DAsync = loadFuncFromLib("cuMemcpy3DAsync");
	*cast(void**)&cuMemsetD8 = loadFuncFromLib("cuMemsetD8");
	*cast(void**)&cuMemsetD16 = loadFuncFromLib("cuMemsetD16");
	*cast(void**)&cuMemsetD32 = loadFuncFromLib("cuMemsetD32");
	*cast(void**)&cuMemsetD2D8 = loadFuncFromLib("cuMemsetD2D8");
	*cast(void**)&cuMemsetD2D16 = loadFuncFromLib("cuMemsetD2D16");
	*cast(void**)&cuMemsetD2D32 = loadFuncFromLib("cuMemsetD2D32");
	*cast(void**)&cuFuncSetBlockShape = loadFuncFromLib("cuFuncSetBlockShape");
	*cast(void**)&cuFuncSetSharedSize = loadFuncFromLib("cuFuncSetSharedSize");
	*cast(void**)&cuFuncGetAttribute = loadFuncFromLib("cuFuncGetAttribute");
	*cast(void**)&cuArrayCreate = loadFuncFromLib("cuArrayCreate");
	*cast(void**)&cuArrayGetDescriptor = loadFuncFromLib("cuArrayGetDescriptor");
	*cast(void**)&cuArrayDestroy = loadFuncFromLib("cuArrayDestroy");
	*cast(void**)&cuArray3DCreate = loadFuncFromLib("cuArray3DCreate");
	*cast(void**)&cuArray3DGetDescriptor = loadFuncFromLib("cuArray3DGetDescriptor");
	*cast(void**)&cuTexRefCreate = loadFuncFromLib("cuTexRefCreate");
	*cast(void**)&cuTexRefDestroy = loadFuncFromLib("cuTexRefDestroy");
	*cast(void**)&cuTexRefSetArray = loadFuncFromLib("cuTexRefSetArray");
	*cast(void**)&cuTexRefSetAddress = loadFuncFromLib("cuTexRefSetAddress");
	*cast(void**)&cuTexRefSetAddress2D = loadFuncFromLib("cuTexRefSetAddress2D");
	*cast(void**)&cuTexRefSetFormat = loadFuncFromLib("cuTexRefSetFormat");
	*cast(void**)&cuTexRefSetAddressMode = loadFuncFromLib("cuTexRefSetAddressMode");
	*cast(void**)&cuTexRefSetFilterMode = loadFuncFromLib("cuTexRefSetFilterMode");
	*cast(void**)&cuTexRefSetFlags = loadFuncFromLib("cuTexRefSetFlags");
	*cast(void**)&cuTexRefGetAddress = loadFuncFromLib("cuTexRefGetAddress");
	*cast(void**)&cuTexRefGetArray = loadFuncFromLib("cuTexRefGetArray");
	*cast(void**)&cuTexRefGetAddressMode = loadFuncFromLib("cuTexRefGetAddressMode");
	*cast(void**)&cuTexRefGetFilterMode = loadFuncFromLib("cuTexRefGetFilterMode");
	*cast(void**)&cuTexRefGetFormat = loadFuncFromLib("cuTexRefGetFormat");
	*cast(void**)&cuTexRefGetFlags = loadFuncFromLib("cuTexRefGetFlags");
	*cast(void**)&cuParamSetSize = loadFuncFromLib("cuParamSetSize");
	*cast(void**)&cuParamSeti = loadFuncFromLib("cuParamSeti");
	*cast(void**)&cuParamSetf = loadFuncFromLib("cuParamSetf");
	*cast(void**)&cuParamSetv = loadFuncFromLib("cuParamSetv");
	*cast(void**)&cuParamSetTexRef = loadFuncFromLib("cuParamSetTexRef");
	*cast(void**)&cuLaunch = loadFuncFromLib("cuLaunch");
	*cast(void**)&cuLaunchGrid = loadFuncFromLib("cuLaunchGrid");
	*cast(void**)&cuLaunchGridAsync = loadFuncFromLib("cuLaunchGridAsync");
	*cast(void**)&cuEventCreate = loadFuncFromLib("cuEventCreate");
	*cast(void**)&cuEventRecord = loadFuncFromLib("cuEventRecord");
	*cast(void**)&cuEventQuery = loadFuncFromLib("cuEventQuery");
	*cast(void**)&cuEventSynchronize = loadFuncFromLib("cuEventSynchronize");
	*cast(void**)&cuEventDestroy = loadFuncFromLib("cuEventDestroy");
	*cast(void**)&cuEventElapsedTime = loadFuncFromLib("cuEventElapsedTime");
	*cast(void**)&cuStreamCreate = loadFuncFromLib("cuStreamCreate");
	*cast(void**)&cuStreamQuery = loadFuncFromLib("cuStreamQuery");
	*cast(void**)&cuStreamSynchronize = loadFuncFromLib("cuStreamSynchronize");
	*cast(void**)&cuStreamDestroy = loadFuncFromLib("cuStreamDestroy");

	//cudaGL.h:
	*cast(void**)&cuGLInit = loadFuncFromLib("cuGLInit");
	*cast(void**)&cuGLCtxCreate = loadFuncFromLib("cuGLCtxCreate");
	*cast(void**)&cuGLRegisterBufferObject = loadFuncFromLib("cuGLRegisterBufferObject");
	*cast(void**)&cuGLMapBufferObject = loadFuncFromLib("cuGLMapBufferObject");
	*cast(void**)&cuGLUnmapBufferObject = loadFuncFromLib("cuGLUnmapBufferObject");
	*cast(void**)&cuGLUnregisterBufferObject = loadFuncFromLib("cuGLUnregisterBufferObject");

	*cast(void**)&cuGLSetBufferObjectMapFlags = loadFuncFromLib("cuGLSetBufferObjectMapFlags");
	*cast(void**)&cuGLMapBufferObjectAsync = loadFuncFromLib("cuGLMapBufferObjectAsync");
	*cast(void**)&cuGLUnmapBufferObjectAsync = loadFuncFromLib("cuGLUnmapBufferObjectAsync");

	version(Windows) {
		*cast(void**)&cuWGLGetDevice = loadFuncFromLib("cuWGLGetDevice");
	}
}
