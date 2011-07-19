module xf.dcuda.CuFunctionPtrs;

private {
	import xf.dcuda.CuConsts;
}

extern (System):
	/*********************************
	** Initialization
	*********************************/
	CUresult  function(uint Flags ) cuInit;

	/************************************
	 **
	 **    Device management
	 **
	 ***********************************/
	CUresult  function(CUdevice *device, int ordinal) cuDeviceGet;
	CUresult  function(int *count) cuDeviceGetCount;
	CUresult  function(char *name, int len, CUdevice dev) cuDeviceGetName;
	CUresult  function(int *major, int *minor, CUdevice dev) cuDeviceComputeCapability;
	CUresult  function(uint *bytes, CUdevice dev) cuDeviceTotalMem;
	CUresult  function(CUdevprop *prop, CUdevice dev) cuDeviceGetProperties;
	CUresult  function(int *pi, CUdevice_attribute attrib, CUdevice dev) cuDeviceGetAttribute;

	/************************************
	 **
	 **    Context management
	 **
	 ***********************************/
	CUresult  function(CUcontext *pctx, uint flags, CUdevice dev ) cuCtxCreate;
	CUresult  function( CUcontext ctx ) cuCtxDestroy;
	CUresult  function(CUcontext *pctx, uint flags) cuCtxAttach;
	CUresult  function(CUcontext ctx) cuCtxDetach;
	CUresult  function( CUcontext ctx ) cuCtxPushCurrent;
	CUresult  function( CUcontext *pctx ) cuCtxPopCurrent;
	CUresult  function(CUdevice *device) cuCtxGetDevice;
	CUresult  function() cuCtxSynchronize;


	/************************************
	 **
	 **    Module management
	 **
	 ***********************************/
	CUresult  function(CUmodule *cuModule, char *fname) cuModuleLoad;
	CUresult  function(CUmodule *cuModule, void *image) cuModuleLoadData;
	CUresult  function(CUmodule *cuModule, void *image, uint numOptions, CUjit_option *options, void **optionValues) cuModuleLoadDataEx;
	CUresult  function(CUmodule *cuModule, void *fatCubin) cuModuleLoadFatBinary;
	CUresult  function(CUmodule hmod) cuModuleUnload;
	CUresult  function(CUfunction *hfunc, CUmodule hmod, char *name) cuModuleGetFunction;
	CUresult  function(CUdeviceptr *dptr, uint *bytes, CUmodule hmod, char *name) cuModuleGetGlobal;
	CUresult  function(CUtexref *pTexRef, CUmodule hmod, char *name) cuModuleGetTexRef;

	/************************************
	 **
	 **    Memory management
	 **
	 ***********************************/
	CUresult function(uint *free, uint *total) cuMemGetInfo;

	CUresult function( CUdeviceptr *dptr, uint bytesize) cuMemAlloc;
	CUresult function( CUdeviceptr *dptr,
					   uint *pPitch,
					   uint WidthInBytes,
					   uint Height,
					   uint ElementSizeBytes
					 ) cuMemAllocPitch;
	CUresult function(CUdeviceptr dptr) cuMemFree;
	CUresult function( CUdeviceptr *pbase, uint *psize, CUdeviceptr dptr ) cuMemGetAddressRange;

	CUresult function(void **pp, uint bytesize) cuMemAllocHost;
	CUresult function(void *p) cuMemFreeHost;

	CUresult function(void **pp, uint bytesize, uint Flags) cuMemHostAlloc;
 	CUresult function(CUdeviceptr *pdptr, void *p, uint Flags) cuMemHostGetDevicePointer;
	CUresult function(uint *pFlags, void *p) cuMemHostGetFlags;

	
	/************************************
	 **
	 **    Synchronous Memcpy
	 **
	 ** Intra-device memcpy's done with these functions may execute in parallel with the CPU,
	 ** but if host memory is involved, they wait until the copy is done before returning.
	 **
	 ***********************************/
	// 1D functions
	// system <-> device memory
	CUresult  function(CUdeviceptr dstDevice, void *srcHost, uint ByteCount ) cuMemcpyHtoD;
	CUresult  function(void *dstHost, CUdeviceptr srcDevice, uint ByteCount ) cuMemcpyDtoH;

	// device <-> device memory
	CUresult  function(CUdeviceptr dstDevice, CUdeviceptr srcDevice, uint ByteCount ) cuMemcpyDtoD;

	// device <-> array memory
	CUresult  function( CUarray dstArray, uint dstIndex, CUdeviceptr srcDevice, uint ByteCount ) cuMemcpyDtoA;
	CUresult  function( CUdeviceptr dstDevice, CUarray hSrc, uint SrcIndex, uint ByteCount ) cuMemcpyAtoD;

	// system <-> array memory
	CUresult  function( CUarray dstArray, uint dstIndex, void *pSrc, uint ByteCount ) cuMemcpyHtoA;
	CUresult  function( void *dstHost, CUarray srcArray, uint srcIndex, uint ByteCount ) cuMemcpyAtoH;

	// array <-> array memory
	CUresult  function( CUarray dstArray, uint dstIndex, CUarray srcArray, uint srcIndex, uint ByteCount ) cuMemcpyAtoA;

	// 2D memcpy
	CUresult  function( CUDA_MEMCPY2D *pCopy ) cuMemcpy2D;
	CUresult  function( CUDA_MEMCPY2D *pCopy ) cuMemcpy2DUnaligned;
	// 3D memcpy

	CUresult  function( CUDA_MEMCPY3D *pCopy ) cuMemcpy3D;

	/************************************
	 **
	 **    Asynchronous Memcpy
	 **
	 ** Any host memory involved must be DMA'able (e.g., allocated with cuMemAllocHost).
	 ** memcpy's done with these functions execute in parallel with the CPU and, if
	 ** the hardware is available, may execute in parallel with the GPU.
	 ** Asynchronous memcpy must be accompanied by appropriate stream synchronization.
	 **
	 ***********************************/
	// 1D functions
	// system <-> device memory
	CUresult  function(CUdeviceptr dstDevice,
					   void *srcHost, uint ByteCount, CUstream hStream ) cuMemcpyHtoDAsync;
	CUresult  function(void *dstHost,
					   CUdeviceptr srcDevice, uint ByteCount, CUstream hStream ) cuMemcpyDtoHAsync;

	// system <-> array memory
	CUresult  function( CUarray dstArray, uint dstIndex,
						void *pSrc, uint ByteCount, CUstream hStream ) cuMemcpyHtoAAsync;
	CUresult  function( void *dstHost, CUarray srcArray, uint srcIndex,
						uint ByteCount, CUstream hStream ) cuMemcpyAtoHAsync;

	// 2D memcpy
	CUresult  function( CUDA_MEMCPY2D *pCopy, CUstream hStream ) cuMemcpy2DAsync;

	// 3D memcpy
	CUresult  function( CUDA_MEMCPY3D *pCopy, CUstream hStream ) cuMemcpy3DAsync;

	/************************************
	 **
	 **    Memset
	 **
	 ***********************************/
	CUresult  function( CUdeviceptr dstDevice, ubyte uc, uint N ) cuMemsetD8;
	CUresult  function( CUdeviceptr dstDevice, ushort us, uint N ) cuMemsetD16;
	CUresult  function( CUdeviceptr dstDevice, uint ui, uint N ) cuMemsetD32;

	CUresult  function( CUdeviceptr dstDevice, uint dstPitch, ubyte uc, uint Width, uint Height ) cuMemsetD2D8;
	CUresult  function( CUdeviceptr dstDevice, uint dstPitch, ushort us, uint Width, uint Height ) cuMemsetD2D16;
	CUresult  function( CUdeviceptr dstDevice, uint dstPitch, uint ui, uint Width, uint Height ) cuMemsetD2D32;

	/************************************
	 **
	 **    Function management
	 **
	 ***********************************/
	CUresult function(CUfunction hfunc, int x, int y, int z) cuFuncSetBlockShape;
	CUresult function(CUfunction hfunc, uint bytes) cuFuncSetSharedSize;
	CUresult function(int *pi, CUfunction_attribute attrib, CUfunction hfunc) cuFuncGetAttribute;

	/************************************
	 **
	 **    Array management
	 **
	 ***********************************/
	CUresult  function( CUarray *pHandle, CUDA_ARRAY_DESCRIPTOR *pAllocateArray ) cuArrayCreate;
	CUresult  function( CUDA_ARRAY_DESCRIPTOR *pArrayDescriptor, CUarray hArray ) cuArrayGetDescriptor;
	CUresult  function( CUarray hArray ) cuArrayDestroy;

	CUresult  function( CUarray *pHandle, CUDA_ARRAY3D_DESCRIPTOR *pAllocateArray ) cuArray3DCreate;
	CUresult  function( CUDA_ARRAY3D_DESCRIPTOR *pArrayDescriptor, CUarray hArray ) cuArray3DGetDescriptor;

	/************************************
	 **
	 **    Texture reference management
	 **
	 ***********************************/
	CUresult  function( CUtexref *pTexRef ) cuTexRefCreate;
	CUresult  function( CUtexref hTexRef ) cuTexRefDestroy;

	CUresult  function( CUtexref hTexRef, CUarray hArray, uint Flags ) cuTexRefSetArray;

	CUresult  function( uint *ByteOffset, CUtexref hTexRef, CUdeviceptr dptr, uint bytes ) cuTexRefSetAddress;
	CUresult  function( CUtexref hTexRef, CUDA_ARRAY_DESCRIPTOR *desc, CUdeviceptr dptr, uint Pitch) cuTexRefSetAddress2D;
	CUresult  function( CUtexref hTexRef, CUarray_format fmt, int NumPackedComponents ) cuTexRefSetFormat;

	CUresult  function( CUtexref hTexRef, int dim, CUaddress_mode am ) cuTexRefSetAddressMode;
	CUresult  function( CUtexref hTexRef, CUfilter_mode fm ) cuTexRefSetFilterMode;
	CUresult  function( CUtexref hTexRef, uint Flags ) cuTexRefSetFlags;


	CUresult  function( CUdeviceptr *pdptr, CUtexref hTexRef ) cuTexRefGetAddress;
	CUresult  function( CUarray *phArray, CUtexref hTexRef ) cuTexRefGetArray;
	CUresult  function( CUaddress_mode *pam, CUtexref hTexRef, int dim ) cuTexRefGetAddressMode;
	CUresult  function( CUfilter_mode *pfm, CUtexref hTexRef ) cuTexRefGetFilterMode;
	CUresult  function( CUarray_format *pFormat, int *pNumChannels, CUtexref hTexRef ) cuTexRefGetFormat;
	CUresult  function( uint *pFlags, CUtexref hTexRef ) cuTexRefGetFlags;

	/************************************
	 **
	 **    Parameter management
	 **
	 ***********************************/
	CUresult  function(CUfunction hfunc, uint numbytes) cuParamSetSize;
	CUresult  function(CUfunction hfunc, int offset, uint value) cuParamSeti;
	CUresult  function(CUfunction hfunc, int offset, float value) cuParamSetf;
	CUresult  function(CUfunction hfunc, int offset, void * ptr, uint numbytes) cuParamSetv;
	CUresult  function(CUfunction hfunc, int texunit, CUtexref hTexRef) cuParamSetTexRef;

	/************************************
	 **
	 **    Launch functions
	 **
	 ***********************************/
	CUresult function( CUfunction f ) cuLaunch;
	CUresult function(CUfunction f, int grid_width, int grid_height) cuLaunchGrid;
	CUresult function( CUfunction f, int grid_width, int grid_height, CUstream hStream ) cuLaunchGridAsync;

	/************************************
	 **
	 **    Events
	 **
	 ***********************************/
	CUresult function( CUevent *phEvent, uint Flags ) cuEventCreate;
	CUresult function( CUevent hEvent, CUstream hStream ) cuEventRecord;
	CUresult function( CUevent hEvent ) cuEventQuery;
	CUresult function( CUevent hEvent ) cuEventSynchronize;
	CUresult function( CUevent hEvent ) cuEventDestroy;
	CUresult function( float *pMilliseconds, CUevent hStart, CUevent hEnd ) cuEventElapsedTime;

	/************************************
	 **
	 **    Streams
	 **
	 ***********************************/
	CUresult function( CUstream *phStream, uint Flags ) cuStreamCreate;
	CUresult function( CUstream hStream ) cuStreamQuery;
	CUresult function( CUstream hStream ) cuStreamSynchronize;
	CUresult function( CUstream hStream ) cuStreamDestroy;

	//cudaGL.h:
	CUresult function() cuGLInit;
	CUresult function( CUcontext *pCtx, uint Flags, CUdevice device ) cuGLCtxCreate;
	CUresult function( uint bufferobj ) cuGLRegisterBufferObject;
	CUresult function( CUdeviceptr *dptr, uint *size, uint bufferobj ) cuGLMapBufferObject;
	CUresult function( uint bufferobj ) cuGLUnmapBufferObject;
	CUresult function( uint bufferobj ) cuGLUnregisterBufferObject;

	CUresult function( uint bufferobj, uint Flags ) cuGLSetBufferObjectMapFlags;
	CUresult function( CUdeviceptr *dptr, uint *size,  uint bufferobj, CUstream hStream ) cuGLMapBufferObjectAsync;
	CUresult function( uint bufferobj, CUstream hStream ) cuGLUnmapBufferObjectAsync;

	version(Windows) {
		CUresult function( CUdevice *pDevice, HGPUNV hGpu ) cuWGLGetDevice;
	}

