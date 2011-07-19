module xf.dcuda.CuConsts;

/* CUDA API version number */
const int CUDA_VERSION = 2030; /* 2.3 */

typedef uint CUdeviceptr;

typedef int CUdevice;
typedef void *CUcontext;
typedef void *CUmodule;
typedef void *CUfunction;
typedef void *CUarray;
typedef void *CUtexref;
typedef void *CUevent;
typedef void *CUstream;

/************************************
 **
 **    Enums
 **
 ***********************************/

//
// context creation flags
//
enum {
	CU_CTX_SCHED_AUTO  = 0,
	CU_CTX_SCHED_SPIN  = 1,
	CU_CTX_SCHED_YIELD = 2,
	CU_CTX_SCHED_MASK  = 0x3,
	CU_CTX_BLOCKING_SYNC = 4,
	CU_CTX_MAP_HOST = 8,
	CU_CTX_FLAGS_MASK  = 0xf	
}
alias int CUctx_flags;

//
// Event creation flags
//
enum {
	CU_EVENT_DEFAULT       = 0,
	CU_EVENT_BLOCKING_SYNC = 1 
}

alias int CUevent_flags;


//
// array formats
//
enum {
	CU_AD_FORMAT_UNSIGNED_INT8  = 0x01,
	CU_AD_FORMAT_UNSIGNED_INT16 = 0x02,
	CU_AD_FORMAT_UNSIGNED_INT32 = 0x03,
	CU_AD_FORMAT_SIGNED_INT8    = 0x08,
	CU_AD_FORMAT_SIGNED_INT16   = 0x09,
	CU_AD_FORMAT_SIGNED_INT32   = 0x0a,
	CU_AD_FORMAT_HALF           = 0x10,
	CU_AD_FORMAT_FLOAT          = 0x20
}
alias int CUarray_format;

//
// Texture reference addressing modes
//
enum {
	CU_TR_ADDRESS_MODE_WRAP = 0,
	CU_TR_ADDRESS_MODE_CLAMP = 1,
	CU_TR_ADDRESS_MODE_MIRROR = 2,
}
alias int CUaddress_mode;

//
// Texture reference filtering modes
//
enum {
	CU_TR_FILTER_MODE_POINT = 0,
	CU_TR_FILTER_MODE_LINEAR = 1
}
alias int CUfilter_mode;

//
// Device properties
//
enum {
	CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1,
	CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X = 2,
	CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y = 3,
	CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z = 4,
	CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X = 5,
	CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y = 6,
	CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z = 7,
	CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK = 8,
	CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8,      // Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK
	CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY = 9,
	CU_DEVICE_ATTRIBUTE_WARP_SIZE = 10,
	CU_DEVICE_ATTRIBUTE_MAX_PITCH = 11,
	CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK = 12,
	CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12,         // Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK
	CU_DEVICE_ATTRIBUTE_CLOCK_RATE = 13,
	CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT = 14,

	CU_DEVICE_ATTRIBUTE_GPU_OVERLAP = 15,
	CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16,
	CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT = 17,   ///< Specifies whether there is a run time limit on kernels
	CU_DEVICE_ATTRIBUTE_INTEGRATED = 18,            ///< Device is integrated with host memory
	CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY = 19,   ///< Device can map host memory into CUDA address space
	CU_DEVICE_ATTRIBUTE_COMPUTE_MODE = 20           ///< Compute mode (See ::CUcomputemode for details)
}
alias int CUdevice_attribute;

//
// Legacy device properties
//
struct CUdevprop {
	int maxThreadsPerBlock;
	int maxThreadsDim[3];
	int maxGridSize[3];
	int sharedMemPerBlock;
	int totalConstantMemory;
	int SIMDWidth;
	int memPitch;
	int regsPerBlock;
	int clockRate;
	int textureAlign;
}

//
// Function properties
//
enum {
	CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 0,
	CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES = 1,
	CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES = 2,
	CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES = 3,
	CU_FUNC_ATTRIBUTE_NUM_REGS = 4,
	CU_FUNC_ATTRIBUTE_MAX
}
alias int CUfunction_attribute;

//
// Memory types
//
enum {
	CU_MEMORYTYPE_HOST = 0x01,
	CU_MEMORYTYPE_DEVICE = 0x02,
	CU_MEMORYTYPE_ARRAY = 0x03
}
alias int CUmemorytype;

//
// Compute Modes
//
enum {
	CU_COMPUTEMODE_DEFAULT    = 0,     ///< Default compute mode (Multiple contexts allowed per device)
	CU_COMPUTEMODE_EXCLUSIVE  = 1,     ///< Compute-exclusive mode (Only one context can be present on this device at a time)
	CU_COMPUTEMODE_PROHIBITED = 2      ///< Compute-prohibited mode (No contexts can be created on this device at this time)
}
alias int CUcomputemode;

//
// Online compiler options
//
enum {
	CU_JIT_MAX_REGISTERS            = 0,
	CU_JIT_THREADS_PER_BLOCK,
	CU_JIT_WALL_TIME,
	CU_JIT_INFO_LOG_BUFFER,
	CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES,
	CU_JIT_ERROR_LOG_BUFFER,
	CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES,
	CU_JIT_OPTIMIZATION_LEVEL,
	CU_JIT_TARGET_FROM_CUCONTEXT,
	CU_JIT_TARGET,
	CU_JIT_FALLBACK_STRATEGY
}
alias int CUjit_option;

//
// Online compilation targets
//
enum {
	CU_TARGET_COMPUTE_10            = 0,
	CU_TARGET_COMPUTE_11,
	CU_TARGET_COMPUTE_12,
	CU_TARGET_COMPUTE_13
}
alias int CUjit_target;

//
// Cubin matching fallback strategies
//
enum {
	CU_PREFER_PTX                   = 0,
	CU_PREFER_BINARY
}
alias int CUjit_fallback;

/************************************
 **
 **    Error codes
 **
 ***********************************/

enum {
	CUDA_SUCCESS                    = 0,
	CUDA_ERROR_INVALID_VALUE        = 1,
	CUDA_ERROR_OUT_OF_MEMORY        = 2,
	CUDA_ERROR_NOT_INITIALIZED      = 3,
	CUDA_ERROR_DEINITIALIZED        = 4,

	CUDA_ERROR_NO_DEVICE            = 100,
	CUDA_ERROR_INVALID_DEVICE       = 101,

	CUDA_ERROR_INVALID_IMAGE        = 200,
	CUDA_ERROR_INVALID_CONTEXT      = 201,
	CUDA_ERROR_CONTEXT_ALREADY_CURRENT = 202,
	CUDA_ERROR_MAP_FAILED           = 205,
	CUDA_ERROR_UNMAP_FAILED         = 206,
	CUDA_ERROR_ARRAY_IS_MAPPED      = 207,
	CUDA_ERROR_ALREADY_MAPPED       = 208,
	CUDA_ERROR_NO_BINARY_FOR_GPU    = 209,
	CUDA_ERROR_ALREADY_ACQUIRED     = 210,
	CUDA_ERROR_NOT_MAPPED           = 211,

	CUDA_ERROR_INVALID_SOURCE       = 300,
	CUDA_ERROR_FILE_NOT_FOUND       = 301,

	CUDA_ERROR_INVALID_HANDLE       = 400,

	CUDA_ERROR_NOT_FOUND            = 500,

	CUDA_ERROR_NOT_READY            = 600,

	CUDA_ERROR_LAUNCH_FAILED        = 700,
	CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = 701,
	CUDA_ERROR_LAUNCH_TIMEOUT       = 702,
	CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = 703,

	CUDA_ERROR_UNKNOWN              = 999
}
alias int CUresult;

const int CU_MEMHOSTALLOC_PORTABLE        = 0x01;
const int CU_MEMHOSTALLOC_DEVICEMAP       = 0x02;
const int CU_MEMHOSTALLOC_WRITECOMBINED   = 0x04;

// 2D memcpy

struct CUDA_MEMCPY2D {
	uint srcXInBytes, srcY;
	CUmemorytype srcMemoryType;
	void *srcHost;
	CUdeviceptr srcDevice;
	CUarray srcArray;
	uint srcPitch; // ignored when src is array

	uint dstXInBytes, dstY;
	CUmemorytype dstMemoryType;
	void *dstHost;
	CUdeviceptr dstDevice;
	CUarray dstArray;
	uint dstPitch; // ignored when dst is array

	uint WidthInBytes;
	uint Height;
}

struct  CUDA_MEMCPY3D {
	uint srcXInBytes, srcY, srcZ;
	uint srcLOD;
	CUmemorytype srcMemoryType;
	void *srcHost;
	CUdeviceptr srcDevice;
	CUarray srcArray;
	void *reserved0;        // must be NULL
	uint srcPitch;  // ignored when src is array
	uint srcHeight; // ignored when src is array; may be 0 if Depth==1

	uint dstXInBytes, dstY, dstZ;
	uint dstLOD;
	CUmemorytype dstMemoryType;
	void *dstHost;
	CUdeviceptr dstDevice;
	CUarray dstArray;
	void *reserved1;        // must be NULL
	uint dstPitch;  // ignored when dst is array
	uint dstHeight; // ignored when dst is array; may be 0 if Depth==1

	uint WidthInBytes;
	uint Height;
	uint Depth;
}

/************************************
 **
 **    Array management
 **
 ***********************************/

struct CUDA_ARRAY_DESCRIPTOR {
	uint Width;
	uint Height;
	CUarray_format Format;
	uint NumChannels;
}

struct CUDA_ARRAY3D_DESCRIPTOR {
	uint Width;
	uint Height;
	uint Depth;
	CUarray_format Format;
	uint NumChannels;
	uint Flags;
}

/************************************
 **
 **    Texture reference management
 **
 ***********************************/
const int CU_TRSA_OVERRIDE_FORMAT = 0x01;
const int CU_TRSF_READ_AS_INTEGER = 0x01;
const int CU_TRSF_NORMALIZED_COORDINATES = 0x02;

/************************************
 **
 **    Parameter management
 **
 ***********************************/
const int CU_PARAM_TR_DEFAULT = -1;

////
////  cudaGL.h
////

//
// Flags to map or unmap a resource
//
enum {
	CU_GL_MAP_RESOURCE_FLAGS_NONE          = 0x00,
	CU_GL_MAP_RESOURCE_FLAGS_READ_ONLY     = 0x01,
	CU_GL_MAP_RESOURCE_FLAGS_WRITE_DISCARD = 0x02,    
}
alias int CUGLmap_flags;

version(Windows){
	pragma(msg, "TODO: !defined(WGL_NV_gpu_affinity) ?");
	alias void* HGPUNV;
}

