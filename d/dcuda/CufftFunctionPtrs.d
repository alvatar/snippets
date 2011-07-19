module xf.dcuda.CufftFunctionPtrs;

private {
	import xf.dcuda.CuConsts;
	import xf.dcuda.CufftConsts;
}

extern (System):
	cufftResult function(cufftHandle *plan, int nx, cufftType type, int batch) cufftPlan1d;
	cufftResult function(cufftHandle *plan, int nx, int ny, cufftType type) cufftPlan2d;
	cufftResult function(cufftHandle *plan, int nx, int ny, int nz, cufftType type) cufftPlan3d;
	cufftResult function(cufftHandle plan) cufftDestroy;
	cufftResult function(cufftHandle plan, cufftComplex *idata, cufftComplex *odata, int direction) cufftExecC2C;
	cufftResult function(cufftHandle plan, cufftReal *idata, cufftComplex *odata) cufftExecR2C;
	cufftResult function(cufftHandle plan, cufftComplex *idata, cufftReal *odata) cufftExecC2R;
	cufftResult function(cufftHandle plan, cufftDoubleComplex *idata, cufftDoubleComplex *odata, int direction) cufftExecZ2Z;
	cufftResult function(cufftHandle plan, cufftDoubleReal *idata, cufftDoubleComplex *odata) cufftExecD2Z;
	cufftResult function(cufftHandle plan, cufftDoubleComplex *idata, cufftDoubleReal *odata) cufftExecZ2D;
//	cufftResult function(cufftHandle p, cudaStream_t stream) cufftSetStream;
//	alias int cudaStream_t
