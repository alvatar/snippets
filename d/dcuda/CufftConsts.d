module xf.dcuda.CufftConsts;

alias cfloat cuComplex;
alias cdouble cuDoubleComplex;

enum {
	CUFFT_SUCCESS        = 0x0,
	CUFFT_INVALID_PLAN   = 0x1,
	CUFFT_ALLOC_FAILED   = 0x2,
	CUFFT_INVALID_TYPE   = 0x3,
	CUFFT_INVALID_VALUE  = 0x4,
	CUFFT_INTERNAL_ERROR = 0x5,
	CUFFT_EXEC_FAILED    = 0x6,
	CUFFT_SETUP_FAILED   = 0x7,
	CUFFT_INVALID_SIZE   = 0x8
}
alias int cufftResult;
    
alias uint cufftHandle;
alias float cufftReal;
alias double cufftDoubleReal;

alias cuComplex cufftComplex;
alias cuDoubleComplex cufftDoubleComplex;

const int CUFFT_FORWARD = -1; // Forward FFT
const int CUFFT_INVERSE = 1; // Inverse FFT

enum {
	CUFFT_R2C = 0x2a,     // Real to Complex (interleaved)
	CUFFT_C2R = 0x2c,     // Complex (interleaved) to Real
	CUFFT_C2C = 0x29,     // Complex to Complex, interleaved
	CUFFT_D2Z = 0x6a,     // Double to Double-Complex
	CUFFT_Z2D = 0x6c,     // Double-Complex to Double
	CUFFT_Z2Z = 0x69      // Double-Complex to Double-Complex
}

alias int cufftType;
