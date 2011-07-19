module MatrixMulDrv;
//import tango.core.stacktrace.TraceExceptions;

import xf.dcuda.cuda;
import xf.dcuda.cutil;
import xf.dcuda.Utils;

import tango.util.log.Trace;
import tango.util.Convert : to;
import tango.stdc.stringz : toStringz;
import tango.stdc.stdlib : malloc, free;
import tango.math.random.Random;
import tango.math.Math : sqrt;


char[] CUdevpropToString(CUdevprop prop) {
	char[] res;
	res ~= "maxThreadsPerBlock: " ~ to!(char[])(prop.maxThreadsPerBlock) ~ "\n";
	res ~= "maxThreadsDim: ["	~ to!(char[])(prop.maxThreadsDim[0]) ~ ", " 
								~ to!(char[])(prop.maxThreadsDim[1]) ~ ", "
								~ to!(char[])(prop.maxThreadsDim[2]) ~ "]\n";
	res ~= "maxGridSize: ["	~ to!(char[])(prop.maxGridSize[0]) ~ ", " 
							~ to!(char[])(prop.maxGridSize[1]) ~ ", "
							~ to!(char[])(prop.maxGridSize[2]) ~ "]\n";
	res ~= "sharedMemPerBlock: " ~ to!(char[])(prop.sharedMemPerBlock) ~ "\n";
	res ~= "totalConstantMemory: " ~ to!(char[])(prop.totalConstantMemory) ~ "\n";
	res ~= "SIMDWidth: " ~ to!(char[])(prop.SIMDWidth) ~ "\n";
	res ~= "memPitch: " ~ to!(char[])(prop.memPitch) ~ "\n";
	res ~= "regsPerBlock: " ~ to!(char[])(prop.regsPerBlock) ~ "\n";
	res ~= "clockRate: " ~ to!(char[])(prop.clockRate) ~ "\n";
	res ~= "textureAlign: " ~ to!(char[])(prop.textureAlign);
	return res;
}

/* Globals */
// Thread block size
const int BLOCK_SIZE = 16;

// Matrix dimensions
// (chosen as multiples of the thread block size for simplicity)
const int WA = (3 * BLOCK_SIZE); // Matrix A width
const int HA = (5 * BLOCK_SIZE); // Matrix A height
const int WB = (8 * BLOCK_SIZE); // Matrix B width
const int HB = WA;  // Matrix B height
const int WC = WB;  // Matrix C width 
const int HC = HA;  // Matrix C height

CUdevice cuDevice;
CUcontext cuContext;
CUmodule cuModule;

CUresult initCUDA(CUfunction *pMatrixMul) {
	initDcuda();
	initDcutil();

	CUfunction cuFunction;

	CUdevice cuDevice;
	int deviceId = 0;
	deviceInitDrv(cuDevice, deviceId);

	int major, minor;
	cuDeviceComputeCapability(&major, &minor, cuDevice);
	Trace.formatln("id:{} major:{} minor: {}", deviceId, major, minor);
	CUdevprop prop;
	cuDeviceGetProperties(&prop, cuDevice);
	Trace.formatln("*** Properties***\n{}\n", CUdevpropToString(prop));

	cuSafeCallNoSync(cuCtxCreate(&cuContext, 0, cuDevice));

	cuSafeCallNoSync(cuModuleLoad(&cuModule, toStringz(".\\data\\matrixMul_kernel.cubin")));

	cuSafeCallNoSync(cuModuleGetFunction( &cuFunction, cuModule, "matrixMul" ));
	*pMatrixMul = cuFunction;
	return CUDA_SUCCESS;
}

void randomInit(float* data, int size) {
	auto rng = new Random;
	for (int i = 0; i < size; ++i)
		data[i] = rng.uniform!(float);
}

void computeGold(float* C, in float* A, in float* B, uint hA, uint wA, uint wB) {
    for(uint i = 0; i < hA; ++i)
        for(uint j = 0; j < wB; ++j) {
            double sum = 0;
            for(uint k = 0; k < wA; ++k) {
                double a = A[i * wA + k];
                double b = B[k * wB + j];
                sum += a * b;
            }
            C[i * wB + j] = cast(float)sum;
        }
}


/* Main */
void main() {
	// initialize CUDA
	CUfunction matrixMul = null;
	cuSafeCall(initCUDA(&matrixMul));

	// allocate host memory for matrices A and B
	uint size_A = WA * HA;
	uint mem_size_A = float.sizeof * size_A;
	float* h_A = cast(float*) malloc(mem_size_A);
	uint size_B = WB * HB;
	uint mem_size_B = float.sizeof * size_B;
	float* h_B = cast(float*) malloc(mem_size_B);

	// initialize host memory
	randomInit(h_A, size_A);
	randomInit(h_B, size_B);

	// allocate device memory
	CUdeviceptr d_A;
	cuSafeCall(cuMemAlloc( &d_A, mem_size_A ));
	CUdeviceptr d_B;
	cuSafeCall(cuMemAlloc( &d_B, mem_size_B )); 

	// copy host memory to device
	cuSafeCall(cuMemcpyHtoD( d_A, h_A, mem_size_A ));
	cuSafeCall(cuMemcpyHtoD( d_B, h_B, mem_size_B ));

	// allocate device memory for result
	uint size_C = WC * HC;
	uint mem_size_C = float.sizeof * size_C;
	CUdeviceptr d_C;
	cuSafeCall(cuMemAlloc(&d_C, mem_size_C));

	// create and start timer
	uint timer = 0;
	cutSafeCall(cutCreateTimer(&timer));
	cutSafeCall(cutStartTimer(timer));

	// setup execution parameters
	cuSafeCall(cuFuncSetBlockShape( matrixMul, BLOCK_SIZE, BLOCK_SIZE, 1 ));
	cuSafeCall(cuFuncSetSharedSize( matrixMul, 2*BLOCK_SIZE*BLOCK_SIZE*float.sizeof ) );
	cuSafeCall(cuParamSeti( matrixMul, 0,  d_C ));
	cuSafeCall(cuParamSeti( matrixMul, 4,  d_A ));
	cuSafeCall(cuParamSeti( matrixMul, 8,  d_B ));
	cuSafeCall(cuParamSeti( matrixMul, 12, WA ));
	cuSafeCall(cuParamSeti( matrixMul, 16, WB ));
	cuSafeCall(cuParamSetSize( matrixMul, 20 ));
	cuSafeCall(cuLaunchGrid( matrixMul, WC / BLOCK_SIZE, HC / BLOCK_SIZE ));

	// allocate mem for the result on host side
	float* h_C = cast(float*) malloc(mem_size_C);

	// copy result from device to host
	cuSafeCall(cuMemcpyDtoH(cast(void *) h_C, d_C, mem_size_C) );

	// stop and destroy timer
	cutSafeCall(cutStopTimer(timer));
	Trace.formatln("Processing time: {} (ms)\n", cutGetTimerValue(timer));
	cutSafeCall(cutDeleteTimer(timer));

	// compute reference solution
	float* reference = cast(float*) malloc(mem_size_C);
	computeGold(reference, h_A, h_B, HA, WA, WB);

	// check result
/+	float diff = 0.0f;
	for(int i = 0; i < size_C; i++) {
//		Trace.formatln("{} {}", h_C[i], reference[i]);
		diff += (h_C[i] - reference[i]) * (h_C[i] - reference[i]);
	}
	diff = sqrt(diff);
	Trace.formatln("Diff: {:f8}", diff);+/
	CUTBoolean res = cutCompareL2fe(reference, h_C, size_C, 1e-6f);
	Trace.formatln("Test {}\n", (1 == res) ? "PASSED" : "FAILED");

	// clean up memory
	free(h_A);
	free(h_B);
	free(h_C);
	free(reference);
	cuSafeCall(cuMemFree(d_A));
	cuSafeCall(cuMemFree(d_B));
	cuSafeCall(cuMemFree(d_C));	
	cuSafeCallNoSync(cuCtxDetach(cuContext));
}

