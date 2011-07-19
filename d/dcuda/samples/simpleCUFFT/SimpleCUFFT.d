module SimpleCUFFT;
//import tango.core.stacktrace.TraceExceptions;

import xf.dcuda.cuda;
import xf.dcuda.cutil;
import xf.dcuda.cufft;
import xf.dcuda.Utils;

import tango.util.log.Trace;
import tango.util.Convert : to;
import tango.stdc.stringz : toStringz;
import tango.stdc.stdlib : malloc, free;
import tango.stdc.string : memset, memcpy;
import tango.math.random.Random;

/* Globals */
// The filter size is assumed to be a number smaller than the signal size
const int SIGNAL_SIZE = 50;
const int FILTER_KERNEL_SIZE = 50;

CUdevice cuDevice;
CUcontext cuContext;

// Pad data
int PadData(cfloat* signal, cfloat** padded_signal, int signal_size, cfloat* filter_kernel, cfloat** padded_filter_kernel, int filter_kernel_size) {
	int minRadius = filter_kernel_size / 2;
	int maxRadius = filter_kernel_size - minRadius;
	int new_size = signal_size + maxRadius;

	// Pad signal
	cfloat* new_data = cast(cfloat*)malloc(cfloat.sizeof * new_size);
	memcpy(new_data +           0, signal,              signal_size * cfloat.sizeof);
	memset(new_data + signal_size,      0, (new_size - signal_size) * cfloat.sizeof);
	*padded_signal = new_data;
	
	// Pad filter
	new_data = cast(cfloat*)malloc(cfloat.sizeof * new_size);  
	memcpy(new_data +                    0, filter_kernel + minRadius,                       maxRadius * cfloat.sizeof);
	memset(new_data +            maxRadius,                         0, (new_size - filter_kernel_size) * cfloat.sizeof);   
	memcpy(new_data + new_size - minRadius,             filter_kernel,                       minRadius * cfloat.sizeof);
	*padded_filter_kernel = new_data;

	return new_size;
}

// Computes convolution on the host
void Convolve(cfloat* signal, int signal_size, cfloat* filter_kernel, int filter_kernel_size, cfloat* filtered_signal) {
	int minRadius = filter_kernel_size / 2;
	int maxRadius = filter_kernel_size - minRadius;
	// Loop over output element indices
	for (int i = 0; i < signal_size; ++i) {
		filtered_signal[i] = cast(cfloat)0;
		// Loop over convolution indices
		for (int j = - maxRadius + 1; j <= minRadius; ++j) {
			int k = i + j;
			if (k >= 0 && k < signal_size) 
				filtered_signal[i] = filtered_signal[i] + (signal[k] * filter_kernel[minRadius - j]);
		}
	}
}


CUresult initCUDA(CUfunction *pFnc) {
	initDcuda();
	initDcutil();
	initDcufft();

	CUmodule cuModule;
	CUfunction cuFunction;

	CUdevice cuDevice;
	int deviceId = 0;
	deviceInitDrv(cuDevice, deviceId);

	int major, minor;
	cuDeviceComputeCapability(&major, &minor, cuDevice);
	Trace.formatln("id:{} major:{} minor: {}", deviceId, major, minor);

	cuSafeCallNoSync(cuCtxCreate(&cuContext, 0, cuDevice));

	cuSafeCallNoSync(cuModuleLoad(&cuModule, toStringz(".\\data\\ComplexPointwiseMulAndScale_kernel.cubin")));

	cuSafeCallNoSync(cuModuleGetFunction( &cuFunction, cuModule, "ComplexPointwiseMulAndScale" ));
	*pFnc = cuFunction;
	return CUDA_SUCCESS;
}

/* Main */
void main() {
// initialize CUDA
	CUfunction ComplexPointwiseMulAndScale = null;
	cuSafeCall(initCUDA(&ComplexPointwiseMulAndScale));


	// Allocate host memory for the signal
	cfloat* h_signal = cast(cfloat*)malloc(cfloat.sizeof * SIGNAL_SIZE);
	// Initalize the memory for the signal
	auto rng = new Random;
	for (uint i = 0; i < SIGNAL_SIZE; ++i) {
		h_signal[i] = cast(cfloat)rng.uniform!(float);
	}

	// Allocate host memory for the filter
	cfloat* h_filter_kernel = cast(cfloat*)malloc(cfloat.sizeof * FILTER_KERNEL_SIZE);
	// Initalize the memory for the filter
	for (uint i = 0; i < FILTER_KERNEL_SIZE; ++i) {
    	h_filter_kernel[i] = cast(cfloat)rng.uniform!(float);
	}

	// Pad signal and filter kernel
	cfloat* h_padded_signal;
	cfloat* h_padded_filter_kernel;
	int new_size = PadData(h_signal, &h_padded_signal, SIGNAL_SIZE, h_filter_kernel, &h_padded_filter_kernel, FILTER_KERNEL_SIZE);
	int mem_size = cfloat.sizeof * new_size;

	// Allocate device memory for signal
	CUdeviceptr d_signal;
	cuSafeCall(cuMemAlloc(&d_signal, mem_size));
	// Copy host memory to device
	cuSafeCall(cuMemcpyHtoD(d_signal, h_padded_signal, mem_size));

	// Allocate device memory for filter kernel
	CUdeviceptr d_filter_kernel;
	cuSafeCall(cuMemAlloc(&d_filter_kernel, mem_size));

	// Copy host memory to device
	cuSafeCall(cuMemcpyHtoD(d_filter_kernel, h_padded_filter_kernel, mem_size));

	// CUFFT plan
	cufftHandle plan;
	cufftSafeCall(cufftPlan1d(&plan, new_size, CUFFT_C2C, 1));

	// Transform signal and kernel
	cufftSafeCall(cufftExecC2C(plan, cast(cufftComplex *)d_signal, cast(cufftComplex *)d_signal, CUFFT_FORWARD));
	cufftSafeCall(cufftExecC2C(plan, cast(cufftComplex *)d_filter_kernel, cast(cufftComplex *)d_filter_kernel, CUFFT_FORWARD));

	// setup execution parameters
	cuSafeCall(cuFuncSetBlockShape( ComplexPointwiseMulAndScale, 256, 1, 1 ));
	cuSafeCall(cuParamSeti( ComplexPointwiseMulAndScale, 0,  d_signal ));
	cuSafeCall(cuParamSeti( ComplexPointwiseMulAndScale, 4,  d_filter_kernel ));
	cuSafeCall(cuParamSeti( ComplexPointwiseMulAndScale, 8,  new_size ));
	cuSafeCall(cuParamSetf( ComplexPointwiseMulAndScale, 12, 1.0f/cast(float)new_size ));
	cuSafeCall(cuParamSetSize( ComplexPointwiseMulAndScale, 16 ));
	cuSafeCall(cuLaunchGrid( ComplexPointwiseMulAndScale, 32, 1 ));

	// Transform signal back
	cufftSafeCall(cufftExecC2C(plan, cast(cufftComplex *)d_signal, cast(cufftComplex *)d_signal, CUFFT_INVERSE));

	// Copy device memory to host
	cfloat* h_convolved_signal = h_padded_signal;
	cuSafeCall(cuMemcpyDtoH(h_convolved_signal, d_signal, mem_size));

	// Allocate host memory for the convolution result
	cfloat* h_convolved_signal_ref = cast(cfloat*)malloc(cfloat.sizeof * SIGNAL_SIZE);

	// Convolve on the host
	Convolve(h_signal, SIGNAL_SIZE, h_filter_kernel, FILTER_KERNEL_SIZE, h_convolved_signal_ref);

	// check result
	CUTBoolean res = cutCompareL2fe(cast(float*)h_convolved_signal_ref, cast(float*)h_convolved_signal, 2 * SIGNAL_SIZE, 1e-5f);
	Trace.formatln("Test {}", (1 == res) ? "PASSED" : "FAILED");

	//Destroy CUFFT context
	cufftSafeCall(cufftDestroy(plan));

    // cleanup memory
	free(h_signal);
	free(h_filter_kernel);
	free(h_padded_signal);
	free(h_padded_filter_kernel);
	free(h_convolved_signal_ref);
	cuSafeCall(cuMemFree(d_signal));
	cuSafeCall(cuMemFree(d_filter_kernel));

    cuSafeCallNoSync(cuCtxDetach(cuContext));
}

