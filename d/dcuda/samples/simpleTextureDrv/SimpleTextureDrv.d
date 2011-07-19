module SimpleTextureDrv;

// includes, system
import tango.util.log.Trace;
import tango.stdc.string : memset;
import tango.stdc.stringz : toStringz;
import tango.stdc.stdlib : malloc, free;

// includes, CUDA
import xf.dcuda.cuda;
import xf.dcuda.Utils;
import xf.dcuda.cutil;

char[] image_path = ".\\data\\lena_bw.pgm";
char[] ref_path   = ".\\data\\ref_rotated.pgm";
float angle = 0.5f;    // angle to rotate image by (in radians)
////////////////////////////////////////////////////////////////////////////////
// Globals
////////////////////////////////////////////////////////////////////////////////
CUdevice cuDevice;
CUcontext cuContext;
CUmodule cuModule;

////////////////////////////////////////////////////////////////////////////////
//! This initializes CUDA, and loads the *.cubin CUDA module containing the
//! kernel function binary.  After the module is loaded, cuModuleGetFunction 
//! retrieves the CUDA function pointer "cuFunction" 
////////////////////////////////////////////////////////////////////////////////
CUresult initCUDA(CUfunction *pMatrixMul) {
	initDcuda();
	initDcutil();

	CUfunction cuFunction;

	CUdevice cuDevice;
	int deviceId = 0;
	deviceInitDrv(cuDevice, deviceId);

	cuSafeCallNoSync(cuCtxCreate(&cuContext, 0, cuDevice));

	cuSafeCallNoSync(cuModuleLoad(&cuModule, toStringz(".\\data\\simpleTexture_kernel.cubin")));

	cuSafeCallNoSync(cuModuleGetFunction( &cuFunction, cuModule, "transformKernel" ));
	*pMatrixMul = cuFunction;
	return CUDA_SUCCESS;
}
////////////////////////////////////////////////////////////////////////////////
//! Run a simple test for CUDA
////////////////////////////////////////////////////////////////////////////////
void main() {
    // initialize CUDA
    CUfunction transform;
    cuSafeCall(initCUDA(&transform));

    // load image from disk
    float* h_data = null;
    uint width, height;
    cutSafeCall( cutLoadPGMf(toStringz(image_path), &h_data, &width, &height));

    uint size = width * height * float.sizeof;
    Trace.formatln("Loaded {}, {} x {} pixels", image_path, width, height);

    // load reference image from image (output)
    float *h_data_ref = cast(float*) malloc(size);

    cutSafeCall( cutLoadPGMf(toStringz(ref_path), &h_data_ref, &width, &height));

    // allocate device memory for result
    CUdeviceptr d_data;
    cuSafeCall( cuMemAlloc( &d_data, size));

    // allocate array and copy image data
    CUarray cu_array;
    CUDA_ARRAY_DESCRIPTOR desc;
    desc.Format = CU_AD_FORMAT_FLOAT;
    desc.NumChannels = 1;
    desc.Width = width;
    desc.Height = height;
    cuSafeCall( cuArrayCreate( &cu_array, &desc ));
	CUDA_MEMCPY2D copyParam;
	memset(&copyParam, 0, copyParam.sizeof);
	copyParam.dstMemoryType = CU_MEMORYTYPE_ARRAY;
	copyParam.dstArray = cu_array;
	copyParam.srcMemoryType = CU_MEMORYTYPE_HOST;
	copyParam.srcHost = h_data;
	copyParam.srcPitch = width * float.sizeof;
	copyParam.WidthInBytes = copyParam.srcPitch;
	copyParam.Height = height;
    cuSafeCall(cuMemcpy2D(&copyParam));

    // set texture parameters
    CUtexref cu_texref;
    cuSafeCall(cuModuleGetTexRef(&cu_texref, cuModule, "tex"));
    cuSafeCall(cuTexRefSetArray(cu_texref, cu_array, CU_TRSA_OVERRIDE_FORMAT));
    cuSafeCall(cuTexRefSetAddressMode(cu_texref, 0, CU_TR_ADDRESS_MODE_WRAP));
    cuSafeCall(cuTexRefSetAddressMode(cu_texref, 1, CU_TR_ADDRESS_MODE_WRAP));
    cuSafeCall(cuTexRefSetFilterMode(cu_texref, CU_TR_FILTER_MODE_LINEAR));
    cuSafeCall(cuTexRefSetFlags(cu_texref, CU_TRSF_NORMALIZED_COORDINATES));
    cuSafeCall(cuTexRefSetFormat(cu_texref, CU_AD_FORMAT_FLOAT, 1));

	int block_size = 8;
    cuSafeCall(cuFuncSetBlockShape( transform, block_size, block_size, 1 ));
	int offset = 0;
    cuSafeCall(cuParamSeti(transform, offset, d_data)); offset += d_data.sizeof;
    cuSafeCall(cuParamSeti(transform, offset, width));  offset += width.sizeof;
    cuSafeCall(cuParamSeti(transform, offset, height)); offset += height.sizeof;
    cuSafeCall(cuParamSetf(transform, offset, angle));  offset += angle.sizeof;
    cuSafeCall(cuParamSetSize(transform, offset));
    cuSafeCall(cuParamSetTexRef(transform, CU_PARAM_TR_DEFAULT, cu_texref));

    // warmup
    cuSafeCall(cuLaunchGrid( transform, width / block_size, height / block_size ));

    cuSafeCall( cuCtxSynchronize() );
    uint timer = 0;
    cutSafeCall( cutCreateTimer( &timer));
    cutSafeCall( cutStartTimer( timer));

    // execute the kernel
    cuSafeCall(cuLaunchGrid( transform, width / block_size, height / block_size ));

    cuSafeCall( cuCtxSynchronize() );
    cutSafeCall( cutStopTimer( timer));
    Trace.formatln("Processing time: {} (ms)", cutGetTimerValue( timer));
    Trace.formatln("{} Mpixels/sec", (width*height / (cutGetTimerValue( timer) / 1000.0f)) / 1e6);
    cutSafeCall( cutDeleteTimer( timer));

    // allocate mem for the result on host side
    float* h_odata = cast(float*) malloc( size);
    // copy result from device to host
    cuSafeCall( cuMemcpyDtoH( h_odata, d_data, size) );

    // write result to file
    char[] output_filename;
    output_filename = image_path;
	output_filename.length = output_filename.length - 4;
	output_filename ~= "_out.pgm";
//    strcpy(output_filename + strlen(image_path) - 4, "_out.pgm");
    cutSafeCall( cutSavePGMf( toStringz(output_filename), h_odata, width, height));
    Trace.formatln("Wrote {}", output_filename);

    // write regression file if necessary
/+    if( cutCheckCmdLineFlag( argc, (const char**) argv, "regression")) 
    {
        // write file for regression test
        cutSafeCall( cutWriteFilef( "./data/regression.dat", h_odata, width*height, 0.0));
    } 
    else +/
    {
        // We need to reload the data from disk, because it is inverted upon output
        cutSafeCall( cutLoadPGMf(toStringz(output_filename), &h_odata, &width, &height));

        Trace.formatln("Comparing files");
        Trace.formatln("\toutput:    <{}>", output_filename);
        Trace.formatln("\treference: <{}>", ref_path);
        CUTBoolean res = cutComparef( h_odata, h_data_ref, width*height);
        Trace.formatln( "Test {}", (1 == res) ? "PASSED" : "FAILED");
    }

    // cleanup memory
    cuSafeCall(cuMemFree(d_data));
    cuSafeCall(cuArrayDestroy(cu_array));
    cutFree(h_data);
    free(h_data_ref);
    free(h_odata);
    cuSafeCallNoSync(cuCtxDetach(cuContext));

    // If we are doing the QAtest, we quite without prompting
/+    if( cutCheckCmdLineFlag( argc, (const char**) argv, "qatest") ||
        cutCheckCmdLineFlag( argc, (const char**) argv, "noprompt"))
    {
        exit(0);
    }+/

}

