module xf.dcuda.Utils;

private {
	import xf.dcuda.cuda;
}

void deviceInitDrv(ref CUdevice cuDevice, int deviceId = 0) {
	CUresult result;
	result = cuInit(0);
	assert(CUDA_SUCCESS == result, "cuInit failed.\n");
	int deviceCount;
	cuSafeCallNoSync(cuDeviceGetCount(&deviceCount));
	assert(deviceCount > 0, "No devices supporting CUDA.\n");
	assert(deviceId < deviceCount, "Wrong device selected.\n");
	cuSafeCallNoSync(cuDeviceGet(&cuDevice, deviceId));
}
