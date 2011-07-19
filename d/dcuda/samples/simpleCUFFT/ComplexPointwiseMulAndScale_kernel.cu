
// float2 addition
__device__ inline float2 ComplexAdd(float2 a, float2 b) {
	float2 c;
	c.x = a.x + b.x;
	c.y = a.y + b.y;
	return c;
}

// float2 scale
__device__ inline float2 ComplexScale(float2 a, float s) {
	float2 c;
	c.x = s * a.x;
	c.y = s * a.y;
	return c;
}

// float2 multiplication
__device__ inline float2 ComplexMul(float2 a, float2 b) {
	float2 c;
	c.x = a.x * b.x - a.y * b.y;
	c.y = a.x * b.y + a.y * b.x;
	return c;
}

// float2 pointwise multiplication
extern "C" __global__ void ComplexPointwiseMulAndScale(float2* a, const float2* b, int size, float scale) {
	const int numThreads = blockDim.x * gridDim.x;
	const int threadID = blockIdx.x * blockDim.x + threadIdx.x;
	for (int i = threadID; i < size; i += numThreads)
		a[i] = ComplexScale(ComplexMul(a[i], b[i]), scale);     
} 

