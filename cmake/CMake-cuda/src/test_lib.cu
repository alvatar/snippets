
#include <external_dependency.h>

__global__ void kernel(unsigned int *in, unsigned int *out) {

  // Note this kernel doesn't do anything. 
  // It's just filler to demonstrate how to setup cmake.

  for (unsigned int i=0;i<blockDim.x;++i) {
    // /*const*/ unsigned int thread = threadIdx.x;
    out[threadIdx.x] = in[threadIdx.x] * in[threadIdx.x];
  }
};






