#if defined(__cplusplus)
extern "C" {
#endif
#include "test_bin.cudafe1.gpu"
#include "crt/host_runtime.h"
struct __T20;
struct __T20 {unsigned *__par0;unsigned *__par1;int __dummy_field;__pad__(volatile char __dummy[4];)};
#if defined(__device_emulation)
static void __device_wrapper__Z6kernelPjS_(char *);
#endif
static void __sti____cudaRegisterAll_16_test_bin_cpp1_ii_dde60ace(void) __attribute__((__constructor__));
void __device_stub__Z6kernelPjS_(unsigned *__par0, unsigned *__par1){auto struct __T20 *__T21;
__cudaInitArgBlock(__T21);__cudaSetupArg(__par0, __T21);__cudaSetupArg(__par1, __T21);__cudaLaunch(((char *)__device_stub__Z6kernelPjS_));}
#if defined(__device_emulation)
static void __device_wrapper__Z6kernelPjS_(char *__T22){_Z6kernelPjS_((((*((struct __T20 *)__T22)).__par0)), (((*((struct __T20 *)__T22)).__par1)));}
#endif
static void __sti____cudaRegisterAll_16_test_bin_cpp1_ii_dde60ace(void){__cudaRegisterBinary();__cudaRegisterEntry(_Z6kernelPjS_, (-1));}
#if defined(__cplusplus)
}
#endif
