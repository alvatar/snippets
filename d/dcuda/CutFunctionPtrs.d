module xf.dcuda.CutFunctionPtrs;

private {
	import xf.dcuda.CutConsts;
}

extern (System):
    void function( void* ptr) cutFree;
    void function( uint tidx, uint tidy, uint tidz,
                   uint bdimx, uint bdimy,
                   uint bdimz, char* file, int line,
                   char* aname, int index) cutCheckBankAccess;
    char* function(char* filename, char* executablePath) cutFindFilePath;
    CUTBoolean function( char* filename, float** data, uint* len,
                         bool verbose = false) cutReadFilef;
    CUTBoolean function( char* filename, double** data, uint* len,
                         bool verbose = false) cutReadFiled;
    CUTBoolean function( char* filename, int** data, uint* len, bool verbose = false) cutReadFilei;
    CUTBoolean function( char* filename, uint** data,
                         uint* len, bool verbose = false) cutReadFileui;
    CUTBoolean function( char* filename, char** data, uint* len,
                         bool verbose = false) cutReadFileb;
    CUTBoolean function( char* filename, ubyte** data,
                         uint* len, bool verbose = false) cutReadFileub;
    CUTBoolean function( char* filename, float* data, uint len,
                         float epsilon, bool verbose = false) cutWriteFilef;
    CUTBoolean function( char* filename, float* data, uint len,
                         double epsilon, bool verbose = false) cutWriteFiled;
    CUTBoolean function( char* filename, int* data, uint len,
                         bool verbose = false) cutWriteFilei;
    CUTBoolean function( char* filename,uint* data,
                         uint len, bool verbose = false) cutWriteFileui;
    CUTBoolean function( char* filename, char* data, uint len,
                         bool verbose = false) cutWriteFileb;
    CUTBoolean function( char* filename,ubyte* data,
                         uint len, bool verbose = false) cutWriteFileub;
    CUTBoolean function( char* file, ubyte** data,
                         uint *w,uint *h) cutLoadPGMub;
    CUTBoolean function( char* file, ubyte** data,
                         uint *w,uint *h) cutLoadPPMub;
    CUTBoolean function( char* file, ubyte** data,
                         uint *w,uint *h) cutLoadPPM4ub;
    CUTBoolean function( char* file, uint** data,
                         uint* w, uint* h) cutLoadPGMi;
    CUTBoolean function( char* file, ushort** data,
                         uint* w, uint* h) cutLoadPGMs;
    CUTBoolean function( char* file, float** data,
                         uint* w, uint* h) cutLoadPGMf;
    CUTBoolean function( char* file, ubyte* data,
                         uint w, uint h) cutSavePGMub;
    CUTBoolean function( char* file, ubyte *data,
                         uint w, uint h) cutSavePPMub;
    CUTBoolean function( char* file, ubyte *data,
                         uint w, uint h) cutSavePPM4ub;
    CUTBoolean function( char* file, uint* data,
                         uint w, uint h) cutSavePGMi;
    CUTBoolean function( char* file, ushort* data,
                         uint w, uint h) cutSavePGMs;
    CUTBoolean function( char* file, float* data,
                         uint w, uint h) cutSavePGMf;
    CUTBoolean function( int argc, char** argv,
                         char* flag_name) cutCheckCmdLineFlag;
    CUTBoolean function( int argc, char** argv,
                         char* arg_name, int* val) cutGetCmdLineArgumenti;
    CUTBoolean function( int argc, char** argv,
                         char* arg_name, float* val) cutGetCmdLineArgumentf;
    CUTBoolean function( int argc, char** argv,
                         char* arg_name, char** val) cutGetCmdLineArgumentstr;
    CUTBoolean function( int argc, char** argv,
                         char* arg_name, char** val,
                         uint* len) cutGetCmdLineArgumentListstr;
    CUTBoolean function( int val, char* file, int line) cutCheckCondition;
    CUTBoolean function( float* reference, float* data,
                         uint len) cutComparef;
    CUTBoolean function( int* reference, int* data,
                         uint len ) cutComparei;
	CUTBoolean function( uint* reference, uint* data,
						 uint len, float epsilon, float threshold ) cutCompareuit;
    CUTBoolean function( ubyte* reference, ubyte* data,
                         uint len ) cutCompareub;
    CUTBoolean function( ubyte* reference, ubyte* data,
						 uint len, float epsilon, float threshold ) cutCompareubt;
    CUTBoolean function( ubyte* reference, ubyte* data,
                         uint len, float epsilon ) cutCompareube;
    CUTBoolean function( float* reference, float* data,
                         uint len, float epsilon ) cutComparefe;
	CUTBoolean function( float* reference, float* data,
						 uint len, float epsilon, float threshold ) cutComparefet;
    CUTBoolean function( float* reference, float* data,
                         uint len, float epsilon ) cutCompareL2fe;
	CUTBoolean function	( char *src_file, char *ref_file, float epsilon, float threshold, bool verboseErrors = false ) cutComparePPM;
    CUTBoolean function( uint* name) cutCreateTimer;
    CUTBoolean function( uint name) cutDeleteTimer;
    CUTBoolean function( uint name) cutStartTimer;
    CUTBoolean function( uint name) cutStopTimer;
    CUTBoolean function( uint name) cutResetTimer;
    float function( uint name) cutGetTimerValue;
    float function( uint name) cutGetAverageTimerValue;
