module xf.dcuda.CutFunctions;

private {
	import xf.dcuda.CutConsts;
	import xf.dcuda.CutFunctionPtrs;
}

void loadCutFunctions_(void* function(char*) loadFuncFromLib) {
	*cast(void**)&cutFree = loadFuncFromLib("cutFree");
	*cast(void**)&cutCheckBankAccess = loadFuncFromLib("cutCheckBankAccess");
	*cast(void**)&cutFindFilePath = loadFuncFromLib("cutFindFilePath");
	*cast(void**)&cutReadFilef = loadFuncFromLib("cutReadFilef");
	*cast(void**)&cutReadFiled = loadFuncFromLib("cutReadFiled");
	*cast(void**)&cutReadFilei = loadFuncFromLib("cutReadFilei");
	*cast(void**)&cutReadFileui = loadFuncFromLib("cutReadFileui");
	*cast(void**)&cutReadFileb = loadFuncFromLib("cutReadFileb");
	*cast(void**)&cutReadFileub = loadFuncFromLib("cutReadFileub");
	*cast(void**)&cutWriteFilef = loadFuncFromLib("cutWriteFilef");
//	*cast(void**)&cutWriteFiled = loadFuncFromLib("cutWriteFiled");
	*cast(void**)&cutWriteFilei = loadFuncFromLib("cutWriteFilei");
	*cast(void**)&cutWriteFileui = loadFuncFromLib("cutWriteFileui");
	*cast(void**)&cutWriteFileb = loadFuncFromLib("cutWriteFileb");
	*cast(void**)&cutWriteFileub = loadFuncFromLib("cutWriteFileub");
	*cast(void**)&cutLoadPGMub = loadFuncFromLib("cutLoadPGMub");
	*cast(void**)&cutLoadPPMub = loadFuncFromLib("cutLoadPPMub");
	*cast(void**)&cutLoadPPM4ub = loadFuncFromLib("cutLoadPPM4ub");
	*cast(void**)&cutLoadPGMi = loadFuncFromLib("cutLoadPGMi");
	*cast(void**)&cutLoadPGMs = loadFuncFromLib("cutLoadPGMs");
	*cast(void**)&cutLoadPGMf = loadFuncFromLib("cutLoadPGMf");
	*cast(void**)&cutSavePGMub = loadFuncFromLib("cutSavePGMub");
	*cast(void**)&cutSavePPMub = loadFuncFromLib("cutSavePPMub");
	*cast(void**)&cutSavePPM4ub = loadFuncFromLib("cutSavePPM4ub");
	*cast(void**)&cutSavePGMi = loadFuncFromLib("cutSavePGMi");
	*cast(void**)&cutSavePGMs = loadFuncFromLib("cutSavePGMs");
	*cast(void**)&cutSavePGMf = loadFuncFromLib("cutSavePGMf");
	*cast(void**)&cutCheckCmdLineFlag = loadFuncFromLib("cutCheckCmdLineFlag");
	*cast(void**)&cutGetCmdLineArgumenti = loadFuncFromLib("cutGetCmdLineArgumenti");
	*cast(void**)&cutGetCmdLineArgumentf = loadFuncFromLib("cutGetCmdLineArgumentf");
	*cast(void**)&cutGetCmdLineArgumentstr = loadFuncFromLib("cutGetCmdLineArgumentstr");
//	*cast(void**)&cutGetCmdLineArgumentListstr = loadFuncFromLib("cutGetCmdLineArgumentListstr");
	*cast(void**)&cutCheckCondition = loadFuncFromLib("cutCheckCondition");
	*cast(void**)&cutComparef = loadFuncFromLib("cutComparef");
	*cast(void**)&cutComparei = loadFuncFromLib("cutComparei");
	*cast(void**)&cutCompareuit = loadFuncFromLib("cutCompareuit");
	*cast(void**)&cutCompareub = loadFuncFromLib("cutCompareub");
	*cast(void**)&cutCompareubt = loadFuncFromLib("cutCompareubt");
	*cast(void**)&cutCompareube = loadFuncFromLib("cutCompareube");
	*cast(void**)&cutComparefe = loadFuncFromLib("cutComparefe");
	*cast(void**)&cutComparefet = loadFuncFromLib("cutComparefet");
	*cast(void**)&cutCompareL2fe = loadFuncFromLib("cutCompareL2fe");
	*cast(void**)&cutComparePPM = loadFuncFromLib("cutComparePPM");
	*cast(void**)&cutCreateTimer = loadFuncFromLib("cutCreateTimer");
	*cast(void**)&cutDeleteTimer = loadFuncFromLib("cutDeleteTimer");
	*cast(void**)&cutStartTimer = loadFuncFromLib("cutStartTimer");
	*cast(void**)&cutStopTimer = loadFuncFromLib("cutStopTimer");
	*cast(void**)&cutResetTimer = loadFuncFromLib("cutResetTimer");
	*cast(void**)&cutGetTimerValue = loadFuncFromLib("cutGetTimerValue");
	*cast(void**)&cutGetAverageTimerValue = loadFuncFromLib("cutGetAverageTimerValue");
}
