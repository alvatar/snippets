sed -e 's/#line.*/module xf.prof.DbgInfo;import xf.utils.Memory;/' -e 's/.*_initLGPLHostExecutableDebugInfo.*/void initDebugInfo(char[] progName) {/' ../tangoTrace3/parts/DbgInfo.di >DbgInfo.d
sed -e 's/#line.*/module xf.prof.Demangler;/' -e 's/module jive.demangle;//' ../tangoTrace3/parts/Demangler.di >Demangler.d
echo module xf.prof.Trace; > Trace.d
echo import xf.prof.WinApi; >> Trace.d
echo import xf.prof.DbgInfo; >> Trace.d
echo import xf.prof.Demangler; >> Trace.d
echo import tango.text.convert.Format; >> Trace.d
echo import xf.utils.Memory; >> Trace.d
echo version=StacktraceUseWinApiStackWalking; >> Trace.d
sed -e '/#line/,/}/d' -e 's/private extern(C)/private extern(C) extern/' -e 's/fiberRunFuncLength = 0;/fiberRunFuncLength;/' ../tangoTrace3/parts/Main.di >>Trace.d
