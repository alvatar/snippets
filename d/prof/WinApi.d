module xf.prof.WinApi;
public:

import xf.platform.win32.winbase;
import xf.platform.win32.winnt;
import tango.text.Util;
import tango.io.Stdout;
import tango.core.Thread;
import tango.core.Array;
import tango.sys.Common : SysError;
import tango.sys.SharedLib : SharedLib;
import tango.stdc.stdio;
import tango.stdc.string;
import tango.stdc.stringz;



static this() {
	auto dbghelp = SharedLib.load(`dbghelp.dll`);
	
	SymEnumerateModules64 = cast(fp_SymEnumerateModules64)dbghelp.getSymbol("SymEnumerateModules64");
	assert (SymEnumerateModules64 !is null);
	EnumerateLoadedModules64 = cast(fp_EnumerateLoadedModules64)dbghelp.getSymbol("EnumerateLoadedModules64");
	assert (EnumerateLoadedModules64 !is null);
	SymFromAddr = cast(fp_SymFromAddr)dbghelp.getSymbol("SymFromAddr");
	assert (SymFromAddr !is null);
	SymLoadModule64 = cast(fp_SymLoadModule64)dbghelp.getSymbol("SymLoadModule64");
	assert (SymLoadModule64 !is null);
	SymInitialize = cast(fp_SymInitialize)dbghelp.getSymbol("SymInitialize");
	assert (SymInitialize !is null);
	SymCleanup = cast(fp_SymCleanup)dbghelp.getSymbol("SymCleanup");
	assert (SymCleanup !is null);
	SymSetOptions = cast(fp_SymSetOptions)dbghelp.getSymbol("SymSetOptions");
	assert (SymSetOptions !is null);
	SymGetLineFromAddr64 = cast(fp_SymGetLineFromAddr64)dbghelp.getSymbol("SymGetLineFromAddr64");
	assert (SymGetLineFromAddr64 !is null);
	SymEnumSymbols = cast(fp_SymEnumSymbols)dbghelp.getSymbol("SymEnumSymbols");
	assert (SymEnumSymbols !is null);
	SymGetModuleBase64 = cast(fp_SymGetModuleBase64)dbghelp.getSymbol("SymGetModuleBase64");
	assert (SymGetModuleBase64 !is null);
	StackWalk64 = cast(fp_StackWalk64)dbghelp.getSymbol("StackWalk64");
	assert (StackWalk64 !is null);
	SymFunctionTableAccess64 = cast(fp_SymFunctionTableAccess64)dbghelp.getSymbol("SymFunctionTableAccess64");
	assert (SymFunctionTableAccess64 !is null);
	
	
	auto psapi = SharedLib.load(`psapi.dll`);
	GetModuleFileNameExA = cast(fp_GetModuleFileNameExA)psapi.getSymbol("GetModuleFileNameExA");
	assert (GetModuleFileNameExA !is null);
}


extern (Windows) {
	fp_SymFromAddr		SymFromAddr;
	fp_SymLoadModule64	SymLoadModule64;
	fp_SymInitialize			SymInitialize;
	fp_SymCleanup			SymCleanup;
	fp_SymSetOptions		SymSetOptions;
	fp_SymGetLineFromAddr64	SymGetLineFromAddr64;
	fp_SymEnumSymbols			SymEnumSymbols;
	fp_SymGetModuleBase64	SymGetModuleBase64;
	fp_GetModuleFileNameExA		GetModuleFileNameExA;
	fp_StackWalk64						StackWalk64;
	fp_SymFunctionTableAccess64	SymFunctionTableAccess64;
	fp_SymEnumerateModules64		SymEnumerateModules64;
	fp_EnumerateLoadedModules64	EnumerateLoadedModules64;


	enum { ERROR_SUCCESS = 0 }

	alias DWORD function(
		DWORD SymOptions
	) fp_SymSetOptions;
	
	enum {
		SYMOPT_ALLOW_ABSOLUTE_SYMBOLS = 0x00000800,
		SYMOPT_DEFERRED_LOADS = 0x00000004,
		SYMOPT_UNDNAME = 0x00000002,
		SYMOPT_LOAD_LINES = 0x10,
		SYMOPT_OMAP_FIND_NEAREST = 0x20,
		SYMOPT_LOAD_ANYTHING = 0x40
	}

	alias BOOL function(
		HANDLE hProcess,
		LPCTSTR UserSearchPath,
		BOOL fInvadeProcess
	) fp_SymInitialize;
	
	alias BOOL function(
		HANDLE hProcess
	) fp_SymCleanup;

	alias DWORD64 function(
		HANDLE hProcess,
		HANDLE hFile,
		LPSTR ImageName,
		LPSTR ModuleName,
		DWORD64 BaseOfDll,
		DWORD SizeOfDll
	) fp_SymLoadModule64;
	
	struct SYMBOL_INFO {
		ULONG SizeOfStruct;
		ULONG TypeIndex;
		ULONG64 Reserved[2];
		ULONG Index;
		ULONG Size;
		ULONG64 ModBase;
		ULONG Flags;
		ULONG64 Value;
		ULONG64 Address;
		ULONG Register;
		ULONG Scope;
		ULONG Tag;
		ULONG NameLen;
		ULONG MaxNameLen;
		TCHAR Name[1];
	}
	alias SYMBOL_INFO* PSYMBOL_INFO;
	
	alias BOOL function(
		HANDLE hProcess,
		DWORD64 Address,
		PDWORD64 Displacement,
		PSYMBOL_INFO Symbol
	) fp_SymFromAddr;

	alias BOOL function(
		HANDLE hProcess,
		PSYM_ENUMMODULES_CALLBACK64 EnumModulesCallback,
		PVOID UserContext
	) fp_SymEnumerateModules64;
	
	alias BOOL function (
		HANDLE hProcess,
		PENUMLOADED_MODULES_CALLBACK64 EnumLoadedModulesCallback,
		PVOID UserContext
	) fp_EnumerateLoadedModules64;

	alias BOOL function(
		  char* ModuleName,
		  DWORD64 ModuleBase,
		  ULONG ModuleSize,
		  PVOID UserContext
	) PENUMLOADED_MODULES_CALLBACK64;
	
	alias BOOL function(
		LPTSTR ModuleName,
		DWORD64 BaseOfDll,
		PVOID UserContext
	) PSYM_ENUMMODULES_CALLBACK64;

	enum {
		TH32CS_SNAPPROCESS = 0x00000002,
		TH32CS_SNAPTHREAD = 0x00000004
	}
	
	HANDLE CreateToolhelp32Snapshot(
		DWORD dwFlags,
		DWORD th32ProcessID
	);

	BOOL Process32First(
		HANDLE hSnapshot,
		LPPROCESSENTRY32 lppe
	);
	
	BOOL Process32Next(
		HANDLE hSnapshot,
		LPPROCESSENTRY32 lppe
	);
	
	BOOL Thread32First(
		HANDLE hSnapshot,
		LPTHREADENTRY32 lpte
	);

	BOOL Thread32Next(
		HANDLE hSnapshot,
		LPTHREADENTRY32 lpte
	);

	struct PROCESSENTRY32 {
		DWORD dwSize;
		DWORD cntUsage;
		DWORD th32ProcessID;
		ULONG_PTR th32DefaultHeapID;
		DWORD th32ModuleID;
		DWORD cntThreads;
		DWORD th32ParentProcessID;
		LONG pcPriClassBase;
		DWORD dwFlags;
		TCHAR szExeFile[MAX_PATH];
	}
	alias PROCESSENTRY32* LPPROCESSENTRY32;
	
	struct THREADENTRY32 {
		DWORD dwSize;
		DWORD cntUsage;
		DWORD th32ThreadID;
		DWORD th32OwnerProcessID;
		LONG tpBasePri;
		LONG tpDeltaPri;
		DWORD dwFlags;
	}
	alias THREADENTRY32* LPTHREADENTRY32;


	enum {
		MAX_MODULE_NAME32 = 255,
		TH32CS_SNAPMODULE = 0x00000008,
	}

   struct MODULEENTRY32 {
		DWORD  dwSize;
		DWORD  th32ModuleID;
		DWORD  th32ProcessID;
		DWORD  GlblcntUsage;
		DWORD  ProccntUsage;
		BYTE  *modBaseAddr;
		DWORD  modBaseSize;
		HMODULE hModule;
		char   szModule[MAX_MODULE_NAME32 + 1];
		char   szExePath[MAX_PATH];
	}

	/+struct IMAGEHLP_LINE {
		DWORD SizeOfStruct;
		PVOID Key;
		DWORD LineNumber;
		PTSTR FileName;
		DWORD Address;
	}
	alias IMAGEHLP_LINE* PIMAGEHLP_LINE;+/

	struct IMAGEHLP_LINE64 {
		DWORD SizeOfStruct;
		PVOID Key;
		DWORD LineNumber;
		PTSTR FileName;
		DWORD64 Address;
	}
	alias IMAGEHLP_LINE64* PIMAGEHLP_LINE64;
 

	BOOL Module32First(HANDLE, MODULEENTRY32*);
	BOOL Module32Next(HANDLE, MODULEENTRY32*);


	alias BOOL function(
		HANDLE hProcess,
		DWORD64 dwAddr,
		PDWORD pdwDisplacement,
		PIMAGEHLP_LINE64 Line
	) fp_SymGetLineFromAddr64;


	
	alias BOOL function(
		PSYMBOL_INFO pSymInfo,
		ULONG SymbolSize,
		PVOID UserContext
	) PSYM_ENUMERATESYMBOLS_CALLBACK;

	alias BOOL function(
		HANDLE hProcess,
		ULONG64 BaseOfDll,
		LPCTSTR Mask,
		PSYM_ENUMERATESYMBOLS_CALLBACK EnumSymbolsCallback,
		PVOID UserContext
	) fp_SymEnumSymbols;


	alias DWORD64 function(
		HANDLE hProcess,
		DWORD64 dwAddr
	) fp_SymGetModuleBase64;
	alias fp_SymGetModuleBase64 PGET_MODULE_BASE_ROUTINE64;
	
	
	alias DWORD function(
	  HANDLE hProcess,
	  HMODULE hModule,
	  LPSTR lpFilename,
	  DWORD nSize
	) fp_GetModuleFileNameExA;
	

	enum ADDRESS_MODE {
		AddrMode1616,
		AddrMode1632,
		AddrModeReal,
		AddrModeFlat
	}
	
	struct KDHELP64 {
		DWORD64 Thread;
		DWORD ThCallbackStack;
		DWORD ThCallbackBStore;
		DWORD NextCallback;
		DWORD FramePointer;
		DWORD64 KiCallUserMode;
		DWORD64 KeUserCallbackDispatcher;
		DWORD64 SystemRangeStart;
		DWORD64 KiUserExceptionDispatcher;
		DWORD64 StackBase;
		DWORD64 StackLimit;
		DWORD64 Reserved[5];
	} 
	alias KDHELP64* PKDHELP64;
	
	struct ADDRESS64 {
		DWORD64 Offset;
		WORD Segment;
		ADDRESS_MODE Mode;
	}
	alias ADDRESS64* LPADDRESS64;


	struct STACKFRAME64 {
		ADDRESS64 AddrPC;
		ADDRESS64 AddrReturn;
		ADDRESS64 AddrFrame;
		ADDRESS64 AddrStack;
		ADDRESS64 AddrBStore;
		PVOID FuncTableEntry;
		DWORD64 Params[4];
		BOOL Far;
		BOOL Virtual;
		DWORD64 Reserved[3];
		KDHELP64 KdHelp;
	}
	alias STACKFRAME64* LPSTACKFRAME64;
	
	
	
	alias BOOL function(
		HANDLE hProcess,
		DWORD64 lpBaseAddress,
		PVOID lpBuffer,
		DWORD nSize,
		LPDWORD lpNumberOfBytesRead
	) PREAD_PROCESS_MEMORY_ROUTINE64;
	
	alias PVOID function(
		HANDLE hProcess,
		DWORD64 AddrBase
	) PFUNCTION_TABLE_ACCESS_ROUTINE64;
	alias PFUNCTION_TABLE_ACCESS_ROUTINE64 fp_SymFunctionTableAccess64;
	
	alias DWORD64 function(
		HANDLE hProcess,
		HANDLE hThread,
		LPADDRESS64 lpaddr
	) PTRANSLATE_ADDRESS_ROUTINE64;
	
	
	alias BOOL function (
		DWORD MachineType,
		HANDLE hProcess,
		HANDLE hThread,
		LPSTACKFRAME64 StackFrame,
		PVOID ContextRecord,
		PREAD_PROCESS_MEMORY_ROUTINE64 ReadMemoryRoutine,
		PFUNCTION_TABLE_ACCESS_ROUTINE64 FunctionTableAccessRoutine,
		PGET_MODULE_BASE_ROUTINE64 GetModuleBaseRoutine,
		PTRANSLATE_ADDRESS_ROUTINE64 TranslateAddress
	) fp_StackWalk64;	
}
