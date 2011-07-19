module xf.prof.Profiler;

private {
	import xf.prof.WinApi;
	import xf.prof.DbgInfo : initDebugInfo, getAddrDbgInfo;
	import xf.prof.Trace;
	import xf.utils.Meta;
	import tango.core.Array;
}



public alias Exception.TraceInfo TraceInfo;


struct ProcessInfo {
	DWORD		id;
	char[]			name;
	char[]			filePath;
	ThreadInfo[]	threads;
	
	bool valid() {
		return id != 0;
	}
}


struct ThreadInfo {
	DWORD id;
}


enum ThreadPriority {
	Lowest = -2,
	Lower = -1,
	Normal = 0,
	Higher = 1,
	Highest = 2
}


struct SpotInfo {
	int	counts;
	int	treeCounts;
}


struct ThreadProfilingInfo {
	/+SpotInfo[size_t]	spots;
	int					totalSamples;
	int					totalTreeSamples;+/
	TraceInfo[] traceInfo;
}


class ThreadProfilingResults {
	ThreadProfilingInfo	info;
	Profiler					profiler;
	
	
	/+ByFuncTreeTimeProxy byFuncTreeTime(int numFuncs) {
		return ByFuncTreeTimeProxy(this, numFuncs);
	}

	ByTreeTimeProxy byTreeTime(int numFuncs) {
		return ByTreeTimeProxy(this, numFuncs);
	}

	ByTimeProxy byTime(int numFuncs) {
		return ByTimeProxy(this, numFuncs);
	}+/
}


struct ProfilingSpot {
	char[]	func;
	char[]	file;
	int		line;
	int		count;
	int		treeCount;
}


extern(C) char[][char[]] sym2Mod;

/+struct ByFuncTreeTimeProxy {
	ThreadProfilingResults	results;
	int								numFuncs;
	ProfilingSpot[]				spots;
	
	
	static ByFuncTreeTimeProxy opCall(ThreadProfilingResults r, int n) {
		ByFuncTreeTimeProxy res;
		res.results = r;
		res.numFuncs = n;
		res.calc;
		return res;
	}
	
	private void calc() {
		ProfilingSpot[char[]] spotByFunc;
		
		foreach (addr, _info; results.info.spots) {
			int cnt = _info.counts;
			int tcnt = _info.treeCounts;
			
			auto addrInfo = results.profiler.getAddrInfo(addr);
			auto name = addrInfo._0;
			auto file = addrInfo._1;
			auto line = addrInfo._2;
			
			ProfilingSpot* spot = null;
			if ((spot = name in spotByFunc) !is null) {
			} else {
				spotByFunc[name] = ProfilingSpot.init;
				spot = name in spotByFunc;
				spot.func = name;
				spot.file = file;
				spot.line = line;
			}
			
			spot.count += cnt;
			spot.treeCount += tcnt;
		}
		
		foreach (name, spot; spotByFunc) {
			spots ~= spot;
		}
		
		tango.core.Array.sort(spots, (ref ProfilingSpot s1, ref ProfilingSpot s2) {
			return s1.treeCount > s2.treeCount;
		});
		
		if (spots.length > numFuncs) {
			spots = spots[0..numFuncs];
		}
	}
}



struct ByTreeTimeProxy {
	ThreadProfilingResults	results;
	int								numFuncs;
	ProfilingSpot[]				spots;
	
	
	static ByTreeTimeProxy opCall(ThreadProfilingResults r, int n) {
		ByTreeTimeProxy res;
		res.results = r;
		res.numFuncs = n;
		res.calc;
		return res;
	}
	
	private void calc() {
		foreach (addr, _info; results.info.spots) {
			int cnt = _info.counts;
			int tcnt = _info.treeCounts;
			
			auto addrInfo = results.profiler.getAddrInfo(addr);
			auto name = addrInfo._0;
			auto file = addrInfo._1;
			auto line = addrInfo._2;
			
			spots ~= ProfilingSpot(name, file, line, cnt, tcnt);
		}
		
		tango.core.Array.sort(spots, (ref ProfilingSpot s1, ref ProfilingSpot s2) {
			return s1.treeCount > s2.treeCount;
		});
		
		if (spots.length > numFuncs) {
			spots = spots[0..numFuncs];
		}
	}
}



struct ByTimeProxy {
	ThreadProfilingResults	results;
	int								numFuncs;
	ProfilingSpot[]				spots;
	
	
	static ByTimeProxy opCall(ThreadProfilingResults r, int n) {
		ByTimeProxy res;
		res.results = r;
		res.numFuncs = n;
		res.calc;
		return res;
	}
	
	private void calc() {
		foreach (addr, _info; results.info.spots) {
			int cnt = _info.counts;
			int tcnt = _info.treeCounts;
			
			auto addrInfo = results.profiler.getAddrInfo(addr);
			auto name = addrInfo._0;
			auto file = addrInfo._1;
			auto line = addrInfo._2;
			
			spots ~= ProfilingSpot(name, file, line, cnt, tcnt);
		}
		
		tango.core.Array.sort(spots, (ref ProfilingSpot s1, ref ProfilingSpot s2) {
			return s1.count > s2.count;
		});
		
		if (spots.length > numFuncs) {
			spots = spots[0..numFuncs];
		}
	}
}+/


class Profiler {
	ProcessInfo[] processes() {
		auto snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS | TH32CS_SNAPTHREAD, 0);
		scope (exit) CloseHandle(snapshot);

		PROCESSENTRY32 processInfo;
		processInfo.dwSize = PROCESSENTRY32.sizeof;
		
		ProcessInfo[] processes;
		if (Process32First(snapshot, &processInfo)) {
			do {
				auto processName = processInfo.szExeFile;
				DWORD processId = processInfo.th32ProcessID;
				
				auto name = processName[0..MAX_PATH];
				name = name[0..name.locate('\0')];
				processes ~= ProcessInfo(processId, name.dup);

				processInfo.dwSize = PROCESSENTRY32.sizeof;
			} while (Process32Next(snapshot, &processInfo));
		}
		
		// threads

		THREADENTRY32 threadInfo;
		threadInfo.dwSize = THREADENTRY32.sizeof;

		if (Thread32First(snapshot, &threadInfo)) {
			do {
				DWORD ownerProcessId = threadInfo.th32OwnerProcessID;

				foreach (ref proc; processes) {
					if (proc.id == ownerProcessId)
					{
						proc.threads ~= ThreadInfo(threadInfo.th32ThreadID);
						break;
					}
				}

				threadInfo.dwSize = THREADENTRY32.sizeof;
			} while (Thread32Next(snapshot, &threadInfo));
		}
		
		return processes;
	}
	
	
	static extern (Windows) BOOL enumSymbolsCallback(
		PSYMBOL_INFO pSymInfo,
		ULONG SymbolSize,
		PVOID UserContext
	) {
		char[] mod = *cast(char[]*)UserContext;
		sym2Mod[fromStringz(pSymInfo.Name.ptr).dup] = mod;
		return TRUE;
	}
	
	
	static extern (Windows) BOOL enumModulesCallback(PCSTR name, DWORD64 base, ULONG size, PVOID user) {
		char			tmp[MAX_PATH];
		HANDLE	hProcess = cast(HANDLE)user;

		if (0 == GetModuleFileNameExA(hProcess, cast(HMODULE)base, tmp.ptr, tmp.length)) {
			lstrcpynA(tmp.ptr, name, tmp.length);
		}

		//printf("loading module: %s"\n, tmp.ptr);
		
		char[] modName = fromStringz(tmp.ptr).dup;

		DWORD loadedBase;
		if (0 == (loadedBase = SymLoadModule64(hProcess, HANDLE.init, tmp.ptr, name, base, size))) {
			if (GetLastError() != ERROR_SUCCESS) {
				printf("Could not load module: %s"\n, tmp.ptr);
			}
		} else {
			SymEnumSymbols(hProcess, loadedBase, null, &enumSymbolsCallback, &modName);
		}
		
		return TRUE;
	}
	

	void attach(ref ProcessInfo process) {
		detach();
		
		_processHandle = OpenProcess(PROCESS_ALL_ACCESS, FALSE, process.id);
		if (_processHandle is null) {
			throw new Exception("Could not OpenProcess: " ~ SysError.lastMsg);
		}
		
		SymSetOptions(
			SYMOPT_LOAD_LINES |
			SYMOPT_OMAP_FIND_NEAREST |
			SYMOPT_ALLOW_ABSOLUTE_SYMBOLS/+ | SYMOPT_UNDNAME+/);
		if (!SymInitialize(_processHandle, null, FALSE)) {
			throw new Exception("Could not SymInitialize: " ~ SysError.lastMsg);
		}
       
		MODULEENTRY32 moduleEntry;
		moduleEntry.dwSize = moduleEntry.sizeof;
		char buffer[4096];

		int len = GetModuleFileNameExA(_processHandle, null, buffer.ptr, 4096);
		//Stdout.formatln("Process image name: '{}'", fromStringz(buffer.ptr));
		
		process.filePath = fromStringz(buffer.ptr).dup;
		
		DWORD base;
		if (0 == (base = SymLoadModule64(_processHandle, HANDLE.init, buffer.ptr, null, 0, 0))) {
			if (GetLastError() != ERROR_SUCCESS) {
				throw new Exception("Could not SymLoadModule64: " ~ SysError.lastMsg);
			}
		}

		//if (loadAllModules) {
			EnumerateLoadedModules64(_processHandle, &enumModulesCallback, cast(void*)_processHandle);
		//}

		initDebugInfo(fromStringz(buffer.ptr));

		_attached = process;
	}
	
	
	void attachToAllThreads() {
		_threadProfilingInfo = new ThreadProfilingInfo[_attached.threads.length];
		
		foreach (thread; _attached.threads) {
			auto h = OpenThread(THREAD_ALL_ACCESS, FALSE, thread.id);
			if (h is null) {
				throw new Exception("Could not OpenThread: " ~ SysError.lastMsg);
			} else {
				_threadHandles ~= h;
			}
		}
	}
	
	
	void detach() {
		if (_processHandle) {
			CloseHandle(_processHandle);
			_processHandle = null;
		}
		
		foreach (th; _threadHandles) {
			CloseHandle(th);
		}
		
		_threadHandles = null;
	}
	
	
	void threadPriority(ThreadPriority priority) {
		_threadPriority = priority;
		
		if (_profilerThread !is null) {
			_setThreadPriority;
		}
	}
	
	
	/+void sampleWindowSize(int size) {
		_sampleWindowSize = size;
	}+/
	
	
	protected void _setThreadPriority() {
		if (_profilerThread !is null) {
			_profilerThread.priority = _threadPriority;
		}
	}
	
	
	void startSampling() {
		_profilerThread = new Thread(&profilerThreadFunc);
		_active = true;
		_profilerThread.start;
		_setThreadPriority;
	}


	void stopSampling() {
		_active = false;
		
		if (_profilerThread !is null) {
			_profilerThread.join;
			delete _profilerThread;
		}
	}
	
	
	/+// sym, file, line
	Stuple!(char[], char[], int) getAddrInfo(ulong addr) {
		ubyte buffer[1024];

		SYMBOL_INFO* symbol_info = cast(SYMBOL_INFO*)buffer.ptr;
		symbol_info.SizeOfStruct = SYMBOL_INFO.sizeof;
		symbol_info.MaxNameLen = buffer.length - SYMBOL_INFO.sizeof + 1;
		
		if (!SymFromAddr(_processHandle, addr, null, symbol_info)) {
			//Stdout.formatln(SysError.lastMsg);
			return stuple("[unknown]", "", 0);
		}

		auto fl = getAddrDbgInfo(addr);
		return stuple(fromStringz(symbol_info.Name.ptr).dup, fromStringz(fl.file), cast(int)fl.line);
	}+/

	
	int numThreads() {
		return _attached.threads.length;
	}
	
	
	ThreadProfilingResults results(int threadIdx) {
		auto res = new ThreadProfilingResults;
		res.profiler = this;
		res.info = _threadProfilingInfo[threadIdx];
		return res;
	}
	
	
	protected void profilerThreadFunc() {
		for (; _active; _profilerThread.sleep(sampleDelayMicros * 0.000001)) {
			foreach (ti, thread; _attached.threads) {
				sampleThread(ti, _threadHandles[ti]);
			}
		}
	}
	
	
	protected void sampleThread(int threadIdx, HANDLE threadHandle) {
		{
			auto result = SuspendThread(threadHandle);
			if (result == 0xffffffff) {
				throw new Exception("SuspendThread failed: " ~ SysError.lastMsg);
			}
		}

		CONTEXT threadContext;
		threadContext.ContextFlags = CONTEXT_i386 | CONTEXT_CONTROL;
		if (!GetThreadContext(threadHandle, &threadContext)) {
			throw new Exception("GetThreadContext failed: " ~ SysError.lastMsg);
		}

		auto traceInfo = tangoTrace3Handler(&threadContext, _processHandle, threadHandle);

		/+auto addr = threadContext.Eip;

		int stackLevels = 0;
		ulong walk[256];
		walk[stackLevels++] = threadContext.Eip;

		STACKFRAME64 frame;
		memset(&frame, 0, frame.sizeof);

		frame.AddrStack.Offset	= threadContext.Esp;
		frame.AddrPC.Offset		= threadContext.Eip;
		frame.AddrFrame.Offset	= threadContext.Ebp;
		frame.AddrStack.Mode	= frame.AddrPC.Mode = frame.AddrFrame.Mode = ADDRESS_MODE.AddrModeFlat;

		while (true) {
			auto swres = StackWalk64(
				IMAGE_FILE_MACHINE_I386,
				_processHandle,
				threadHandle,
				&frame,
				&threadContext,
				null,
				SymFunctionTableAccess64,
				SymGetModuleBase64,
				null
			);
			
			if (!swres || stackLevels >= walk.length) {
				break;
			}

			walk[stackLevels++] = frame.AddrReturn.Offset;
		}+/

		if (!ResumeThread(threadHandle)) {
			throw new Exception("ResumeThread failed: " ~ SysError.lastMsg);
		}
		
		auto thrInfo = &_threadProfilingInfo[threadIdx];
		thrInfo.traceInfo ~= traceInfo;

		/+SpotInfo* getSpotInfo(size_t addr) {
			if (auto r = addr in thrInfo.spots) {
				return r;
			} else {
				thrInfo.spots[addr] = SpotInfo.init;
				return addr in thrInfo.spots;
			}
		}
		
		++getSpotInfo(addr).counts;
		++thrInfo.totalSamples;
		
		foreach (a; walk[0..stackLevels]) {
			++getSpotInfo(a).treeCounts;
		}
		thrInfo.totalTreeSamples += stackLevels;+/
	}
	
	
	protected {
		ProcessInfo		_attached;
		HANDLE			_processHandle;
		HANDLE[]			_threadHandles;
		
		Thread				_profilerThread;
		ThreadPriority	_threadPriority = ThreadPriority.Normal;
		
		//int					_sampleWindowSize = 0;
		bool					_active;
		
		ThreadProfilingInfo[]	_threadProfilingInfo;
	}
	
	public {
		int				sampleDelayMicros = 100;
		int				totalSamples;
		int				totalTreeSamples;
	}
}



ProcessInfo findProcess(ProcessInfo[] infos, char[] name) {
	foreach (ref proc; infos) {
		if (proc.name == name) {
			return proc;
		}
	}
	return ProcessInfo.init;
}
