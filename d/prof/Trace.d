module xf.prof.Trace; 
import xf.prof.WinApi; 
import xf.prof.DbgInfo; 
import xf.prof.Demangler; 
import tango.text.convert.Format; 
import xf.utils.Memory; 
version=StacktraceUseWinApiStackWalking; 



version = StacktraceTryMatchCallAddresses;
version = StacktraceTryToBeSmart;
//version = UseCustomFiberForDemangling;
version = DemangleFunctionNames;


private extern(C) extern {
	void		_Dmain();
	void		D5tango4core6Thread5Fiber3runMFZv();
	size_t	fiberRunFuncLength;
}


void walkStack(LPCONTEXT ContextRecord, HANDLE hProcess, HANDLE hThread, void delegate(size_t[]) traceReceiver) {
	const int maxStackSpace	= 32;
	const int maxHeapSpace		= 256;
	static assert (maxHeapSpace  > maxStackSpace);
	
	size_t[maxStackSpace]	stackTraceArr = void;
	size_t[]							heapTraceArr;
	size_t[]							stacktrace = stackTraceArr;
	uint								i = void;
	
	void addAddress(size_t addr) {
		if (i < maxStackSpace) {
			stacktrace[i++] = addr;
		} else {
			if (maxStackSpace == i) {
				if (heapTraceArr is null) {
					heapTraceArr.alloc(maxHeapSpace, false);
					heapTraceArr[0..maxStackSpace] = stackTraceArr;
					stacktrace = heapTraceArr;
				}
				stacktrace[i++] = addr;
			} else if (i < maxHeapSpace) {
				stacktrace[i++] = addr;
			}
		}
	}


	version (StacktraceUseWinApiStackWalking) {
		STACKFRAME64 frame;
		memset(&frame, 0, frame.sizeof);

		frame.AddrStack.Offset	= ContextRecord.Esp;
		frame.AddrPC.Offset		= ContextRecord.Eip;
		frame.AddrFrame.Offset	= ContextRecord.Ebp;
		frame.AddrStack.Mode	= frame.AddrPC.Mode = frame.AddrFrame.Mode = ADDRESS_MODE.AddrModeFlat;

		//for (int sanity = 0; sanity < 256; ++sanity) {
		for (i = 0; i < maxHeapSpace; ) {
			auto swres = StackWalk64(
				IMAGE_FILE_MACHINE_I386,
				hProcess,
				hThread,
				&frame,
				ContextRecord,
				null,
				SymFunctionTableAccess64,
				SymGetModuleBase64,
				null
			);
			
			if (!swres) {
				break;
			}
			
			version (StacktraceSpam) printf("pc:%x ret:%x frm:%x stk:%x parm:%x %x %x %x"\n,
					frame.AddrPC.Offset, frame.AddrReturn.Offset, frame.AddrFrame.Offset, frame.AddrStack.Offset,
					frame.Params[0], frame.Params[1], frame.Params[2], frame.Params[3]);

			addAddress(frame.AddrPC.Offset);
		}
	} else {
		struct Layout {
			Layout*	ebp;
			size_t	ret;
		}
		Layout* p = cast(Layout*)ContextRecord.Esp;
		
		
		bool foundMain = false;		
		enum Phase {
			TryEsp,
			TryEbp,
			GiveUp
		}
		
		Phase phase = ContextRecord.Esp == ContextRecord.Ebp ? Phase.TryEbp : Phase.TryEsp;
		stacktrace[0] = ContextRecord.Eip;
		
		version (StacktraceTryToBeSmart) {
			Thread tobj = Thread.getThis();
		}
		
		while (!foundMain && phase < Phase.GiveUp) {
			version (StacktraceSpam) printf("starting a new tracing phase"\n);
			
			version (StacktraceTryToBeSmart) {
				auto curStack = tobj.topContext();
			}
			
			for (i = 1; p && !IsBadReadPtr(p, Layout.sizeof) && i < maxHeapSpace && !IsBadReadPtr(cast(void*)p.ret, 4);) {
				auto sym = p.ret;
				
				enum {
					NearPtrCallOpcode = 0xe8,
					RegisterBasedCallOpcode = 0xff
				}

				uint callAddr = p.ret;
				if (size_t.sizeof == 4 && !IsBadReadPtr(cast(void*)(p.ret - 5), 8) && NearPtrCallOpcode == *cast(ubyte*)(p.ret - 5)) {
					callAddr += *cast(uint*)(p.ret - 4);
					version (StacktraceSpam) printf("ret:%x frm:%x call:%x"\n, sym, p, callAddr);
					version (StacktraceTryMatchCallAddresses) {
						addAddress(p.ret - 5);	// a near call is 5 bytes
					}
				} else {
					version (StacktraceTryMatchCallAddresses) {
						if (!IsBadReadPtr(cast(void*)p.ret - 2, 4) && RegisterBasedCallOpcode == *cast(ubyte*)(p.ret - 2)) {
							version (StacktraceSpam) printf("ret:%x frm:%x register-based call:[%x]"\n, sym, p, *cast(ubyte*)(p.ret - 1));
							addAddress(p.ret - 2);	// an offset-less register-based call is 2 bytes for the call + register setup
						} else if (!IsBadReadPtr(cast(void*)p.ret - 3, 4) && RegisterBasedCallOpcode == *cast(ubyte*)(p.ret - 3)) {
							version (StacktraceSpam) printf("ret:%x frm:%x register-based call:[%x,%x]"\n, sym, p, *cast(ubyte*)(p.ret - 2), *cast(ubyte*)(p.ret - 1));
							addAddress(p.ret - 3);	// a register-based call is 3 bytes for the call + register setup
						} else {
							version (StacktraceSpam) printf("ret:%x frm:%x"\n, sym, p);
							addAddress(p.ret);
						}
					}
				}

				version (StacktraceTryToBeSmart) {
					bool inFiber = false;
					if	(
							callAddr == cast(uint)&_Dmain
							|| true == (inFiber = (
								callAddr >= cast(uint)&D5tango4core6Thread5Fiber3runMFZv
								&& callAddr < cast(uint)&D5tango4core6Thread5Fiber3runMFZv + fiberRunFuncLength
							))
						)
					{
						foundMain = true;
						if (inFiber) {
							version (StacktraceSpam) printf("Got or Thread.Fiber.run"\n);

							version (StacktraceTryMatchCallAddresses) {
								// handled above
							} else {
								addAddress(p.ret);
							}

							curStack = curStack.within;
							if (curStack) {
								void* newp = curStack.tstack;
								if (!IsBadReadPtr(newp + 28, 8)) {
									addAddress(*cast(size_t*)(newp + 32));
									p = *cast(Layout**)(newp + 28);
									continue;
								}
							}
						} else {
							version (StacktraceSpam) printf("Got _Dmain"\n);
						}
					}
				}
				
				version (StacktraceTryMatchCallAddresses) {
					// handled above
				} else {
					addAddress(p.ret);
				}
				
				p = p.ebp;
			}

			++phase;
			p = cast(Layout*)ContextRecord.Ebp;
			version (StacktraceSpam) printf("end of phase"\n);
		}
		
		version (StacktraceSpam) printf("calling traceReceiver"\n);
	}

	traceReceiver(stacktrace[0..i]);
	heapTraceArr.free();
}


struct SymbolDetails {
	char[] func;
	char[] file;
	int line;
	ptrdiff_t addrOffset;
}

SymbolDetails[size_t] symbolCache;


extern(C) extern char[][char[]] sym2Mod;


void addrToSymbolDetails(size_t addr, HANDLE hProcess, void delegate(char[] func, char[] file, int line, ptrdiff_t addrOffset) dg) {
	if (auto sd = addr in symbolCache) {
		dg(sd.tupleof);
	} else {
		ubyte buffer[256];

		SYMBOL_INFO* symbol_info = cast(SYMBOL_INFO*)buffer.ptr;
		symbol_info.SizeOfStruct = SYMBOL_INFO.sizeof;
		symbol_info.MaxNameLen = buffer.length - SYMBOL_INFO.sizeof + 1;
		
		ptrdiff_t addrOffset = 0;
		auto ln = getAddrDbgInfo(addr, &addrOffset);

		char* symname = null;
		char* filename = ln.file;
		char[MAX_PATH] tmp;
		char[MAX_PATH] tmp2;
		
		char[] filename2;
		char[] symname2;
		
		if (!SymFromAddr(hProcess, addr, null, symbol_info)) {
			//printf("%.*s"\n, SysError.lastMsg);
			symname = ln.func;
		} else {
			symname = symbol_info.Name.ptr;
			assert (symname !is null);
			
			if (filename is null) {
				if (auto mod = fromStringz(symname) in sym2Mod) {
					filename2 = *mod;
				} else {
					printf("sym name not found: %s"\n, symname);
				}
			}
		}
		
		if (symname !is null) {
			symname2 = fromStringz(symname).dup;
		}/+ else {
			symname2 = Format("{:x}", addr >> 16);
		}+/
		
		if (filename2 is null) {
			filename2 = fromStringz(filename).dup;
		}

		auto sd = SymbolDetails(symname2, filename2, ln.line, addrOffset);
		symbolCache[addr] = sd;
		dg(sd.tupleof);
	}
}


class TangoTrace3Info : Exception.TraceInfo {
	int opApply(int delegate(ref char[] func, ref char[] file, ref int line, ref ptrdiff_t offset, ref size_t address) dg) {
		foreach (i, it; items) {
			char[] func, file;
			int line;
			ptrdiff_t addrOffset;
			
			int ret;
			addrToSymbolDetails(it, hProcess, (char[] func, char[] file, int line, ptrdiff_t addrOffset) {
				func = demangledFunc(func);
				ret = dg(func, file, line, addrOffset, it);
			});
			
			if (ret) {
				return ret;
			}
		}
		
		return 0;
	}
	

	static char[] demangledFunc(char[] func) {
		version (DemangleFunctionNames) {
			if (func is null) {
				return "???";
			}
			if ("__Dmain" == func) {
				return "main";
			}

			static char[][char[]] cache;
			if (auto c = func in cache) {
				return *c;
			}
			
			auto origFunc = func;

			try {
				version (UseCustomFiberForDemangling) {
					scope f = new Fiber({
						scope demangler = new Demangler;
						func = demangler.demangle(func);
					}, 8192*16);
					f.call();
				} else {
					scope demangler = new Demangler;
					func = demangler.demangle(func);
				}
			} catch {}
			
			return cache[origFunc] = func;
		} else {
			return func;
		}
	}

	
	int opApply(int delegate(ref char[]) dg) {
		foreach (i, it; items) {
			char[] func, file;
			int line;
			ptrdiff_t addrOffset;
			
			int ret = 0;
			bool breakLoop = false;
			addrToSymbolDetails(it, hProcess, (char[] func, char[] file, int line, ptrdiff_t addrOffset) {
				if (
						func != "_D11TangoTrace318tangoTrace3HandlerFPvZC9Exception9TraceInfo" &&
						func != "_D6object12traceContextFPvZC9Exception9TraceInfo" &&
						func != "_D6object9Exception5_ctorMFAaC9ExceptionZC9Exception"
				) {
					char[] str;
					if (0 == line) {
						str = Format("    at {}({}) {}{} [{:x}]", demangledFunc(func), file is null ? "???" : file, addrOffset >= 0 ? "+" : "", addrOffset, it);
					} else {
						str = Format("    at {}({}:{}) {}{} [{:x}]", demangledFunc(func), file is null ? "???" : file, line, addrOffset >= 0 ? "+" : "", addrOffset, it);
					}
					ret = dg(str);
				}
				
				if ("__Dmain" == func) {
					breakLoop = true;
				}
			});
			
			if (ret) {
				return ret;
			}
			
			if (breakLoop) {
				break;
			}
		}
		return 0;
	}
	
	
	char[] toString() {
		char[] res;
		foreach (char[] c; this) {
			res ~= c;
			res ~= \n;
		}
		return res;
	}
	
	
	~this() {
		if (items !is null) {
			tango.stdc.stdlib.free(items.ptr);
			items = null;
		}
	}
	
	
	this(HANDLE hProc) {
		this.hProcess = hProc;
	}
	
	
	size_t[]		items;
	HANDLE	hProcess;
}


Exception.TraceInfo tangoTrace3Handler(void* ptr) {
	return tangoTrace3Handler(ptr, GetCurrentProcess(), GetCurrentThread());
}


Exception.TraceInfo tangoTrace3Handler(void* ptr, HANDLE hProcess, HANDLE hThread) {
	CONTEXT		context;
	CONTEXT*	ctxPtr = &context;
	
	if (ptr is null) {
		uint eipReg, espReg, ebpReg;
		asm {
			call GIMMEH_EIP;
			GIMMEH_EIP:
				pop EAX;
				mov eipReg, EAX;
			mov espReg, ESP;
			mov ebpReg, EBP;
		}

		context.ContextFlags = CONTEXT_i386 | CONTEXT_CONTROL;
		GetThreadContext(hThread, &context);
		context.Eip = eipReg;
		context.Esp = espReg;
		context.Ebp = ebpReg;
	} else {
		ctxPtr = cast(CONTEXT*)ptr;
	}
	
	version (StacktraceSpam) printf("Eip: %x, Esp: %x, Ebp: %x"\n, ctxPtr.Eip, ctxPtr.Esp, ctxPtr.Ebp);
	
	version (StacktraceUseWinApiStackWalking) {
		// IsBadReadPtr will always return true here
	} else {
		if (IsBadReadPtr(cast(void*)ctxPtr.Ebp, 4)) {
			ctxPtr.Ebp = ctxPtr.Esp;
		}
	}

	size_t[] items;
	walkStack(ctxPtr, hProcess, hThread, (size_t[] trace) {
		size_t totalSize = trace.length * size_t.sizeof;
		size_t* ptr = cast(size_t*)tango.stdc.stdlib.malloc(totalSize);
		if (ptr) {
			tango.stdc.string.memcpy(ptr, trace.ptr, totalSize);
			items = ptr[0..trace.length];
		}
	});
	
	if (items) {
		auto info = new TangoTrace3Info(hProcess);
		info.items = items;
		return info;
	} else {
		return null;
	}
}
