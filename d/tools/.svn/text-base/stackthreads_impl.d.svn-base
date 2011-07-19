module tools.stackthreads_impl;
import tools.base, tools.compat;

static if (size_t.sizeof==8) const string regsize="r", suffix="q";
else const string regsize="e", suffix="l";

const string pushall = ctReplace(
  "push! #ax\n push! #cx\n push! #dx\n push! #bx\n push! #bp\n push! #si\n push! #di\n ",
  "#", "%%"~regsize, "!", suffix
);
const string popall = ctReplace(
  "pop! #di\n pop! #si\n pop! #bp\n pop! #bx\n pop! #dx\n pop! #cx\n pop! #ax\n ",
  "#", "%%"~regsize, "!", suffix
);

version (BigEndian) const bool enableStackthreads = false;
else version (LLVM) const bool enableStackthreads = false;
else {
  const bool enableStackthreads = true;
  // const bool sjlj = true; // whee
  const bool sjlj = false;
}
extern(C) {
  void longjmp(void*, int retval);
  int setjmp(void*);
}
version(DS) {
  alias uint[23] jmp_buf; // Magic number, woooh
  struct djmpinfo {
    jmp_buf buf;
    static void switch_gc(ref djmpinfo from, ref djmpinfo to) {
      // no-op, since there is no GC on the DS.
    }
    // switch to _to_, set up return to _from_.
    static void swap(ref djmpinfo from, ref djmpinfo to) {
      if (setjmp(&from.buf) == 1) return;
      longjmp(&to.buf, 1);
    }
    void set(void* vstack, ref djmpinfo invoker, void delegate() dg) {
      if (setjmp(&invoker.buf)) return;
      auto dgPtr = &dg; void delegate()* dgPtr2;
      asm { "mov %1, sp\nmov %2, %0" : "=p" dgPtr2 : "p" vstack, "p" dgPtr: ; }
      (*dgPtr2)();
    }
  }
  struct context {
    djmpinfo info; // jump info for stackthread.
    void[] vstack; /// the virtual stack
    void genstack(size_t size) { vstack = cmalloc(size)[0 .. size]; }
    void cleanup() { cfree(vstack.ptr); }
    private {
      context* caller;
      void delegate(proc) dg;
      void yield() { caller.activate(this); }
      void mainproc() { dg(&yield); }
      void activate(context* invoker) {
        typeof(info).swap(invoker.info, info);
      }
    }
    void run(context* caller, void delegate(proc yield) dg) {
      this.dg = dg;
      this.caller = caller;
      info.set(vstack.ptr + vstack.length - 16, caller.info, &mainproc);
    }
    void call() { activate(caller); }
  }
} else static if (enableStackthreads) {
  const bool hasPatch = is(typeof(addRange(null, null, null)));
  static if (!hasPatch) pragma(msg, "Patch not found. Context switches will be an order of magnitude slower. Poke W. to get this fixed.");
  // this is a template so it's not compiled into static library code
  // thus allowing the debug segments to depend on the application's debug mode, not the library's
  struct djmpinfo() {
    void* esp, start; // start is for the GC
    static if (sjlj) int[23] jmp_buf;
    static if (hasPatch) size_t range_index = -1;
    static void switch_gc(ref djmpinfo from, ref djmpinfo to) {
      auto thr = Thread.getThis();
      disable; // don't enable it again until you've switched over
      from.start = thr.stackBottom;
      thr.stackBottom = to.start;
      auto esp = getESP();
      static if (hasPatch) {
        if (esp < from.start) addRange(esp, from.start, &from.range_index); // this range is now static and not being updated on collection
        else addRange(from.start, esp, &from.range_index);
        // remove to's range from the scanlist because it's going Active
        if (to.range_index != -1) removeRange(to.range_index);
      } else {
        if (esp < from.start) {
          addRange(esp, from.start);
          removeRange(to.esp);
        } else {
          addRange(from.start, esp);
          removeRange(to.start);
        }
      }
      // I hope I didn't forget anything.
    }
    static void swap(ref djmpinfo from, ref djmpinfo to) {
      switch_gc(from, to);
      auto ep=&from.esp;
      // now save to local, resume from other.
      void* tp=to.esp;
      static if (sjlj) {
        if (setjmp(&from.jmp_buf) != 1)
          longjmp(&to.jmp_buf, 1);
      } else static if (gccasm) {
        mixin(ctReplace("asm {\"
          call 0f
          jmp 1f
          0:
          
          "~pushall~"
          
          mov #sp, (#cx)
          mov #dx, #sp
          
          "~popall~"
          
          ret
          1:\" : : \"c\" ep, \"d\" to.esp : \"ax\", \"bx\";
        }", "#", "%%"~regsize));
      } else {
        asm {
          mov ECX, ep; mov EDX, tp;
          call fuckery; jmp end; fuckery:
          
          pushad;
          
          mov [ECX], ESP;
          mov ESP, EDX;
          
          popad;
          
          ret;
          end:;
        }
      }
      enable;
    }
    void set(void* vstack, ref djmpinfo invoker, void delegate() dg) {
      void *ep=&invoker.esp;
      start = esp = vstack;
      void* temp_esp = esp;
      switch_gc(invoker, *this);
      bool loaded = void;
      void* dg_ptr = &dg;
      typeof(dg_ptr) new_ptr;
      static if (sjlj) {
        scope(exit) enable;
        if (setjmp(&invoker.jmp_buf)) return;
        auto dgPtr = &dg; void delegate()* dgPtr2;
        mixin(ctReplace(`asm { "mov #cx, #sp" : "=d" dgPtr2 : "c" vstack, "d" dgPtr: ; }`, "#", "%%"~regsize));
        (*dgPtr2)();
        asm { int 3; }
      } else static if (gccasm) {
        mixin(ctReplace("asm {
            \"call 0f
            push $0
            jmp 1f
            
            0: "~pushall~"
            mov #sp, (#bx)
            mov #cx, #sp
            
            push $1
            1: pop #cx\" : \"=c\" loaded, \"=d\" new_ptr : \"b\" ep, \"c\" esp, \"d\" dg_ptr;
          }", "#", "%%"~regsize
        ));
      } else asm {
        mov EBX, ep; mov ECX, temp_esp; mov EDX, dg_ptr;
        call set_fuckery;
        mov loaded, 0; jmp set_end;
        set_fuckery:
        
        pushad;
        mov [EBX], ESP;
        mov ESP, ECX;
        
        mov new_ptr, EDX;
        mov loaded, 1;
        set_end:;
      }
      enable;
      if (!loaded) return;
      (*cast(typeof(dg)*)new_ptr)();
      asm { int 3; }
    }
  }
  
  import std.gc, std.mmfile, std.thread, tools.threads;
  
  /// NOT "mov res, EBP". Trust me on this one.
  void* getESP() { void* res = void; asm { mov res, ESP; } return res; }
  
  enum ctxState { dormant, running, sleeping, aborted, completed }
  
  TLS!(context) mainline;
  TLS!(context*) active;
  static this() {
    New(mainline, { return new context; });
    New(active, { auto res = new Stuple!(context*); return &res._0; });
  }
  void yield_main(context* invoker) { mainline.ptr().activate(invoker); }
  context* getActiveContext() {
    auto res = active.ptr();
    if (!*res) return mainline.ptr();
    else return *res;
  }
  context** getActiveContextPtr() {
    return active.ptr();
  }
  
  struct context {
    djmpinfo!() info; // jump info for stackthread.
    ctxState state = ctxState.dormant; /// my current state (running, sleeping, dead)
    void[] vstack; /// the virtual stack
    MmFile vstack_file=null; /// the underlying MmFile for the stack.
    void genstack(size_t size) {
      assert(!vstack_file);
      vstack=(vstack_file=new MmFile(null, MmFile.Mode.ReadWriteNew, size, null))[];
      // hasPointers(vstack.ptr);
    }
    void cleanup() { vstack_file=null; }
    // size_t size() { return vstack.length/uint.sizeof; }
    bool runsMe() {
      auto esp = getESP();
      return (esp>=vstack.ptr) && (esp < vstack.ptr+vstack.length);
    }
    void run(void delegate() dg, context* invoker) {
      scope(failure) logln(" >>run fails");
      state = ctxState.running;
      if (!invoker) invoker = mainline.ptr();
      invoker.state = ctxState.sleeping;
      *active.ptr() = this;
      /// leave a bit of space on the other side
      info.set(vstack.ptr + vstack.length - 256, invoker.info, dg);
    }
    void activate(context* invoker) {
      *active.ptr() = this;
      if (state != ctxState.sleeping) {
        logln("Invalid state ", state, " in @", cast(void*) this, "!");
        asm { int 3; }
      }
      state = ctxState.running;
      if (!invoker) invoker = mainline.ptr();
      invoker.state = ctxState.sleeping;
      typeof(info).swap(invoker.info, info);
    }
  }
}
