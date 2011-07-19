module tools.stackthreads;
import tools.stackthreads_impl, tools.log, tools.threads: TLS;

static if (enableStackthreads) {
  
  class Coroutine {
    private context ctx;
    void delegate(proc) dg;
    proc dg2;
    Exception failure;
    bool runsMe() { return ctx.runsMe; }
    void safe_exec() {
      yield();
      try if (dg) dg(&yield); else dg2();
      catch (Exception ex) {
        failure = ex;
        toMain();
        asm { int 3; }
      }
      yield();
      failure = new Exception("Coroutine has ended. Cannot invoke again.");
      toMain();
    }
    Coroutine other;
    context** ct_p; context* main_p;
    static TLS!(Coroutine) active, caller;
    static this() { New(active, { return cast(Coroutine) null; }); New(caller, { return cast(Coroutine) null; }); }
    void changed_thread() {
      ct_p = getActiveContextPtr();
      main_p = mainline.ptr();
    }
    context* getContextPtr() {
      if (*ct_p) return *ct_p;
      else return main_p;
    }
    this(proc dg, size_t stacksize=1024*1024, Coroutine other = null) {
      this.dg2 = dg;
      this.other = other;
      this(stacksize);
    }
    this(void delegate(proc) dg, size_t stacksize=1024*1024, Coroutine other = null) {
      this.dg = dg;
      this.other = other;
      this(stacksize);
    }
    private this(size_t stacksize=1024*1024) {
      changed_thread();
      ctx.genstack(stacksize);
      auto backup = caller(); caller.set(active());
      scope(exit) { active.set(caller()); caller.set(backup); }
      active.set(this);
      ctx.run(&safe_exec, getContextPtr);
    }
    void yield() {
      if (other) other();
      else if (auto ctx = caller()) ctx.enter();
      else toMain();
    }
    void failcheck(string info) {
      if (failure) throw new Exception(info~failure.toString());
    }
    // call with origin redirection
    void opCall(Coroutine other) {
      this.other = other; opCall();
    }
    private void enter() { ctx.activate(getContextPtr); }
    void opCall() {
      failcheck("Trying to invoke coroutine failed with ");
      auto backup = caller(); caller.set(active());
      scope(exit) { active.set(caller()); caller.set(backup); }
      active.set(this);
      enter();
      failcheck("Coroutine failed with ");
    }
    void toMain() { yield_main(getContextPtr); }
    void yieldToMain() { yield_main(&ctx); }
  }
  
  class Source(T) {
    Coroutine routine;
    T res; void delegate(void delegate(T)) dg;
    bool finished;
    void run(proc yield) { dg((T t) { res = t; yield(); }); finished = true; yield(); }
    int opApply(int delegate(ref T) dg) {
      while (true) {
        auto v = opCall();
        if (finished) return 0;
        if (auto res = dg(v)) return res;
      }
    }
    T opCall() { routine(); return res; }
    mixin This!("dg; #New(routine, &run); ");
  }
  
  class Sink(T) {
    Coroutine routine;
    T v; void delegate(T delegate()) dg;
    void run(proc yield) { dg({ yield(); return v; }); }
    void opCall(T t) { v = t; routine(); }
    mixin This!("dg; #New(routine, &run); #routine(); ");
  }
  
  T delegate() source(T)(void delegate(void delegate(T)) dg) {
    auto src = new Source!(T)(dg);
    return &src.opCall;
  }
  
  void delegate(T) sink(T)(void delegate(T delegate()) dg) {
    auto snk = new Sink!(T)(dg);
    return &snk.opCall;
  }
  
  T delegate() iterate(T)(T[] array) {
    return source(array /apply/ (T[] array, void delegate(T) yield) { foreach (entry; array) yield(entry); });
  }
  
  import tools.tests, tools.functional;
  unittest {
    logln("StackThread test");
    auto test = new Coroutine((proc yield) {
      logln("This is virtual stack context!");
      yield();
      logln("Hi again. Finishing now.");
    });
    logln("> This is main."); test();
    logln("> Reinvoking."); test();
    logln("> done");
    mustFail("ReinvokeTest", test());
    auto dg = ([2, 3, 4, 5]).iterate();
    mustEqual("IterTest #1", dg(), 2); mustEqual("IterTest #2", dg(), 3);
    mustEqual("IterTest #3", dg(), 4); mustEqual("IterTest #4", dg(), 5);
    int e;
    auto dg2 = sink((int delegate() get) { while (true) e += get(); });
    dg2(2); dg2(3); dg2(4);
    mustEqual("SinkTest", e, 9);
    auto src = new Source!(int)((void delegate(int) yield) { for (int i = 0; i < 5; ++i) yield(i); });
    int[] foo; foreach (v; src) foo ~= v;
    mustEqual("IterTest", foo, [0, 1, 2, 3, 4]);
    string match;
    sink((int delegate() dg) {
      match ~= "1"; sink((int delegate() dg) { match ~= "2"; dg(); match ~= "3"; })(dg()); match ~= "4";
    })(5);
    mustEqual("NestTest", match, "1234");
  }
}
