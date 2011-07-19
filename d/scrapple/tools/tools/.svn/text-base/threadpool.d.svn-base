module tools.threadpool;
public import tools.base, tools.compat;
// mixin(expandImport!("std.thread, tools.[threads, log, functional]"));
public import tools.threads;
import tools.log, tools.functional, tools.compat;

Lock lock; // perpetually blocking
static this() { New(lock); lock.lock; }
void halt() { lock.lock; }

struct task {
  string name;
  proc dg;
  bool crit;
  void opCall() { dg(); }
  static task opCall(string n, proc d, bool crit = false) { task res=void; res.name=n; res.dg=d; res.crit = crit; return res; }
}

class TaskUnfinished : Exception { this() { super("Task unfinished!"); } }

const bool CRITICAL = true;

class Threadpool {
  poolthread[] threads;
  bool finish, grow;
  MessageMultiChannel!(task, false, false) tasks;
  int getThreadId() { foreach (id, thread; threads) if (isSelf(thread)) return id; throw new Exception("Not a thread of this pool!"); }
  task getTask() { if (finish) return task("ShutdownThreadsBusy", &shutdown_me, CRITICAL); else return tasks.get(); }
  void addTask(string name, proc c, bool crit = false) {
    if (grow && (busy == threads.length)) newThread;
    tasks.put(task(name, c, crit));
  }
  Semaphore shutdown_counter;
  void shutdown_me() { shutdown_counter.release; halt; }
  void shutdown() {
    shutdown_counter = new Semaphore;
    finish = true;
    auto me = Thread.getThis();
    foreach (thread; threads) if (thread !is me) addTask("ShutdownThreadsWakeUp", &shutdown_me);
    foreach (thread; threads) if (thread !is me) shutdown_counter.acquire;
    return;
  }
  void slow_shutdown(void delegate() idle) {
    shutdown_counter = new Semaphore;
    finish = true;
    int left;
    auto me = Thread.getThis();
    foreach (thread; threads) if (thread !is me) {
      addTask("ShutdownThreadsWakeUp", &shutdown_me);
      left++;
    }
    while (left) if (!shutdown_counter.try_acquire) idle(); else left--;
    return;
  }
  class Future(T) {
    bool done; MessageChannel!(bool) channel;
    this() { New(channel); }
    static if (!is(T==void)) T res;
    T eval() {
      if (!done) synchronized(this) volatile if (!done) done = channel.get();
      static if (is(typeof(res))) return res;
    }
    alias eval opCall;
    bool finished() { if (done) return true; synchronized(this) volatile return done || channel.active; }
  }
  void mt_foreach(T, C)(T iterable, C callable) {
    int left; auto sync = new Object;
    void loop_body(void delegate() dg) {
      scope(exit) synchronized(sync) left --;
      dg();
    }
    static if (is(typeof({ foreach (i, entry; iterable) (stuple(i, entry) /apply/ callable)(); }))) {
      foreach (i, entry; iterable) {
        synchronized(sync) left ++;
        addTask((stuple(i, entry) /apply/ callable) /apply/ &loop_body);
      }
    } else static if (is(typeof({ foreach (entry; iterable) (entry /apply/ callable)(); }))) {
      foreach (entry; iterable) {
        synchronized(sync) left ++;
        addTask((entry /apply/ callable) /apply/ &loop_body);
      }
    } else static assert(false, "Cannot seem to iterate over "~T.stringof~", calling "~C.stringof~"!");
    // could cause deadlock
    while (left) /*if (idle()) */slowyield();
  }
  Future!(typeof(Init!(C)())) future(C)(C callable) {
    auto res=new Future!(typeof(callable()));
    addTask((typeof(res) fut, C call) {
      static if (is(typeof(fut.res))) fut.res = call();
      else call();
      fut.channel.put(true);
    } /fix/ stuple(res, callable));
    return res;
  }
  import tools.tests;
  unittest {
    int test(int i, int j=10) {
      // logln("Test of ", i, " and ", j);
      if (!j) return i;
      else return test(i*i, j-1);
    }
    auto tp = new Threadpool(2);
    logln("Beginning futures .. ");
    auto fut1 = tp.future({ return test(3, 3); }), fut2 = tp.future({ return test(4, 2); });
    mustEqual("FutureTest", fut1()+fut2(), test(3, 3)+test(4, 2), 6817);
  }
  void addTask(proc c) { addTask("Unnamed task", c); }
  void addTask(bool crit, proc c) { addTask("Unnamed task", c, crit); }
  void newThread() {
    gcdisable; scope(exit) gcenable;
    auto thr=new poolthread(tasks, &getTask);
    thr.mainthr=mainthr;
    // logln("Starting thread .. ");
    thr.start;
    threadnames[thr] = "Freshling PoolThread";
    // logln("Add thr");
    synchronized(this) threads~=thr;
    // logln("Meepness.");
  }
  Thread mainthr=null;
  int busy() {
    int res;
    foreach (thr; threads) if (thr.busy) res++;
    return res;
  }
  bool idle() {
    typeof(getTask()) task;
    if (!tasks.try_get(task)) return true;
    else {
      try task();
      catch (TaskUnfinished tu) addTask(task.name, task.dg);
      catch (Exception e) logln("Idle task failed with ", e);
      return false;
    }
  }
  static const int GROW=-1;
  this(int count) {
    New(tasks);
    mainthr=Thread.getThis;
    if (count == GROW) grow = true;
    else while (count--) newThread;
  }
}

final class poolthread : Thread {
  Thread mainthr=null;
  bool busy;
  MessageMultiChannel!(task, false, false) tasksource;
  task delegate() getTask; // to allow overrides on finish=true
  this(typeof(tasksource) ts, task delegate() dg) {
    gcdisable;
    super();
    gcenable;
    mainthr=Thread.getThis;
    getTask = dg;
    tasksource = ts;
    // static if (is(typeof(super(&tango_is_dumb_wrapper)))) super(&tango_is_dumb_wrapper);
  }
  ~this() { fail("POOLTHREAD DELETED. BAD."); }
  void tango_is_dumb_wrapper() { return run(); }
  int run() {
    while (true) {
      // logln("Thread: polling for task");
      auto t = getTask();
      threadnames[this] = t.name;
      try {
        busy = true;
        scope(exit) busy = false;
        t();
      }
      catch (Exception e) {
        logln("Task failed with ", e);
        if (t.crit) fail("Aborting. ");
      }
    }
    return 0;
  }
}
