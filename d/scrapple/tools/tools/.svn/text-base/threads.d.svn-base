module tools.threads;

import tools.page_queue, tools.base, tools.compat;
import tools.behave_as;

Object ThreadSync, GenSync;
static this() { New(ThreadSync); New(GenSync); }

interface LockImpl {
  void sys_lock(); void sys_unlock();
}

class Lock {
  LockImpl syslock;
  this() { version(Windows) syslock = new Win32Lock; else syslock = new PosixLock; }
  import tools.log;
  invariant() { if (!syslock) { logln("Lock violation!"); *(cast(int*) null) = 0; } }
  Thread owner; int count;
  // removed final, due to GDC bug. TODO: restore
  void lock() {
    auto thr = Thread.getThis();
    if (owner !is thr) {
      syslock.sys_lock;
      if (owner) throw new Exception("We have the lock but there's a previous owner - wtf?");
      if (count) throw new Exception("Lock newly acquired but nesting count set - wtf?");
      owner = thr;
    }
    count ++;
  }
  void unlock() {
    auto thr = Thread.getThis();
    if (owner !is thr) throw new Exception("Trying to unlock a lock that you don't hold - wtf?");
    if (!count) throw new Exception("Trying to unlock a lock more times than you locked it - wtf?");
    count --;
    if (!count) { owner = null; syslock.sys_unlock; }
  }
  void Synchronized(void delegate()[] dgs...) { lock; scope(exit) unlock; foreach (dg; dgs) dg(); }
  void Unsynchronized(void delegate()[] dgs...) { unlock; scope(exit) lock; foreach (dg; dgs) dg(); }
}

final class CondVar {
  bool blocking;
  void wait(Lock unlock = null) { blocking=true; if (unlock) unlock.unlock; while(blocking) slowyield(); if (unlock) unlock.lock; }
  void signal() { blocking=false; }
}

version(Windows) {
  struct crits {
    void* DebugInfo;
    int LockCount, RecursionCount;
    void* OwningThread, LockSemaphore;
    uint SpinCount;
  }
  extern(Windows) {
    void InitializeCriticalSection(crits*);
    void EnterCriticalSection(crits*);
    void LeaveCriticalSection(crits*);
    void DeleteCriticalSection(crits*);
  }
  class Win32Lock : LockImpl {
    crits crit;
    this() { InitializeCriticalSection(&crit); }
    ~this() { DeleteCriticalSection(&crit); }
    override void sys_lock() { EnterCriticalSection(&crit); }
    override void sys_unlock() { LeaveCriticalSection(&crit); }
  }
  extern(Windows) {
    void* CreateSemaphoreA(void*, int, int, char* name = null); alias CreateSemaphoreA CreateSemaphore;
    bool CloseHandle (void*);
    const INFTE = uint.max; // not INFINITE: overlaps with math
    uint WaitForSingleObject(void*, uint);
    bool ReleaseSemaphore (void*, int, int*);
  }
  final class Semaphore {
    const WAIT_TIMEOUT = 0x0000_0102, WAIT_ABANDONED = 0x0000_0080;
    private void* handle;
    this(int count=0) { handle=CreateSemaphore (null, count, int.max); }
    ~this() { CloseHandle (handle); }
    bool try_acquire() {
      auto res = WaitForSingleObject (handle, 0);
      if (res == WAIT_TIMEOUT) return false;
      if (res == WAIT_ABANDONED) throw new Exception("State violation: thread abandoned semaphore!");
      return true;
    }
    void acquire() {
      auto res = WaitForSingleObject (handle, INFTE);
      if (res == WAIT_TIMEOUT) throw new Exception("Reached end of time. Aborting.");
      if (res == WAIT_ABANDONED) throw new Exception("State violation: thread abandoned semaphore!");
    }
    void release() {
      auto res = ReleaseSemaphore (handle, 1, null);
      if (!res) throw new Exception("Cannot release semaphore - win32 error!");
    }
  }
} else {
  extern(C) char* strerror(int);
  class PosixLock : LockImpl {
    pthread_mutex_t mutex;
    this() {
      if (auto res = pthread_mutex_init(&mutex, null))
        throw new Exception(Format("Cannot create Mutex - ", res));
    }
    ~this() { pthread_mutex_destroy(&mutex); }
    override void sys_lock() {
      if (auto res = pthread_mutex_lock(&mutex))
        throw new Exception(Format("Cannot lock mutex - ", res));
    }
    override void sys_unlock() {
      if (auto res = pthread_mutex_unlock(&mutex))
        throw new Exception(Format("Cannot unlock mutex - ", res));
    }
  }
  extern(C) {
    version(freebsd) alias void* pthread_cond_t;
    else struct pthread_cond_t { ubyte[48] data; }
    int pthread_cond_init(pthread_cond_t*, void* attr);
    int pthread_cond_destroy(pthread_cond_t*);
    int pthread_cond_signal(pthread_cond_t*);
    int pthread_cond_broadcast(pthread_cond_t*);
    int pthread_cond_wait(pthread_cond_t*, pthread_mutex_t* mutex);
  }
  // Something is broken with condvar
  // use the default impl instead
  // removed
  static if (!is(typeof(EAGAIN))) const EAGAIN = 11;
  // Yay, a GDC bug >_< TODO: remove on latter versions
  /*final */class Semaphore {
    private sem_t handle;
    this(int count=0) { if (sem_init(&handle, false, count) == -1) throw new Exception("sem_init failed!"); }
    ~this() { sem_destroy(&handle); }
    template sem_fn(string S) {
      const string sem_fn = "
        static if (!is(typeof(EINVAL))) const EINVAL = 22;
        typeof("~S~") res;
        do {
          res = "~S~";
          if (res == -1) {
            auto errno = getErrno();
            if (errno == EINTR) continue;
            if (errno == EINVAL) throw new Exception(\""~S~" failed: not a semaphore!\");
            if (errno == EAGAIN) break; // sem_trywait
            throw new Exception(Format(\""~S~": unknown error: \", strerror(errno), \"!\"));
          }
          break;
        } while (true);
      ";
    }
    void acquire() { mixin(sem_fn!("sem_wait(&handle)")); }
    void release() { mixin(sem_fn!("sem_post(&handle)")); }
    bool try_acquire() {
      mixin(sem_fn!("sem_trywait(&handle)"));
      return !res;
    }
  }
}

class ThreadBlock {
  private Thread which;
  private char[] mesg="!";
  this(char[] msg="") { if (msg.length) mesg=": "~msg; }
  void checkSame() {
    if (which) {
      if (which!=Thread.getThis) throw new Exception("ThreadBlock failed (checkSame)"~mesg);
    } else which = Thread.getThis();
  }
  void checkDifferent() { if (which && which==Thread.getThis) throw new Exception("ThreadBlock failed (checkDifferent)"~mesg); }
}

class DifferentThreadsBlock(int count) {
  private { ThreadBlock[count] block; char[] mesg; }
  this(char[] mesg) { this.mesg=mesg; foreach (ref bl; block) New(bl, mesg.length?"Thread "~.toString(which)~": "~mesg:""); }
  void opIn(int which) {
    if ((which<0)||(which!<count)) throw new Exception("Trying to block on invalid thread in DifferentThreadsBlock!");
    synchronized(this) {
      block[which].checkSame;
      foreach (id, b; block) if (id!=which) b.checkDifferent;
    }
  }
  void resume() { foreach (b; block) if (b.which) b.which.resume; }
}

import tools.log;
class MessageMultiChannel(T, bool SingleGet, bool SinglePut) {
  private {
    PageQueue!(T) mesgs;
    bool[Thread] waiting_threads;
  }
  static if (SinglePut) ThreadBlock put_block;
  static if (SingleGet) ThreadBlock get_block;
  Semaphore sem;
  this() {
    static if (SinglePut) New (put_block, "MessageMultiChannel_put");
    static if (SingleGet) New (get_block, "MessageMultiChannel_get");
    New(sem);
    New(mesgs);
  }
  int messages() { return mesgs.length; }
  bool active() { return mesgs.has; }
  void put(T t) {
    static if (SingleGet) get_block.checkDifferent;
    static if (SinglePut) {
      put_block.checkSame;
      mesgs.push(t);
    } else synchronized(this) mesgs.push(t);
    sem.release;
  }
  bool try_get(out T t) {
    static if (SinglePut) put_block.checkDifferent;
    static if (SingleGet) {
      get_block.checkSame;
      if (sem.try_acquire) {
        assert (mesgs.has);
        t = mesgs.pop;
        return true;
      } else return false;
    } else {
      if (sem.try_acquire) {
        assert (mesgs.has);
        synchronized(this) t = mesgs.pop;
        return true;
      } else return false;
    }
  }
  T get() {
    static if (SinglePut) put_block.checkDifferent;
    static if (SingleGet) {
      get_block.checkSame;
      sem.acquire;
      assert (mesgs.has);
      return mesgs.pop;
    } else {
      sem.acquire;
      synchronized(this) {
        if (!mesgs.has) {
          logln("Semaphore acquired but no message available!");
          asm { int 3; }
        }
        return mesgs.pop;
      }
    }
  }
}

template MessageChannel(T) { alias MessageMultiChannel!(T, true, true) MessageChannel; }

template _SyncObj(alias A) { Object obj; }
Object SyncObj(alias A)() {
  mixin(ctReplace(`
  if (!·)
    synchronized(GenSync) 
      if (!·)
        New(·);
  return ·;
  `, `·`, `_SyncObj!(A).obj`));
}

bool StackGrowsDown;
static this() {
  selfcall((void delegate() dg) { int a; StackGrowsDown = &a < dg.ptr; })();
}

struct ThreadInfo {
  void* low, high;
  void* base() {
    if (StackGrowsDown) return high;
    else return low;
  }
  void* base(void* nv) {
    if (StackGrowsDown) return high = nv;
    else return low = nv;
  }
  void* tip() {
    if (StackGrowsDown) return low;
    else return high;
  }
  void* tip(void* nv) {
    if (StackGrowsDown) return low = nv;
    else return high = nv;
  }
  void *[] list;
  void setEntry(int id, void* p) {
    if (id !< list.length) list.length = id + 1;
    list[id] = p;
  }
  void* getEntry(int id) {
    // logln("Get ", id, " from ", cast(void*) this);
    if (id !< list.length) list.length = id + 1;
    return list[id];
  }
  bool opIn_r(void* p) {
    /*if (p >= low && p <= high) {
      logln(p, " in ", low, " .. ", high);
    }*/
    return p >= low && p <= high;
  }
}

ThreadInfo[] threads; // immutable .. kind of.

version(Tango) {
  // TODO: unbreak
  pragma(msg, "Multithreading TLS is unimplemented and buggy! ");
  void* getBottom(Thread thr) { return cast(void*) -1; }
  void* getTop(Thread thr) { return cast(void*) 1; }
} else {
  import std.thread : Thread;
  void* getBottom(Thread thr) {
    return thr.stackBottom;
  }
  void* getTop(Thread thr) {
    return thr.stackTop;
  }
}

ThreadInfo* lookupThread(bool doAdd = true) {
  int local;
  auto lp = &local; // pointer of a local variable
  if (threads.length == 1 && lp in threads[0]) return &threads[0]; // trivial case
  // binary search the thread list
  auto low = 0, high = threads.length;
  while (low != high) {
    // remember, high is +1 to emulate slice semantics
    auto pivot = (low + high - 1) / 2;
    if (high < low) asm { int 3; }
    if (lp in threads[pivot]) {
      return &threads[pivot];
    }
    if (lp < threads[pivot].low) high = pivot;
    if (lp > threads[pivot].high) low = pivot + 1;
  }
  // logln("Add/extend thread. ");
  // thread not found. Query GC for thread info and look up again
  auto thr = Thread.getThis();
  auto bottom = getBottom(thr), top = getTop(thr);
  if (!top) top = bottom;
  if (!bottom) throw new Exception("Need a reliable thread bottom");
  foreach (ref thread; threads) {
    if (thread.base == bottom) {
      if (lp in thread) {
        // Must have missed that one?
        // Alternately, it was added in-between. Meh. Works for me.
        return &thread;
      }
      if (top /notin/ thread)
        thread.tip = top; // correct it
      if (lp /notin/ thread)
        thread.tip = lp;
      return &thread;
    }
    if (bottom in thread) {
      throw new Exception(Format(bottom, " in ", thread, ": fuck-up"));
    }
  }
  if (!doAdd) return null;
  // Entire new thread entry required
  synchronized {
    // Double Checked Locking Biatch
    if (auto res = lookupThread(false)) return res;
    ThreadInfo nthread;
    nthread.tip = top;
    nthread.base = bottom;
    if (lp /notin/ nthread)
      nthread.tip = lp;
    threads = threads[0 .. low] ~ nthread ~ threads[low .. $]; // reallocate!
    return &threads[low];
  }
}

int tls_max_key;
int[] freelist;

int getKey() {
  synchronized(SyncObj!(tls_max_key)) {
    if (freelist.length) return freelist.take();
    else return tls_max_key ++;
  }
}

template TLS(T) {
  final class TLS {
    static if (is(T: Object)) alias T Ref;
    else alias T* Ref;
    Ref delegate() maker;
    int key;
    private Ref[] values;
    static if (is(T: Object)) void each(void delegate(T) dg) { foreach (value; values) dg(value); }
    else void each(void delegate(T) dg) { foreach (value; values) dg(*value); }
    this(typeof(maker) m = null) {
      key = getKey();
      if (m) maker = m;
      else {
        static if (is(T: Object))
          static if (is(typeof(new T)))
            maker = { return new T; };
          else assert(false);
        else maker = { return &(new Stuple!(T))._0; };
      }
    }
    static if (is(T: Object)) {
      this(void delegate(T) i) {
        this(i /apply/ (typeof(i) i) {
          static if (is(typeof(new T)))
            auto t = new T;
          else {
            T t;
            assert(false);
          }
          i(t);
          return t;
        });
      }
    } else {
      this(void delegate(ref T) i) {
        this(i /apply/ (typeof(i) i) {
          auto t = &(new Stuple!(T))._0;
          i(*t);
          return t;
        });
      }
    }
    void set(Ref r) {
      lookupThread().setEntry(key, cast(void*) r);
    }
    Ref check() { return cast(Ref) lookupThread().getEntry(key); }
    Ref ptr() {
      auto res = cast(Ref) lookupThread().getEntry(key);
      if (!res) {
        res = maker();
        synchronized(this) values ~= res;
        set(res);
      }
      return res;
    }
    static if (canEmulate!(T)) {
      PointerProxy!(T) opCall() {
        PointerProxy!(T) res = void;
        res.ptr = ptr();
        return res;
      }
    } else alias ptr opCall;
    T value() {
      static if (is(T: Object)) return ptr();
      else return *ptr();
    }
    ~this() {
      synchronized(SyncObj!(tls_max_key)) {
        freelist ~= key;
      }
    }
  }
}

version(DS) { }
else {
  struct Sync(T) {
    static assert(canEmulate!(T), "Cannot emulate "~T.stringof~": sync not possible!");
    T value; Lock lock;
    T access() { return value; } T access(T t) { return value = t; }
    mixin(PropertyForward!("access",
      "if (!lock) volatile synchronized if (!lock) lock = new Lock; lock.lock(); scope(exit) lock.unlock();"
    ));
  }
}

static TLS!(int) ThreadID;
static int numThreads;
static this() {
  New(ThreadID, (ref int i) {
    synchronized i = numThreads ++;
  });
}
