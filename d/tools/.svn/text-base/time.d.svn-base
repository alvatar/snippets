module tools.time;

import tools.log, tools.compat, tools.base: slowyield;

alias void delegate()[] no;
long time(no see...) {
  auto start = µsec();
  foreach (dg; see) {
    dg();
  }
  return cast(long) (µsec() - start);
}

alias ulong d_time;
version(Windows) {
  //ulong myClock() { return clock(); }
  extern(Windows) int timeGetTime();
  d_time µsec() { return cast(d_time) timeGetTime() * 1000; }
  double sec() { return timeGetTime() / 1_000.0; }
} else {
  extern(C) {
    static if ((void*).sizeof == 4) alias uint time_t;
    else static if ((void*).sizeof == 8) alias ulong time_t;
    else static assert(false, "WHAT");
    struct timespec {
      time_t sec;
      int nsec;
    }
    int clock_gettime(int clk_id, timespec* tp);
    int gettimeofday(timespec* tp, void* tz);
    d_time µsec() {
      timespec ts = void;
      // if (clock_gettime(1, &ts) == -1)
      gettimeofday(&ts, null);
      return ts.sec*1_000_000 + ts.nsec;
    }
    double sec() {
      timespec ts = void;
      // if (clock_gettime(1, &ts) == -1)
      gettimeofday(&ts, null);
      auto res = (cast(double) ts.sec) * 1.0 + (cast(double) ts.nsec) / 1_000_000.0;
      return res;
    }
  }
}

version(Windows) { } else extern(C) {
  int nanosleep(timespec* req, timespec* rem);
}
void sleep(float s) {
  version(Windows) {
    auto start = sec();
    while (sec() < start + s) slowyield();
  } else {
    timespec t;
    t.sec = cast(time_t) s; s -= t.sec;
    t.nsec = cast(int) (s * 1_000_000_000);
    nanosleep(&t, null);
  }
}

struct Timer {
  alias typeof(sec()) TT;
  TT backdated_started, last_stopped;
  bool beenStarted() { return !isnan(backdated_started); }
  bool running() { return beenStarted && isnan(last_stopped); }
  void start() in {
    assert(!running, "Tried to start Timer that was already running.");
  } body {
    if (!beenStarted) backdated_started = sec;
    else {
      backdated_started += sec - last_stopped; // backdate started to contain previous times
      last_stopped = TT.nan;
    }
  }
  TT elapsed() in {
    assert(beenStarted, "Tried to take time from Timer that wasn't started!");
  } body {
    if (running) return sec - backdated_started;
    else return last_stopped - backdated_started;
  }
  TT stop() in {
    assert(running, "Tried to stop Timer that wasn't running.");
  } body {
    last_stopped = sec;
    return elapsed;
  }
  void zero() {
    backdated_started = last_stopped = TT.nan;
  }
  void restart() { zero; start; }
}

ulong ticks() {
  uint a, b;
  asm {
    rdtsc;
    mov a, EAX;
    mov b, EDX;
  }
  ulong res = b;
  res <<= 32UL;
  res |= a;
  return res;
}
