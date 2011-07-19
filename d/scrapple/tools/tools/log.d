module tools.log;

import tools.text, tools.compat;

version(DS) { int getThread() { return 0; } extern(C) void iprintf(char*, ...); }
else {
  int[Thread] ids;
  int getThread() {
    auto thr = Thread.getThis();
    synchronized {
      if (auto p = thr in ids) return *p;
      else return ids[thr] = ids.length;
    }
  }
}

bool log_threads = true;

Object log_sync;
static this() { log_sync = new Object; }

version(DS) { } else {
  FILE * logfile=null;
  static ~this() {
    if (log_threads && lastThread) log("\\", repeat("-", Format("+--[ ", lastThread, " ]------").length-1), "\n");
    if (logfile) fclose(logfile);
  }
}
void _printf(T...)(char[] fmt, T t) {
  version(DS) { assert(*(fmt.ptr + fmt.length) == 0, "Weird format string"); iprintf(fmt.ptr, t); }
  else {
    printf(toStringz(fmt), t);
    if (logfile) fprintf(logfile, toStringz(fmt), t);
  }
}
void _log(T...)(T t) {
  foreach (index, part; t) {
    alias typeof(part) PT;
    static if (is(PT : char[])) _printf("%.*s", part.length, part.ptr);
    else static if (is(PT==char)) _printf("%c", part);
    else static if (is(PT==bool)) _log(part?"True":"False");
    else static if (is(PT==delegate)) _log(part.funcptr, "@", part.ptr);
    else static if (is(PT: int) && PT.sizeof <= 4) _printf("%i", cast(int) part);
    else static if (is(PT: uint) && PT.sizeof <= 4) _printf("%u", cast(uint) part);
    else static if (is(PT: long)) _log(Format(part));
    else static if (is(PT: ulong)) _log(Format(part));
    //else static if (is(PT: real)) _printf("%21.6Lf", cast(real) part);
    else static if (is(PT: real)) _printf("%f", cast(double) part);
    else static if (is(PT: creal)) _log(part.re, " + ", part.im, "i");
    else static if (is(PT==void *)) { _printf("(%p)", part); }
    else static if (isPointer!(PT)) {
      static if (is(typeof(PT.Name): string))
        _log(PT.Name, cast(void *)part);
      else _log(PT.stringof, cast(void *)part);
    }
    else static if (is(typeof(part.classinfo))) {
      auto c=cast(Object) part;
      if (c) _log(c.toString());
      else _log("(null)");
    }
    else static if (is(PT==struct)) {
      static if(is(typeof(part.toString))) {
        _log(part.toString());
      } else {
        static if (is(typeof(PT.Name): string)) _log(PT.Name);
        else _log(PT.stringof);
        _log("{ ");
        // pragma(msg, typeof(part.tupleof).stringof);
        foreach (id, entry; part.tupleof) {
          // pragma(msg, typeof(id).stringof, " - ", typeof(entry).stringof);
          if (id) _log(", ");
          _log(entry);
        }
        _log(" }");
      }
    }
    else static if (isArray!(PT)) {
      _log("[");
      foreach (id, elem; part) {
        if (id) _log(", ");
        _log(elem);
      }
      _log("]");
    }
    else static if (is(typeof(part.keys))) {
      _log("[");
      int count;
      foreach (id, elem; part) {
        _log(id, " => ", elem);
        if (count++ < part.length - 1) _log(", ");
      }
      _log("]");
    }
    else static assert(false, "Type "~PT.stringof~" not supported. Maybe you can add support for it?");
    //else _log("Unsupported :"~typeof(part).stringof~": ");
  }
}

version(DS) {
  void logln(T...)(T t) { _log(t, "\n"); }
} else {
  Thread lastThread=null;
  string[Thread] threadnames;
  
  void logln(T...)(T t) {
    auto thr = Thread.getThis();
    char prefix='+';
    synchronized (log_sync) {
      if (!lastThread) prefix='/';
      auto threadname = (thr in threadnames)?threadnames[thr]:((getThread==0)?"Main Thread":Format("Unidentified #", getThread));
      if (log_threads && (lastThread !is thr))
        _log(prefix, "--[ ", threadname, " ]------", "\n");
      if (log_threads) _log("| ", t, "\n");
      else _log(t, "\n");
      lastThread = thr;
    }
  }
}

void log(T...)(T t) {
  synchronized(log_sync) _log(t);
}
