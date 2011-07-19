module dglut.set;

// A delicious hack
struct Empty { string toString() { return ""; } } Empty empty;

import std.string: format, find, ifind;
struct Set(T) {
  Empty[T] aa;
  bool opIn_r(T what) { return (what in aa)!=null; }
  void set(T what) { aa[what]=empty; }
  void remove(T what) { aa.remove(what); }
  int opApply(int delegate(ref T) dg) {
    foreach (idx, bogus; aa) if (auto res=dg(idx)) return res;
    return 0;
  }
  string toString() { return format(aa.keys); }
  Set grep(bool delegate(T) dg) {
    Set res;
    foreach (entry; *this) {
      if (dg(entry)) res.set(entry);
    }
    return res;
  }
  static if (is(T: string)) {
    Set  grep(string what) { return grep((T t) { return  t.find(what)!=-1; }); }
    Set igrep(string what) { return grep((T t) { return t.ifind(what)!=-1; }); }
  }
  size_t length() { return aa.length; }
}
