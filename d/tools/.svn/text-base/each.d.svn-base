module tools.each;
import tools.base, tools.compat;

template Each(string INT, string INT_REV, KEY, VALUE) {
  // Presumed existing: 
  //    void each(void delegate(size_t, ref T, proc) dg) {
  //    void each_reverse(void delegate(size_t, ref T, proc) dg) {
  void each(void delegate(KEY, ref VALUE, proc) dg) { mixin(INT~"(dg); "); }
  void each(void delegate(ref VALUE, proc) dg) { each((KEY i, ref VALUE t, proc p) { dg(t, p); }); }
  void each(void delegate(ref VALUE) dg) { each((KEY i, ref VALUE t, proc p) { dg(t); }); }
  void each(void delegate(VALUE) dg) { each((KEY i, ref VALUE t, proc p) { dg(t); }); }
  void each_reverse(void delegate(KEY, ref VALUE, proc) dg) { mixin(INT_REV~"(dg); "); }
  void each_reverse(void delegate(ref VALUE, proc) dg) { each_reverse((KEY i, ref VALUE t, proc p) { dg(t, p); }); }
  void each_reverse(void delegate(ref VALUE) dg) { each_reverse((KEY i, ref VALUE t, proc p) { dg(t); }); }
  void each_reverse(void delegate(VALUE) dg) { each_reverse((KEY i, ref VALUE t, proc p) { dg(t); }); }
  int opApply(int delegate(ref KEY, ref VALUE) dg) {
    int res;
    each((KEY k, ref VALUE v, proc brk) {
      if (auto r = dg(k, v)) { res = r; return brk(); }
    });
    return res;
  }
  int opApply(int delegate(ref VALUE) dg) {
    int res;
    each((ref VALUE v, proc brk) {
      if (auto r = dg(v)) { res = r; return brk(); }
    });
    return res;
  }
}
