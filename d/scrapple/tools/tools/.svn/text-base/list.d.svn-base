module tools.list;
import tools.base, tools.each, tools.compat;

class ListBoundsException : Exception { this() { super("List bounds exceeded!"); } }

private const quietly = true;

struct List(T) {
  struct Entry {
    T value;
    Entry* next, prev;
    static Entry *opCall(T value, Entry *next, Entry *prev) {
      auto res = new Entry;
      res.value = value; res.next = next; res.prev = prev;
      return res;
    }
  }
  struct ListIterator {
    Entry* entry;
    List* parent;
    bool negative; // parked in negative space
    T value() { return entry.value; }
    Entry* prev() {
      if (entry) return entry.prev;
      else if (!negative) return parent.last;
      else throw new Exception("No predecessor for -1");
    }
    Entry* next() {
      if (entry) return entry.next;
      else if (!negative) return parent.first;
      else throw new Exception("No successor for $");
    }
    void back(bool quietly = false) {
      if (!entry) {
        if (negative) {
          if (!quietly) throw new Exception("Cannot go back further than -1! ");
        } else entry = parent.last;
      } else {
        entry = entry.prev;
        negative = true;
      }
    }
    void forward(bool quietly = false) {
      if (!entry) {
        if (!negative) {
          if (!quietly) throw new Exception("Cannot go back further than -1! ");
        } else entry = parent.first;
      } else {
        entry = entry.next;
        negative = false;
      }
    }
    void insert_after(T t) {
      if (!entry) {
        if (!negative) throw new Exception("Cannot insert after after the end! "); // Not a typo!
        parent.prepend(t);
      } else if (entry is parent.last) {
        parent.append(t);
      } else {
        entry.next = Entry(t, entry.next, entry);
        entry.next.next.prev = entry.next; // Ha-ha!
      }
    }
    void insert_before(T t) {
      if (!entry) {
        if (negative) throw new Exception("Cannot insert before before the start! "); // Not a typo either!
        parent.append(t);
      } else if (entry is parent.first) {
        parent.prepend(t);
      } else {
        entry.prev = Entry(t, entry, entry.prev);
        entry.prev.prev.next = entry.prev; // Hee-hee!
      }
    }
    T remove(bool quietly = false) {
      if (!entry) {
        if (!quietly) throw new Exception("Cannot erase from outside proper iteration space: nothing there! ");
        return Init!(T);
      }
      
      if (entry is parent.first) { parent.first = entry.next; parent.first.prev = null; }
      else entry.prev.next = entry.next;
      
      if (entry is parent.last) { parent.last = entry.prev; parent.last.next = null; }
      else entry.next.prev = entry.prev;
      
      return entry.value;
    }
  }
  string toString() {
    string res;
    foreach (entry; *this) {
      if (res.length) res ~= " -> ";
      res ~= Format("[", entry, "]");
    }
    return res;
  }
  Entry* first, last;
  T top() in { assert(last); } body { return last.value; }
  T bottom() in { assert(first); } body { return first.value; }
  void append(T v) {
    last = Entry(v, null, last);
    if (!first) first = last;
    if (last.prev) last.prev.next = last;
  }
  alias append opCatAssign;
  void prepend(T v) {
    first = Entry(v, first, null);
    if (!last) last=first;
    if (first.next) first.next.prev = first;
  }
  ListIterator lookup(size_t pos) in { assert(first && last); } body {
    auto cur = first;
    while (pos--) {
      if (cur is last) throw new ListBoundsException;
      cur = cur.next;
    }
    return ListIterator(cur, this);
  }
  T opIndex(size_t pos) { return lookup(pos).entry.value; }
  size_t length() {
    auto cur = first;
    if (!cur) return 0;
    size_t l = 1;
    while (cur !is last) { cur = cur.next; ++l; }
    return l;
  }
  private {
    void _each(void delegate(size_t, ref T, proc) dg) {
      auto cur = first;
      if (!cur) return;
      bool brk;
      size_t count=0;
      while (true) {
        dg(count, cur.value, { brk = true; });
        ++count;
        if (brk || cur is last) break;
        cur = cur.next;
      }
    }
    void _each_reverse(void delegate(size_t, ref T, proc) dg) {
      auto cur = last;
      if (!cur) return;
      bool brk;
      size_t count = length();
      while (true) {
        --count;
        dg(count, cur.value, { brk = true; });
        if (brk || cur is first) break;
        cur = cur.prev;
      }
    }
  }
  mixin Each!("_each", "_each_reverse", size_t, T);
  T remove(size_t which) { return lookup(which).remove; }
}

struct UnrolledList(T) {
  alias List!(T[]).ListIterator LI;
  List!(T[]) chunks;
  string toString() { return "UnrolledList "~chunks.toString(); }
  size_t length() {
    size_t res;
    foreach (chunk; chunks) res += chunk.length;
    return res;
  }
  Stuple!(LI, int) lookup(int i) {
    auto res = stuple(cast(LI) chunks.lookup(0), i);
    foreach (chunk; chunks) {
      if (chunk.length <= res._1) {
        res._0.forward();
        res._1 -= chunk.length;
      } else {
        return res;
      }
    }
    if (!res._1) return res; // One past the end is still fine. 
    throw new Exception(Format("Cannot lookup ", i, " in unrolled list: length ", length, "!"));
  }
  LI presplit(int where, bool omit /* omit the index from the result*/) {
    auto id = lookup(where);
    auto iter = id._0, subidx = id._1;
    if (!iter.entry) {
      assert (!subidx);
      logln("presplit at ", where, ", ", omit, ": no value");
      return iter; // nothing to change.
    }
    auto
      prevchunk = iter.value[0 .. subidx],
      nextchunk = iter.value[subidx+omit .. $];
    if (prevchunk.length)
      iter.insert_before(prevchunk);
    if (nextchunk.length)
      iter.insert_after(nextchunk);
    return iter;
  }
  void insert(T t, int where) {
    auto iter = presplit(where, false);
    T[] foo; foo ~= t;
    iter.entry.value = foo;
  }
  Stuple!(LI, LI) split(int where) {
    auto li = presplit(where, false), pre = li, post = li;
    pre.back(quietly); post.forward(quietly);
    li.remove(quietly);
    return stuple(pre, post);
  }
  Stuple!(LI, LI) split_range(int from, int to) {
    return stuple(split(from)._0, split(to)._1);
  }
  UnrolledList opSlice(int from, int to) {
    auto l1 = lookup(from), l2 = lookup(to);
    if (l1._0.entry is l2._0.entry) {
      UnrolledList res;
      res ~= l1._0.value[l1._1 .. l2._1];
      return res;
    }
    auto
      partial1 = l1._0.entry?l1._0.value[l1._1 .. $]:null,
      partial2 = l2._0.entry?l2._0.value[0 .. l2._1]:null;
    UnrolledList res;
    if (partial1.length) res ~= partial1;
    while (true) {
      l1._0.forward;
      if (!l1._0.entry || l1._0.entry is l2._0.entry) break;
      res ~= l1._0.value;
    }
    if (partial2.length) res ~= partial2;
    return res;
  }
  UnrolledList opSlice() { return opSlice(0, length); }
  T[] flatten() {
    auto res = new T[length];
    int i;
    foreach (chunk; chunks) {
      res[i .. i + chunk.length] = chunk;
      i += chunk.length;
    }
    return res;
  }
  UnrolledList dup() {
    UnrolledList res;
    res.append(flatten);
    return res;
  }
  int opApply(int delegate(ref int i, ref T t) dg) {
    int i;
    foreach (chunk; chunks) {
      foreach (elem; chunk) {
        if (auto res = dg(i, elem)) return res;
        i++;
      }
    }
    return 0;
  }
  int opApply(int delegate(ref T t) dg) {
    foreach (chunk; chunks) {
      foreach (elem; chunk) {
        if (auto res = dg(elem)) return res;
      }
    }
    return 0;
  }
  void remove(int where) { presplit(where, true).remove; }
  T opIndex(int where) { with (lookup(where)) return _0.value[_1]; }
  T opIndexAssign(T t, int where) { with (lookup(where)) return _0.value[_1] = t; }
  void append(T t) { T[] foo; foo ~= t; chunks.append(foo); }
  void append(T[] ta) { chunks.append(ta); }
  alias append opCatAssign;
  void prepend(T t) { T[] foo; foo ~= t; chunks.prepend(foo); }
  void prepend(T[] ta) { chunks.prepend(ta); }
}
