module tools.page_queue;

class PageQueueBoundsException : Exception { this(char[] L) { super("Page queue underrun - "~L~"!"); } }

import tools.base;
class PageQueue(T, int FREELIST_SIZE=128) {
  Page* [FREELIST_SIZE] freelist;
  size_t freelist_idx;
  struct Page {
    static assert(T.sizeof < 4000);
    T[4000/T.sizeof] data; // almost one page .. leave some space for GC structures
    int start, end; // four bytes -> distinct processor access possible
    invariant { assert (start <= end, Format("Start ", start, " higher than end ", end, "! Desperation!")); } 
    Page* next;
    bool push(T what) {
      if (end < data.length) {
        data [end++] = what;
        return true;
      } else return false;
    }
    bool pop(ref T what) {
      if (start < end) {
        what = data [start++];
        return true;
      } else return false;
    }
    bool has() { return start < end; }
  }
  Page* first, last;
  size_t length() {
    if (first == last) return first.end - first.start;
    size_t res = first.data.length - first.start;
    auto cur = first.next;
    while (cur != last) {
      res += cur.data.length;
      cur = cur.next;
    }
    res += last.end;
    return res;
  }
  this() { first = last = new Page; }
  bool has() { return first.has || (first.next && first.next.has) || last.has; }
  Page* getPagePtr() {
    if (!freelist_idx) return new Page;
    else synchronized (this) {
      volatile if (!freelist_idx) return new Page;
      else {
        auto res = freelist[--freelist_idx];
        (*res) = Init!(Page);
        return res;
      }
    }
  }
  void addPagePtr(Page* p) {
    if (freelist_idx != FREELIST_SIZE)
      synchronized (this)
        volatile if (freelist_idx != FREELIST_SIZE) freelist[freelist_idx ++] = p;
  }
  void push(T what) {
    if (!last.push (what)) {
      last = (last.next = getPagePtr());
      last.push (what);
    }
  }
  T pop() {
    T res;
    if (!first.pop (res)) {
      if (first == last) throw new PageQueueBoundsException("1");
      auto backup = first;
      first = first.next;
      assert (first);
      addPagePtr (backup);
      if (!first.pop (res)) throw new PageQueueBoundsException("2");
    }
    return res;
  }
}
