module tools.array2d;
import tools.base, tools.log, tools.compat;

struct Array2D(T, bool sparse=false, int Width = int.max, int Height = int.max) {
  void copy(Array2D other) {
    assert(width == other.width && height == other.height);
    for (int y=0; y<height; ++y) {
      T* start=data + y*realwidth, end=start+width;
      T* other_start = other.data + y*realwidth;
      for (auto ptr=start, other_ptr = other_start; ptr!=end; ++ptr, ++other_ptr) {
        *ptr = *other_ptr;
      }
    }
  }
  T *data, end; // end is the end of _my_ data, not the containing array!
  static if (Width != int.max) const width = Width;
  else int width;
  static if (Height != int.max) const height = Height;
  else int height;
  alias width w; alias height h;
  static if (sparse) int realwidth; else alias width realwidth;
  void calcHeight() {
    static if (Height == int.max)
      height = 1 + (end-data-width) / realwidth;
  }
  int opEquals(U)(U other) {
    static assert(is(typeof(other.width))&&is(typeof(other.height)));
    if (width!=other.width || height!=other.height) return false;
    foreach (x, y, elem; other) {
      if (elem != opIndex(x, y)) return false;
    }
    return true;
  }
  string toString() {
    string res;
    auto h=height;
    for (int i=0; i<h; ++i) res~=Format(data[realwidth*i..realwidth*i+width]);
    return "["~res~"]";
  }
  Array2D!(T, true) xslice(size_t from, size_t to) {
    Array2D!(T, true) res=void;
    res.data=data+from; res.end=end-width+to;
    res.width=to-from;
    assert(res.width!>width);
    res.realwidth=realwidth;
    res.calcHeight;
    return res;
  }
  Array2D!(T, true) yslice(size_t from, size_t to) {
    Array2D!(T, true) res=void;
    res.data=data+realwidth*from;
    res.width=width; res.realwidth=realwidth;
    res.calcHeight;
    res.end=data+realwidth*to;
    assert(res.height!>height);
    return res;
  }
  static Array2D opCall(size_t l1, size_t l2) {
    Array2D res=void;
    res.data=(new T[l1*l2]).ptr;
    // breaks under LLVM
    static if (Width == int.max && Height == int.max) {
      res.width = l1; res.realwidth = l1;
      res.height = l2; res.end=res.data+l1*l2;
    } else {
      assert(l1 == Width && l2 == Height);
    }
    return res;
  }
  static Array2D opCall(size_t width, T[] source) {
    assert((source.length%width)==0);
    Array2D res=void;
    res.data=source.ptr;
    static if (Width == int.max) {
      res.width = width;
      res.realwidth = width;
    }
    // breaks under LLVM
    // res.width = res.realwidth = width;
    res.end=res.data+source.length;
    res.calcHeight;
    return res;
  }
  T[] yindex(size_t where) {
    int h=height;
    while (where<0) where += h;
    while (where!<h) where -= h;
    return (data+realwidth*where)[0..width];
  }
  T* ptrAt(int x, int y) {
    while (x<0) x += width; while (x!<width) x -= width;
    while (y<0) y += height; while (y!<height) y -= height;
    return data + y*realwidth + x;
  }
  T opIndex(int x, int y) { return *ptrAt(x, y); }
  T opIndexAssign(T value, int x, int y) { return *ptrAt(x, y) = value; }
  int opApply(int delegate(ref T) dg) {
    foreach (x, y, ref elem; *this) if (auto res=dg(elem)) return res;
    return 0;
  }
  int opApply(int delegate(ref int, ref int, ref T) dg) {
    for (int y=0; y<height; ++y) {
      T *start=data + y*realwidth, end=start+width;
      for (auto ptr=start; ptr!=end; ++ptr) {
        int x=ptr-start;
        if (auto res=dg(x, y, *ptr)) return res;
        assert(x == ptr-start, "Excuse me WTF R U doing");
      }
    }
    return 0;
  }
  void opAssign(T what) {
    /*static if (!sparse && (T.sizeof==ubyte.sizeof)) {
      memset(data, what, end-data);
    } else {*/
      for (int y=0; y<height; ++y) {
        auto iter=sh_iter(y);
        for (int x=0; x<width; ++x) { iter=what; ++iter; }
      }
    //}
  }
  Array2D dup() {
    auto res=Array2D(width, height);
    for (int y=0; y<height; ++y) {
      res.data[y*res.width .. (y+1)*res.width] = data[y*width .. (y+1)*width];
    }
    return res;
  }
  struct HorizLoopIterator {
    T *ptr, start, endminusone; Array2D *sup;
    T opCall() { assert(ptr<=endminusone); return *ptr; }
    static HorizLoopIterator opCall(T *a, T *b, T *c, Array2D *d) {
      HorizLoopIterator res=void;
      with (res) { ptr=a; start=b; endminusone=c-1; sup=d; }
      return res;
    }
    T prev() { assert(ptr<=endminusone); if (ptr==endminusone-sup.width+1) return *endminusone; return *(ptr-1); }
    T next() { assert(ptr<=endminusone); if (ptr==endminusone) return *start; return *(ptr+1); }
    T prev(T t) { if (ptr==start) *endminusone=t; else *(ptr-1)=t; return t; }
    T next(T t) { if (ptr==endminusone) *start=t; else *(ptr+1)=t; return t; }
    // ignore wraparound
    T prev_nowrap() { return *(ptr-1); }
    T next_nowrap() { return *(ptr+1); }
    size_t pos(size_t where) { ptr=start+where; return where; }
    size_t pos() { return ptr-start; }
    bool done() { return ptr == endminusone; }
    T aboveValue() {
      if (ptr < sup.data+sup.width) return *(ptr+(sup.end-sup.width-sup.data));
      else return *(ptr-sup.realwidth);
    }
    T belowValue() {
      if (ptr !< sup.end-sup.width) return *(ptr-(sup.end-sup.width-sup.data));
      else return *(ptr+sup.realwidth);
    }
    T opAssign(T value) { return (*ptr=value); }
    /// WARNING: FOR SPEED REASONS, THESE TWO FUNCTIONS DO *NOT* WRAP AROUND
    T *opPostInc() { return ptr++; }
    T *opAddAssign(size_t offs) { return ptr += offs; }
    // no such guarantee here
  }
  HorizLoopIterator h_iter(int y) {
    while (y<0) y += height;
    while (y!<height) y -= height;
    auto start = &data[realwidth*y];
    return HorizLoopIterator(start, start, start+width, this);
  }
  struct SimpleHorizLoopIterator {
    T *ptr;
    T opCall() { return *ptr; }
    static SimpleHorizLoopIterator opCall(T *x) { SimpleHorizLoopIterator res=void; res.ptr=x; return res; }
    SimpleHorizLoopIterator opAdd(int i) { auto res = *this; res += i; return res; }
    void opAssign(T value) { (*ptr)=value; }
    void opPostInc() { ptr++; }
    void opAddAssign(size_t offs) { ptr+=offs; }
  }
  SimpleHorizLoopIterator sh_iter(int y) {
    while (y<0) y += height;
    while (y!<height) y -= height;
    return SimpleHorizLoopIterator(&data[realwidth*y]);
  }
}

import tools.tests;
unittest {
  auto flat=[1, 2, 1, 2, 3, 2, 1, 2, 1];
  auto test=Array2D!(int)(3, flat);
  mustEqual("WidthTest", test.width, 3); mustEqual("HeightTest", test.height, 3);
  mustEqual("opEqual/SliceTest", test.xslice(0, 2), Array2D!(int)(2, [1, 2, 2, 3, 1, 2]));
  auto test2=Array2D!(bool)(3, 3);
  test2[1, 3]=true;
  test2[3, 1]=true;
  mustEqual("WrapTest", test2[1, 3], test2[3, 1], test2[0, 1], test2[1, 0], true);
}
