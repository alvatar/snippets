module test19;

import qd;

import tools.base, tools.functional, tools.log, tools.time, tools.compat;

alias tools.base.max max;
alias tools.base.min min;

alias ubyte Cell;

int scale = 1;

struct rect {
  int x, y, w, h;
  bool overlaps(rect other) {
    if (x > other.x + other.w) return false; // right of it
    if (x+w < other.x) return false; // left of it
    if (y > other.y + other.h) return false; // bottom of it
    if (y+h < other.y) return false; // top of it
    return true; // otherwise overlap
  }
}

struct subtree {
    /**
       x 0 1
       y ___
       0|0 2
       1|1 3
    **/
  static assert(Cell.sizeof < (subtree*).sizeof);
  union {
    Repeat!(subtree*, 4) subtrees;           Repeat!(Cell, 4) values;
    subtree*[2][2] subtrees_xy;              Cell[2][2] values_xy;
    struct { subtree* tl, bl, tr, br; }      struct { Cell c_tl, c_bl, c_tr, c_br; }
  }
  bool bottom() { return !br; }
  int level() {
    if (bottom) return 0;
    else return 1 + tl.level;
  }
  bool isNull() {
    if (bottom) {
      return !c_tl && !c_bl && !c_tr && !c_br;
    } else {
      if (!tl.isNull) return false;
      // logln("Start");
      foreach (i, tree; subtrees) {
        // logln("Loop ", i, " @", this, ", child: ", tree);
        // if (!i) continue; BREAKS LDC
        if (i) {
          // if (tree is tl) continue; BREAKS SEE ABOVE // null
          if (!(tree is tl)) {
            if (!tree.isNull) return false;
            // we found a duplicate null. adapt.
            subtrees[i] = tl;
          }
        }
      }
      return true;
    }
  }
  int descend(int xbase, int ybase, int delegate(int x, int y, ref Cell v) dg) {
    if (bottom) {
      foreach (x; Tuple!(0, 1))
        foreach (y; Tuple!(0, 1))
          if (auto res = dg(xbase + x, ybase + y, values_xy[x][y]))
            return res;
    } else {
      foreach (x; Tuple!(0, 1))
        foreach (y; Tuple!(0, 1))
          if (auto res = subtrees_xy[x][y].descend(xbase + (size / 2) * x, ybase + (size / 2) * y, dg))
            return res;
    }
    return 0;
  }
  int opApply(int delegate(ref int x, ref int y, ref Cell v) dg) {
    return descend(0, 0, (int x, int y, ref Cell v) { return dg(x, y, v); });
  }
  void render() {
    int a, b, c, d;
    foreach (x, y, cell; *this) {
      auto xp = x * scale, yp = y * scale;
      if (xp !< screen.w || yp !< screen.h) continue;
      rgb color;
      switch (cell) {
        case 0: color = Black; a ++; break;
        case 1: color = Yellow; b ++; break;
        case 2: color = Blue ~ White; c ++; break;
        case 3: color = Red; d ++; break;
      }
      for (int i = 0; i < scale; ++i)
        for (int j = 0; j < scale; ++j)
          pset(xp+i, yp+j, color);
    }
  }
  void renderAt(int x, int y, int w, int h, void delegate(int, int, Cell) dg, int my_x = 0, int my_y = 0) {
    if (bottom) {
      dg(my_x, my_y, c_tl);
      dg(my_x+1, my_y, c_tr);
      dg(my_x, my_y+1, c_bl);
      dg(my_x+1, my_y+1, c_br);
    } else {
      auto s = size();
      auto my_xr = my_x + s, my_yr = my_y + s;
      auto my_xhalf = my_x + s / 2, my_yhalf = my_y + s / 2;
      auto scr = rect(x, y, w, h);
      if (scr.overlaps(rect(my_x, my_y, s/2, s/2))) tl.renderAt(x, y, w, h, dg, my_x, my_y);
      if (scr.overlaps(rect(my_x, my_yhalf, s/2, s/2))) bl.renderAt(x, y, w, h, dg, my_x, my_yhalf);
      if (scr.overlaps(rect(my_xhalf, my_y, s/2, s/2))) tr.renderAt(x, y, w, h, dg, my_xhalf, my_y);
      if (scr.overlaps(rect(my_xhalf, my_yhalf, s/2, s/2))) br.renderAt(x, y, w, h, dg, my_xhalf, my_yhalf);
    }
  }
  ulong size() { return 2UL << level; }
}

void delegate()[] cleans;

class MyDict(K, V) {
  const K EmptyKey = K.init;
  bool emptySet;
  Entry empty;
  struct Entry {
    K key;
    V value;
  }
  static uint hash(K key) {
    static if (K.sizeof == 4)
      return *(cast(uint*) &key);
    else {
      return typeid(K).getHash(&key);
    }
  }
  this(int i) { setSizePower2(i); cleans ~= &clean; }
  Entry[4][] list;
  uint mask;
  
  void clean() {
    emptySet = false;
    foreach (ref entry; list) {
      entry[] = Init!(Entry[8]);
    }
  }
  void setSizePower2(int p2) {
    if (p2 >= 2) p2 -= 2;
    list.length = 1 << p2;
    mask = list.length - 1;
  }
  
  Entry* findPos(K key, ref bool found) {
    if (empty.key == key) { scope(success) emptySet = true; found = emptySet; return &empty; }
    auto h = hash(key);
    auto le = &(list[h&mask]);
    foreach (k, ref entry; *le) {
      if (entry.key == key) {
        found = true;
        if (k) {
          swap(entry, (*le)[k-1]);
          return &((*le)[k-1]);
        } else return &entry;
      } else if (entry.key == EmptyKey) {
        found = false;
        return &entry;
      }
    }
    found = false;
    return &((*le)[$-1]);
  }
  int size() { return list.length; }
}

final class MemoCache(K, V) {
  MyDict!(K, V) backend;
  this() { New(backend, 20); }
  size_t length() { return backend.size; }
  V get(string FN)(K k) {
    bool found = void;
    auto pos = backend.findPos(k, found);
    if (found) {
      return pos.value;
    }
    auto v = mixin(FN);
    pos.key = k; pos.value = v;
    return v;
  }
}

int Size(int d) {
  switch (d) {
    case 0, 1: return 22;
    case 2, 3, 4, 5, 6: return 20;
    case 7: return 16;
    default: return 12;
  }
}

final class MemoCacheDepthBased(K, V, alias D) {
  alias MyDict!(K, V) Dict;
  Dict[] backend;
  size_t length() { return 0; }
  V get(string FN)(K k) {
    auto d = D(k);
    while (d !< backend.length) { backend ~= new Dict(Size(backend.length)); }
    
    bool found = void;
    auto entry = backend[d].findPos(k, found);
    if (found) {
      return entry.value;
    }
    auto v = mixin(FN);
    entry.key = k;
    entry.value = v;
    return v;
  }
}

template hasDepth(T) {
  const hasDepth = is(typeof(Init!(T).level())) || is(typeof(Init!(T)._0.level()));
}

int getDepth(T)(T t) {
  static if (is(typeof(!t))) if (!t) return 0; // failure in progress; returned value doesn't really matter
  static if (is(typeof(!t._0))) if (!t._0) return 0; // failure in progress; returned value doesn't really matter
  
  static if (is(typeof(t.level()))) return t.level(); else
  static if (is(typeof(t._0.level()))) return t._0.level(); else
  static assert(false, "Whuh? "~T.stringof);
}

template Memoize(string FN, string NEWNAME, string SALT = "") {
  const string Memoize = ctReplace(`
    alias Ret!(typeof(&FN)) IDENT_R;
    alias Params!(typeof(&FN)) IDENT_P;
    static if (hasDepth!(Stuple!(IDENT_P)))
      MemoCacheDepthBased!(Stuple!(IDENT_P), IDENT_R, getDepth!(Stuple!(IDENT_P))) IDENT_cache;
    else
      MemoCache!(Stuple!(IDENT_P), IDENT_R) IDENT_cache;
    static this() { New(IDENT_cache); logln("STATIC CONSTRUCTOR: built IDENT_cache"); }
    IDENT_R NEWNAME(IDENT_P p) { return IDENT_cache.get!("FN"~"(k.tupleof)")(stuple(p)); }
    `, `NEWNAME`, NEWNAME, `IDENT`, NEWNAME~SALT, `FN`, FN
  );
}

class CheapHeap {
  void[] mem;
  int freeptr;
  this(int size) { mem = cmalloc(size)[0 .. size]; }
  ~this() { cfree(mem.ptr); }
  int size() { return mem.length; }
  int free() { return mem.length - freeptr; }
  int used() { return freeptr; }
  float load() { return used * 1f / size; }
  void* malloc(int i) {
    if (freeptr + i < size) {
      auto res = &mem[freeptr];
      freeptr += i;
      return res;
    } else {
      return null;
    }
  }
  T* alloc(T)() {
    return cast(T*) malloc(T.sizeof);
  }
}

CheapHeap chirp;
static this() { New(chirp, 64*1024*1024); }

subtree* _getSubtree1(Repeat!(subtree*, 4) trees) {
  foreach (tree; trees) if (!tree) return null;
  debug foreach (value; Tuple!(0, 1, 2)) {
    assert(trees[value].level == trees[value+1].level, Format(
      "Cannot build subtree from trees with different levels: ", 
      [trees[0].level, trees[1].level, trees[2].level, trees[3].level]
    ));
  }
  auto res = chirp.alloc!(subtree);
  if (!res) return null;
  foreach (id, value; trees) res.subtrees[id] = value;
  return res;
}

subtree* _getSubtree2(Repeat!(Cell, 4) values) {
  auto res = chirp.alloc!(subtree);
  if (!res) return null;
  foreach (id, value; values) res.values[id] = value;
  return res;
}

mixin(Memoize!("_getSubtree1", "getSubtree", "1") ~ Memoize!("_getSubtree2", "getSubtree", "2"));

subtree* _getNulltree(ulong level) {
  if (!level) {
    return chirp.alloc!(subtree);
  }
  auto below = getNulltree(level-1);
  return getSubtree(below, below, below, below);
}

mixin(Memoize!("_getNulltree", "getNulltree"));

subtree* grow(subtree* center) {
  if (!center) return null;
  /**
    0 0 | 0 0
    0 tl|tr 0    0 2
    - - + - -    1 3
    0 bl|br 0
    0 0 | 0 0
  **/
  const string RET = "return getSubtree(
    getSubtree(·, ·, ·, %tl),
    getSubtree(·, ·, %bl, ·),
    getSubtree(·, %tr, ·, ·),
    getSubtree(%br, ·, ·, ·)
  );";
  if (center.bottom) { mixin(ctReplace(RET, "·", "0", "%", "center.c_")); }
  else {
    auto zero = getNulltree(center.level - 1);
    mixin(ctReplace(RET, "·", "zero", "%", "center."));
  }
}

subtree* buildSubtree(int width, int height, Cell delegate(int, int) dg) {
  auto size = cast(int) (tools.compat.log(max(width, height)) / tools.compat.log(2) + 1);
  subtree* build_descend(int x, int y, int level) {
    if (x > width || y > height) return getNulltree(level);
    if (!level) {
      return getSubtree(dg(x, y), dg(x, y+1), dg(x+1, y), dg(x+1, y+1));
    }
    auto offs = 1 << level;
    return getSubtree(
      build_descend(x, y, level - 1), build_descend(x, y + offs, level - 1),
      build_descend(x + offs, y, level - 1), build_descend(x + offs, y + offs, level - 1)
    );
  }
  return build_descend(0, 0, size);
}

Cell calc(Repeat!(Cell, 9) field) {
  switch (field[4]) {
    case 0: return 0;
    case 2: return 3;
    case 3: return 1;
    case 1:
      ubyte heads;
      foreach (id, value; field)
        if (id != 4 && value == 2) ++heads;
      if (heads == 1 /or/ 2) return 2;
      return 1;
  }
}

import tools.threadpool, tools.page_queue;
Threadpool tp;

// grows input if needed to make sure its periphery is null
subtree* force_shrink(subtree* input) {
  if (!input) return null;
  assert(!input.bottom, "Cannot shrink bottom");
  if (input.tl.bottom) {
    return getSubtree(input.tl.c_br, input.bl.c_tr, input.tr.c_bl, input.br.c_tl);
  } else {
    return getSubtree(input.tl.br, input.bl.tr, input.tr.bl, input.br.tl);
  }
}

// returns a subtree one level below the parameter, centered on the parameter
subtree* _step(subtree* node, int depth) {
  if (!node) return null;
  assert(node.level, "Cannot step LV0 node");
  if (node.level == 1) {
    assert(!depth);
    /**
      00 02      20 22   00 02     08 10
      01  +--  --+  23   01  +-- --+  11
          |03  21|           |03 09|

          |12  30|           |06 12|
      10  +--  --+  32   04  +-- --+  14
      11 13      31 33   05 07     13 15
    **/
    alias Tuple!(
       0,  2,  8,  1,  3,  9,  4,  6, 12,
       1,  3,  9,  4,  6, 12,  5,  7, 13,
       2,  8, 10,  3,  9, 11,  6, 12, 14,
       3,  9, 11,  6, 12, 14,  7, 13, 15
    ) indices;
    Repeat!(Cell, 36) values;
    foreach (id, value; indices)
      values[id] = node.subtrees[value/4].values[value%4];
    return getSubtree(
      calc(values[ 0 ..  9]), calc(values[ 9 .. 18]),
      calc(values[18 .. 27]), calc(values[27 .. 36])
    );
  }
  with (*node) {
    /**
      0 1 2
      3 4 5
      6 7 8
    **/
    Repeat!(subtree*, 9) nines;
    auto next = depth; if (next) next--;
    
    nines[0] = step(tl, next); nines[1] = step(getSubtree(tl.tr, tl.br, tr.tl, tr.bl), next); nines[2] = step(tr, next);
    nines[3] = step(getSubtree(tl.bl, bl.tl, tl.br, bl.tr), next); // left
    nines[4] = step(getSubtree(tl.br, bl.tr, tr.bl, br.tl), next); // center
    nines[5] = step(getSubtree(tr.bl, br.tl, tr.br, br.tr), next); // right
    nines[6] = step(bl, next); nines[7] = step(getSubtree(bl.tr, bl.br, br.tl, br.bl), next); nines[8] = step(br, next);
    foreach (nine; nines) if (!nine) return null;
    // now split into fours to calculate the inner quad
    alias Tuple!(
      0, 3, 1, 4, // first square
      3, 6, 4, 7, // second square
      1, 4, 2, 5, // etc
      4, 7, 5, 8
    ) indices;
    Repeat!(subtree*, 16) values;
    foreach (id, value; indices) values[id] = nines[value];
    if (depth)
      return getSubtree(
        step(getSubtree(values[ 0 ..  4]), next), step(getSubtree(values[ 4 ..  8]), next),
        step(getSubtree(values[ 8 .. 12]), next), step(getSubtree(values[12 .. 16]), next)
      );
    else
      return getSubtree(
        force_shrink(getSubtree(values[ 0 ..  4])), force_shrink(getSubtree(values[ 4 ..  8])),
        force_shrink(getSubtree(values[ 8 .. 12])), force_shrink(getSubtree(values[12 .. 16]))
      );
  }
}

mixin(Memoize!("_step", "step"));

// grows input if needed to make sure its periphery is null
subtree* grow_if_needed(subtree* input) {
  if (!input) return null;
  if (input.bottom) return grow(input);
  /**
     0  2  8 10
     1  3  9 11
     4  6 12 14
     5  7 13 15
  **/
  alias Tuple!(0, 2, 8, 10, 1, 11, 4, 14, 5, 7, 13, 15) border;
  if (input.tl.bottom) {
    foreach (v; border) if (input.subtrees[v/4].values[v%4]) return grow(input);
  } else {
    foreach (v; border) if (!input.subtrees[v/4].subtrees[v%4].isNull()) return grow(input);
  }
  return input;
}

subtree* rebuild(subtree* st) {
  if (!st) throw new Exception("Bad. ");
  if (st.bottom) return getSubtree(st.values);
  else return getSubtree(
    rebuild(st.subtrees[0]), rebuild(st.subtrees[1]),
    rebuild(st.subtrees[2]), rebuild(st.subtrees[3])
  );
}

Lock DeletionLock;
static this() { New(DeletionLock); }

void wipe(ref subtree* root) {
  logln("Wiping caches");
  foreach (dg; cleans) dg();
  auto backup = chirp; chirp = new CheapHeap(chirp.size);
  logln("Rebuild tree");
  root = rebuild(root);
  if (!root) {
    logln("Couldn't fit main tree into new heap. Cannot continue. ");
    exit(1);
  }
  logln("Load post-rebuild: ", chirp.load);
  DeletionLock.Synchronized = delete backup;
}

subtree* iterate(subtree* input, ref subtree* forble, int steps) {
  while (steps) {
    auto grown = grow_if_needed(input);
    // yes this circuitous back and forth is indeed, _necessary_.
    if (!grown) { forble = input; wipe(forble); input = forble; continue; }
    auto l = grown.level;
    // find closest power of two
    int i = 1, k = 0;
    while (i <= steps) { if (k == l) break; i *= 2; k ++; }
    i /= 2; k --;
    auto stepped = step(grown, k);
    if (!stepped) { forble = input; wipe(forble); input = forble; continue; }
    input = stepped;
    steps -= i;
  }
  return input;
}

bool isDigit(char c) { return c >= '0' && c <= '9'; }

extern(C) void exit(int);

import tools.rd;
void main(string[] args) {
  scope(failure) exit(0);
  auto exec = args[0]; args = args[1 .. $];
  tp = new Threadpool(3);
  string filename = "primes.wi";
  if (args.length) filename = args[0];
  if (args.length > 1) scale = args[1].atoi();
  auto wf = (cast(string) tools.compat.read(filename)).split("\n")[1 .. $];
  int maxlen;
  foreach (line; wf) if (line.length > maxlen) maxlen = line.length;
  auto temp = new ubyte[maxlen];
  ubyte[] array;
  foreach (i, line; wf) {
    foreach (k, ch; line) {
      switch (ch) {
        case ' ': temp[k] = 0; break;
        case '#': temp[k] = 1; break;
        case '@': temp[k] = 2; break;
        case '~': temp[k] = 3; break;
      }
    }
    foreach (ref value; temp[line.length .. $]) value = 0;
    array ~= temp;
  }
  auto test = buildSubtree(maxlen, wf.length, (int x, int y) { if (y>=wf.length || x >= maxlen) return cast(ubyte) 0; return array[y * maxlen + x]; });
  // screen(maxlen * scale, wf.length * scale);
  screen(640, 480);
  ulong gen, last; auto start = sec(), last_sec = sec();
  int stepsize = 256;
  tp.addTask({
    int sz;
    while (true) {
      volatile sz = stepsize;
      test = iterate(test, test, sz);
      gen += sz;
    }
  });
  int xoffs, yoffs, factor = 1;
  while (true) {
    if (key.pressed('+') || key.pressed(93)) { stepsize *= 2; logln("Stepsize now ", stepsize); }
    if (key.pressed('-') || key.pressed(47)) { if (stepsize > 1) stepsize /= 2; logln("Stepsize now ", stepsize); }
    auto xstep = screen.w * factor / 4, ystep = screen.h * factor / 4;
    if (key.pressed(276)) xoffs -= xstep;
    if (key.pressed(275)) xoffs += xstep;
    if (key.pressed(273)) yoffs -= ystep;
    if (key.pressed(274)) yoffs += ystep;
    if (key.pressed('y')) { factor *= 2; logln("Factor -> ", factor); }
    if (key.pressed('x')) { if (factor > 1) factor /= 2; logln("Factor -> ", factor); }
    scope(failure) logln("Render failure");
    auto diff = gen - last;
    if (diff > 0) {
      last = gen;
      auto s = sec(), sec_diff = s - last_sec;
      last_sec = s;
      logln(s - start, "s: generation ", gen, "; ", diff / sec_diff, " gen/s; ", gen / (s - start), " total gen/s");
      logln("subtree1 [", getSubtree1_cache.length, "] subtree2 [", getSubtree2_cache.length, "] getNulltree [",
        getNulltree_cache.length, "] step [", step_cache.length, "]");
    }
    // logln("Render ", screen.w * factor, "x", screen.h * factor, " at ", xoffs, ", ", yoffs);
    cls;
    DeletionLock.Synchronized = test.renderAt(xoffs, yoffs, screen.w * factor, screen.h * factor, (int x, int y, Cell cell) {
      rgb color;
      switch (cell) {
        case 0: color = Black; break;
        case 1: color = Yellow; break;
        case 2: color = Blue ~ White; break;
        case 3: color = Red; break;
      }
      if (factor == 1) pset(x-xoffs, y-yoffs, color);
      else pset((x-xoffs) / factor, (y-yoffs) / factor, color);
    });
    flip; events;
    // test.render; flip; events;
  }
}
