module python_hash;

import std.random, tools.base;
static import std.gc, std.c.stdlib;
bool dontScan;
void* my_calloc(size_t length) {
  auto res = (cast(ubyte*) std.c.stdlib.malloc(length))[0..length];
  res[] = 0;
  if (!dontScan) std.gc.addRange(res.ptr, res.ptr + length);
  return res.ptr;
}

void my_free(void* p) {
  if (!dontScan) std.gc.removeRange(p);
  std.c.stdlib.free(p);
}

pragma(msg, "This class was modified for hashing purposes and may drop entries!");

import tools.log;

// models a write-only cache.
final class PyDict(K, V) {
  private:
  //ulong, double etc. on 32bit
  struct BigPODWrapper(T)
  {
    union {
      T data;
      uint[(T.sizeof - 1) / 4 + 1] field;
    }
    size_t hash;
    
    void ctor()
    {
      static assert(T.sizeof >= size_t.sizeof);
      
      static if (is(typeof(data._0): void*) && is(typeof(data._1): void*) && is(typeof(data._2): void*) && is(typeof(data._3): void*)) {
        // presume 16-byte alignment
        hash = ((cast(size_t) data._0)+
                (cast(size_t) data._1)+
                (cast(size_t) data._2)+
                (cast(size_t) data._3)) >> 4;
      } else hash = typeid(T).getHash(&data);
      //will work for ulong with additional hash
      //hash = *cast(size_t*) &data + (cast(size_t*) &data)[1];
      //avoid special hashes
      if(isSpecialKey(*this)) hash += 2;
    }
    
    void markDummy() { hash = dummy_hash; }

    alias typeof(*this) TT;
    // static bool cmp1(TT a, TT b) { return a.data == b.data; }
    // static bool cmp2(TT a, TT b) { return false; }
    // static bool cmp3(TT a, TT b) { return a.data == b.data; }
    static bool cmp1(TT a, TT b) { for (int i = 0; i < field.length; ++i) if (a.field[i] != b.field[i]) return false; return true; }
    static bool cmp2(TT a, TT b) { return false; }
    static bool cmp3(TT a, TT b) { for (int i = 0; i < field.length; ++i) if (a.field[i] != b.field[i]) return false; return true; }
  }
  
  //byte, uint etc. on 32bit
  struct SmallPODWrapper(T)
  {
    union {
      T data;
      size_t hash;
    }

    void ctor() { static assert(T.sizeof <= size_t.sizeof); }

    void markDummy() { this.data = cast(T) dummy_hash; }

    alias typeof(*this) TT;
    static bool cmp1(TT a, TT b) { return a.data == b.data; }
    static bool cmp2(TT a, TT b) { return false; }
    static bool cmp3(TT a, TT b) { return a.data == b.data; }
  }
  
  template SelectKeyWrapper(K)
  {
    static if (isArray!(K)) static assert(false);
    else static if (isRefType!(K)) static assert(false);
    else static if (K.sizeof <= size_t.sizeof) alias SmallPODWrapper!(K) type;
    else static if (K.sizeof > size_t.sizeof) alias BigPODWrapper!(K) type;
    pragma(msg, "Selected ", type.stringof, " for ", K.stringof);
  }

  //key wrapper type
  alias SelectKeyWrapper!(K).type KW;

  //need to be 0 for the algorithm to terminate
  static const size_t unused_hash = 0; 
  static const size_t dummy_hash = 1;

  //need to be a power of two
  static const size_t MINSIZE = 8;
  static const size_t PERTURB_SHIFT = 5;

  struct Entry
  {
    KW key;
    V value;
  }

  //active + dummy entries
  size_t fill = 0;

  //active entries
  size_t used = 0;

  /*
  * The table contains mask + 1 slots, and that's a power of 2.
  * We store the mask instead of the size because the mask
  * is more frequently needed.
  */
  size_t mask = MINSIZE - 1;

  //table of size 2**n
  Entry* table = void;

  /*
  * Since this.table can't hold entries for both special keys,
  * they have to be stored and handled separately.
  */
  bool is_unused = false;
  KW unused_key = KW.init;
  V unused_value = V.init;

  bool is_dummy = false;
  KW dummy_key = KW.init;
  V dummy_value = V.init;

  public this()
  {
    // this.table = cast(Entry*) GC.calloc(Entry.sizeof * MINSIZE);
    // this.table = cast(Entry*) std.gc.malloc(Entry.sizeof * MINSIZE);
    this.table = cast(Entry*) my_calloc(Entry.sizeof * MINSIZE);
    /*const SIZE=1024*1024*4 / Entry.sizeof;
    dictresize(SIZE);*/
  }

  ~this()
  {
          delete this.table;
  }

  /*
  * Any key that is not special is active.
  */
  private static bool isActiveKey(KW key)
  {
          return (key.hash > 1);
  }

  private static bool isDummyKey(KW key)
  {
          return (key.hash == dummy_hash);
  }

  private static bool isUnusedKey(KW key)
  {
          return (key.hash == unused_hash);
  }

  private static bool isSpecialKey(KW key)
  {
          return (key.hash < 2);
  }

  /*
  * Lookup an entry in the table.
  * This is the workhorse.
  */
  private Entry* lookdict(KW key)
  {
    assert(!isSpecialKey(key));
    auto hash = key.hash, i = hash, ep = &table[i & mask];
    
    /*
    * This first lookup will succeed in the very most cases
    */
    if (isUnusedKey(ep.key) || KW.cmp1(ep.key, key)) return ep;
    
    Entry* freeslot = void;
    if (isDummyKey(ep.key)) freeslot = ep;
    else {
      if (KW.cmp2(ep.key, key)) return ep;
      freeslot = null;
    }
    
    /*
    * In the loop, key == dummy is by far (factor of 100s) the
    * least likely outcome, so test for that last.
    */
    for (size_t perturb = hash; ; perturb >>= PERTURB_SHIFT)
    {
      i = (i << 2) + i + perturb + 1;
      ep = &table[i & mask];
      
      if (isUnusedKey(ep.key)) return freeslot ? freeslot : ep;
      if (KW.cmp3(ep.key, key)) return ep;
      if (!freeslot && isDummyKey(ep.key)) freeslot = ep;
    }
    assert(0);      //never reached
  }
  
  public V* opIn_r(K k)
  {
    //wrap
    auto key = KW(k);
    key.ctor();
    
    if (isSpecialKey(key))
    {
      if (isUnusedKey(key))
      {
        return is_unused ? &unused_value : null;
      }
      else //must be dummy
      {
        assert(isDummyKey(key));
        return is_dummy ? &dummy_value : null;
      }
      assert(0);
    }
    
    Entry* ep = lookdict(key);
    assert(ep);
    
    if (isActiveKey(ep.key)) return &ep.value;
    else return null;
  }
  
  public void opIndexAssign(V value, K k)
  {
    assert(this.fill <= this.mask);  //algorithm need at least one empty slot
    
    //wrap
    auto key = KW(k);
    key.ctor();
    
    if (isSpecialKey(key))
    {
      if (isUnusedKey(key))
      {
        is_unused = true;
        unused_key = key;
        unused_value = value;
        return;
      }
      else //must be dummy
      {
        assert(isDummyKey(key));
        is_dummy = true;
        dummy_key = key;
        dummy_value = value;
        return;
      }
    }
    
    Entry* ep = lookdict(key);
    assert(ep);

    if (isActiveKey(ep.key)) ep.value = value; // this may overwrite, but so what
    else {
      if (isUnusedKey(ep.key)) this.fill++;
      else assert(isDummyKey(ep.key));
      ep.key = key;
      ep.value = value;
      this.used++;
      checkLoad();
    }
  }

  /*
  * Check load factor and allocate new table
  */
  private void checkLoad()
  {
    //Make table bigger if load factor > 3/4.
    //This can also result in smaller table if there are many dummy entries)
    // if (this.fill * 4 >= (this.mask + 1) * 3) //load factor is 3/4
    if (this.fill * 8 >= (this.mask + 1) * 7) //load factor is 7/8
    {
      const limit = 64*1024*1024;
      // dictresize(2 * this.used);
      if (this.used * Entry.sizeof < limit) dictresize(2 * this.used);
      else {
        logln("PURGE THE UNCLEAN");
        dictresize(limit / Entry.sizeof, false); // drop dict. This can only work because we use it as a cache.
      }
    }
    /*
    //make table smaller, table size > MINSIZE and load factor is < 1/8
    else if ((this.mask + 1) > MINSIZE && this.fill * 4 < (this.mask + 1))
    {
            dictresize(this.used / (this.used > 50000 ? 4 : 2));
    }*/
  }
  
  private void dictresize(size_t minused, bool copy = true)
  {
    // Find the smallest table size > minused and size == 2**n.
    size_t newsize = MINSIZE;
    while(newsize <= minused) newsize <<= 1;
    // Get space for a new table.
    Entry* oldtable = this.table;
    assert(oldtable !is null);
    
    logln("Resize to ", newsize, " * ", Entry.sizeof, " -> ", Entry.sizeof * newsize);
    auto newtable = oldtable;
    if (newsize != used || copy) newtable = cast(Entry*) my_calloc(Entry.sizeof * newsize);
    else (cast(int[])newtable[0..used])[] = 0;
    
    assert(newtable);
    // assert(newtable != oldtable);
    
    this.table = newtable;
    this.mask = newsize - 1;
    
    this.used = 0;
    size_t i = this.fill;
    this.fill = 0;
    
    //copy the data over; filter out dummies
    if (copy) for (Entry* ep = oldtable; i > 0; ep++)
    {
      if (isActiveKey(ep.key))
      {
        --i;
        insertdict_clean(ep.key, ep.value);
      }
      else if (isDummyKey(ep.key))
      {
        --i;
      }
    }
    // delete oldtable;
    if (oldtable != newtable) my_free(oldtable);
  }

  /*
  * Insert an item which is known to be absent from the dict. 
  * This routine also assumes that the dict contains no deleted entries.
  */
  private void insertdict_clean(KW key, V value)
  {
          assert(!isSpecialKey(key));

          size_t hash = key.hash;
          size_t perturb = void;
          size_t mask = this.mask;
          Entry *ep0 = this.table;

          size_t i = hash & mask;
          Entry* ep = &ep0[i];

          for (perturb = hash; !isUnusedKey(ep.key); perturb >>= PERTURB_SHIFT)
          {
                  i = (i << 2) + i + perturb + 1;
                  ep = &ep0[i & mask];
          }

          this.fill++;
          ep.key = key;
          ep.value = value;
          this.used++;
  }

  alias Unstatic!(K) K_;
  alias Unstatic!(V) V_;
  /*
  * Get number of active entries stored.
  */
  public size_t size() { return used + is_dummy + is_unused; }
}
