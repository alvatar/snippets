module tools.mersenne;

const N=624, M=397;
const MATRIX_A=0x9908b0dfUL, UPPER_MASK=0x80000000UL, LOWER_MASK=0x7fffffffUL;

import tools.base;
version(DS) { } else version(HotBits) {
import tools.downloader, tools.log;
  ubyte[] get_hotbits(int count) {
    ubyte[] res;
    while (res.length < count) {
      logln("Downloading random data from Hotbits");
      res ~= cast(ubyte[]) download(Format("http://www.fourmilab.ch/cgi-bin/Hotbits?nbytes=", count-res.length, "&fmt=bin"));
      logln("Now got ", res.length);
    }
    return res;
  }
}

final class Mersenne : IRandom {
  uint[N] mt;
  int mti=N;
  this() { seed(0); }
  this(uint s) { seed(s); }
  void seed(uint s) {
    mt[0]=s;
    foreach (id, elem; mt[0..$-1]) {
      mt[id+1] = cast(uint) (1812433253UL * (elem ^ (elem >> 30)) + id + 1);
      mt[id+1] &= 0xffffffffUL;
    }
  }
  static if (is(typeof(&get_hotbits))) void seed_hotbits() {
    //mt[0 .. 16] = cast(size_t[]) get_hotbits(64);
    init_by_array(cast(uint[]) get_hotbits(64));
  }
  void seedFrom(IRandom other) {
    uint[N] array;
    foreach (ref entry; array) entry = other.rand();
    init_by_array(array);
  }
  void init_by_array(uint[] key) {
    int i=1, j=0, k=(N>key.length ? N : key.length);
    seed(19650218UL);
    while (k--) {
      mt[i] = cast(uint) ((mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL)) + key[j] + j); /* non linear */
      mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++; j++;
      if (i>=N) { mt[0] = mt[N-1]; i=1; }
      if (j>=key.length) j=0;
    }
    k=N-1; while (k--) {
      mt[i] = cast(uint) ((mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL)) - i); /* non linear */
      mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++;
      if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }
    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
  }
  /* generates a random number on [0,0xffffffff]-interval */
  final uint rand() {
    uint y;
    static uint[2] mag01=[0x0UL, MATRIX_A];
    if (mti >= N) { /* generate N words at one time */
      //if (mti == N+1) seed(5489UL);
      for (int kk=0; kk<N-M; kk++) {
        y = cast(uint) ((mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK));
        mt[kk] = cast(uint) (mt[kk+M] ^ (y >> 1) ^ mag01[y & 1u]);
      }
      for (int kk=N-M; kk<N-1; kk++) {
        y = cast(uint) ((mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK));
        mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 1u];
      }
      y = cast(uint) ((mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK));
      mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 1u];
      mti = 0;
    }
    y = mt[mti++];
    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);
    return y;
  }
  alias rand opCall;
  final float randf() { return rand() / (1f * typeof(rand()).max); }
}

version(DS) {
  Mersenne deflt;
  static this() { New(deflt, 23); }
  uint rand() { return deflt(); }
} else {
  import tools.threads;
  private TLS!(Mersenne) deflt;
  uint glob_seed = 23; 
  
  void seed(uint k) {
    Mersenne gen_mers() { return new Mersenne(glob_seed + k); }
    deflt = new typeof(deflt)(&gen_mers);
  }
  int i;
  static this() {
    synchronized seed(i++);
  }
  uint rand() { return deflt()(); }
  float randf() { return rand() / (1f * typeof(rand()).max); }
}
