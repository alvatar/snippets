module tools.fractional;

import tools.compat;
int prime_gen(int which) {
  static int[] primes=[2];
  void addPrime(int count) {
    auto new_primes = primes;
    new_primes.length = new_primes.length + count;
    long test = primes[$-1];
    for (int i = 0; i < count; ++i) {
      while (true) {
        test ++;
        auto limit = sqrt(cast(real) test)+1;
        bool prime = true;
        foreach (num; new_primes[0 .. primes.length + i]) {
          if (num > limit) break;
          if ((test % num) == 0) { prime = false; break; }
        }
        if (!prime) continue; // look ma it's goto
        new_primes[primes.length + i] = test;
        break;
      }
    }
    primes = new_primes;
  }
  if (which !< primes.length) {
    synchronized {
      if (which !< primes.length) addPrime(which - primes.length + 1);
    }
  }
  return primes[which];
}

struct prime_factors {
  long number;
  int opApply(int delegate(ref int) dg) {
    auto work = number, prime_index = 0;
    while (work > 1) {
      auto np = prime_gen(prime_index);
      if ((work % np) == 0) {
        work /= np;
        if (auto res = dg(np)) return res;
      } else prime_index ++;
    }
    return 0;
  }
  // GOD how I WISH we had fibers in phobos.
  // then I wouldn't need to write this SHIT.
  struct Iterator {
    int work, prime_index;
    int opCall() {
      if (work !> 1) return 0;
      while (true) {
        auto np = prime_gen(prime_index);
        if ((work % np) == 0) {
          work /= np;
          return np;
        } else prime_index ++;
      }
    }
  }
  Iterator iterator() { Iterator res = void; res.work = number; res.prime_index = 0; return res; }
}

// there is probably a vastly superior way to this
// if you know it, please replace.
int lcd (int a, int b, out int fa, out int fb) {
  auto itera = prime_factors(a).iterator(), iterb = prime_factors(b).iterator();
  fa = 1; fb = 1;
  int pa = itera(), pb = iterb(), res = 1;
  while (true) { 
    if (!pa || !pb) break;
    if (pa == pb) { res *= pa; pa = itera(); pb = iterb(); }
    else if (pb < pa) { res *= pb; fa *= pb; pb = iterb(); }
    else if (pa < pb) { res *= pa; fb *= pa; pa = itera(); }
  }
  if (!pa && pb) while (pb) { res *= pb; fa *= pb; pb = iterb(); }
  if (!pb && pa) while (pa) { res *= pa; fb *= pa; pa = itera(); }
  return res;
}

import tools.base;
struct Fractional {
  int num, denom;
  static Fractional opCall(int a, int b) {
    Fractional res = void; res.num = a; res.denom = b; return res;
  }
  static Fractional opCall(int a) { return opCall(a, 1); }
  Fractional opAdd(Fractional other) {
    int f = void, f_other = void;
    auto new_denom = lcd(denom, other.denom, f, f_other);
    return Fractional(num * f + other.num * f_other, new_denom);
  }
  Fractional opSub(Fractional other) {
    int f = void, f_other = void;
    auto new_denom = lcd(denom, other.denom, f, f_other);
    return Fractional(num * f - other.num * f_other, new_denom);
  }
  static void divCommon(ref int a, ref int b) {
    foreach (factor; prime_factors(a)) {
      if ((b % factor) == 0) { a /= factor; b /= factor; }
    }
  }
  void cancel() { divCommon (num, denom); }
  Fractional cancelled() { Fractional res = *this; res.cancel; return res; }
  Fractional opMul(Fractional other) {
    divCommon (num, other.denom);
    divCommon (denom, other.num);
    return Fractional (num*other.num, denom*other.denom);
  }
  Fractional inv() { return Fractional(denom, num); }
  Fractional opDiv(Fractional other) { return opMul(other.inv); }
  Fractional opMul(int i) { return Fractional(num*i, denom); }
  Fractional opDiv(int i) { return Fractional(num, denom*i); }
  float toFloat() { return num * 1f / denom; }
  string toString() { return Format("[", num, "/", denom, "]"); }
}
