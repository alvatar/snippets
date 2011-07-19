module dice;

import tools.base, tools.functional, tools.rd, tools.compat, tools.log;

File urandom; static this() { New(urandom, "/dev/urandom"); }
uint delegate() dice_rand_override;
uint rand() {
  if (dice_rand_override) return dice_rand_override();
  uint res;
  synchronized(urandom)
    version(Tango) urandom.read((cast(void*) &res)[0 .. 1]);
    else urandom.read(res);
  return res;
}

// TODO: use this
void SizeFail() {
  auto messages=["This isn't DBZ, kids."];
  throw new Exception("Out of bounds. "~messages[rand() % $]);
}

long SizeCheck(long l, long limit = 64*1024) {
  if (l > limit) SizeFail;
  else return l;
}

Result delegate() num(long l) { return l /apply/ (long l) { return Result(l, null); }; }

import tools.threads;
TLS!(void delegate()[]) reset_funcs;
static this() {
  New(reset_funcs, { return &(new Stuple!(void delegate()[]))._0; });
}

void reset() { foreach (entry; *reset_funcs.ptr()) entry(); }

class MemoizerSet(T, alias FN, EXTRA...) {
  struct Entry {
    bool matched;
    string output;
    T res;
  }
  Entry[ubyte[Stuple!(string, EXTRA).sizeof]] list; // to enforce bytewise comparison
  bool gotLookup(ref string st, out T res, EXTRA extra) {
    auto str = stuple(st, extra);
    ubyte[Stuple!(string, EXTRA).sizeof] key;
    key[] = (cast(ubyte*) &str)[0 .. typeof(str).sizeof];
    if (auto ptr = key in list) {
      if (!ptr.matched) return false;
      st = ptr.output;
      res = ptr.res;
      return true;
    }
    Entry newEntry;
    auto matched = FN(st, res, extra);
    newEntry.matched = matched;
    newEntry.output = st;
    newEntry.res = res;
    list[key] = newEntry;
    return matched;
  }
  void reset() { list = null; }
  this() { *reset_funcs() ~= &reset; }
}

template MemoizedStuff(T, alias FN, EXTRA...) {
  TLS!(MemoizerSet!(T, FN, EXTRA)) set;
  static this() { New(set, { return new MemoizerSet!(T, FN, EXTRA); }); }
}

bool memoized(T, alias FN, EXTRA...)(ref string st, out T res, EXTRA extra) {
  return MemoizedStuff!(T, FN, EXTRA).set().gotLookup(st, res, extra);
}

template memoize(string FNAME) {
  const string memoize = ctReplace("alias memoized!(Params!(typeof(&_·))[1], _·, Params!(typeof(&_·))[2..$]) ·; ", "·", FNAME);
}

bool _gotUInt(ref string st, out Result delegate() res) {
  bool isDigit(char ch) { return ch >= '0' && ch <= '9'; }
  if (mixin(gotMatchExpr("st: ($res<-gotResult$)"))) return true;
  if (!st.length || !isDigit(st[0])) return false;
  uint u;
  do {
    u = u * 10 + (st[0] - '0');
    st = st[1 .. $];
  } while (st.length && isDigit(st[0]));
  res = num(u);
  return true;
}
mixin(memoize!("gotUInt"));
// alias memoized!(Result delegate(), _gotUInt) gotUInt;

mixin(genOperator("then_do", "if (lhs) rhs(); return lhs; "));

mixin(genOperator("edit", "rhs(lhs); return lhs; "));

bool accept(ref string s, string what) {
  auto rest = s.strip().startsWith(what);
  if (!rest) return false;
  s = rest.strip();
  return true;
}

struct Die {
  int eyes;
  int value = -1;
  void roll() { value = rand() % eyes + 1; }
  static Die opCall(int eyes) {
    Die res;
    res.eyes = eyes;
    res.roll;
    return res;
  }
}

struct Roll {
  Die[] dice;
  string infotext;
  bool count;
  string genDiceList() {
    if (!dice.length) return null;
    string res = Format(dice[0].value);
    foreach (die; dice[1 .. $]) {
      if (die.value >= 0) res ~= Format(" + ", die.value);
      else res ~= Format(" - ", die.value);
    }
    if (dice.length == 1) return ""; // folded into sum
    return res;
  }
}

struct Result {
  long sum;
  string text;
  bool suppressNum;
  string info() {
    if (text)
      if (suppressNum) return Format(text);
      else return Format("(", text, ": ", sum, ")");
    else return Format(sum);
  }
}

bool _gotDee(ref string st, out Roll delegate() dg) {
  Result delegate() dice, eyes;
  return mixin(gotMatchExpr("st: [$dice$|$#dice=num(1)$]d$eyes$")) /then_do/ {
    dg = stuple(dice, eyes) /apply/ (typeof(dice) dice, typeof(eyes) eyes) {
      auto dv = dice(), ev = eyes();
      SizeCheck(dv.sum); SizeCheck(ev.sum);
      // logln(dv.sum, " dice being rolled.");
      if (dv.sum < 1 || ev.sum < 1)
        throw new Exception("Cut it out, asshole. It gets old. ");
      Roll roll;
      Range[dv.sum].each = { roll.dice ~= Die(ev.sum); };
      if (dv.sum > 1) roll.infotext = Format(dv.info, "d", ev.info);
      else roll.infotext = Format("d", ev.info);
      // logln("Done.");
      return roll;
    };
  };
}
mixin(memoize!("gotDee"));

int cmp(T)(T a, T b) { if (a < b) return -1; else if (a > b) return 1; else return 0; }

bool _gotKay(ref string st, out Roll delegate() dg) {
  Result delegate() keep;
  return mixin(gotMatchExpr("st: $dg<-gotRoll/0$k$keep$")) /then_do/ {
    dg = stuple(dg, keep) /apply/ (typeof(dg) dg, typeof(keep) keep) {
      auto roll = dg();
      auto sorted = roll.dice /qsort/ (Die d, Die e) { return cmp(d.value, e.value); };
      auto kv = keep();
      if (kv.sum > sorted.length) throw new Exception(Format("Cannot keep ", kv.sum, " dice: only rolled ", sorted.length));
      roll.dice = sorted[$-kv.sum .. $];
      roll.infotext ~= Format(", ", kv.info, " kept");
      return roll;
    };
  };
}
mixin(memoize!("gotKay"));

bool _gotReroll(ref string st, out Roll delegate() dg) {
  Result delegate() num;
  typeof(num)[] nums;
  bool add;
  return mixin(gotMatchExpr("st: $dg<-gotRoll/1$r[+$#add=true$]$num#nums~=num${,$num#nums~=num$}")) /then_do/ {
    dg = stuple(dg, nums, add) /apply/ (typeof(dg) dg, typeof(nums) nums, bool add) {
      auto roll = dg();
      int i = 0;
      auto rerolls = nums /map/ ex!("x -> x()"); int rerolled;
      with (roll) while (i < dice.length) {
rerun:
        foreach (rres; rerolls)
          if (dice[i].value == rres.sum) {
            if (add) {
              dice ~= Die(-1);
              dice[$-1] = dice[i];
              dice[$-1].roll;
            } else dice[i].roll();
            rerolled ++;
            goto rerun;
          }
        SizeCheck(i);
        i++;
      }
      roll.infotext = Format(roll.infotext, ", ", rerolled, " ", rerolls /map/ member!("info()"), "rerolled");
      return roll;
    };
  };
}
mixin(memoize!("gotReroll"));

bool _gotCount(ref string st, out Roll delegate() dg) {
  Result delegate() comparison;
  bool gr(int a, int b) { return b > a; }
  bool ls(int a, int b) { return b < a; }
  bool eq(int a, int b) { return b == a; }
  bool delegate(int, int)[] conds; string op;
  return mixin(gotMatchExpr("st: $dg<-gotRoll/3$count"
    "[<$#conds~=&ls$$#op~='<'$]"
    "[>$#conds~=&gr$$#op~='>'$]"
    "[=$#conds~=&eq$$#op~='='$]$comparison$")) /then_do/ {
    if (!conds.length) throw new Exception("No conditions given!");
    dg = stuple(dg, comparison, conds, op) /apply/ (typeof(dg) dg, typeof(comparison) comparison, typeof(conds) conds, string op) {
      auto roll = dg(), cv = comparison();
      typeof(roll) roll2;
      foreach (die; roll.dice) {
        bool meep = false;
        foreach (cdg; conds)
          meep |= cdg(cv.sum, die.value);
        if (meep) roll2.dice ~= die;
      }
      roll2.infotext = Format(roll.infotext, ", ", roll2.dice.length, (roll2.dice.length>1)?" dice ":" die ", op, " ", cv.info);
      roll2.count = true;
      return roll2;
    };
  };
}
mixin(memoize!("gotCount"));

bool _gotModify(ref string st, out Roll delegate() dg) {
  Result delegate() count; bool add, low;
  return mixin(gotMatchExpr("st: $dg<-gotRoll/2$[-|+$#add=true$][$count$|$#count=num(1)$][[H|h]|[L|l]$#low=true$]")) /then_do/ {
    dg = stuple(dg, count, add, low) /apply/ (typeof(dg) dg, typeof(count) count, bool add, bool low) {
      auto roll = dg();
      auto sorted = roll.dice /qsort/ (Die d, Die e) { return cmp(d.value, e.value); };
      auto cv = count();
      if (cv.sum > sorted.length) throw new Exception(Format("Cannot ", add?"add":"remove", " more dice than are in dice pool!"));
      if (!low) sorted = sorted.reverse;
      if (add) sorted ~= sorted[0 .. cv.sum];
      else sorted = sorted[cv.sum .. $];
      roll.dice = sorted;
      roll.infotext ~= Format(" ", add?"+":"-", " ", cv.info, low?"L":"H");
      return roll;
    };
  };
}
mixin(memoize!("gotModify"));

bool _gotRoll(ref string st, out Roll delegate() dg, int level = 4) {
  if (level) {
    if (level > 1) {
      if (level > 2) {
        if (level > 3) {
          if (st.gotCount(dg)) return true;
        }
        if (st.gotModify(dg)) return true;
      }
      if (st.gotReroll(dg)) return true;
    }
    if (st.gotKay(dg)) return true;
  }
  if (st.gotDee(dg)) return true;
  return false;
}
bool gotRoll(ref string st, out Roll delegate() dg, int level = 4) {
  return MemoizedStuff!(typeof(dg), _gotRoll, int).set().gotLookup(st, dg, level);
}

int sum(int[] i) { int res; foreach (v; i) res += v; return res; }
int count(int[] i) { return i.length; }

mixin(genOperator("call", "return rhs(lhs); "));

bool _gotEcks(ref string st, out Result delegate() res) {
  Result delegate() times;
  return mixin(gotMatchExpr("st: $times$[x|#]$res<-gotResultBase$")) /then_do/ {
    res = stuple(res, times) /apply/ (typeof(res) dg, typeof(times) times) {
      auto tv = times();
      Result r; 
      // r.text = Format(tv.info, "x [");
      r.text = "[";
      for (int i = 0; i < tv.sum; ++i) {
        auto sr = dg();
        /*auto foo = sr.info();
        recheck: if (auto bar = foo.startsWith("(").endsWith(")")) {
          foo = bar;
          goto recheck;
        }*/
        auto foo = Format(sr.sum); // x resets formatting
        r.sum += sr.sum;
        if (r.text.length < 200) {
          if (i) r.text ~= ", ";
          r.text ~= foo;
        }
      }
      if (r.text.length >= 200) r.text ~= "[...]";
      r.text ~= "]";
      r.suppressNum = true;
      return r;
    };
  };
}
mixin(memoize!("gotEcks"));

bool _gotResultBase(ref string st, out Result delegate() res) {
  Roll delegate() dg;
  if (gotRoll(st, dg)) {
    alias ex!("d -> d.value") values;
    res = dg /apply/ (typeof(dg) dg) {
      auto roll = dg();
      auto fn = &sum;
      if (roll.count) fn = &count;
      return Result(
        roll.dice /map/ values /call/ fn,
        // Format(roll.infotext, " ◟<u>", roll.genDiceList(), "</u>◞")
        Format(roll.infotext, " <u>", roll.genDiceList(), "</u>")
      );
    };
    return true;
  }
  if (st.gotEcks(res)) return true;
  return gotUInt(st, res);
}
mixin(memoize!("gotResultBase"));

long pow(long a, long b) {
  if (!a) return 0;
  if (a == 1) return 1;
  if (!b) return 1;
  long res = a;
  for (int i = 1; i < b; ++i) {
    auto nres = res * a;
    if (nres < res) throw new Exception("64-bit overflow error!");
    res = nres;
  }
  return res;
}

bool _gotMath(ref string st, out Result delegate() res, int level = 3) {
  Result delegate() op2;
  // pragma(msg, gotMatchExpr("st: $res<-gotMath/1$+[$op2<-gotMath/2$|$op2<-gotMath/1$]"));
  const string ARITH = `
    if (mixin(gotMatchExpr("st: $res<-gotMath/_LVL_-1$_MATCH_[$op2<-gotMath/_LVL_$|$op2<-gotMath/_LVL_-1$]"))) {
      res = stuple(res, op2) /apply/ (typeof(res) res, typeof(op2) op2) {
        auto v1 = res(), v2 = op2();
        if ("_MATCH_" == "/" && !v2.sum) throw new Exception("OH SHI-");
        return Result(
          mixin(ctReplace("_OPERATOR_", "x1", "v1.sum", "x2", "v2.sum")),
          v1.info ~ " _NAME_ " ~ v2.info
        );
      };
      return true;
    }`;
  switch (level) {
    case 3:
      mixin(ctReplace(ARITH, "_OPERATOR_", "x1+x2", "_NAME_", "+", "_MATCH_", "+", "_LVL_", "3"));
      mixin(ctReplace(ARITH, "_OPERATOR_", "x1-x2", "_NAME_", "-", "_MATCH_", "-", "_LVL_", "3"));
      return st.gotMath(res, 2); // keep this in .. no idea why
    case 2:
      mixin(ctReplace(ARITH, "_OPERATOR_", "x1*x2", "_NAME_", "x", "_MATCH_", "*", "_LVL_", "2"));
      // never invoked: higher directive overrides it
      mixin(ctReplace(ARITH, "_OPERATOR_", "x1*x2", "_NAME_", "x", "_MATCH_", "x", "_LVL_", "2"));
      mixin(ctReplace(ARITH, "_OPERATOR_", "x1/x2", "_NAME_", "/", "_MATCH_", "/", "_LVL_", "2"));
      return st.gotMath(res, 1);
    case 1:
      mixin(ctReplace(ARITH, "_OPERATOR_", "pow(x1, x2)", "_NAME_", "^", "_MATCH_", "^", "_LVL_", "1"));
    case 0: return st.gotResultBase(res);
  }
}
bool gotMath(ref string st, out Result delegate() res, int level = 3) {
  return MemoizedStuff!(typeof(res), _gotMath, int).set().gotLookup(st, res, level);
}

bool _gotAgainst(ref string st, out Result delegate() res) {
  Result delegate() against;
  bool smaller, bigger, equal;
  return mixin(gotMatchExpr("st: $res<-gotMath$[<$#smaller=true$][>$#bigger=true$][=$#equal=true$][$against<-gotMath$]")) /then_do/ {
    if (!smaller && !bigger && !equal) return; // only res
    else if (!against) throw new Exception("against what? ");
    auto cmp = stuple(smaller, bigger, equal) /apply/ (bool smaller, bool bigger, bool equal, int a, int b) {
      if (smaller) { if (a < b) return true; }
      if (bigger) { if (a > b) return true; }
      if (equal) { if (a == b) return true; }
      return false;
    };
    string ch;
    if (smaller) ch ~= "<";
    if (bigger) ch ~= ">";
    if (equal) ch ~= "=";
    res = stuple(res, against, cmp, ch) /apply/ (Result delegate() res, Result delegate() against, bool delegate(int, int) cmp, string str) {
      auto pre_res = res(), cmp_res = against();
      string result = cmp(pre_res.sum, cmp_res.sum)?"success":"failed";
      pre_res.text = Format(pre_res.info(), " ", str, " ", cmp_res.info(), ": ", result, "!");
      pre_res.suppressNum = true;
      return pre_res;
    };
  };
}
mixin(memoize!("gotAgainst"));

alias gotAgainst gotResult; // bottom

mixin Got!(Result delegate(), gotUInt);
