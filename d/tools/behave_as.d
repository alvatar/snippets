module tools.behave_as;
import tools.base, tools.compat;

template ForwardBinaryOperator(alias A, alias B, bool ASSIGN, string INS, string NAME, string OP, bool _R, S...) {
  const string AH = A!("#"), VALUE1 = AH~OP~"param", VALUE2 = "param"~OP~AH;
  const string INIT1 = AH~OP~"Init!(T)", INIT2 = "Init!(T)"~OP~AH;
  const string RES1 = (ASSIGN?B!("#", VALUE1):VALUE1), RES2 = (ASSIGN?B!("#", VALUE2):VALUE2);
  const string INITRES1 = (ASSIGN?B!("#", INIT1):INIT1), INITRES2 = (ASSIGN?B!("#", INIT2):INIT2);
  const string res = "
    typeof("~INITRES1~") "~NAME~"(T)(T param) {
      // pragma(msg, \""~NAME~" with \"~T.stringof);
      "~INS~"
      static assert(is(typeof("~RES1~")), \"Forwarded operator "~NAME~" not supported on #\");
      return "~RES1~";
    }
  "~(_R?("
    typeof("~INITRES2~") "~NAME~"_r(T)(T param) {
      pragma(msg, \""~NAME~"_r with \"~T.stringof);
      "~INS~"
      static assert(is(typeof("~RES2~")), \"Forwarded operator "~NAME~"_r not supported on #!\");
      return "~RES2~";
    }
  "):"");
  static if (S.length) const string op = ForwardBinaryOperator!(A, B, ASSIGN, INS, S).op ~ res;
  else const string op = res;
}

template PropertyRead(string S) { const string PropertyRead = S~"()"; }
template PropertyWrite(string S, string T) { const string PropertyWrite = S~"(cast(typeof("~S~"())) ("~T~"))"; }

template canEmulate(T) {
  const bool canEmulate = is(T: long) || is(T==ulong) || isArray!(T) || isAssocArray!(T);
}

template PropertyForward(string W, string INS = "") {
  const string PropertyForward=ctReplace("
    static if (is(typeof(-#()))) typeof(-#()) opNeg() { · return -#(); }
    static if (is(typeof(+#()))) typeof(+#()) opPos() { · return +#(); }
    static if (is(typeof(~#()))) typeof(~#()) opCom() { · return ~#(); }
    static if (is(typeof(#()+1))) typeof(#()) opPostInc() { · auto res = #(); "~PropertyWrite!("#", "res+1")~"; return res; }
    static if (is(typeof(#()-1))) typeof(#()) opPostDec() { · auto res = #(); "~PropertyWrite!("#", "res-1")~"; return res; }
    typeof(#()) opCast() { · return #(); }
    typeof(#()) opCall() { · return #(); }
    typeof(#(Init!(T))) opAssign(T)(T other) { · return #(other); }
    "~ForwardBinaryOperator!(PropertyRead, PropertyWrite, false, INS,
      "opAdd", "+", true, "opSub", "-", true, "opMul", "*", true, "opDiv", "/", true,
      "opMod", "%", true, "opAnd", "&", true, "opOr", "|", true, "opXor", "^", true,
      "opShl", "<<", true, "opShr", ">>", true, "opUShr", ">>>", true, "opCat", "~", true,
      "opIn", " in ", true
    ).op~ForwardBinaryOperator!(PropertyRead, PropertyWrite, true, INS,
      "opAddAssign", "+", false, "opSubAssign", "-", false,
      "opMulAssign", "*", false, "opDivAssign", "/", false, "opModAssign", "%", false,
      "opAndAssign", "&", false, "opOrAssign", "|", false, "opXorAssign", "^", false,
      "opShlAssign", "<<", false, "opShrAssign", ">>", false, "opUShrAssign", ">>>", false,
      "opCatAssign", "~", false
    ).op~"
    int opEquals(T)(T other) { · auto me = #(); return typeid(typeof(me)).equals(&me, &other); }
    int opCmp(T)(T other) { · auto me = #(); return typeid(typeof(me)).compare(&me, &other); }
    static if (is(typeof(#().length())))
      typeof(#().length()) length() { · return #().length(); }
    else static if (is(typeof(#().length)))
      typeof(#().length) length() { · return #().length; }
    static if (is(typeof(#().length = Init!(size_t))))
      typeof(#().length=Init!(size_t)) length(size_t i) { · return #().length = i; }
    typeof(#()[Init!(T)]) opIndex(T)(T index) { · return #()[index]; }
    typeof(function { typeof(#()) foo; return foo[Init!(U)] = Init!(T); }()) opIndexAssign(T, U)(T value, U index) {
      ·
      auto v = #(), res = v[index] = value;
      #(v); return res;
    }
    typeof(Init!(T) in #()) opIn(T)(T key) { · return key in #(); }
    typeof(Init!(typeof(#())).remove(Init!(T))) remove(T)(T t) {
      ·
      auto v = #();
      static if (is(typeof(v.remove(t)) == void)) { v.remove(t); #(v); }
      else { res = v.remove(t); #(v); return res; }
    }
    static if (is(ElemType!(typeof(#())))) {
      int opApply(int delegate(ref ElemType!(typeof(#()))) dg) {
        · auto v = #(); scope(exit) #(v);
        foreach (ref value; v) if (auto res = dg(value)) return res;
        return 0;
      }
      static if (is(KeyType!(typeof(#())))) {
        int opApply(int delegate(KeyType!(typeof(#())), ref ElemType!(typeof(#()))) dg) {
          · auto v = #(); scope(exit) #(v);
          foreach (key, ref value; v) if (auto res = dg(key, value)) return res;
          return 0;
        }
      }
    }
  ", "#", W, "·", INS);
}

struct Test {
  int e;
  static Test opCall(int e) { Test res; res.e = e; return res; }
  invariant { assert(e<100, "Range exceeded!"); }
  int e_access() { return e; } int e_access(int x) { return e = x; }
  mixin(PropertyForward!("e_access"));
}

struct UnionProperty(T) {
  T value;
  T property_access() { return value; }
  T property_access(T assign) { return value = assign; }
  mixin(PropertyForward!("property_access"));
}

class Test2 {
  union { private int f; UnionProperty!(int) f_pf; }
}

struct PointerProxy(T) {
  static assert(canEmulate!(T));
  T* ptr;
  T access(T i) { return (*ptr) = i; }
  T access() { return *ptr; }
  mixin(PropertyForward!("access"));
}

import tools.tests;
unittest {
  logln("behave_as tests");
  Test foo = 5;
  foo++;
  foo -= 4;
  mustEqual("BehaveAsEqualityTest", foo, 2);
  mustFail("RangedTest", foo += 100);
  auto x = new Test2; x.f = 5;
  x.f_pf++; x.f_pf -= 4;
  mustEqual("PropertyForwardTest", x.f, 2);
}
