module tools.base;

public import tools.smart_import;

public import tools.ctfe, tools.compat, tools.text: Format, isArray, isPointer, isAssocArray;

alias void delegate() proc;
const bool off=false;

static if (size_t.sizeof==8)
  const bool gccasm=true;
else
  version(GNU) const bool gccasm = true;
  else const bool gccasm = false;

template Tuple(T...) { alias T Tuple; }
template Init(T) { const T Init; }

struct TupleWrapper(T...) { alias T Tuple; }

template GlobalVar(T...) {
  static assert(T.length == 1);
  typeof(T[0]) GlobalVar = T[0];
}

template Unstatic(T) { alias T Unstatic; }
template Unstatic(T: T[]) { alias T[] Unstatic; }

template TupleMap(alias T, U...) {
  static if (!U.length) alias Tuple!() TupleMap;
  else alias Tuple!(T!(U[0]), TupleMap!(T, U[1..$])) TupleMap;
}

template TupleJoin(string SEP, T...) {
  static if (T.length == 0) const string TupleJoin = "";
  else static if (T.length == 1) const string TupleJoin = T[0];
  else const string TupleJoin = T[0] ~ SEP ~ TupleJoin!(SEP, T[1..$]);
}

struct __isRef(C, size_t which) {
  alias ParameterTypeTuple!(C) pt;
  pt[0..which] pre;
  const pt[which] test;
  pt[which+1..$] post;
  const bool r = !is(typeof(C(pre, test, post)));
}

template _isRef(C, size_t which) { static if (__isRef!(C, which).r) const char _isRef='t'; else const char _isRef='f'; }

string refliteral(int len) {
  char[] res;
  for (int i=0; i<len; ++i) {
    res~="~_isRef!(C, "~ctToString(i)~")";
  }
  return "\"\""~res;
}

template isRef(C) { const string isRef=mixin(refliteral(Params!(C).length)); }

string refToParamList(string lead, string TUP, string refs) {
  string res;
  for (int i=0; i<refs.length; ++i) {
    if (refs[i]=='t') res~="ref ";
    res~=TUP~"["~ctToString(i)~"] param_"~ctToString(i);
    if (i<refs.length-1) res~=", ";
  }
  return res;
}

string refToValueList(string lead, string refs) {
  string res="";
  for (int i=0; i<refs.length; ++i) {
    if (i) res ~= ", ";
    else res ~= lead;
    res~="param_"~ctToString(i);
  }
  return res;
}

template Params(T) {
  static if (is(typeof(T.tupleof))) alias ParameterTypeTuple!(T.opCall) Params;
  else alias ParameterTypeTuple!(T) Params;
}

template Ret(T) {
  static if (is(typeof(Init!(T).tupleof))) alias ReturnType!(typeof(&Init!(T).opCall)) Ret;
  else alias ReturnType!(T) Ret;
}

template StupleMembers(T...) {
  static if (T.length) {
    const int id=T[0..$-1].length;
    const string str=StupleMembers!(T[0..$-1]).str~"Unstatic!(T["~id.stringof~"]) _"~id.stringof~"; ";
  } else const string str="";
}

struct Stuple(T...) {
  alias Tuple!() StupleMarker;
  mixin(StupleMembers!(T).str);
  static if (T.length) { alias _0 x; alias _0 r; }
  static if (T.length>1) { alias _1 y; alias _1 g; }
  static if (T.length>2) { alias _2 z; alias _2 b; }
  static if (T.length>3) { alias _3 w; alias _3 a; }
  string toString() {
    static if (T.length == 2) {
      return Format(_0, ": ", _1);
    } else {
      string res="stuple(";
      foreach (id, entry; this.tupleof) {
        if (id) res ~= ", ";
        res ~= Format(entry);
      }
      return res~")";
    }
  }
}

template PointersTo(T...) {
  static if (!T.length) alias Tuple!() PointersTo;
  else alias Tuple!(T[0]*, PointersTo!(T[1..$])) PointersTo;
}

struct PTuple(T...) {
  PointersTo!(TupleMap!(Unstatic, T)) ptr;
  template Elem(T, int I) {
    static if (isArray!(T)) typeof(Init!(T)[0]) Elem(T t) { return t[I]; }
    else typeof(Init!(T).tupleof[I]) Elem(T t) { return t.tupleof[I]; }
  }
  U opAssign(U)(U other) {
    const ErrMesg = "Cannot assign "~U.stringof~" to "~T.stringof~": invalid count members";
    static if (isArray!(U)) assert(ptr.length == other.length, ErrMesg);
    else static assert(ptr.length == U.tupleof.length, ErrMesg);
    foreach (i, p; ptr) {
      static assert(is(typeof(Elem!(U, i)(other)): typeof(*p)), "Cannot assign "~U.stringof~" to "~T.stringof~": cannot assign "~
        typeof(Elem!(U, i)(other)).stringof~" to "~typeof(*p).stringof);
      *p = Elem!(U, i)(other);
    }
    return other;
  }
}

PTuple!(T) ptuple(T...)(ref T whee) {
  PTuple!(T) res = void;
  foreach (i, bogus; whee) res.ptr[i] = &whee[i];
  return res;
}

Stuple!(TupleMap!(Unstatic, T)) stuple(T...)(T t) { return Stuple!(TupleMap!(Unstatic, T))(t); }
struct orderTuple(T...) {
  static Stuple!(T)[] opCall(U...)(U u) {
    static assert((U.length%T.length)==0, "Cannot divide "~U.stringof~" into "~T.stringof~" parts");
    auto res=new Stuple!(T)[U.length/T.length];
    foreach (id, entry; u) {
      res[id/T.length].values[id%T.length]=entry;
    }
    return res;
  }
}

template isIterable(T) {
  static if (is(typeof(&Init!(T).opApply) == delegate)) const bool isIterable=true;
  else const bool isIterable=isArray!(T);
}

// DMD bug 2243. Please simplify when fixed.
template _isRefType(T) { const _isRefType = is(typeof(function { T foo; foo = null; })); }
template isRefType(T) { const bool isRefType = _isRefType!(T); }

template _ElemType(T) {
  // default to char
  static if (is(typeof((function() { foreach (string elem; Init!(T)) return elem; assert(false); })()))) {
    alias string type;
  } else {
    alias typeof((function() { foreach (elem; Init!(T)) return elem; assert(false); })()) type;
  }
}

template ElemType(T) {
  alias _ElemType!(T).type ElemType;
}

template KeyType(T) {
  alias typeof((function() { foreach (key, elem; Init!(T)) return key; assert(false); })()) KeyType;
}

void swap(T)(ref T a, ref T b) { T c=a; a=b; b=c; }

template Pair(T) { alias Tuple!(T, T) Pair; }
template Triple(T) { alias Tuple!(T, T, T) Triple; }
template ArrayOrVoid(T) { static if (is(T==void)) alias void ArrayOrVoid; else alias T[] ArrayOrVoid; }

template Split(string SOURCE, string WHERE, string BUFFER="") {
  static if (SOURCE.length<WHERE.length) alias Tuple!(BUFFER~SOURCE) Split;
  else static if (SOURCE[0..WHERE.length]==WHERE) alias Tuple!(BUFFER, Split!(SOURCE[WHERE.length..$], WHERE)) Split;
  else alias Split!(SOURCE[1..$], WHERE, BUFFER~SOURCE[0]) Split;
}

template FindOr(char[] SRC, char[] WHAT, size_t OR, bool REV = false) {
  static if(SRC.length<WHAT.length) const size_t FindOr=OR; else
  static if (REV) {
    static if (SRC[$-WHAT.length .. $] == WHAT) const size_t FindOr = SRC.length-WHAT.length; else
    static if (FindOr!(SRC[0..$-1], WHAT, OR, true) == OR) const size_t FindOr = OR; else
    const size_t FindOr = FindOr!(SRC[0..$-1], WHAT, OR, true);
  } else {
    static if(SRC[0..WHAT.length]==WHAT) const size_t FindOr=0; else
    static if(FindOr!(SRC[1..$], WHAT, OR)==OR) const size_t FindOr=OR; else
    const size_t FindOr=FindOr!(SRC[1..$], WHAT, OR)+1;
  }
}

// reverse insert at WHERE or start
template RInsert(char[] SRC, char[] WHERE, char[] WITH) {
  static if(SRC.length<WHERE.length) const char[] RInsert=WITH~SRC; else
  static if(SRC[$-WHERE.length..$]==WHERE) const char[] RInsert=SRC~WITH; else
  const char[] RInsert=RInsert!(SRC[0..$-1], WHERE, WITH)~SRC[$-1];
}

class AxiomaticException : Exception { this(char[] msg) { super("!!!!Basic axiom violated: "~msg); } }

void removeFrom(T, U)(ref T[] array, U index) {
  if (index!<array.length) throw new Exception("remove: Index "~toString(index)~" not in [0.."~toString(array.length)~"] !");
  // this also ensures array length is >0
  array[index]=array[$-1];
  array=array[0..$-1];
}

T[] toArray(T)(T *ptr) {
  int pos=0; while (ptr[pos]) pos++;
  return ptr[0..pos];
}

T *toPointer(T)(T[] array) {
  return (array~cast(T)0).ptr;
}

T* has(T, U)(T[] array, U what) {
  foreach (ref elem; array) if (elem == what) return &elem;
}

struct arguments {
  char[] executable;
  char[][char[]] flags;
  private void nows(ref string s) { /// No whitespace
    while (s.length&&(s[0]==' ')) s=s[1..$];
    while (s.length&&(s[$-1]==' ')) s=s[0..$-1];
  }
  bool has(char[] o) { if (o in flags) return true; return false; }
  string* opIn_r(string s) {
    if (!has(s)) return null;
    return s in flags;
  }
  char[] opIndex(char[] o) { return flags[o]; }
  void opIndexAssign(char[] v, char[] o) { flags[o]=v; }
  bool empty() { return flags.length==0; }
  char[] need(char[] o) {
    if (o in flags) return flags[o]; else throw new Exception("Missing argument: "~o);
  }
  static arguments opCall(string[] args) {
    arguments res; res.gen(args); return res;
  }
  void gen(char[][] args) {
    executable=args[0]; args=args[1..$];
    foreach (arg; args) {
      nows(arg);
      if (arg[0]!='-') throw new Exception("Not an argument: "~arg~" .. forgot the \"=\"?"); // "
      while (arg.length&&(arg[0]=='-')) arg=arg[1..$];
      if (find(arg, "=")==-1) arg~="=";
      auto pair=split(arg, "="); flags[tolower(pair[0])]=join(pair[1..$], "=");
    }
  }
}

T[] startsWith(T, U)(T[] array, U match) {
  static if (is(typeof(match.values))) {
    foreach (value; match.values) if (auto rest = array.startsWith(value)) return rest;
    return null;
  } else {
    Unstatic!(U) da = match;
    if (!array || !da) return null;
    if (array.length<da.length) return null;
    if (array[0..da.length]!=da) return null;
    return array[da.length..$];
  }
}
alias startsWith beginsWith;

T[] endsWith(T, U)(T[] array, U match) {
  static if (is(typeof(match.values))) {
    foreach (value; match.values) if (auto rest = array.endsWith(value)) return rest;
    return null;
  } else {
    Unstatic!(U) da = match;
    if (!array || !da) return null;
    if (array.length<da.length) return null;
    if (array[$-da.length..$]!=da) return null;
    return array[0..$-da.length];
  }
}

version(DS) { void slowyield() { } } else {
  version(Windows) {
    version(Tango) {
      extern(System) void Sleep(int i);
    } else {
      import std.c.windows.windows: Sleep;
    }
  } else {
    extern(System) void usleep(uint us);
  }
  void slowyield() { version(Windows) Sleep(10); else usleep(10000); }
  void fastyield() { Thread.yield; }
}

// broken in llvm
// void function() yield=&slowyield;
void function() _yield;
struct yield {
  static void opCall() { if (!_yield) _yield = &slowyield; _yield(); }
  static void opAssign(void function() fn) { _yield = fn; }
}

void remove(T)(ref T[] array, T key, bool all=true) {
  size_t pos=size_t.max; foreach (i, e; array) if (e==key) { pos=i; break; }
  if (pos==size_t.max) return;
  if (pos!=array.length-1) array[pos]=array[$-1];
  array=array[0..$-1];
  if (all) remove(array, key); /// tail recursion, theoretically
}

const KB=1024; const MB=KB*1024; const GB=MB*1024;

T[0] max(T...)(T t) {
  auto res = t[0];
  foreach (s; t[1..$]) if (s>=res) res = s;
  return res;
}
T[0] min(T...)(T t) {
  auto res = t[0];
  foreach (s; t[1..$]) if (s<=res) res = s;
  return res;
}

T concat(T)(T a, T b) { return a~b; }
T add(T)(T a, T b) { return a+b; }
T subtract(T)(T a, T b) { return a-b; }
struct _op(string O) {
  typeof(mixin("Init!(T) "~O~" Init!(U)")) opCall(T, U)(T a, U b) {
    return mixin("a "~O~" b");
  }
}
template op(string O) { const _op!(O) op; }
struct _member(string NAME) { typeof(mixin("Init!(T)."~NAME)) opCall(T)(T t) { return mixin("t."~NAME); } }
template member(string NAME) { _member!(NAME) member; }

template Atoi(string A, int EMPTY=0) { static if (!A.length) const int Atoi = EMPTY; else const int Atoi = mixin(A); }

template Count(string S, string P) {
  static if (FindOr!(S, P, -1) == -1) const int Count = 0;
  else const int Count = 1 + Count!(S[FindOr!(S, P, -1)+P.length .. $], P);
}

template Trim(string S) {
  static if (!S.length) const string Trim = "";
  else static if (S[0] == ' ') const string Trim = Trim!(S[1..$]);
  else static if (S[$-1] == ' ') const string Trim = Trim!(S[0..$-1]);
  else const string Trim = S;
}

template isCharacter(char c) {
  const bool isCharacter = ((c>='a' && c<='z') || (c>='A' && c<='Z'));
}

template CarefulReplace(string S, string WHAT, string WITH, int OFFS = 0) {
  const _Pos = FindOr!(S[OFFS .. $], WHAT, -1);
  static if (_Pos == -1) const string repl = S[OFFS .. $];
  else {
    const Pos = _Pos + OFFS;
    static if (Pos == 0) const string repl = WITH ~ CarefulReplace!(S, WHAT, WITH, WHAT.length).repl;
    else static if (Pos + WHAT.length == S.length) { // match is at very end of string
      static if (S[Pos-1] == '.') const string repl = S[OFFS .. $]; // no changes
      else const string repl = CarefulReplace!(S[0 .. $-WHAT.length], WHAT, WITH, OFFS).repl ~ WITH;
    } else {
      static if (!isCharacter!(S[Pos - 1]) && !isCharacter!(S[Pos + WHAT.length]) && S[Pos-1] != '.')
        const string repl = S[OFFS .. Pos] ~ WITH ~ CarefulReplace!(S, WHAT, WITH, Pos + WHAT.length).repl;
      // else skip the replace
      else const string repl = S[OFFS .. Pos+WHAT.length] ~ CarefulReplace!(S, WHAT, WITH, Pos + WHAT.length).repl;
    }
  }
}

static assert(CarefulReplace!("foobar b", "b", "xx").repl == "foobar xx");
static assert(CarefulReplace!("a.foobaa()", "a", "#").repl == "#.foobaa()");

// V is the array/tuple used to store values,
// with % in V being the index
// Every loop, replace % with the last variable's index,
// substitute the result for S[$-1], and recurse.
template ParamSubstitute(string EX, string V, S...) {
  static if (!S.length)
    const string ParamSubstitute = EX;
  else const string ParamSubstitute = ParamSubstitute!(
    CarefulReplace!(
      EX,
      Trim!(S[$-1]),
      ctReplace(V, "%", ctToString(S.length-1))
    ).repl,
    V,
    S[0..$-1]
  );
}

const string ExSource = "
  struct _ex(string EX, T...) {
    T Ω; // unlikely to come up as a user expression
    static assert(FindOr!(EX, \"->\", -1) != -1, \"EX \"~EX~\" incomplete!\");
    const string
      Params = EX[0 .. FindOr!(EX, \"->\", -1)],
      Rest = EX[FindOr!(EX, \"->\", -1)+2 ..$];
    static if (Count!(EX, \"->\") == 1) {
      typeof(mixin(ParamSubstitute!(
        Rest,
        \"Init!(Stuple!(U)).tupleof[%]\",
        Split!(Params, \",\")
      ))) opCall(U...)(U u) {
        static assert(U.length == Split!(Params, \",\").length, \"Cannot call \"~Params~\" in ex with \"~U.stringof);
        return mixin(ParamSubstitute!(
          Rest,
          \"u[%]\",
          Split!(Params, \",\")
        ));
      }
    } else {
      const string ReplacedEx = ParamSubstitute!(
        Rest,
        \"Ω[\"~ctToString(T.length)~\"+%]\",
        Split!(Params, \",\")
      );
      _ex!(ReplacedEx, T, U) opCall(U...)(U u) {
        _ex!(ReplacedEx, T, U) res = void;
        // pragma(msg, \"Res type is \", ReplacedEx, \" and \", T.stringof, \" and \", U.stringof);
        foreach (i, var; Ω) res.Ω[i] = var;
        static assert(is(typeof(u) == U), \"Yay Compiler Bug\");
        foreach (i, v; u) res.Ω[T.length + i] = v;
        return res;
      }
    }
  }
  
  template ex(string EX) { const _ex!(EX) ex; }";

mixin(ExSource);

size_t reflookup(T)(T[] array, T elem) {
  foreach (idx, entry; array)
    if (entry is elem) return idx;
  return size_t.max;
}

extern(C) void abort();

void fail(string why="") {
  logln("Failure"~(why.length?": "~why:""));
  //version (BigEndian) {
  //int* foo; foo--; (*foo)++; // trigger a sigsegv
  abort();
  //} else asm { int 3; }
}

void dbg_break() {
  version(DS) assert(false, "Debug break triggered");
  else asm { int 3; }
}

void New(S, T...)(ref S inst, T t) { inst=new S(t); }

string genOperator(string name, string code, bool leftOnly = false, string op = "opDiv") {
  string res;
  if (!leftOnly) res ~= "
        struct _OP__$NAME(LHS) {
          LHS lhs;
          typeof(function() { LHS lhs; RHS rhs; $CODE }()) $OP(RHS)(RHS rhs) { $CODE }
          alias $OP opCall;
        }
    ";
  res ~= "struct _OP_$NAME {\n";
  if (leftOnly) res ~= "
    typeof(function() { Unstatic!(LHS) lhs; $CODE }()) $OP_r(LHS)(LHS lhs) { $CODE }
    alias $OP_r opCall;
  "; else res ~= "
    _OP__$NAME!(Unstatic!(LHS)) $OP_r(LHS)(LHS lhs) {
      _OP__$NAME!(Unstatic!(LHS)) res=void; res.lhs = lhs; return res;
    }
    alias $OP_r opCall;
  ";
  res ~= "
    }
    /*const .. not needed*/_OP_$NAME $NAME;
  ";
  return res.ctReplace("$NAME", name, "$CODE", code, "$OP", op);
}

string genParamOperator(string name, string code, bool leftOnly = false, string op = "opDiv") {
  string res;
  if (!leftOnly) res ~= "
    struct __$NAME(LHS, PAR...) {
      PAR params;
      LHS lhs;
      typeof(function() { LHS lhs; RHS rhs; $CODE }()) $OP(RHS)(RHS rhs) { $CODE }
    }
  ";
  res ~= "
    struct _$NAME(PAR...) {
      PAR params;
  ";
  if (leftOnly) res ~= "
    typeof(function() { LHS lhs; $CODE }()) $OP_r(LHS)(LHS lhs) { $CODE }
  "; else res ~= "
    __$NAME!(LHS, PAR) $OP_r(LHS)(LHS lhs) { return __$NAME!(LHS, PAR)(params, lhs); }
  ";
  res ~= "
    }
    struct $NAME { static _$NAME!(PAR) opCall(PAR...)(PAR p) { return _$NAME!(PAR)(p); } }
  ";
  return res.ctReplace("$NAME", name, "$CODE", code, "$OP", op);
}

template DualOperator(string NAME, string PAR_CODE, string NOPAR_CODE, bool LEFTONLY=false, string OP="opDiv") {
  const string DualOperator=(LEFTONLY
    ?""
    :"struct _par_"~NAME~"(LHS, PAR...) {
        PAR params;
        LHS lhs;
        typeof(function() { LHS lhs; RHS rhs; "~PAR_CODE~" }()) "~OP~"(RHS)(RHS rhs) { "~PAR_CODE~" }
      }"
    )~"
    struct par_"~NAME~"(PAR...) {
      PAR params;"~
      (LEFTONLY
        ?"typeof(function() { LHS lhs; "~PAR_CODE~" }()) "~OP~"_r(LHS)(LHS lhs) { "~PAR_CODE~" }"
        :"_par_"~NAME~"!(LHS, PAR) "~OP~"_r(LHS)(LHS lhs) { return _par_"~NAME~"!(LHS, PAR)(params, lhs); }"
      )~"
    }"~(LEFTONLY
      ?""
      :"
        struct nopar_"~NAME~"(LHS) {
          LHS lhs;
          typeof(function() { LHS lhs; RHS rhs; "~NOPAR_CODE~" }()) "~OP~"(RHS)(RHS rhs) { "~NOPAR_CODE~" }
        }"
    )~"
    struct "~NAME~" {
      static par_"~NAME~"!(PAR) opCall(PAR...)(PAR p) { return par_"~NAME~"!(PAR)(p); }
      "~(LEFTONLY
        ?"static typeof(function() { LHS lhs; "~NOPAR_CODE~" }()) "~OP~"_r(LHS)(LHS lhs) { "~NOPAR_CODE~" }"
        :"static nopar_"~NAME~"!(LHS) "~OP~"_r(LHS)(LHS lhs) { return nopar_"~NAME~"!(LHS)(lhs); }"
      )~"}";
}

mixin(genParamOperator("times", "
  LHS[] res;
  static assert(PAR.length==1 && is(PAR[0]: int), \"Invalid parameters to times\");
  while (params[0]--) res~=lhs;
  return res;
", true));

template ExpandPointers(FN, string V, P, int OFFS=0) {
  // pragma(msg, "ExpandPointers("~FN.stringof~", "~V~", "~P.stringof~", "~OFFS.stringof~") ");
  alias typeof(Init!(P).tupleof) tuple;
  static if (is(Params!(FN))) {
    alias Params!(FN) params;
    const bool hasParamTypes = true;
  } else const bool hasParamTypes = false;
  static if (OFFS !< tuple.length) {
    const string code = "";
    alias Tuple!() type;
  } else {
    alias ExpandPointers!(FN, V, P, OFFS+1) next;
    const string DerefType="*tuple[OFFS]";
    static if (hasParamTypes) {
      static assert(OFFS < params.length, "Too many parameters for "~FN.stringof~": cannot expand "~P.stringof~" at "~OFFS.stringof~" considering "~params.stringof~"!");
    }
    static if (hasParamTypes && is(typeof(mixin(DerefType))) && is(typeof(mixin(DerefType)): params[OFFS])) {
      const string _code = "*("~V~".tupleof["~ctToString(OFFS)~"])";
      alias typeof(mixin(DerefType)) _type;
    } else {
      const string _code = V~".tupleof["~ctToString(OFFS)~"]";
      alias tuple[OFFS] _type;
    }
    static if (next.code.length) const string code = _code ~ ", " ~ next.code;
    else const string code = _code;
    alias Tuple!(_type, next.type) type;
  }
}

// what to do when calling a U with a T, its name V
template CallConfig(T, U, string V = "") {
  // can we even take its parameter types?
  // pragma(msg, "What to do when calling "~U.stringof~" with "~T.stringof~" named \""~V~"\"");
  static if (is(Params!(U))) const bool hasParamTypes = true;
  else const bool hasParamTypes = false;
  
  // always(!) expand
  static if (is(T.StupleMarker)) {
    const string value = ExpandPointers!(U, V, T).code;
    alias ExpandPointers!(U, V, T).type type;
    const int length = type.length;
  } else static if (hasParamTypes && is(typeof(*Init!(T))) && is(typeof(*Init!(T)): Params!(U)[0])) { // if U can be called with dereferenced T
    const string value = "*"~V;
    alias typeof(*Init!(T)) type;
    const int length = 1;
  } /*else static if (hasParamTypes && is(typeof(function(){ T t; return t(); }())) && is(typeof(Init!(T)()): Params!(U)[0])) { // if U can be called with calling T
    const string value = V~"()";
    alias typeof(Init!(T)()) type;
    const int length = 1;
  } */else {
    const string value = V;
    alias T type;
    const int length = 1;
  }
}

template Value(T, U, string V) { const string Value=CallConfig!(T, U, V).value; }

template Type(T, U) { alias CallConfig!(T, U).type Type; }

template Length(T, U) { const int Length=CallConfig!(T, U).length; }

struct fix_late_holder(LHS, RHS) {
  LHS lhs; RHS rhs;
  typeof(lhs(rhs, Stuple!(X).tupleof)) opCall(X...)(X par) {
    return lhs(rhs, par);
  }
}

mixin(genOperator("fix", "
  // pragma(msg, \"fix: \"~LHS.stringof~\" -> \"~RHS.stringof);
  struct holder {
    LHS fix_lhs; Unstatic!(RHS) fix_rhs;
    mixin(\"Ret!(LHS) call(\"~refToParamList(\"\", \"Params!(LHS)[Length!(RHS, LHS)..$]\", isRef!(LHS)[Length!(RHS, LHS)..$])~\") {
      return mixin(\\\"fix_lhs(\\\"~Value!(RHS, Ret!(LHS) function(Params!(LHS)[0..Length!(RHS, LHS)]), \\\"fix_rhs\\\")~\\\"
        \"~refToValueList((Length!(RHS, LHS) && (Params!(LHS).length-Length!(RHS, LHS)))?\", \":\"\", isRef!(LHS)[Length!(RHS, LHS)..$])~\")\\\");
    }\");
  }
  static if(!is(Params!(LHS))) {
    // static assert(false, \"fix: \"~LHS.stringof~\" is not callable!\");
    fix_late_holder!(LHS, RHS) res = void;
    res.lhs = lhs; res.rhs = rhs; return res;
  } else {
    auto res=new holder; res.fix_lhs=lhs; res.fix_rhs=rhs;
    return &res.call;
  }
"));

mixin(genOperator("apply", " return rhs /fix/ lhs; "));

template PartialCallString(string NAME, C, int apply=1) {
  static if (Params!(C).length<=apply) {
    const string str="";
  } else {
    const string str="
      _partial!(Ret!(typeof("~NAME~")) delegate(Params!(typeof("~NAME~"))[0.."~apply.stringof~"]))
        opCall(Params!(typeof("~NAME~"))[0.."~apply.stringof~"] params) {
          return partial("~NAME~" /fix/ stuple(params));
      }
    "~PartialCallString!(NAME, C, apply+1).str;
  }
}

struct _partial(C) {
  C callable;
  Ret!(C) opCall(Params!(C) params) { return callable(params); }
  mixin(PartialCallString!("callable", C).str);
}

_partial!(C) partial(C)(C callable) { _partial!(C) res; res.callable=callable; return res; }

struct chain_holder(LHS, RHS) {
  LHS _lhs; RHS _rhs;
  typeof(function() { X foo; return _rhs(_lhs(foo)); }()) opCall(X...)(X par) {
    return _rhs(_lhs(par));
  }
}
mixin(genOperator("chain", " chain_holder!(LHS, RHS) res = void; res._lhs = lhs; res._rhs = rhs; return res; "));

template Reverse(T...) {
  static if(T.length) alias Tuple!(Reverse!(T[1..$]), T[0]) Reverse;
  else alias Tuple!() Reverse;
}

template ReverseIndices(string BASE, int LEN) {
  static if(!LEN) const string ReverseIndices=""; else
  static if(LEN==1) const string ReverseIndices=BASE~"[0]"; else
  const string ReverseIndices=BASE~"["~(LEN-1).stringof~"], "~ReverseIndices!(BASE, LEN-1);
}

struct reverse_holder(LHS) {
  LHS lhs;
  typeof(lhs(Stuple!(Reverse!(X)).tupleof)) opCall(X...)(X par) {
    return mixin("lhs("~ReverseIndices!("par", X.length)~")");
  }
}

mixin(genOperator("reverse", "
  static if(isIterable!(LHS)) {
    auto res=new ElemType!(LHS)[lhs.length];
    foreach (id, ref entry; res) entry=lhs[lhs.length-id-1];
    return res;
  } else {
    // reverse parameter order
    static if (is(Params!(LHS))) {
      static typeof(mixin(\"lhs(\"~ReverseIndices!(\"Stuple!(Reverse!(Params!(LHS))).tupleof\", Params!(LHS).length)~\")\"))
        res_fn(typeof(lhs) lhs, Reverse!(Params!(LHS)) rev) {
          return mixin(\"lhs(\"~ReverseIndices!(\"rev\", Params!(LHS).length)~\")\");
      }
      return lhs /apply/ &res_fn;
    } else {
      reverse_holder!(LHS) res = void;
      res.lhs = lhs;
      return res;
    }
  }", true
));

mixin(genOperator("rfix", "pragma(msg, LHS.stringof, \" - \", RHS.stringof); return lhs /reverse /fix/ rhs /reverse; "));

Stuple!(string, string) tsplit(string what, string sep) {
  if (what.find(sep)==-1) throw new Exception("No \""~sep~"\" in \""~what~"\"!");
  return stuple(what[0..what.find(sep)], what[what.find(sep)+1..$]);
}

string cutOff(string what, string where) {
  if (what.length < where.length) return what;
  for (int i = 0; i < what.length - where.length; ++i) {
    if (what[i .. i+where.length] == where) return what[0 .. i];
  }
  return what;
}

T take(T)(ref T[] array) {
  if (!array.length) throw new Exception("Cannot take from empty array!");
  auto res=array[0]; array=array[1..$]; return res;
}
T pop(T)(ref T[] array) {
  if (!array.length) throw new Exception("Cannot pop from empty array!");
  auto res = array[$-1]; array = array[0..$-1]; return res;
}
T[] take(T, U)(ref T[] array, U count) {
  if (array.length < count) throw new Exception(Format("Array not big enough to take from: ", count, " from ", array));
  auto res=array[0..count];
  array=array[count..$];
  return res;
}
T[] peek(T, U)(T[] array, U count) { if (count > array.length) count = array.length; return array.take(count); }
T peek(T)(T[] array) { if (!array.length) throw new Exception("Cannot peek into empty array!"); return array[0]; }

template rmSpace(string s, bool command=false) {
  static if (!s.length) const string rmSpace=""; else
  static if (command) {
    static if (s[0]==';') const string rmSpace=';'~rmSpace!(s[1..$]);
    else const string rmSpace=s[0]~rmSpace!(s[1..$], true);
  } else
  static if (s[0]=='#') const string rmSpace='#'~rmSpace!(s[1..$], true); else
  static if (s[0]==' ' || s[0]=='\r' || s[0]=='\n' || s[0]=='\t') const string rmSpace=rmSpace!(s[1..$]); else
  const string rmSpace=s[0]~rmSpace!(s[1..$]);
}

string cut_off(string what, string where) {
  if (what.length < where.length) return what;
  for (int i = 0; i < what.length-where.length; ++i) {
    if (what[i .. i+where.length] == where) return what[0 .. i];
  }
  return what;
}

string This_fn(string s) {
  string buffer;
  bool sup; // currently parsing a super call
  bool command; // currently parsing a #; command
  string res_assign, res_header;
  foreach (ch; s) {
    if (command) {
      if (ch == ';') {
        res_assign ~= buffer~"; ";
        buffer="";
        command=sup=false;
      } else buffer ~= ch; // read until ;
    } else {
      if (ch == '(') {
        assert(buffer=="super", "Invalid code "~buffer);
        res_assign ~= buffer~"(";
        buffer="";
        sup = true;
        continue;
      }
      bool clean_buffer;
      // list of closing elements
      if (ch == ')' || ch == ',' || ch == ';' || ch == '#') {
        if (buffer.length) {
          if (res_header.length) res_header ~= ", ";
          res_header ~= "typeof("~cut_off(buffer, "=")~") _"~buffer;
          clean_buffer = true; // we still need the buffer for later
        }
      }
      if (ch == '#') { // literal command mode
        assert(!sup, "# invalid in super!");
        if (buffer.length) { // flush
          res_assign ~=
            "this."~cut_off(buffer, "=")
            ~" = _"~cut_off(buffer, "=")~"; ";
          buffer="";
        }
        command = true;
      } else if (ch == ')') { // super done
        assert(sup, "Parenthesis mismatch!");
        if (buffer.length)
          res_assign ~= "_"~cut_off(buffer, "=");
          res_assign ~= "); ";
        buffer = "";
        sup = false;
      } else if (ch == ',' || ch == ';') { // assign closing elements
        if (buffer.length) {
          if (sup) res_assign ~=
            "_"~cut_off(buffer, "=")~", ";
          else res_assign ~=
            "this."~cut_off(buffer, "=")
            ~" = _"~cut_off(buffer, "=")~"; ";
        }
        buffer = "";
      } else buffer ~= ch;
      if (clean_buffer) buffer = "";
    }
  }
  // terminal flush
  assert(!command, "Unterminated command literal!");
  if (buffer.length) {
    res_assign ~= "this."~cut_off(buffer, "=")~" = _"~cut_off(buffer, "=")~"; ";
    if (res_header.length) res_header ~= ", ";
    res_header ~= "typeof("~cut_off(buffer, "=")~") _"~buffer;
    buffer = "";
  }
  return "this("~res_header ~ ") { " ~ res_assign~" }";
}

template This(string T) {
  mixin(This_fn(rmSpace!(T)));
}

void runState(T)(void delegate(ref T)[T] states, T *external, proc yield) {
  T st;
  auto stp=&st;
  if (external) stp=external;
  while (true) {
    auto oldstate=*stp;
    if (!(*stp in states)) {
      logln("Invalid state reached: ", *stp, " not in ", states, "!");
      dbg_break;
    }
    states[*stp](*stp);
    if (oldstate!=*stp) continue;
    yield();
  }
}
void runState(T, Foo=void)(void delegate(ref T)[T] states, proc yield) { runState!(T)(states, null, yield); }
void runState(T, Foo=void, Bar=void)(void delegate(ref T)[T] states, ref T external, proc yield) { runState(states, &external, yield); }
void addEdge(T)(ref void delegate(ref T)[T] states, bool delegate() on, T from, T to) {
  if (from in states) {
    states[from]=(typeof(on) _on, T _to, void delegate(ref T) old, ref T state) {
      T oldstate = state;
      old(state); // try this first
      if (oldstate == state) if (_on()) state=_to; // if unchanged, try condition
    } /fix/ stuple(on, to, states[from]);
  } else
    states[from]=(typeof(on) _on, T _to, ref T state) { if (_on()) state = _to; } /fix/ stuple(on, to);
}
void addEdges(T, U...)(ref void delegate(ref T)[T] states, U u) {
  states.addEdge(u[0..3]);
  static if (U.length>3) states.addEdges(u[3..$]);
}
void addBidirectionalEdge(T, U...)(ref void delegate(ref T)[T] states, bool delegate() on, U rest) {
  static if (U.length==2) {
    states.addEdge(on, rest[0], rest[1]);
    states.addEdge((bool delegate() _on) { return !_on(); } /fix/ on, rest[1], rest[0]);
  } else static if (U.length==3) {
    states.addEdge(on, rest[1], rest[2]);
    states.addEdge(rest[0], rest[2], rest[1]);
  } else static assert(false);
}
void addBidirectionalEdges(T, U...)(ref void delegate(ref T)[T] states, U u) {
  static assert(U.length>=3);
  static if (is(typeof(u[1]())==bool)) {
    static assert(U.length>=4);
    states.addBidirectionalEdge(u[0..4]);
    static if (U.length>3) states.addBidirectionalEdges(u[4..$]);
  } else {
    states.addBidirectionalEdge(u[0..3]);
    static if (U.length>3) states.addBidirectionalEdges(u[3..$]);
  }
}

template IF (bool cond, T...) {
  static assert(T.length == 2);
  static if (T.length == 2) {
    static if (is(T[0]) && is(T[1])) /* are types */ {
      static if(cond) alias T[0] IF; else alias T[1] IF;
    } else {
      static if(cond) const IF = T[0]; else const IF = T[1];
    }
  } else static if (T.length == 1) {
    static if (is(T[0])) /* is type */ {
      static if(cond) alias T[0] IF; else alias Tuple!() IF;
    } else {
      static if(cond) const IF = T[0]; else static assert(false, "No alternate case for IF!(bool, "~T.stringof~")");
    }
  } else static if (T.length == 0) {
    alias Tuple!() IF;
  } else static assert(false, "Too many parameters for IF: "~T.stringof);
}

Stuple!(string, string) splitAt(string source, string sep) {
  auto pos = source.find(sep);
  if (pos == -1) return stuple(source, "");
  return stuple(source[0..pos], source[pos+sep.length..$]);
}

struct _Range_each(T) {
  T from, to;
  void opAssign(U...)(void delegate(U) dg) {
    static if (U.length) static assert(U.length == 1 && is(T: U[0]));
    T iter = from;
    while (iter < to) { static if (U.length) dg(iter); else dg(); iter++; }
  }
}

struct _Range(T) {
  union {
    struct { T from, to; }
    _Range_each!(T) each;
  }
  bool _startIncl = true, _endExcl = true;
  _Range switchMode(string P, string V, bool INC)() {
    auto res = *this;
    if (mixin((INC?"!":"")~"res."~P)) {
      mixin("res."~P~" = "~(INC?"true":"false")~"; ");
      mixin("res."~V~" "~(INC?"--":"++")~"; ");
    }
    return res;
  }
  alias switchMode!("_startIncl", "from", true) startIncl;
  alias switchMode!("_startIncl", "from", false) startExcl;
  alias switchMode!("_endExcl", "to", false) endIncl;
  alias switchMode!("_endExcl", "to", true) endExcl;
  static if (is(typeof(to-from): size_t))
    size_t length() { return to-from; }
  T opIndex(size_t where) { return from+cast(T) where; }
  int opApply(int delegate(ref T) dg) {
    T iter=from;
    while (iter<to) if (auto res=dg(iter)) return res; else ++iter;
    return 0;
  }
  int opApplyReverse(int delegate(ref T) dg) {
    T iter = cast(T) (to-1);
    while (iter >= from) if (auto res=dg(iter)) return res; else --iter;
    return 0;
  }
  void opAssign(C)(lazy C c) {
    T iter=from; while (iter<to) {
      static if (is(C==void)) c();
      else static if (is(C==void delegate())) c()();
      else static if (is(C==void delegate(T))) c()(iter);
      else static assert(false, C.stringof);
      ++iter;
    }
  }
  static _Range opCall(T from, T to) {
    _Range res; res.from = from; res.to = to; return res;
  }
  alias T IterType;
  bool opIn_r(T what) { return (what < to) && (what !< from); }
  string toString() {
    string res = "[";
    foreach (num; *this)
      if (res.length == 1) res ~= Format(num);
      else res ~= Format(", ", num);
    return res~"]";
  }
}

struct Range {
  static _Range!(T) opSlice(T, U)(T from, U to) { return _Range!(T)(from, to); }
  static _Range!(T) opIndex(T)(T to) { return _Range!(T)(Init!(T), to); }
  static void opIndexAssign(T, U)(lazy U whut, T to) { (opIndex(to))=whut; }
}

import tools.tests;
unittest {
  auto fn=(int a, int b) { return a+b; };
  mustEqual("PartialTest", partial(fn)(1)(2), partial(fn)(1, 2), 3);
  assert(5 in Range[0..10]);
  auto put = (int a, ref int b) { b = a; };
  int x; put(5, x);
  int y; (put /fix/ 5)(y);
  mustEqual("RefFixTest", x, y, 5);
  alias ex!("a -> b -> a+b") add;
  alias ex!("a, b -> c -> a+b-c") add2;
  mustEqual("ExAddTest", add(2)(3), 5, add2(2, 7)(4));
  int w; auto foo = function(ref int i) { i++; };
  toDg(foo)(w);
  mustEqual("toDgTest", w, 1);
}

T delegate(T) Loop(T)(int count, T delegate(T) dg) {
  assert(count>0);
  if (count==1) return dg; else {
    auto foo = Loop(count-1, dg) /chain/ dg;
    return foo /apply/ (typeof(foo) foo, T t) { return foo(t); };
  }
}

T delegate(T) tee(T)(void delegate(T) dg) { return (typeof(dg) _dg, T t) { _dg(t); return t; } /fix/ dg; }

T[] field(T)(size_t count, lazy T generate) {
  assert(!is(T==void));
  // avoid array initialization to default values (that's why it's not new T[count])
  auto res=(cast(T*)(new void[count*T.sizeof]).ptr)[0..count];
  assert(res.length==count, "Sanity failed");
  foreach (ref v; res) v=generate();
  return res;
}

struct _eval {
  template RetType(T) {
    static if (isArray!(T)) alias ArrayOrVoid!(RetType!(ElemType!(T))) RetType;
    static if (is(typeof(Init!(T)()))) alias typeof(Init!(T)()) RetType;
    else alias T RetType;
  }
  RetType!(T) opCall(T)(T t) {
    static if (isArray!(T)) {
      static if (is(typeof(opCall(Init!(ElemType!(T)))) == void))
        foreach (elem; t) opCall(elem);
      else {
        auto res = new ElemType!(T)[t.length];
        foreach (i, ref elem; res) elem = opCall(t[i]);
        return res;
      }
    } else static if (is(typeof(t()))) return t();
    else return t;
  }
}
_eval eval;

struct _BoolSet(string OP, T...) {
  alias OP BoolSetOp;
  TupleMap!(Unstatic, T) values;
  int opEquals(U)(U other) {
    auto res = other == values[0];
    foreach (value; values[1..$])
      res = mixin("res "~OP~" (other == value)");
    return res;
  }
  bool opIn_r(U)(U other) {
    bool res = !!(other in values[0]);
    foreach (value; values[1 .. $])
      res = mixin("res "~OP~" !!(other in value)");
    return res;
  }
  typeof(values[0] in Init!(U)) opIn(U)(U other) {
    auto res = values[0] in other;
    if (res) return res;
    foreach (value; values[1 .. $]) {
      res = value in other;
      if (res) return res;
    }
    return res;
  }
}

template BoolSet(string OP) {
  _BoolSet!(OP, T) BoolSet(T...)(T t) {
    _BoolSet!(OP, T) res = void;
    foreach (i, v; t)
      static if (is(Unstatic!(typeof(v)) == string))
        res.values[i] = v.dup;
      else res.values[i] = v;
    return res;
  }
}

template isStaticArray(T) {
  static if (isArray!(T)) const bool isStaticArray = !is(T == typeof(T[0])[]);
  else const bool isStaticArray = false;
}

alias BoolSet!("||") OrSet; alias BoolSet!("&&") AndSet;
mixin(genOperator("or", "return OrSet(lhs, rhs); "));
mixin(genOperator("and", "return AndSet(lhs, rhs); "));

template IfBranch(T...) {
  static if (T.length == 1) const string IfBranch = T[0];
  else const string IfBranch = "if ("~T[0]~") {
"~ctReplace(IfBranch!(T[4..$]), T[1], T[2])~"
} else {
"~ctReplace(IfBranch!(T[4..$]), T[1], T[3])~"
}";
}

T[][Unstatic!(U)] group(T, C, U)(T[] array, C callable, U startkey) {
  Unstatic!(U) key = startkey;
  T[][Unstatic!(U)] res;
  foreach (entry; array) {
    static if (isRefType!(T)) {
      if (auto nkey = callable(entry)) {
        key = nkey;
        res[key] = Init!(T[]);
      } else res[key] ~= entry;
    } else {
      if (callable(entry)) {
        key = entry;
        res[key] = Init!(T[]);
      } else res[key] ~= entry;
    }
  }
  return res;
}

typeof(Init!(C)(Init!(T).values[0]))[typeof(Init!(T).keys[0])] assocMap(T, C)(T array, C callable) {
  typeof(callable(array.values[0]))[typeof(array.keys[0])] res;
  foreach (key, value; array) res[key] = callable(value);
  return res;
}

struct YWrapper(C) {
  C callable;
  alias Ret!(C) R;
  mixin("R opCall("~
    refToParamList("", "Params!(C)[1 .. $]", isRef!(C)[1 .. $])~
    ") { return callable(&opCall"~
    refToValueList(",", isRef!(C)[1 .. $])~
    "); }"
  );
}

YWrapper!(C) selfcall(C)(C callable) {
  YWrapper!(C) res;
  res.callable = callable;
  return res;
}

S[][T] invert(S, T)(T[S] aa) {
  S[][T] res;
  foreach (key, value; aa) {
    if (value /notin/ res) res[value] = [key].dup;
    else res[value] ~= key;
  }
  return res;
}

struct multicall_chain(T) {
  T callable;
  multicall_chain opOr(U)(U what) {
    callable(what);
    return *this;
  }
}

mixin(genOperator("multicall", "return multicall_chain!(LHS)(lhs); ", true));

import tools.log: logln;
unittest {
  logln("base tests");
  mustEqual("ChainTest", ((int e) { return e*2; } /chain/ (int e) { return e/2; })(5), 5);
  mustEqual("LoopSquares", Loop(4, (int e) { return e*e; })(2), 65536);
  int bar = 0;
  Range[5].each={ bar++; };
  mustEqual("ParamLessForeach", bar, 5);
  int i = 4;
  void test(int g, ref int f) { f += g; }
  (&test /fix/ 2)(i);
  mustEqual("FixPreservesRefnessTest", i, 6);
  int a = 0; float b = 2f;
  ptuple(a, b) = stuple(2, 5f);
  mustEqual("PtupleBasicTest", a==2, b==5f, true);
  mustEqual("ExTest", ex!("a, b, c -> d -> a(b, c) == d")((int a, int b) { return a == b; }, 2, 3)(false), true);
  mustEqual("BoolSetTestOr", 2 /or/ 3 == 3, true);
  mustEqual("BoolSetTestAnd", AndSet(2, 3) == 3, false);
  mustEqual("RangeInclTest", 5 in Range[1..3].endIncl /or/ Range[2..5].endIncl, true);
  int mc_test = 4;
  (int i) { mc_test += i; } /multicall |5 |6UL |0x7;
  mustEqual("MulticallTest", mc_test, 22);
}

template Repeat(T, int count) {
  static if (!count) alias Tuple!() Repeat;
  else static if (count == 1) alias Tuple!(T) Repeat;
  else static if ((count%2) == 1) alias Tuple!(Repeat!(T, count/2), Repeat!(T, count/2), T) Repeat;
  else alias Tuple!(Repeat!(T, count/2), Repeat!(T, count/2)) Repeat;
}

template FnTypeToDgType(C) { alias typeof(Init!(C) /fix/ stuple()) FnTypeToDgType; }

// thanks h3r3tic
FnTypeToDgType!(C) toDg(C)(C fn) {
  struct Hack {
    mixin("Ret!(C) call("~refToParamList("", "Params!(C)", isRef!(C))~") {
      auto str_func = cast(C) cast(void*) this;
      return str_func("~refToValueList("", isRef!(C))~");
    }");
  }
  Hack hack;
  auto res = &hack.call;
  res.ptr = fn;
  return res;
}

string between(string text, string from, string to, bool adhere_right = false) {
  int pos1, pos2;
  if (adhere_right) {
    if (to.length) pos2 = text.find(to);
    else pos2 = text.length;
    if (pos2 == -1) return null;
    
    if (from.length) pos1 = text[0 .. pos2].rfind(from);
    else pos1 = 0;
    if (pos1 == -1) return null;
    
    return text[pos1 + from.length .. pos2];
    
  } else {
    if (from.length) pos1 = text.find(from);
    if (pos1 == -1) return null;
    text = text[pos1 + from.length .. $];
    
    if (to.length) pos2 = text.find(to);
    else pos2 = text.length;
    if (pos2 == -1) return null;
    
    return text[0 .. pos2];
  }
}

const GLOMP_RIGHT = true;
string[] betweens(string text, string from, string to, bool adhere_right = false) {
  string[] res;
  while (true) {
    int pos2;
    if (adhere_right) {
      pos2 = text.find(to); if (pos2 == -1) break;
      auto pos1 = text[0 .. pos2].rfind(from); if (pos1 != -1) {
        res ~= text[pos1 + from.length .. pos2];
      }
    } else {
      auto pos1 = text.find(from); if (pos1 == -1) break;
      text = text[pos1 + from.length .. $];
      pos2 = text.find(to); if (pos2 == -1) break;
      res ~= text[0 .. pos2];
    }
    text = text[pos2 + to.length .. $];
  }
  return res;
}

string between(string text, string from, string over, string to) {
  foreach (attempt; text.betweens(from, to)) {
    if (attempt.find(over) != -1) return attempt;
  }
  return null;
}

void glomp_parse(string text, void delegate(string pre, ref string post)[string] words, void delegate(string) rest) {
  while (true) {
    void delegate(string, ref string) dg; int min; string match;
    min = text.length;
    foreach (key, value; words) {
      auto pos = text.find(key);
      if (pos == -1) continue;
      if (pos < min) {
        min = pos;
        dg = value;
        match = key.dup;
      }
    }
    if (dg) {
      auto pre = text[0 .. min];
      // logln(text.length, " :: ", min + match.length, " => ", text.length - (min + 2));
      text = text[min + cast(int) match.length .. $];
      // logln("Found at ", min, ", pre ", pre, ", text ", text);
      dg(pre, text);
    } else {
      rest(text);
      break;
    }
  }
}

string SwitchCaseFn(string code) {
  int arrowpos = ctFind(code, "->");
  assert(arrowpos != -1);
  auto value = code[0 .. arrowpos];
  string Case;
  if (ctFind(value, "default") == -1) Case = "case ";
  return Case~value~": "~code[arrowpos+2 .. $]~"; break; ";
}

mixin(genOperator("notin", `
  static if (is(typeof({ return lhs in rhs; })))
    return !(lhs in rhs);
  else static if (is(typeof({ foreach (key, value; rhs) return key == lhs; })))
    foreach (key, value; rhs) if (key == lhs) return false;
  else static if (is(typeof({ foreach (value; rhs) return value == lhs; })))
    foreach (value; rhs) if (value == lhs) return false;
  else {
    static assert(false, "Cannot iterate "~RHS.stringof~", comparing against "~LHS.stringof);
  }
  return true;
`));

string Switch(string var, string code) {
  string res = "switch("~var~") { ";
  string buffer;
  foreach (ch; code) {
    if (ch == '|') {
      res ~= SwitchCaseFn(buffer);
      buffer = "";
    } else buffer ~= ch;
  }
  res ~= SwitchCaseFn(buffer);
  return res ~ "}";
}

// pragma(msg, Switch("1->a|2->b"));

T Cast(S, T)(S orig) { return cast(T) orig; }
Unstatic!(T) castLike(S, T)(S orig, T target_sample) { return cast(Unstatic!(T)) orig; }

interface IRandom {
  void seed(uint s);
  uint rand();
}

string toHex(uint u) {
  if (!u) return "0";
  string res;
  while (u) { res = "0123456789ABCDEF"[u%16] ~ res; u /= 16; }
  return res;
}

template mustBe(T) {
  T mustBe(U)(U what) {
    auto res = cast(T) what;
    if (!res) {
      static if (is(typeof(T.label) == string)) const string st = T.label;
      else const string st = T.stringof;
      asm { int 3; }
      throw new Exception("mustBe!("~T.stringof~"): expected "~st~", got "~Format(cast(Object) what));
    }
    return res;
  }
}

class NestedException : Exception {
  Exception nested;
  this(string s, Exception ex) { nested = ex; super(s~":"~ex.msg); }
  static void wrap(string mesg, lazy void action) {
    try action(); catch (Exception ex) { throw new NestedException(mesg, ex); }
  }
}

// This removes ".."!
version(DS) { } else {
  string sub(string a, string b, bool safe = false) {
    if (safe)
      if (b == ".." || b.startsWith("../") || b.endsWith("/..") || b.find("/../") != -1)
        throw new Exception("Security problem: attempt to exploit .sub to reach higher folder");
    if (auto pre = a.endsWith(sep)) a = a[0 .. $ - sep.length];
    while (true) {
      if (auto rest = b.startsWith("..")) {
        if (a.find(sep) == -1) {
          if (a == "." || a == "..") break;
          else a = ".";
        }
        else if (a[a.rfind(sep) + 1 .. $] == "..") break;
        else a = a[0 .. a.rfind(sep)];
        b = rest;
        if (auto n = b.startsWith("/")) b = n;
      } else break;
    }
    if (a == ".") a = null;
    if (!a.length || !b.length || a.endsWith(sep)) return a ~ b; else return a ~ sep ~ b;
  }
}

mixin(genOperator("todg", "return lhs /fix/ stuple(); ", true));

string _slice(bool REV)(ref string what, string where, bool cutLast = true) {
  static if (REV) auto loc = what.rfind(where);
  else auto loc = what.find(where);
  if (loc == -1) {
    if (cutLast) {
      auto res = what;
      what = null;
      return res;
    } else return null;
  }
  auto res = what[0 .. loc];
  what = what[loc+where.length .. $];
  return res;
}

alias _slice!(false) slice;
alias _slice!(true) rslice;

template AddType(T...) {
  static if (T.length == 1) alias T[0] AddType;
  else alias typeof(Init!(T[0]) + Init!(AddType!(T[1 .. $]))) AddType;
}

AddType!(T) delegate(AddType!(T)) translate(T...)(T stuff) {
  static assert(T.length == 4, "translate(from_src, to_src, from_dest, to_dest) does not match "~T.stringof~"!");
  auto from_src = stuff[0], to_src = stuff[1],
    from_dest = stuff[2], to_dest = stuff[3];
  return stuple(from_src, from_dest, (to_dest - from_dest) / (to_src - from_src)) /apply/
    (T[0] offs_src, T[2] offs_dest, AddType!(T) scale, AddType!(T) value) {
      return cast(AddType!(T)) (value - offs_src) * scale + offs_dest;
    };
}

template makeString(T...) {
  static assert(T.length == 1);
  static if (is(typeof(T[0]): string)) const string makeString = T[0];
  else static if (is(typeof(T[0]): int)) const string makeString = ctToString(T[0]);
  else static assert(false, "Cannot make string: "~T.stringof);
}

template ReplaceReform(string S, T...) {
  static if (!T.length) const string ReplaceReform = S;
  else
    const string ReplaceReform = ReplaceReform!(ctReplace(S, T[0], makeString!(T[$/2])), T[1 .. $/2], T[$/2+1 .. $]);
}

template ReplaceConcat(int DEG, string BASE, T...) {
  static if (T.length == DEG) const string ReplaceConcat = "";
  else
    const string ReplaceConcat = ReplaceReform!(BASE, T[0..DEG*2]) ~ ReplaceConcat!(DEG, BASE, T[0 .. DEG], T[DEG*2 .. $]);
}

template CountOff(int OFFS, T...) {
  static if (!T.length) alias Tuple!() CountOff;
  else alias Tuple!(T[0], ctToString(OFFS), CountOff!(OFFS+1, T[1 .. $])) CountOff;
}

mixin(genOperator("then_do", "if (lhs) rhs(); return lhs; "));

string fancyEnum(string code) {
  string spec = code.ctSlice(":").ctStrip();
  
  string type = spec.ctSlice(" ");
  if (!spec.length) { spec = type; type = "int"; } // no type spec
  
  string res = "struct "~spec~" { "~type~" num; alias num value; ";
  string nameswitch = "switch(num) { ";
  string iterate1, iterate2, iterate3, iterate4;
  int curval;
  while (code.length) {
    string entry = code.ctSlice(",");
    string base = entry.ctSlice("=").ctStrip();
    if (entry.length) curval = ctAtoi(entry[1 .. $]);
    
    res ~= "static "~spec~" "~base~" = { cast("~type~") "~ctToString(curval)~" }; ";
    nameswitch ~= "case cast("~type~") "~ctToString(curval)~": return \""~base~"\"; ";
    iterate1 ~= "{
      string temp = \""~base~"\"; auto temp2 = cast("~type~") "~ctToString(curval)~";
      if (auto res = dg(temp, temp2)) return res;
    }";
    iterate2 ~= "{
      auto temp = cast("~type~") "~ctToString(curval)~";
      if (auto res = dg(temp)) return res;
    }";
    iterate3 ~= "{
      string temp = \""~base~"\"; auto temp2 = "~base~";
      if (auto res = dg(temp, temp2)) return res;
    }";
    iterate4 ~= "{
      auto temp = "~base~";
      if (auto res = dg(temp)) return res;
    }";
    curval ++;
  }
  nameswitch ~= "default: return tools.base.Format(\"Invalid "~spec~": \", num); }";
  res ~= "string toString() { "~nameswitch~" }";
  res ~= type~" opCall() { return num; }";
  res ~= "
    static int opApply(int delegate(ref string, ref "~type~") dg) {
      " ~ iterate1 ~ "
      return 0;
    }
    static int opApply(int delegate(ref "~type~") dg) {
      " ~ iterate2 ~ "
      return 0;
    }
    static int opApply(int delegate(ref string, ref typeof(*this)) dg) {
      " ~ iterate3 ~ "
      return 0;
    }
    static int opApply(int delegate(ref typeof(*this)) dg) {
      " ~ iterate4 ~ "
      return 0;
    }";
  return res ~ "}";
}

string fancyBitfield(string spec) {
  string toaster = "string toString() { string res; auto val = value; \n";
  string prev, restspec; bool inRest;
  string name = spec.ctSlice(":").ctStrip();
  if (!spec.length) assert(false);
  string type = name.ctSlice(" ").ctStrip();
  if (!name.length) {
    name = type;
    type = "uint";
  }
  string res = "struct "~name~" { " ~ type~" value; ";
  while (spec.length) {
    string chunk = spec.ctSlice(",");
    string value = chunk.ctStrip();
    string chunkname = value.ctSlice("=").ctStrip();
    if (!value.length) 
      if (prev.length) value = prev ~ " * 2";
      else value = "1";
    prev = chunkname;
    res ~= "const "~chunkname~" = ("~value~"); \n";
    toaster ~= "if (value & ("~value~")) { value &= ~("~value~"); if (res.length) res ~= \" | \"; res ~= \""~chunkname~"\"; }\n";
    chunk = "";
  }
  toaster ~= "return res;\n}\n";
  res ~= toaster;
  res ~= "bool has(uint mask) { return !!(value & mask); }\n";
  res ~= "void set(uint mask) { value |= mask; }\n";
  res ~= "}";
  return res;
}

ulong rdtsc() {
  uint r1, r2;
  asm { rdtsc; mov r1, EAX; mov r2, EDX; }
  ulong res;
  auto ufield = (cast(uint*) &res)[0 .. 2];
  ufield[0] = r1; ufield[1] = r2;
  return res;
}

int bound(int i, int low, int high) {
  auto k = i - low;
  high -= low;
  if (k >= high) k -= (k / high) * high;
  if (k < 0) k -= (k / high - 1) * high;
  return k + low;
}

// alias PI π;

string collapseDots(string s) {
  auto temp = s.split("/");
  string[] res;
  foreach (part; temp) {
    if (part == ".") { // ignore outright
    } else if (part == "..") {
      if (res.length) {
        assert(res[$-1].length);
        res = res[0 .. $-1];
      } else res ~= part;
    } else res ~= part;
  }
  return res.join("/");
}

string relpath(string cwd, string target) {
  cwd = cwd.collapseDots();
  target = target.collapseDots();
  string[] split(string s) {
    auto res = .split(s, "/");
    while (res.length && !res[$-1].length) res = res[0 .. $-1];
    return res;
  }
  bool eq(string a, string b) { return a && b && a == b; }
  auto cwd_list = split(cwd), target_list = split(target);
  string car(string[] foo) { if (!foo.length) return null; return foo[0]; }
  string[] cdr(string[] foo) { if (!foo.length) return null; return foo[1 .. $]; }
  while (eq(cwd_list.car(), target_list.car())) { cwd_list = cwd_list.cdr(); target_list = target_list.cdr(); }
  string res = ".";
  foreach (entry; cwd_list) res = res.sub("..");
  foreach (entry; target_list) res = res.sub(entry);
  if (auto rest = res.startsWith("./")) res = rest;
  return res;
}

string constEnum(string spec) {
  spec = spec.ctStrip() ~ ",";
  string type = spec.ctSlice(":"); spec = spec.ctStrip();
  string offset = "0+1";
  if (spec.ctFind(":") != -1) {
    offset = spec.ctSlice(":");
  }
  string res, list = ctExpand(spec);
  if (list.length) {
    string op, start;
    foreach (i, ch; offset) {
      if ("+-*/<>".ctFind(ch) != -1) {
        start = offset[0 .. i];
        op = offset[i .. $];
        break;
      }
    }
    if (offset.length && !start.length) start = offset;
    if (!op.length) op = "+1";
    if (!start.length) start = "0";
    list = list[1 .. $];
    string prev;
    int defpos = type.ctFind("<-");
    if (defpos != -1) {
      string def = type[defpos+2 .. $];
      type = type[0 .. defpos];
      res ~= "typedef "~def~" "~type~"; \n";
    }
    do {
      string chunk = list.ctSlice("|");
      string value;
      if (prev) value = prev ~ op;
      else value = start;
      int valpos = chunk.ctFind("[");
      if (valpos != -1) {
        value = chunk.ctBetween("[", "]");
        chunk = chunk[0 .. valpos] ~ chunk[valpos+2+value.length .. $];
      }
      res ~= "const "~type~" "~chunk~" = "~value~"; \n";
      prev = chunk;
    } while (list.length);
  }
  return res;
}

void enforce(T...)(bool b, T text) {
  if (!b) throw new Exception(Format(text));
}

void ArgParse(T...)(string[] args, T t) {
  outer:while (args.length) {
    auto cur = args.take().tolower();
    foreach (i, bogus; t) {
      static if (is(T[i]: string)) {
        if (t[i] != cur) continue;
        foreach (_k, boguser; t[i+1 .. $]) {
          const k = _k + i+1;
          static if (!is(T[k]: string)) {
            static if (is(typeof(*t[k]))) alias typeof(*t[k]) DestType;
            else alias T[k] DestType;
            static if (is(DestType == bool))
              *t[k] = true;
            else static if (is(float: DestType))
              *t[k] = args.take().atof();
            else static if (is(int: DestType))
              *t[k] = args.take().atoi();
            else static if (is(string: DestType))
              *t[k] = args.take();
            else static if (is(typeof(t[k]())))
              t[k]();
            else static assert(false, "Unsupported ini type: "~T[k].stringof);
            goto outer;
          }
        }
        assert(false, "No target value after "~cur);
      }
    }
    assert(false, "Unknown argument: "~cur);
  }
}
