module tools.functional;
public import tools.base; import tools.compat;

mixin(genOperator("map", "
  static if (is(typeof(Init!(LHS).keys))) {
    static if (is(typeof(mixin(\"Init!(RHS)(Init!(LHS).keys[0], \"~Value!(ElemType!(LHS), typeof(Init!(RHS) /fix/ Init!(LHS).keys[0]), \"Init!(LHS).values[0]\")~\")\")))) {
      const bool spreadAA=true;
    } else {
      static assert(false, \"Cannot call \"~RHS.stringof~\" with \"~Tuple!(typeof(Init!(LHS).keys[0]), typeof(Init!(LHS).values[0])).stringof ~ 
        \" - cannot convert implicitly!\");
    }
  } else const bool spreadAA=false;
  static if (spreadAA) {
    alias typeof(Init!(LHS).keys[0]) Key;
    alias typeof(Init!(LHS).values[0]) Values;
    alias typeof(mixin(\"rhs(Init!(Key), \"~Value!(ElemType!(LHS), typeof(Init!(RHS) /fix/ Init!(LHS).keys[0]), \"Init!(Values)\")~\")\")) Res;
    static if (is(Res==void)) {
      foreach (id, ref entry; lhs) mixin(\"rhs(id, \"~Value!(ElemType!(LHS), typeof(Init!(RHS) /fix/ Init!(LHS).keys[0]), \"entry\")~\");\");
    } else {
      auto res=new Res[lhs.length];
      size_t cur=0;
      foreach (id, ref entry; lhs)
        res[cur++]=mixin(\"rhs(id, \"~Value!(ElemType!(LHS), typeof(rhs /fix/ id), \"entry\")~\")\");
      return res;
    }
  } else {
    ElemType!(LHS) foob;
    alias typeof(mixin(\"rhs(\"~Value!(ElemType!(LHS), RHS, \"foob\")~\")\")) Res;
    static if (is(Res==void)) {
      foreach (ref ElemType!(LHS) entry; lhs) mixin(\"rhs(\"~Value!(ElemType!(LHS), RHS, \"entry\")~\");\");
    } else {
      static if (is(typeof(lhs.length))) {
        auto res = new Res[lhs.length];
        foreach (id, ref entry; res)
          entry=mixin(\"rhs(\"~Value!(ElemType!(LHS), RHS, \"lhs[id]\")~\")\");
        return res;
      } else {
        Res[] res;
        foreach (ElemType!(LHS) elem; lhs) res ~= mixin(\"rhs(\"~Value!(ElemType!(LHS), RHS, \"elem\")~\")\");
        return res;
      }
    }
  }
"));

mixin(genOperator("select", "
  ElemType!(LHS)[] res;
  foreach (entry; lhs) if (mixin(\"rhs(\"~Value!(ElemType!(LHS), RHS, \"entry\")~\")\")) res~=entry;
  return res;
"));

mixin(genOperator("zip", "
  static if(isIterable!(RHS)) {
    auto res=new Stuple!(ElemType!(LHS), ElemType!(RHS))[lhs.length];
    foreach (id, ref entry; res) {
      entry=stuple(lhs[id], rhs[id]);
    }
  } else {
    auto res=new Stuple!(ElemType!(LHS), Ret!(RHS))[lhs.length];
    foreach (id, ref entry; res) entry=typeof(entry)(lhs[id],
      mixin(\"rhs(\"~Value!(ElemType!(LHS), RHS, \"lhs[id]\")~\")\")
    );
  }
  return res;
"));

mixin(genOperator("cross", "
  alias Stuple!(ElemType!(LHS), ElemType!(RHS)) Elem;
  Elem[] res;
  foreach (l; lhs)
    foreach (r; rhs)
      res~=Elem(l, r);
  return res;
"));

mixin(DualOperator!("reduce", `
  // parameter given
  //if (!lhs.length) throw new Exception("Cannot reduce empty array");
  static assert(params.length == 1, "Invalid number of reduce start values");
  // logln("Params: ", params);
  auto res = params[0];
  static if (is(typeof(rhs(res, Init!(ElemType!(LHS)))))) const string value = "elem";
  else static if (is(typeof(rhs(res, Init!(ElemType!(LHS)).tupleof)))) const string value = "elem.tupleof";
  else static assert(false, "Cannot call RHS "~RHS.stringof~" with res "~typeof(res).stringof~" and/or element of LHS "~ElemType!(LHS).stringof~"!");
  foreach (elem; lhs) {
    // logln("res: ", res, " with ", elem);
    static if (is(typeof(rhs(res, mixin(value))) == void)) rhs(res, mixin(value));
    else res = rhs(res, mixin(value));
  }
  return res;
`, "
  // no parameter given
  static assert(is(Params!(RHS)), \"reduce: Cannot extract parameters from RHS expression \"~RHS.stringof~\"! \"~
  \"Please specify the start value manually\");
  static if (is(typeof(RHS.Start!(ElemType!(LHS))))) return lhs /reduce(RHS.Start!(ElemType!(LHS)))/ rhs;
  else return lhs /reduce(Init!((Params!(RHS)[0])))/ rhs;
"));

mixin(genOperator("unique", "
  ElemType!(LHS)[] res; bool[ElemType!(LHS)] map;
  foreach (elem; lhs) if (!(elem in map)) { res~=elem; map[elem]=true; } return res;
", true)); alias unique uniq;

mixin(genOperator("sort", "auto res=lhs.dup; res.sort; return res;", true));

struct curry_holder(LHS, G...) {
  LHS lhs; G g;
  const string CODE = "
    static if (is(typeof(lhs(g, par)))) return lhs(g, par);
    else {
      curry_holder!(LHS, G, X) res = void;
      res.lhs = lhs;
      foreach (id, entry; g) res.g[id] = entry;
      foreach (id, entry; par) res.g[G.length + id] = entry;
      return res;
    }";
  Ret!(typeof(function { X par; mixin(CODE); })) opCall(X...)(X par) {
    mixin(CODE);
  }
}

mixin(genOperator("curry", "
  static if (is(Params!(LHS))) {
    static if (Params!(LHS).length!>1) return lhs;
    else return (LHS _lhs, Params!(LHS)[0] first) { return _lhs /fix/ first /curry; } /fix/ lhs;
  } else {
    curry_holder!(LHS) res = void;
    res.lhs = lhs; return res;
  }
", true));

template CurryRes(S, T...) {
  static if (T.length == 1) alias typeof(Init!(S) (Init!(T[0]))) CurryRes;
  else alias typeof((Init!(S) (Init!(T[0])) /uncurry) (Init!(T[1 .. $]))) CurryRes;
}

template CollectParameters(T) {
  static if (is(T==function) || is (T==delegate)) {
    alias Tuple!(Params!(T), CollectParameters!(Ret!(T)).tuple) tuple;
    alias CollectParameters!(Ret!(T)).last last;
  } else {
    alias Tuple!() tuple;
    alias T last;
  }
}

template CurryCalls(int depth) {
  static if (!depth) const string CurryCalls="";
  else const string CurryCalls=CurryCalls!(depth-1)~"(cp["~depth.stringof~"-1])";
}
 
mixin(genOperator("uncurry", "
  // pragma(msg, LHS.stringof);
  alias CollectParameters!(LHS).tuple CPAR;
  struct holder {
    LHS l;
    CollectParameters!(LHS).last call(CPAR cp) {
      return mixin(\"l\"~CurryCalls!(CPAR.length));
    }
  }
  auto res=new holder;
  res.l=lhs;
  return &res.call;
", true));

mixin(genOperator("groupby", `
  static assert(is(ElemType!(LHS)), "Cannot group what I can't iterate: "~LHS.stringof~" and "~ElemType!(LHS).stringof);
  alias ElemType!(LHS) et;
  alias typeof(mixin("rhs("~Value!(et, typeof(rhs), "Init!(et)")~")")) RET;
  ElemType!(LHS)[][RET] res;
  foreach (ref et elem; lhs) {
    auto key=mixin("rhs("~Value!(typeof(elem), typeof(rhs), "elem")~")");
    if (!(key in res)) res[key]=Init!(typeof(res[key]));
    res[key] ~= elem;
  }
  return res;
`));

template MinTemp(T) { const MinTemp=T.min; }

T[] qsortfn(T, C)(T[] input, C smaller) {
  if (input.length < 3) {
    if (input.length == 0 /or/ 1) return input;
    if (smaller(input[1], input[0])) return [input[1], input[0]];
    return input;
  }
  auto pivot = input[$/2];
  return (input /select/ (T t) { return smaller(t, pivot); }).qsortfn(smaller) ~ (input /select/ (T t) { return !smaller(t, pivot) && !smaller(pivot, t); }) ~ (input /select/ (T t) { return smaller(pivot, t); }).qsortfn(smaller);
}

mixin(genOperator("qsort", "return qsortfn(lhs, rhs); "));

import tools.tests;
unittest {
  logln("functional tests");
  mustEqual("MapCurryTest", [2, 3] /map/ (&add!(int) /curry)(4), [6, 7]);
  mustEqual("TrivialSelectTest", [4, 5, 6, 7, 8] /select/ ex!("a -> (a&1)==0"), [4, 6, 8]);
  struct Foo { int a; } auto test=[Foo(2), Foo(4), Foo(3)];
  mustEqual("MemberSumTest",
    test /map/ member!("a") /reduce(0)/ ex!("a, b -> a+b"),
    test /map/ ex!("e -> e.a") /reduce(0)/ ex!("a, b -> a+b"),
    9
  );
  mustEqual("ZipTest",
    [4, 5] /zip/ ["abcd", "abcde"] /map/ (int a, string b) { return a-b.length; },
    [4: "abcd"[], 5: "abcde"] /map/ (int a, string b) { return a-b.length; },
    [0u, 0u]
  );
  mustEqual("ZipTest2",
    [4, 5] /zip/ [0, 0] /map/ ex!("a, b -> a"),
    [4, 5]
  );
  mustEqual("ComboTest", [4, 5, 6] /cross/ [2, 3] /map/ op!(" * ") /select/ ex!("a -> (a&1)==0") /unique /sort, [8, 10, 12, 18]);
  mustEqual("AAMapTest", [4: 0.1, 5: 0.3, 6: 0.7] /map/ (int a, double f) { return f; }, [0.1, 0.3, 0.7]);
  // weird things in llvm
  // mustEqual("UncurryTest", (op!("+") /curry /uncurry)(2, 2), 4);
  // broken in llvm
  // mustEqual("ReduceTest2", [2, 3, 4, 5] /reduce(0)/ op!("+"), 14);
  mustEqual("GroupbyTest", [4, 5, 6] /groupby/ (int e) { return e&1; }, [0: [4, 6][], 1: [5][]]);
  mustEqual("RangeSliceMap", Range[3..6] /map/ (int e) { return e*2; }, [6, 8, 10]);
  mustEqual("RangeMap",
    Range[5..10] /map/ (int e) { return e*3; },
    [15, 18, 21, 24, 27]
  );
  mustEqual("RangeSelect",
    Range[10..20] /select/ (int e) { return (e&1)?true:false; },
    // Range[10..20] /select/ function(int foo) { return (foo&1)?true:false; },
    [11, 13, 15, 17, 19]
  );
}
