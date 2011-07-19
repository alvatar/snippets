module tools.multi;

import tools.base, tools.compat;

template SafeUnionCode(int id, T...) {
  const string idstr = ctToString(id);

  static if (id == T.length) const string unioncode = "";
  else const string unioncode = "T["~idstr~"] member_"~idstr~"; " ~ SafeUnionCode!(id+1, T).unioncode;

  static if (id == T.length) const string switchcode = "
    default: throw new Exception(Format(\"Cannot find a callable in \", C.stringof, \" to handle \", T.stringof, \"[\", which, \"]\"));
  }";
  else {
    static if (!id) const string pre_switchcode = "switch (which) {";
    else const string pre_switchcode = "";
    const string switchcode = pre_switchcode ~ "
      case "~idstr~":
        foreach (callable; callables)
          static if (is(typeof(callable(member_"~idstr~"))))
            return callable(member_"~idstr~");
        break;" ~ SafeUnionCode!(id+1, T).switchcode;
  }
}

struct SafeUnion(T...) {
  mixin("union { "~SafeUnionCode!(0, T).unioncode~" } ");
  short which = -1;
  typeof(function() { C callables; mixin(SafeUnionCode!(0, T).switchcode); }()) call(C...)(C callables) {
    mixin(SafeUnionCode!(0, T).switchcode);
  }
  bool isType(U)() {
    foreach (I, TYPE; T)
      static if (is(TYPE==U))
        return I==which;
    return false;
  }
  U get(U)() {
    foreach (I, TYPE; T)
      static if (is(TYPE==U)) if (I == which) return mixin("member_"~ctToString(I));
    throw new Exception(Format("Type ", U.stringof, " does not match ", T.stringof, "[", which, "]: cannot get!"));
  }
  static SafeUnion opCall(V)(V value) {
    SafeUnion res = void;
    res.which = -1;
    foreach (I, Type; T) static if (is(Type == V)) {
      mixin("res.member_"~ctToString(I)~" = value; ");
      res.which = I;
    }
    if (res.which == -1) throw new Exception("Unable to match "~V.stringof~" to one of "~T.stringof~"!");
    return res;
  }
}

struct TupleSet(T...) {
  alias T Tuple;
  template _Has(U, V...) {
    static if (!V.length) const bool _Has = false;
    else const bool _Has = is(U == V[0]) || _Has!(U, V[1..$]);
  }
  template Has(U) { const bool Has = _Has!(U, T); }
  template Add(T) { alias TupleSet!(Tuple, T) Add; }
}

import tools.tests;
unittest {
  alias SafeUnion!(int, float) ifun;
  auto ifun_test = ifun(2), ifun_test2 = ifun(3.14159f);
  mustEqual("SafeUnionTypeTest", ifun_test.isType!(int), ifun_test2.isType!(float), true);
  mustEqual("SafeUnionTypeTest2", ifun_test.isType!(float), ifun_test2.isType!(int), false);
  mustFail("SafeUnionInvalidTest", ifun_test2.get!(int));
  float whee = 4;
  ifun_test.call((float f) { whee = f; });
  mustEqual("SafeUnionConversionTest", whee, 2);
}
