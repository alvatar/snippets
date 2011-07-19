// generic recursive descent parser
module tools.rd;
import tools.base, tools.compat;
public import tools.base: Params;

string next_text(string s, int len = 64) { if (s.length >= len) s = s[0 .. len]; return s.replace("\n", "\\n"); }

int ctMustFind(string text, string match) {
  int res = ctFind(text, match);
  if (res == -1) assert(false, "Couldn't find "~match~" in "~text);
  return res;
}

// the stuff between $$s
string parseVar(string var) {
  // order: $foo<-param/5#test()$
  string res = "(";
  auto pos = ctFind(var, "#");
  string cmd; // what to do on match
  if (pos != -1) {
    cmd = var[pos+1 .. $];
    var = var[0 .. pos];
  }
  pos = ctFind(var, "/");
  string param; // what to pass to got
  if (pos != -1) {
    param = ", " ~ var[pos+1 .. $];
    var = var[0 .. pos];
  }
  pos = ctFind(var, "<-");
  string gotFunc = "got"; // function to call to extract variable
  if (pos != -1) {
    gotFunc = var[pos+2 .. $];
    var = var[0 .. pos];
  }
  if (var.length) res ~= gotFunc~"(scratch, "~var~param~")";
  if (cmd.length) {
    if (var.length) res ~= "&& ";
    res ~= "(("~cmd~"), true)";
  }
  return res ~ ")";
}

string gotMatchExpr(string expr) {
  string res;
  int varPos = ctMustFind(expr, ":");
  string var = expr[0 .. varPos]; expr = expr[varPos+1 .. $].ctStrip();
  
  string tokenBuf; // ordinary text tokens to match
  string stack; // [{}] etc
  string conditionalOrUsed; // ynnynyy [|] or []
  // true so we can always go "&& foo", "&& bar"
  string OPEN_LITERAL = "(ref string s) { auto scratch = s; return (true",
    CLOSE_LITERAL = ") && ((s = scratch), true); }";
  res = OPEN_LITERAL;
  bool escape_mode;
  for (int i = 0; i < expr.length; ++i) {
    bool lastChar;
    if (escape_mode) {
      tokenBuf ~= expr[i];
      escape_mode = false;
      if (i == expr.length - 1) expr ~= " "; // make sure it's not the last character
      continue;
    }
    if (expr[i] == '\\') {
      escape_mode = true;
      continue;
    }
    // if it's the last character, as well as a normal text character: closing tokens don't count
    // in this case, we're just using the flush.
    // evidently, if your last character is an opening token, you're Doing It Wrong.
    if (i == expr.length - 1 && ctFind("$|]}", expr[i]) == -1) { tokenBuf ~= expr[i]; lastChar = true; }
    if (ctFind("[{$|}]", expr[i]) != -1 || lastChar) {
      // flush
      tokenBuf = ctStrip(tokenBuf);
      if (tokenBuf.length) {
        res ~= " && scratch.accept(\""~tokenBuf~"\")";
        tokenBuf = "";
      }
      if (ctFind("[{", expr[i]) != -1) stack ~= expr[i];
    }
    if (expr[i] == '[') {
      res ~= " && "~OPEN_LITERAL;
      conditionalOrUsed ~= 'n';
    } else if (expr[i] == '|') {
      assert(stack.length && stack[$-1] == '[', "| only allowed in conditional blocks: "~expr);
      assert(conditionalOrUsed.length);
      conditionalOrUsed = conditionalOrUsed[0 .. $-1] ~ 'y';
      // THIS IS WRONG
      // We forgot to reset scratch from the previous match attempt
      // res ~= ") && ((s=scratch), true) || (true";
      res ~= ") && ((s=scratch), true) || (((scratch=s), true)";
    } else if (expr[i] == ']') {
      assert(stack.length && stack[$-1] == '[', "Matching [ expected in "~expr~"; instead, stack is "~stack);
      stack = stack[0 .. $-1];
      assert(conditionalOrUsed.length, "Internal fuck-up.");
      res ~= ") && ((s = scratch), true) "~((conditionalOrUsed[$-1] == 'n')?"|| true":"") ~ "; }(scratch)";
      conditionalOrUsed = conditionalOrUsed[0 .. $-1];
    } else if (expr[i] == '{') {
      res ~= " && (ref string s) { auto scratch = s; while ((true";
    } else if (expr[i] == '}') {
      assert(stack.length && stack[$-1] == '{', "Matching { expected in "~expr~"; instead, stack is "~stack);
      stack = stack[0 .. $-1];
      res ~= ")) s = scratch; return true; }(scratch)";
    } else if (expr[i] == '$') {
      i ++;
      string target_varname;
      // fast-forward
      while (expr[i] != '$') {
        target_varname ~= expr[i];
        i ++;
        assert(i != expr.length, "Closing $ not found in "~expr~"!");
      }
      res ~= " && " ~ parseVar(target_varname);
    } else tokenBuf ~= expr[i];
  }
  assert(!stack.length, "Brackets left unclosed: "~stack~" in "~expr);
  res ~= CLOSE_LITERAL ~ "("~var~")";
  return res;
}

string MatchExpr(string expr) {
  auto varname = expr.ctSlice(":");
  expr = varname ~ ":" ~ expr;
  return "
  { auto _backup = "~varname~", _match = "~gotMatchExpr(expr)~";
    if (!_match) throw new Exception(Format(*failtext.ptr(), \" at `\", _backup.next_text(), \"´ \"));
  }";
}

template getWrap(string name) {
  const string getWrap = "Params!(typeof(got"~name~"))[1] get"~name~"(ref string s) {
    Params!(typeof(got"~name~"))[1] res;
    if (!got"~name~"(s, res))
      throw new ParseException(r\"Expected `\"~typeof(res).stringof~\"´, got `\"~s.next_text()~\"´\");
    return res;
  }";
}

// Don't ask. Just .. don't.
template RepeatAs(T, U...) {
  static if (!U.length) alias Tuple!() RepeatAs;
  else alias Tuple!(T, RepeatAs!(T, U[1 .. $])) RepeatAs;
}

// Nth!(1, 3, X) == every second entry in every three's group in X.
template Nth(int which, int of, T...) {
  static assert(T.length % of == 0, "Invalid parameters to Nth: "~of.stringof~" and "~T.stringof);
  static if (!T.length) alias Tuple!() Nth;
  else alias Tuple!(T[which], Nth!(which, of, T[of .. $])) Nth;
}

template Eq(X...) {
  static assert(X.length == 2);
  static if (is(X[0]) && is(X[1])) const Eq = is(X[0] == X[1]);
  else static if (is(typeof(X[0])) && is(typeof(X[1]))) const Eq = X[0] == X[1];
  else static assert(false, "Eq: Unsure what to do with this: "~X.stringof);
}

template Tuple2DLookup(int which, int of, S, T...) {
  static assert(T.length % of == 0, "Invalid parameters to Tuple2DLookup: "~of.stringof~" and "~S.stringof~" in "~T.stringof);
  static assert(T.length, "Missing last parameter to Tuple2DLookup / Entry "~S.stringof~" not found!");
  static if (Eq!(T[which], S)) alias T[0 .. of] Tuple2DLookup;
  else alias Tuple2DLookup!(which, of, S, T[of .. $]) Tuple2DLookup;
}

template Revert(T...) {
  static if(!T.length) alias Tuple!() Revert;
  else alias Tuple!(Revert!(T[1 .. $]), T[0]) Revert;
}

template LookupExpand(int which, int of, S, T...) {
  static if (!S.Tuple.length) alias Tuple!() LookupExpand;
  else alias Tuple!(
    Tuple2DLookup!(which, of, S.Tuple[0], T),
    LookupExpand!(which, of, TupleWrapper!(S.Tuple[1 .. $]), T)
  ) LookupExpand;
}

template _GotSort(T...) {
  alias LookupExpand!(0, 2, TupleWrapper!(DerivedToFront!(Nth!(0, 2, T))), T) _GotSort;
}

template Got(T...) {
  bool got(U, V...)(ref string s, out U u, V extra) {
    alias _GotSort!(T) Sorted;
    bool ass = true;
    foreach (i, entry; RepeatAs!(int, T)) {
      static if (!(i % 2)) {
        static if (is(U == Sorted[i])) return Sorted[i+1](s, u, extra);
        else static if (is(typeof(u.classinfo))) {
          static if (is(U: Sorted[i])) {
            ass = false;
            Sorted[i] foo;
            if (!Sorted[i+1](s, foo, extra)) continue;
            u = cast(U) foo;
            if (u) return true;
          }
        }
      }
    }
    assert(!ass, "Cannot get "~U.stringof~" from input: type not supported");
    return false;
  }
}

import tools.threads;
TLS!(string) failtext;
static this() { New(failtext, { return &(new Stuple!(string))._0; }); }

bool accept(ref string s, string what) {
  if (!s.startsWith(what)) { *failtext.ptr() = what; return false; }
  s = s[what.length .. $];
  return true;
}
