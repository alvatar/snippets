module tools.tests;
import tools.base, tools.compat;
public import tools.log;

size_t passed, failed;

char[] logResult(char[] name, bool succeed, char[] reason) {
  if (succeed) ++passed; else ++failed;
  char[] spacing; if (name.length<30) spacing=' ' /times(30-name.length);
  return " -"~name~"- "~spacing~"["~(succeed?"good":"fail")~"] - "~reason;
}

void mustFail(char[] name, void delegate()[] dgs...) {
  Exception ge=null;
  try foreach (dg; dgs) dg(); catch (Exception e) { ge=e; goto good; }
  // I <3 this line
  throw new Exception(name~": fail, Expected Exception not caught");
good:
  logln(logResult(name, true, "Expected exception ("~ge.toString~") caught"));
  return;
}

struct DeltaHolder(T) { T sigma; }
struct delta { static DeltaHolder!(T) opAssign(T)(T s) { return DeltaHolder!(T)(s); } }

string formatTuple(T...)(T stufs) {
  string fmts="(";
  foreach (stuf; stufs) fmts~=Format(stuf)~", ";
  if (fmts.length>1) fmts=fmts[0..$-2];
  return fmts~")";
}

bool unequal(T, U)(T a, U b) {
  static if (is(typeof(Init!(T).keys))) {
    typeof(b) test; foreach (id, elem; b) test[id]=elem;
    foreach (id, elem; a) {
      if (!(id in test)) return true;
      if (unequal(test[id], elem)) return true;
      test.remove(id);
    }
    if (test.length) return true;
    else return false;
  } else static if (isArray!(T)) {
    if (a.length != b.length) return true;
    foreach (i, elem; a) if (unequal(elem, b[i])) return true;
    return false;
  } else return a!=b;
}

void mustEqual(T...)(char[] name, T stuffs) {
  static assert(T.length, "Cannot establish equality without things to compare");
  static if (is(typeof(T[$-1].sigma))) {
    typeof(T[$-1].sigma) lower=stuffs[0], upper=lower;
    foreach (thingie; stuffs[1..$-1]) {
      if (thingie<lower) lower=thingie;
      if (thingie>upper) upper=thingie;
      if (upper-lower>stuffs[$-1].sigma)
        throw new Exception(Format(name, ": fail, value ", thingie, " violated delta of ", stuffs[$-1].sigma));
    }
    logln(logResult(name, true, Format("Parameters ", formatTuple(stuffs[0..$-1]), " are equal within delta of ", stuffs[$-1].sigma)));
  } else {
    foreach (thing; stuffs[0..$-1]) if (unequal(thing, stuffs[$-1]))
      throw new Exception(name~": failed; "~Format(thing)~" is not "~Format(stuffs[$-1])~".");
    logln(logResult(name, true, Format("Parameters ", formatTuple(stuffs), " are equal")));
  }
}

void Assert(char[] name, bool delegate()[] dgs...) {
  foreach (dg; dgs) if (!dg()) throw new Exception(name~": fail, eval to false");;
  logln(logResult(name, true, "No exceptions, all conditions true"));
}

unittest {
  logln("tests tests tests' tests");
  mustFail("UnitTestFailure", mustFail("EvidentFailure", {}));
  mustFail("UnitTestAssertFailure", Assert("EvidentFailure", 1==0));
  mustFail("UnitTestmustBeFailure", mustEqual("Fail", true, false));
  mustEqual("DeltaTest", 3, 5, delta=2);
  mustFail("DeltaFailureTest", mustEqual("DeltaMustFail", 3, 7.5, delta=1f));
}
