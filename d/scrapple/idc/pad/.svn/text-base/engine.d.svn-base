module pad.engine; // paste adventure

import tools.rd, tools.base, tools.functional, tools.downloader, tools.threads, tools.log;
import tools.compat;

import pad.utils;

template castToFinal(S) {
  S castToFinal(T)(T t) {
    // Mind: t must not be an interface!
    // TODO check for that
    if (t && t.classinfo is S.classinfo) {
      return cast(S) cast(void*) t;
    } else return null;
  }
}

alias castToFinal!(Scope) castToScope;
alias castToFinal!(ListStatement) castToListStatement;

mixin(genOperator("ifnull", `if (!lhs) rhs(); return lhs; `));

class ReturnException : Exception {
  this() { super("So sue me, motherfuckers"); }
}

string pastepost(string predata, string sub_bin = null, string expiry = "N") {
  string data;
  static char[3][] map;
  synchronized if (!map) {
    const hex = "0123456789abcdef";
    map.length = 256;
    foreach (i, ref v; map) {
      v[0] = '%';
      v[1] = hex[i/16];
      v[2] = hex[i%16];
    }
  }
  foreach (ch; cast(ubyte[]) predata) data ~= map[ch];
  if (sub_bin) sub_bin ~= ".";
  // auto url = "POST=parent_pid=&format=text&code2="~data~"&poster=&paste=Send&expiry="~expiry~"&email= "
  //   "http://"~sub_bin~"pastebin.com/pastebin.php";
  // Pastebin v2 ..
  auto url = "POST=submit=submit&paste_parent_key=&paste_subdomain="~sub_bin~
    "&paste_code="~data~"&paste_format=1&paste_expire_date="~expiry~"&paste_private=1&paste_name=&paste_remember=0&submit=submit "
    "http://"~sub_bin~"pastebin.com/post.php";
  string redir;
  url.download(&redir);
  if (redir.startsWith("COOKIE=")) redir.slice(" ");
  return redir;
}

class Graph {
  string info;
  Graph[] nodes;
  static Graph opCall(string data, Statement[] subs...) {
    auto res = new Graph;
    res.info = data;
    foreach (sub; subs)
      if (sub) res.nodes ~= sub.getInfo();
      else res.nodes ~= null;
    return res;
  }
  string gen() {
    string res;
    int depth;
    void recurse(int depth, Graph g) {
      string line;
      for (int i = 0; i < depth; ++i) line ~= "  ";
      line ~= " -";
      if (g) line ~= g.info;
      else line ~= "(null)";
      line ~= "\n";
      res ~= line;
      if (g) foreach (node; g.nodes) recurse(depth+1, node);
    }
    recurse(0, this);
    return res;
  }
  string post() {
    return gen().pastepost();
  }
}

class AreaGraph {
  Stuple!(string, string)[] links;
  string[string] short_names, long_names;
  int names;
  string simplify(string s) {
    if (auto p = s in short_names) return *p;
    auto res = Format("cell", ++names);
    short_names[s] = res;
    long_names[res] = s;
    return res;
  }
  void connect(string a, string b) {
    links ~= stuple(a.simplify(), b.simplify());
  }
  string toDot() {
    string res;
    res ~= "Digraph G { concentrate = true; \n";
    foreach (key, value; short_names) {
      res ~= Format("  ", value, " [label=\"", key, "\"]; \n");
    }
    foreach (link; links) {
      auto line = Format("  ", link._0, " -> ", link._1);
      /*if (target._1) {
          string headport;
          auto word = target._1.replace("param.", "").replace(".go", "");
          line ~= " [label=\""~word~"\", decorate, ";
      } else */line ~= " [";
      line ~= "style=dotted]; \n";
      res ~= line;
    }
    res ~= "}\n";
    return res;
  }
}

TLS!(AreaGraph) g_AGList;
static this() { New(g_AGList); }

struct Text {
  string[] cache;
  int chunks, length;
  void opCatAssign(string s) {
    if (!cache.length) cache.length = 1024;
    else if (chunks >= cache.length) cache.length = cache.length * 2;
    cache[chunks++] = s;
    length += s.length;
  }
  string collate() {
    auto res = new char[length];
    int base;
    foreach (chunk; cache) {
      res[base .. base + chunk.length] = chunk;
      base += chunk.length;
    }
    assert(base == res.length);
    return res;
  }
}

abstract class Statement {
  Scope eval(ref Text text, Scope location = null);
  Statement dup();
  void iterate(void delegate(Statement) dg);
  Graph getInfo();
}

void flatIterate(Scope sc, void delegate(Statement) dg) {
  // logln("iterate @", sc.fqn, ", parent ", sc.parent);
  Scope location = sc;
  void recurse(Statement st) {
    /*if (location.parent) {
      if (auto ms = cast(MergeStatement) st) {
        ms.checkBuild(location);
      }
    }*/
    auto sc = castToScope(st), ls = castToListStatement(st);
    if (ls || sc && sc.name == "") st.iterate(&recurse); // transparent
    else dg(st);
  }
  sc.iterate(&recurse);
}

Statement[] flatten(Scope sc, Statement[] prevCache = null) {
  auto res = prevCache;
  int counter;
  flatIterate(sc, (Statement st) {
    if (counter !< res.length) {
      assert(counter == res.length);
      res ~= st;
      counter ++;
    } else res[counter++] = st; // Update
  });
  return res[0 .. counter];
}

enum TokenMode {
  Normal, Name, Identifier, IdentifierRest
}

bool isSeparator(char c) {
  return " \n\r\t[]{}()<>,-+*/!:%&;=\"".find(c) != -1;
}

bool gotToken(ref string st, out string s, TokenMode mode = TokenMode.Normal) {
  string s2 = st.strip();
  bool first = true;
  bool valid(char c) {
    scope(exit) first = false;
    if (first && mode == TokenMode.IdentifierRest) return c == '.';
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (mode == TokenMode.Normal || !first) && (c >= '0' && c <= '9')
      || mode != TokenMode.Name && c == '.'
      || c == '_';
  }
  if (!s2.length) { return false; }
  int matched;
  while (matched < s2.length && valid(s2[matched]))
    matched++;
  auto res = s2.take(matched);
  if (!res.length) {
    if (mode != TokenMode.Normal || !s2.length) { *failtext.ptr() = "Invalid token "~st.strip().next_text(); return false; }
    st = s2; s = st.take(1); // single-symbol token
    return true;
  }
  // TODO: list of reserved tokens to fail on, would go nicely here
  if (res == "roll") return false; // reserved keyword
  // don't match name token off compound expression
  if (s2.length && mode == TokenMode.Name && !isSeparator(s2[0])) {
    return false;
  }
  st = s2; s = res;
  return true;
}

bool accept(ref string st, string compare) {
  string s2 = st, token; compare = compare.strip();
  if (compare == ".") // The tokenizer is broken here. Special handling.
    if (auto res = st.strip().startsWith(".")) { st = res; return true; }
  while (compare.length) {
    if (!s2.gotToken(token)) return false;
    // logln("accept ", compare, " on ", token, " - ", s2.next_text());
    if (auto rest = compare.startsWith(token)) {
      compare = rest;
    } else return false;
  }
  st = s2; return true;
}

template CopyDup() {
  Statement dup() {
    typeof(this) res;
    New(res);
    foreach (i, v; this.tupleof) res.tupleof[i] = v;
    return res;
  }
}

// data is invariant; no need to copy anything
template InvarDup() {
  Statement dup() {
    return this;
  }
}

bool prot;
class LogStatement : Statement {
  override {
    Scope eval(ref Text text, Scope location) { prot = !prot; return null; }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("LogStatement"); }
  }
  static bool gotThis(ref string s, out LogStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: log"));
  }
}

class AbortStatement : Statement {
  override {
    Scope eval(ref Text text, Scope location) { throw new ReturnException; }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("AbortStatement"); }
  }
  static bool gotThis(ref string s, out AbortStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: abort"));
  }
}

// trigger an internal callback
string delegate(Scope, Value)[string] callbacks;
static this() {
  callbacks["break"] = (Scope sc, Value v) { asm { int 3; } return cast(string) null; };
  callbacks["arealink"] = (Scope sc, Value v) {
    auto ag = g_AGList();
    auto param = v.to!(string);
    assert(param.find("->") != -1);
    auto to = param, from = to.slice("->");
    ag.connect(from.strip(), to.strip());
    return cast(string) null;
  };
  callbacks["info"] = (Scope sc, Value v) {
    return "[info "~v.to!(Scope).info~"]";
  };
}

class PragmaStatement : Statement {
  string pragmaType;
  Value delegate(Scope) param;
  override {
    Scope eval(ref Text text, Scope location) {
      if (auto cb = pragmaType in callbacks) text ~= (*cb)(location, param(location));
      return null;
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Pragma("~pragmaType~")"); }
  }
  static bool gotThis(ref string s, out PragmaStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: pragma($stmt.pragmaType$[, $stmt.param<-genExpr$])"));
  }
}

class SimpleStatement : Statement {
  Value delegate(Scope) start;
  string name, extra;
  string toString() { return Format("(call ", name, ")"); }
  Stuple!(Scope, bool) getScope(Scope location) {
    if (start) {
      auto val = start(location);
      location = val.to!(Scope);
      if (!location) throw new Exception(Format("Unable to evaluate start expression as scope: ", val));
    }
    if (!name.length) { assert(start); return stuple(location, true); }
    return location.lookup(name);
  }
  override {
    Scope eval(ref Text text, Scope location) {
      auto l = getScope(location);
      if (prot)
        text ~= Format("[calling ", name, " @", location.fqn, " => ", l._0?l._0.fqn:"(nil)", "]");
      if (l._0) return l._0.eval(text, l._1?location:l._0);
      else throw new Exception("Not found: "~name~" in "~location.fqn~" @"~extra);
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("SimpleStatement -> "~name); }
  }
  static bool gotThis(ref string s, out SimpleStatement stmt) {
    New(stmt);
    stmt.extra = s.next_text();
    return mixin(gotMatchExpr("s: ["
      "$stmt.start <- genExprSingle$[.$stmt.name<-gotToken/TokenMode.Identifier$]|"
      "$#stmt.start = null$ $stmt.name<-gotToken/TokenMode.Identifier$"
    "]")) && {
      if (s.startsWith(".")) {
        string test;
        auto r = s.gotToken(test);
        throw new Exception("Unmatched rest: "~s.next_text()~Format(": test ", test, " => ", r));
      }
      return true;
    }();
  }
}

struct Value {
  mixin(fancyEnum("Type: Boolean, Integer, String, Float, ScopeRef, ScopeValue"));
  enum FlatType { Boolean, Integer, String, Float, ScopeRef, ScopeValue }
  union {
    Type type;
    FlatType flatType;
  }
  string toString() {
    if (type == Type.ScopeRef && !sr)
      return Format("Value<ScopeRef:null>");
    return Format("Value<", type, ":", to!(string), ">");
  }
  Value opAssign(T)(T t) {
    return *this = Value(t);
  }
  static Value opCall(T...)(T t) {
    alias t[0] v;
    alias T[0] V;
    static if (T.length == 2) alias t[1] isValue;
    static assert (T.length >= 1 && T.length <= 2, "Invalid number of parameters to Value.opCall");
    Value res;
    with (res) {
           static if (is(V == bool)) { b = v; type = Type.Boolean; }
      else static if (is(V == int)) { i = v; type = Type.Integer; }
      else static if (is(V == string)) { s = v; type = Type.String; }
      else static if (is(V == float)) { f = v; type = Type.Float; }
      else static if (is(V == Scope)) {
        assert(is(typeof(isValue)), "Scope to Value construction must be ref (false) or value (true)");
        if (isValue && !v) { // bug
          logln("Trying to assign null scope as value! ");
          asm { int 3; }
        }
        sr = v;
        type = isValue ? Type.ScopeValue : Type.ScopeRef;
      }
      else static assert(false, "Unsupported type: "~V.stringof);
    }
    return res;
  }
  bool isType(T)() {
    if (type == Type.ScopeValue) return true;
         static if (is(T == bool)) return type == Type.Boolean;
    else static if (is(T == int)) return type == Type.Integer;
    else static if (is(T == string)) return type == Type.String;
    else static if (is(T == float)) return type == Type.Float;
    else static if (is(T == Scope)) return type == Type.ScopeRef;
    else static assert(false, "Unsupoorted type: "~T.stringof);
  }
  // can convert nicely
  bool isLike(T)() {
    if (type == Type.ScopeValue) return true;
    static if (is(T == int)) return type == Type.Integer || type == Type.Boolean;
    return isType!(T);
  }
  T to(T)() {
    const string Table = `
                 | bool          | int         | string               | float   | Scope
      -----------+---------------+-------------+----------------------+---------+----------
      Boolean    | b             | b           | b?q{true}p:q{false}p | ø       | ø
      Integer    | i != 0        | i           | Format(i)            | i       | ø
      String     | s == q{true}p | atoi(s)     | s                    | atof(s) | ø
      Float      | ø             | cast(int) f | Format(f)            | f       | ø
      ScopeRef   | !!sr          | ø           | (sr?sr.fqn:q{(null:r)}p) | ø       | sr
      ScopeValue | sr.value().to!(T) | sr.value().to!(T) | sr.value().to!(T) | sr.value().to!(T) | sr`;
    mixin(ctTableUnrollColMajor(Table,
      `static if (is(T == $COL))
        switch (flatType) {
          $BODY
          default: throw new Exception(Format("Invalid type: ", flatType));
        }
      else `,
      `case FlatType.$ROW:
        static if (q{$CELL}p == "ø")
          throw new Exception(q{Cannot convert $ROW to $COL: }p~to!(string)~q{! }p);
        else return $CELL;
      `
    ).litstring_expand() ~ `static assert(false, "Unsupported type: "~T.stringof); `);
  }
  union {
    bool b;
    int i;
    string s;
    float f;
    Scope sr;
  }
}

bool gotRootSymbol(ref string st, out Value v) {
  return mixin(gotMatchExpr("st: [true$#v=true$|false$#v=false$]"));
}

bool isDigit(char c) { return c in Range['0' .. '9'].endIncl; }

bool gotInt(ref string st, out int i) {
  auto s2 = st.strip();
  if (!s2.length) return false;
  auto s3 = s2;
  if (s3.take() == '-' && s3.gotInt(i)) { i = -i; st = s3; return true; }
  if (!isDigit(s2[0])) return false;
  do i = i * 10 + (s2.take() - '0');
  while (s2.length && isDigit(s2[0]));
  st = s2;
  return true;
}

bool gotFloat(ref string st, out float f) {
  auto s2 = st;
  int base;
  auto backup = st;
  if (!s2.gotInt(base) || !(s2.length && s2.take() == '.')) return false;
  if (!s2.length || !isDigit(s2[0])) return false;
  auto factor = 0.1f;
  f = base;
  do { f += (s2.take() - '0') * factor; factor /= 10f; }
  while (s2.length && isDigit(s2[0]));
  st = s2;
  return true;
}

static import dice;
bool gotDice(ref string st, out dice.Result delegate() dg) {
  auto s2 = st.strip();
  if (auto rest = s2.startsWith("roll(")) s2 = rest;
  else return false;
  if (!dice.gotResult(s2, dg)) throw new Exception("Invalid dice syntax: "~st.next_text());
  s2 = s2.strip();
  if (auto rest = s2.startsWith(")")) s2 = rest;
  else throw new Exception("Expected closing paren at "~st.next_text());
  st = s2;
  return true;
}

bool gotNumber(ref string st, out Value v) {
  int i; float f;
  return mixin(gotMatchExpr("st: [$f<-gotFloat#v=f$|$i<-gotInt#v=i$]"));
}

bool gotString(ref string st, out Value v) {
  auto s2 = st.strip();
  if (!s2.length || s2.take() != '"') return false;
  v = s2.getLiteral();
  st = s2;
  return true;
}

bool genExprBase(ref string st, out Value delegate(Scope) dg) {
  Value v;
  dice.Result delegate() diceDg;
  if (st.gotDice(diceDg)) {
    dg = diceDg /apply/ (typeof(diceDg) diceDg, Scope s) {
      Value v = cast(int) diceDg().sum; return v;
    };
    return true;
  }
  if (st.gotRootSymbol(v) || st.gotNumber(v) || st.gotString(v)) { dg = v /apply/ (Value v, Scope s) { return v; }; return true; }
  if (mixin(gotMatchExpr("st: ($dg <- genExpr$)"))) return true;
  return false;
}

bool genExprSingle(ref string st, out Value delegate(Scope) dg, bool free = false) {
  if (mixin(gotMatchExpr("st: !$dg<-genExpr$"))) {
    dg = dg /apply/ (typeof(dg) dg, Scope sc) { return Value(!dg(sc).to!(bool)); };
    return true;
  }
  string token, preMunch = st;
  auto backup = st;
  if (!st.genExprBase(dg)) {
    dg = null;
    preMunch = st;
    if (!st.gotToken(token, TokenMode.Identifier)) return false;
  } else {
    preMunch = st;
    if (!st.gotToken(token, TokenMode.IdentifierRest)) token = null;
  }
  
  scope(success) {
    if (token.length) {
      if (preMunch.startsWith(token)) {
        // un-eat token; must have been part of something else (like a SimpleStatement?)
        st = preMunch;
      } else {
        throw new Exception("Leftover token: |"~token~"|; premunch would be "~preMunch.next_text()~"! ");
      }
    }
  }
  static Scope resolve(typeof(dg) dg, string base, Scope sc, void delegate() err) {
    assert(dg || base);
    Scope res = sc;
    if (dg) {
      res = dg(res).to!(Scope);
      if (!res) throw new Exception(Format("Invalid as scope: ", dg(sc)));
    }
    if (base) {
      res = res.lookup(base)._0;
      if (!res) err();
    }
    return res;
  }
  rerun: // we got a new token; reevaluate. Can happen because [foo] isn't part of token.
  if (auto pre = token.slice(".visible", false)) {
    dg = stuple(dg, pre) /apply/ (typeof(dg) dg, string base, Scope sc) {
      return Value(!resolve(dg, base, sc, {throw new Exception("Don't know how to evaluate "~base); }).hidden);
    };
  }
  if (auto pre = token.slice(".area", false)) {
    dg = stuple(dg, pre) /apply/ (typeof(dg) dg, string base, Scope sc) {
      return Value(resolve(dg, base, sc, {throw new Exception("Don't know how to evaluate "~base); }).area);
    };
  }
  if (auto pre = token.slice(".name", false)) {
    dg = stuple(dg, pre) /apply/ (typeof(dg) dg, string base, Scope sc) {
      return Value(resolve(dg, base, sc, {throw new Exception("Cannot find "~base~" in "~sc.info()); }).name);
    };
  }
  if (auto pre = token.slice(".length", false)) {
    dg = stuple(dg, pre) /apply/ (typeof(dg) dg, string base, Scope sc) {
      auto var = resolve(dg, base, sc, {throw new Exception("Don't know how to evaluate "~base); });
      int i;
      foreach (st; var.statements) if (castToScope(st)) i++;
      return Value(i);
    };
  }
  Value delegate(Scope) num;
  if (mixin(gotMatchExpr("st: \\[$num<-genExpr$\\]"))) {
    dg = stuple(num, token, dg) /apply/ (typeof(num) num, string token, typeof(dg) dg, Scope sc) {
      Value var;
      assert(!!token ^ !!dg);
      if (token) {
        var = Value(sc.lookup(token)._0, true);
        if (!var.to!(Scope))
          throw new Exception("Cannot find "~token~" in "~sc.info());
      } else var = dg(sc);
      auto n = num(sc).to!(int);
      auto vsc = var.to!(Scope);
      // throw new Exception(Format("Statements: ", vsc.statements, "[", n, "]"));
      if (auto vs = cast(ValueStatement) vsc.statements[n])
        return vs.dg(sc);
      else if (auto ss = cast(SimpleStatement) vsc.statements[n]) {
        auto res = ss.getScope(sc)._0;
        if (!res) throw new Exception(Format("Unable to lookup ", ss, " in ", sc));
        return Value(res, true);
      }
      else if (auto ms = cast(MergeStatement) vsc.statements[n]) {
        ms.checkBuild(sc);
        if (!ms.src) throw new Exception("Instantiation failed to construct! ");
        return Value(ms.src, true);
      }
      else throw new Exception("Not a value, SimpleStatement or MergeStatement: "~(cast(Object) vsc.statements[n]).toString());
    };
    preMunch = st;
    if (st.gotToken(token, TokenMode.IdentifierRest)) {
      goto rerun;
    }
  }
  bool toStr, toVal;
  if (auto pre = token.endsWith(".string")) {
    toStr = true;
    token = pre;
  }
  if (auto pre = token.endsWith(".value")) {
    toVal = true;
    token = pre;
  }
  auto
    inExpr = mixin(gotMatchExpr("st: in ")),
    atExpr = mixin(gotMatchExpr("st: at ")),
    ninExpr = mixin(gotMatchExpr("st: !in")),
    anExpr = inExpr || atExpr || ninExpr
  ;
  if (!dg) {
    if (free && !toStr && !anExpr) { st = backup; token = null; return false; }
    dg = stuple(token, anExpr, toVal) /apply/ (string cond, bool subExpr, bool toVal, Scope sc) {
      if (subExpr && !toVal) return Value(cond);
      else if (auto var = sc.lookup(cond)._0) { return Value(var, true); }
      throw new Exception(Format("Don't know how to evaluate ", cond, " in ", sc.fqn, "!"));
    };
    token = null; // munched
  }
  if (toStr) {
    dg = dg /apply/ (typeof(dg) dg, Scope sc) {
      return Value(dg(sc).to!(string));
    };
  }
  Value delegate(Scope) op2;
  auto st_info = st.next_text();
  if (anExpr && mixin(gotMatchExpr("st: $op2<-genExprSingle$"))) {
    dg = stuple(dg, op2, inExpr, ninExpr, st_info) /apply/
    (Value delegate(Scope) name, typeof(op2) op2, bool inExpr, bool ninExpr, string st_info, Scope sc) {
      auto str2 = name(sc).to!(string);
      Scope where;
      {
        exwrap("Cannot get second operand "~(inExpr?" in ":" at ")~sc.toString()~": "~st_info, {
          where = op2(sc).to!(Scope);
        });
      }
      // throw new Exception(Format("Test! ", where, " has ", where.statements, ": lookup ", str2));
      Scope.RecursionCheck rc;
      if (inExpr) {
        return Value(where.lookdown(str2, rc, false, true), false);
      } else if (ninExpr) {
        // logln("check ", str2, " not in ", where, ": ", !where.lookdown(str2, rc, false, true));
        return Value(!where.lookdown(str2, rc, false, true));
      } else {
        // throw new Exception(Format("Lookup ", str2, " in ", where.fqn, " .. ", where.lookup(str2, false)));
        return Value(where.lookup(str2, false)._0, false);
      }
    };
  }
  return true;
}

Value doMath(string OP)(Value delegate(Scope) op1, Value delegate(Scope) op2, Scope sc) {
  auto v1 = op1(sc), v2 = op2(sc);
  if (v1.isLike!(int) && v2.isLike!(int)) return Value(mixin("v1.to!(int) "~OP~" v2.to!(int)"));
  else return Value(mixin("v1.to!(float) "~OP~" v2.to!(float)"));
}

Value doBool(string OP)(Value delegate(Scope) op1, Value delegate(Scope) op2, Scope sc) {
  auto v1 = op1(sc);
  // short-circuiting.
  static if (OP == "&&") {
    if (!v1.to!(bool)) return v1;
    return op2(sc);
  } else static if (OP == "||") {
    if (v1.to!(bool)) return v1;
    return op2(sc);
  } else {
    auto v2 = op2(sc);
    return Value(mixin("v1.to!(bool) "~OP~" v2.to!(bool)"));
  }
}

bool genExprMath(ref string st, out Value delegate(Scope) dg, bool free = false, int depth = 0) {
  Value delegate(Scope) op2;
  const Steps = 3;
  if (!mixin(gotMatchExpr("st: $dg <- genExprSingle/free$"))) return false;
  if (depth < Steps) while (true) {
    auto backup = dg;
    if (mixin(gotMatchExpr("st: / $op2 <- genExprSingle/free$")))
      dg = stuple(dg, op2) /apply/ &doMath!("/");
    if (mixin(gotMatchExpr("st: * $op2 <- genExprSingle/free$")))
      dg = stuple(dg, op2) /apply/ &doMath!("*");
    if (backup == dg) break;
  }
  if (depth < Steps - 1) while (true) {
    auto backup = dg;
    if (mixin(gotMatchExpr("st: - $op2 <- genExprMath/free, Steps-1$")))
      dg = stuple(dg, op2) /apply/ &doMath!("-");
    if (mixin(gotMatchExpr("st: + $op2 <- genExprMath/free, Steps-1$")))
      dg = stuple(dg, op2) /apply/ &doMath!("+");
    if (backup == dg) break;
  }
  if (depth < Steps - 2) while (true) {
    auto backup = dg;
    if (mixin(gotMatchExpr("st: ~ $op2 <- genExprMath/free, Steps-2$")))
      dg = stuple(dg, op2) /apply/ (Value delegate(Scope) dg1, Value delegate(Scope) dg2, Scope sc) {
        auto v1 = dg1(sc), v2 = dg2(sc);
        if (!v1.isType!(string) || !v2.isType!(string))
          throw new Exception(Format("Can't concatenate non-strings: ", v1, " ~ ", v2));
        return Value(v1.to!(string) ~ v2.to!(string));
      };
    if (backup == dg) break;
  }
  return true;
}

bool genExprLogic(ref string st, out Value delegate(Scope) dg, bool free = false, int depth = 0) {
  Value delegate(Scope) op2;
  const Steps = 2;
  if (!mixin(gotMatchExpr("st: $dg <- genExprCompare/free$"))) return false;
  if (depth < Steps) while (true) {
    auto backup = dg;
    if (mixin(gotMatchExpr("st: [&&|and] $op2 <- genExprCompare/free$")))
      dg = stuple(dg, op2) /apply/ &doBool!("&&");
    if (backup == dg) break;
  }
  if (depth < Steps - 1) while (true) {
    auto backup = dg;
    if (mixin(gotMatchExpr("st: [\\|\\| |or] $op2 <- genExprLogic/free, Steps-1$")))
      dg = stuple(dg, op2) /apply/ &doBool!("||");
    if (backup == dg) break;
  }
  return true;
}

bool genExprCompare(ref string st, out Value delegate(Scope) dg, bool free = false) {
  if (!mixin(gotMatchExpr("st: $dg <- genExprMath/free$"))) return false;
  Value delegate(Scope) dg2;
  bool smaller, greater, eq, neq;
  if (mixin(gotMatchExpr("st: [==$#eq=true$|!=$#neq=true$|<=$#eq=smaller=true$|>=$#eq=greater=true$|<$#smaller=true$|>$#greater=true$] "
    "$dg2 <- genExprMath/false$"
  ))) {
    dg = stuple(dg, dg2, smaller, greater, eq, neq)
      /apply/ (typeof(dg) dg, typeof(dg2) dg2, bool smaller, bool greater, bool eq, bool neq, Scope sc) {
      auto v1 = dg(sc), v2 = dg2(sc);
      bool res, set;
      const string CODE = `
        // logln("Compare $1 (", $1, ") and $2 (", $2, ") for ", smaller?"<":"", greater?">":"", eq?"==":"", neq?"!=":"");
        if (smaller && $1 < $2) res = true;
        if (greater && $1 > $2) res = true;
        if (eq && $1 == $2) res = true;
        if (neq && $1 != $2) res = true;
      `;
      const string Table = `
                | int | float | bool | string
        --------+-----+-------+------+--------
        int     | x   | x     | x    |
        float   | x   | x     |      |
        bool    | x   |       | x    |
        string  |     |       |      | x`;
      mixin(ctTableUnrollColMajor(Table,
        `if (v1.isType!($COL)) {
          $BODY
        }`, `
        if (v2.isType!($ROW)) {
          static if ("$CELL" == "x") {
            mixin(CODE.ctReplace("$1", "v1.to!($COL)", "$2", "v2.to!($ROW)"));
            set = true;
          }
        }`
      ));
      if (!set) throw new Exception(Format("Cannot compare ", v1, " and ", v2, ": bad types"));
      return Value(res);
    };
  }
  return true;
}

alias genExprLogic genExpr;

Value eval(string st, Scope location) {
  Value delegate(Scope location) dg;
  if (!genExpr(st, dg)) throw new Exception("Cannot get value from "~st.next_text());
  return dg(location);
}

class IfStatement : Statement {
  Value delegate(Scope location) expr;
  string asBind;
  Statement branch1, branch2;
  static bool gotThis(ref string s, out IfStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: "
      "if $stmt.expr<-genExpr$ [as $stmt.asBind<-gotToken/TokenMode.Identifier$] "
        "$stmt.branch1$"
      " [else $stmt.branch2$]"
    ));
  }
  override {
    Scope eval(ref Text text, Scope location) {
      auto exvalue = expr(location);
      if (prot)
        text ~= Format("[debug: eval if @", location, " -> \"", exvalue, "\"] ");
      if (exvalue.to!(bool)) {
        auto ns = new Scope;
        ns.replant(location);
        ns.name = "_ifbody";
        ns.reparent;
        ns ~= branch1;
        if (asBind) {
          auto sc = exvalue.to!(Scope);
          if (!sc) throw new Exception("Can't bind if expression to "~asBind~": not a scope");
          if (prot)
            text ~= Format("[bind ", sc.fqn, " to ", asBind, "]");
          ns ~= new AliasStatement(asBind, sc.refstr);
        }
        return ns.eval(text, location);
      } else if (branch2) {
        auto ns = new Scope;
        ns.replant(location);
        ns.name = "_elsebody";
        ns ~= branch2;
        return ns.eval(text, location);
      }
      else return null;
    }
    mixin CopyDup;
    void iterate(void delegate(Statement) dg) { dg(branch1); if (branch2) dg(branch2); }
    Graph getInfo() { return Graph("IfStatement", branch1, branch2); }
  }
}

class ForeachStatement : Statement {
  string varname, countname, iterscope;
  bool visible, recursive;
  Statement loop;
  Value delegate(Scope) cond;
  Value delegate(Scope) recursionTest;
  string countVarId, countDestId;
  static bool gotThis(ref string s, out ForeachStatement stmt) {
    New(stmt);
    void exfail(string s) { throw new Exception(s~": "~*failtext.ptr()); }
    *failtext.ptr() = null;
    with (stmt) return mixin(gotMatchExpr("s: foreach "
      "[$countname<-gotToken/TokenMode.Name$, |$#countname=null$]"
      "[visible$#visible=true$]"
      "$varname<-gotToken/TokenMode.Name$: "
      "$iterscope<-gotToken/TokenMode.Identifier$"
      "["
        "[where $cond<-genExpr$ and ]count{[ to $countDestId<-gotToken/TokenMode.Name$| in $countVarId<-gotToken/TokenMode.Name$]}|"
        "where $cond<-genExpr$|"
        ""
      "]"
      "[recurse $#recursive=true$ [where $recursionTest<-genExpr$]]"
      `[where$#exfail("Failure to parse the 'where' block")$]`
      `[count$#exfail("Failure to parse the 'count' block")$]`
      " $loop$"
    ));
  }
  override {
    Scope eval(ref Text text, Scope location) {
      auto context = location.lookup(iterscope)._0;
      if (!context) throw new Exception(Format("Couldn't find target for foreach: ", iterscope, " in ", location, "!"));
      int count;
      Scope countVar, countDest;
      auto id = new Scope;
      id.name = countname;
      if (countVarId) {
        countVar = new Scope;
        countVar.name = countVarId;
      }
      Scope pass(bool blind) {
        int i;
        if (countVar) countVar.value = 0;
        Scope loopbody(Scope sc, out bool shouldRecurse) {
          auto al = new AliasStatement;
          al.name = varname;
          id.value = i++;
          auto lb = new Scope;
          lb ~= al;
          if (countname) lb ~= id;
          lb ~= loop.dup; // loop body
          lb.replant(location);
          lb.name = "_loopbody";
          lb.reparent;
          if (countVar) lb ~= countVar;
          if (countDest) lb ~= countDest;
          al.to = sc.refstr; // point loop variable at current target
          // logln("Alias now: ", al.name, " -> ", al.to);
          if (recursionTest) shouldRecurse = recursionTest(lb).to!(bool);
          if (visible && sc.hidden) return null;
          if (cond && !cond(lb).to!(bool)) return null;
          if (blind) ++count;
          else if (auto res = lb.eval(text, location)) return res;
          if (countVar) countVar.value = countVar.value().to!(int) + 1;
          return null;
        }
        Scope iterate(Scope context) {
          foreach (stmt; context.statements)
            if (auto sc = castToScope(stmt)) {
              bool shouldRecurse;
              if (auto res = loopbody(sc, shouldRecurse)) return res;
              if (recursive && shouldRecurse) {
                if (auto res = iterate(sc)) return res;
              }
            }
          return null;
        }
        return iterate(context);
        return null;
      }
      if (countDestId) {
        pass(true);
        countDest = new Scope;
        countDest.name = countDestId;
        countDest.value = count;
      }
      return pass(false);
    }
    mixin CopyDup;
    void iterate(void delegate(Statement) dg) { dg(loop); } // don't iterate every pass.
    Graph getInfo() { return Graph("ForeachStatement", loop); }
  }
}

class EnterStatement : Statement {
  string newpos;
  static bool gotThis(ref string s, out EnterStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: enter $stmt.newpos<-gotToken/TokenMode.Identifier$"));
  }
  override {
    Scope eval(ref Text text, Scope location) {
      Scope recurse(Scope dest) {
        if (!dest.area) throw new Exception("Error: cannot enter "~dest.toString()~": not an area!");
        foreach (sc; location.allNamed("onExit")) {
          if (prot) text ~= "[dbg evaluate onExit]";
          if (auto redir = sc.eval(text)) { location = sc; return recurse(redir); }
        }
        Scope.CurrentRoom.set(dest);
        foreach (sc; dest.allNamed("onEntry")) {
          if (prot)
            text ~= Format("[dbg evaluate onEntry @", newpos, " to ", dest.fqn, ". SC is ", sc.fqn, "]<br>");
          // THIS is WRONG.
          // the onEntry for the redirect will already have been called by its own entry statement.
          // A similar problem might exist with onExit. TODO check.
          // if (auto redir = sc.eval(text)) { location = sc; return recurse(redir); }
          if (auto redir = sc.eval(text)) return redir;
        }
        return dest;
      }
      if (auto dest = location.lookup(newpos)._0) {
        return recurse(dest);
      } else throw new Exception("Error: cannot enter location "~newpos~": not found");
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Enter: "~newpos); }
  }
}

class TermStatement : Statement {
  override {
    Scope eval(ref Text text, Scope location) {
      throw new Exception("Game terminated! ");
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Term"); }
  }
  static bool gotThis(ref string s, out TermStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: term"));
  }
}

string genSpec(string str, string spec) {
  return "auto __backup = "~str~"; scope(exit) "~spec~" = __backup[0 .. __backup.length - "~str~".length].strip();";
}

class MoveStatement : Statement {
  Value delegate(Scope) var, target;
  string spec;
  override {
    Scope eval(ref Text text, Scope location) {
      Scope item, dest;
      exwrap(Format("Cannot find source for '"~spec~"' in ", location), {
        item = var(location).to!(Scope);
      });
      exwrap("Cannot find destination for '"~spec~"'", {
        dest = target(location).to!(Scope);
      });
      if (!item.parent) throw new Exception("Cannot move root node! Alternately, a structural error has occured. In that case, panic. ");
      // logln("Remove ", item.info, " from ", item.parent.info);
      if (!item.parent.remove(item)) {
        throw new Exception(Format("Can't remove statement (not found): ", item, " in ", item.parent.fqn));
      }
      dest ~= item;
      item.replant(dest);
      return null;
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Move "~spec); }
  }
  static bool gotThis(ref string s, out MoveStatement stmt) {
    New(stmt);
    mixin(genSpec("s", "stmt.spec"));
    return mixin(gotMatchExpr("s: move $stmt.var<-genExprSingle$ $stmt.target<-genExprSingle$"));
  }
}

class CopyStatement : Statement {
  Value delegate(Scope) var, target;
  string spec;
  override {
    Scope eval(ref Text text, Scope location) {
      Scope item, dest;
      exwrap(Format("Cannot find source for '"~spec~"' in ", location), {
        item = var(location).to!(Scope);
      });
      exwrap("Cannot find destination for '"~spec~"'", {
        dest = target(location).to!(Scope);
      });
      item = cast(Scope) cast(void*) item.dup;
      dest ~= item;
      item.replant(dest);
      return null;
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Copy "~spec); }
  }
  static bool gotThis(ref string s, out CopyStatement stmt) {
    New(stmt);
    mixin(genSpec("s", "stmt.spec"));
    return mixin(gotMatchExpr("s: copy $stmt.var<-genExprSingle$ $stmt.target<-genExprSingle$"));
  }
}

void exwrap(lazy string msg, void delegate() dg) {
  try dg();
  catch (Exception ex) throw new Exception(msg~": "~ex.toString());
}

class DeleteStatement : Statement {
  Value delegate(Scope) var;
  string spec;
  override {
    Scope eval(ref Text text, Scope location) {
      Scope item;
      exwrap("Cannot '"~spec~"'", { item = var(location).to!(Scope); });
      if (!item.parent) throw new Exception("Cannot delete root! Alternately, a structural error has occured. In that case, panic. ");
      if (!item.parent.remove(item)) {
        throw new Exception(Format("Couldn't delete ", item, " from ", item.parent.fqn));
      }
      return null;
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Delete "~spec); }
  }
  static bool gotThis(ref string s, out DeleteStatement stmt) {
    New(stmt);
    mixin(genSpec("s", "stmt.spec"));
    return mixin(gotMatchExpr("s: delete $stmt.var<-genExprSingle$"));
  }
}

class InsertStatement : Statement {
  string var; Scope sc;
  override {
    Scope eval(ref Text text, Scope location) {
      auto area = location.lookup(var)._0;
      if (!area) throw new Exception("Cannot insert into "~var~": not found! ");
      auto nsc = cast(Scope) cast(void*) sc.dup;
      area ~= nsc;
      nsc.replant(area);
      return null;
    }
    mixin CopyDup;
    void iterate(void delegate(Statement) dg) { dg(sc); }
    Graph getInfo() { return Graph("Create "~var); }
  }
  static bool gotThis(ref string s, out InsertStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: insert")) /then_do/ {
      mixin(MatchExpr("s: $stmt.sc<-Scope.gotThis$ into $stmt.var$"));
    };
  }
}

class AssignStatement : Statement {
  string varname, value;
  override {
    string toString() { return Format("[Assign: "~varname~" = "~value~"]"); }
    Scope eval(ref Text text, Scope location) {
      bool visible;
      auto var = varname;
      if (auto rest = var.endsWith(".visible")) { var = rest; visible = true; }
      if (auto pos = location.lookup(var)._0) {
        if (visible) {
          pos.hidden = !.eval(value, location).to!(bool);
          foreach (ch; pos.allNamed("onVisibleChange")) {
            if (auto res = ch.eval(text, pos)) return res;
          }
        } else {
          pos.value = .eval(value, location);
          foreach (ch; pos.allNamed("onChange"))
            if (auto res = ch.eval(text, pos)) return res;
        }
        return null;
      } else throw new Exception("Cannot locate "~var);
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Assign "~varname~" <- "~value); }
  }
  static bool gotThis(ref string s, out AssignStatement stmt) {
    New(stmt);
    auto s2 = s;
    if (!mixin(gotMatchExpr("s2: $stmt.varname<-gotToken/TokenMode.Identifier$ = "))) return false;
    while (!s2.strip().startsWith(";")) {
      string token;
      if (!gotToken(s2, token)) {
        throw new Exception(Format("Failed to get token from ", s2.next_text()));
      }
      stmt.value ~= token;
    }
    // throw new Exception(Format("Matched assign to ", stmt.varname, " with ", stmt.value, "; left ", s2.next_text()));
    s = s2;
    return true; // may want to insert further code here later on
  }
}

static bool gotStmt(ref string st, out Statement stmt, bool acceptEmpty = false) {
  Scope sc; AssignStatement as; ListStatement ls; ValueStatement vs; SimpleStatement si;
  AliasStatement als; IfStatement ifs; MoveStatement ms; DeleteStatement ds; EnterStatement es;
  RegexStatement rs; TermStatement ts; LogStatement los; MergeStatement mes; ForeachStatement fos;
  AbortStatement ast; PragmaStatement pst; InsertStatement ist; CopyStatement cs;
  if (acceptEmpty) if (auto rest = st.startsWith(";")) {
    stmt = new Scope; /* empty */
    st = rest;
    return true;
  }
  auto s2 = st;
  // statements terminated by semicolon
  if (AssignStatement.gotThis(s2, as)) stmt = as;  else
  if (AliasStatement.gotThis(s2, als)) stmt = als; else
  if (RegexStatement.gotThis(s2, rs))  stmt = rs;  else
  if (MoveStatement.gotThis(s2, ms))   stmt = ms;  else
  if (CopyStatement.gotThis(s2, cs))   stmt = cs;  else
  if (DeleteStatement.gotThis(s2, ds)) stmt = ds;  else
  if (TermStatement.gotThis(s2, ts))   stmt = ts;  else
  if (LogStatement.gotThis(s2, los))   stmt = los; else
  if (AbortStatement.gotThis(s2, ast)) stmt = ast; else
  if (PragmaStatement.gotThis(s2, pst))stmt = pst; else
  if (EnterStatement.gotThis(s2, es))  stmt = es;  else
  if (InsertStatement.gotThis(s2, ist))stmt = ist; else
  if (IncludeStatement.gotThis(s2, stmt)) { }      else {
    // statements not terminated by semicolon
    if (ListStatement.gotThis(st, ls)) { stmt = ls; return true; }
    if (IfStatement.gotThis(st, ifs)) { stmt = ifs; return true; }
    if (ForeachStatement.gotThis(st, fos)) { stmt = fos; return true; }
    if (MergeStatement.gotThis(st, mes)) { stmt = mes; return true; }
    if (Scope.gotThis(st, sc)) { stmt = sc; return true; }
    // statements that _are_ terminated by semicolon; so sue me
    // OOPS!! MISTAKE, was a SimpleStatement all along ---v
    if (ValueStatement.gotThis(s2, vs) && !s2.startsWith(".")) { stmt = vs; } else {
      s2 = st; // put it back like it was, just to be sure
      // final catch-all
      if (SimpleStatement.gotThis(s2, si)) stmt = si;  else
      return false;
    }
  }
  if (!s2.accept(";"))
    throw new Exception(Format("Missing semicolon after matching ", stmt, " off ", st.next_text()~" ::: ["~s2[0]~"]"~s2[1..$].next_text()));
  st = s2;
  return true;
}

mixin Got!(string, gotToken, Statement, gotStmt);

final class ListStatement : Statement {
  Statement[] statements;
  override {
    Scope eval(ref Text text, Scope location) {
      if (prot)
        text ~= "[dbg eval list of "~Format(statements)~"]<br>";
      Scope res;
      auto tempstat = statements;
      Scope[] last;
      foreach (statement; tempstat) {
        if (auto sc = castToScope(statement)) { // like sub functions, they are not evaluated.
          if (sc.name == "finally") last ~= sc;
          continue;
        }
        if (auto r = statement.eval(text, location)) { res = r; break; }
      }
      foreach_reverse (sc; last)
        if (auto r = sc.eval(text, location)) { res = r; break; }
      return res;
    }
    Statement dup() {
      auto res = new ListStatement;
      res.statements = statements.dup;
      foreach (ref stmt; res.statements) stmt = stmt.dup;
      return res;
    }
    void iterate(void delegate(Statement) dg) {
      foreach (stmt; statements) dg(stmt);
    }
    Graph getInfo() { return Graph(Format("List: ", statements.length), statements); }
  }
  static bool gotThis(ref string s, out ListStatement stmt) {
    New(stmt);
    string s2 = s;
    if (!accept(s2, "{")) return false;
    while (true) {
      Statement st;
      if (gotStmt(s2, st, true)) {
        stmt.statements ~= st;
      } else break;
    }
    if (!accept(s2, "}")) return false;
    s = s2;
    return true;
  }
}

class ValueStatement : Statement {
  Value delegate(Scope) dg;
  this(Value v) { dg = v /apply/ (Value v, Scope sc) { return v; }; }
  this() { }
  string expr_str;
  override {
    Scope eval(ref Text text, Scope location) {
      auto v = dg(location);
      if (v.isType!(string)) {
        text ~= v.to!(string);
      } else throw new Exception("Cannot evaluate ValueStatement:"~Format(v.type));
      return null;
    }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph(toString); }
  }
  static bool gotValue(ref string s, out Value delegate(Scope) dg, out string expr_str) {
    string buf, s2 = s.strip();
    auto backup = s2;
    return s2.genExpr(dg, true) /then_do/ {
      expr_str = backup[0 .. $-s2.length];
      s = s2;
    };
  }
  static bool gotThis(ref string s, out typeof(this) stmt) {
    New(stmt);
    return gotValue(s, stmt.dg, stmt.expr_str);
  }
  string toString() { return Format("Value<"~expr_str~">"); }
}

class AliasStatement : Statement {
  string name, to;
  this(string a, string b) { name = a; to = b; }
  this() { }
  override {
    Scope eval(ref Text text, Scope location) { return null; }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Alias "~name~" = "~to); }
    string toString() {
      if (auto rest = to.startsWith("\n")) {
        return Format("Alias(", name, " =ref> ", Scope.refdecode(rest).info);
      } else {
        return Format("Alias(", name, " => ", to, ")");
      }
    }
  }
  static bool gotThis(ref string s, out AliasStatement stmt) {
    New(stmt);
    return mixin(gotMatchExpr("s: alias [$stmt.name$ = $stmt.to$ | $stmt.to$ $stmt.name$]"));
  }
}

class RegexStatement : Statement {
  Value delegate(Scope) from, to;
  string from_str, to_str;
  override {
    Scope eval(ref Text text, Scope location) { return null; }
    mixin InvarDup;
    void iterate(void delegate(Statement) dg) { }
    Graph getInfo() { return Graph("Regex substitute: "~from_str~" with "~to_str); }
  }
  static bool gotThis(ref string st, out RegexStatement stmt) {
    New(stmt);
    ValueStatement vst_from, vst_to;
    return mixin(gotMatchExpr("st: regex $vst_from<-ValueStatement.gotThis$ -> $vst_to<-ValueStatement.gotThis$")) /then_do/ {
      stmt.from = vst_from.dg; stmt.from_str = vst_from.expr_str;
      stmt.to   = vst_to.dg;   stmt.to_str   = vst_to.expr_str;
    };
  }
}

class StoredPos : Statement {
  Scope location; Statement sup;
  mixin This!("location, sup");
  override {
    Scope eval(ref Text text, Scope) { return sup.eval(text, location); }
    Statement dup() { assert(false); } // Just a temporary
    void iterate(void delegate(Statement) dg) { dg(sup); }
    Graph getInfo() { return Graph("SoftLink: ", sup); }
  }
}

TLS!(bool) stillParsing; // cheap hack but effective.
static this() { New(stillParsing, { return new bool; }); }

class MergeStatement : Statement {
  string from;
  Statement child;
  Scope src;
  bool checkingRightNow;
  void checkBuild(Scope location) {
    if (*stillParsing.ptr())
      throw new Exception("Tried to evaluate merge while still in parsing! This cannot work. ");
    if (src) return;
    // logln("Building merge at ", location.fqn, " for ", from);
    if (checkingRightNow) return;
    {
      checkingRightNow = true;
      scope(exit) checkingRightNow = false;
      src = castToScope(location.lookup(from)._0);
    }
    if (!src) throw new Exception(Format("Can't find source for merge: ", from, " in ", location));
    src = cast(Scope) cast(void*) src.dup;
    src.replant(location); // relink
    src.name = ""; // flatten, see flatIterate up top
    Statement param;
    if (!child) throw new Exception("Merge statement has no child! ");
    if (auto si = cast(SimpleStatement) child) { // named reference
      auto sc = location.lookup(si.name)._0;
      if (!sc) throw new Exception("Couldn't find target for simple ref "~si.name);
      auto as = new AliasStatement;
      as.name = "param";
      as.to = sc.fqn(); // absolute alias
      param = as;
    } else {
      auto ns = new Scope;
      ns.name = "param";
      ns.replant(location);
      // ns ~= new StoredPos(location, child);
      ns ~= child;
      param = ns;
    }
    src ~= param;
    // logln("src is now ", src, " @", src.fqn);
    foreach (ins; src.allNamed("onInstantiate")) {
      Text bogus;
      bogus.cache.length = 16; // not expecting much traffic here
      ins.eval(bogus);
    }
  }
  override {
    Scope eval(ref Text text, Scope location) {
      checkBuild(location);
      return src.eval(text);
    }
    Statement dup() {
      auto res = new MergeStatement;
      res.from = from; res.child = child.dup;
      return res;
    }
    string toString() { return Format(super.toString(), " [", src?src.statements:null, "]"); }
    void iterate(void delegate(Statement) dg) { if (src) dg(src); }
    Graph getInfo() { return Graph("Merge with param: "~from, child, src); }
  }
  static bool gotThis(ref string st, out MergeStatement stmt) {
    New(stmt);
    bool commit;
    return
      mixin(gotMatchExpr("st: $stmt.from$\\: $#commit=true$$stmt.child<-gotStmt/true$"))
      || {
        if (commit)
          throw new Exception(Format(
            "Couldn't parse merge at ", st.next_text(), ": ",
            "unable to parse child. "
          ));
        return false;
      }();
  }
}

const User = true;

class IncludeStatement {
  static bool gotThis(ref string st, out Statement stmt) {
    string src;
    bool req;
    return mixin(gotMatchExpr("st: [required$#req=true$] include $src<-gotLiteral$")) /then_do/ {
      auto text = readSrc(src, readMode).filterComments();
      Statement[] list;
      do {
        Statement st2;
        try {
          if (!mixin(gotMatchExpr("text: $st2$")))
            throw new Exception("Parse error!");
          text = text.strip();
          list ~= st2;
        } catch (Exception ex) {
          if (req) throw ex;
          list ~= new ValueStatement(Value(Format("Failed to include ", src, ": ", ex)));
          text = null;
        }
      } while (text.length);
      auto sc = new Scope;
      foreach (entry; list) sc ~= entry;
      sc.reparent;
      stmt = sc;
    };
  }
}

struct Bloom(T) {
  T value;
  void insert(T t) { value |= t; }
  bool opIn_r(T t) { return (value & t) == t; }
  void reset() { value = 0; }
}

/// WARNING don't inherit from this!! It will break fastcasts
final class Scope : Statement {
  import tools.threads;
  static TLS!(Scope) CurrentRoom;
  static this() { New(CurrentRoom, { return cast(Scope) null; }); }
  string name;
  Value _value;
  Value value() {
    if (initializer) {
      _value = initializer(this);
      initializer = null;
    }
    return _value;
  }
  Value value(Value v) { return _value = v; }
  Value value(int i) { _value = i; return _value; }
  Value delegate(Scope) initializer;
  string[] aliases;
  bool isNamed(string s) {
    if (name == s) return true;
    foreach (al; aliases) if (al == s) return true;
    return false;
  }
  Scope parent;
  void replant(Scope newPlace) {
    parent = newPlace;
    fqnCache = null;
  }
  // user can't recurse across area. user can't see hidden. user can always see globals below root. (inventory and suchlike)
  bool area, hidden, global, local;
  Statement sub;
  static struct ScopeNameIterator {
    Scope parent;
    string match;
    int opApply(int delegate(ref Scope) dg) {
      foreach (stmt; parent.statements) {
        if (auto mst = cast(MergeStatement) stmt) {
          mst.checkBuild(parent);
          if (!mst.checkingRightNow)
            foreach (sc; mst.src.allNamed(match))
              if (auto res = dg(sc)) return res;
        }
        if (auto sc = castToScope(stmt)) {
          if (sc.isNamed(match))
            if (auto res = dg(sc)) return res;
        }
      }
      return 0;
    }
  }
  ScopeNameIterator allNamed(string name) {
    ScopeNameIterator res;
    res.parent = this;
    res.match = name;
    return res;
  }
  Scope opIn_r(string name) {
    foreach (sc; allNamed(name)) return sc;
    return null;
  }
  void opCatAssign(Statement st) {
    scope(exit) reparent;
    if (!st) asm { int 3; }
    if (auto list = castToListStatement(sub)) {
      list.statements ~= st;
    } else {
      if (sub) {
        auto nsub = new ListStatement;
        nsub.statements ~= sub;
        nsub.statements ~= st;
        sub = nsub;
      } else sub = st;
    }
  }
  static bool gotThis(ref string s, out Scope sc) {
    New(sc);
    bool match;
    auto s2 = s;
    string al;
    if (!mixin(gotMatchExpr(
      "s2: [global $#sc.global = true, match = true$ | local $#sc.local = true, match = true$ |]"
      "{[area $#sc.area=true, match = true$ |#$#sc.hidden = true, match = true$]}"
      "$sc.name<-gotToken/TokenMode.Name$ [($al<-gotToken/TokenMode.Name#sc.aliases~=al${, $al<-gotToken/TokenMode.Name#sc.aliases~=al$})]"
      "[ = $sc.initializer<-genExpr$]"
    ))) {
      return false;
    }
    if (sc.name == "alias") // TODO extend
      throw new Exception("Forbidden identifier: "~sc.name);
    // if one of the qualifiers for scopes match, we don't need actual brackets to know we're a scope.
    // this allows #foo; variable declarations.
    if (!match) {
      if (!mixin(gotMatchExpr("s2: $sc.sub<-gotStmt$"))) return false;
    } else {
      if (!mixin(gotMatchExpr("s2: [;|$sc.sub<-gotStmt$]"))) return false;
    }
    s = s2;
    sc.reparent;
    return true;
  }
  static void getThis(ref string s, out Scope sc) {
    try _getThis(s, sc);
    catch (Exception ex) throw new Exception(Format("Failed to parse Scope: ", ex, ". "));
  }
  static void _getThis(ref string s, out Scope sc) {
    New(sc);
    bool matched;
    auto s2 = s;
    string al;
    mixin(MatchExpr(
      "s2: [global $#sc.global = true, matched = true$ | local $#sc.local = true, matched = true$ |]"
      "[area $#sc.area=true, matched = true$] [#$#sc.hidden = true, matched = true$]"
      "$sc.name<-gotToken/TokenMode.Name$ [($al<-gotToken/TokenMode.Name#sc.aliases~=al${, $al<-gotToken/TokenMode.Name#sc.aliases~=al$})]"
      "[ = $sc.initializer<-genExpr$]"
    ));
    // if one of the qualifiers for scopes match, we don't need actual brackets to know we're a scope.
    // this allows #foo; variable declarations.
    if (!matched) {
      mixin(MatchExpr("s2: $sc.sub<-gotStmt$"));
    } else {
      mixin(MatchExpr("s2: [;|$sc.sub<-gotStmt$]"));
    }
    s = s2;
    sc.reparent;
  }
  Statement[] StmtCache;
  Statement[] statements() {
    auto res = flatten(this, StmtCache);
    /*if (parent) {
      auto as = new AliasStatement;
      as.name = "parent"; as.to = parent.fqn();
      res ~= as;
    }*/
    return res;
  }
  Statement[] nonflatStatements() {
    if (auto ls = castToListStatement(sub))
      return ls.statements;
    return (&sub)[0 .. 1];
  }
  void reparent() {
    foreach (entry; statements) {
      if (auto scp = castToScope(entry))
        scp.replant(this);
    }
  }
  // breaks on gdc with final
  // override {
    Scope eval(ref Text text, Scope location = null) {
      if (prot)
        text ~= "[scope '"~name~"':"~value().to!(string)~"]<br>";
      scope(exit) if (prot) text ~= "[/scope "~name~"]";
      // if (!location) location = this;
      location = this;
      try {
        foreach (statement; statements) {
          if (auto sc = castToScope(statement)) {
            if (sc.isNamed("default")) {
              return sc.eval(text, location);
            }
          }
        }
        if (sub) return sub.eval(text, location); // fallback to sequence evaluation
        else return null;
      } catch (ReturnException rex) {
        return null;
      }
    }
    Statement dup() {
      auto res = new Scope;
      res.name = name; res.value = value; res.aliases = aliases;
      res.parent = parent; res.area = area; res.hidden = hidden;
      res.fqnCache = fqnCache; res.StmtCache = StmtCache.dup; // no reason to rebuild this
      if (sub) res.sub = sub.dup();
      res.reparent;
      return res;
    }
    void iterate(void delegate(Statement) dg) { if (sub) dg(sub); }
    Graph getInfo() {
      return Graph(Format("Scope ", name, ": ", value,
        area?"area ":"", hidden?"hidden ":"", global?"global ":"", local?"local ":""
      ), sub);
    }
  // }
  string fqnCache;
  string fqn() {
    if (!parent) return name;
    else {
      if (!fqnCache) fqnCache = parent.fqn() ~ "." ~ name;
      return fqnCache;
    }
  }
  string info() {
    string res = fqn();
    bool set;
    void brk() {
      if (!set) res ~= ": ";
      else res ~= ", ";
      set = true;
    }
    foreach (stmt; statements) {
      if (auto sc = castToScope(stmt)) {
        brk();
        res ~= sc.name;
      }
      if (auto al = cast(AliasStatement) stmt) {
        brk();
        res ~= "("; res ~= al.toString(); res ~= ")";
      }
    }
    return res;
  }
  static Scope refdecode(string id) { return cast(Scope) *(cast(void**) id.ptr); }
  string refstr() {
    // no way to get \n into a token from source
    auto foo = cast(void*) this;
    return "\n"~(cast(char*) &foo)[0 .. size_t.sizeof];
  }
  Scope root() { if (parent) return parent.root(); else return this; }
  static struct RecursionCheck {
    Bloom!(size_t) firstCheck;
    bool[Stuple!(Scope, string)] carefulCheck;
    bool beCareful; // something triggered the bloom filter. Let's be careful.
    size_t fastkey(Scope sc, string var) {
      // left shift the pointer by half; fill up some of those high bits
      return ((cast(size_t) var.ptr) << (size_t.sizeof * 4)) ^ (cast(size_t) sc) ^ var.length;
    }
    alias Stuple!(typeof(firstCheck), bool) BackupData;
    void backup(ref BackupData bd) { bd = stuple(firstCheck, beCareful); }
    void restore(ref BackupData bd) {
      firstCheck = bd._0;
      beCareful = bd._1;
    }
    bool seen(Scope sc, string var) {
      // logln("Check ", sc.fqn, " and ", var);
      if (fastkey(sc, var) in firstCheck) {
        beCareful = true;
      } else return false;
      return !!(stuple(sc, var) in carefulCheck);
    }
    void insert(Scope sc, string var) {
      // logln("Insert ", sc.fqn, " and ", var);
      auto fk = fastkey(sc, var);
      firstCheck.insert(fk);
      if (!beCareful) return;
      carefulCheck[stuple(sc, var)] = true;
    }
    void remove(Scope sc, string var) { if (beCareful) carefulCheck.remove(stuple(sc, var)); }
  }
  /// below: only recurse downwards
  Scope lookdown(string var, ref RecursionCheck visited, bool userquery = false, bool below = false) {
    if (!var.length) {
      if (userquery && "failLookup" in this) return null;
      return this;
    }
    if (visited.seen(this, var)) {
      throw new Exception(Format("Infinite recursion in lookdown request for ", var, " @", fqn(), "!"));
    }
    RecursionCheck.BackupData bd = void;
    visited.insert(this, var);
    visited.backup(bd);
    auto var_backup = var;
    scope(exit) {
      // undo bloom change
      visited.restore(bd);
      // remove AA change
      visited.remove(this, var_backup);
    }
    // if (fqn().find("loopbody") != -1) logln("statements ", statements);
    auto first = var.slice(".");
    // logln("Lookdown in ", fqn(), "; ", first, " -> ", var);
    if (first == "this") return this.lookdown(var, visited, userquery, below);
    else if (first == "instance") {
      if (!name.length) return lookdown(var, visited, userquery, below);
      else return null; 
    } else if (first == "location") return (
        CurrentRoom.ptr() /ifnull/ { throw new Exception("No current room set in this thread! "); }
      ).lookdown(var, visited, userquery, below);
    else if (!userquery && first == "parent") {
      auto par = parent;
      while (par && par.name == "") par = par.parent;
      if (!par) throw new Exception("Couldn't find parent in "~fqn());
      return par.lookdown(var, visited, userquery, below);
    } else if (first == "here") {
      auto cur = this;
      while (true) {
        if (cur.area) return cur.lookdown(var, visited, userquery, below);
        else if (cur.parent) cur = cur.parent;
        else throw new Exception("Structural violation: root must be area! ");
      }
      return cur.lookdown(var, visited, userquery, below);
    } else if (!below && isNamed(first)) return lookdown(var, visited, userquery, below);
    else {
      Scope[] fallbacks;
      Scope loopback;
      foreach (entry; statements) {
        if (auto mst = cast(MergeStatement) entry) {
          // logln("!! Forcing merge of ", mst.from, " @", fqn(), " due to lookup of ", first, " -> ", var);
          mst.checkBuild(this);
          // pure recursion
          if (!mst.checkingRightNow) // a scope can't depend on itself
            if (auto res = mst.src.lookdown(var_backup, visited, userquery, below)) return res;
        }
        if (auto ast = cast(AliasStatement) entry) {
          // logln("alias: ", ast.name, " -> ", ast.to);
          // logln("check for ", first, " ~ ", var);
          if (ast.name == first) {
            auto intermed = lookup(ast.to)._0;
            // invalid aliases can have a valid role.
            // if (!intermed) throw new Exception("Invalid alias: "~ast.to~" does not exist under "~fqn());
            if (intermed) return intermed.lookdown(var, visited, userquery);
          }
        }
        if (auto sc = castToScope(entry)) {
          // logln("Considering scope ", sc);
          if (sc.isNamed("failLookup")) return null;
          if (userquery) {
            if (sc.area || sc.hidden) continue;
            if (sc.isNamed("anys")) {
              if (isNamed("anys")) throw new Exception("Cannot nest an anys into an anys! ");
              loopback = sc;
              loopback.value = Value(""[]);
            }
            if (isNamed("anys")) {
              loopback = this;
            }
            if (sc.isNamed("any")) {
              fallbacks ~= sc;
            }
          }
          if (sc.isNamed(first)) {
            if (auto res = sc.lookdown(var, visited, userquery)) return res;
          }
        }
      }
      // Multiple fallbacks because instances might introduce more than one "any" block
      if (fallbacks) {
        foreach (fallback; fallbacks) if (auto res = fallback.lookdown(var, visited, userquery)) {
          fallback.value = Value(first);
          return res;
        }
        return null;
      }
      if (loopback) {
        string sep;
        if (loopback.value().to!(string).length) sep = " ";
        loopback.value = Value(loopback.value().to!(string) ~ sep ~ first);
        return loopback.lookdown(var, visited, userquery);
      }
    }
    return null;
  }
  Stuple!(Scope, bool) lookup(string name, bool userquery = false, RecursionCheck* visitedX = null) { // also recurse upwards
    // DIRECT REFERENCE LOOKUP, DANGER, HACK .. my editor highlights those words; which is fun
    if (auto num = name.startsWith("\n")) {
      return stuple(Scope.refdecode(num), false);
    }
    // logln("Lookup in ", info(), "; var:", name, ":");
    RecursionCheck visited;
    auto vp = &visited;
    if (visitedX) vp = visitedX;
    if (auto res = lookdown(name, *vp, userquery)) return stuple(res, false);
    if (parent)
      if (!userquery || !parent.area) return parent.lookup(name, userquery, vp);
    Stuple!(Scope, bool) handleRegex(RegexStatement re, Scope sc) {
      auto from = re.from(sc).to!(string), to = re.to(sc).to!(string);
      auto alt = regex_sub(name, from, to);
      if (alt != name) {
        auto res = lookup(alt, userquery, vp);
        if (res._0) return res;
      }
      return stuple(cast(Scope) null, false);
    }
    foreach (st; root.statements) {
      if (auto sc = castToScope(st)) {
        if (sc.global) {
          auto rest = name, first = rest.slice(".");
          if (auto res = sc.lookdown(first, *vp, userquery)) {
            if (!res.local) {
              if (auto res2 = res.lookdown(rest, *vp, userquery)) return stuple(res2, true);
            }
          }
          // if (auto res = sc.lookdown(name, *vp, userquery)) return stuple(res, true);
          foreach (st2; sc.statements) {
            if (auto re = cast(RegexStatement) st2) {
              auto res = handleRegex(re, sc);
              if (res._0) return res;
            }
          }
        }
      }
      if (auto re = cast(RegexStatement) st) {
        auto res = handleRegex(re, root);
        if (res._0) return res;
      }
    }
    return stuple(cast(Scope) null, false);
  }
  bool remove(Statement st) {
    bool removed;
    int i1;
    auto list = nonflatStatements;
    for (int i2 = 0; i2 < list.length; ++i2) {
      while (i2 < list.length && list[i2] is st) { removed = true; i2++; }
      if (i2 == list.length) break;
      // logln("skip-overwrite ", list[i1], " with ", list[i2]);
      if (i1 != i2) list[i1] = list[i2];
      i1++;
    }
    list = list[0 .. i1];
    if (!list.length) sub = null;
    else if (list.length == 1) sub = list[0];
    else castToListStatement(sub).statements = list;
    foreach (stmt; list) {
      if (auto mst = cast(MergeStatement) stmt) {
        if (mst.src && mst.src.remove(st)) removed = true;
      }
    }
    return removed;
  }
  // string toString() { return "scope{"~name~", "~Format(statements)~"}"; }
  string toString() { return "scope{"~fqn~"}"; }
}
