module vars;

import irc, tools.base, std.string;
import tools.ini, tools.base;

iniFile variables;
static this() { New(variables, "bot_vars.txt"); }

string getVariable(nickname nick, string name) {
  synchronized(variables) {
    return variables.get!(string)(cast(string) nick, name, null);
  }
}

void setVariable(nickname nick, string name, string value) {
  synchronized(variables) {
    logln("Set: section ", cast(string) nick, ", name ", name, ", value ", value, "|");
    variables.set!(string)(cast(string) nick, name, value);
  }
}

bool delVariable(nickname nick, string name) {
  synchronized (variables) return variables.del(cast(string) nick, name);
}

void doInit(T)(ref T t) { t = Init!(T); }

bool _isCharacter(char c) {
  return ((c>='a' && c<='z') || (c>='A' && c<='Z'));
}

// function version of CarefulReplace
string var_expand(string text, string var, string val) {
  auto pos = text.find(var);
  if (pos == -1) return text;
  else {
    if (!pos) return val ~ var_expand(text[var.length .. $], var, val);
    else if (pos + var.length == text.length) {
      if (text[pos-1] == '.') return text;
      else return var_expand(text[0 .. $-var.length], var, val) ~ val;
    } else {
      if (!_isCharacter(text[pos-1]) && !_isCharacter(text[pos + var.length]) && text[pos-1] != '.')
        return text[0 .. pos] ~ val ~ var_expand(text[pos+var.length .. $], var, val);
      else return text[0 .. pos+var.length] ~ var_expand(text[pos+var.length .. $], var, val);
    }
  }
}

string fnbase(string f) {
  if (f.find("(") == -1) return f;
  else return f[0 .. f.find("(")];
}

string fnpar(string f) {
  if (f.find("(") == -1 || f.find(")") == -1 || f.find("(") > f.find(")")) return null;
  else return f[f.find("(")+1 .. f.find(")")];
}

T top(T)(T[] array) { return array[$-1]; }

class CallTree {
  struct Child {
    string leaf; // is always set
    CallTree branch; // is sometimes set
    string toString() { string res = leaf; if (branch) res ~= "[" ~ branch.toString() ~ "]"; return res; }
    string toOriginalString() { string res = leaf; if (branch) res ~= "(" ~ branch.toOriginalString() ~ ")"; return res; }
  }
  Child[] children;
  this() { }
  this(Child ch) { children = [ch]; }
  string toOriginalString() {
    string res;
    bool first = true;
    foreach (child; children) {
      if (first) first = false; 
      else res ~= ", ";
      res ~= child.leaf;
      if (child.branch) res ~= "(" ~ child.branch.toOriginalString() ~ ")";
    }
    return res;
  }
  string toString() {
    string res;
    bool first = true;
    foreach (child; children) {
      if (first) first = false; 
      else res ~= "|";
      res ~= child.leaf;
      if (child.branch) res ~= "[" ~ child.branch.toString() ~ "]";
    }
    return res;
  }
  void prune(bool delegate(string) dg) {
    int i = 0;
    while (i < children.length) {
      if (dg(children[i].leaf)) removeFrom(children, i);
      else if (children[i].branch) {
        children[i].branch.prune(dg);
        // if (!children[i].branch.children.length) removeFrom(children, i);
      } else i++;
    }
  }
}

CallTree.Child glomp_child(ref string s, bool glomp_all = false) {
  CallTree.Child res;
  CallTree[] stack;
  bool ate_a_bit;
  while (s.length) {
    s = s.strip();
    if (s.startsWith("(")) { s = s[1 .. $]; stack ~= new CallTree; }
    else if (s.startsWith(")")) {
      s = s[1 .. $];
      if (!stack.length) throw new Exception("Too many closing brackets in "~s);
      if (stack.length > 1) {
        with (stack[$-2]) {
          if (!children.length || !children[$-1].leaf) throw new Exception("Opening bracket out of nowhere!");
          if (children[$-1].branch) throw new Exception("Double ()!");
        }
        stack[$-2].children[$-1].branch = stack.top();
        stack = stack[0 .. $-1];
      } else {
        if (res.branch) throw new Exception("internal Error; glomped too much");
        res.branch = stack[0]; stack.length = 0;
      }
    } else {
      // eat as little as we can
      if (ate_a_bit && !stack.length) break; // done with glomping ex
      CallTree.Child* target;
      if (stack.length) with (stack.top()) {
        bool new_ch;
        if (auto rest = s.startsWith(",")) {
          s = rest.strip();
          new_ch = true;
        }
        if (new_ch || !children.length || children[$-1].branch) {
          children ~= Child(null, null);
        }
        target = &children[$-1];
      } else target = &res;
      string tk;
      if (glomp_all) {
        while (s.length) {
          if (s[0] == '(' || s[0] == ')' || s[0] == ',') break;
          tk ~= s[0];
          s = s[1 .. $];
        }
      } else {
        tk = s.getToken();
        s = s[tk.length .. $];
      }
      // plus the interrupting character, if nothing else was glomped
      // if (s.length && !tk.length) { tk ~= s[0]; s = s[1 .. $]; }
      target.leaf ~= tk;
      ate_a_bit = true;
    }
  }
  if (stack.length) throw new Exception("Bracket mismatch in "~s);
  return res;
}

extern(C) extern string CTRL(string channel);
void set(Query query) {
  if (!query.param.length) {
    query.answer(CTRL(query.channel), "set variable value");
    return;
  }
  auto rest = query.param;
  rest = rest.strip();
  auto basecall = glomp_child(rest);
  rest = rest.strip();
  if (!rest.length) throw new Exception("Invalid parameter form for set: "~query.param);
  basecall.leaf = basecall.leaf.strip();
  setVariable(query.name, basecall.toOriginalString(), rest);
  query.answer("Variable set: "~basecall.toOriginalString()~" = "~rest);
}

void unset(Query query) {
  auto var = query.param.strip();
  if (!var.length) {
    query.answer(CTRL(query.channel), "unset variable");
    return;
  }
  if (delVariable(query.name, var))
    query.answer("Variable removed: "~var);
  else query.answer("Variable not found!");
}

import tools.functional, tools.serialize;
void show(Query query) {
  if (!query.param.length) {
    query.answer(CTRL(query.channel), "show variable; available [" ~ (variables.section(cast(string) query.name)
      /map/ (string s) { return s.split("=")[0].strip(); }).join(", ") ~ "]"
    );
    return;
  }
  string val, call;
  auto mine = variables.section(cast(string) query.name);
  foreach (line; mine) {
    auto parts = line.split("="), key = parts[0], value = parts[1 .. $].join("=");
    value = deserialize!(string)(value);
    if (key.startsWith(query.param)) {
      call = key;
      val = value;
      break;
    }
  }
  if (!val) {
    query.answer("no such variable");
    return;
  }
  query.answer(call~" -> "~val);
}

struct LimitQuery {
  Query sup;
  int depth;
}

void exec(Query query) {
  if (!query.param.length) {
    query.answer(CTRL(query.channel), "eval variable");
    return;
  }
  string param_tmp = query.param, res = eval(glomp_child(param_tmp, true));
  if (!res) res = query.param;
  res = res.strip();
  // logln("exec res: ", res, " and separator ", CTRL(query.channel), " for ", query.channel);
  if (auto cmd2 = res.startsWith(CTRL(query.channel))) {
    if (query.depth > 128) throw new Exception("Recursion limit (128) exceeded. Please simplify your queries or cease to attempt to crash the bot.");
    auto subquery = query;
    subquery.param = cmd2.strip();
    subquery.depth ++;
    // logln("runQuery(", subquery, ") as compared to (", query, ")");
    runQuery(subquery);
  } else {
    query.answer(res);
  }
}

string eval(CallTree.Child call_ch) {
  logln("Evaluate ", call_ch.toString());
  string res, fmt;
  auto name = *current_nick();
  auto mine = variables.section(cast(string) name);
  foreach (line; mine) {
    auto parts = line.split("="), key = parts[0], value = parts[1 .. $].join("=");
    value = deserialize!(string)(value);
    if (key.split("(")[0].strip() == call_ch.leaf) {
      fmt = key;
      res = value.dup;
      break;
    }
  }
  if (!res) {
    logln("No such variable: ", call_ch.toString(), " in ", mine);
    // return null;
    return call_ch.toString();
  }
  logln("eval: fmt: ", fmt);
  auto fnch = glomp_child(fmt, true); // how is it called
  if (!fnch.branch) {
    logln("No call; returning value: ", res);
    return res;
    // return fnch.leaf;
  }
  if (fnch.branch.children.length != call_ch.branch.children.length)
    throw new Exception("invalid parameter count in "~call_ch.toString());
  foreach (i, param; fnch.branch.children) {
    if (param.branch) throw new Exception("nested parameter in "~fnch.toString()~", wtf");
    with (call_ch.branch.children[i]) {
      string /*target = eval(call_ch.branch.children[i]);
      if (!target) */target = call_ch.branch.children[i].toString();
      // logln("eval: nested evaluation ", toString(), " -> ", target);
      logln("eval: skipped nested eval for ", target);
      res = res.var_expand(param.leaf, target);
    }
  }
  logln("  eval: Result ", res);
  return res;
}

static this() {
  commands["set"] = &set /todg;
  commands["unset"] = &unset /todg;
  commands["eval"] = &exec /todg;
  commands["show"] = &show /todg;
}
