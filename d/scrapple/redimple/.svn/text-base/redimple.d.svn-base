module redimple;
import tools.mersenne, tools.compat: logf = log;

bool equal(T)(T a, T b) { static if(is(T: Object)) return a is b; else return a==b; }

bool has(T)(T[] array, T elem) {
  foreach (e; array) if (equal(e, elem)) return true;
  return false;
}

int search(T)(T[] array, T[] match) {
  if (array.length<match.length) return -1;
  foreach (id, elem; array[0..$-match.length+1]) if (array[id..id+match.length]==match) return id;
  return -1;
}

void remove(T)(ref T[] array, T match) {
  if (array.length==1) { array.length=0; return; }
  size_t pos=size_t.max; foreach (i, elem; array) if (elem==match) pos=i;
  if (pos==size_t.max) { writefln("Can't remove - not in array!"); asm { int 3; } }
  array[pos]=array[$-1]; array.length=array.length-1;
}

T pop(T)(ref T[] array) { T res=array[$-1]; array=array[0..$-1]; return res; }
typeof(T+U) max(T, U)(T a, U b) { return a>b?a:b; }

import tools.base: stuple, Stuple, startsWith;
class Graph(T) {
  T[] nodes;
  T[][T] edges;
  size_t _incoming[T];
  void addNode(T node) { nodes~=node; }
  void addEdge(T src, T dest) {
    if (!(dest in _incoming)) _incoming[dest]=1; _incoming[dest]++;
    if (!nodes.has(src)) addNode(src);
    if (!nodes.has(dest)) addNode(dest);
    if (!(src in edges)) edges[src]=[];
    if (!edges[src].has(dest)) edges[src]~=dest;
  }
  string[Stuple!(T, T)] annotate;
  void addEdge(T src, T dest, string anno) {
    addEdge(src, dest);
    annotate[stuple(src, dest)] = anno;
  }
  Graph dup() {
    auto res=new Graph;
    res.nodes=nodes.dup;
    foreach (id, entry; edges) { res.edges[id]=entry.dup; }
    return res;
  }
  void rmEdge(T src, T dest) {
    _incoming[dest]--;
    if (!edges[src].length) return;
    edges[src][edges[src].search([dest])]=edges[src][$-1];
    edges[src]=edges[src][0..$-1];
  }
  char[] toString() {
    char[] res="{";
    foreach (id, entry; edges) res~=format(id, "->", entry, ", ");
    if (res.length<2) return res~"}";
    return res[0..$-2]~"}";
  }
  size_t incoming(T node) { if (!(node in _incoming)) return 0; return _incoming[node]; }
  size_t outgoing(T node) { return edges[node].length; }
}

alias Graph!(string) G;

/// Tarjan algorithm copied from Wikipedia. Thanks guys :)
T[][] tarjan(T)(Graph!(T) G, T start) {
  auto max_dfs=0; // counter for dfs
  T[] stack;
  T[][] res;
  size_t[T] dfs, lowlink;
  void _tarjan(T v) {
    dfs[v]=lowlink[v]=max_dfs++;
    stack~=v;
    T[] dests=[];
    if (auto p = v in G.edges) dests = *p;
    foreach (dest; dests) {
      if (dest /notin/ dfs) {
        _tarjan(dest); 
        if (lowlink[dest]<lowlink[v]) lowlink[v]=lowlink[dest];
      } else if (stack.has(dest)) //if (dfs[dest]<lowlink[v]) lowlink[v]=dfs[dest];
        if (lowlink[dest] < lowlink[v]) lowlink[v] = lowlink[dest];
    }
    if (lowlink[v]==dfs[v]) { // the root of a strongly connected component
      res.length=res.length+1;
      T _v;
      do { _v=stack.pop(); res[$-1]~=_v; } while (v!=_v);
    }
  }
  _tarjan(start);
  return res;
}

import tools.functional;

interface excluder { bool opCall(string from, string to); }

struct Import { string mod, what; }

Import[] imports(string mod, excluder xc) {
  auto filename=modToFile(mod);
  if (!filename.exists()) throw new Exception("File "~filename~" cannot be found");
  char[] rid=format(rand());
  auto list = (
    (cast(string)filename.read()).split(";") /map/ &strip
    /select/ (string s) { s = s.strip(); return !!s.startsWith("import") || (s.find(" import ") != -1); }
    /map/ (string s) {
      bool pub;
      if (s.startsWith("public ")) pub = true;
      s = s[s.find("import")+6 .. $];
      Import[] foo;
      auto sel = s.find(":");
      string what;
      if (sel != -1) {
        what = s[sel+1 .. $];
        s = s[0 .. sel];
      }
      foreach (part; s.split(","))
        foo ~= Import(part.strip(), pub?"#"[]:""[]);
      foo[$-1].what ~= what.strip();
      return foo;
    }
  );
  Import[] foo;
  foreach (sub; list) foreach (i; sub) {
    if (!i.mod.length) continue;
    if (!xc(mod, i.mod)) continue;
    foo ~= i;
  }
  return foo;
}

bool java_mode;

string modToFile(string mod) {
  auto res=mod.replace(".", "/");
  if (java_mode) return res~".java";
  else return res~".d";
}

//void log(char[] l) { writefln(l); }
//void log(char[] l) { system("echo "~quote(l)~" >> /tmp/gdcgraph.log"); }
void log(char[] l) { }

G genJavaImportsGraph(char[] mod, excluder xc) {
  auto files = listdir(".", "*.java") /map/ (string s) { return s[s.rfind("/")+1 .. $].endsWith(".java"); }
  /select/ (string s) { return xc("", s); };
  auto res = new G;
  foreach (string file; files) {
    res.addNode(file);
  }
  foreach (string file; files) {
    auto data = cast(string) (file~".java").read();
    foreach (string file2; files) {
      if (file2 == file) continue;
      if (data.find(file2) != -1) {
        res.addEdge(file, file2);
      }
    }
  }
  return res;
}

G genImportsGraph(string mod, excluder xc, bool incExtNodes=false) {
  if (java_mode) return genJavaImportsGraph(mod, xc);
  auto res=new G;
  string[] scanned;
  int depth=0;
  void recurse(string mod) {
    depth++; scope(exit) depth--;
    log("recurse into "~mod~" at level "~format(depth));
    scope(exit) log("done");
    if (scanned.has(mod)) return;
    scanned~=mod;
    res.addNode(mod);
    foreach (entry; mod.imports(xc)) {
      if (mod == entry.mod) continue; 
      if (modToFile(entry.mod).exists()) {
        log("  "~mod~" -> "~entry.mod);
        res.addEdge(mod, entry.mod, entry.what);
        recurse(entry.mod);
      } else { log(modToFile(entry.mod)~" doesn't exist"); if (incExtNodes) res.addEdge(mod, entry.mod, entry.what); }
    }
  }
  mod.recurse();
  log("Graph generated");
  return res;
}

class Package {
  string name;
  Package[string] pkgs;
  string[] modules;
  this(string name, G graph) {
    log("Creating package "~name);
    this.name=name;
    foreach (entry; graph.nodes) {
      if (entry.length!>name.length) continue;
      if (entry[0..name.length]!=name) continue;
      char[] subname=entry[name.length..$];
      if (subname[0]=='.') subname=subname[1..$];
      auto dotpos=subname.find(".");
      if ((dotpos!=-1)&&!(subname[0..dotpos] in pkgs)) pkgs[subname[0..dotpos]]=new Package((name.length?name~".":"")~subname[0..dotpos], graph);
      else if (name.length||(dotpos==-1)) modules~=entry;
    }
  }
}

char[] quote(char[] e) { return '"'~e~'"'; }

char[] rmPkg(char[] inp) { if (inp.find(".")==-1) return inp; return inp[inp.rfind(".")+1..$]; }

bool leftright=false;

string getsettings(string mod) {
  string rmvStrings(string inp) {
    bool inString=false;
    string res; char mode; int skip=0;
    foreach (ch; inp) {
      if (skip) { skip--; continue; }
      if (!inString) {
        res~=ch;
        if ((ch=='"')||(ch=='\'')) { inString=true; mode=ch; continue; }
      }
      if (ch=='\\') { skip=1; continue; }
      if (ch==mode) inString=false;
    }
    return res;
  }
  auto preparts=((cast(string)mod.modToFile().read())).rmvStrings().split("\n") /map/ (string s) {
    if (s.find("//")!=-1) return s[0..s.find("//")];
    return s;
  } /reduce(""[])/ ex!("a, b -> a~b");
  string s; bool inComment=false;
  foreach (pos, ch; preparts[0..$-1]) {
    if (!inComment) s~=ch;
    if (preparts[pos..pos+2]=="/*") { s~="*"; inComment=true; }
    if (preparts[pos..pos+2]=="*/") { s~="/"; inComment=false; }
  }
  auto parts=s.replace("\n", "").replace("\r", "").split("{");
  auto classes=parts /select/ (string s) {
    auto pos=s.find("class ");
    if (pos==-1) return false;
    return (letters~digits~"_").find(s[pos-1])==-1;
  } /map/ (string s) { return s[s.find("class ")+6..$].strip(); } /uniq;
  char[] res;
  bool skipName;
  foreach (entry; classes) if (entry.find(mod.rmPkg()) != -1) skipName = true;
  /+if (classes.length) res="shape=record, style="~quote(/*"rounded, filled"*/"filled")~", label="~'"'~
    (leftright?"{":"")~"{ "~(skipName?"":("{ "~mod.rmPkg()~" } "));
  // else res="shape=record, style="~quote(/*"rounded, filled"*/"filled")~", label=\""~mod.rmPkg();
  else +/res="label=\""~mod.rmPkg();
  /+string[] splitSkipBrackets(string inp, char sep) {
    size_t bracketlevel=0;
    string[] res; string buf;
    foreach (ch; inp) {
      buf~=ch;
      if (!bracketlevel) {
        if (ch=='(') { ++bracketlevel; continue; }
        if (ch==sep) { res~=buf[0..$-1]; buf=""; }
      } else { if (ch=='(') ++bracketlevel; else if (ch==')') --bracketlevel; }
    }
    if (buf.length) res~=buf; return res;
  }
  foreach (entry; classes) {
    string[] supers;
    auto ddp = entry.find(":");
    if (ddp!=-1) {
      supers=entry[ddp+1..$].replace("\"", "_").splitSkipBrackets(',');
      entry=entry[0..ddp];
    }
    auto ep = entry.find(" extends ");
    if (ep != -1) {
      supers = entry[ep+9 .. $].splitSkipBrackets(',');
      entry = entry[0 .. ep];
    }
    if (!skipName) res ~= " | "; else skipName = false;
    //res~="{ "~entry~(supers /map/ (string s) { return " | "~s; } /reduce(""[])/ ex!("e, f -> e~f"))~" } ";
    if (supers.length) res ~= entry~": "~supers.join(", ");
    else res ~= entry;
  }
  if (classes.length) return res~"}"~(leftright?"}":"")~'"';
  else +/return res ~ "\"";
}

class PExcluder : excluder {
  string[] matches; this() { }
  void add(string s) { matches~=s; }
  bool opCall(string a, string b) { foreach (m; matches) if (b.ifind(m)!=-1) return false; return true; }
}

void main(char[][] args) {
  auto name = args[0]; args = args[1 .. $];
  if (!args.length) { writefln(name, " <start module> [-x<filename_to_be_excluded>] [-lr (left/right)]"); return; }
  char[] startmod=args[0]; args = args[1 .. $];
  auto xc=new PExcluder;
  foreach (arg; args) {
    if (auto rest = arg.startsWith("-x")) xc.add(rest);
    if (arg == "-java") java_mode = true;
    if (arg=="-lr") leftright=true;
  }
  writefln("Digraph D { graph [concentrate=true, remincross=true, labeljust=l, ratio=compress, nodesep=0.2, "~(leftright?"rankdir=LR, ":"")~"fontname=Helvetica];");
  writefln(" node [ fontname=Helvetica ]; ");
  writefln(" edge [ fontname=Helvetica, fontsize=7 ]; ");
  auto graph=genImportsGraph(startmod, xc);
  bool utility[string]; foreach (node; graph.nodes) utility[node]=(graph.incoming(node)>3);
  log("Utility entries marked");
  auto pkg=new Package("", graph);
  log("Package generated");
  char[] rmMod(char[] inp) { if (inp.find(".")==-1) return "root"; return inp[0..inp.rfind(".")]; }
  
  auto cycles=tarjan(graph, startmod) /select/ ex!("e -> e.length > 1");
  auto cycle_list=cycles /reduce(Init!(string[]))/ ex!("x,y -> x~y");
  bool cycle(string from, string to) {
    foreach (cycle; cycles) {
      for (int i = 0; i < cycle.length - 1; ++i) if (cycle[i] == from && cycle[i+1] == to) return true;
      if (cycle[$-1] == from && cycle[0] == to) return true;
    }
    return false;
  }
  
  float[string] fsize;
  void printClustering(Package p) {
    if (p.name.length) {
      writefln(" subgraph cluster_", p.name.replace(".", "_"), " { ");
      writefln("label=", quote("package "~p.name), "; ");
    }
    foreach (sp; p.pkgs) printClustering(sp);
    foreach (mod; p.modules) {
      char[] color="white";
      if (cycle_list.has(mod)) color="lightsalmon";
      fsize[mod]=logf((cast(char[])mod.modToFile().read()) /map/ ex!("p -> p=='\\073'?1:0") /reduce(0)/ ex!("a,b->a+b"))*2+6;
      writefln(quote(mod), " [", getsettings(mod), ", fillcolor="~color~", fontsize="~toString(max(fsize[mod], 11))~"]; ");
    }
    if (p.name.length) writefln("}");
  }
  printClustering(pkg);
  log("Clusters printed");
  bool sameModule(string a, string b) {
    auto pos1=a.rfind("."); auto pos2=b.rfind(".");
    if ((pos1==-1)&&(pos2==-1)) return true;
    if ((pos1==-1)||(pos2==-1)) return false;
    return a[0..pos1]==b[0..pos2];
  }
  //foreach (id, entry; graph.dup.edges) foreach (target; entry)
  //  if (utility[target]&&!sameModule(id, target)) graph.rmEdge(id, target);
  string[] res;
  foreach (from, entry; graph.edges) {
    foreach (to; entry) {
      //auto line=format("  ", quote(from), " -> ", quote(to), "[");
      auto line=format("  ", quote(to), " -> ", quote(from), "[");
      float importance(string mod, int depth) {
        float res=fsize[mod];
        if (depth&&(mod in graph.edges)) foreach (target; graph.edges[mod]) res+=0.3*importance(target, depth-1);
        return res;
      }
      float i=pow(importance(to, 4), 0.5)*0.2;
      if (sameModule(from, to)) i*=2f;
      if (i<0.3) i=0.3;
      line~=(
        i>1f
        ?"style="~quote(format("setlinewidth(", i, ")"))
        :format("color=gray", "123456789"[cast(ulong)(10-i*10)], "0")
      )~format(", weight=", i*2, ", ");
      if (cycle(from, to)) line~="color=firebrick, weight=10, ";
      if (auto anno = stuple(from, to) in graph.annotate) {
        if (auto rest = (*anno).startsWith("#")) {
          *anno = rest;
          line~="arrowtail=dot, ";
        }
        line~="label=\""~*anno~"\", ";
      }
      res~=line~"]; ";
    }
  }
  foreach (line; res) writefln(line);
  writefln("}");
}