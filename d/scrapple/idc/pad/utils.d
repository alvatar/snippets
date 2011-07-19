module pad.utils;

import tools.base, tools.downloader;

bool readMode;

string readSrc(string addr, bool readFS = false) {
  string data;
  if (addr.startsWith("http://")) {
    data = addr.download();
    if (addr.startsWith("http://pastebin.ca")) {
      data = data.between("pastecontent\">", "<");
    } else if (addr.startsWith("http://pastebin.com")) {
      data = data.between("onTextareaKey(this,event)\">", "</textarea");
    } else if (addr.startsWith("http://paste.dprogramming.com")) {
      string data2;
      foreach (line; data.between(`align="left"><pre style="padding: 0px; margin: 0px;">`, `</pre>`).betweens(">", "<")) {
        data2 ~= line ~ "\n";
      }
      data = data2;
    } else if (addr.find("doku.php") != -1) {
      string data2;
      foreach (entry; data.betweens("<pre class=\"code\">", "<"))
        data2 ~= entry;
      data = data2;
    }
    if (data.find("%PAD START%") != -1) {
      string data2;
      foreach (entry; data.betweens("%PAD START%", "%PAD END%"))
        data2 ~= entry;
      data = data2;
    }
  } else if (hasSobby && addr.startsWith("gobby://")) {
    addr = addr[8 .. $];
    auto sobbydata = SobbyStorage.read().castLike("").split("\n");
    bool inTarget;
    foreach (line; sobbydata) {
      if (line.find("title=\"") != -1) inTarget = false;
      if (inTarget)
        data ~= line.between("content=\"", "")[0 .. $-1]
          .replace(`\n`, "\n").replace(`\"`, "\"").replace(`\\`, "\\");
      if (line.endsWith("title=\""~addr~"\"")) inTarget = true;
    }
  } else if (readFS) data = cast(string) .read(addr);
  else throw new Exception("Don't know how to read "~addr);
  return data.replaceEntities();
}

string[string] entmap;
static this() {
  foreach (line; import("htmlsymbols.txt").castLike("").split("\n")) {
    if (!line.startsWith("&")) continue;
    auto key = line.slice(" ");
    entmap[key] = line.chomp();
  }
}

string replaceEntities(string str) {
  string res;
  str.glomp_parse([
    "&": (string pre, ref string post) {
      res ~= pre;
      auto ent = "&"~post.slice(";")~";";
      if (auto p = ent in entmap) res ~= *p;
      else res ~= ent;
    }
  ], (string rest) { res ~= rest; });
  return res;
}

import tools.rd;
string getLiteral(ref string st, bool unescape = false) {
  auto backup = st;
  string buf;
  while (st.length && st[0] != '"') {
    if (st[0] == '\\') {
      if (st.length < 2) throw new Exception("Error: unterminated escape literal: "~backup.next_text());
      st.take();
      if (unescape) buf ~= '\\';
    }
    buf ~= st.take();
  }
  if (!st.length) throw new Exception("Error: unterminated character literal: "~backup.next_text());
  st.take();
  return buf;
}

bool gotLiteral(ref string st, out string l) {
  auto s2 = st.strip();
  if (!s2.length || s2.take() != '"') return false;
  l = s2.getLiteral();
  st = s2;
  return true;
}

string filterComments(string text) {
  string res;
  int nest_level = 0;
  void eatToBreak(ref string s) {
    auto pos = s.find("\n");
    if (pos == -1) return;
    s = s[pos .. $];
  }
  text.glomp_parse([
    "\""[]: (string pre, ref string post) { if (!nest_level) { res ~= pre ~ "\"" ~ getLiteral(post, true) ~ "\""; } },
    "//": (string pre, ref string post) { if (!nest_level) { res ~= pre; post.eatToBreak(); } },
    "/*": (string pre, ref string post) { if (!nest_level) res ~= pre; nest_level ++; },
    "*/": (string pre, ref string post) { if (!nest_level) throw new Exception("Too many */"); nest_level --; }
  ], (string rest) { res ~= rest; });
  return res;
}

const string SobbyStorage = "/root/sobby-autosave/backup";
bool hasSobby;
static this() { hasSobby = !!SobbyStorage.exists(); }
