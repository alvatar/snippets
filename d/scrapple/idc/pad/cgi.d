module pad.cgi;

import pad.mainloop, pad.utils;
import tools.base, tools.functional, tools.compat, tools.time, tools.mersenne;
static import tools.downloader, dice;

const self_address = "http://demented.no-ip.org/~feep/pad_test.cgi";

extern(C) {
  struct rlimit { size_t soft, hard; }
  const RLIMIT_CPU = 0, RLIMIT_AS = 9;
  int setrlimit(int resource, rlimit*);
}

static this() {
  rlimit cpu_limit, ram_limit;
  cpu_limit.soft = cpu_limit.hard = 10; // Should be plenty.
  setrlimit(RLIMIT_CPU, &cpu_limit);
  ram_limit.soft = ram_limit.hard = 32 * 1024 * 1024; // dito
  setrlimit(RLIMIT_AS, &ram_limit);
}

string my_getenv(string s) {
  return toString(getenv(s.toStringz()));
}

void dynReplace(string text, void delegate(string) dg, string[] whatwith...) {
  if ((whatwith.length % 2) != 0) throw new Exception("Invalid parameters to dynReplace! ");
  int count = whatwith.length / 2;
  auto markers = new int[count];
  for (int i = 0; i < count; ++i) {
    markers[i] = text.find(whatwith[i*2]);
  }
  int lastPosition;
  while (true) {
    int min = int.max, min_id = -1;
    foreach (i, marker; markers)
      if (marker != -1 && marker < min) {
        min = marker;
        min_id = i;
      }
    if (min_id == -1) break;
    auto what = whatwith[min_id * 2], repl = whatwith[min_id * 2 + 1];
    dg(text[lastPosition .. min]);
    dg(repl);
    lastPosition = min + what.length;
    markers[min_id] = text[lastPosition .. $].find(what);
    if (markers[min_id] != -1) markers[min_id] += lastPosition; // preserve failure
  }
  dg(text[lastPosition .. $]);
}

void sanitize1(string text, void delegate(string) dg) {
  text.dynReplace(dg, "&", "&amp;", "\"", "&quot;", "'", "&apos;", "<", "&lt;", ">", "&gt;");
}

string _sanitize1(string text) {
  string res;
  text.dynReplace((string s) { res ~= s; }, "&", "&amp;", "\"", "&quot;", "'", "&apos;", "<", "&lt;", ">", "&gt;");
  return res;
}

string sanitize(string text) {
  string temp1, res;
  text.sanitize1((string s) { temp1 ~= s; });
  // A certain list of "safe" HTML entities ..
  // no need to reallocate this every time
  static void delegate(string, ref string)[string] ent_map;
  void fun(string ent, string pre, ref string post) {
    res ~= pre;
    if (ent == "br") res ~= "<br/>";
    else res ~= "<"~ent~">";
  }
  foreach (ent; Tuple!("br", "b", "/b", "i", "/i")) {
    ent_map["&lt;"~ent~"&gt;"] = ent.dup /apply/ &fun;
  }
  ent_map["&lt;img"] = (string pre, ref string post) {
    res ~= pre;
    auto imgbody = post.slice("&gt;");
    if (imgbody.find(" on") != -1) throw new Exception("JavaScript is a security risk, and not supported. ");
    res ~= "<img"~imgbody~">";
  };
  ent_map["&lt;p"] = (string pre, ref string post) {
    res ~= pre;
    auto peabody = post.slice("&gt;");
    if (peabody.find(" on") != -1) throw new Exception("JavaScript is a security risk, and not supported. ");
    res ~= "<p"~peabody~">";
  };
  ent_map["http://"] = (string pre, ref string post) {
    res ~= pre;
    auto link = post.slice(" ");
    res ~= "<a href=\"http://"~link~"\">http://"~link~"</a>";
  };
  /*int pos;
  string res;
  while (pos < text.length) {
    pos += text[pos .. $].find("&lt;");
    auto rest = 
  }*/
  temp1.glomp_parse(ent_map, (string rest) { res ~= rest; });
  return res;
}

string entitiesDecode(string text) {
  string res;
  text.dynReplace((string s) { res ~= s; },"%2B","+","%3D","=","%2F","/","%3A",":","%3F","?","%26","&","+"," ");
  return res;
}

string entitiesEncode(string text) {
  string res;
  text.dynReplace((string s) { res ~= s; }," ","+","+","%2B","=","%3D","/","%2F",":","%3A","?","%3F","&","%26");
  return res;
}

string toLog;

import tools.log, gcstats, std.gc: getStats;
alias tools.log.log log;
void _main(string[] args) {
  log_threads = false;
  auto exec = args.take();
  string input;
  while (true) {
    auto line = readln();
    if (!line) break;
    input ~= line;
  }
  auto qs = my_getenv("QUERY_STRING");
  if (qs.length) input = qs;
  string[] lines;
  string[string] chunks;
  foreach (s; input.split("&")) {
    auto name = s.slice("=");
    // SEPARATOR
    if (name.find("_") != -1) { s = name; name = s.slice("_"); }
    chunks[name] = s;
  }
  bool back;
  if ("Back.x" in chunks) back = true;
  uint seed;
  if (auto s = "seed" in chunks) {
    seed = (*s).atoi();
  } else seed = dice.rand();
  dice.dice_rand_override = new Mersenne(seed) /apply/ (Mersenne rng) { return rng(); };
  if (auto p = "prev" in chunks) {
    if (p.length) {
      toLog = *p;
      lines = base64_decode((*p).entitiesDecode()).pad_decompress().castLike("").split("\x00");
    }
  }
  if (auto f = "backlog" in chunks) {
    if (!back && f.length) {
      if (auto rest = (*f).startsWith(">")) (*f) = rest.strip();
      toLog = *f;
      lines ~= (*f).entitiesDecode();
    }
  }
  if (back && lines.length) lines = lines[0 .. $-1];
  string default_input;
  if (auto lstr = "backToLine" in chunks) {
    auto line = (*lstr).atoi();
    default_input = lines[line];
    lines = lines[0 .. line];
  }
  string encoded;
  {
    scope st = new SaveStream;
    lines /map/ &st.addLine;
    encoded = base64_encode(st.getData());
  }
  string[] output;
  // output ~= Format(chunks);
  auto start = sec();
  auto stoppedAt = typeof(start).nan, stoppedFor = 0f;
  bool stopped;
  string error;
  try {
    runLoop("",
      (string s) { output ~= s; },
      {
        if (!lines.length) return "forcequit"[];
        auto line = lines.take();
        output ~= "> "~line;
        return line;
      },
      !ReadFS, DontSave, { // stop/restart stopwatch
        if (!stopped) { stopped = true; stoppedAt = sec(); }
        else { stopped = false; start += sec() - stoppedAt; stoppedFor += sec() - stoppedAt; }
      }
    );
  } catch (Exception ex) {
    output = null;
    ex.msg = ex.msg.replace("<", "&lt;").replace(">", "&gt;");
    error = Format("<h3>An error has occurred. </h3><h4>", ex, "</h4><a href=\"javascript:history.go(-1); \">Back</a>");
  }
  auto needed = sec() - start;
  string mkpretty(string[] foo) {
    string res;
    int i;
    foreach (str; foo) {
      if (auto rest = str.startsWith("> "))
        res ~=
          "<div style=\"text-indent: 2%; \"><b>&gt; </b><i>"~rest.sanitize()~"</i>"
          "<input type=\"image\" src=\"pilcrow-icon.png\" name=\"backToLine_"~Format(i++)~"\" value=\"&para;\" />"
          "</div>";
      else res ~= "<div style=\"margin-left: 4%; width: "~(("Embed" in chunks)?"95%":"60%")~"; text-align: justify; \">"~str.sanitize()~"</div>";
    }
    return res;
  }
  string preText, postText;
  if ("Embed" in chunks && chunks["Embed"] == "on") {
    preText = "<div style=\"height: 80%; width: 65%; border: 1px solid; overflow: auto; \" id=\"wrapperDiv\">";
    postText = "</div>";
  }
  "Content-type: text/html; charset=utf-8\r\n
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
  <meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\" />
  <title>IF Web Interface - generated in %TIME%s + %TIME2%s, %MEM%MB used</title>
</head>
<body>
  <script type=\"text/javascript\">
    window.onload = function() {
      document.getElementById(\"backlog\").focus();
      var wrapper = document.getElementById(\"wrapperDiv\");
      if (wrapper != null) wrapper.scrollTop = wrapper.scrollHeight;
    }
  </script>
  <h3>PAD Web Interface</h3>
  %ERROR%
  %PRE%
  <form method=\"post\" action=\"?#bottom\">
    <input type=\"image\" id=\"placeholder-submit\" type=\"image\" />
    %TEXT%
    <div style=\"text-indent: 2%; \"><b>&gt; </b><i>
    <input name=\"backlog\" id=\"backlog\" type=\"text\" value=\"%DEFAULT%\" />
    <input name=\"prev\" type=\"hidden\" value=\"%PREVENCODED%\" />
    <input name=\"seed\" type=\"hidden\" value=\"%RANDSEED%\" />
    <input type=\"image\" name=\"Submit\" src=\"return-icon.png\" />
    <input type=\"image\" name=\"Back\" src=\"back-icon.png\" />
    </i> </div>
    <!--%MAYBE_BR%-->
  %POST%
    <div style=\"font-size: 75%; \"><input type=\"checkbox\" name=\"Embed\"%OLDEMBED% />Embed log window</div>
  </form>%MAYBE_NOT_BR%
  <div style=\"font-size: 75%; \">
    <a href=\"http://tinyurl.com/create.php?url=%SELF%?seed=%RANDSEED%&amp;prev=%PREV_REENCODED%&amp;submit\">Permalink (TinyURL)</a><br/>
    <a href=\"?\">Reset</a><br/>
  </div>
  <div><a name=\"bottom\"></a></div>
</body>
</html>
  ".dynReplace((string s) { printf("%.*s", s); },
    "%PREVENCODED%", encoded,
    "%PREV_REENCODED%", encoded.entitiesEncode(),
    "%TEXT%", mkpretty(output),
    "%TIME%", Format(needed),
    "%TIME2%", Format(stoppedFor),
    "%INFO%", Format(chunks),
    "%RANDSEED%", Format(seed),
    "%DEFAULT%", default_input,
    "%ERROR%", error,
    "%SELF%", self_address,
    "%PRE%", preText,
    "%POST%", postText,
    "%CHUNK%", Format(chunks),
    "%OLDEMBED%", ("Embed" in chunks)?" checked":"",
    "%MAYBE_BR%", preText.length?"":"<br/>",
    "%MAYBE_NOT_BR%", preText.length?"<br/>":"",
    "%MEM%", { GCStats gs = void; getStats(gs); return Format(gs.poolsize / 1_000_000f); }()
  );
}

void main(string[] args) {
  tools.downloader.quiet = true;
  try _main(args);
  catch (Exception ex) {
    ("Content-type: text/html; charset=utf-8\r\n
    <html><head><title>Some sort of mess-up. </title></head><body>
    An internal error has occured.<br/>
    <b>"~Format(ex)._sanitize1()~"</b><br/>
    Further info: "~toLog~"<br/>
    Sorry.
    </body>
    </html>").log();
  }
}
