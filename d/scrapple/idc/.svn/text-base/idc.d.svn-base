module idc; // Internet delay chat! Yay!

import tools.fixed_socket, tools.base, std.string;
import tools.log, tools.threads, tools.threadpool;
import tools.time;

import irc;
import std.process;
alias std.process.system system;

import cah;

string characters(string s) {
  string res;
  foreach (ch; s)
    if (ch in Range['a' .. 'z'].endIncl /or/ Range['A' .. 'Z'].endIncl) res ~= ch;
  return res;
}

string clean(string s) {
  return s.tolower().replace("'", "").replace("_", "");
}

extern(C) string CTRL(string channel) {
  return IRCconfig.get!(string)(channel.clean(), "ControlChar", ".");
}

void setCtrl(string channel, string value) {
  IRCconfig.set(channel.clean(), "ControlChar", value);
}

string[] onStart(string server) {
  return IRCconfig.get!(string[])(server, "onStartup", cast(string[]) null);
}

void addOnStart(Query query) {
  if (query.name != root_user) return query.answer("Unauthorized access! ");
  auto serv = query.connection.host;
  IRCconfig.set(serv, "onStartup", onStart(serv) ~ query.param);
  query.answer("Command added. ");
}

string exec; string[] args;

extern(C) nickname root_user, reload_nick;

uint socket_handle;

int count = 1;

void rr(Query query) {
  auto channel = query.channel;
  void reload() {
    int roll;
    roll = rand() % 6;
    IRCconfig.set(channel.clean(), "RR", roll);
    query.answer("The gun has been reloaded. ");
  }
  if (query.param == "load") return reload();
  auto value = IRCconfig.get!(int)(channel.clean(), "RR", 0);
  if (value) {
    query.answer("Click. ");
    IRCconfig.set(channel.clean(), "RR", value-1);
  } else {
    query.answer("Boom! You're dead. ");
    reload();
  }
}

string[] grepv(string[] s, string t) {
  string[] res;
  foreach (str; s)
    if (str.find(t) == -1) res ~= str;
  return res;
}

import std.file;
void reload(Query query) {
  if (system("sh -c \"dsss build 2> /tmp/idc_errors || exit 1\"") != 0) {
    query.answer("Error occured while rebuilding: "~"/tmp/idc_errors".read().castLike("").split("\n").grepv("EXPER")[0]);
    return;
  } else logln("Build complete.");
  logln("Asked to reload by ", query.name, " in ", query.channel);
  if (query.name == root_user || query.name == cast(nickname) "Makoryu") {
    execv(exec, exec~args~["--resume"[], toString(socket_handle),
      query.channel.length?query.channel:cast(string) query.name, cast(string) query.name, toString(count)]);
  } else query.answer("Unauthorized!");
}

string urlencode(string q) {
  string res;
  foreach (ch; q) {
    if (ch ==  ' ') res ~= "+";
    else if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')) res ~= ch;
    else if ("._".find(ch) != -1) res ~= ch;
    else {
      const hex = "0123456789ABCDEF";
      res ~= "%" ~ hex[ch/16] ~ hex[ch%16];
    }
  }
  return res;
}

string googleQuery(string q) {
  auto hash = "http://www.google.com/uds/?file=search&v=1.0".download().between("JSHash = '", "'");
  auto url = "http://www.google.com/uds/GwebSearch?callback=google.search.WebSearch.RawCompletion&hl=en&v=1.0&q="~q.urlencode();
  auto res = url.download().between("unescapedUrl\":\"", "\"").replace("\\u003d", "\u003d");
  if (!res.length) throw new Exception("Cannot identify Google result");
  if (!res.startsWith("http://")) res = "http://"~res;
  if (res.find("dwarf.lendemaindeveille.com") != -1) { // special handling
    res = res.replace("&redirect=no", "");
  }
  return res;
}

string mktiny(string url) {
  auto tinydata = ("POST=url="~url.urlencode()~"&submit=Make+TinyURL%21&alias= http://tinyurl.com/create.php").download();
  return "http://tiny"~tinydata.between("<b>http://tiny", "</b>");
}

class AbortException : Exception { this() { super("Bad mojo. "); } }

string download_first(string url, string* redir = null) {
  string res;
  try url.streamload((string chunk) {
    res ~= chunk;
    if (res.length > 50_000)
      throw new AbortException;
  }, redir);
  catch (AbortException) return res;
  return res; // Geez.
}

ubyte hexdecode(char c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  throw new Exception("'"~c~"' is not a hex! >_<");
}

ubyte hexdecode(string s) {
  assert(s.length == 2);
  return (hexdecode(s[0]) << 4) + hexdecode(s[1]);
}

char hexencode(ubyte ub) {
  if (ub >= 16) throw new Exception(Format(ub, " too large to encode in single char! "));
  if (ub >= 10) return 'a' + ub - 10;
  return '0' + ub;
}

string unescape(string s) {
  string res;
  while (s.length) {
    auto ch = s.take();
    if (ch != '%') res ~= ch;
    else res ~= cast(char) s.take(2).hexdecode();
  }
  return res;
}

// Happily ripping off http://userscripts.org/scripts/review/44261 ..

string decrypt(string un, int k1, int k2) {
  string bin;
  foreach (ch; un) {
    auto ub = hexdecode(ch);
    foreach (b; [8, 4, 2, 1]) {
      if (ub & b) bin ~= "1"; else bin ~= "0";
      ub &= ~b;
    }
  }
  auto keys = new ubyte[384];
  foreach (ref val; keys) {
    k1 = (k1 * 11 + 77213) % 81371;
    k2 = (k2 * 17 + 92717) % 192811;
    val = (k1 + k2) % 128;
  }
  for (int i = 256; i >= 0; --i) {
    swap(bin[keys[i]], bin[i%128]);
  }
  for (int i = 0; i < 128; ++i) {
    bin[i] ^= keys[i+256] & 1;
  }
  string res;
  for (int i = 0; i < bin.length; i += 4) {
    auto data = bin[i .. i+4];
    int val = ((data[0] == '1') << 3) + ((data[1] == '1') << 2) + ((data[2] == '1') << 1) + (data[3] == '1');
    res ~= hexencode(val);
  }
  return res;
}

bool works(void delegate()[] dgs...) {
  try foreach (dg; dgs) dg();
  catch (Exception ex) return false;
  return true;
}

bool pwn_site(string url, ref string[string] urls, ref string src) {
  if (url.find("youtube.com") != -1) {
    if (!src) src = url.download();
    // Youtube is in the sucking-of-ass business. This won't work.
    /*if (src.find(`id="verify-age"`) != -1) {
      url = "http://www.youtube.com/verify_age?action_confirm=Confirm Birth Date&next_url="~url.between("youtube.com", "").urlencode();
      src = url.download();
    }*/
    auto
      hd = src.between("isHDAvailable = ", ";") == "true",
      t = src.between(`"t": "`, `"`), id = src.between(`"video_id": "`, `"`);
    auto hqurl = url.followLink("/get_video?fmt=22&video_id="~id~"&t="~t);
    auto mp4url = url.followLink("/get_video?fmt=18&video_id="~id~"&t="~t);
    if (hd) urls["hd"] = hqurl.mktiny();
    urls["mp4"] = mp4url.mktiny();
    return true;
  }
  if (url.find("vimeo.com") != -1) {
    auto id = url.between("vimeo.com/", "");
    if (!id.atoi()) return false;
    auto tmp = ("http://www.vimeo.com/moogaloop/load/clip:"~id).download();
    auto
      sign = tmp.between("request_signature>", "<"),
      expiry = tmp.between("signature_expires>", "<");
    auto
      murl = "http://www.vimeo.com/moogaloop/play/clip:"~id~"/"~sign~"/"~expiry~"/?q=!Q!&type=local&embed_location=",
      hd = murl.replace("!Q!", "hd"),
      sd = murl.replace("!Q!", "sd");
    if (works(hd.download_first()))
      urls["HD"] = hd.mktiny();
    // if (works(sd.download_first())) // assumed to always work
      urls["SD"] = sd.mktiny();
    return true;
  }
  bool res;
  auto data = url.download_first();
  auto megacheck = data;
  if (auto codes = megacheck.between("write(unescape('", "'")) {
    // Tricky bastards.
    megacheck = codes.unescape();
  }
  if (auto hit = megacheck.between("\"", "megavideo.com", "\"")) {
    string redir;
    hit.download_first(&redir);
    if (redir) hit = redir ~ "&"; // hackaround: make .between(foo, "&") work
    auto sesame = ("http://www.megavideo.com/xml/videolink.php?v="~hit.between("v=", "&")~"&id=").download();
    auto diamond = sesame.between("un=\"", "\""), k1 = sesame.between("k1=\"", "\"").atoi(), k2 = sesame.between("k2=\"", "\"").atoi();
    urls["mp4"] = ("http://www"~sesame.between(" s=\"", "\"")~".megavideo.com/files/"~diamond.decrypt(k1, k2)~"/").mktiny();
    res = true;
  }
  if (auto hit = data.between("value='", "www.yuvitube.com", "'").between("vid=", "")) {
    urls["yuvitube/flv"] = Format("http://www.yuvitube.com/uploads/flvideo/", hit, ".flv");
    res = true;
  }
  return res;
}

import tools.downloader;
void google(Query query) {
  auto q = query.param.googleQuery();
  query.answer(q ~ " <u>" ~ q.getTitle() ~ "</u>");
}

extern(C) {
  int pipe(int* fd);
  int close(int);
  FILE *fdopen(int fd, char* mode);
}

string readStream(InputStream IS) {
  string res;
  ubyte[512] buffer;
  int i;
  do {
    i = IS.read(buffer);
    if (i < 0) throw new Exception(Format("Read error: ", i));
    res ~= cast(string) buffer[0 .. i];
  } while (i);
  return res;
}

string readback(string cmd) {
  int[2] fd; // read end, write end
  if (-1 == pipe(fd.ptr)) throw new Exception(Format("Can't open pipe! "));
  scope(exit) close(fd[0]);
  auto cmdstr = Format(cmd, " >&", fd[1], " &");
  system(cmdstr);
  close(fd[1]);
  scope fs = new CFile(fdopen(fd[0], "r"), FileMode.In);
  return readStream(fs);
}

void gun(Query query) {
  auto gs = readback("echo $(fwoosh/fwoosh)");
  query.answer(gs);
}

void fwoosh_upd(Query query) {
  query.answer(readback("cd fwoosh; ./scriptfile.sbcl |wc -l"));
}

import std.utf;
string htmlFormat(string s) {
  wstring res;
  s.glomp_parse([
    "<sup>"[]: (string pre, ref string post) { res ~= pre.toUTF16(); },
    "</sup>": (string pre, ref string post) {
      bool onlyAsciiHighables = true, onlyUnicodeHighables = true;
      foreach (ch; pre) {
        if (ch < '1' || ch > '3') onlyAsciiHighables = false;
        if (ch /notin/ Range['0' .. '9'].endIncl && ch != '-' && ch != '+' && ch != ' ') onlyUnicodeHighables = false;
      }
      if (onlyAsciiHighables) {
        foreach (ch; pre) {
          switch (ch) {
            case '1': res ~= '¹'; break;
            case '2': res ~= '²'; break;
            case '3': res ~= '³'; break;
            case ' ': res ~= ' '; break;
            default: assert(false);
          }
        }
      } else if (onlyUnicodeHighables) {
        foreach (ch; pre) {
          if (ch in Range['4' .. '9'].endIncl) { res ~= '⁴' + (ch - '4'); continue; }
          switch (ch) {
            case '0': res ~= '⁰'; break;
            case '1': res ~= '¹'; break;
            case '2': res ~= '²'; break;
            case '3': res ~= '³'; break;
            case ' ': res ~= ' '; break;
            case '+': res ~= '⁺'; break;
            case '-': res ~= '⁻'; break;
            default: assert(false);
          }
        }
      } else res ~= ("^"~pre).toUTF16();
    },
    "<font": (string pre, ref string post) {
      res ~= pre.toUTF16();
      post = post[post.find(">")+1 .. $];
    },
    "</font>": (string pre, ref string post) { res ~= pre.toUTF16(); }
  ], (string rest) { res ~= rest.toUTF16(); });
  return res.toUTF8().replace("&#215;", "×");
}

void gcalc(Query query) {
  auto url = "http://www.google.com/search?hl=en&q="~query.param.urlencode();
  auto page = url.download();
  auto res = page.between("h2 class=r", "</b").between("<b>", "");
  if (!res) return query.answer("No GCalc result.");
  return query.answer("<b>"~res.htmlFormat()~"</b>");
}

void wp(Query query) {
  query.param = "site:wikipedia.org "~query.param;
  return google(query);
}

void trope(Query query) {
  auto words = query.param.split(" ");
  int wordcount; string[] altlist;
  auto backup = words;
  foreach (word; words) {
    word = word.strip();
    if (!word.length) continue;
    if (wordcount >= 1) { if (word.length && word[0] in Range['a' .. 'z'].endIncl) { words = altlist; break; } }
    else if (word.length && word[0] in Range['a' .. 'z'].endIncl) break;
    wordcount ++;
    altlist ~= word;
  }
  auto j = std.string.join(words, " ").strip();
  string q = query.param;
  if (j.length) q = j;
  string res;
  const string exclude = "-intitle:\"Discussion\" -inurl:editors.php -inurl:inboundcount.php -inurl:review_comments.php";
  try
    res = ("site:tvtropes.org "~exclude~" inurl:main intitle:\""~q~"\"").googleQuery();
  catch (Exception ex) try
    res = ("site:tvtropes.org "~exclude~" intitle:\""~q~"\"").googleQuery();
  catch (Exception ex)
    res = ("site:tvtropes.org "~exclude~" "~q).googleQuery();
  auto alt = res.between("", "?");
  query.answer(alt?alt:res);
}

void df(Query query) {
  string q = query.param, res;
  try res = ("site:dwarffortresswiki.net intitle:\""~q~"\" -inurl:Talk -inurl:User_talk").googleQuery();
  catch (Exception ex) try res = ("site:dwarffortresswiki.net intitle:\""~q~"\" -inurl:Talk -inurl:User_talk").googleQuery();
  catch (Exception ex) try res = ("site:dwarffortresswiki.net \""~q~"\"").googleQuery();
  catch (Exception ex) res = ("site:dwarffortresswiki.net "~q).googleQuery();
  query.answer(res);
}

import std.regexp: regex_sub = sub;
void tropesample(Query query) {
  auto site = ("site:tvtropes.org -discussion "~query.param).googleQuery().download();
  auto samples = site.between("<h2>", "").betweens("<li>", "</li>")
    /map/ (string s) {
      return s.regex_sub("<[^>]*>", "", "g");
    } /select/ (string s) { return s.length < 128; };
  if (!samples.length) return query.answer("No short samples found. ");
  query.answer("Sample for ", site.between("<title>", " - "), ":", samples[rand() % $]);
}

void delegate(Query)[nickname] nexts; // trigger the next action for a nick
Object nexts_sync;
static this() { New(nexts_sync); }

string stripTags(string s) {
  string res = "";
  bool inTag = false;
  foreach (ch; s) {
    if (inTag) {
      if (ch == '>') inTag = false;
    } else {
      if (ch == '<') inTag = true;
      else if (ch == '>') {
        res = "";
      } else res ~= ch;
    }
  }
  return res;
}

string cleanup(string s) {
  string res = "";
  bool hadSpace = false;
  foreach (ch; s) {
    if (ch == '\n' || ch == '\t') continue;
    if (hadSpace) {
      if (ch != ' ') {
        hadSpace = false;
        res ~= ch;
      }
    } else {
      if (ch == ' ') hadSpace = true;
      res ~= ch;
    }
  }
  return res;
}

class Formatter {
  string buffer;
  int maxcols;
  void delegate(string) dg;
  mixin This!("dg, maxcols=80");
  void flush() { dg(buffer); buffer = ""; }
  string last_section;
  import std.utf;
  void append(string text, string sep="") {
    auto test = text ~ buffer ~ sep;
    if (test.toUTF32().length > maxcols) {
      flush;
      if (last_section.length) buffer ~= "(cont.) ";
    } else if (last_section.length) buffer ~= sep;
    buffer ~= text;
  }
  void print(string section, string info, string sep="") {
    if (buffer.length) {
      if (section == last_section) {
        append(info, sep);
      } else {
        if (last_section.length) {
          last_section = "";
          append(" | ");
        }
        append(section);
        append(info);
        last_section = section;
      }
    } else {
      if (section.length) append(section);
      last_section = section;
      append(info);
    }
  }
}

void anidb(Query query, int offset, string pagebuf, bool rewrote = false) {
  string address = "http://anidb.net/perl-bin/animedb.pl?show=animelist&adb.search="~query.param.urlencode();
  string redirect, page;
  if (!query.param.strip().length && !offset) {
    query.answer("anidb <search term>. Performs an AniDB lookup.");
    return;
  }
  if (atoi(query.param) && !offset) {
    redirect = "http://anidb.net/perl-bin/animedb.pl?show=anime&aid="~toString(atoi(query.param)); /* sanitize */
  } else if (pagebuf.length) page = pagebuf; else page = address.download(&redirect);
  if (redirect.length) {
    if (!redirect.startsWith("http://")) redirect = "http://"~redirect;
    page = redirect.download();
    auto fm = new Formatter((string s) { query.answer(s); });
    scope(exit) fm.flush();
    auto offc = page.betweens("Official Title</th>", "</td>")
      /map/ (string s) { return s.between("<label>", "</label>"); };
    string type = page.between("Type</th", "</td").between("value\">", "");
    string year = page.between("Year</th", "</td").between("value\">", "");
    string cats = page.between("Categories</a></th", "- <a").stripTags().cleanup();
    string rats = page.between("Rating</th>", "</td").stripTags().cleanup();
    fm.print("", page.between("Main Title</th", "<a").between("value\">", "\n"));
    fm.print("", ": ");
    if (offc.length)
      foreach (o; offc) fm.print("official title ", o, "/");
    if (type.length) fm.print("Type: ", type);
    if (year.length) fm.print("Year: ", year);
    if (cats.length) fm.print("Categories: ", cats);
    if (rats.length) fm.print("Rating: ", rats);
    fm.print("", redirect);
  } else {
    auto entries = page.betweens("class=\"thumb anime\"", "</tr");
    int count = 3;
    if (offset) {
      entries = entries[offset .. $];
      address = ""; // shouldn't be needed
      if (query.param.length) count = query.param.atoi();
    }
    if (!entries.length) {
      query.answer("no results. Sorry.");
      return;
    }
    bool[int] already_seen; int doubles;
    typeof(entries) n_entries;
    foreach (i, entry; entries) {
      auto name = entry.between("<a", "");
      if (name.find("<img") == -1) name = name.between(">", "</a>");
      else name = entry.between("<td", "").between("<a", "").between(">", "</a");
      
      /+if (!rewrote && !offset && name.tolower().startsWith(query.param.tolower())) {
        /* Perfect match; rewrite */
        auto np = entry.between("href=\"", "\"").between("aid=", "");
        if (np.atoi()) {
          auto q = query;
          q.param = np;
          return anidb(q, 0, null, true);
        }
      }+/
      auto id = entry.between("href=\"", "\"").between("aid=", "").atoi();
      if (id in already_seen) { doubles++; continue; }
      already_seen[id] = true;
      n_entries ~= entry;
    }
    entries = n_entries;
    foreach (i, entry; entries) {
      if (i >= count) break;
      auto name = entry.between("<a", "");
      if (name.find("<img") == -1) name = name.between(">", "</a>");
      else name = entry.between("<td", "").between("<a", "").between(">", "</a");
      auto altname = entry.between("class=\"main\">", "<");
      auto id = entry.between("href=\"", "\"").between("aid=", "");
      query.answer(Format(i + 1 + offset,
        "(", id, ") - ", name,
        altname.length?" ":"", altname,
        ", rated ", entry.between("rating perm\">", "\n"),
        ", aired from ", entry.between("date airdate\">", "<"),
        " to ", entry.between("date enddate\">", "<")
      ));
    }
    if (entries.length > count) {
      query.answer(Format(entries.length - count, " omitted."));
      synchronized (nexts_sync) nexts[query.name] = stuple(page, offset+count) /apply/ (string page, int offset, Query q) {
        return anidb(q, offset, page, false);
      };
    }
  }
}

import tools.functional;

void decide(Query query) {
  auto choices = query.param.split("|") /map/ &strip;
  if (choices.length <= 1) query.answer("decide a|b|c -> a, or b, or c");
  else {
    auto choice = choices[rand() % $];
    if (!choice.length) choice = ["What?"[], "Eh?", "Whuh?", " ... ?"][rand() % $];
    query.answer(choice);
  }
}

import tools.time: sec; import tools.base: yield;
// waits for condition to become true
bool timeout(ref double left, lazy bool condition) {
  auto start = sec();
  double elapsed() { return sec() - start; }
  double end() { return start + left; }
  scope(exit) left -= elapsed();
  logln("start: ", start, ", left: ", left, ", end ", end(), ", elapsed ", elapsed());
  while (sec() < end()) {
    if (condition()) return false;
    yield();
  }
  return true;
}

MessageMultiChannel!(Query, false, false)[string] votes;

void vote(Query query) {
  typeof(votes[""]) my_chan;
  struct Alternative {
    int count;
    string name;
  }
  Alternative[] res; double secs;
  synchronized(SyncObj!(votes)) {
    if (auto ch = query.channel in votes)
      return ch.put(query);
    secs = query.param.slice(" ").atof();
    auto choices = query.param.split("|") /map/ &strip;
    if (choices.length <= 1) return query.answer("vote <timeout> a|b|c");
    if (secs !>= 1) return query.answer("Timeout cannot be null or smaller than one.");
    // if (secs > 600) return query.answer("Don't you think that's a tad excessive?");
    res.length = choices.length;
    foreach (i, choice; choices) res[i].name = choice;
    New(my_chan);
    votes[query.channel] = my_chan;
  }
  string floatclean(float f) {
    auto i = cast(int) (f * 100);
    auto rest = i % 100;
    return Format(i/100, ".", (rest<10)?"0":"", i%100);
  }
  string[] temp;
  bool[string] voted;
  foreach (i, vote; res) temp ~= Format(i+1, ": ", vote.name);
  query.say("Channel vote! The choices are: ", temp, ". You have ", floatclean(secs), "s to decide. Go. ");
  Query votequery;
  auto start = sec(), total_secs = secs; // timeout modifies secs
  double left() { return start - sec() + total_secs; }
  void unref() {
    synchronized(SyncObj!(votes)) votes.remove(query.channel);
  }
  while (!timeout(secs, my_chan.try_get(votequery))) {
    if (votequery.name == root_user && votequery.param == "abort") {
      unref;
      return votequery.answer("admin override: vote aborted");
    }
    auto index = votequery.param.atoi();
    if (!index) {
      votequery.answer("Your choices are: ", temp, ". Please vote by using \"vote <number>\".");
      continue;
    }
    if (index > res.length) {
      votequery.answer("That is not a valid choice.");
      continue;
    }
    if (votequery.name in voted) {
      votequery.answer("You have already voted!");
      continue;
    }
    voted[votequery.name] = true;
    res[index-1].count++;
    string[] nutemp;
    foreach (vote; res) nutemp ~= Format(vote.name, ": ", vote.count);
    votequery.say("Current score: ", nutemp, ". ", floatclean(left), " seconds left. ");
  }
  unref;
  res = res /qsort/ ex!("fn -> a, b -> fn(a.count, b.count)")(&cmp!(int));
  if (!res[$-1].count) {
    return query.say("Time's up and nobody voted! Sorry. Better luck next time!");
  }
  if (res.length > 1 && res[$-1].count == res[$-2].count) {
    auto ties = res /select/ ex!("cmp -> x -> x.count == cmp")(res[$-1].count) /map/ ex!("x -> x.name");
    return query.say("Time's up aaand .. it's a tie between ", ties, " with ", res[$-1].count, " votes! Thanks for voting. ");
  } else with (res[$-1])
    return query.say("Time's up! The winner is ", name, " with ", count, (count>1)?" votes":" vote", "! Thanks for voting. ");
}

MessageMultiChannel!(Query, false, false)[string] padgame;

// Thanks stacy!
string filterComments(string text) {
  string res;
  int nest_level = 0;
  text.glomp_parse([
    "/*": (string pre, ref string post) { if (!nest_level) res ~= pre; nest_level ++; },
    "*/": (string pre, ref string post) { if (!nest_level) throw new Exception("Too many */"); nest_level --; }
  ], (string rest) { res ~= rest; });
  string res2;
  foreach (i, line; res.split("\n")) {
    if (i) res2 ~= "\n";
    res2 ~= line.cutOff("//");
  }
  return res2;
}

Lock padlock;
static this() { New(padlock); }

string lastPadURL(string id) {
  return IRCconfig.get!(string)(id, "lastpad", "");
}

void setLastPadURL(string id, string v) {
  IRCconfig.set(id, "lastpad", v);
}

void repad(Query query) {
  auto id = query.connection.host ~ query.channel;
  auto lpu = lastPadURL(id);
  if (!lpu.length) return query.answer("No previous command recorded");
  Query foo = query;
  foo.param = lpu ~ " " ~ query.param;
  padfn(foo);
}

import pad.mainloop;
void padfn(Query query) {
  typeof(padgame[""]) my_chan;
  // auto mykey = (query.channel~"!"~cast(string)query.name).tolower();
  auto mykey = query.channel.tolower();
  query.param = query.param.replace("\x02", "").replace("\x1f", "").replace("\x16", "");
  string url;
  bool inpmode;
  query.param = query.param.strip();
  if (auto rest = query.param.startsWith("> ")) { query.param = rest.strip(); inpmode = true; }
  bool abort;
  padlock.Synchronized = {
    if (auto ch = mykey in padgame) {
      if (query.param.startsWith("http://" /or/ "gobby://")) {
        auto q2 = query; q2.param = "forcequit";
        ch.put(q2);
        // wait for other instance to forcequit
        while (mykey in padgame) padlock.Unsynchronized = { slowyield(); };
      } else {
        ch.put(query);
        abort = true; return;
      }
    }
    if (inpmode) { abort = true; return; }; // Do not give info if no game is running
    url = query.param.slice(" ");
    if (!url.length) { abort = true; return query.answer("Paste Adventure: ", CTRL(query.channel), "pad <pastebin url> <initial commands>"); }
    New(my_chan);
    padgame[mykey] = my_chan;
  };
  if (abort) return;
  setLastPadURL(query.connection.host ~ query.channel, url);
  scope(exit) padlock.Synchronized = { padgame.remove(mykey); };
  runLoop(url, query.param, (string s) {
    auto chunks = s.split("<br>");
    foreach (ref chunk; chunks) {
      if (chunk.length > 256) chunk = chunk[0 .. 256] ~ "[...] EXCESS HERE";
    }
    if (!chunks.length) return query.answer("-- ");
    query.answer(chunks[0]);
    chunks[1 .. $] /map/ &query.say!(string);
  }, { query = my_chan.get(); return query.param; });
}

MessageMultiChannel!(Query, false, false)[string] rps_game;

void rps(Query query) {
  typeof(rps_game[""]) my_chan;
  string[] players;
  auto id = query.channel ~ " _ " ~ cast(string) query.name;
  synchronized(SyncObj!(rps_game)) {
    if (auto ch = id in rps_game) return ch.put(query);
    players = [cast(string) query.name, query.param.strip()].dup;
    if (players[0] == players[1]) return query.answer("Your other self says hi! Also don't worry about the dead body. ");
    if (players[1].find(" ") != -1 || !query.connection.exists(cast(nickname) players[1]))
      return query.answer("rps <other player>");
    New(my_chan);
    auto id1 = query.channel ~ " _ " ~ players[0], id2 = query.channel ~ " _ " ~ players[1];
    if (id1 in rps_game) return query.answer(players[0], " is already playing!");
    if (id2 in rps_game) return query.answer(players[1], " is already playing!");
    rps_game[id1] = rps_game[id2] = my_chan;
  }
  int[] genMap() {
    auto map = ([0, 1, 2]).dup;
    swap(map[0], map[rand() % 3]);
    swap(map[1], map[1+(rand() % 2)]);
    return map;
  }
  auto map1 = genMap(), map2 = genMap();
  auto q1 = query, q2 = query; q1.name = cast(nickname) players[0]; q2.name = cast(nickname) players[1];
  query.say("A game of RPS between ", q1.name, " and ", q2.name, " is running!");
  q1.notice(Format("Your parameters: ", map1[0], ": Rock |", map1[1], ": Paper |", map1[2], ": Scissors"));
  q2.notice(Format("Your parameters: ", map2[0], ": Rock |", map2[1], ": Paper |", map2[2], ": Scissors"));
  Query getOnce() {
    while (true) {
      auto merp = my_chan.get();
      if (merp.param.atoi() /notin/ Range[0..3]) continue;
      synchronized(SyncObj!(rps_game)) {
        auto id = merp.channel ~ " _ " ~ cast(string) merp.name;
        if (id in rps_game) { rps_game.remove(id); return merp; }
      }
    }
  }
  auto move1 = getOnce(), move2 = getOnce();
  logln("Got both merp!");
  if (cast(string) move1.name == players[1]) swap(move1, move2);
  int resolve(int player, int move) {
    auto map = player?map2:map1;
    foreach (i, entry; map) if (entry == move) return i;
    throw new Exception("Brain breakage: Invalid move");
  }
  auto id1 = resolve(0, move1.param.atoi()), id2 = resolve(1, move2.param.atoi());
  auto choices = ["Rock"[], "Paper", "Scissors"], challenge = Format(choices[id1], " vs. ", choices[id2]);
  if (id1 == id2) return query.say(challenge, ": It's a tie!");
  if (((id1 + 1) % 3) == id2) return query.say(challenge, ": ", players[1], " wins!");
  return query.say(challenge, ": ", players[0], " wins!");
}

void join(Query query) {
  if (query.name == root_user) {
    with (query.connection) join(query.param, defaultChanHandler);
  } else query.answer("unauthorized!");
}

void part(Query query) {
  if (query.name == root_user) {
    query.connection.part(query.param);
  } else query.answer("unauthorized!");
}

void more(Query query) {
  synchronized (nexts_sync) {
    if (!(query.name in nexts)) {
      query.answer("I don't know more.");
    } else {
      auto task = nexts[query.name];
      nexts.remove(query.name);
      check_ex(task /fix/ query, query);
    }
  }
}

void say(Query query) {
  if (auto rest = query.param.startsWith("\x01")) {
    if (!rest.startsWith("ACTION")) {
      query.answer("arbitrary ctcp denied!");
      return;
    }
  }
  bool authd(nickname n) {
    return query.connection.registered(n) && ((n == root_user) || (n == cast(nickname) "Lazy_Zefiris") || (n == cast(nickname) "Meow_Zefiris"));
  }
  query.param = query.param.strip();
  if (query.param.startsWith("!" /or/ "]" /or/ "ljrbot")) {
    if (!authd(query.name)) {
      query.answer("You can't make me do that!");
      return;
    }
  }
  auto cutpos = query.param.ifind("candlejack");
  if (cutpos != -1) {
    cutpos += 10;
    if (cutpos < query.param.length) {
      cutpos += rand() % (query.param.length - cutpos);
    }
    query.param = query.param[0 .. cutpos] ~ "-";
  }
  query.say(query.param);
}

class GeoIP {
  import std.file;
  static {
    struct entry {
      uint start;
      string country;
    }
    entry[] list;
    string lookup(uint what) {
      auto range = list;
      while (range.length > 1) {
        auto center = range[$/2];
        if (center.start == what) return center.country;
        if (center.start < what) range = range[$/2 .. $];
        else range = range[0 .. $/2];
      }
      return range[0].country;
    }
  }
  static this() {
    "/home/mathis/GeoIPCountryWhois.csv".read().castLike("").split("\n") /map/ (string line) {
      line = line.strip();
      if (!line.length) return;
      auto parts = line[1 .. $-1].split("\",\""); // remove ""s
      auto ip = parts[0];
      entry e;
      // why parse IPs? Let the system do it for us.
      e.start = (new ActuallyWorkingInternetAddress(ip, 0)).addr();
      e.country = parts[$-1];
      list ~= e; // yes they're ordered
    };
  }
}

mixin(ExSource);

void cstats(Query query) {
  auto stats = &(new Stuple!(int[string]))._0;
  auto namestats = &(new Stuple!(string[string]))._0;
  tp.addTask(&query.connection.whoare /fix/ stuple(query.channel, stuple(stats, namestats) /apply/
  (ref int[string] stats, ref string[string] namestats, string user, string host) {
    string country;
    auto parts = host.split(".");
    while (parts.length > 1) {
      try {
        auto addr = new ActuallyWorkingInternetAddress(std.string.join(parts, "."), 0);
        auto ip = addr.addr();
        country = GeoIP.lookup(ip);
        break;
      } catch (Exception ex) { parts = parts[1 .. $]; }
    }
    if (!country) return;
    synchronized {
      if (country /notin/ stats) stats[country] = 1;
      else stats[country] = stats[country] + 1;
      auto tmp = user;
      if (tmp.length > 1) tmp = tmp[0] ~ "​" ~ tmp[1 .. $];
      if (country /notin/ namestats) namestats[country] = tmp;
      else namestats[country] ~= ", " ~ tmp;
    }
  }, stuple(stats, namestats, query) /apply/ (ref int[string] stats, ref string[string] namestats, Query query) {
    auto rstats = invert(stats), top = rstats.keys.sort;
    string[][int] res;
    namestats[rstats[top[$-1]][0]] = "rest";
    foreach (key, list; rstats) {
      foreach (country; list) {
        res[key] ~= Format(country, " (", namestats[country], ")");
      }
    }
    if (top.length > 10) top.length = 10;
    query.answer("Country stats: ", top /map/ ex!("r -> b -> stuple(b, r[b])")(res));
  }));
}

void cfind(Query query) {
  if (!query.param.strip().length)
    return query.answer("Please supply a filter criterium! ");
  auto stats = &(new Stuple!(string[][string]))._0;
  tp.addTask(&query.connection.whoare /fix/ stuple(query.channel, stats /apply/ (ref string[][string] stats, string user, string host) {
    auto parts = host.split(".");
    auto domain = std.string.join(parts[1 .. $], ".");
    if (!domain) return;
    synchronized {
      stats[domain] ~= user;
    }
  }, stuple(stats, query) /apply/ (ref string[][string] stats, Query query) {
    bool found;
    string[][string] mapping;
    foreach (key, value; stats) {
      if (key.find(query.param.strip()) != -1) {
        mapping[key] = value;
        found = true;
      }
    }
    query.answer("Result: ", mapping);
    if (found) return;
    query.answer("No matches. ");
  }));
}

import tools.ini;
iniFile wbfile, IRCconfig, seenfile, notefile;
static this() {
  New(wbfile, "wb.txt");
  New(IRCconfig, "idc.ini");
  New(seenfile, "seen.txt");
  New(notefile, "notice.txt");
}

import std.stream;
File bridge_write;
string bridge_channel;

void bridge(Query query) {
  if (query.name != root_user) return query.answer("You are not authorized to set up a bridge!");
  auto params = query.param.split(" ");
  if (!query.param.length || params.length != 3) return query.answer("Usage: bridge <order [first/second]> <read pipe> <write pipe>");
  auto order = params[0];
  bridge_channel = query.channel;
  File bridge_read;
  void setupRead() {
    query.say("Setting up read pipe on |", params[1], "|");
    bridge_read = new File(params[1], FileMode.In);
  }
  void setupWrite() {
    query.say("Setting up write pipe on |", params[2], "|");
    bridge_write = new File(params[2], FileMode.Out);
  }
  if (order == "first") {
    setupWrite();
    setupRead();
  } else {
    setupRead();
    setupWrite();
  }
  query.say("Ready. ");
  while (true) {
    auto line = bridge_read.readLine();
    query.say(line);
  }
}

void wbOnJoin(Query query) {
//   sleep(1);
//   auto q2 = query;
//   if (q2.channel == "#tropers") q2.name = cast(nickname) q2.channel;
//   q2.notice("No it hasn't. STFU GreetBot you tard. ");
  auto ch = query.channel.replace("#", ""), name = cast(string) query.name;
  if (!wbfile.has(ch.clean(), name)) return;
  auto wbs = wbfile.get!(string[])(ch.clean(), name, null);
  int rsel = rand() % wbs.length;
  query.act(Format("welcomes ", name, " (", rsel+1, " / ", wbs.length, "): <u>", wbs[rsel], "</u>"));
}

void onFind(IRCconn ic, string channel, hostmask host) {
  auto q = Query(ic, .nick(host), channel), name = host.nick();
  // q.say("TEST: name ", name, ", host ", host, ", channel ", channel);
  synchronized(notefile) {
    auto notes = notefile.get!(Stuple!(string, typeof(µsec()), string)[])(channel.clean(), name, null);
    if (notefile.has(channel.clean(), name)) notefile.del(channel.clean(), name);
    
    notes ~= notefile.get!(Stuple!(string, typeof(µsec()), string)[])("global", name, null);
    if (notefile.has("global", name)) notefile.del("global", name);
    
    foreach (note; notes) q.answer(note._2.replace("$WHEN", timediff(µsec() - note._1)));
  }
}

void dbg(Query q) {
  q.notice("Users: ", q.connection.users);
}

void addnote(Query q) {
  bool global;
  if (auto rest = q.param.startsWith("global ")) {
    q.param = rest;
    global = true;
  }
  if (!q.param.length) return q.answer("Usage: addnote <target name> <message>");
  // if (!q.connection.registered(q.name)) {
  //   return q.answer("You have to be registered to do that! ");
  // }
  q.param = q.param.strip();
  auto name = q.param.slice(" ");
  /*if (q.channel in q.connection.users) 
    foreach (other; q.connection.users[q.channel]) {
      if (name == other) return q.answer("But s/he's right here! ");
    }*/
  synchronized(notefile) {
    if (!q.param.strip().length) return q.answer("No message set! ");
    auto a = q.channel, b = name;
    if (global) a = "global";
    auto notes = notefile.get!(Stuple!(string, typeof(µsec()), string)[])(a, b, null);
    int count; foreach (note; notes) if (note._0 == cast(string) q.name) count++;
    if (count >= 3) return q.answer("Cannot leave more than three messages for somebody! ");
    notes ~= stuple(cast(string) q.name, µsec(), Format(q.name, " left a note $WHEN: ", q.param));
    notefile.set(a, b, notes);
    return q.notice("Note saved! ");
  }
}

string[string] last_added;

import tools.downloader;
void addquote(Query query) {
  query.param = query.param.strip();
  if (!query.param.length) {
    return query.answer(CTRL(query.channel), "addquote <nick> <quote> or <quote>");
  }
  auto ch = query.channel.replace("#", "");
  auto nick = query.param.slice(" ");
  if (nick.startsWith("<")) {
    query.param = nick ~ " " ~ query.param; // part of message
    nick = nick[1 .. $];
    nick = nick.slice(">");
    while (nick.length && "%@+-*".find(nick[0]) != -1) nick = nick[1 .. $];
    if (!nick.length) return query.answer("Invalid nickname! ");
  }
  if (cast(nickname) nick == query.name && query.name != root_user) return query.answer("Please don't selfquote! ");
  last_added[ch.clean() ~ "\x00" ~ cast(string) query.name] = nick;
  auto quotes = wbfile.get!(string[])(ch.clean(), nick, null);
  quotes ~= query.param;
  wbfile.set(ch.clean(), nick, quotes);
  // return query.answer(Format("Added quote for ", nick, " (", quotes.length, "): ", query.param, "."));
  return query.answer("Quote added (", quotes.length, ")");
}

int do_remove(string channel, string nick) {
  auto quotes = wbfile.get!(string[])(channel.clean(), nick, null);
  if (!quotes.length) return -1;
  quotes = quotes[0 .. $-1];
  wbfile.set(channel.clean(), nick, quotes);
  return quotes.length;
}

void unquote(Query query) {
  query.param = query.param.strip();
  if (!query.param.length) {
    auto key = query.channel.replace("#", "").clean() ~ "\x00" ~ cast(string) query.name;
    scope(exit) last_added.remove(key);
    if (auto n = key in last_added) {
      auto nick = *n;
      auto u = do_remove(query.channel.replace("#", "").clean(), nick);
      if (u == -1) throw new Exception("What the hell? ");
      return query.answer("Last quote removed (", u, ")");
    }
    return query.answer("Usage: unquote <name>");
  }
  if (query.name != root_user) return query.answer("Not authorized. ");
  auto ch = query.channel.replace("#", "");
  auto nick = query.param;
  auto n = do_remove(ch, nick);
  if (n == -1) return query.answer("No quotes for "~nick~"!");
  return query.answer("Last quote removed (", n, ")");
}

import tools.downloader, tools.serialize;
void quote(Query query) {
  if (query.param == "s") return quotes(query);
  if (query.param == "help") {
    return query.answer(CTRL(query.channel), "quote [nick]");
  }
  void fn(string q, Query query) {
    auto ch = query.channel.replace("#", "").strip();
    if (!ch.length) return;
    synchronized (nexts_sync) nexts[query.name] = q /apply/ &fn;
    string[] quotes;
    if (q.length) {
      quotes = wbfile.get!(string[])(ch.clean(), q, null);
      if (!quotes.length) {
        auto quotemap = wbfile.section_map(ch.clean());
        foreach (key, value; quotemap)
          quotes ~= deserialize!(string[])(value);
        string[] matches;
        foreach (quote; quotes) if (quote.ifind(q) != -1) matches ~= quote;
        if (!matches.length) return query.answer("No quotes for "~q~"!");
        return query.say(matches[rand()%$]);
      }
    } else {
      auto quotemap = wbfile.section_map(ch.clean());
      foreach (key, lines; quotemap) {
        foreach (entry; wbfile.get!(string[])(ch.clean(), key, null))
          quotes ~= Format(key, ": ", entry);
      }
    }
    if (!quotes.length) return query.say(Format("No quotes! "));
    else return query.say(Format("Quote for ", quotes[rand%$]));
  }
  fn(query.param, query);
}

alias loli.cmp cmp;

import pad.engine: pastepost;
void quotes(Query query) {
  auto ch = query.channel.replace("#", "");
  auto quotelist = wbfile.section_map(ch.clean())
    /map/ (string key, string value) { return stuple(key, deserialize!(string[])(value)); };
  string[] parts;
  foreach (x; quotelist) {
    auto key = x._0, value = x._1;
    string mine;
    mine ~= Format(key, ": \n");
    string[] pieces;
    foreach (line; value) {
      pieces ~= "  "~line.replace(" <", "\n  <");
    }
    mine ~= std.string.join(pieces, "\n  --------\n");
    parts ~= mine;
  }
  auto text = std.string.join(parts, "\n  ==========\n");
  return query.answer(pastepost(text, query.connection.nick, "d"));
}

void wb(Query query) {
  auto ch = query.channel.replace("#", "");
  auto quotes = wbfile.section_map(ch.clean());
  query.say("Debug: ch ", ch, " -> ", ch.clean());
  string[int] answer;
  foreach (nick, line; quotes) {
    int len = wbfile.get!(string[])(ch.clean(), nick, null).length;
    if (len in answer) answer[len] ~= ", "~nick;
    else answer[len] = Format(len, " for ", nick);
  }
  Stuple!(string, int)[] flat;
  foreach (key, value; answer) flat ~= stuple(value, key);
  flat = flat.qsortfn(ex!("fn -> a, b -> fn(a._1, b._1)")(&cmp!(int)));
  query.answer("WB stats: ", std.string.join(flat /map/ ex!("e, f -> e"), "; "));
}

import pad.utils: replaceEntities;

extern(C) int chmod(char* path, ushort perms);
struct PixivSession {
  string session_id;
  const User = "454545", Pass = "454545";
  string relogin() {
    logln("Acquiring new session id");
    string[] ids;
    string redir;
    auto res = .download("POST=mode=login&pixiv_id="~User~"&pass="~Pass~" http://www.pixiv.net/index.php", &redir, 16, (string cookie) {
      logln("COOKIE ", cookie);
      ids ~= cookie.between("PHPSESSID=", ";");
    });
    logln("=> ", ids);
    if (ids.length != 2) throw new Exception("Site behavior has changed. Please reverify. ");
    session_id = ids[1];
    if (res.find("Sign up for pixiv</div") != -1)
      throw new Exception("Pixiv login data is bad! ");
    return res;
  }
  string download(string url) {
    auto start_id = session_id;
    logln("Nurl: COOKIE=PHPSESSID="~session_id~" "~url);
    auto res = .download("COOKIE=PHPSESSID="~session_id~" "~url);
    if (res.find("Sign up for pixiv</div") != -1) {
      synchronized {
        if (start_id == session_id) {
          relogin();
        } // else another thread already logged us in
        res = .download("COOKIE=PHPSESSID="~session_id~" "~url);
      }
    }
    return res;
  }
  string savePicture(string url) {
    auto nurl = url.replace("mode=medium", "mode=big");
    auto data = download("REFER="~url~" "~nurl), img = data.between("img src=\"", "\"");
    if (!img) {
      logln("Data for ", nurl, ": \n", data);
      throw new Exception("BAIL");
    }
    auto picurl = "REFER="~nurl~" "~img;
    auto target = "/mnt/data/www/srv/pixiv_unsuck".sub(picurl.getFilename());
    if (!target.exists()) {
      target.write(download(picurl));
      chmod(toStringz(target), 0744);
    }
    return "http://demented.no-ip.org/pixiv_unsuck".sub(picurl.getFilename().urlencode());
  }
}

static PixivSession pixivUnsuck;

string getTitle(string url, string src = null) {
  try return _getTitle(url, src);
  catch (Exception ex) return Format("[", ex, "]");
}

string _getTitle(string url, string src = null) {
  if (url.find("pixiv.net/") != -1) {
    auto nurl = pixivUnsuck.savePicture(url);
    return "Unsucked to "~nurl~" !";
  }
  auto anchor = url.find("#");
  if (anchor != -1) url = url[0 .. anchor];
  if (!src) src = url.download_first();
  if (src.startsWith("module")) return "D code. ";
  
  auto title = src.between("<title>", "</title>");
  if (!title) return null;
  
  string extra;
  string[string] extras;
  if (pwn_site(url, extras, src)) {
    foreach (key, value; extras)
      extra ~= Format("[", key, " ", value, "]");
  }
  if (url.ifind("escapistmagazine.com/videos/view/zero") != -1) {
    auto js_url = src.between("flashvars=\"config=", "\"");
    if (!js_url) goto mistake;
    auto js = js_url.download();
    string video;
    foreach (str; js.betweens("'", "'")) {
      if (str.endsWith(".flv" /or/ ".mp4")) { video = str; break; }
    }
    if (!video) goto mistake;
    if (video.startsWith("rtmp")) {
      auto fn = title.between("Video Galleries : ", "").replace(": ", "").replace("'", "").replace(" ", "_")~".mp4";
      extra ~= " [ flvstreamer -o "~fn~" -r $(wget "~video.mktiny()~" 2>&1 |grep Location |awk '{print $2}') ]";
    } else {
      extra ~= " ["~video[$-3 .. $]~" "~video.mktiny()~"]";
    }
  }
  mistake: // Wait, my mistake. Not a ZP video after all.
  auto res = title.replaceEntities().replace("\n", "").strip() ~ extra;
  bool booru =
    url.tolower().find("gelbooru.com") != -1
    || url.tolower().find("booru.donmai.us") != -1;
  if (booru) {
    auto tags = res.split(" ");
    auto shown = tags; if (shown.length > 14) shown.length = 14;
    string omit;
    if (tags.length > shown.length)
      omit = Format("[", tags.length - shown.length, " omitted]");
    res = std.string.join(shown, " ") ~ omit;
  }
  return res;
}

void tell(Query query) {
  auto backup = query;
  query.name = cast(nickname) query.param.slice(" ");
  if (auto rest = query.param.strip().startsWith(CTRL(query.channel))) {
    if (query.name == root_user || rest.startsWith("cah" /or/ "rps"))
      return backup.answer("This used to be a security leak. It's closed now. ");
    query.param = rest;
    runQuery(query);
  } else query.answer(query.param);
}

void ip(Query query) {
  query.answer("I'm online from ", "whatismyip.com/automation/n09230945.asp".download());
}

void control_char(Query query) {
  setCtrl(query.channel, query.param);
  if (query.name != root_user) return query.answer("You are not authorized to do that.");
  query.answer("Control character is now `", CTRL(query.channel), "'.");
}

string timediff(typeof(µsec()) dist) {
  dist /= 1_000_000;
  string res;
  void add(string st) { if (res.length) res ~= ", "; res ~= st; }
  int count;
  bool addUnit(int size, string name) {
    if (dist > size) {
      count ++;
      auto div = dist / size;
      add(Format(div, " ", name));
      dist -= div * size;
      if (size == 1) return true;
    }
    if (count == 2) return true;
    return false;
  }
  if (addUnit(60*60*24*7, "weeks")
    ||addUnit(60*60*24, "days")
    ||addUnit(60*60, "hours")
    ||addUnit(60, "minutes")
    ||addUnit(1, "seconds")
  ) return res~" ago";
  return "right now";
}

void seen(Query query) {
  if (!query.param.length) return query.answer("Usage: "~CTRL(query.channel)~"seen <nick>");
  if (!seenfile.has(query.channel.clean(), query.param.tolower())) return query.answer("I have not seen "~query.param~" in this channel!");
  auto entry = seenfile.get!(Stuple!(string, typeof(µsec())))(query.channel.clean(), query.param.tolower(), { fail; return stuple("", µsec()); }());
  string mesg;
  if (auto rest = entry._0.startsWith("\x01ACTION ")) {
    rest = rest[0 .. $-1];
    mesg = Format(" doing \" * ", query.param, " ", rest, "\".");
  } else mesg = Format(" saying \"", entry._0, "\".");
  query.answer("I have last seen ", query.param, " ", timediff(µsec() - entry._1), mesg);
}

void vstats(Query query) {
  auto chquery = query;
  chquery.name = cast(nickname) chquery.channel;
  int[string] stats;
  notice_cb = (hostmask h, string s) {
    auto parts = s.split(" ")[1 .. $];
    synchronized {
      if (parts.length) {
        parts[0] = parts[0].strip();
        if (parts[0] != "(") stats[parts[0]] ++;
      }
    }
  };
  chquery.say("\x01VERSION\x01");
  auto start = sec();
  while (start + 8 > sec()) slowyield();
  notice_cb = null;
  // cribbed from cstats
  auto rstats = invert(stats), top = rstats.keys.sort;
  if (top.length > 10) top.length = 10;
  if (top.length && top[0] == 1) top = top[1 .. $];
  query.answer("Version stats: <u>", top /map/ ex!("a -> b -> stuple(b, a[b])")(rstats), "</u>");
}

void main(string[] args) {
  while (true) {
    try {
      Main(args);
      return;
    } catch (Exception ex) {
      logln(ex, "! Restarting in 10s. ");
      sleep(10);
    }
  }
}

void delegate(hostmask, string) notice_cb;

void delegate(Query)[nickname][string] greenmap;
void delegate(Query) delegate(nickname)[string] threadgens;

void dispatchGreenCmd(string base, Query q) {
  auto n = q.name;
  if (base /notin/ threadgens) fail("No such green command: "~base);
  void delegate(Query) sk;
  synchronized {
    if (base /notin/ greenmap) {
      greenmap[base] = Init!(typeof(greenmap[base]));
    }
    if (n /notin/ greenmap[base]) {
      greenmap[base][n] = threadgens[base](n); // start up thread
    }
    sk = greenmap[base][n];
  }
  sk(q);
}

Threadpool tgtp;

static this() {
  New(tgtp, Threadpool.GROW);
  threadgens["ps"] = (nickname nick) {
    auto conduit = new MessageMultiChannel!(Query, true, false);
    tp.addTask(stuple(conduit, nick) /apply/ (typeof(conduit) conduit, nickname nick) {
      start: scope(exit) goto start;
      Query last;
      string getCmd() { last = conduit.get(); return last.param; }
      auto max = getCmd().atoi();
      if (!max) return last.answer("Please provide a point limit. ");
      int[6] points; // str dex con int wis cha
      points[] = 8;
      int used(int[] field) {
        int res;
        foreach (point; field) {
          point -= 8;             res += point;
          point -= min(point, 6); res += point;
          point -= min(point, 2); res += point;
        }
        return res;
      }
      int[string] map = ["str": 0, "dex": 1, "con": 2, "int": 3, "wis": 4, "cha": 5];
      while (true) {
        last.answer("Str ", points[0], " Dex ", points[1], " Con ", points[2], " Int ", points[3], " Wis ", points[4], " Cha ", points[5],
          ". ", max - points.used(), " left. ");
        dontReprint:
        auto cmd = getCmd();
        auto parts = cmd.split(";");
        int[6] dest = points;
        foreach (part; parts) {
          if (auto rest = part.startsWith("add " /or/ "put ")) {
            auto name = rest.slice(" "), value = rest;
            if (!value) value = "1";
            if (value.atoi() == 0 && name.atoi() != 0) swap(value, name);
            auto v = value.atoi(), n = name.strip().tolower();
            if (n /notin/ map) { last.answer("No such attribute: ", n, "!"); goto dontReprint; }
            dest[map[n]] += v;
          } else if (auto rest = part.startsWith("take " /or/ "sub ")) {
            auto name = rest.slice(" "), value = rest;
            if (!value) value = "1";
            if (value.atoi() == 0 && name.atoi() != 0) swap(value, name);
            auto v = value.atoi(), n = name.strip().tolower();
            if (n /notin/ map) { last.answer("No such attribute: ", n, "!"); goto dontReprint; }
            dest[map[n]] -= v;
          } else if (auto rest = part.startsWith("reset").strip()) {
            if (rest.atoi()) max = rest.atoi();
            dest[] = 8;
          } else if (auto rest = part.startsWith("set").strip()) {
            auto sp = rest.split(" ");
            if (sp.length != 6 /or/ 1) { last.answer("Invalid number of attributes! "); goto dontReprint; }
            if (sp.length == 1) dest[] = sp[0].atoi();
            else dest[] = sp /tools.functional.map/ (string s) { return cast(int) atoi(s); };
          } else if (auto rest = part.startsWith("move ")) {
            auto num = rest.slice(" ").atoi(), from = rest.slice(" ").tolower(), to = rest.slice(" ").tolower();
            if (num !> 0 || !from || !to) { last.answer("Invalid parameter! "); goto dontReprint; }
            if (from /or/ to /notin/ map) { last.answer("No such attribute! "); goto dontReprint; }
            dest[map[from]] -= num;
            dest[map[to]] += num;
          }
        }
        if (dest.used() > max) { last.answer("Not enough points: short by ", dest.used() - max, ". "); goto dontReprint; }
        foreach (val; dest) {
          if (val < 8) { last.answer("Can't lower attributes below 8! "); goto dontReprint; }
        }
        points[] = dest;
      }
    });
    return &conduit.put;
  };
}

void pointbuy(Query q) {
  if (!q.param.length) return q.answer("Usage: pointbuy attr1 attr2 .. attr6");
  auto attrs = q.param.split(" ") /map/ &atoi;
  if (attrs.length != 6) return q.answer("Invalid number of attributes! ");
  int points;
  attrs /map/ (int i) {
    i -= 8;         points += i;
    i -= min(i, 6); points += i;
    i -= min(i, 2); points += i;
  };
  return q.answer(points, " points. ");
}

void overlap(Query q) {
  q.param = q.param.strip();
  auto ch1 = q.param.slice(" ");
  if (!q.param.length) return q.answer("Usage: overlap <channel 1> <channel 2>");
  auto ch2 = q.param.strip();
  auto users1 = new Stuple!(bool[string]);
  q.connection.whoare(ch1,
    users1 /apply/ (typeof(users1) users1, string nick, string line) { users1._0[nick] = true; },
    stuple(q, ch1, ch2, users1) /apply/ (Query q, string ch1, string ch2, typeof(users1) users1) {
      auto dups = new Stuple!(string[]);
      q.connection.whoare(ch2,
        stuple(users1, dups) /apply/ (typeof(users1) users1, typeof(dups) dups, string nick, string line) {
          if (nick in users1._0) dups._0 ~= nick;
        }, stuple(q, ch1, ch2, dups) /apply/ (Query q, string ch1, string ch2, typeof(dups) dups) {
          string rest;
          if (dups._0.length > 16) {
            rest = Format(" and ", dups._0.length - 16, " more");
            dups._0 = dups._0[0 .. 16];
          }
          q.answer("Shared users between ", ch1, " and ", ch2, ": ", dups._0, rest, ".");
        }
      );
    }
  );
}

void msg(Query query) {
  if (query.name != root_user) {
    return query.answer("Unauthorized! ");
  }
  query.name = cast(nickname) query.param.slice(" ");
  query.privtell(query.param);
}

import dice;
void roll(Query query) { roll2(query, false); }

void roll2(Query query, bool sentence) {
  scope(exit) reset();
  string str = query.param.strip();
  if (!sentence && !str.length) {
    return query.answer("roll - dice rolls. Supported syntax: 8d4k4 + 3L - H < 15 (<b>k</b>eep, <b>L</b>ow, <b>H</b>igh).");
    // query.answer("foo against bar, >= bar, <= bar, >, < can be used to roll against a value.");
  }
  Result delegate() dg;
  auto start_str = str;
  if (!gotResult(str, dg) && !sentence) {
    return query.answer("I do not understand "~str~"!");
  }
  if (!dg) return; // sentence
  auto munched = start_str[0 .. $-str.length].strip();
  bool number = true;
  foreach (ch; munched) if ((ch < '0' || ch > '9') && ch != '-') number = false;
  if (number) return;
  str = str.strip();
  if (!sentence && str.length && !query.inMiddle) {
    return query.answer("Left over: "~str~"!");
  }
  auto res = dg();
  if (res.text.length > 256) res.text = res.text[0 .. 256] ~ "[...]";
  // return query.answer(res.text, (res.text.length?" :":""), "▕<i>", res.sum, "</i>▏");
  if (res.suppressNum) return query.answer(res.text);
  else return query.answer(res.text, (res.text.length?": ":""), "<b>", res.sum, "</b>");
}

import vars, loli, mafia, maid, std.date: getUTCtime, dateToString = toString;
// Organically grown.
void Main(string[] _args) {
  exec = _args[0]; args = _args[1 .. $];
  uint resume_handle = -1;
  logln("Arguments: ", args);
  string hdl_t;
  if (args.length > 4)
    if (args[4] == "--resume") {
      auto parts = args[5 .. $];
      args = args[0 .. 4];
      auto hdl_id = parts[0]; hdl_t = parts[1];
      reload_nick = cast(nickname) parts[2];
      count = parts[3].atoi() + 1;
      resume_handle = hdl_id.atoi();
    }
  
  if (args.length != 4) {
    logln("Usage: ", exec, " <server> <nick> <channel> <root user>");
    return;
  }
  root_user = cast(nickname) args[3];
  
  void help(Query query) {
    query.notice(Format("Supported commands: ", commands.keys));
  }
  string[string] last_url;
  auto last_url_sync = new Object;
  anypos["trope"] = anypos["roll"] = true; 
  foreach (key, value; [
    "roll"[]: &roll /todg, "dice": &roll /todg, "google": &google /todg, "gcalc": &gcalc /todg,
    "anidb": (Query q) { return anidb(q, 0, "", false); }, "decide": &decide /todg,
    "vote": &vote /todg, "rr": &rr /todg, "rps": &rps /todg,
    "more": &more /todg, "join": &join /todg, "part": &part /todg,
    "help": &help, "cstats": &cstats /todg, "cfind": &cfind /todg, "vstats": &vstats /todg,
    "msg": &msg /todg,
    "reload": &reload /todg, "ps": &dispatchGreenCmd /fix/ "ps",
    "wb": &wb /todg, "tell": &tell /todg, "ip": &ip /todg,
    "say": &say /todg, "wp": &wp /todg, "addquote": &addquote /todg,  // "loli": &loliHandle /todg,
    "quotes": &quotes /todg, "quote": &quote /todg, "mafia": &handle_mafia /todg,
    "ctrl_char": &control_char /todg, "link": (Query q) { q.answer("I like pie!"); },
    "unquote": &unquote /todg,
    "bridge": &bridge /todg, "seen": &seen /todg, "trope ": &trope /todg,
    "tropesample": &tropesample /todg, "maid": &mkMaid /todg, "mansion": &mkMansion /todg,
    "pad": &padfn /todg, "addstart": &addOnStart /todg, "repad": &repad /todg,
    "pointbuy": &pointbuy /todg, "cah": &cahfn /todg, "df": &df /todg,
    "note": &addnote /todg, "dbg": &dbg /todg, "overlap": &overlap /todg,
    "fwoosh": &gun /todg, "fwoosh_up": &fwoosh_upd /todg,
    "title": (Query q) {
      if (q.param.length) q.answer("<b>", q.param.getTitle(), "</b>");
      else synchronized(last_url_sync) if (auto p = q.channel in last_url) q.answer("<b>", (*p).getTitle(), "</b>");
    }
  ]) commands[key] = value;
  auto conn = new IRCconn(args[0], cast(nickname) args[1], 6667, resume_handle);
  conn.onFind = conn /apply/ &onFind;
  auto aborted = new bool;
  scope(exit) *aborted = true;
  tp.addTask(aborted /apply/ (ref bool aborted) {
    while (!aborted) {
      conn.raw_sendln("PING "~args[1]);
      auto start = sec();
      sleep(30);
      if (sec() - start < 20) {
        logln("Timing is off. ");
        while (sec() - start < 30) sleep(1);
      }
    }
  });
  socket_handle = conn.sock.handle;
  auto lastpoke = sec(), cooldown = 0.0;
  bool isPoking() { return sec() < lastpoke + 5; }
  bool blockPoking() { return sec() < cooldown + 40; }
  void delegate(int)[string] channel_guards, wb_guards;
  bool[hostmask] justJoined;
  // int[hostmask] strikes;
  with (conn) {
    defaultChanHandler = (string channel, hostmask host, string msg) {
      if (!channel.length && notice_cb) { notice_cb(host, msg); return true; }
      logln("< ", msg);
      if (msg is IRCconn.JOIN) {
        bool netsplit;
        if (channel /notin/ wb_guards)
          wb_guards[channel] = floodguard!(false)(3, 1, { netsplit = true; }, { netsplit = false; });
        wb_guards[channel](1);
        if (!netsplit) wbOnJoin(Query(conn, .nick(host), channel));
        justJoined[host] = true;
      } else {
        if (host in justJoined) {
          if (msg.find("sendspace.com") != -1 || msg.ifind("thescripthere.tk") != -1 || msg.find("proxygod.com") != -1
            || msg.ifind("xroxy.") != -1) {
            // strikes[host] ++;
            auto q = Query(conn, cast(nickname) "", channel);
            // if (strikes[host] >= 3) {
              q.say("Breakin' out the banhammer! ");
              ban(channel, host);
            // } else q.say(Format(3-strikes[host], " strikes left. "));
            return true;
          } else justJoined.remove(host);
        }
      }
      // #fetishfuel special handling
      // ah well. Batabii is gone anyway. 
      // gah. he's back.
      if (channel.tolower() == "#fetishfuel" && msg.characters().tolower() == "lol") {
        kick(channel, .nick(host), "Ell Oh Ell! ");
      }
      // #himitsu special handling
      if (channel.tolower() == "#himitsu" && msg.startsWith("!list" /or/ "!new")) {
        tp.addTask(stuple(Query(conn, .nick(host), channel), conn) /apply/ (Query q, IRCconn ic) {
          if (ic.exists(cast(nickname) "Himitsu_Millenium_Robot")) return;
          q.notice("Our bot isn't here right now. Perhaps try the archive bots in #lurk? ");
        });
        return true;
      }
      if (.nick(host) == root_user && msg.find("SHUTDOWN") != -1) throw new Exception("Shut it doooooooown!");
      seenfile.set(channel.clean(), (cast(string) .nick(host)).tolower(), stuple(msg, µsec()));
      bool permit = true;
      // if (channel.endsWith("tropers")) permit = false; // fucking bot nazis
      if (channel.tolower().endsWith("anime")) permit = false; // moar nazis
      // echo is a silly feature
      /*if (permit)
        if (auto action = msg.beginsWith("\x01ACTION ")) {
          if (blockPoking()) return false;
          action = action[0 .. $-1];
          if (channel /notin/ users) return false;
          string match;
          foreach (nick; users[channel]) {
            if (match.length < nick.length && action.find(nick) != -1) match = nick;
          }
          if (!match) return false;
          if (.nick(host) == cast(nickname) "ImoutoBot") return false;
          action = action.replace(match, cast(string) .nick(host));
          message(channel, "\x01ACTION "~action~"\x01");
          lastpoke = sec();
          return false;
        }
      */
      if (false && msg.ifind("stop") != -1 && isPoking()) {
        auto r = rand();
        logln(r, " -- ", r % 5);
        Query(conn, .nick(host), channel).answer(
          ["Aww. Okays."[], "Okay .. :/", "If you say so ._.", "Alright.", "No more pokety :("][r % $]
        );
        cooldown = sec();
      }
      auto animes = msg.ifind("animes");
      const vocals="aeiouAEIOUqQ";
      if (animes != -1 && (animes+6 == msg.length || vocals.find(msg[animes+6]) == -1)) {
        Query(conn, .nick(host), channel).answer(
          ["Anime."[], "It's anime.", "Anime is its own plural. ", "Anime.", "Anime >_>", "Just \"anime\"."]
          [rand()%$]
        );
        return true;
      }
      if (onFind) onFind(channel, host);
      auto name = .nick(host);
      if (!channel.length) channel = .nick(host);
      auto lpos = msg.find("http://");
      if (auto rest = msg.startsWith(CTRL(channel))) {
        try roll2(Query(conn, name, channel, rest), true);
        catch (Exception ex) {
          Query(conn, name, channel, rest).say("Error: ", ex);
        }
      }
      if (auto rest = msg.startsWith("> ")) {
        auto q = Query(conn, name, channel, "> "~rest);
        try padfn(q);
        catch (Exception ex) q.answer("Error: ", ex);
      }
      // logln("Ctrl code is ", CTRL(channel), " for ", channel);
      if (auto cmd = msg.startsWith(CTRL(channel))) {
        if (.nick(host) == cast(nickname) "ljrbot") {
          Query(conn, .nick(host), channel, "").say("I don't take orders from a <i>bot. </i>");
          return true;
        }
        bool flooded;
        if (channel /notin/ channel_guards)
          channel_guards[channel] = floodguard!(false)(6, 3, { flooded = true; }, { flooded = false; });
        channel_guards[channel](1);
        auto query = Query(conn, name, channel, cmd);
        if (flooded && (name != root_user)) query.notice("You have triggered my flood guard prevention. Please wait a bit.");
        else tp.addTask(&runQuery /fix/ query);
      } else if (channel.endsWith("anime")) {
        if (msg.ifind("meow") != -1 && msg.ifind("meow_") == -1)
          Query(conn, name, channel, "meow").act("licks ", name);
        if (msg.ifind("#techtalk") != -1)
          Query(conn, name, channel, "meow").act("nods");
      } else if (lpos != -1) {
        auto link = msg[lpos .. $];
        if (auto res = link.endsWith("\x01")) link = res; // part of a /me
        if (link.find(" ") != -1) link = link.slice(" ");
        synchronized(last_url_sync) last_url[channel] = link;
        if (channel.endsWith("tropers")) {
          permit |= link.ifind("youtube.") != -1;
          permit |= link.ifind("tinyurl.") != -1;
          permit |= link.ifind("/ptitle") != -1;
        }
        if (channel.tolower() == "#d") permit = false;
        permit &= link.ifind("scp-wiki.wikidot.com") == -1;
        if (channel.tolower() == "#d") permit = false;
        if (permit) {
          tp.addTask(stuple(link, channel, Query(conn, name, channel, ""), lpos + link.length + 2)
            /apply/ (string link, string channel, Query query, int ll) {
            auto title = link.getTitle();
            if (channel.endsWith("tropers")) {
              string rss;
              if (std.file.exists("rss_tropers"))
                rss = cast(string) "rss_tropers".read();
              auto name = title;
              if (title.find("LostWorlds") != -1) {
                query.answer("Fuck off, spammer. ");
                return;
              }
              if (!name.length) name = link;
              string ent_encode(string s) {
                return s.replace("&", "&amp;");
              }
              auto isImage = name.tolower().endsWith(".jpg" /or/ ".jpeg" /or/ ".gif" /or/ ".png");
              if (!rss.length) rss = `<?xml version="1.0"?><rss version="2.0">
              <channel><title>#tropers feed</title></channel></rss>`;
              rss = rss.replace("</channel>", Format("
                <item>
                  <title>", name.ent_encode(), "</title>
                  <link>", link.ent_encode(), "</link>
                  <pubDate>", dateToString(getUTCtime()), "</pubDate>
                  <guid>", rss.length, "</guid>",
                  (!isImage)?"":("<description>&lt;img src=\""~link~"\" /&gt;</description>"),
                "</item>
              </channel>"));
              synchronized { 
                "rss_tropers.tmp".write(cast(void[]) rss);
                system("rm rss_tropers; mv rss_tropers.tmp rss_tropers; chmod a+r rss_tropers");
              }
            }
            //string foo;
            //for (int i = 0; i < ll; ++i) foo ~= " ";
            string f = "u";
            if (channel.endsWith("tropers")) f = "b";
            if (title.length) query.say(/*foo, */"<", f, ">", title, "</", f, ">");
          });
        }
      } else foreach (key, value; commands) {
        auto match = msg.find(CTRL(channel)~key);
        if (key.strip() /notin/ anypos) continue;
        if (match != -1) {
          tp.addTask(&runQuery /fix/ Query(conn, name, channel, msg[match+1 .. $], 0, true));
        }
      }
      // This is silly. BUT SO WHAT.
      if (permit && msg.find("♥") != -1) {
        Query(conn, name, channel, "wub").say("Awwwwwwwwwwww!");
        return false;
      }
      if (bridge_write && channel == bridge_channel) {
        if (auto rest = msg.startsWith("\x01ACTION ")) {
          bridge_write.writeLine(Format("* ", name, " ", rest[0 .. $-1]));
        } else if (msg == IRCconn.JOIN) {
          bridge_write.writeLine(Format("> ", name, " has joined. "));
        } else {
          bridge_write.writeLine(Format("<", name, "> ", msg));
        }
      }
      return false;
    };
    privmsg = defaultChanHandler /fix/ "";
    tp.addTask(aborted /apply/ (ref bool aborted) {
      sleep (10);
      if (aborted) return;
      join(args[2], defaultChanHandler);
      foreach (line; onStart(args[0])) {
        privmsg(cast(hostmask) (cast(string) root_user ~ "!@"), line);
      }
    });
    if (hdl_t.length) {
      if (hdl_t.startsWith("#") && args[2] != hdl_t) join(hdl_t, defaultChanHandler);
      message(hdl_t, Format("Executable reloaded at request of ", reload_nick, ", ", count, " times on this connection. "));
      if (hdl_t.startsWith("#"))
        whoare(hdl_t, stuple(conn, hdl_t) /apply/
          (IRCconn conn, string channel, string nick, string line) { conn.users[channel] ~= nick; }, { }
        );
      hdl_t = null;
    }
    try {
      loop();
    } catch (Exception ex) {
      /*foreach (channel, bogus; channels) {
        message(channel, ex.toString~". Time to die. :(");
      }*/
      logln(ex, " Rethrowing. ");
      throw ex;
    }
  }
}
