module irc;
import tools.fixed_socket, tools.threads, tools.functional, tools.compat, tools.log;
import tools.page_queue; // work around a dsss bug

typedef string nickname; typedef string hostmask;

nickname nick(hostmask host) { return cast(nickname) host.slice("!"); }

import tools.time;

template GuardTypes(bool B) {
  // calls, seconds
  static if (B) { alias void delegate(int) Ret; alias Tuple!(int, int) Param; }
  else { alias void delegate(int) Ret; alias Tuple!(int, int, void delegate(), void delegate()) Param; /* or else */ }
}

GuardTypes!(delaymode).Ret floodguard(bool delaymode)(GuardTypes!(delaymode).Param p) {
  auto st = stuple(p);
  return stuple(new Stuple!(typeof(sec()), int)[0], new Lock, st) /apply/
  (ref Stuple!(typeof(sec()), int)[] times, Lock lock, typeof(st) st, int count) {
    auto calls = st._0, seconds = st._1;
    // remove all times that are longer away than "seconds"
    void cleanup() {
      auto s = sec();
      while (times.length && ((s - times[0]._0) > seconds || (s - times[0]._0) < 0)) {
        // logln(s, ": @", &times, ": remove 1 of ", times);
        times = times[1 .. $];
      }
    }
    int sum() {
      int res;
      foreach (time; times) res += time._1;
      return res;
    }
    lock.Synchronized = {
      static if (delaymode) {
        while (sum > calls) {
          cleanup();
          lock.Unsynchronized = { slowyield(); };
        }
      } else cleanup();
      times ~= stuple(sec(), count);
      static if (!delaymode) {
        if (sum > calls)
          st._2();
        else
          st._3();
      }
    };
  };
}

string delegate() lineReader(TcpSocket _sock) {
  return stuple("", _sock) /apply/
  (ref string buffer, TcpSocket sock) {
    int loc;
    while (true) {
      if ((loc = buffer.find("\r\n")) == -1) {
        char[256] recv_buffer;
        auto received = sock.receive(recv_buffer);
        if (received > 0)
          buffer ~= recv_buffer[0 .. received];
        else {
          version(Windows) { }
          else {
            if (received == -1 && getErrno() == 4) // eintr
              continue;
          }
          throw new Exception(Format("Connection closed/failed with ", received, " - ", getErrno()));
        }
      } else break;
    }
    auto res = buffer[0 .. loc];
    buffer = buffer[loc+2 .. $];
    return res;
  };
}

class IRCconn {
  TcpSocket sock;
  string host;
  string delegate() lines;
  nickname _nick;
  nickname nick() { return _nick; }
  static string JOIN;
  static this() {
    JOIN = "JOIN"; // compare "is", not "==".
  }
  bool delegate(string channel, hostmask host, string msg)[string] channels;
  string[][string] users;
  void delegate(int) guard;
  void delegate(string channel, hostmask host) onFind;
  void delegate(string) onJoin;
  bool registered(nickname n) {
    auto waiter = new Semaphore;
    bool reg;
    n.whois([320: (string name, string s) {
      if (s.find("is identified to services") != -1) return;
      if (s.find("is signed on as account "~cast(string) n) != -1) reg = true;
    }, 318: (string name, string s) { waiter.release; }]);
    waiter.acquire;
    return reg;
  }
  bool exists(nickname n) {
    auto waiter = new Semaphore, exists = true;
    n.whois([401: (string name, string s) { exists = false; }, 318: (string name, string s) { waiter.release; }]);
    waiter.acquire;
    return exists;
  }
  int sock_hack_offset;
  void kick(string channel, nickname name, string reason = "") {
    raw_sendln(Format("KICK ", channel, " ", cast(string) name, " :", reason));
  }
  void ban(string channel, hostmask mask) {
    raw_sendln(Format("MODE ", channel, " +b ", mask));
  }
  this(string host, nickname nick, int port = 6667, uint resume_handle = -1) {
    this.host = host;
    guard = floodguard!(true)(100_000, 60); // 100 kb per 60s
    if (resume_handle == -1) {
      auto dpos = host.find(":");
      if (dpos != -1) {
        auto backup = host;
        host = backup[0 .. dpos];
        port = backup[dpos+1 .. $].atoi();
      }
      logln("Opening TCP connection <", host, "> <", port, "> .. ");
      sock = new TcpSocket(new ActuallyWorkingInternetAddress(host, port));
      /*auto match = sock.handle(), test = cast(typeof(match)*) sock;
      int sock_hack_offset;
      while (*test != match) {
        test ++;
        sock_hack_offset ++;
      }
      logln("sock_hack_offset: ", sock_hack_offset);
      asm { int 3; }*/
      lines = lineReader(sock);
      _nick = nick;
      auto nick_s = cast(string) nick;
      raw_sendln("NICK "~nick_s);
      raw_sendln("USER "~nick_s~" "~nick_s~" "~nick_s~" :"~nick_s);
    } else {
      logln("Attempting to reuse TCP connection .. ");
      sock = new TcpSocket();
      static if ((void*).sizeof == 4) const int hack_offs = 2;
      else static if ((void*).sizeof == 8) const int hack_offs = 4;
      else static assert(false, "Unsupported platform!");
      (cast(socket_t*) sock)[hack_offs] = cast(socket_t) resume_handle;
      if (sock.handle() != resume_handle)
        throw new Exception("Class layout confusion! Hack failed :(");
      lines = lineReader(sock);
      _nick = nick;
    }
    logln("Done");
  }
  bool delegate(string) raw_recvln;
  bool delegate(string)[] callbacks;
  bool delegate(hostmask host, string msg) privmsg;
  bool delegate(string, hostmask, string) defaultChanHandler;
  void step() {
    auto line = lines();
    if (raw_recvln && !raw_recvln(line)) return;
    hostmask host;
    if (line.startsWith(":")) host = cast(hostmask) line.slice(" ")[1 .. $];
    if (auto rest = line.startsWith("PRIVMSG " /or/ "NOTICE ")) {
      auto target = rest.slice(" ");
      if (cast(nickname) target == _nick) {
        if (host && privmsg) privmsg(host, rest[1 .. $]);
        return;
      }
      if (target[0] != '#') {
        logln("As of now, can only dispatch channel privmsgs - ", target, " not acceptable");
        return;
      }
      assert(rest[0] == ':');
      if (target /notin/ channels) {
        if (defaultChanHandler) channels[target] = defaultChanHandler;
        else return;
      }
      if (!channels[target](target, host, rest[1 .. $])) return;
    }
    if (auto rest = line.startsWith("PING ")) {
      raw_sendln("PONG "~rest);
      return;
    }
    if (auto rest = line.startsWith("353 ")) {
      string header, main; ptuple(header, main) = rest.splitAt(":");
      auto chan = header.strip().split(" ")[$-1];
      if (chan /notin/ users) users[chan] = null;
      users[chan] ~= main.strip().split(" ") /map/ (string s) { if (auto r = s.startsWith("+" /or/ "@")) s = r; return s; };
      if (onJoin) onJoin(chan);
      logln(chan, " -> users: ", users[chan]);
    }
    if (auto rest = line.startsWith("JOIN")) {
      auto channel = rest[2 .. $].tolower(); // "JOIN :[channel]"
      if (onFind) onFind(channel, host);
      if (channel /notin/ channels)
        if (defaultChanHandler) channels[channel] = defaultChanHandler;
        else throw new Exception("Received JOIN for non-joIned channel "~channel~"; wtf?");
      if (!channels[channel](channel, host, JOIN)) return;
    }
    if (auto rest = line.startsWith("PART ")) {
      auto channel = rest.tolower();
      if (channel in users) users[channel] = users[channel] /select/ (string s) { return s != cast(string) .nick(host); };
      return;
    }
    if (auto rest = line.startsWith("KICK ")) {
      auto channel = rest.split(" ")[0];
      raw_sendln("JOIN "~channel);
      logln("Kicked by ", host);
      //message(channel, "Up yours.");
      return;
    }
    if (auto rest = line.startsWith("NICK")) {
      outer:foreach (channel, value; users) {
        foreach (ref person; value) {
          if (person == cast(string) .nick(host)) {
            if (onFind) onFind(channel, cast(hostmask) host.replace(person, rest));
            break outer;
          }
        }
      }
    }
    foreach (callback; callbacks) if (callback && !callback(line)) break;
    if (auto rest = line.startsWith("433")) {
      if (line.find("already in use") != -1) {
        throw new Exception("Nick already in use! ");
      }
    }
    logln(line);
  }
  void addCallback(bool delegate(string) dg) { synchronized(this) {
    foreach (ref callback; callbacks)
      if (!callback) { callback = dg; return; }
    callbacks ~= dg;
  } }
  void removeCallback(bool delegate(string) dg) { synchronized(this) {
    foreach (ref callback; callbacks) if (callback is dg) { callback=null; return; }
    throw new Exception("Found no callback to remove!");
  } }
  void loop() { while (true) step(); }
  void raw_sendln(string s) {
    if (s.length > 512) return logln("Message too large.");
    if (s.length > 16384) return logln("Excess flood averted");
    guard(s.length+2);
    logln("> ", s);
    synchronized {
      sock.send(s); sock.send("\r\n");
    }
  }
  void message(string where, string what) in { assert(where.length); } body {
    /*if (where.startsWith("#") && !(where in channels))
      throw new Exception(Format("Cannot send message to channel you haven't joined: ", where, " where channels are ", channels));*/
    if (what.length > 512) what = "Message too long! "; // asm { int 3; }
    raw_sendln("PRIVMSG "~where~" :"~what);
  }
  void whoare(string chan, void delegate(string nick, string line) response, void delegate() fin) {
    auto callback = selfcall(
      stuple(response, fin, this) /apply/
      (typeof(response) response, typeof(fin) fin, typeof(this) that, bool delegate(string) self, string line) {
        if (auto rest = line.startsWith("352 ")) {
          auto s = rest.split(" ");
          auto channel = s[1], user = s[2], host = s[3]; // , nick = .join(rest[rest.find(":")+1 .. $].split(" ")[1 .. $], " ");
          auto nick = s[5];
          response(nick, host);
          return false;
        }
        if (auto rest = line.startsWith("315 ")) { // ENDOFWHO
          logln("Done; removing callback");
          that.removeCallback(self);
          fin();
          return false;
        }
        return true;
      });
    auto ptr = new typeof(callback);
    *ptr = callback;
    addCallback(&ptr.opCall);
    raw_sendln("WHO "~chan);
  }
  void whois(nickname nick, void delegate(string, string)[int] responses) {
    auto cb = selfcall(
      stuple(nick, responses, this) /apply/
      (nickname nick, typeof(responses) responses, typeof(this) that, bool delegate(string) self, string line) {
        string repl_nick, user, host, message;
        void fill(string rest) {
          auto s = rest.split(" ");
          repl_nick = s[1]; user = s[2]; host = s[3]; message = rest[rest.find(":")+1 .. $];
        }
        foreach (key, value; responses) {
          if (auto rest = line.startsWith(Format(key, " "))) {
            fill(rest);
            if (nick != cast(nickname) repl_nick) return true; // not us
            bool block_remove;
            value(host, message);
            if (key == 318) that.removeCallback(self);
            return false;
          }
        }
        return true;
      }
    );
    auto ptr = new typeof(cb);
    *ptr = cb;
    addCallback(&ptr.opCall);
    raw_sendln("WHOIS "~cast(string) nick);
  }
  void join(string channel, bool delegate(string, hostmask, string) dg = null) {
    // This happens VERY often.
    if (channel in channels) return; // throw new Exception("Cannot join channel twice");
    if (!dg) dg = defaultChanHandler;
    if (!dg) throw new Exception("No channel handler found");
    channels[channel] = dg;
    raw_sendln("JOIN "~channel);
  }
  void part(string channel) {
    if (!(channel in channels)) throw new Exception("Cannot part channel you're not in");
    channels.remove(channel);
    raw_sendln("PART "~channel);
  }
  void quit() {
    raw_sendln("QUIT");
  }
}

string IRCFormat(T...)(T t) {
  string res;
  int bold_depth, ul_depth, inv_depth;
  void delegate(string, ref string)[string] commands =
  ["<b>"[]: (string pre, ref string post) {
    if (!bold_depth) res ~= pre~"\x02";
    bold_depth++;
  }, "</b>": (string pre, ref string post) {
    bold_depth--;
    if (!bold_depth) res ~= pre~"\x02";
  }, "<u>": (string pre, ref string post) {
    if (!ul_depth) res ~= pre~"\x1F";
    ul_depth++;
  }, "</u>": (string pre, ref string post) {
    ul_depth--;
    if (!ul_depth) res ~= pre~"\x1F";
  }, "<i>": (string pre, ref string post) {
    if (!inv_depth) res ~= pre~"\x16";
    inv_depth++;
  }, "</i>": (string pre, ref string post) {
    inv_depth--;
    if (!inv_depth) res ~= pre~"\x16";
  }, "<br>": (string pre, ref string post) {
    res ~= pre~"\n";
    // recreate formatting
    if (bold_depth) res ~= "\x02";
    if (ul_depth) res ~= "\x1F";
    if (inv_depth) res ~= "\x16";
  }];
  auto base = Format(t);
  auto nfpos = base.find("<noformat/>");
  if (nfpos != -1) return IRCFormat(base[0 .. nfpos]) ~ base[nfpos + 8+3 .. $];
  base.glomp_parse(commands, (string rest) { res ~= rest; });
  return res;
}

struct Query {
  IRCconn connection;
  nickname name; string channel;
  string param;
  int depth; // recursion safeguard
  bool inMiddle; // if query was in middle of line
  void privtell(T...)(T t) {
    auto copy = *this;
    copy.channel = name; // send privmsg
    copy.say(t);
  }
  void notice(T...)(T t) {
    connection.raw_sendln(IRCFormat("NOTICE ", name, " :", t));
  }
  void answer(T...)(T t) { say(cast(string) name, ": ", t); }
  void say(T...)(T t) {
    auto msg = Format(t);
    string pre_line;
    void gotLine(string s) {
      if (pre_line.length) pre_line ~= "<br>";
      pre_line ~= s;
    }
    const limit = 300;
    void breakIt(string msg) {
      int brk = 0;
      if (msg.length > limit) {
        brk = limit;
        while (brk && msg[brk] != '|') brk--;
        if (!brk) {
          // brk=limit;
          // while (brk < msg.length && msg[brk] != '|') brk++;
          // if (brk !< msg.length) {
            brk = limit;
            while (brk && msg[brk] != ' ') brk--;
            if (!brk) {
              brk=limit;
              while (brk < msg.length && msg[brk] != ' ') brk++;
              if (brk !< msg.length) brk = limit; // sigh. giving up.
            }
          // }
        }
      }
      if (brk) {
        gotLine(msg[0 .. brk]);
        breakIt(msg[brk+1 .. $].strip());
      } else gotLine(msg);
    }
    auto nbp = msg.find("<nobreak/>");
    if (nbp != -1) pre_line = msg.replace("<nobreak/>", "");
    else breakIt(msg);
    auto lines = IRCFormat(pre_line).split("\n");
    foreach (line; lines)
      connection.message(channel, line);
  }
  void act(T...)(T t) { connection.message(channel, "\x01"~Format("ACTION ", t).IRCFormat()~"\x01"); }
}

import tools.threads, tools.threadpool;
Threadpool tp;
void delegate(Query query) [string] commands;
bool[string] anypos;
TLS!(nickname) current_nick;

static this() {
  New(tp, 16);
  New(current_nick, { return &(new Stuple!(nickname))._0; });
}

import pad.engine: pastepost;
void check_ex(void delegate() dg, Query q) {
  try { dg(); }
  catch (Exception ex) {
    q.answer(Format(ex.classinfo.name, ": ", ex));
    if (fpe_trace) {
      auto ploc = fpe_trace.pastepost();
      // q.answer("Backtrace: ", ploc);
      fpe_trace = null;
    }
  }
}

extern(C) void function(int) signal(int signum, void function(int) handler);

string fpe_trace;

extern(C) void fpe_ex(int i) {
  fpe_trace = "/proc/self/stack".read().castLike("");
  throw new Exception("If you do that again, the bot will die. ");
}

const SIGFPE=8, SIG_IGN = 1;
void setupSignals() { /* fuck you posix */
  // signal(SIGFPE, &fpe_ex);
  signal(SIGFPE, &fpe_ex);
}
static this() { setupSignals(); }

void runQuery(Query query) {
  (*current_nick()) = query.name;
  scope(exit) (*current_nick()) = null;
  foreach (command, dg; commands) {
    if (auto param = query.param.startsWith(command)) {
      logln("Match for ", command);
      query.param = param.strip();
      check_ex(dg /fix/ query, query);
      return;
    }
  }
  // check_ex(commands["eval"] /fix/ query, query);
}

string getToken(string text) {
  string res;
  foreach (ch; text) {
    if (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || /*res.length && */ch >= '0' && ch <= '9' || ch == '_') res ~= ch;
    else return res;
  }
  return res;
}

import tools.mersenne;
Mersenne maingen;
static this() {
  New(maingen, 23);
  /*maingen.seed_hotbits();*/
  maingen.seed(µsec());
}
