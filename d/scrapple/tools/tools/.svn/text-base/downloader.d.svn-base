module tools.downloader;

const bool Delete = false;

import tools.base, tools.fixed_socket, tools.compat;

string agent_override;

string htmlEscape(string s) {
  //\todo: complete this
  return s.replace(" ", "%20").replace("\"", "%22");
}

string getFilename(string url) {
  auto pos1 = url.rfind("/");
  if (pos1 == -1) return "index.html";
  url = url[pos1+1 .. $];
  auto pos2 = url.find("?");
  if (pos2 != -1) url = url[0 .. pos2];
  if (url.length) return url;
  else return "index.html";
}

bool validAddress(string foob) {
  auto tld=foob.split(".")[$-1];
  ///\todo: check against list of valid TLDs.
  if (tld.ifind("htm") != -1) return false;
  if (!tld.length) return false;
  if (tld.find("%") != -1) return false;
  return true;
}

string CopyAppend(ref string s, string t) {
  if (!s) t = t.dup;
  auto backup = s;
  s ~= t;
  static if (Delete) if (s.ptr != backup.ptr)
    delete backup;
  return s;
}

string toSize(int i) {
  // example result: 24.3K
  string sizeformat(string suffix, ulong max, ulong i) {
    auto r = Format(cast(int) ((i * 10f) / max));
    r = r[0 .. $-1] ~ "."~r[$-1]; // 243 -> 24.3
    return r~suffix;
  }
  ulong[string] map = ["T": 1_000_000_000_000UL, "G": 1_000_000_000UL, "M": 1_000_000UL, "K": 1_000UL];
  foreach (suffix; ["T", "G", "M", "K"]) {
    if (i > map[suffix]) 
      return sizeformat(suffix, map[suffix], i);
  }
  return Format(i, "B");
}

import tools.tests;
unittest {
  mustEqual("toSizeTest", toSize(123456), "123.4K");
}

void filterLink(ref string addr, ref string prepend) {
  while (addr.length && addr[$-1]==' ') addr=addr[0..$-1];
  string post, refer, cookie, range;
  bool old_html, head;
  while (true) {
    if (addr.startsWith("POST=")) { post=addr[0..addr.find(" ")+1]; addr=addr[addr.find(" ")+1..$]; continue; }
    if (addr.startsWith("REFER=")) { refer=addr[0..addr.find(" ")+1]; addr=addr[addr.find(" ")+1..$]; continue; }
    if (addr.startsWith("COOKIE=")) { cookie=addr[0..addr.find(" ")+1]; addr=addr[addr.find(" ")+1..$]; continue; }
    if (addr.startsWith("RANGE=")) { range = addr[0..addr.find(" ")+1]; addr = addr[addr.find(" ")+1..$]; continue; }
    if (auto rest = addr.startsWith("HEAD ")) { head = true; addr = rest; continue; }
    if (addr.startsWith("1.0 ")) { old_html=true; addr=addr[addr.find(" ")+1..$]; }
    break;
  }
  
  addr=addr.replace("&amp;", "&").replace(" ", "%20");
  
  prepend = post~refer~cookie~range~(head?"HEAD ":"")~(old_html?"1.0 ":"");
}

bool quiet = false;

char[] followLink(char[] oldAddr, char[] newAddr) {
  if (!newAddr) return null;
  string new_prepend; newAddr.filterLink(new_prepend);
  string old_prepend; oldAddr.filterLink(old_prepend);
  string[string] tags;
  foreach (entry; old_prepend.split(" ")~new_prepend.split(" ")) {
    if (!entry.length) continue;
    auto sp = entry.split("=");
    tags[sp[0]] = (sp.length>1)?(sp[1..$].join("=")):"";
  }
  if ("POST" in tags) tags.remove("POST"); // does not hold over links
  string prepend;
  foreach (key, value; tags) if (value.length) prepend ~= key~"="~value~" "; else prepend ~= key~" ";
  
  if (auto rest = oldAddr.startsWith("http://")) oldAddr = rest;
  if (newAddr.startsWith("data://")) return new_prepend~newAddr;
  if (auto rest = newAddr.startsWith("http://")) return prepend~"http://"~rest;
  if (auto rest = newAddr.startsWith("https://")) return prepend~"https://"~rest;
  if (!newAddr.length) throw new Exception("Empty newAddr found! Aborting followLink.");
  if (newAddr[0]=='/') return prepend~"http://"~oldAddr.split("/")[0]~newAddr;
  /// simplified query link
  if (newAddr[0]=='?') return prepend~"http://"~oldAddr.split("?")[0]~newAddr;
  while (newAddr.length>3 && newAddr[0..3]=="../") {
    if (oldAddr.split("/").length==2) {
      if (!quiet) logln("Link error - cannot find parent directory of |"~oldAddr~"|. Ignoring ..");
    } else {
      oldAddr=oldAddr[0..oldAddr.rfind("/")];
      oldAddr=oldAddr[0..oldAddr.rfind("/")+1];
    }
    newAddr=newAddr[3..$];
  }
  while (newAddr.length>2 && newAddr[0..2]=="./") newAddr=newAddr[2..$];
  /// If the new URL is not absolute ..
  if (newAddr.find("/")==-1 || newAddr.split("/")[0].find(".")==-1 || !newAddr.split("/")[0].validAddress()) {
    string oldAddrCut = oldAddr;
    if (oldAddrCut.split("/").length > 1) oldAddrCut = oldAddrCut.split("/")[0 .. $-1].join("/");
    /// add it to the end of the old URL.
    return prepend~"http://"~oldAddrCut~"/"~newAddr;
  }
  /// Otherwise presume it's an absolute URL
  return prepend~"http://"~newAddr;
}

public import tools.time: sleep;

class cbs {
  void delegate(int, int)[][string] entries;
  void delegate(int, int)[Stuple!(string, void delegate(float))] redirect;
  void doCbs(string url, int i, int k) { synchronized(this) {
    if (url in entries) {
      foreach (dg; entries[url]) dg(i, k);
    }
  }}
  void remove(string url, void delegate(int, int) rem) { synchronized(this) {
    if (!(url in entries)) return;
    if (entries[url].length==1) { entries.remove(url); return; }
    size_t pos=size_t.max; foreach (i, dg; entries[url]) if (dg is rem) pos=i;
    if (pos==size_t.max) fail;
    if (pos!=entries[url].length-1) entries[url][pos]=entries[url][$-1];
    entries[url].length=entries[url].length-1;
  }}
  void remove(string url, void delegate(float) rem) { synchronized(this) {
    auto p = stuple(url, rem) in redirect;
    if (!p) return;
    redirect.remove(stuple(url, rem));
    remove(url, *p);
  }}
  void add(string url, void delegate(float) n) { synchronized(this) {
    entries[url]~= redirect[stuple(url, n)] = n /apply/ (typeof(n) n, int at, int max) { n((at * 1f) / max); };
  }}
  void add(string url, void delegate(int, int) n) { synchronized(this) {
    entries[url]~=n;
  }}
}

cbs download_callbacks;
static this() { New(download_callbacks); synchronized(download_callbacks) { } }

string download(string param,
                     string *redirect=null, int maxredir = 16,
                     void delegate(string) cookie_dg = null, string[] visited = null, void delegate(int, string) gotChunk = null) {
  if (!maxredir) throw new Exception("Maximal redirection level reached for "~param~"! Aborting.");
  string url=param;
  foreach (s; visited) if (s == url) throw new Exception(Format("Circular redirect for ", url, " in ", s, ". Aborting."));
  if (url=="") throw new Exception("URL empty");
  if (url.beginsWith("data://")) return "<data>"~url[7 .. $];
  string post, refer, cookie, range; bool http_1_0, head;
  while (true) {
    if (auto rest=url.startsWith("POST=")) {
      url=rest;
      post=url[0..url.find(" ")];
      url=url[url.find(" ")+1..$];
      // if (!quiet) logln("Posting ", post);
      continue;
    }
    if (auto rest=url.startsWith("REFER=")) {
      refer=rest[0..rest.find(" ")];
      url=rest[rest.find(" ")+1..$];
      if (refer.length<7 || refer[0..7]!="http://") refer="http://"~refer; // referrer needs that
      // if (!quiet) logln("Refer ", refer);
      continue;
    }
    if (auto rest=url.startsWith("COOKIE=")) {
      cookie=rest[0..rest.find(" ")];
      url=rest[rest.find(" ")+1..$];
      // if (!quiet) logln("Cookie ", cookie);
      continue;
    }
    if (auto rest=url.startsWith("RANGE=")) {
      range=rest[0..rest.find(" ")];
      url=rest[rest.find(" ")+1..$];
      // if (!quiet) logln("Range: ", range);
      continue;
    }
    if (auto rest=url.startsWith("HEAD ")) {
      head = true;  url=rest;
      // if (!quiet) logln("HEAD");
      continue;
    }
    if (auto rest=url.startsWith("1.0 ")) {
      url=rest;
      // if (!quiet) logln("Fallback to HTTP 1.0");
      http_1_0=true;
      continue;
    }
    break;
  }
  if (auto rest=url.startsWith("https://")) throw new Exception("SSL/TLS is not supported. Cannot follow link: "~url);
  if (auto rest=url.startsWith("http://")) url=rest;
  if (find(url, "/")==-1) url~="/";
  auto address=split(url, "/")[0];
  if (!address.length) throw new Exception("No address found in "~param);
  // check for auth embedded in URL
  string basicauth;
  if (find(address, "@")!=-1) {
         basicauth = base64_encode(cast(ubyte[])split(address,"@")[0]);
         address = split(address,"@")[1];
  }
  ushort port=80;
  if (find(address, ":")!=-1) {
    port=cast(ushort)atoi(split(address, ":")[1]);
    address=split(address, ":")[0];
  }
  url=url.replace(" ", "%20");
  auto folder="/"~join(split(url, "/")[1..$], "/");

  static TcpSocket[][string] cpool;
  static Object cpoolSync; if (!cpoolSync) cpoolSync=new Object;
  TcpSocket openSocket(string firstMesg) {
    TcpSocket sock;
    synchronized (cpoolSync) {
      if (address in cpool) {
        foreach (potentialSocket; cpool[address]) {
          auto res=potentialSocket.send(firstMesg);
          if (res>0) {
            //logln("Succeeded in finding a viable socket in the threadpool: ", res);
            sock=potentialSocket;
            break;
          }
        }
        if (sock) {
          size_t pos=0;
          while (cpool[address][pos] !is sock) ++pos;
          if (pos<cpool[address].length-1) cpool[address]=cpool[address][0..pos]~cpool[address][pos+1..$];
          cpool[address]=cpool[address][0..$-1];
          if (!cpool[address].length) cpool.remove(address);
        }
      }
    }
    if (!sock) {
      sock=new TcpSocket;
      scope(failure) sock.close;
      sock.blocking=true;
      size_t tries=3;
      while (true) {
        retry: try { sock.connect(new ActuallyWorkingInternetAddress(address, port)); }
        catch (Exception s) {
          version(Win32) { } else version(Tango) { } else {
            auto sex=cast(SocketException)s;
            if (sex&&sex.errorCode==4) goto retry; /// silent retry in case 4==EINTR
          }
          if (!quiet) logln("Socket exception ", s?s.toString:"[INVALID EXCEPTION]", " occured; tries ", tries, " while connecting to ", address, ":", port);
          if (tries--) { if (!quiet) logln("Waiting 1s before retry"); sleep(1); continue; }
          else throw new Exception("Cannot connect to target");
        }
        break; /// if we got here, exit the loop
      }
      sock.send(firstMesg);
    }
    return sock;
  }
  TcpSocket sock;
  int retries=3;
retry:
  if (!retries) throw new Exception("Can't download page: "~param);
  retries--;
  if (post.length) sock=openSocket("POST "~folder~" HTTP/"~(http_1_0?"1.0":"1.1")~"\r\n");
  else if (head) sock=openSocket("HEAD "~folder~" HTTP/"~(http_1_0?"1.0":"1.1")~"\r\n");
  else sock=openSocket("GET "~folder~" HTTP/"~(http_1_0?"1.0":"1.1")~"\r\n"); 
  scope(failure) sock.close;
  sock.send("Connection: keep-alive\r\n");
  if (agent_override)
    sock.send("User-Agent: "~agent_override~"\r\n");
  else
    sock.send("User-Agent: Mozilla/5.0 (D/tools downloader)\r\n");
  sock.send("Accept: */*\r\n");
  // sock.send("Accept-Encoding: deflate;q=1.0, identity;q=1.0, *;q=0\r\n");
  sock.send("Host: "~address~"\r\n");
  if (basicauth.length)
    sock.send("Authorization: Basic "~basicauth~"\r\n");
  if (post.length)
    sock.send(Format("Content-Type: application/x-www-form-urlencoded\r\nContent-Length: ", post.length, "\r\n"));
  if (refer.length)
    sock.send("Referer: "~refer~"\r\n");
  if (cookie.length)
    sock.send("Cookie: "~cookie.replace(";", "; ")~"\r\n");
  if (range.length)
    sock.send("Range: "~range~"\r\n");
  string cookiebuf;
  
  if (range.length)
    sock.send("Range: "~range~"\r\n");
  sock.send("\r\n"~post);
  string received;
  int length=int.max;
  string header;
  bool gzip, deflate;
  void got(string data) {
    if (gotChunk) gotChunk(gzip?1:(deflate?2:0), data);
  }
  bool checkHeader(ref bool chunked, out string res) {
    auto hpos = cast(size_t)find(received, "\r\n\r\n");
    if (hpos == -1) return false;
    hpos += 4;
    header = received[0..hpos].dup;
    auto backup = received;
    received = received[header.length .. $].dup;
    static if (Delete) delete backup;
    string[string] headmap;
    foreach (line; header.split("\n")) {
      line = line.chomp();
      auto key = line.slice(":").tolower();
      line = line.strip();
      if (auto p = key in headmap) (*p) ~= "\x00"~line;
      else headmap[key] = line;
    }
    if ("content-length" /notin/ headmap) {
      auto p = "transfer-encoding" in headmap;
      if (p && (*p).strip() == "chunked") {
        if (!quiet) logln("Recognize chunked mode");
        chunked=true;
      }
      length = 0;
    } else {
      length = headmap["content-length"].atoi();
    }
    string location_redirect;
    if (auto p = "content-encoding" in headmap) {
      if (*p == "deflate") deflate = true;
      if (*p == "gzip") gzip = true;
    }
    if (auto p = "location" in headmap) {
      location_redirect = *p;
    }
    if (auto p = "set-cookie" in headmap) {
      foreach (part; (*p).split("\x00")) {
        if (cookie_dg)
          cookie_dg(part);
        if (cookiebuf.length)
          cookiebuf ~= ";";
        cookiebuf = part.between("", "; ").strip();
      }
    }
    if (location_redirect) {
      if (cookiebuf.length)
        location_redirect = "COOKIE="~cookiebuf~" "~location_redirect;
      string loc = param.followLink(location_redirect);
      if (!quiet) logln("Following redirect from ", param, " to ", loc);
      if (redirect) { *redirect=loc; res=null; return true; }
      if (loc in download_callbacks.entries) throw new Exception("Web site at "~param~" tried to set up a circular redirect! ");
      auto dg = (int at, int max) { download_callbacks.doCbs(param, at, max); };
      download_callbacks.add(loc, dg);
      scope(exit) download_callbacks.remove(loc, dg);
      res = download(loc, redirect, maxredir-1, cookie_dg, visited~url); return true;
    }
    return false;
  }
  bool checkProgress(ref bool chunked, out string res) {
    if (length==int.max) { if (checkHeader(chunked, res)) return true; }
    else {
      // At this point we know the header has already been received
      if (length) {
        download_callbacks.doCbs(param, received.length, length);
        if (!chunked) got(received); // chunks are got directly
      }
    }
    return false;
  }
  string chunks;
  // finalize: move data into chunks even if incomplete
  bool receiveChunked(bool finalize=false) {
    // try to grab the first line of the remaining source
    auto line_end=received.find("\r\n");
    if (line_end==-1) return true;
    ulong chars;
    {
      auto line = received[0..line_end].strip();
      if (line.find(";")!=-1) line=line[0..line.find(";")];
      ulong hex2ulong(string hex) {
        ulong res;
        ubyte digit=0;
        foreach (ch; hex.tolower()) {
          if (ch>='a' && ch<='f') digit=cast(ubyte) (ch+10-'a'); else
          if (ch>='0' && ch<='9') digit=cast(ubyte) (ch-'0'); else
          throw new Exception("Invalid character '"~ch~"' in hex string");
          res=res*16+digit;
        }
        return res;
      }
      chars=hex2ulong(line);
    }
    if (chars) {
      if (received.length < line_end+2) return true;
      if (received.length < chars+line_end+4) {
        if (finalize) {
          CopyAppend(chunks, received[line_end+2..$]);
          got(chunks);
        }
        return true;
      }
      received = received[line_end+2 .. $];
      CopyAppend(chunks, received[0..cast(uint) chars]);
      got(chunks);
      received = received[cast(uint) chars+2 .. $];
      return receiveChunked;
    } else {
      if (received[line_end..$].find("\r\n\r\n")==-1) return true;
      return false; // got everything and trailing newline
    }
  }
  int len=0;
  bool reuseConnection=true;
  bool chunked=false;
  auto buffer=new char[1024];
  while (true) {
    len=sock.receive(buffer);
    if (len<0) {
      version(Windows) { }
      else { if (getErrno==4) continue; } // EINTR (interrupted);
      goto retry;
    }
    if (len==0) { reuseConnection=false; if (!quiet) logln("Connection closed at ", received.length); break; }
    CopyAppend(received, buffer[0..len]);
    
    string _res; if (checkProgress(chunked, _res)) return _res;
    if (chunked) if (receiveChunked) continue; else break;
    if (length && (received.length !< length)) { if (!quiet) logln("Received all data"); break; }
  }
  if (!header.length) {
    if (!quiet) logln("Connection closed, but no body found. Presuming invalid persistant connection; retrying ... ");
    goto retry;
  }
  foreach (line; split(header, "\r\n")) {
    if (line.length!<12 && line[0..12]=="Connection: ") {
      //logln("Server signalled connection mode '", line, "'");
      if (line.ifind("close")!=-1) reuseConnection=false;
    }
  }
  if (reuseConnection) synchronized (cpoolSync) if (address in cpool) cpool[address]~=sock; else cpool[address]=[sock];
  if (chunked) {
    if (!chunks.length) {
      if (retries) goto retry;
      receiveChunked(true); // favor partial download over no download
      if (!chunks.length) goto retry; // with intent to fail
    }
    // return chunks;
    static if (Delete) delete received;
    received = chunks;
  }
  if (!received.length) goto retry;
  auto res=received;
  if (deflate || gzip) {
    auto newdata = cast(string) uncompress(cast(void[]) res, 0, 15 + (gzip ? 16 : 0));
    static if (Delete) delete res;
    res = newdata;
  }
  if (res.length>2 && (res[0..2]==x"feff" || res[0..2]==x"fffe")) return (cast(wchar *)res.ptr)[0..res.length/2].toUTF8();
  if (res.length>3 && res[0..3]==x"efbbbf") return res[3 .. $]; // UTF8 coded
  return res;
}

void streamload(string url, void delegate(string) gotChunk, string* redir = null) {
  bool first = true, wcharDecode;
  ZlibUncompress decomp;
  int gotten;
  download(url, redir, 16, null, null, (int compress, string st) {
    if (compress) {
      New(decomp, (compress==1)?31:15);
    }
    auto newdata = st[gotten .. $];
    gotten = st.length;
    st = newdata;
    if (decomp) st = cast(string) decomp.uncompress(cast(void[]) st);
    if (first) {
      if (st.startsWith(x"feff") || st.startsWith(x"fffe")) wcharDecode = true;
      first = false;
    }
    string output = st;
    if (wcharDecode) {
      if ((st.length % 2) == 1)
        output = (cast(wchar*) st.ptr)[0 .. st.length / 2].toUTF8();
      gotten = st.length & ~1;
    }
    gotChunk(output);
  });
}
