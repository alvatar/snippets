module tools.compat;

version(Tango) {
  private {
    import
      tango.io.compress.c.zlib, tango.stdc.stdlib;// , tango.util.encode.Base64;
  }
  import tango.net.device.Socket;
  import tools.base: Format;
  public {
    import tools.smart_import;
    alias tools.text.string string;
    version(Win32) { } else {
      import tango.stdc.posix.pthread, tango.stdc.posix.semaphore;
    }
    mixin(expandImport("tango.[core.[Traits, Thread, Memory, BitManip]
      , stdc.[stringz, tgmath, stdio]
      , text.[Util, Unicode, convert.Utf, Regex], math.[Math, IEEE]
      , io.[Path: exists, Console: Cin, Path: createFolder, device.File]
      , net.InternetAddress
    ]"));
    //   , util.[digest.Md5]
    alias createFolder mkdir;
    alias tango.stdc.stdlib.getenv getenv;
    alias tango.stdc.stdlib.system system;
    // alias tango.util.encode.Base64.encode base64_encode;
    // alias tango.util.encode.Base64.decode base64_decode;
    static import tango.core.Tuple;
    alias tango.core.Tuple.DerivedToFront DerivedToFront;
    void system(string s) { tango.stdc.stdlib.system(s.toStringz()); }
    void rename(string a, string b) { return tango.stdc.stdio.rename(toStringz(a), toStringz(b)); }
    string readln() { return Cin.copyln(); }
    void[] read(string name) {
      scope f = new File(name, File.ReadExisting), buffer = new char[cast(size_t) f.length];
      scope(exit) f.close; // just to be sure
      f.read(buffer);
      return buffer;
    }
    void write(string name, void[] data) {
      scope f = new File(name, File.WriteCreate);
      scope(exit) f.close;
      f.write(data);
    }
    alias RegExpT!(char) Regexp;
    void addRange(void* foo, size_t sz) { GC.addRange(foo, sz); }
    void removeRange(void* foo) { GC.removeRange(foo); }
    void gcdisable() { GC.disable; }
    void gcenable() { GC.enable; }
    void gccollect() { GC.collect; }
    alias tango.text.convert.Utf.toString toUTF8;
    string toString(char* c) {
      if (!c) return null;
      int length;
      while (c[length]) length++;
      return c[0 .. length];
    }
    extern(C) int getErrno();
    const EINTR = 4; // the more you know
    alias tango.text.Unicode.toLower tolower;
    int find(string text, string what) {
      if (what.length > text.length) return -1;
      for (int i = 0; i <= text.length - what.length; ++i) {
        if (text[i .. i+what.length] == what) return i;
      }
      return -1;
    }
    int find(string text, char what) { return text.find((&what)[0 .. 1]); }
    int rfind(string text, string what) {
      auto i = text.locatePatternPrior(what, text.length);
      if (i == text.length) return -1;
      else return i;
    }
    int ifind(string text, string what) { return text.tolower().find(what.tolower()); }
    template ParameterTypeTuple(T) { alias ParameterTupleOf!(T) ParameterTypeTuple; }
    template ReturnType(T) { alias ReturnTypeOf!(T) ReturnType; }
    long atoi(string s) {
      if (!s.length) return 0;
      if (s[$-1] != '\x00') s ~= '\x00';
      return tango.stdc.stdlib.atoi(s.ptr);
    }
    string chomp(string s) {
      while (true) {
        if (s.length >= 2 && s[$-2 .. $] == "\r\n") { s = s[0 .. $-2]; continue; }
        if (s.length >= 1 && s[$-1 .. $] == "\n") { s = s[0 .. $-1]; continue; }
        return s;
      }
    }
    double atof(string s) {
      if (!s.length) return double.nan;
      if (s[$-1] != '\x00') s ~= '\x00';
      return tango.stdc.stdlib.atof(s.ptr);
    }
    class TcpSocket : Socket {
      this() { super(AddressFamily.INET, SocketType.STREAM, ProtocolType.TCP); }
      this(InternetAddress iaddr) {
        this();
        connect(iaddr);
      }
      int send(void[] buf) { return super.write(buf); }
      int receive(void[] buf) { return super.read(buf); }
      bool blocking() { return native().blocking(); }
      bool blocking(bool b) { native().blocking(b); return b; }
    }
    static if (is(Md5))
      void sum(ubyte[16] digest, void[] data) {
        auto h = new Md5;
        h.update(data);
        h.binaryDigest(digest);
      }
    abstract class Stream {
      int read(void[] buf) { assert(false); }
      int write(void[] buf) { assert(false); }
      void writeExact(void* ptr, size_t len) {
        auto foo = ptr[0 .. len];
        while (foo.length) {
          auto res = write(foo);
          if (res <= 0) throw new Exception("WTF? ");
          foo = foo[res .. $];
        }
      }
      void readExact(void* ptr, size_t len) {
        auto foo = ptr[0 .. len];
        while (foo.length) {
          auto res = read(foo);
          if (res <= 0) throw new Exception("WTF? ");
          foo = foo[res .. $];
        }
      }
      void writefln(T...)(T t) { writeLine(tools.text.Format(t)); }
      void writeLine(string s) {
        writeExact(s.ptr, s.length); writeExact("\n".ptr, 1);
      }
      string readLine() {
        char[1] bogus;
        string res;
        readExact(bogus.ptr, bogus.length);
        while (bogus != "\n") {
          res ~= bogus;
          readExact(bogus.ptr, bogus.length);
        }
        return res;
      }
    }
    static if (!is(FileMode)) {
      enum FileMode { Read, Write }
    }
    class CFile : Stream {
      FILE* file;
      this(FILE* foo, FileMode fm) { file = foo; }
      override int read(void[] buf) {
        return fread(buf.ptr, 1, buf.length, file);
      }
      override int write(void[] buf) {
        return fwrite(buf.ptr, 1, buf.length, file);
      }
    }
    class SocketStream : Stream {
      Socket sock;
      this(Socket sock) { this.sock = sock; }
      void flush() { sock.flush; }
      import tools.text: Format;
      void close() { sock.close; }
      override int read(void[] buf) {
        auto res = sock.read(buf);
        if (res == -1) return 0;
        return res;
      }
      override int write(void[] buf) {
        return sock.write(buf);
      }
    }
    string replace(string text, string what, string repl) {
      while (true) {
        auto pos = text.find(what);
        if (pos == -1) return text;
        text = text[0 .. pos] ~ repl ~ text[pos+what.length .. $];
      }
    }
    string regex_sub(string text, string pattern, string format, string attribs = null) {
      auto re = new Regexp(pattern, attribs);
      return re.replaceAll(text, format);
    }
    alias tango.stdc.stdlib.malloc cmalloc;
    alias tango.stdc.stdlib.realloc crealloc;
    alias tango.stdc.stdlib.free cfree;
    alias tango.stdc.stdlib.exit cexit;
    alias tango.text.Util.trim strip;
    alias tango.math.Math.abs abs;
    alias tango.math.Math.sqrt sqrt;
    alias tango.math.Math.ceil ceil;
    alias tango.math.Math.tan tan; alias tango.math.Math.sin sin; alias tango.math.Math.cos cos; alias tango.math.Math.exp exp;
    alias tango.math.Math.sinh sinh;
    alias tango.math.Math.atan2 atan2;
    alias tango.math.Math.acos acos;
    alias tango.math.Math.floor floor;
    alias tango.stdc.tgmath.pow pow;
    // alias tools.base.min min;
    // alias tools.base.max max;
    alias tango.math.Math.log log;
    alias tango.math.IEEE.frexp frexp;
    version(Windows) const string sep = "\\";
    else const string sep = "/";
    bool isSelf(Thread t) { return Thread.getThis() is t; }
  }
} else public {
  version(linux)
    version(GNU) import std.c.unix.unix: EINTR,
      sem_t, sem_init, sem_destroy, sem_wait, sem_post, sem_trywait,
      pthread_mutex_t, pthread_mutex_init, pthread_mutex_destroy, pthread_mutex_lock, pthread_mutex_unlock;
    else import std.c.linux.pthread, std.c.linux.linux;
  import std.traits, std.cstream;
  version(DS) { } else { import std.thread, std.math, std.file, std.path: sep; import std.c.stdlib: getErrno, getenv, system; }
  static import std.typetuple;
  alias std.typetuple.DerivedToFront DerivedToFront;
  import std.string, std.c.stdio, std.stdio, std.stream;
  import std.c.stdlib: cfree = free, cmalloc = malloc, crealloc = realloc, cexit = exit;
  version(DS) { } else {
    import std.gc: removeRange, addRange, gcdisable=disable, gcenable=enable, gccollect=fullCollect;
    import std.intrinsic;
  }
  import std.utf, std.stream, std.socketstream, std.socket, std.md5: sum;
  import tools.base;
  alias std.file.rename rename;
  version(DS) { } else { bool isSelf(Thread t) { return !!t.isSelf(); } }
  alias std.regexp.sub regex_sub;
  static import std.base64;
  string base64_encode(ubyte[] u) { return std.base64.encode(cast(string) u); }
  ubyte[] base64_decode(string s) { return cast(ubyte[]) std.base64.decode(s); }
}

version(DS) { } else {
  bool isnan(float f) { return f != f; }
  class ZlibUncompress {
    version(Tango) { } else {
      import etc.c.zlib;
    }
    z_stream zs;
    this(int ws = 15) {
      if (auto err = inflateInit2(&zs, ws))
        throw new ZlibException(err);
    }
    void[] buffer;
    void[] uncompress(void[] buf) {
      if (!buffer.length) buffer.length = 1024;
      
      zs.next_in = cast(ubyte*) buf.ptr;
      zs.avail_in = buf.length;
      
      zs.next_out = cast(ubyte*) buffer.ptr;
      zs.avail_out = buffer.length;
      
      while (true) {
        version(Tango) const Z_NO_FLUSH = Z.NoFlush;
        auto err = inflate(&zs, Z_NO_FLUSH);
        if (err == Z.StreamEnd) return buffer[0 .. $ - zs.avail_out];
        if (err) throw new ZlibException(err);
        
        if (zs.avail_in) { // need to make more space.
                           // "inflate() provides as much output as possible,
                           // until there is no more input data or no more
                           // space in the output buffer". As per spec.
          auto used = buffer.length - zs.avail_out;
          buffer.length = buffer.length * 2;
          zs.next_out = cast(ubyte*) (buffer.ptr + used);
          zs.avail_out = buffer.length - used;
        } else return buffer[0 .. $ - zs.avail_out];
      }
    }
  }
}

class ZlibException : Exception {
  this(int errnum) {
    char[] msg;
    switch (errnum) {
      case Z.StreamEnd:		msg = "stream end"; break;
      case Z.NeedDict:		msg = "need dict"; break;
      case Z.Errno:		msg = "errno"; break;
      case Z.StreamError:	msg = "stream error"; break;
      case Z.DataError:		msg = "data error"; break;
      case Z.MemError:		msg = "mem error"; break;
      case Z.BufError:		msg = "buf error"; break;
      case Z.VersionError:	msg = "version error"; break;
      default:			msg = "unknown error";	break;
    }
    super(msg);
  }
}

enum Z {
  DefaultCompression = -1,
  OK = 0, StreamEnd, NeedDict,
  VersionError = -6, BufError, MemError, DataError, StreamError, Errno,
  NoFlush = 0, PartialFlush, SyncFlush, FullFlush, Finish, Block
}

extern(C) {
  struct z_stream {
    ubyte*  next_in; size_t  avail_in,  total_in;
    ubyte* next_out; size_t avail_out, total_out;
    char* msg;
    void* state;
    /+ stuff goes here +/
    typeof(&cmalloc) zalloc;
    typeof(&cfree) zfree;
    void* opaque;
    size_t[3] stuff;
  }
  Z compress2(ubyte* dest, size_t* destlen, ubyte* src, size_t srclen, int level);
  Z deflateInit_(z_stream*, int level, char *ver, int stream_size);
  Z inflateInit2_(z_stream*, int windowBits, char* ver, int stream_size);
  Z inflate(z_stream*, Z flush);
  Z inflateEnd(z_stream*);
  Z deflate(z_stream*, Z flush);
  Z deflateEnd(z_stream*);
  size_t compressBound(size_t srclen);
  char* zlibVersion();
}

int inflateInit2(z_stream* p, int windowBits) {
  return inflateInit2_(p, windowBits, "1.2.3", z_stream.sizeof);
}

int deflateInit(z_stream* p, int level) {
  return deflateInit_(p, level, "1.2.3", z_stream.sizeof);
}

void[] compress(void[] srcbuf, int level = Z.DefaultCompression) {
  size_t destlen = compressBound(srcbuf.length);
  auto destbuf = new ubyte[destlen];
  if (auto err = compress2(destbuf.ptr, &destlen, cast(ubyte *)srcbuf, srcbuf.length, level)) {
    version(Delete) delete destbuf;
    throw new ZlibException(err);
  }
  destbuf.length = destlen;
  return destbuf;
}

void[] uncompress(void[] srcbuf, size_t destlen = 0u, int winbits = 15)
{
  int err;
  if (!destlen)
    destlen = srcbuf.length * 2 + 1;
  
  auto destbuf = new ubyte[destlen];
  
  int finished, lastSetLength;
  
  z_stream zs;
  
  void forward() {
    if (lastSetLength) finished += lastSetLength - zs.avail_out;
    
    auto curbuf = destbuf[finished .. $];
    
    zs.next_out = curbuf.ptr;
    zs.avail_out = lastSetLength = curbuf.length;
  }
  
  zs.next_in = cast(ubyte*) srcbuf;
  zs.avail_in = srcbuf.length;
  
  auto curbuf = destbuf[finished .. $];
  
  zs.next_out = curbuf.ptr;
  zs.avail_out = curbuf.length;
  
  err = inflateInit2(&zs, winbits);
  
  forward;
  
  if (err) {
    version(Delete) delete destbuf;
    throw new ZlibException(err);
  }

  while (true) {
    
    err = inflate(&zs, Z.NoFlush);
    
    switch (err) {
      case Z.OK:
        destbuf.length = destbuf.length * 2;
        forward;
        continue;
      case Z.StreamEnd:
        err = inflateEnd(&zs);
        if (err != Z.OK)
          goto Lerr;
        return destbuf[0 .. zs.total_out];
      default:
        inflateEnd(&zs);
      Lerr:
        version(Delete) delete destbuf;
        throw new ZlibException(err);
    }
  }
}
