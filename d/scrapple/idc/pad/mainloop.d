module pad.mainloop;

import pad.engine, pad.utils, tools.log, tools.compat: string;
static import tools.compat;

class PadException : Exception { this(string s) { super(s); } }

import tools.base, tools.downloader;

extern(C) {
  enum BZ {
    Run, Flush, Finish,
    OK = 0, RunOK, FlushOK, FinishOK, StreamEnd,
    ConfigError = -9, OutbufFull, UnexpectedEOF, IOError, DataErrorMagic, DataError,
    MemError, ParamError, SequenceError
  }
  BZ BZ2_bzBuffToBuffCompress(ubyte *dest, uint *destLen,
    ubyte *src, uint srclen,
    int bs100k, int verbose = 0, int workFactor = 0
  );
  BZ BZ2_bzBuffToBuffDecompress(ubyte *dest, uint *destlen,
    ubyte *src, uint srclen,
    bool small, int verbose = 0
  );
}

ubyte[] pad_compress(string s) {
  if (!s.length) return null;
  auto res = new ubyte[cast(int) (s.length * 1.02 + 604)];
  (cast(int*) res.ptr)[0] = cast(int) s.length;
  uint reslen = res.length - 4;
  if (auto res = BZ2_bzBuffToBuffCompress(res.ptr + 4, &reslen, cast(ubyte*) s.ptr, cast(uint) s.length, 9)) {
    throw new Exception(Format("BZ2 error: ", res));
  }
  return res[0 .. reslen + 4];
}

string pad_decompress(ubyte[] s) {
  if (s.length < 4) return null;
  auto len = (cast(int*) s.ptr)[0];
  s = s[4 .. $];
  auto res = new ubyte[len];
  uint reslen = res.length;
  if (auto res = BZ2_bzBuffToBuffDecompress(res.ptr, &reslen, s.ptr, cast(uint) s.length, false)) {
    throw new Exception(Format("BZ2 error: ", res));
  }
  return cast(string) res;
}

class SaveStream {
  string databuffer;
  bool empty = true;
  void addLine(string s) {
    if (empty) empty = false;
    else databuffer ~= '\x00';
    databuffer ~= s;
  }
  ubyte[] getData() {
    return cast(ubyte[]) pad_compress(databuffer);
  }
}

string[] loadDecompress(ubyte[] data) {
  logln("Decompress ", data.length);
  return pad_decompress(data).castLike("").split("\x00");
}

void runLoop(string src, void delegate(string) write, string delegate() read,
  bool readFS = false, bool dontSave = false, void delegate() stopstart = null)
{
  return runLoop(src, "", write, read, readFS, dontSave, stopstart);
}

const bool ReadFS = true, DontSave = true;
void runLoop(string src, string initial, void delegate(string) write, string delegate() read,
  bool readFS = false, bool dontSave = false, void delegate() stopstart = null)
{
  string data;
  if (src.length) data = readSrc(src, readFS).filterComments();
  Scope root;
  SaveStream stream;
  Scope location;
  void reset(bool verbose, bool full = true) {
    auto s2 = data.dup;
    if (s2.length) {
      (*stillParsing.ptr()) = true;
      scope(exit) (*stillParsing.ptr()) = false;
      Scope.getThis(s2, root);
    }
    if (full || !stream) stream = new SaveStream;
    if (!root) return;
    Text initial;
    location = root.eval(initial, root);
    if (!location) {
      throw new PadException("Evaluating root didn't generate a redirect! ");
    }
    if (verbose) write(initial.collate);
  }
  
  void process(string line) {
    if (!line.length) return;
    
    stream.addLine(line);
    string cmd;
    if(!location) {
      write("No world loaded! ");
      return;
    }
    while (line) {
      auto chunk = line.slice(";");
      auto parts = chunk.split(" ");
      if (parts.length == 1) cmd = parts[0];
      else cmd = join(parts[1 .. $], ".")~"."~parts[0];
      bool quiet;
      
      auto sc = location.lookup(cmd, User);
      if (sc._0) {
        Text output;
        scope(success) write(output.collate);
        if (auto newpos = sc._0.eval(output, sc._1?location:null))
          location = newpos;
      } else write("I don't know what that means. ");
    }
  }
  void loadLines(string[] lines) {
    reset(false);
    auto backup_read = read, backup_write = write;
    write = (string s) { };
    read = { return lines.take(); };
    while (lines.length) {
      try process(read());
      catch (Exception ex) { }
    }
    write = backup_write;
    read = backup_read;
  }
  if (initial.strip().length) {
    loadLines([initial]);
    write("Forwarded. ");
  } else reset(true);
  string lastLoaded;
  while (true) {
    auto line = read();
    if (line == "quit" || line == "q") return write("Bye! ");
    if (line == "forcequit") return;
    if (root && !dontSave) {
      if (line == "dumpgraph") {
        write("Dumped to "~root.getInfo().post());
        continue;
      }
      if (line == "dag") {
        try {
          .write("temp.dot", g_AGList().toDot());
          system("dot -Tpng -o areas.png < temp.dot");
          write("Area written! ");
        } catch (Exception ex) write(ex.toString());
        continue;
      }
      if (line == "save") {
        auto predata = stream.getData();
        auto postdata = base64_encode(predata);
        auto url = pastepost(postdata);
        write(Format("Saved ", postdata.length, " (", predata.length, ") to ", url));
        continue;
      }
      void load(ubyte[] compressed) {
        loadLines(compressed.loadDecompress());
      }
      if (auto from = line.startsWith("load ")) {
        (cast(ubyte[]) base64_decode(readSrc(from, false))).load();
        write("Loaded. ");
        continue;
      }
      if (auto from = line.startsWith("SYS_retry")) {
        from = from.strip();
        if (!from.length) from = src;
        auto backup = stream.getData();
        write("Reacquiring source .. ");
        data = readSrc(from, readFS).filterComments(); // re-read source also
        reset(false);
        load(backup);
        write("Forwarded! ");
      }
    }
    if (line == "reopen" && lastLoaded) {
      if (stopstart) stopstart();
      scope(exit) if (stopstart) stopstart();
      data = readSrc(lastLoaded, readFS).filterComments();
      write("World resetting");
      reset(true, false);
      continue;
    }
    if (auto from = line.startsWith("open ")) {
      if (from.find(" ") != -1) throw new Exception("Invalid URL: \""~from~"\". "); // No POST hacking! Bad user!
      if (stopstart) stopstart();
      scope(exit) if (stopstart) stopstart();
      lastLoaded = from;
      data = readSrc(from, readFS).filterComments(); // re-read source
      write("World loading .. ");
      reset(true, false);
      continue;
    }
    try process(line);
    catch (Exception ex) { write("[Error] "~ex.toString()); }
  }
}
