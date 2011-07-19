module tools.remake;

// kinda pointless on there
version(DS) { } else {
  import tools.base, std.file, std.process, std.stream, std.cstream, tools.compat, tools.log;
  
  struct FileInfo {
    long creat, acc, mod;
    static FileInfo opCall(string filename) {
      FileInfo res;
      filename.getTimes(res.creat, res.acc, res.mod);
      return res;
    }
  }
  
  string Compiler = "gdc", IncList = "-fd-verbose", ObjFolder = "obj/", Linker, Flags;
  string delegate(string) perFileFlags;
  bool verbose = true;
  
  extern(C) int pipe(int* pipefd);
  extern(C) int close(int fd);
  
  scope class Pipe {
    int fd_read, fd_write;
    this() {
      int[2] temp;
      auto res = pipe(temp.ptr);
      if (res == -1) throw new Exception("Could not create pipe!");
      fd_read = temp[0]; fd_write = temp[1];
    }
    ~this() { close(fd_read); close(fd_write); }
  }
  
  string systemCapture(string cmdline) {
    scope pipe = new Pipe;
    system(Format("(", cmdline, "; echo ==\\> $?) >&", pipe.fd_write, " &"));
    close(pipe.fd_write);
    scope resfile = new CFile(fdopen(pipe.fd_read, "r"), FileMode.In);
    ubyte[1024] buffer; string res;
    while (true) {
      auto rv = resfile.read(buffer);
      if (!rv) return res;
      res ~= (cast(string) buffer)[0 .. rv];
    }
  }
  
  string getObjFile(string sfile) {
    if (auto base = ObjFolder.sub(sfile.replace(sep, "_")).endsWith(".d"))
      return base~".o";
    else throw new Exception(sfile~" is not a D source file! ");
  }
  
  string compile(string file, int i, int max) {
    if (!ObjFolder.exists()) ObjFolder.mkdir();
    auto flags = Flags;
    if (perFileFlags)
      if (auto pfflags = perFileFlags(file)) flags ~= pfflags;
    if (verbose) writefln("[COMPILE ", i, " / ", max, "] ", file, " -> ", getObjFile(file));
    return Format(Compiler, " ", IncList, " ", file, " -c -o ", getObjFile(file), " ", flags).systemCapture();
  }
  
  string[] findIncludes(string sfile, ref bool wasChanged) {
    string[] res;
    string outp;
    wasChanged = checkObj(sfile, 1, 1, &outp);
    int result;
    foreach (line; outp.split("\n")) {
      if (auto res = line.startsWith("==> ")) result = atoi(res);
      if (auto rest = line.startsWith("import").strip()) {
        auto open = rest.find("("), close = rest.find(")");
        if (open == -1 || close == -1) continue;
        assert(close > open, "Malformed line "~line);
        res ~= rest[open+1 .. close];
      }
    }
    if (result) throw new Exception("Compilation failed! ");
    return res;
  }
  
  void relink(string target) {
    if (verbose) writefln("[LINK] ", ObjFolder.listdir());
    system(Format(Compiler, " ", ObjFolder, "*.o -o ", target, " ", Linker));
  }
  
  void clean() {
    system("rm -rf obj/");
  }
  
  bool checkObj(string source, int i, int max, string* outp = null) {
    auto obj = source.getObjFile();
    bool should_rebuild(string src, string obj) {
      if (!obj.exists()) return true;
      if (!src.exists()) throw new Exception("Could not find "~src~"! ");
      auto time_o = FileInfo(obj), time_s = FileInfo(src);
      return time_s.mod > time_o.mod;
    }
    if (outp) {
      auto res = should_rebuild(source, obj);
      *outp = compile(source, i, max);
      return res;
    } else {
      if (should_rebuild(source, obj)) {
        auto res = compile(source, i, max);
        auto lastline = res.split("\n")[$-2];
        if (auto num = lastline.startsWith("==> "))
          if (num.atoi()) throw new Exception("System error: command returned "~num~"! ");
        return true;
      } else return false;
    }
  }
  
  bool remade(string arg0, string main, bool delegate(string) exclude = null) {
    if (!exclude) exclude = (string s) {
      return s.find("..") != -1;
    };
    bool changed;
    auto inc = findIncludes(main, changed) /select/ (string s) { return !exclude(s); };
    foreach (i, file; inc) {
      if (checkObj(file, i+1, inc.length)) changed = true;
    }
    if (changed) relink(arg0);
    return changed;
  }
  
  extern(C) void _moduleDtor();
  
  void checkRemake(ref string[] args, string me) {
    bool doRebuild = true;
    if (args[$-1] == "--dont-rebuild") {
      doRebuild = false;
      args = args[0 .. $-1];
    }
    if (doRebuild && remade(args[0], me)) {
      if (verbose) writefln("[RES] Source changed, restarting ..");
      _moduleDtor();
      execv(args[0], args ~ "--dont-rebuild"[]);
    } else if (verbose) writefln("[RES] Source fresh! ");
  }
  
  // void main(string[] args) { checkRemake(args, "tools/remake.d"); }
}
