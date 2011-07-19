module tools.ini;

import std.process: system;

/*
Example:
[section_name]
booleanvar=true
stringvar=whee lmao = " = okay
intvar=-5
*/

import tools.log, tools.serialize, tools.compat;
struct iniVal(T) {
  iniFile where;
  string name, section;
  T *val;
  static iniVal opCall(iniFile where, string section, string name, T deflt) {
    iniVal res;
    res.where=where; res.name=name; res.section=section;
    res.init;
    if (!res.val) { res.val=new T; *res.val=deflt; }
    return res;
  }
  void write() { synchronized(where) if (val) where.set(section, name, *val); }
  void init() { synchronized(where) if (!val && where.has(section, name)) { val=new T; *val=where.get(section, name, *val); } }
  int opEquals(T whut) { init; return *val==whut; }
  T opCall() { init; return *val; }
  T opAssign(T whee) {
    init;
    *val=whee;
    write;
    return whee;
  }
  static if (is(typeof(Init!(T) + 1) == T)) {
    T opPostInc() { return opAssign(opCall() + 1); }
    T opPostDec() { return opAssign(opCall() - 1); }
    T opAddAssign(T t) { return opAssign(opCall() + t); }
    T opSubAssign(T t) { return opAssign(opCall() - t); }
  }
}

string escape(string s) { return " " ~ s;}
string unescape(string s) { if (auto rest = s.startsWith(" ")) s = rest; return s; }

extern(C) char* tmpnam(char* buffer);

/*
  This class is built rather strongly on the assumption that your filesystem is buffered.
  Please don't run it on a floppy disk or similar (or expect heavy seeking)
*/

class iniFile {
  string name;
  string[] lines;
  this(string fn) {
    this.name=fn;
    // on error resume next
    // basically; if we can't read the file, that
    // probably just means the file didn't exist.
    // in that case, it will be created on write.
    // if there's a different error, we are fucked :)
    // thanks phobos, for the embarrassing
    // lack of FileNotFoundException
    try load; catch(Exception e) { }
  }
  private {
    void load() {
      auto _lines=(cast(string) .read(name)).split("\n");
      lines.length = 0;
      foreach (line; _lines)
        if (line && line.length)
          lines ~= line;
    }
    void save(int retries = 10) {
      string res;
      foreach (line; lines) res~=line~"\n";
      char[512] buffer;
      auto temp = .toString(tmpnam(buffer.ptr));
      .write(temp, res);
      try {
        version(Posix) .rename(temp, name);
        else version(Windows) {
          temp.copy(name);
          .unlink(temp.toStringz());
        }
        else static assert(false);
      } catch (Exception ex) {
        .unlink(temp.toStringz());
        if (retries) { slowyield(); save(retries-1); }
        else throw ex;
      }
      // .write(name, res);
    }
    size_t findEntry(string section, string label) {
      synchronized(this) {
        load();
        string cursec;
        foreach (pos, line; lines) {
          if (line.startsWith("#")) continue; // ignore comments
          if (line.length && line[0]=='[' && line.find("=")==-1) {
            if (line.find("]") == -1) throw new Exception("Invalid line: "~line);
            cursec=line[1..line.find("]")].strip();
            if (!label.length && cursec==section) return pos;
            continue;
          }
          if (cursec==section && line.find("=")!=-1) {
            string ll=line[0..line.find("=")].strip();
            if (ll==label) return pos;
          }
        }
        //throw new Exception("Label "~label~" not found"~(section.length?" in section "~section:"")~"!");
        return size_t.max;
      }
    }
  }
  string[string] section_map(string name) {
    string[string] res;
    foreach (line; section(name)) {
      auto key = line.slice("=").strip();
      res[key] = line;
    }
    return res;
  }
  string[] section(string name) {
    synchronized(this) {
      load();
      string[] res;
      bool found = false;
      foreach (line; lines) {
        if (found) {
          if (line.length && line[0] == '[') return res;
          else {
            if (line.startsWith("#")) continue;
            if (line.strip().length) res ~= line.unescape();
          }
        } else {
          if (line.length && line[0] == '[') {
            if (line[1 .. line.find("]")].strip() == name) found = true;
          }
        }
      }
      return res;
    }
  }
  void set(T)(string section, string label, T what) {
    synchronized(this) {
      load(); scope(exit) save();
      auto str = serialize(what);
      // logln("Serialized ", what, " to ", str);
      size_t pos=findEntry(section, label);
      label = label.escape();
      // entry doesn't exist yet
      if (pos == size_t.max) {
        pos=findEntry(section, "");
        // section doesn't exist yet
        if (pos == size_t.max) {
          if (section.length) {
            lines~=("["~section~"]");
            lines~=(label~" = "~str);
          } else {
            lines=(label~" = "~str)~lines;
          }
        } else lines=lines[0..pos+1]~(label~" = "~str)~lines[pos+1..$];
      } else {
        lines[pos]=label~" = "~str;
      }
    }
  }
  bool del(string section, string label) {
    if (!label.length) {
      foreach (lbl; this.section(section)) del(section, lbl[0 .. lbl.find("=")].strip());
      // return; continue to delete the actual section header
    }
    synchronized(this) {
      load(); scope(exit) save();
      size_t pos = findEntry(section, label);
      if (pos == size_t.max) return false;
      if (pos+1 !< lines.length) lines = lines[0 .. pos];
      else lines = lines[0 .. pos] ~ lines[pos+1 .. $];
      save();
    }
    return true;
  }
  bool has(string section, string label) {
    synchronized(this) {
      load();
      return findEntry(section, label) != size_t.max;
    }
  }
  // the lazy on deflt lets us do something really sweet
  // like throw an exception when the deflt is queried.
  //    :)
  T get(T)(string section, string label, lazy T deflt) {
    if (!label.length) throw new Exception("Get from where?! Stupid. ");
    synchronized(this) {
      load();
      try {
        auto pos = findEntry(section, label);
        if (pos == size_t.max) return deflt;
        string value=lines[pos];
        value=value[value.find("=")+1..$].strip();
        return deserialize!(T)(value);
      } catch (Exception ex) {
        throw new Exception("Couldn't get "~T.stringof~" from section "~section~", label "~label~": "~ex.toString());
      }
    }
  }
}
