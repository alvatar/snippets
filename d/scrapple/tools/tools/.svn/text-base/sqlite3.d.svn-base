module tools.sqlite3;

import tools.base;

static if (size_t.sizeof == 4) {
  alias int C_int;
  alias int C_long;
} else static if (size_t.sizeof == 8) {
  alias int C_int;
  alias long C_long;
} else static assert(false, "Unsupported platform - please add C_ type aliases!");

string[int] MessageLookup;

extern(C) {
  typedef void sqlite3_db;
  typedef void sqlite3_stmt;
  int sqlite3_open(char* filename, sqlite3_db**);
  int sqlite3_close(sqlite3_db*);
  mixin(ReplaceConcat!(2,
    "int sqlite3_bind_?(sqlite3_stmt*, int !); \n",
    "?", "!",
      "blob", ", void*, int, void function(void*)",
      "double", ", double",
      "int", ", C_int",
      "int64", ", long", // always 64 bit
      "null", "",
      "text", ", char*, int, void function(void*) destr",
      "text16", ", void*, int, void function(void*) destr",
      // "value", ", sqlite3_value*", // what is this? \\todo
      "zeroblob", ", int n"
  ));
  mixin(ReplaceConcat!(2,
    "! sqlite3_column_?(sqlite3_stmt*, int iCol); \n",
    "!", "?",
      "void*", "blob", "int", "bytes", "int", "bytes16", "double", "double",
      "C_int", "int", "long", "int64", "char*", "text", "wchar*", "text16"
      // "sqlite3_value*", "value"
  ));
  mixin(ReplaceConcat!(2,
    `const SQLITE_? = !; static this() { MessageLookup[!] = "?"; }`, "?", "!",
    CountOff!(0,
      "OK", "ERROR", "INTERNAL", "PERM", "ABORT", "BUSY", "LOCKED", "NOMEM", "READONLY",
      "INTERRUPT", "IOERR", "CORRUPT", "NOTFOUND", "FULL", "CANTOPEN", "PROTOCOL", "EMPTY",
      "SCHEMA", "TOOBIG", "CONSTRAINT", "MISMATCH", "MISUSE", "NOLFS", "AUTH"),
    CountOff!(100, "ROW", "DONE")
  ));
  const SQLITE_TRANSIENT = cast(void function(void*)) -1;
  int sqlite3_prepare_v2(sqlite3_db* db, char* ch, int len, sqlite3_stmt** stmt, char** tail);
  int sqlite3_bind_parameter_index(sqlite3_stmt*, char* name);
  int sqlite3_step(sqlite3_stmt*);
  char* sqlite3_errmsg(sqlite3_db*);
  int sqlite3_finalize(sqlite3_stmt* stmt);
  int sqlite3_column_count(sqlite3_stmt* stmt);
  // enum { int: 1, float, text, blob, null }
  int sqlite3_column_type(sqlite3_stmt* stmt, int col);
  int sqlite3_reset(sqlite3_stmt* stmt);
  int sqlite3_busy_timeout(sqlite3_db*, C_int ms);
}

import std.string;

class Sqlite3Exception : Exception {
  int id;
  this(sqlite3_db* ptr, int id) {
    this.id = id;
    if (id in MessageLookup) super("SQLite3: "~MessageLookup[id]~": "~.toString(sqlite3_errmsg(ptr)));
    else super(Format("SQLite3: ", id, ": ", .toString(sqlite3_errmsg(ptr))));
  }
}

void CheckCall(sqlite3_db* ptr, int res) {
  if (res != SQLITE_OK) throw new Sqlite3Exception(ptr, res);
}

class DB {
  sqlite3_db* db;
  char* fp;
  this(string filename) { fp = filename.toStringz(); open; }
  void open() { CheckCall(db, sqlite3_open(fp, &db)); }
  void close() { if (db) { CheckCall(db, sqlite3_close(db)); db = null; } }
  int synchronous(int mode = -1) {
    if (mode != -1) {
      scope st = statement(Format("PRAGMA synchronous = ", mode, "; "));
      st.run;
    }
    scope st = statement("PRAGMA synchronous; ");
    st.step;
    return st.getColumn!(int)(0);
  }
  int lastTimeout = -1;
  void flush() { close; open; if (lastTimeout != -1) setBusyTimeout(lastTimeout); }
  ~this() { close(); }
  void setBusyTimeout(int ms) {
    lastTimeout = ms;
    CheckCall(db, sqlite3_busy_timeout(db, ms));
  }
  Statement statement(T...)(string sql, T t) { // t: bindings
    auto res = new Statement(sql);
    const FOO = T.length % 2;
    static assert(!(T.length%2), "Statement() bindings must be of the form \"name\", value, \"name2\", value2");
    foreach (i, entry; t) {
      static if (!(i % 2)) {
        static assert(is(typeof(entry): string), "Statement() bindings must be of the form \"name\", value, not "~typeof(entry).stringof);
        res.bind(entry, t[i+1]);
      }
    }
    return res;
  }
  alias statement stmt;
  class Statement {
    sqlite3_stmt* stmt;
    this(string sql) {
      char* tail;
      CheckCall(db, sqlite3_prepare_v2(db, sql.ptr, sql.length, &stmt, &tail));
    }
    void finalize() { if (stmt) { CheckCall(db, sqlite3_finalize(stmt)); stmt = null; } }
    ~this() { finalize(); }
    void reset() {
      CheckCall(db, sqlite3_reset(stmt));
    }
    void bind(S, T)(S s, T param) {
      int id;
      static if (is(S: int)) id = cast(int) s;
      else static if (is(S: string)) id = getID(cast(string) s);
      else static assert(false, "Bad index type: "~S.stringof~"!");
      
      static if (is(T: C_int) && T.sizeof <= C_int.sizeof) CheckCall(db, sqlite3_bind_int(stmt, id, cast(C_int) param));
      else static if (is(T: long)) CheckCall(db, sqlite3_bind_int64(stmt, id, cast(long) param));
      else static if (is(T: double)) CheckCall(db, sqlite3_bind_double(stmt, id, cast(double) param));
      // must be transient because of garbage collection [!]
      else static if (is(T: string)) CheckCall(db, sqlite3_bind_text(stmt, id, param.ptr, param.length, SQLITE_TRANSIENT));
      else static assert(false, "Cannot bind SQL parameter of type "~T.stringof~"!");
    }
    void opIndexAssign(S, T)(S s, T t) { bind(t, s); }
    int getID(string name) {
      auto res = sqlite3_bind_parameter_index(stmt, name.toStringz());
      if (!res) throw new Exception("SQLite3: \""~name~"\" not in statement!");
      return res;
    }
    T getColumn(T)(int id) {
      T res;
      // enum { int: 1, float, text, blob, null }
      auto type = sqlite3_column_type(stmt, id);
      if (type == 5) {
        static if (is(T: long) || is(T==ulong)) return T.max;
        else static if (is(T: double)) return Init!(T); // NaN
        else static if (is(T: string)) return null;
        else static assert(false, "Cannot generate null value for "~T.stringof~"!");
      }
      void cht(int t) {
        if (type != t) // null is always okay
          throw new Exception(Format("Got type ", [1: "INT"[], 2: "FLOAT", 3: "TEXT", 4: "BLOB", 5: "NULL"][type],
            ", but was asked for ", T.stringof, ". Mismatch. "));
      }
      static if (is(T: C_int) && T.sizeof <= C_int.sizeof) { cht(1); res = cast(T) sqlite3_column_int(stmt, id); }
      else static if (is(T: long)) { cht(1); res = cast(T) sqlite3_column_int64(stmt, id); }
      else static if (is(T: double)) { cht(2); res = cast(T) sqlite3_column_double(stmt, id); }
      else static if (is(T: string)) { cht(3); res = cast(T) .toString(sqlite3_column_text(stmt, id)); }
      else static assert(false, "Cannot get column of type "~T.stringof~"!");
      return res;
    }
    bool step(bool fin = true) {
      auto res = sqlite3_step(stmt);
      if (res == SQLITE_DONE) { if (fin) finalize; else reset; return false; }
      if (res == SQLITE_ROW) return true;
      CheckCall(db, res);
      return true;
    }
    void run(bool fin = true) { while (step(fin)) { } }
    int opApply(C)(C callable) {
      auto cols = sqlite3_column_count(stmt);
      scope(exit) finalize; //CheckCall(db, sqlite3_reset(stmt));
      alias Params!(C) PARAMS;
      if (cols != PARAMS.length)
        throw new Exception(Format("Mismatch: expected ", PARAMS.stringof, ", got ", cols, " columns .. "));
      while (step) {
        PARAMS p;
        foreach (i, T; PARAMS) p[i] = getColumn!(T)(i);
        if (auto res = callable(p)) return res;
      }
      return 0;
    }
  }
}

/*
void main() {
  auto db = new DB("test.txt");
  db.statement("drop table test; ").step;
  db.statement("create table test(foo int, bar int); ").step;
  db.statement("insert into test values($foo, $bar); ", "$foo", 4, "$bar", 5).step;
  foreach (int a, int b; db.statement("select * from test; ")) logln("a: ", a, ", b: ", b);
  logln("Finished successful!");
}
*/
