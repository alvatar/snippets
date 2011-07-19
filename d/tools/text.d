// everything required for Format
module tools.text;

version(Tango)
  alias char[] string;

template isPointer(T) { const bool isPointer=false; }
template isPointer(T: T*) { const bool isPointer=true; }

template isArray(T) { const bool isArray=false; }
template isArray(T: T[]) { const bool isArray=true; }

template isAssocArray(T) {
  const bool isAssocArray =
    is(typeof(T.keys)) &&
    is(typeof(T.values)) &&
    is(typeof(T[T.keys[0]]) == typeof(T.values[0]));
  }

// Copypasta from tools.ctfe
int find(string text, string match) {
  if (match.length > text.length) return -1;
  for (int i = 0; i <= text.length - match.length; ++i) {
    if (text[i .. i+match.length] == match) return i;
  }
  return -1;
}

string fmtReal(real r) {
  if (r != r) return "NaN";
  if (r > real.max) return "Inf";
  if (r<0) return "-"~fmtReal(-r);
  string res;
  auto rest = r - cast(long) r;
  for (int i=0; i<10; ++i) {
    rest *= 10;
    if (!(rest>=0 && rest<10)) return "ERROR";
    res ~= "0123456789"[cast(int) rest];
    rest -= cast(int) rest;
  }
  if (res.length > 6) res = res[0 .. 6];
  return fmtLong(cast(long) r) ~ "." ~ res;
}

string fmtLong(long l) {
  auto tl = l;
  int len_needed;
  if (tl < 0) {
    tl = -tl;
    len_needed ++;
  }
  do {
    len_needed ++;
    tl /= 10;
  } while (tl);
  auto res = new char[len_needed];
  tl = l;
  if (tl<0)
    if (-tl>=0) { tl = -tl; res[0] = '-'; }
    else { version(Delete) delete res; return "Long Artifact Number"; }
  int x;
  do {
    res[$-++x] = "0123456789"[cast(int)(tl%10)];
    tl /= 10;
  } while (tl);
  return res;
}

string fmtPointer(void *p) {
  auto i = cast(size_t) p;
  int len_needed = 2;
  do {
    // res = "0123456789ABCDEF"[i%16] ~ res;
    len_needed ++;
    i /= 16;
  } while (i);
  auto res = new char[len_needed];
  res[0 .. 2] = "0x";
  
  i = cast(size_t) p;
  int x;
  do {
    res[$-++x] = "0123456789ABCDEF"[i%16];
    i /= 16;
  } while (i);
  return res;
}

struct StringBuffer {
  char[] buffer;
  int length;
  string opCall() { return buffer[0 .. this.length]; }
  void opCatAssign(string s) {
    if (!buffer.length) buffer.length = 1024;
    while (length + s.length !< buffer.length) buffer.length = buffer.length * 2;
    buffer[this.length .. $][0 .. s.length] = s;
    length += s.length;
  }
}

StringBuffer FormatBuffer;

typedef bool areRecursing;

string Format(_T...)(_T _t) {
  version(DS) { // need to conserve memory badly
    static if (is(_T[0] == areRecursing)) {
      alias _t[1 .. $] t;
      alias _T[1 .. $] T;
    } else {
      FormatBuffer.length = 0;
      alias _t t;
      alias _T T;
    }
    alias FormatBuffer buf;
  } else {
    alias _t t;
    alias _T T;
    string buf; // not that urgently
  }
  foreach (i, elem; t) {
    alias typeof(elem) E;
    static if (is(E: string)) {
      // if (elem.length > int.max) throw new Exception("Sanity Violation in format:"~T.stringof~": entry "~ctToString(i));
      buf ~= elem;
    } else
    static if (is(E==char*)) {
      if (!elem) buf ~= "null";
      else {
        auto end = elem; while (*end) end ++;
        buf ~= elem[0 .. (end-elem)];
      }
    } else static if (isPointer!(E)) {
      auto ptr = fmtPointer(cast(void*) elem);
      scope(exit) version(Delete) delete ptr;
      buf ~= ptr;
    } else static if (is(typeof(elem.keys))) {
      buf ~= "[";
      bool first=true;
      foreach (key, value; elem) {
        if (first) first=false;
        else buf ~= ", ";
        version(DS) Format(cast(areRecursing) true, key, ": ", value);
        else buf ~= Format(key, ": ", value);
      }
      buf ~= "] ";
    } else static if (isArray!(E)) {
      static if (is(E==void[])) {
        version(DS) Format(cast(areRecursing) true, "[", elem.length, "*void]");
        else buf ~= Format("[", elem.length, "*void]");
      } else {
        buf ~= "[";
        foreach (i2, v; elem) {
          version(DS) Format(cast(areRecursing) true, v);
          else buf ~= Format(v);
          if (i2 < elem.length - 1) buf ~= ", ";
        }
        buf ~= "] ";
      }
    } else static if (is(typeof(elem.classinfo))) {
      auto obj = cast(Object) elem;
      if (!obj) buf ~= "(null)";
      else buf ~= obj.toString();
    } else static if (is(typeof(elem.toString()): string)) {
      buf ~= elem.toString();
    } else static if (is(E: long)) {
      auto lon = fmtLong(elem);
      scope(exit) version(Delete) delete lon;
      buf ~= lon;
    }
    else static if (is(E: real)) buf ~= fmtReal(elem);
    else static if (is(typeof(elem.tupleof))) {
      buf ~= E.stringof~" {";
      foreach (k, v; elem.tupleof) {
        if (k) buf ~= ", ";
        buf ~= Format(v);
      }
      buf ~= "} ";
    } else buf ~= "[Unsupported: "~E.stringof~"] ";
  }
  version(DS) return buf();
  else return buf;
}
