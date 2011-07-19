module tools.serialize;
import tools.base, tools.compat;

// BEWARE! ser/unser is intended for use on 32/64-bit widdle-endian boxes.
// It cannot be used to transfer data between architectures with different endiannesses.

// With protocol version 0.2, a version header was introduced.
// This means that 0.2 files, on an attempt to read them on 0.1, _will_ lead to corruption.

// serialize/unserialize encode data in a human-readable form.
// they are thus endian-independent.

long atol(string s) {
  s = s.strip();
  if (s.length && s[0] == '-') return -atol(s[1 .. $]);
  long res;
  foreach (ch; s) {
    assert(ch >= '0' && ch <= '9', "Invalid digit: "~ch);
    res = res * 10 + (ch - '0');
  }
  return res;
}

void expect(ref string s, string t) {
  s = s.strip();
  if (auto rest = s.startsWith(t)) s = rest;
  else throw new Exception("Couldn't get "~t~" from "~s);
}

template serialization(bool DIR) {
  void func(T)(ref T t, ref string str) {
    int findSemicolon() {
      int mode;
      foreach (i, ch; str) {
        if (mode == 1) mode = 0;
        else {
          if (ch == '\\') mode = 1;
          else if (ch == ';') return i;
        }
      }
      return -1;
    }
    static if (is(T==bool)) {
      static if (DIR) str = t?"true; ":"false; ";
      else {
        if (str.tolower() == "true") t = true;
        else if (str.tolower() == "false") t = false;
        else if (auto rest = str.startsWith("true;")) {
          t = true; str = rest.strip();
        } else if (auto rest = str.startsWith("false;")) {
          t = false; str = rest.strip(); 
        } else throw new Exception("Cannot parse "~str~" as bool");
      }
    } else static if (is(string: T)) {
      static if (DIR) str = t.replace("\\", "\\\\").replace(";", "\\;") ~ "; ";
      else {
        auto pos = findSemicolon();
        if (pos == -1) {
          // throw new Exception("Cannot extract string from "~str~": invalid serialized stream, `;' missing");
          // logln("No semicolon found. Falling back to old behavior.");
          t = str;
        } else {
          auto meep = str[0 .. pos]; str = str[pos+1 .. $].strip();
          t = meep.replace("\\;", ";").replace("\\\\", "\\");
        }
      }
    } else static if ((is(T: long) || is(T==ulong)) && is(typeof(cast(T) Init!(long)))) {
      static if (DIR) str = .toString(cast(long) t)~"; ";
      else {
        auto pos = str.find(";");
        if (pos == -1) {
          // throw new Exception("Cannot extract number from "~str~": invalid serialized stream, `;' missing");
          logln("No semicolon found. Falling back to old behavior.");
          t = cast(T) str.atol();
        } else {
          auto meep = str[0 .. pos]; str = str[pos+1 .. $].strip();
          t = cast(T) meep.atol();
        }
      }
    } else static if (is(T: real) && is(typeof(cast(T) Init!(real)))) {
      static if (DIR) str = .toString(cast(real) t)~"; ";
      else {
        auto pos = str.find(";");
        // no floats in old behavior
        if (pos == -1) throw new Exception("Cannot extract number from "~str~": invalid serialized stream, `;' missing");
          auto meep = str[0 .. pos]; str = str[pos+1 .. $].strip();
          t = cast(T) meep.atof();
      }
    } else static if (is(typeof(t.SerField))) {
      static if (DIR) {
        foreach (i, val; t.SerField) {
          str ~= serialize(val);
        }
      } else {
        foreach (i, bogus; t.SerField)
          t.SerField[i] = deserialize!(typeof(bogus))(str);
      }
    } else static if (is(typeof(t.tupleof))) {
      static if (DIR) {
        str ~= "{";
        foreach (i, val; t.tupleof) {
          str ~= serialize(val);
        }
        str ~= "}";
      } else {
        str.expect("{");
        foreach (i, bogus; t.tupleof)
          t.tupleof[i] = deserialize!(typeof(bogus))(str);
        str.expect("}");
      }
    } else static if(is(typeof(t[0])) && is(typeof(t[0]=Init!(typeof(t[0])))) && is(typeof(t.length))) {
      static if (DIR) {
        str ~= serialize(t.length);
        str ~= "[";
        foreach (value; t) str ~= serialize(value);
        str ~= "]";
      } else {
        int len = deserialize!(typeof(t.length))(str);
        t.length = len;
        str.expect("[");
        for (int i = 0; i < len; ++i)
          t[i] = deserialize!(typeof(t[i]))(str);
        str.expect("]");
      }
    } else static assert(false, "Cannot "~(DIR?"":"de")~"serialize "~T.stringof~": unsupported type");
  }
}

string serialize(T)(T what) {
  string res;
  serialization!(true).func(what, res);
  return res;
}

Unstatic!(T) deserialize(T)(ref string str) {
  Unstatic!(T) res;
  serialization!(false).func(res, str);
  return res;
}

/// serialize
void ser(T)(T value, void delegate(char[]) dg, bool* has_sent_version = null) {
  if (!has_sent_version) has_sent_version = new bool;
  if (!*has_sent_version) {
    dg("tools/ser/0.2");
    *has_sent_version = true;
  }
  static if (isArray!(T)) {
    /// First dump the length (as ulong, so it's fixed across 32/64 platforms), then the data
    ser!(ulong)(cast(ulong)value.length, dg, has_sent_version);
    static if (is(T: char[])) dg(value);
    else foreach (element; value) ser(element, dg, has_sent_version);
  } else {
    static if (is(T==struct)) {
      /// Just serialize the elements in order
      foreach (element; value.tupleof) ser(element, dg, has_sent_version);
    } else {
      // fallback
      dg((cast(char*)&value)[0..value.sizeof]);
    }
  }
}

char[] ser(T, dummy=void)(T value) {
  size_t len=0; ser(value, (char[] f) { len+=f.length; });
  auto res=new char[len]; size_t offs=0;
  ser(value, (char[] f) { res[offs..offs+f.length]=f; offs+=f.length; });
  return res;
}

char[] splinter(ref char[] c, size_t amount) {
  if (c.length<amount) fail(Format("Problem: can't splinter ", amount, " from ", c));
  auto res=c[0..amount];
  c=c[amount..$];
  return res;
}

string first_text(string s) {
  if (s.length > 100) return s[0 .. 100];
  else return s;
}

T carve_0_2(T)(ref char[] t) {
  T result;
  static if (isArray!(T)) {
    result.length=carve_0_2!(ulong)(t);
    static if (is(T == char[]))
      result[] = t.splinter(result.length);
    else foreach (inout elem; result) elem=carve_0_2!(ElemType!(T))(t);
  } else {
    static if (is(T==struct)) {
      foreach (idx, bogus; result.tupleof)
        result.tupleof[idx]=carve_0_2!(typeof(bogus))(t);
    } else {
      result=*(cast(T*)(splinter(t, T.sizeof).ptr));
    }
  }
  return result;
}

T carve(T)(ref char[] t) {
  if (auto rest = t.beginsWith("tools/ser/0.2")) { t = rest; return carve_0_2!(T)(t); }
  if (auto rest = t.beginsWith("tools/ser/"))
    if (rest.length < 3) fail("Unsupported version!");
    else fail("Unsupported version: "~rest[0 .. 3]);
  T result;
  static if (isArray!(T)) {
    result.length=carve!(uint)(t);
    foreach (inout elem; result) elem=carve!(ElemType!(T))(t);
  } else {
    static if (is(T==struct)) {
      foreach (idx, bogus; result.tupleof)
        result.tupleof[idx]=carve!(typeof(bogus))(t);
    } else {
      // fallback
      result=*(cast(T*)(splinter(t, T.sizeof).ptr));
    }
  }
  return result;
}

import tools.tests;
unittest {
  auto s = ser([2, 3, 4, 5][]);
  mustEqual("ArraySerTest", carve!(int[])(s), [2, 3, 4, 5]);
  mustEqual("RestTest", s.length, 0);
}
