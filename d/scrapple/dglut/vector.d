module dglut.vector;

import std.math;

private template Init(T) { T Init; }

template Tuple(T...) { alias T Tuple; }

struct TupleWrapper(T...) { alias T Tuple; }

template isArray(T) { const bool isArray=false; }
template isArray(T: T[]) { const bool isArray=true; }

template ElemType(T: T[]) { alias T ElemType; }

template Accessor(string name, string what) {
  mixin("
    typeof("~what~") "~name~"() { return "~what~"; }
    void "~name~"(typeof("~what~") nval) { this."~what~"=nval; }
  ");
}

template opGeneric(string name, string param, string assign) {
  mixin("Vector "~name~"("~param~") {
    Vector res=void;
    foreach (idx, part; parts) res.parts[idx]="~assign~";
    return res;
  }");
}

template CountTo(size_t what) {
  static if (!what) alias Tuple!() CountTo;
  else alias Tuple!(CountTo!(what-1), what-1) CountTo;
}

// stolen from quake
float InvSqrt(float x) {
   float xhalf = 0.5f * x;
   int i = *cast(int*)&x; // store floating-point bits in integer
   i = 0x5f3759d5 - (i >> 1); // initial guess for Newton's method
   x = *cast(float*)&i; // convert new bits into float
   x = x*(1.5f - xhalf*x*x); // One round of Newton's method
   return x;
}

import std.math, std.stdio: format;
struct Vector(T, int size) {
  T[size] parts;
  static if ((T.sizeof==4) && (size==3)) T filler=0; // for 16-byte alignment
  static if (size>0) mixin Accessor!("x", "parts[0]");
  static if (size>1) mixin Accessor!("y", "parts[1]");
  static if (size>2) mixin Accessor!("z", "parts[2]");
  T opIndex(size_t which) { return parts[which]; }
  void opIndexAssign(size_t which, T n) { parts[which]=n; }
  static Vector opCall(T[] what...) {
    Vector res=void; res.parts[]=what[0..size];
    static if (is(typeof(res.filler))) res.filler=0;
    return res;
  }
  static Vector opCall(T[size] what) {
    Vector res=void; res.parts[]=what;
    static if (is(typeof(res.filler))) res.filler=0;
    return res;
  }
  // who said D has no duck typing?
  static if (is(typeof(Init!(T)*Init!(T)))) {
    float length() { return 1f/invlength; }
    float invlength() {
      float res=0;
      foreach (id; CountTo!(size)) res += parts[id]*parts[id];
      return InvSqrt(res);
    }
    const bool use_iasm=true;
    void length(float f) {
      f *= invlength();
      foreach (ref part; parts) part *= f;
    }
    void normalize() {
      auto len=invlength();
      foreach (ref part; parts) part *= len;
    }
    Vector normalized() { Vector res=*this; res.normalize; return res; }
    static if (size==3) {
      Vector cross(ref Vector other) {
        return Vector(y*other.z-z*other.y, z*other.x-x*other.z, x*other.y-y*other.x);
      }
    }
    mixin opGeneric!("opAdd", "ref Vector other", "part+other.parts[idx]");
    mixin opGeneric!("opSub", "ref Vector other", "part-other.parts[idx]");
    mixin opGeneric!("opMul", "float f",      "part*f");
    mixin opGeneric!("opDiv", "float f",      "part/f");
    string toString() { return format("vec(", T.stringof, "[", size, "]) = ", parts); }
  }
}
alias Vector!(float, 3) vec3f;
