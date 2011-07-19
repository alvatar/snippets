module tools.vector;

import tools.compat, tools.base, tools.ziggy;
const PI180 = PI / 180.0;

version(GNU) {
  import gcc.builtins;
  alias __builtin_sqrtf fastsqrt;
  alias __builtin_sqrt fastsqrt;
  alias __builtin_sqrtl fastsqrt;
} else {
  // TODO: ldc intrinsics here
  alias sqrt fastsqrt;
}

// flatten doesn't do what you think it does
/*version(GNU) const string prepend = "pragma(GNU_attribute, flatten) ";
else */const string prepend = "";

version(GNU) const bool Extend3to4 = true;
else const bool Extend3to4 = false;

import tools.log: logln;

template RelFloatType(T) {
  static if (is(T == float) || is(T == double) || is(T == real))
    alias T RelFloatType;
  else
    alias float RelFloatType;
}

string opGeneric(string specs) {
  string res;
  while (specs.length) {
    auto spec = specs.ctSlice(";").ctStrip();
    specs = specs.ctStrip();
    auto type = spec.ctSlice("·");
    auto name = spec.ctSlice("·").ctStrip();
    auto op = spec.ctSlice("·").ctStrip();
    auto reverse = spec.ctSlice("·").ctStrip() == "true";
    res ~= prepend ~ ` Vector op`~name~`(`~type~` other) {
      Vector res = void;
      for (int i = 0; i < (Extend3to4?real_size:size); ++i)
        static if (is(typeof(other.real_field)))
          res.real_field[i] = cast(T) (real_field[i] `~op~` other.real_field[i]);
        else
          res.real_field[i] = cast(T) (real_field[i] `~op~` other);
      return res;
    }
    ` ~ prepend ~ ` void op`~name~`Assign(`~type~` other) {
      for (int i = 0; i < (Extend3to4?real_size:size); i++)
        static if (is(typeof(other.real_field)))
          real_field[i] `~op~`= other.real_field[i];
        else
          real_field[i] `~op~`= other;
    }
    `;
    if (reverse) {
      res ~= prepend ~ ` Vector op`~name~`_r(`~type~` other) {
        Vector res = void;
        for (int i = 0; i < (Extend3to4?real_size:size); ++i)
          static if (is(typeof(other.real_field)))
            res.real_field[i] = cast(T) (other.real_field[i] `~op~` real_field[i]);
          else
            res.real_field[i] = cast(T) (other `~op~` real_field[i]);
        return res;
      }
      `;
    }
  }
  return res;
}

struct Vector(T, int size, int _real_size = 0) {
  version(noSSE) const bool useSSE = false;
  else /*const bool useSSE = is(typeof({
    void* foo;
    asm { mov EAX, foo; movups XMM0, [EAX]; }
  }));*/
    const bool useSSE = false;
  version(NoExtend) const ExtendSize = false;
  else const ExtendSize = size == 3 && is(T == float);
  const real_size = ExtendSize ? 4 : size;
  // [   real_field   ]
  //            [bogus]
  // [  field  ]
  // [  tuple  ]
  // [x] [y] [z]
  // [r] [g] [b]
  union {
    static if (is(int: T)) T[real_size] real_field = 0;
    else T[real_size] real_field;
    struct {
      union {
        T[size] field;
        Repeat!(T, size) tuple;
        alias tuple SerField;
        struct { static if (size>0) T x; static if (size>1) T y; static if (size>2) T z; static if (size>3) T w; }
        struct { static if (size>0) T r; static if (size>1) T g; static if (size>2) T b; static if (size>3) T a; }
      }
      static if (size == 3 && real_size == 4) T bogus;
    }
  }
  static if (is(typeof(-T))) {
    Vector mirror(int id)() { return Vector(tuple[0..id], -tuple[id], tuple[id+1..$]); }
    static if (size>0) alias mirror!(0) xmirror;
    static if (size>1) alias mirror!(1) ymirror;
    static if (size>2) alias mirror!(2) zmirror;
    static if (size>3) alias mirror!(3) wmirror;
  }
  template _swizzle(string HOW) {
    // the extra parameters are taken as extensions of the existing vector tuple
    // vec2f foo; foo.xyz(4) is vec3f
    // If you can remove this hackaround and not get frontend crashes, PLEASE do so.
    // I feel dirty just writing it.
    static if (HOW.length == size) alias Vector Type;
    else alias Vector!(T, HOW.length) Type;
    Type swizzle(S...)(S extra) {
      Vector!(T, size+S.length) newsource = void;
      newsource.field[0..size][] = field;
      foreach (id, value; extra) newsource.tuple[size+id] = value;
      Type res = void;
      foreach (i, bogus; Repeat!(float, HOW.length))
        res.tuple[i] = mixin("newsource."~HOW[i]);
      return res;
    }
  }
  template swizzle(string HOW) { alias _swizzle!(HOW).swizzle swizzle; }
  template expr(string EX) {
    typeof(({ T params; return mixin(EX); })()) expr(T...)(T params) {
      return mixin(EX);
    }
  }
  alias swizzle!("xy") xy; alias swizzle!("yx") yx;
  alias swizzle!("xz") xz; alias swizzle!("zx") zx;
  alias swizzle!("yz") yz; alias swizzle!("zy") zy;
  Vector!(U, size, _real_size) to(U)() {
    static assert(is(typeof(cast(U) Init!(T))), "Cannot call to!("~U.stringof~") on Vector!("~T.stringof~", "~size.stringof~")");
    Vector!(U, size, _real_size) res = void;
    foreach (i, entry; tuple) res.tuple[i] = cast(U) entry;
    return res;
  }
  T opIndex(size_t which) { return field[which]; }
  void opIndexAssign(size_t which, T n) { field[which]=n; }
  static Vector opCall(T what) {
    Vector res = void;
    res.field[] = what;
    static if (is(T: float)) res.real_field[size .. real_size] = 0;
    return res;
  }
  static if (size > 1)
    static Vector opCall(Repeat!(T, size) what) {
      Vector res = void;
      foreach (i, v; what) res.field[i] = v;
      static if (is(T: float))
        res.real_field[size .. real_size] = 0;
      return res;
    }
  static Vector opCall(T[size] what) {
    Vector res=void; res.field[]=what;
    static if (is(typeof(res.filler))) res.filler=0;
    return res;
  }
  static if (is(typeof(abs(field[0])))) Vector abs() {
    Vector res = void;
    for (int i = 0; i < size; ++i)
      res.field[i] = .abs(field[i]);
    return res;
  }
  static if (is(typeof(field[0]>0))) Vector!(byte, size, size) sgn() {
    Vector!(byte, size, size) res = void;
    for (int i = 0; i < size; ++i)
      if (field[i] >= 0) res.field[i] = 1;
      else res.field[i] = -1;
    return res;
  }
  static if (is(T == float)) Vector sqrt() {
    Vector res = void;
    for (int i = 0; i < size; ++i) res.field[i] = .fastsqrt(field[i]);
    return res;
  }
  static if (is(typeof(T*T))) Vector sqr() {
    Vector res = void;
    for (int i = 0; i < real_size; ++i) res.field[i] = field[i] * field[i];
    return res;
  }
  static if (is(typeof(T > T))) {
    Vector min(Vector other) {
      Vector res = void;
      for (int i = 0; i < size; ++i) res.field[i] = tools.base.min(field[i], other.field[i]);
      return res;
    }
    T min() {
      T res = field[0];
      foreach (entry; field[1 .. $]) if (entry < res) res = entry;
      return res;
    }
    Vector max(Vector other) {
      Vector res = void;
      for (int i = 0; i < size; ++i) res.field[i] = tools.base.max(field[i], other.field[i]);
      return res;
    }
    T max() {
      T res = field[0];
      foreach (entry; field[1 .. $]) if (entry > res) res = entry;
      return res;
    }
  } else pragma(msg, "Cannot compare ", T.stringof, " for size");
  static if (is(typeof(T + T))) mixin(opGeneric("Vector · Add · +; T · Add · + · true; "));
  static if (is(typeof(T - T))) mixin(opGeneric("Vector · Sub · -; T · Sub · - · true; "));
  static if (is(typeof(T * T))) mixin(opGeneric("Vector · Mul · *; T · Mul · * · true; "));
  static if (is(typeof(T / T))) mixin(opGeneric("Vector · Div · /; T · Div · / · true; "));
  static if (is(T: real) || is (T: long)) {
    RelFloatType!(T) length() { return .fastsqrt(lensq); }
    Vector length(RelFloatType!(T) t) {
      t /= length();
      for (int i = 0; i < real_size; ++i) real_field[i] *= t;
      return *this;
    }
    void normalize() {
      static if ((size == 3) && is(T == float) && useSSE) { // problems on 64-bit
        float factor = void;
        auto p = field.ptr;
        asm {
          mov EAX, p;
          movups XMM0, [EAX];
          movaps XMM1, XMM0; // [b] == [a]
          mulps XMM1, XMM1; // [b: a*a]
          movaps XMM2, XMM1; // [c: a*a]
          shufps XMM2, XMM2, 0x1; // [c: a*a shift1]
          
          movaps XMM3, XMM1; // [d: a*a]
          shufps XMM3, XMM3, 0x2; // [d: a*a shift2]
          
          addss XMM1, XMM2; addss XMM1, XMM3; // [b: x+y+z ? ? ?]
          rsqrtss XMM1, XMM1; // [b: sqrt(x+y+z) ? ? ?]
          shufps XMM1, XMM1, 0x0; // [b: "" same same same]
          mulps XMM0, XMM1; // [a: res]
          
          movups [EAX], XMM0;
        }
      } else {
        auto len = length();
        for (int i = 0; i < real_size; ++i)
          real_field[i] /= len;
      }
    }
    void normalize(out typeof(length()) len) {
      len = length();
      for (int i = 0; i < size; ++i) field[i] /= len;
    }
    static if (size == 3) {
      float angle(Vector v) { // unsigned .. 
        return acos(dot(v) / .fastsqrt(lensq * v.lensq));
      }
      float angle(Vector v, Vector reference) { // signed
        // yay, http://tomyeah.com/signed-angle-between-two-vectors3d-in-cc/
        bool flipped = cross(v).dot(reference) < 0;
        auto res = acos(dot(v) / .fastsqrt(lensq * v.lensq));
        // fudge
        if (flipped) res = -res;
        return res;
      }
    } else static if (size == 2) {
      float angle(Vector v) {
        auto res = atan2(v.y, v.x) - atan2(y, x);
        while (res < -PI) res += 2*PI;
        while (res > PI) res -= 2*PI;
        return res;
      }
    }
    Vector normalized() { Vector res=*this; res.normalize; return res; }
    static if (is(typeof(cast(float) Init!(T))) && !is(T == float))
      Vector!(float, size, _real_size) opCast() {
        Vector!(float, size, _real_size) res = void;
        foreach (i, v; tuple) res.tuple[i] = cast(float) v;
        return res;
      }
    static if (size == 3) {
      Vector cross(ref Vector other) {
        return Vector(cast(T) (y*other.z-z*other.y), cast(T) (z*other.x-x*other.z), cast(T) (x*other.y-y*other.x));
      }
      Vector opMul_r(T[] mat) {
        assert(mat.length==16);
        Vector res=void;
        res.x=cast(T) (x*mat[0] + y*mat[4] + z*mat[8] + mat[12]);
        res.y=cast(T) (x*mat[1] + y*mat[5] + z*mat[9] + mat[13]);
        res.z=cast(T) (x*mat[2] + y*mat[6] + z*mat[10]+ mat[14]);
        /*auto w = cast(T) (x*mat[3] + y*mat[7] + z*mat[11] + mat[15]);
        res /= w;*/
        return res;
      }
    }
    static if (size == 4) {
      Vector ham_mult(Vector v) { // yay quaternions
        Vector res = void;
        res.x = x*v.x - y*v.y - z*v.z - w*v.w;
        res.y = x*v.y + y*v.x + z*v.w - w*v.z;
        res.z = x*v.z - y*v.w + z*v.x + w*v.y;
        res.w = x*v.w + y*v.z - z*v.y + w*v.x;
        return res;
      }
      Vector ham_sqr() {
        Vector res = void;
        res.x = x*x - y*y - z*z - w*w;
        res.y = 2 * x*y;
        res.z = 2 * x*z;
        res.w = 2 * x*w;
        return res;
      }
    }
    static if (!is(T==byte)) {
      mixin(opGeneric("Vector!(byte, size) · Mul · *; Vector!(byte, size) · Div · /; "));
    }
    Vector opNeg() {
      Vector res = *this; foreach (ref v; res.field) v = -v; return res;
    }
    float dot(Vector v) {
      Vector temp = opMul(v);
      float sum = 0f;
      foreach (elem; temp.tuple) sum += elem;
      return sum;
    }
    float lensq() {
      Vector temp = opMul(*this);
      float sum = 0f;
      foreach (elem; temp.tuple) sum += elem;
      return sum;
    }
    string toString() {
      static if (is(T==float)) {// default
        string res = "<";
        foreach (i, entry; field) {
          if (i) res ~= " ";
          res ~= Format(entry);
        }
        return res ~ ">";
      } else return Format("vec(", T.stringof, ") ", field);
    }
    static if (is(float: T)) {
      Vector mix(Vector other, float f) {
        return opMul(1f - f) + other*f;
      }
      Vector mix(Vector other, Vector f) {
        return opMul(Vector(1f) - f) + other*f;
      }
      alias mix blend;
      static if (size==2) {
        Vector rotate(float deg) {
          Vector res = void;
          res.field[0] = .cos(deg) * field[0] - .sin(deg) * field[1];
          res.field[1] = .sin(deg) * field[0] + .cos(deg) * field[1];
          field[] = res.field;
          return res;
        }
        Vector rotated(float deg) { Vector res = *this; return res.rotate(deg); }
      }
      static if (size==3) {
        // http://www.mines.edu/~gmurray/ArbitraryAxisRotation/ArbitraryAxisRotation.html
        Vector rotate(Vector pos, Vector dir, float angle) {
          Vector res;
          auto a=pos.tuple[0], b=pos.tuple[1], c=pos.tuple[2];
          auto u=dir.tuple[0], v=dir.tuple[1], w=dir.tuple[2];
          auto vw=v*v+w*w, uw=u*u+w*w, uv=u*u+v*v, cosa=.cos(angle), sina=.sin(angle);
          auto lsq=dir.lensq, len=.fastsqrt(lsq), dd=dot(dir);
          res.x=a*vw+u*(-b*v-c*w+dd)+((x-a)*vw+u*(v*(b-y)+w*(c-z))) * cosa + len*(w*(b-y)+v*(z-c)) * sina;
          res.y=b*uw+v*(-a*u-c*w+dd)+((y-b)*uw+v*(u*(a-x)+w*(c-z))) * cosa + len*(w*(x-a)+u*(c-z)) * sina;
          res.z=c*uv+w*(-a*u-b*v+dd)+((z-c)*uv+w*(u*(a-x)+v*(b-y))) * cosa + len*(v*(a-x)+u*(y-b)) * sina;
          res/=lsq;
          field[]=res.field;
          return res;
        }
        Vector rotate(Vector dir, float angle) body {
          debug if (.abs(dir.length - 1.0) > 0.001) {
            logln("Bad rotation dir: ", dir);
            asm { int 3; }
          }
          Vector res;
          float dd=dot(dir), cosa=.cos(angle), sina=.sin(angle);
          
          static if (is(T==float) && useSSE) { // \TODO: there was some issue here .. investigate. verify.
            auto dp = dir.real_field.ptr, mp = real_field.ptr, ddp = &dd, sinlp = &sina, cosp = &cosa, tp = res.real_field.ptr;
            asm {
              mov EAX, dp; movups XMM4, [EAX];
              mov EAX, mp; movups XMM5, [EAX];
              // XMM4: uvw_ (dir)
              // XMM5: xyz_
              shufps XMM5, XMM5, 0b_01_10_01_00; // xyzy, a little trick for later
              
              movaps XMM6, XMM4;
              shufps XMM6, XMM6, 0b_00_01_10_10;
              // XMM6: wwv_
              
              movaps XMM7, XMM4;
              shufps XMM7, XMM7, 0b_00_00_00_01;
              // XMM7: vuu_
              
              // start accumulating the result in XMM0
              movaps XMM0, XMM4;
              mov EAX, ddp; movss XMM1, [EAX]; shufps XMM1, XMM1, 0b_00_00_00_00;
              mulps XMM0, XMM1; // res = dir * dd
              
              // start accumulating the cosa part in XMM1
              movaps XMM1, XMM6;
              mulps XMM1, XMM1;
              movaps XMM2, XMM7;
              mulps XMM2, XMM2;
              addps XMM1, XMM2; // (vv_uu_uu * ww_ww_vv
              mulps XMM1, XMM5;
              
              xorps XMM3, XMM3; // zero out
              subps XMM3, XMM5; // -xyz
              
              movaps XMM2, XMM3; // -xyz
              shufps XMM2, XMM2, 0b_00_00_00_01; // -yxx_
              mulps XMM2, XMM7;
              addps XMM1, XMM2; // + vuu*[-yxx]
              
              movaps XMM2, XMM3; // -xyz
              shufps XMM2, XMM2, 0b_00_01_10_10; // -zzy_
              mulps XMM2, XMM6;
              addps XMM1, XMM2; // + wwv*[-zzy]
              
              mov EAX, cosp; movss XMM2, [EAX];
              shufps XMM2, XMM2, 0b_00_00_00_00;
              mulps XMM1, XMM2; // ) * cosa
              
              // flush
              addps XMM0, XMM1;
              
              // start accumulating the sina part in XMM1
              // remember, XMM3 is still -xyz
              movaps XMM1, XMM5;
              unpcklps XMM1, XMM3; // [x, -x, y, -y]
              shufps XMM1, XMM1, 0b_00_01_00_11; // [-y][x][-x][_]
              mulps XMM1, XMM6;
              
              movaps XMM2, XMM5; // xyzy .. this is what the trick is for:
              unpckhps XMM2, XMM3; // [z, -z, y, _] ! No shufps needed. :)
              mulps XMM2, XMM7; // * vuu
              addps XMM1, XMM2; // almost done
              
              mov EAX, sinlp; movss XMM2, [EAX];
              shufps XMM2, XMM2, 0b_00_00_00_00; // let it be heard in all your registers
              mulps XMM1, XMM2; // GO FORTH AND MULTIPLY
              
              addps XMM0, XMM1; // .. it is done.
              mov EAX, tp;
              movups [EAX], XMM0; // finishing touch
              
              // res.? = dir.? * dd + (?*[vw,uw,uv].? + [v,u,u].?*[-y,-x,-x].? + [w,w,v].?*[-z,-z,-y].?) * cosa
              //    + [w,w,v].?*[-y,x,-x].?+[v,u,u].?*[z,-z,y].? * sina
              // u: -x -y -z
              // v:  x -y  z
              // w:  x -y -z
            }
          } else {
            float u=dir.x, v=dir.y, w=dir.z;
            float uu = u*u, vv = v*v, ww = w*w;
            float v_w=vv+ww, u_w=uu+ww, u_v=uu+vv;
            auto dots = Vector(v_w, u_w, u_v);
            res.x = dir.x*dd+(x*v_w+dir.x*(v*(-y)+w*(-z))) * cosa + (w*(-y)+v*z) * sina;
            res.y = dir.y*dd+(y*u_w+dir.y*(u*(-x)+w*(-z))) * cosa + (w*x+u*(-z)) * sina;
            res.z = dir.z*dd+(z*u_v+dir.z*(u*(-x)+v*(-y))) * cosa + (v*(-x)+u*y) * sina;
            res /= dir.lensq;
          }
          field[]=res.field;
          return res;
        }
        Vector rotated(Vector pos, Vector dir, float angle) {
          Vector res=void; res.field[]=field;
          return res.rotate(pos, dir, angle);
        }
        Vector rotated(Vector dir, float angle) {
          Vector res=void; res.field[]=field;
          return res.rotate(dir, angle);
        }
        /*static Vector rand_halfsphere_surf(T)(Vector normal, ref float d, ref T rng) {
          Vector res=void; float lsq = void;
          Vector!(float, 2) v = void;
          while (true) {
            v = typeof(v).rand({ return rng() * 1f / typeof(rng()).max; });
            lsq = v.lensq();
            if (lsq<1) break;
          }
          auto sq = .fastsqrt(1f - lsq);
          res.x = 2f * v.x * sq; res.y = 2f * v.y * sq; res.z = 1f - 2f * lsq;
          d = res.dot(normal);
          if (d>0) return res; else { d = -d; return -res; }
        }*/
        import tools.ziggy;
        const SQRT2 = 1.414213562373095048801688724;
        static Vector rand_halfsphere_surf(T)(Vector normal, ref float d, ref T rng) {
          Vector res = void;
          res.x = gaussian(rng, SQRT2);
          res.y = gaussian(rng, SQRT2);
          res.z = gaussian(rng, SQRT2);
          res /= res.length;
          d = res.dot(normal);
          if (d > 0) return res; else { d = -d; return -res; }
        }
        static Vector rand_halfsphere_cosine_weighted(T)(Vector normal, ref float d, ref T rng) {
          for (int i = 0; i < 256; ++i) {
            auto res = rand_halfsphere_surf(normal, d, rng);
            if (d >= (rng() * 1.0 / typeof(rng()).max)) return res;
          }
          return normal; // fallback;
        }
        Vector mat_mult(float[] mat, float w) in { assert(mat.length == 16 || mat.length == 12); } body {
          Vector res = void;
          static if (real_size == 4) real_field[3] = w;
          auto mat0 = mat[0..4], mat1 = mat[4..8], mat2 = mat[8..12];
          float[real_size] sumfield0, sumfield1, sumfield2;
          sumfield0[] = 0f; sumfield1[] = 0f; sumfield2[] = 0f;
          for (int k = 0; k < real_size; ++k) {
            sumfield0[k] = real_field[k] * mat0[k];
            sumfield1[k] = real_field[k] * mat1[k];
            sumfield2[k] = real_field[k] * mat2[k];
          }
          static if (real_size == 4) {
            res.x = sumfield0[0] + sumfield0[1] + sumfield0[2] + sumfield0[3];
            res.y = sumfield1[0] + sumfield1[1] + sumfield1[2] + sumfield1[3];
            res.z = sumfield2[0] + sumfield2[1] + sumfield2[2] + sumfield2[3];
          } else {
            res.x = sumfield0[0] + sumfield0[1] + sumfield0[2] + w * mat0[3];
            res.y = sumfield1[0] + sumfield1[1] + sumfield1[2] + w * mat1[3];
            res.z = sumfield2[0] + sumfield2[1] + sumfield2[2] + w * mat2[3];
          }
          return res;
        }
      }
      static Vector rand_sphere(float delegate() dg = null) {
        while (true) {
          auto test = rand(dg);
          if (test.lensq < 1) return test;
        }
      }
      static Vector rand(float delegate() dg = null) {
        const string code = `
        Vector res=void;
        foreach (id, v; res.tuple) res.tuple[id] = #*2f-1f;
        return res;
        `;
        if (dg) { mixin(ctReplace(code, "#", "dg()")); }
        else { mixin(ctReplace(code, "#", "vec_randf()")); }
      }
      Vector sin() { Vector res = void; for (int i = 0; i < size; ++i) res.field[i] = .sin(field[i]); return res; }
      Vector cos() { Vector res = void; for (int i = 0; i < size; ++i) res.field[i] = .cos(field[i]); return res; }
      Vector tan() { Vector res = void; for (int i = 0; i < size; ++i) res.field[i] = .tan(field[i]); return res; }
      Vector exp() { Vector res = void; for (int i = 0; i < size; ++i) res.field[i] = .exp(field[i]); return res; }
    }
  }
}

alias Vector!(float, 2) vec2f;
alias Vector!(float, 3) vec3f;
alias Vector!(float, 4) vec4f;
alias Vector!(int, 2) vec2i;

string mad(string s) {
  string numbuf="", elsebuf=""; bool nummode=false;
  string res="";
  bool blockNum;
  foreach (ch; s) {
    if (ch == '[') blockNum = true;
    if (ch == ']') blockNum = false;
    if (!blockNum && ch >= '0' && ch <= '9') {
      if (!nummode) { res ~= elsebuf; numbuf = ""; }
      nummode = true;
    } else {
      if (nummode) { res ~= "mat[" ~ numbuf[0] ~ "*4+" ~ numbuf[1] ~ "]"; elsebuf = ""; }
      nummode = false;
    }
    if (nummode) numbuf ~= ch;
    else elsebuf ~= ch;
  }
  if (nummode) {
    res ~= "mat[" ~ numbuf[0] ~ "*4+" ~ numbuf[1] ~ "]";
  }
  else res ~= elsebuf;
  return res;
}

float[] transpose(float[] mat) in { assert(mat.length == 16); } body {
  float[16] res = void;
  res[0 .. 4] = [mat[0], mat[4], mat[ 8], mat[12]];
  res[4 .. 8] = [mat[1], mat[5], mat[ 9], mat[13]];
  res[8 ..12] = [mat[2], mat[6], mat[10], mat[14]];
  res[12..16] = [mat[3], mat[7], mat[11], mat[15]];
  mat[] = res;
  return mat;
}

// Thanks! http://inside.mines.edu/~gmurray/ArbitraryAxisRotation/ArbitraryAxisRotation.html
void genRotation(float[] mat, vec3f base, vec3f dir, float angle) {
  float cosa=.cos(angle), sina=.sin(angle);
  float a=base.x, b=base.y, c=base.z;
  float u=dir.x, v=dir.y, w=dir.z;
  float uu = u*u, vv = v*v, ww = w*w;
  float v_w=vv+ww, u_w=uu+ww, u_v=uu+vv;
  float bvcw  = b*v + c*w, aucw  = c*w + a*u, aubv  = a*u + b*v;
  float bw_cv = b*w - c*v, cu_aw = c*u - a*w, av_bu = a*v - b*u;
  // don't forget to divide by dir.lensq, i.e. uu+vv+ww
  auto dd = dir.lensq, d = fastsqrt(dd); // OWCH! Square root!
  mat[ 0.. 4] = [uu + v_w*cosa, u*v*(1f - cosa) - w*d*sina, u*w*(1f - cosa) + v*d*sina, a*v_w - u*bvcw + (u*bvcw - a*v_w)*cosa + bw_cv*d*sina];
  mat[ 4.. 8] = [u*v*(1f - cosa) + w*d*sina, vv + u_w*cosa, w*v*(1f - cosa) - u*d*sina, b*u_w - v*aucw + (v*aucw - b*u_w)*cosa + cu_aw*d*sina];
  mat[ 8..12] = [u*w*(1f - cosa) - v*d*sina, v*w*(1f - cosa) + u*d*sina, ww + u_v*cosa, c*u_v - w*aubv + (w*aubv - c*u_v)*cosa + av_bu*d*sina];
  if (mat.length == 16) mat[12..16] = [0f, 0f, 0f, 1f];
  foreach (ref value; mat[0..12]) value /= dd;
}

vec3f bogus; // force template instantiation for unittests

import tools.mersenne;
alias tools.mersenne.rand rand;
float default_randf() { return (1f*rand())/(1f*typeof(rand()).max); }
float function() vec_randf;
static this() { vec_randf = &default_randf; }
