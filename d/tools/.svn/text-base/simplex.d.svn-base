module tools.simplex;
import tools.mersenne, tools.base;
// taken from http://staffwww.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf

int fastfloor(T)(T t) {
  static if (is(T: float)) {
    if (t > 0) return cast(int) t; else return cast(int) (t - 1f);
  } else static if (is(typeof(T(0)))) { // fixed-point DS
    if (t > T(0)) return t.to!(int); else return (t - T(1)).to!(int);
  } else static assert(false);
}

version(DS) {
  byte[3][12] grad3;
  byte[4][32] grad4;
} else {
  int[3][12] grad3;
  int[4][32] grad4;
}

static this() { 
  int k;
  grad3[k++][] = [1, 1, 0]; grad3[k++][] = [-1, 1, 0];
  grad3[k++][] = [-1, -1, 0]; grad3[k++][] = [1, -1, 0];
  for (int i = 0; i < 4; ++i) {
    auto v = grad3[i];
    grad3[k++][] = [v[0], v[2], v[1]];
    grad3[k++][] = [v[2], v[1], v[0]];
  }
  k = 0;
  // "binary count" to 8
  grad4[k++][] = [0, 1, 1, 1]; grad4[k++][] = [0, 1, 1, -1];
  grad4[k++][] = [0, 1, -1, 1]; grad4[k++][] = [0, 1, -1, -1];
  grad4[k++][] = [0, -1, 1, 1]; grad4[k++][] = [0, -1, 1, -1];
  grad4[k++][] = [0, -1, -1, 1]; grad4[k++][] = [0, -1, -1, -1];
  // then shuffle the zero.
  for (int i = 1; i < 4; ++i) {
    for (int j = 0; j < 8; ++j) {
      auto my_id = i*8+j;
      grad4[my_id][] = grad4[my_id - 8];
      swap(grad4[my_id][i], grad4[my_id][i-1]); // move the zero to pos i
    }
  }
}

F dot(int I, F)(ushort id, F[] a) {
  static if (I==2 || I == 3) auto v = grad3[id];
  else auto v = grad4[id];
  auto res = v[0] * a[0];
  foreach (i, val; a[1 .. $]) res += v[i+1] * val;
  return res;
}

F selfdot(F)(F[] a) {
  F res = a[0] * a[0];
  foreach (v; a[1 .. $]) res += v * v;
  return res;
}

struct Simplex {
  version(DS) {
    ubyte[512] perm, perm12;
  } else {
    int[512] perm, perm12;
  }
  static Simplex opCall(C)(C c) {
    static if (is(C == int[])) {
      // not gonna use this.
      version(DS) static assert(false); else {
        Simplex res = void;
        with (res) {
          perm[0 .. 256] = perm[256 .. $] = c;
          foreach (i, v; perm) perm12[i] = cast(ubyte) (v % 12);
        }
        return res;
      }
    } else {
      Simplex res = void;
      with (res) {
        version(DS) foreach (ref v; cast(uint[]) perm[0 .. 256]) v = c();
        else foreach (ref v; perm[0 .. 256]) v = cast(ubyte) c();
        perm[256 .. $] = perm[0 .. 256];
        foreach (i, v; perm) perm12[i] = cast(ubyte) (v % 12);
      }
      return res;
    }
  }
  // NM == number-making template
  template FloatConst(string S) { const float FloatConst = mixin(S); }
  typeof(NM!("0")) noise(int NoiseDim, alias NM = FloatConst)(Repeat!(typeof(NM!("0")), NoiseDim) p) {
    alias typeof(NM!("0")) FP;
    alias Repeat!(FP, NoiseDim) T;
    static assert(NoiseDim == 2 || NoiseDim == 3 || NoiseDim == 4);
    // Skew the input space to determine which simplex cell we're in
    // const SQRT_3 = NM!("1.732050807568877293527446341");
    const SQRT_3 = NM!("1.73205");
    const SQRT_5 = NM!("2.236068");
    static if (NoiseDim == 2)
      // const F = NM!("0.5")*(SQRT_3-NM!("1")), G = (NM!("3")-SQRT_3)/NM!("6");
      const F = NM!("0.3660254"), G = NM!("0.211325");
    else static if (NoiseDim == 3)
      const F = NM!("0.33333"), G = NM!("0.16666");
    else static if (NoiseDim == 4)
      // const F = (SQRT_5 - NM!("1"))/NM!("4"), G = (NM!("5") - SQRT_5)/NM!("20");
      const F = NM!("0.30902"), G = NM!("0.1382");
    
    auto s = p[0]; foreach (v; p[1 .. $]) s += v;
    s *= F;
    int[NoiseDim] floors = void;
    auto t = NM!("0");
    foreach (i, v; p) {
      floors[i] = fastfloor(v + s);
      t += floors[i];
    }
    
    t *= G;
    
    FP[NoiseDim][NoiseDim+1] ps = void;
    foreach (i, v; p) ps[0][i] = v - floors[i] + t;
    
    ushort[NoiseDim+1] gi = void;
    // For the 2D case, the simplex shape is an equilateral triangle.
    // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
    // For the 4D case, well, Lovecraft would have a field day.
    // Determine which simplex we are in.
    static if (NoiseDim == 2) {
      int[2] offs_1; // Offsets for second (middle) corner of simplex in (i,j) coords
      // lower triangle, XY order: (0,0)->(1,0)->(1,1)
      if(ps[0][0]>ps[0][1]) { offs_1[0] = 1; offs_1[1] = 0; }
      // upper triangle, YX order: (0,0)->(0,1)->(1,1)
      else { offs_1[0] = 0; offs_1[1] = 1; }
    
      // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
      // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
      // c = (3-sqrt(3))/6
      for (int i = 0; i < NoiseDim; ++i) {
        ps[1][i] = ps[0][i] - offs_1[i] + G; // Offsets for middle corner in (x,y) unskewed coords
        ps[1][i] = ps[0][i] - NM!("1") + NM!("2") * G; // Offsets for last corner in (x,y) unskewed coords
      }
      
      // Work out the hashed gradient indices of the three simplex corners
      int ii = floors[0] & 255, jj = floors[1] & 255;
      gi[0] = cast(ushort) perm12[ii+perm[jj]];
      gi[1] = cast(ushort) perm12[ii+offs_1[0]+perm[jj+offs_1[1]]];
      gi[2] = cast(ushort) perm12[ii+1+perm[jj+1]];
    } else static if (NoiseDim == 3) {
      // Offsets for second and third corner of simplex in (i,j,k) coords
      const int[6]
        branch_1 = [1, 0, 0, 1, 1, 0], branch_2 = [1, 0, 0, 1, 0, 1], branch_3 = [0, 0, 1, 1, 0, 1],
        branch_4 = [0, 0, 1, 0, 1, 1], branch_5 = [0, 1, 0, 0, 1, 1], branch_6 = [0, 1, 0, 1, 1, 0];
      int ii = floors[0] & 255, jj = floors[1] & 255, kk = floors[2] & 255;
      int[] offs = void;
      if (ps[0][0]>=ps[0][1]) {
        if (ps[0][1]>=ps[0][2]) offs = branch_1;// X Y Z order
        else if (ps[0][0]>=ps[0][2]) offs = branch_2; // X Z Y order
        else offs = branch_3; // Z X Y order
      } else { // x0<y0
        if (ps[0][1]<ps[0][2]) offs = branch_4; // Z Y X order
        else if (ps[0][0]<ps[0][2]) offs = branch_5; // Y Z X order
        else offs = branch_6; // Y X Z order
      }
      //  A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
      //  a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
      //  a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
      //  c = 1/6.
      for (int i = 0; i < NoiseDim; ++i) {
        ps[1][i] = ps[0][i] - offs[i] + G;
        ps[2][i] = ps[0][i] - offs[i+3] + NM!("2") * G;
        ps[3][i] = ps[0][i] - NM!("1") + NM!("3") * G;
      }
      // Work out the hashed gradient indices of the four simplex corners
      gi[0] = cast(ushort) perm12[ii+perm[jj+perm[kk]]];
      gi[1] = cast(ushort) perm12[ii+offs[0]+perm[jj+offs[1]+perm[kk+offs[2]]]];
      gi[2] = cast(ushort) perm12[ii+offs[3]+perm[jj+offs[4]+perm[kk+offs[5]]]];
      gi[3] = cast(ushort) perm12[ii+1+perm[jj+1+perm[kk+1]]];
    } else static if (NoiseDim == 4) {
      auto n = ps[0];
      auto index =
        (n[0] > n[1])*32 + (n[0] > n[2])*16 + (n[1] > n[2])*8
      + (n[0] > n[3])*4  + (n[1] > n[3])*2  + (n[2] > n[3])*1;
      static string make_simplex() {
        string simplex_str =
          "0123 0132 0000 0231 0000 0000 0000 1230 "
          "0213 0000 0312 0321 0000 0000 0000 1320 "
          "0000 0000 0000 0000 0000 0000 0000 0000 "
          "1203 0000 1302 0000 0000 0000 2301 2310 "
          "1023 1032 0000 0000 0000 2031 0000 2130 "
          "0000 0000 0000 0000 0000 0000 0000 0000 "
          "2013 0000 0000 0000 3012 3021 0000 3120 "
          "2103 0000 0000 0000 3102 0000 3201 3210 ";
        string res = "const ubyte[4][64] simplex = [";
        for (int i = 0; i < simplex_str.length; i += 5) {
          if (i) res ~= ", ";
          res ~= "["~simplex_str[i]~", "~simplex_str[i+1]~", "~simplex_str[i+2]~", "~simplex_str[i+3]~"]";
        }
        return res ~ "]; ";
      }
      mixin(make_simplex);
      ubyte[4][3] greaters;
      {
        auto si = simplex[index];
        foreach (i, ref a; greaters) {
          foreach (k, ref v; a) {
            v = si[k] >= (3-i);
          }
        }
      }
      for (int i = 0; i < 4; ++i) {
        ps[1][i] = ps[0][i] - greaters[0][i] + G;
        ps[2][i] = ps[0][i] - greaters[1][i] + NM!("2")*G;
        ps[3][i] = ps[0][i] - greaters[2][i] + NM!("3")*G;
        ps[4][i] = ps[0][i] - true + NM!("4")*G;
      }
      int ii = floors[0]&255, jj = floors[1]&255, kk = floors[2]&255, ll = floors[3]&255;
      // not worth it to make a perm32, seeing how %32 is a single op and thus fast.
      gi[0] = cast(ushort) (perm[ii+perm[jj+perm[kk+perm[ll]]]]%32);
      // do not unroll this!
      for (int i = 0; i < 3; ++i)
        gi[i+1] = cast(ushort) (perm[ii+greaters[i][0]+perm[jj+greaters[i][1]+perm[kk+greaters[i][2]+perm[ll+greaters[i][3]]]]]%32);
      gi[4] = cast(ushort) (perm[ii+true+perm[jj+true+perm[kk+true+perm[ll+true]]]]%32);
    }
    
    auto contrib_sum = NM!("0");
    // Calculate the contribution from the n corners
    for (int i = 0; i < NoiseDim+1; ++i) {
      static if (NoiseDim == 2)
        auto cc = NM!("0.5") - selfdot(ps[i]);
      else static if (NoiseDim == 3 || NoiseDim == 4)
        auto cc = NM!("0.6") - selfdot(ps[i]);
      if (cc > NM!("0")) {
        cc *= cc;
        contrib_sum += cc * cc * dot!(NoiseDim, FP)(gi[i], ps[i]);
      }
    }
    
    static if (NoiseDim == 2) return NM!("0.5") + NM!("35") * contrib_sum;
    else static if (NoiseDim == 3) return NM!("0.5") + NM!("16") * contrib_sum;
    else static if (NoiseDim == 4) return NM!("0.5") + NM!("13.5") * contrib_sum;
  }
  // don't instantiate unneeded code
  version(DS) { } else {
    alias noise!(2) noise2;
    alias noise!(3) noise3;
    alias noise!(4) noise4;
  }
}
