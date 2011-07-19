module ff;

import qd, tools.base, std.math, dglut.vector;
alias PI π;

alias Vector!(float, 6) vec6f;
alias Vector!(float, 8) vec8f;

float int_pow(float a, int i) {
  float res = 1f;
  while (i--) res *= a;
  return res;
}

float ps(float f) { return f; }

vec4f calcθ(ref vec4f x, ref vec4f y) {
  vec4f θ = void;
  // yes it's the right way around. Srsly, look it up in the PDF.
  for (int i = 0; i < 4; ++i) θ.field[i] = atan2(x[i], y[i]);
  return θ;
}

void linear_0(ref vec4f x, ref vec4f y) { }
void sinusoidal_1(ref vec4f x, ref vec4f y) { x = x.sin(); y = y.sin(); }
void spherical_2(ref vec4f x, ref vec4f y) {
  auto sum = x * x + y * y;
  x /= sum; y /= sum;
}
void swirl_3(ref vec4f x, ref vec4f y) {
  auto sum = x * x + y * y;
  auto sins = sum.sin(), coss = sum.cos();
  auto xres = x * sins - y * coss, yres = x * coss + y * sins;
  x = xres; y = yres;
}
void horseshoe_4(ref vec4f x, ref vec4f y) {
  auto xsq = x * x, ysq = y * y, sum = xsq + ysq, r = sum.sqrt();
  auto nx = (xsq - ysq), ny = 2 * x * y;
  x = nx; y = ny;
  x /= r; y /= r;
}
void polar_5(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  x = θ / π; y = r - vec4f(1);
}
void handkerchief_6(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt(), θpr = θ + r, θmr = θ - r;
  x = θpr.sin() * r; y = θmr.cos() * r;
}
void heart_7(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt(), θr = θ * r;
  x = θr.sin() * r; y = -θr.cos() * r;
}
void disc_8(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  auto πr = r * π, θ_π = θ / π;
  x = πr.sin() * θ_π; y = πr.cos() * θ_π;
}
void spiral_9(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  x = (θ.cos() - r.sin()) / r; y = (θ.sin() - r.cos()) / r;
}
void hyperbolic_10(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  x = θ.sin() / r; y = θ.cos() * r;
}
void diamond_11(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  x = θ.sin() * r.cos(); y = θ.cos() * r.sin();
}
void ex_12(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  x = (θ + r).sin(); y = (θ - r).cos();
  x *= x*x*r; y *= y*y*r; // ^3 * r
}
Vector!(byte, 4) Ωs_1, Ωs_2 = void; // intentionally not threadsafe - it's supposed to be random after all
                                    // don't worry - enough randomness is created in the point selection.
                                    // even if there's threading-related errors, at thousands of hits per pixel
                                    // they're completely irrelevant.
                                    // this is art, not maths. ;)
static this() {
  foreach (ref Ω; Ωs_1.field) if (Ω >= 0) Ω = 1; else Ω = -1;
  foreach (ref Ω; Ωs_2.field) if (Ω >= 0) Ω = 1; else Ω = -1;
}
void julia_13(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  auto rsqt = r.sqrt(), θ_2 = θ / 2f;
  x = θ_2.cos() * Ωs_1 * rsqt;
  y = θ_2.sin() * Ωs_2 * rsqt;
  Ωs_1 *= -1; Ωs_2 *= -1; // "so-called randomness"
}
void bent_14(ref vec4f x, ref vec4f y) {
  auto xs = (-x.sgn() + 3) / 2, ys = (-y.sgn() + 3) / 2; // 1/-1 -> -1/1 -> 2/4 -> 1/2
  x *= xs; y /= ys;
}
void waves_15(ref vec4f x, ref vec4f y, float b, float c, float e, float f) {
  auto
    nx = x + b * (y / (c*c)).sin(),
    ny = y + e * (x / (f*f)).sin();
  x = nx; y = ny;
}
void fisheye_16(ref vec4f x, ref vec4f y) {
  auto r = (x * x + y * y).sqrt();
  auto factor = 2f / (r + 1f);
  auto nx = y * factor, ny = x * factor;
  x = nx; y = ny;
}
void popcorn_17(ref vec4f x, ref vec4f y, float c, float f) {
  x = (x*3).tan().sin() * c + x; y = (y*3).tan().sin() * f + y;
}
void exponential_18(ref vec4f x, ref vec4f y) {
  auto ex = (x-1).exp();
  x *= π; y *= π;
  x = x.cos(); y = y.sin();
  x *= ex; y *= ex;
}
void power_19(ref vec4f x, ref vec4f y) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  auto r_sinθ = y = θ.sin();
  for (int i = 0; i < 4; ++i) r_sinθ.field[i] = pow(r.field[i], r_sinθ.field[i]);
  x = θ.cos() * r_sinθ; y *= r_sinθ;
}
void cosine_20(ref vec4f x, ref vec4f y) {
  vec4f ch = void, sh = void;
  for (int i = 0; i < 4; ++i) {
    ch.field[i] = cosh(y.field[i]);
    sh.field[i] = sinh(y.field[i]);
  }
  auto cp = π * x;
  x = cp.cos()*ch; y = -cp.sin()*sh;
}
void rings_21(ref vec4f x, ref vec4f y, float c) {
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  auto csq = c*c;
  auto n = r + csq, o = 2f*csq;
  for (int i = 0; i < 4; ++i) if (n.field[i] > o) n.field[i] -= cast(int)(n.field[i] / o) * o;
  n -= csq;
  n += r*(1f-csq);
  x = θ.cos() * n; y = θ.sin() * n;
}
void fan_22(ref vec4f x, ref vec4f y, float c, float f) {
  auto t = π * c * c;
  auto θ = calcθ(x, y), r = (x * x + y * y).sqrt();
  auto test = θ + f;
  for (int i = 0; i < 4; ++i) while (test.field[i] > t) test.field[i] -= t;
  vec4f v = void;
  auto t_half = t/2f;
  for (int i = 0; i < 4; ++i)
    if (test.field[i] > t_half) v.field[i] = θ.field[i] - t_half;
    else v.field[i] = θ.field[i] + t_half;
  x = v.cos() * r; y = v.sin()*r;
}
void eyefish_27(ref vec4f x, ref vec4f y) {
  auto r = (x * x + y * y).sqrt();
  auto factor = 2f / (r + 1f);
  x *= factor; y *= factor;
}
void bubble_28(ref vec4f x, ref vec4f y) {
  auto r = (x * x + y * y).sqrt();
  auto factor = 4f / (r*r + 4f);
  x *= factor; y *= factor;
}
void cylinder_29(ref vec4f x, ref vec4f y) { x = x.sin(); }
void tangent_42(ref vec4f x, ref vec4f y) {
  auto nx = x.sin() / y.cos();
  y = y.sin() / x.cos(); x=nx;
}
void cross_48(ref vec4f x, ref vec4f y) {
  auto temp = x*x + y*y;
  auto factor = (1f/(temp*temp)).sqrt();
  x *= factor; y *= factor;
}

const num_functions = 28;
void fun(int which, ref vec4f x, ref vec4f y, ref vec6f coeffs) {
  switch (which) {
    case 0: linear_0(x, y); break;
    case 1: sinusoidal_1(x, y); break;
    case 2: spherical_2(x, y); break;
    case 3: swirl_3(x, y); break;
    case 4: horseshoe_4(x, y); break;
    case 5: polar_5(x, y); break;
    case 6: handkerchief_6(x, y); break;
    case 7: heart_7(x, y); break;
    case 8: disc_8(x, y); break;
    case 9: spiral_9(x, y); break;
    case 10: hyperbolic_10(x, y); break;
    case 11: diamond_11(x, y); break;
    case 12: ex_12(x, y); break;
    case 13: julia_13(x, y); break;
    case 14: bent_14(x, y); break;
    case 15: waves_15(x, y, coeffs.tuple[1..3], coeffs.tuple[4..6]); break;
    case 16: fisheye_16(x, y); break;
    case 17: popcorn_17(x, y, coeffs.tuple[2], coeffs.tuple[5]); break;
    case 18: exponential_18(x, y); break;
    case 19: power_19(x, y); break;
    case 20: cosine_20(x, y); break;
    case 21: rings_21(x, y, coeffs.tuple[2]); break;
    case 22: fan_22(x, y, coeffs.tuple[2], coeffs.tuple[5]); break;
    case 23: eyefish_27(x, y); break;
    case 24: bubble_28(x, y); break;
    case 25: cylinder_29(x, y); break;
    case 26: tangent_42(x, y); break;
    case 27: cross_48(x, y); break;
    default: fail;
  }
}

import tools.ca_rng;
struct variation {
  vec6f pre;
  vec6f post;
  vec3f color;
  ushort fn;
  void exec(ref vec4f x, ref vec4f y) {
    transform(x, y, pre);
    fun(fn, x, y, pre);
    transform(x, y, post);
  }
  static {
    void transform(ref vec4f x, ref vec4f y, vec6f coeff) {
      vec4f nx = x * coeff.field[0] + y * coeff.field[1] + coeff.field[2];
      vec4f ny = x * coeff.field[3] + y * coeff.field[4] + coeff.field[5];
      x = nx; y = ny;
    }
    void random_matrix(ref vec6f v) {
      auto a = vec2f(0), b = a;
      a.x = 1; b.y = 1; // identity matrix 
      a.rotate(int_pow(randf(), 3)*360f-180f); b.rotate(int_pow(randf(), 3)*360f-180f);
      a *= sqrt(randf()*2f); b *= sqrt(randf()*2f);
      v.field[0..2] = a.field; v.field[2] = randf-0.5f;
      v.field[3..5] = b.field; v.field[5] = randf-0.5f;
    }
    variation rand() {
      variation res;
      random_matrix(res.pre);
      random_matrix(res.post);
      res.pre *= 2f; res.post /= 2f;
      auto r = .rand;
      res.fn = r % num_functions;
      logln("fn: ", res.fn, ", Pre: ", res.pre, ", Post: ", res.post);
      res.color = vec3f.rand() / 2f + 0.5f;
      with (res.color) {
        auto h = hsv(rgb(r*255f, g*255f, b*255f));
        //h.s = 128 + h.s/2;
        h.v = 128 + h.v/2;
        auto re = rgb(h);
        r = re.r/255f; g = re.g/255f; b = re.b/255f;
      }
      return res;
    }
  }
}

import tools.array2d;
class buffer {
  array2d!(vec3f) sum;
  array2d!(long) count;
  vec6f final_transform;
  this(int w, int h) {
    sum = typeof(sum)(w, h); count = typeof(count)(w, h);
    sum = vec3f(0);
    final_transform = vec6f.rand() * 0.4f;
    final_transform.field[2] = final_transform.field[5] = 0f;
    final_transform.field[0] += 1; final_transform.field[4] += 1; // unit
  }
  ulong total_sum;
  invariant { assert (sum.w == count.w); assert (sum.h == count.h); }
  void increase (float px, float py, vec3f c) {
    int nx, ny;
    with (final_transform) {
      nx = cast(int) (px * field[0] + py * field[1] + field[2]);
      ny = cast(int) (px * field[3] + py * field[4] + field[5]);
    }
    if (nx<0 || ny<0 || nx!<sum.w || ny!<sum.h) return;
    sum[nx, ny] = sum[nx, ny] + c;
    count[nx, ny] = count[nx, ny] + 1;
    ++total_sum;
  }
  void draw() {
    const γ = 2.2;
    for (int x = 0; x < screen.w; ++x) {
      line(x, 0, x, screen.h-1, Black);
      flip; events;
      yield();
      // yes, do do this every line
      auto scale_factor = (1.0 * total_sum) / (sum.w * sum.h);
      for (int y = 0; y < screen.h; ++y) {
        real c = count[x, y], f = c / ((scale_factor+1)) + 1;
        f = log(f);
        f = pow(f, 1.0/γ); // gamma
        vec3f cc = sum[x, y] / (c+1) * vec3f(256) * f;
        foreach (ref entry; cc.field) if (entry>255) entry=255;
        pset(x, y, rgb(cc.tuple));
      }
    }
  }
}

vec6f delegate(float) vecbezier(vec6f[] fields) {
  auto values = new float[][6];
  foreach (vec; fields) foreach (i, v; vec.field) values[i] ~= v;
  auto bzrs = values /map/ &multibezier;
  return (typeof(bzrs) bs, float x) {
    vec6f res = void;
    foreach (i, ref field; res.field) field = bs[i](x);
    return res;
  } /fix/ bzrs;
}

import std.string, std.stdio, std.date, tools.threadpool, tools.threads, tools.functional, tools.log: logln;
import splines;
void main(string[] args) {
  auto start = getUTCtime(), seedval = cast(ushort) start;
  if (args.length > 1) seedval = atoi(args[1]);
  int[] subseeds; auto transition = 0.0;
  if (args.length > 2) subseeds = args[2].split(",") /map/ (string s) { return cast(int) atoi(s); };
  if (args.length > 4) transition = atoi(args[4]) * 1f / atoi(args[3]);
  string file;
  if (args.length > 5) file = args[5];
  logln("Seed: ", seedval);
  seed(seedval);
  vec_randf = &randf;
  screen(1024, 768, 0, false);
  
  auto tp = new Threadpool(2);
  
  auto buf = new buffer(screen.w, screen.h);
  float[32] xs, ys; // (<= 256), so we can use one rand call to determine all four points
  vec3f[32] colors;
  for (int i = 0; i < 32; ++i) colors[i] = vec3f(xs [i] = ys [i] = 0f);
  variation[] vars;
  for (int i = 0; i < 3; ++i) vars ~= variation.rand();
  vec6f delegate(float)[] var_pre_generators, var_post_generators;
  if (subseeds.length) {
    vec6f[][] targets_pre, targets_post;
    targets_pre.length = targets_post.length = vars.length;
    foreach (subseed; subseeds) {
      if (!subseed) {
        foreach (i, var; vars) {
          targets_pre[i] ~= var.pre;
          targets_post[i] ~= var.post; 
        }
        continue;
      }
      seed(subseed);
      foreach (i, var; vars) {
        vec6f my_pre, my_post;
        variation.random_matrix(my_pre); variation.random_matrix(my_post);
        targets_pre[i] ~= my_pre; targets_post[i] ~= my_post;
      }
    }
    logln("Building generators");
    foreach (i, var; vars) {
      var_pre_generators ~= vecbezier(targets_pre[i]);
      var_post_generators ~= vecbezier(targets_post[i]);
    }
    logln("Transitioning");
    foreach (i, ref var; vars) {
      var.pre = var_pre_generators[i](transition);
      var.post = var_post_generators[i](transition);
    }
  }
  
  flip = false;
  TLS!(long) count; New(count, { return new long; });
  long total_count() { long res; count.each((long l) { res += l; }); return res; }
  TLS!(Generator) gen; New(gen, { return new Generator(23); });
  logln("Setup done");
  void iterate(ref int iter) {
    union IntUbyteConv { int i; ubyte[4] bf; }
    IntUbyteConv cv = void; cv.i = gen()();
    foreach (ref p; cv.bf) p %= xs.length;
    vec4f cxs = void, cys = void;
    foreach (k, bogus; cxs.tuple) {
      cxs.tuple[k] = xs[cv.bf[k]];
      cys.tuple[k] = ys[cv.bf[k]];
    }
    scope(exit) {
      iter++;
      if (iter == vars.length) iter = 0;
    }
    
    vars[iter].exec(cxs, cys);
    foreach (k, p; cv.bf) {
      xs[p] = cxs[k]; ys[p] = cys[k]; // write-back
      auto
        x = xs[p] * (screen.w/2) + (screen.w/2),
        y = ys[p] * (screen.h/2) + (screen.h/2);
      colors[p] = (colors[p] + vars[iter].color) * 0.5;
      buf.increase(x, y, colors[p]);
    }
    count() += cast(ulong) 4; // four points at a time
  }
  MessageMultiChannel!(bool, false, true) stop; New(stop);
  foreach (bogus; tp.threads) tp.addTask({
    int iter;
    while (true) {
      iterate(iter);
      if (stop.active) { stop.get; return; }
    }
  });
  auto last_count = total_count();
  bool halted;
  void halt() { if (halted) return; foreach (bogus; tp.threads) stop.put(true); halted = true; }
  scope(exit) halt;
  while (true) {
    //logln("Drawing");
    buf.draw;
    float t = (getUTCtime() - start) / 1000f;
    writefln(total_count, " / ", t, "s -> ", total_count / t, " iter/s");
    /*if (total_count() > 1024*1024*2) {
      halt;
      buf.draw;
      if (args.length > 4) {
        string file_nr = args[4];
        while (file_nr.length < 4) file_nr = "0"~file_nr;
        file ~= file_nr~".bmp";
      }
      if (file) SDL_SaveBMP(display.target, file);
      break;
    }*/
    yield();
  }
}
