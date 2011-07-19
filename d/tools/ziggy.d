module tools.ziggy; // The Ziggurat method, as per http://seehuhn.de/pages/ziggurat

import tools.base, tools.compat;

const TableSize = 128;

alias float FP;

FP[TableSize] ytab; // level values
uint[TableSize] ktab; // quick acceptance check
FP[TableSize] wtab; // quick value conversion

static this() {
  FP[TableSize] x = void; FP v = void;
  FP f(FP x) { return exp(-x * x / 2.0); } // target density
  FP finv(FP x) { return sqrt(-2.0 * tools.compat.log(x)); }
  FP try_r_value(FP r) {
    v = r * f(r) + exp(-0.5 * r * r) / r;
    x[$-1] = r;
    for (int i = x.length - 1; i >= 2; --i) {
      x[i - 1] = finv(v / x[i] + f(x[i]));
    }
    return x[1] * (1.0 - f(x[1])) / v;
  }
  FP a = 0.0, b = 10.0, r = void, aa = void, bb = void;
  do {
    aa = a; bb = b;
    r = (a + b) / 2.0;
    if (try_r_value(r) > 1.0) b = r;
    else a = r;
  } while (aa < r && r < bb);
  x[0] = 0;
  for (int i = 0; i < TableSize - 1; ++i) {
    ytab[i] = f(x[i]);
    ktab[i] = cast(uint) ((1 << 24) * x[i+1] / x[i+1]);
    wtab[i] = x[i+1] / (1 << 24);
  }
  ktab[$-1] = cast(uint) ((1 << 24) * r * f(r) / v);
  wtab[$-1] = v / ((1 << 24) * f(r));
}

const double RightPos = 3.44428647676;
FP gaussian(T, S)(ref T gen, S sigma) {
  uint rval = gen();
  auto i = rval & 0x7f, sign = !!(rval & 0x80), j = rval >> 8;
  FP x = void;
  float genf() { return gen() * 1.0 / typeof(gen()).max; }
  while (true) {
    x = j * wtab[i];
    if (j < ktab[i]) break;
    
    FP y = void;
    if (i < 127) {
      FP y0 = ytab[i], y1 = ytab[i + 1];
      y = y1 + (y0 - y1) * genf();
    } else {
      x = RightPos - tools.compat.log(1.0 - genf()) / RightPos;
      y = exp(-RightPos * (x - RightPos / 2.0)) * genf();
    }
    if (y < exp(-0.5 * x * x)) break;
  }
  return sign?(sigma * x):(- sigma * x);
}

/*
import tools.mersenne, tools.log, qd;
void main() {
  auto gen = new Mersenne;
  screen(640, 480);
  line(120, 160, 520, 320, Box=White);
  int x;
  while (true) {
    auto zigg = gaussian(gen, 1f);
    auto xi = 320 + cast(int) (100f * zigg);
    auto prev = pget(xi, 240);
    auto next = rgb(prev.r + 1, prev.g + 1, prev.b + 1);
    line(xi, 170, xi, 310, next);
    if (x++ == 256) {
      x = 0;
      flip; events;
    }
  }
}
*/
