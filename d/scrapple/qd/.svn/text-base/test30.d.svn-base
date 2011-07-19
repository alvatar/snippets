module test30;

import qd, SDL_image, tools.base, SDL_ttf, tools.log;

class VM {
  float[16] regs;
  ubyte[] program;
  int pc;
  this() {
    program.length = 1024;
  }
  void reset() {
    regs[] = 0f;
    pc = 0;
  }
  void run(int cycles, lazy float randf) {
    while (cycles--) {
      auto inst = program[pc];
      int cmd = inst & 15, param = inst >> 4;
      cmd &= 7; // ignore high bit
      switch (cmd) {
        case 0:
          version(LOG) logln("branch to ", pc, "+", param + 1);
          pc = (pc + param) % program.length;
          break;
        case 1:
          version(LOG) logln("store 0(", regs[0], ") to ", param);
          regs[param] = regs[0];
          break;
        case 2:
          version(log) logln("skip cond on ", regs[0], " => ", atan2(regs[0]) / (PI/4) + 0.5);
          if (atan(regs[0]) / (PI/4) + 0.5 > randf()) pc++;
          break;
          /*version(LOG) logln("skip if ", regs[0], " > 0");
          if (regs[0] > 0) pc++;
          break;*/
        case 3:
          version(LOG) logln("load ", (param - 8) / 6f);
          regs[0] = (param - 8) / 6f;
          break;
        case 4:
          version(LOG) logln("swap 0(", regs[0], ") and ", param, "(", regs[param], ")");
          swap(regs[0], regs[param]);
          break;
        case 5:
          version(LOG) logln("add 0(", regs[0], ") + ", param, "(", regs[param], ")");
          regs[0] += regs[param];
          break;
        case 6:
          version(LOG) logln("mul 0(", regs[0], ") + ", param, "(", regs[param], ")");
          regs[0] *= regs[param];
          break;
        case 7:
          version(LOG) logln("div 0(", regs[0], ") / (1 + ", param, "(", regs[param], ") )");
          regs[0] /= regs[param] + 1;
          break;
      }
      pc++;
      if (pc == program.length) pc = 0;
    }
  }
  void generate(uint delegate() dg) {
    foreach (ref val; program) {
      val = dg() % 256;
    }
  }
}

long atol(string s) {
  long res;
  s = s.strip();
  while (s.length) {
    auto ch = s.take();
    assert(ch >= '0' && ch <= '9');
    res = res * 10L + (ch - '0');
  }
  return res;
}

// Thank you George Marsaglia .. you're a lifesaver

class KISS {
  uint z, w, jcong, jsr;
  void reset() {
    z = 362436069;
    w = 521288629;
    jcong = 380116160;
    jsr = 123456789;
  }
  this() { reset; }
  uint znew() { return z = 36969 * (z & 65535) + (z >> 16); }
  uint wnew() { return w = 18000 * (w & 65535) + (w >> 16); }
  uint mwc() { return (znew << 16) + wnew; }
  uint cong() { return jcong = 69069 * jcong + 1234567; }
  uint shr3() {
    jsr ^= jsr << 17;
    jsr ^= jsr >> 13;
    jsr ^= jsr << 5;
    return jsr;
  }
  uint kiss() { return (mwc() ^ cong()) + shr3; }
  alias kiss rand;
  float randf() { return rand() * 1f / typeof(rand()).max; }
}

import tools.mersenne, tools.time;
void main(string[] args) {
  auto exec = args.take();
  auto a2 = args;
  int steps = 1024;
  ulong seed;
  bool filter = true, show = true, quiet = false;
  int aa;
  string output;
  int w = 640, h = 480;
  while (args.length) {
    auto flag = args.take();
    switch (flag) {
      case "-steps": steps = args.take().atoi(); break;
      case "-seed": seed = args.take().atol(); filter = false; break;
      case "-o": output = args.take(); break;
      case "-nofilter": filter = false; break;
      case "-filter": filter = true; break;
      case "-noshow": show = false; break;
      case "-aa": aa = args.take().atoi(); break;
      case "-w": w = args.take().atoi(); break;
      case "-h": h = args.take().atoi(); break;
      case "-q": quiet = true; break;
    }
  }
  auto test = new VM;
  screen(w, h, 0, !show);
  auto last = sec();
  while (true) {continue_outer:
    auto sv = µsec();
    if (seed) sv = seed;
    void update() {
      if (sec() - last > 0.2) {
        if (!quiet)
          print(10, 10, Bottom|Right, Back = Black, Format("::", sv));
        flip;
        events;
        last = sec();
      }
    }
    auto rng = new Mersenne(sv);
    test.generate(&rng.rand);
    const STEP = 1;
    bool testmode = filter;
    rerun:
    int fail;
    rgb prev;
    auto rung = new KISS;
    rgb eval(float x, float y) {
      test.reset;
      if (testmode) rung.reset();
      test.regs[4] = (x - 0.5) * 4f;
      test.regs[5] = (y - 0.5) * 4f;
      test.run(steps, rung.randf);
      // logln("post-register file ", test.regs);
      rgb col;
      foreach (i, ref val; test.regs[1..4]) {
        auto ub = cast(ubyte) (val * 255);
        if (ub > 0) {
          if (ub < 128) col.values[i] = ub * 2;
          else if (ub < 256) col.values[i] = (255 - ub) * 2;
        }
      }
      return col;
    }
    for (int y = 0; y < screen.h; y += STEP) {
      for (int x = 0; x < screen.w; x += STEP) {
        rgb col;
        if (aa) {
          int[3] sums;
          for (int y2 = 0; y2 < aa; ++y2) {
            for (int x2 = 0; x2 < aa; ++x2) {
              auto lc = eval((x * 1f + x2 * 1f / aa) / screen.w, (y * 1f + y2 * 1f / aa) / screen.h);
              foreach (i, v; lc.values)
                sums[i] += v;
            }
          }
          foreach (k, ref v; col.values)
            v = sums[k] / (aa*aa);
        } else {
          col = eval(x * 1f / screen.w, y * 1f / screen.h);
        }
        if (col == prev) fail++;
        else fail = 0;
        prev = col;
        if (testmode) {
          if (filter && y < 16 && fail > 4096) {
            // logln("Giving up. ");
            goto continue_outer;
          }
          if (y !< 16) {
            testmode = false;
            goto rerun; // redo
          }
        }
        putpixel32(x, y, col);
      }
      update;
    }
    auto bmpname = Format("temp_", sv, ".bmp");
    SaveBMP(display, bmpname);
    auto dest = output;
    if (!dest.length) dest = Format("images/", sv, ".png");
    string num = Format(steps);
    while (num.length < 6) num = "0" ~ num;
    dest = dest.replace("%NUM%", num);
    system(Format("convert ", bmpname, " ", dest, " && rm ", bmpname).toStringz());
    logln("Saved to ", dest);
    if (seed) break;
  }
}
