module buddhabrot;

import qd, tools.base, std.math;

void main() {
  screen(1024, 768);
  cls(Black);
  auto tx = translate(0, screen.w, -2.0, 2.0), ty = translate(0, screen.h, -2.0, 2.0);
  auto backx = translate(-2.0, 2.0, 0, screen.w), backy = translate(-2.0, 2.0, 0, screen.h);
  int aa = 4;
  for (int y = 0; y < screen.h; ++y) {
    for (int x = 1; x < screen.w; ++x) {
      for (int yaa = 0; yaa < aa; ++yaa) {
        for (int xaa = 0; xaa < aa; ++xaa) {
          creal start = tx(x + xaa * 1.0 / aa) * 1fi + ty(y + yaa * 1.0 / aa), c = 0.0 + 0.0i;
          int iters = 4000;
          int bailout = -1;
          for (int i = 0; i < iters; ++i) {
            c = c * c + start;
            if (sqrt(c.re * c.re + c.im * c.im) > 2) { bailout = i; break; }
          }
          if (bailout == -1) continue;
          c = 0.0 + 0.0i;
          for (int i = 0; i < bailout; ++i) {
            c = c * c + start;
            int round(float f) { return cast(int) (f + 0.5); }
            auto bx = round(backx(c.im)), by = round(backy(c.re));
            if (bx < 1 || by < 0 || bx !< screen.w || by !< screen.h) break;
            auto target = pget(bx, by);
            if (i < 20) { if (target.r < 255) target.r ++; }
            else if (i < 200) { if (target.g < 255) target.g ++; }
            else /*if (bailout < 2000)*/ { if (target.b < 255) target.b ++; }
            pset(bx, by, target);
          }
        }
      }
    }
    pset(0, y, White);
    flip; events;
  }
  while (true) { flip; events; }
}
