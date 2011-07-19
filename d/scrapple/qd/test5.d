module test5;

import qd;
import std.math;

creal e_to_i_times(real x) { return cos(x) + 1.0i * sin(x); }

import std.stdio;
void main() {
  screen(640, 480);
  aa=false;
  for (float f=0; true; f+=1) {
    line(0, 0, 640, 480, Fill=Black);
    void transform(creal p, void delegate(int, int) dg) {
      int x=cast(int)((p.re*screen.width+screen.width)*0.5), y=cast(int)((p.im*screen.height+screen.height)*0.5);
      if (x<0 || y<0) return;
      if (x!<screen.width || y!<screen.width) return;
      dg(x, y);
    }
    creal prev;
    for (float i=0; i<640; i+=0.1) {
      float t=((i-320.0)/320.0)*6.0;
      float y=sin(t);
      auto p=y*e_to_i_times(-2.0*PI*f*t);
      if (!i) prev=p;
      else {
        int px, py;
        transform(prev, (int x, int y) { px=x; py=y; });
        transform(p, (int x, int y) { line(px, py, x, y, White); });
        prev=p;
      }
    }
    flip; events;
  }
  while (true) { flip; events; }
}
