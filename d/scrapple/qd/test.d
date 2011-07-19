module test;
import qd, std.random;

import tools.base;
void delegate(proc) every(int i) {
  return stuple(i, 0) /apply/ (int i, ref int count, proc p) {
    count++;
    if (count == i) { count = 0; p(); }
  };
}

void main() {
  screen(640, 480);
  auto e = every(1024);
  while (true) {
    line(rand() % 640, rand() % 480, rand() % 640, rand() % 480, rgb.rand);
    e({
      flip;
      events;
    });
  }
}
