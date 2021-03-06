module example;
import qd, std.c.time: sleep;

void main() {
  screen(640, 480);
  pset(10, 10);
  line(0, 0, 100, 100, Box, Back(Red~Black));
  for (int i=0; i<=100; i+=10) {
    line(i, 0, 100-i, 100);
    line(0, i, 100, 100-i);
  }
  circle(50, 50, 50, 15, White~Black, Fill=White~Black);
  paint(200, 200, Red, Back=White);
  circle(100, 100, 50, 15, White);
  paint(200, 200, Black);
  pset(10, 11); pset(10, 11, Black);
  pset(10, 10);
  while (true) { flip; events((ushort a, bool b) { }, (int a, int b) { }); }
}
