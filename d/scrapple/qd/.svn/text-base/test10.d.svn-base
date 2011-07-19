module test10;
import std.stdio, qd;

extern(System) uint sleep(uint t);

void main() {
  screen(640, 480);
  bool[9] state = [false, false, false, false, true, false, false, false, false];
  int index(int x, int y) { return y*3+x; }
  bool inside(int x, int y) { return (x!<0 && y!<0 && x<3 && y<3); }
  void draw() {
    line(0, 0, 640, 480, Fill=White~Black~Black);
    for (int y=0; y<3; ++y) for (int x=0; x<3; ++x) {
      line(x*screen.w/3, y*screen.h/3, (x+1)*screen.w/3-1, (y+1)*screen.h/3-1,
        Fill=(state[y*3+x]?White:Black)
      );
    }
    flip;
  }
  void checkEnd() {
    if (state == [true, true, true, true, false, true, true, true, true]) {
      sleep(1);
      throw new Exception("Congratulations!");
    }
  }
  void turn(int x, int y) {
    auto dg = (int x, int y) {
      if (inside(x, y)) (ref bool b) { b = !b; } (state[index(x, y)]);
    };
    dg(x, y); dg(x+1, y); dg(x-1, y); dg(x, y+1); dg(x, y-1);
  }
  flip=false;
  while (true) {
    events((int key, bool pressed) { }, (int x, int y, ubyte button, int change) {
      if (button == 1 && change == 1) turn(3*x/screen.w, 3*y/screen.h);
    });
    draw;
    checkEnd;
  }
}
