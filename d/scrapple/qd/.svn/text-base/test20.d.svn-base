module test20;

import qd, SDL_ttf, tools.base;

string NullStrip(string s, ulong fract_factor) {
  while (s.length && s[$-1] == '0') s = s[0 .. $-1];
  if (!fract_factor) s = s[0 .. $-1];
  return s;
}

void main() {
  screen(320, 240);
  initFont(24);
  const xrange = 4, yrange = 5;
  auto xsize = screen.w / xrange, ysize = screen.h / yrange;
  double number = double.epsilon;
  double delegate(double, double) op; double buffer = double.epsilon;
  bool finished; ulong fract_factor;
  proc onContinue;
  void eq() {
    if (op) {
      number = op(buffer, number);
      op = null;
    }
    finished = true;
    onContinue = stuple(onContinue, &fract_factor) /apply/ (proc p, ref ulong u) { u = 0; if (p) p(); };
  }
  void plus() { eq(); op = (double i, double k) { return i + k; }; }
  void minus() { eq(); op = (double i, double k) { return i - k; }; }
  void addDigit(int digit) {
    if (finished) {
      if (onContinue) onContinue();
      finished = false;
      buffer = number;
      number = 0;
    }
    if (fract_factor) {
      number = number + (digit * 1.0 / fract_factor);
      fract_factor *= 10;
    } else number = number * 10 + digit;
  }
  void dot() { fract_factor = 10; }
  void print_area(Area area, Align al, string str) {
    area = area.shrink(5);
    auto f = Fill = Black;
    if (mouse in area) f = Fill = Black.blend(White, 0.2);
    if (mouse.pressed(area)) f = Fill = Black.blend(Green, 0.6).blend(White, 0.5);
    line(area.tl, area.br, Box, f);
    area = area.shrink(5);
    print(area, al, str);
  }
  void print_square(int x, int y, string str, proc onClick, proc onPress) {
    auto dx = x * xsize, dy = y * ysize;
    auto area = display.select(dx, dy, xsize, ysize);
    print_area(area, Center, str);
    if (mouse.pressed(area) && onPress) onPress();
    if (mouse.clicked(area) && onClick) onClick();
  }
  void draw() {
    for (int x = 0; x < 3; ++x)
      for (int y = 0; y < 3; ++y)
        print_square(x, y+1, Format((2 - y) * 3 + x + 1), { addDigit((2 - y) * 3 + x + 1); }, null);
    print_square(0, 4, "=", &eq, null);
    print_square(1, 4, "0", { addDigit(0); }, null);
    print_square(2, 4, ".", &dot, null);
    print_square(3, 2, "+", &plus, null);
    print_square(3, 3, "-", &minus, null);
    auto field = display.select(0, 0, screen.width, ysize);
    print_area(field, Right, NullStrip(Format(number), fract_factor));
  }
  while (true) { cls; draw; flip; events; slowyield; }
}
