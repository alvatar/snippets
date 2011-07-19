module test15;
import qd, SDL_ttf, tools.base;

char[] delegate() atrocity(string s, bool start = true) {
  return stuple(s, start) /apply/ (string s, ref bool bph) {
    scope(exit) bph = !bph;
    char[] res = s.dup;
    foreach (i, ref ch; res) if (((i&1)==0) ^ bph) ch = ' ';
    return res;
  };
}

import tools.time;
R delegate() blink(R)(R delegate() src, float delay) {
  return stuple(sec(), src, src(), delay) /apply/ (ref double last, R delegate() src, ref R value, float delay) {
    auto s = sec();
    if (s - last > delay) { last = s; value = src(); }
    return value;
  };
}

proc every(proc p, int count) {
  return stuple(p, count, 0) /apply/ (proc p, int count, ref int i) {
    i++;
    if (i == count) { i = 0; p(); }
  };
}

void main() {
  screen(640, 480);
  initFont(12, "ProggyClean.ttf");
  auto text = blink(atrocity("f__"), .5f);
  auto text2 = blink(atrocity("This is an atrocity!", false), .5f);
  auto tf = every({ tintfill(0, 0, 640, 480, Black); }, 16);
  while (true) {
    tf();
    print(display.select(0, 40), Center, White, "foo bar foo   bar");
    print(display, Center, White, text());
    print(display.select(0, 20), Center, White, text2());
    flip; events;
  }
}
