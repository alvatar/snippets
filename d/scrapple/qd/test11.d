module test11;
import qd, SDL_ttf, tools.base, tools.log;

void main() {
  screen(640, 480);
  auto button = display.select(20, 20, 100, 40);
  void towards(ref int a, int b) { if (a<b) ++a; if (a>b) --a; }
  auto toggleColor = new bool /apply/ (ref bool b) { return White ~ [Red, Green][b = !b]; };
  rgb target = toggleColor();
  auto draw_button = new int /apply/ (ref int p) {
    towards(p, {
      if (mouse in button) {
        if (mouse.clicked) target = toggleColor();
        return 50;
      } else return 0;
    }());
    with (button) line(tl, br,
      Box=White, Fill=Black.blend(White, 0.1).blend(Black.blend(target, 0.9), p/50f)
    );
    print(button, Center, White, Format("Hello World"));
  };
  while (true) {
    draw_button(); flip; events;
  }
}
