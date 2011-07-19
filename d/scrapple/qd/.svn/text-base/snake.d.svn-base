module snake;
import qd, tools.base, tools.mersenne;

struct pt { int x, y; }
struct Snake { pt[] fields; }

import tools.log;
class Field {
  enum State { empty, apple, snake }
  State[] board;
  int width, height;
  mixin This!("width, height; #board.length=width*height; #snake.fields ~= pt(width/2, height/2); ");
  State getState(pt p) { return board[p.y*width+p.x]; }
  void setState(pt p, State s) { board[p.y*width+p.x] = s; }
  void updateState() {
    foreach (ref entry; board) if (entry != State.apple) entry = State.empty;
    foreach (part; snake.fields) setState(part, State.snake);
  }
  enum Direction { up, left, down, right }
  Direction dir;
  Snake snake;
  bool outside(pt p) { return p.x < 0 || p.y < 0 || p.x !< width || p.y !< height; }
  void spawnApple() {
    pt rp;
    do { rp = pt(rand % width, rand % height); } while (getState(rp) != State.empty);
    setState(rp, State.apple);
  }
  void step() {
    scope(exit) updateState;
    pt next = snake.fields[$-1];
    [{next.y --; }, {next.x --; }, {next.y ++; }, {next.x ++; }][dir]();
    if (outside(next)) throw new Exception("You have left the board!");
    if (getState(next) == State.snake) throw new Exception("You have lost!");
    snake.fields ~= next;
    if (getState(next) != State.apple) with (snake) fields = fields[1 .. $];
    else spawnApple;
  }
  void draw() {
    line(10, 10, 10+width*16, 10+height*16, Box=White);
    for (int x = 0; x < width; ++x)
      for (int y = 0; y < height; ++y)
        switch (getState(pt(x, y))) {
          case State.apple: circle(10+x*16+8, 10+y*16+8, 7.5, Fill=Red); break;
          case State.snake: circle(10+x*16+8, 10+y*16+8, 7.5, Fill=Green); break;
          default: break;
        }
  }
}

import tools.time;
void main() {
  screen(640, 480);
  auto field = new Field(39, 29);
  for (int i = 0; i < 8; ++i) field.spawnApple;
  while (true) {
    cls;
    events;
    foreach (i, k; [SDLKey.Up, SDLKey.Left, SDLKey.Down, SDLKey.Right])
      if (key.pressed(k)) field.dir = cast(Field.Direction) i;
    field.step; field.draw;
    flip;
    sleep(0.3);
  }
}
