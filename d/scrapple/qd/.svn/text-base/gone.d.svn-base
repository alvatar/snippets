import qd;
import std.stdio, std.math, std.stdio: format;
import tools.base;

bool even(int i) { return (i%2)==0; }
bool odd(int i) { return (i%2)!=0; }

bool has(T)(T[] array, T what) {
  foreach (elem; array) if (elem==what) return true;
  return false;
}

size_t count(T)(T[] array, T what) {
  size_t res; foreach (elem; array) if (elem==what) res++;
  return res;
}

void swap(T)(ref T a, ref T b) { T c=a; a=b; b=c; }

void merge(T)(ref T[] a, T[] b) { foreach (elem; b) if (!a.has(elem)) a~=elem; }

template Tuple(T...) { alias T Tuple; }

void swipe(T)(ref T target, T replacement, void delegate() dg) {
  auto backup=target;
  target=replacement;
  scope(exit) target=backup;
  dg();
}

class SuperGoBoard(C, C free) {
  private C[] fields;
  C turn;
  this(C start) { turn=start; }
  struct connects {
    int[2] fields;
    bool opIn_r(int id) {
      foreach (field; fields) if (field==id) return true;
      return false;
    }
    static connects opCall(int a, int b) { connects res=void; res.fields[]=[a, b]; return res; }
  }
  private connects[] conns;
  private {
    bool connected(int id1, int id2) {
      foreach (conn; conns) if ((id1 in conn) && (id2 in conn)) return true;
      return false;
    }
    void checkRange(int id) {
      if (id<0 || id!<fields.length) throw new Exception("ID out of range");
    }
  }
  SuperGoBoard deepdup() {
    auto res=new SuperGoBoard(turn);
    foreach (id, value; this.tupleof)
      static if (is(typeof(value.dup))) res.tupleof[id]=value.dup;
      else res.tupleof[id]=value;
    res.conns=conns.dup;
    return res;
  }
  C opIndex(int id) {
    checkRange(id);
    return fields[id];
  }
  void opIndexAssign(C col, int id) {
    checkRange(id);
    fields[id]=col;
  }
  int[] adj(int id) {
    int[] res; for (int i=0; i<fields.length; ++i) if ((i!=id) && connected(i, id)) res~=i;
    return res;
  }
  private void tryRemove(C exempt=free) {
    auto processed=new bool[fields.length];
    size_t current=0;
    while (current<fields.length) {
      while (current<fields.length && (processed[current]||fields[current]==free||fields[current]==exempt)) current++;
      if (current==fields.length) break;
      auto curGrp=group(current); scope(exit) foreach (entry; curGrp) processed[entry]=true;
      if (!freedoms(curGrp)) foreach (entry; curGrp) fields[entry]=free;
    }
  }
  void rule() {
    tryRemove(turn);
    tryRemove;
  }
  int[] adj(int[] ids) {
    int[] res;
    foreach (id; ids) res.merge(adj(id));
    return res;
  }
  void connect(int a, int b) {
    if (connected(a, b)) throw new Exception(format("Can't connect (", a, ", ", b, ") twice!"));
    conns~=connects(a, b);
  }
  int[] group(int id) {
    int[] res;
    auto col=fields[id];
    void recurse(int id) {
      res~=id;
      foreach (next; adj(id)) if ((fields[next]==col) && !res.has(next)) recurse(next);
    }
    recurse(id);
    return res;
  }
  int freedoms(int[] ids) {
    int res;
    foreach (id; adj(ids)) if (fields[id]==free) res++;
    return res;
  }
  bool canSet(int id, C col) {
    checkRange(id);
    if (fields[id]!=free) return false;
    foreach (ac; adj(id)) if (fields[ac]==free) return true;
    auto test=deepdup();
    test.fields[id]=col;
    test.rule;
    return test.fields[id]==col;
  }
  int opCatAssign(C c) { fields~=c; return fields.length-1; }
}

enum Color { None, Black, White }

struct AnonStruct(string s) {
  mixin(s);
  static AnonStruct opCall(typeof(AnonStruct.tupleof) t) {
    AnonStruct res=void;
    foreach (id, value; t) res.tupleof[id]=value;
    return res;
  }
}

void drawDigit(int px, int py, ubyte digit) {
  void draw(bool a, bool b, bool c, bool d, bool e, bool f, bool g) {
    if (a) line(px-2, py-5, px+2, py-5);
    if (b) line(px-2, py-5, px-2, py);
    if (c) line(px+2, py-5, px+2, py);
    if (d) line(px-2, py, px+2, py);
    if (e) line(px-2, py, px-2, py+5);
    if (f) line(px+2, py, px+2, py+5);
    if (g) line(px+2, py+5, px-2, py+5);
  }
  const y=true;
  const n=false;
  switch (digit) {
    case 0: draw(y, y, y, n, y, y, y); break;
    case 1: draw(n, n, y, n, n, y, n); break;
    case 2: draw(y, n, y, y, y, n, y); break;
    case 3: draw(y, n, y, y, n, y, y); break;
    case 4: draw(n, y, y, y, n, y, n); break;
    case 5: draw(y, y, n, y, n, y, y); break;
    case 6: draw(y, y, n, y, y, y, y); break;
    case 7: draw(y, y, y, n, n, y, n); break;
    case 8: draw(y, y, y, y, y, y, y); break;
    case 9: draw(y, y, y, y, n, y, y); break;
    default: break;
  }
}

void drawNumber(int x, int y, int num) {
  const width=7;
  ubyte[] digits; do { digits~=num%10; num/=10; } while(num);
  foreach (id, digit; digits) drawDigit(-id*width+x+(digits.length-1)*width/2, y, digit);
}

void main() {
  int size=3;
  screen(640, 480);
  flip=false;
  alias AnonStruct!("int x, y; ") vec2;
  vec2[int] position;
  auto board=new SuperGoBoard!(Color, Color.None)(Color.White);
  int[int][int] columns;
  const offs=0.5;
  int width=cast(int)(screen.width/(size*2+1+offs));
  int height=cast(int)(screen.height/(size*4+3));
  for (int i=0; i<=size; i++) {
    for (int dir=-1; dir<=1; dir+=2) {
      auto cid=dir*(i+1);
      writefln(even(i)?1:0);
      for (int j=-size*2-1+i; j<=size*2+1-i; ++j) {
        writefln("j = ", j);
        //int offset=(screen.width/8)/(size+1);
        int offset=cast(int)((width*offs)/4);
        if (even(i) != even(j)) offset=-offset;
        //if (odd(i)) offset=-offset;
        auto newPos=board~=Color.None;
        columns[cid][j]=newPos;
        bool function(int) cond;
        if (even(i)) cond=&odd;
        else cond=&even;
        if (i) {
          if (cond(j)) board.connect(newPos, columns[dir*i][j]);
          if (cond(j) && (j + 2) in columns[dir*i])
            board.connect(newPos, columns[dir*i][j+2]);
          // connect outsides
          /*if (j==i-size-1) board.connect(newPos, columns[dir*i][j-1]);
          if (j==size-i+1) board.connect(newPos, columns[dir*i][j+1]);*/
        }
        position[newPos]=vec2(screen.width/2+dir*(width/2+width*i+offset), screen.height/2+j*height);
      }
      writefln(columns[cid]);
      foreach (id, entry; columns[cid]) if (id+1 in columns[cid]) board.connect(entry, columns[cid][id+1]);
    }
  }
  bool function(int) cond;
  foreach (id, entry; columns[1]) if (odd(id)) board.connect(entry, columns[-1][id]);
  while(true) {
    void delegate() click=null;
    circle(20, 20, 10, Fill=(board.turn==Color.White)?White:Black, Red);
    foreach (id, conn; board.conns) {
      auto p1=position[conn.fields[0]];
      auto p2=position[conn.fields[1]];
      line(p1.x, p1.y, p2.x, p2.y);
    }
    foreach (id, color; board.fields) with (position[id]) {
      auto fill=([Color.None: Blue, Color.Black: Black, Color.White: White][color]);
      auto overlay=fill;
      if (distance(x, y, mouse.x, mouse.y)<10) {
        if (!board.canSet(id, board.turn)) overlay=Red;
        else {
          overlay=(board.turn==Color.Black?Black:White);
          click=stuple(id, board) /apply/ (int id, typeof(board) board) {
            with (board) if (canSet(id, turn)) {
              board[id]=turn;
              rule;
              turn=(turn==Color.White)?Color.Black:Color.White;
            }
          };
        }
      }
      fill=fill~(fill~overlay);
      circle(x, y, 10, Red, Fill=fill);
      if (board[id]) drawNumber(x, y, board.freedoms(board.group(id)));
      // drawNumber(x, y, id);
    }
    
    flip; events;
    if (key.pressed(27)) return;
    if (mouse.clicked(Button.Left) && click) click();
  }
}
