module test6;

import qd, std.math, SDL_ttf;
import tools.base, std.stdio, std.stdio: format;

alias cdouble cfn;

void transform(T...)(T thing) {
  static if (is(T[1]: creal)) {
    int t1, t2;
    transform(thing[0], (int k, int l) { t1=k; t2=l; });
    transform(thing[1], (int k, int l) { foreach (th; thing[2..$]) th(t1, t2, k, l); });
  } else {
    auto x=cast(int)((thing[0].re*0.5*screen.width+screen.width)*0.5);
    auto y=cast(int)((thing[0].im*-0.5*screen.height+screen.height)*0.5);
    foreach (th; thing[1..$]) th(x, y);
  }
}
pt toScreen(creal c) {
  return pt(cast(int)((c.re*0.5*screen.width+screen.width)*0.5), cast(int)((c.im*0.5*screen.height+screen.height)*0.5));
}

struct _call(alias T) { typeof(T(TupleInit!(U))) opCall(U...)(U u) { return T(u); } }

template call(alias T) { _call!(T) call; }

void main() {
  // init("FreeSans.ttf");
  screen(640, 480);
  flip=false;
  float step=0.004;
  for (float i=0; true; i+=step) {
    if (i>2*PI) i-=2*PI;
    line(0, 0, 640, 480, Fill=Black);
    transform(0+0i, call!(pset), (int x, int y) { print(x, y, Bottom|Right, "a"); });
    transform(cos(i)+0i, call!(line), call!(line), (int x, int y) { print(x, y, Bottom|Right, "y"); });
    transform(cos(i)+sin(i)*1i, call!(line), (int x, int y) { print(x, y, Bottom|Right, "b"); });
    transform(0+0i, call!(line));
    transform(1+0i, call!(pset));
    for (float j=0; j<i; j+=step) transform(cos(j)+sin(j)*1i, call!(line));
    
    print(10, 400, Top|Right, format("sin a = ", cast(int)(sin(i)*100)/100.0));
    pset(10, 440); for (float j=0; j<i; j+=step) line(10+j*10, 440-sin(j)*40);
    
    print(330, 400, Top|Right, format("cos a = ", cast(int)(cos(i)*100)/100.0));
    pset(330, 400); for (float j=0; j<i; j+=step) line(330+j*10, 440-cos(j)*40);
    auto a = i*(180.0/PI), _a = a;
    if (_a > 180) _a -= 180;
    if (_a > 90) _a = 180 - _a;
    auto b = 90.0 - _a;
    print(10, 10, Bottom|Right, format("a = ", cast(int)(a*100)/100.0, "°"));
    
    print(10, 50, Bottom|Right, format("b = ", cast(int)(b*100)/100.0, "°"));
    print(10, 90, Bottom|Right, format("y = 90°"));
    flip; events;
  }
  while (true) { flip; events; }
}
