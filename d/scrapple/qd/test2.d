module test2;
import qd, SDL_ttf;
import tools.base, tools.functional, tools.stackthreads, tools.threadpool, std.string, std.file, tools.log: logln;
import std.c.stdlib: exit;

import std.random;
import gcc.builtins, std.math:trunc, log, pow, PI, atan;
real sqrtl(real r) { return __builtin_sqrtl(r); }
float sqrtf(float f) { return __builtin_sqrtf(f); }
double sqrt(double d) { return __builtin_sqrt(d); }

float randf() { return (rand()*1f)/(typeof(rand()).max*1f); }
real fract(real r) { if (r<0) return -r-trunc(-r); else return r-trunc(r); }

template Pow(string what, int count) {
  static if (!count) const string Pow="typeof("~what~").init";
  else static if (count==1) const string Pow=what;
  else const string Pow=what~"*"~Pow!(what, count-1);
}

real φ;
static this() { φ=(1.0+sqrt(5.0))/2.0; }

void delegate() idle;

class Primitives {
  void drawFrame(ref Area orig) {
    with (orig) screen.With(orig) = line(0, 0, (size-pt(1, 1)).tupleof, Box=White);
    orig=orig.shrink(1);
  }
  TTF_FontClass font;
  this(string fn) {
    New(font, read(fn), 16);
  }
}

Primitives primitives;

interface Widget {
  void draw();
  void area(Area); Area area();
}

class StandardWidget : Widget {
  protected Area myArea;
  abstract void draw();
  void area(Area target) { myArea=target; }
  Area area() { return myArea; }
  this(Area _area) { area=_area; }
  this() { }
}

class StackThreadSched {
  Coroutine[] threads;
  void opCatAssign(Coroutine c) { threads ~= c; }
  void opCall() { foreach (thread; threads) thread(); }
  bool runsMe() { foreach (thread; threads) if (thread.runsMe) return true; return false; }
}

StackThreadSched mouse_sched;
static this() { New(mouse_sched); }

import std.thread, tools.threads;
class FractalWidget(T=cfloat) : StandardWidget {
  static assert(is(T: creal));
  Coroutine drawthr;
  Threadpool pool;
  T from, to;
  int iterations;
  enum DrawState { Reset, Stop }
  bool cancel;
  Area temp;
  this() {
    New(drawthr, &render);
    iterations = 16384;
    New(pool, 4);
    idle=&drawthr.opCall;
    New(temp);
    temp.tl=pt(0, 0);
    from=-2-2i; to=2+2i;
    super();
  }
  void zoom(T nfrom, T nto) {
    from = nfrom; to = nto;
    cancel=true; while (cancel) drawthr(); New(drawthr, drawthr.dg);
    // auto backup = myArea; area = null; area = backup;
  }
  /*void handleMouse(proc yield) {
    enum State { start, inside, drag_zoomin, doZoomin, drag_zoomout, doZoomout };
    pt start, end;
    void delegate(ref State)[State] tf=[
      State.inside: (ref State s) { if (mouse.pressed) start=mouse.pos; },
      State.drag_zoomin: (ref State s) { if (!mouse.pressed) end=mouse.pos; },
      State.drag_zoomout: (ref State s) { if (!mouse.pressed) end=mouse.pos; },
      State.doZoomin: (ref State s) {
        T nfrom=myArea.locate(start, from, to), nto=myArea.locate(end, from, to);
        zoom(nfrom, nto);
        logln("New range: ", from, " -> ", to);
        s=State.start;
      },
      State.doZoomout: (ref State s) {
        T ifrom=myArea.locate(start, from, to), ito=myArea.locate(end, from, to);
        // determine transformation from new range to current range
        T scale=(to.re-from.re)/(ito.re-ifrom.re) + (to.im-from.im)/(ito.im-ifrom.im)*1i;
        T offs=(to.re+from.re-ito.re-ifrom.re)/2.0 + ((to.im+from.im-ito.im-ifrom.im)/2.0)*1i;
        offs=offs.re*scale.re + offs.im*scale.im*1i;
        // now apply transformation to current range
        T nfrom=(from.re+to.re - (to.re-from.re)*scale.re)/2.0 + ((from.im+to.im - (to.im-from.im)*scale.im)/2.0)*1i;
        T nto=(from.re+to.re + (to.re-from.re)*scale.re)/2.0 + ((from.im+to.im + (to.im-from.im)*scale.im)/2.0)*1i;
        zoom(nfrom+offs, nto+offs);
        s=State.start;
      }
    ];
    tf.addBidirectionalEdge({ return (mouse in area) && !mouse.pressed; }, { return !(mouse in area); }, State.start, State.inside);
    tf.addEdges(
      { return mouse.pressed(Button.Left); }, State.inside, State.drag_zoomin,
      { return mouse.pressed(Button.Right); }, State.inside, State.drag_zoomout,
      { return !mouse.pressed; }, State.drag_zoomin, State.doZoomin,
      { return !mouse.pressed; }, State.drag_zoomout, State.doZoomout
    );
    tf.runState(yield);
  }*/
  alias void delegate(T p, void delegate(float)) func;
  void lyapunov(T p, void delegate(float) dg) {
    p*=2; // prescale
    string str="AAAAAAAAAABBAABB";
    int limit=200;
    size_t which=0;
    float next_r() {
      auto ch=str[which++];
      if (which>=str.length) which=0;
      if (ch=='A') return p.im;
      if (ch=='B') return p.re;
      asm { int 3; }
    }
    float sum=0;
    int i=0;
    float x=0.5;
    for (i=0; i<limit; ++i) {
      float r=next_r;
      x=x*r*(1f-x);
      if (i>limit/2) sum+=log(abs(r*(1f-2f*x)));
      if (sum>=float.max) break; // definitely outside
    }
    float f=sum/(limit/2f);
    if (f>0) dg(1);
    else dg(pow(atan(-f)/PI, cast(real)0.3f));
  }
  void bailout_type(alias FN) (T p, void delegate(float) dg) {
    T c = 0+0i;
    static assert(is(ParameterTypeTuple!(FN) == Tuple!(T, T)));
    int i = iterations;
    for (; i>0; i--) {
      FN(c, p);
      if (c.re*c.re + c.im*c.im!<4) break;
    }
    if (i !> 0) dg(1); else {
      //auto smooth = (iterations - i + 1) - log(log(abs(pre_c)))/log(2);
      dg(i / 128f);
    }
  }
  void mandelbrot_sse(T[4] ps, void delegate(int, float) dg) {
    T[4] cs;
    foreach (ref c; cs) c = 0+0i;
    ubyte left = 15; // bit array
    int i = iterations;
    for (; i > 0; i--) {
      for (int k = 0; k < 4; ++k) {
        cs[k] *= cs[k]; cs[k] += ps[k];
      }
      for (int k = 0; k < 4; ++k) {
        if (left & (1 << k)) {
          auto c = cs[k];
          if (c.re*c.re + c.im * c.im !< 4) {
            left &= ~(1 << k);
            dg(k, i/128f);
          }
        }
      }
      if (!left) break;
    }
    if (!i) for (int k = 0; k < 4; ++k) if (left & (1 << k)) dg(k, 1);
  }
  final void mandelbrot_fn(ref T c, T p) { c *= c; c += p; }
  final void burning_ship_fn(ref T c, T p) {
    c = abs(c.re) + abs(c.im)*1.0i;
    c *= c; c += p;
  }
  alias bailout_type!(mandelbrot_fn) mandelbrot;
  alias bailout_type!(burning_ship_fn) burning_ship;
  void julia(T p, T c, void delegate(float) dg) {
    int limit=iterations;
    T x=p;
    int i=0;
    float sq=void;
    for (;i<limit; ++i) {
      x=x*x + c;
      sq=x.re*x.re+x.im*x.im;
      if (sq>=4) break;
    }
    dg((i-atan(sqrtf(sq)-2)/1.18)*0.1);
  }
  void render(proc yield) {
    logln("render proc - temp=", temp);
    assert(drawthr.runsMe());
    size_t aa=1;
    float scale = 255f / (aa*aa);
    size_t cc=0;
    int count=0;
    void renderRange(int y1, int y2) {
      scope(exit) synchronized(this) --count;
      auto size = (to.re-from.re) + (to.im-from.im)*1.0i;
      for (int yc=y1; yc<y2; ++yc) {
        for (int xc=0; xc<temp.width; xc += 4) {
          if (cc) synchronized { cc--; return; }
          temp.pset(pt(xc, yc), 255, 255, 255);
          //float red=0f, green=0f, blue=0f;
          float[4] red=[0f, 0f, 0f, 0f], green=[0f, 0f, 0f, 0f], blue=[0f, 0f, 0f, 0f];
          for (int xa=0; xa<aa; ++xa) for (int ya=0; ya<aa; ++ya) {
            T[4] start;
            for (int k = 0; k < 4; ++k) {
              auto mxc = xc + k;
              if (mxc !< temp.width) break;
              start[k]=(mxc*aa*1f + xa)/(temp.width*aa*1f) + (yc*aa*1f + ya)/(temp.height*aa*1f)*1i;
              start[k]=from.re+size.re*start[k].re - (from.im+size.im*start[k].im)*1.0i;
            }
            //lyapunov(start, (float t) {
            //mandelbrot(start, (float t) {
            mandelbrot_sse(start, (int k, float t) {
            //burning_ship(start, (float t) {
            //julia(start, -0.835-0.2321i, (float t) {
              green[k]+=fract(t*2); red[k]+=fract(t); blue[k]+=fract(t/2);
            });
          }
          for (int k = 0; k < 4; ++k) {
            if (k !< temp.width) break;
            red[k]*=scale; green[k]*=scale; blue[k]*=scale;
            temp.pset(pt(xc+k, yc), cast(ubyte) red[k], cast(ubyte) green[k], cast(ubyte) blue[k]);
          }
        }
      }
    }
    for (int y=0; y<temp.height; y+=16) {
      pool.addTask("Worker "~.toString(y), &renderRange /fix/ stuple(y, min(y+16, temp.height)));
      synchronized(this) ++count;
    }
    while (true) {
      yield();
      if (!count) if (temp.surface && filename.length) {
        SDL_SaveBMP(temp.surface, filename);
        //exit(0);
        filename = "";
      }
      if (cancel) {
        cc=count;
        while (cc) yield();
        cancel=false;
        yield();
      }
    }
  }
  Area area() { return super.area; }
  void area(Area target) {
    if (target && (!myArea || target.dimensions!=myArea.dimensions)) {
      temp = screen.With(target.width, target.height) = cls;
    }
    super.area=target;
  }
  void draw() {
    myArea.blit(temp);
  }
}

template UseWidget(alias W) {
  private bool valid() { static if(isPointer!(typeof(W))) return W&&*W; else return W !is null; }
  void draw() { if (valid) W.draw(); }
  void area(Area target) { if (valid) W.area=target; }
  Area area() { if (valid) return W.area; return null; }
}

class GroupWidget : StandardWidget {
  mixin This!("super(myArea)");
  Widget[] children;
  void opCatAssign(Widget[] w) { children~=w; }
  void opCatAssign(Widget w) { children~=w; }
}

class Draggable : Area {
  void delegate(ref pt)[] constraints;
  this(int x, int y, int w, int h) { tl=pt(x, y); dimensions=pt(w, h); }
  void set(Area area) {
    tl=area.tl;
    dimensions=area.dimensions;
    surface=area.surface;
  }
  void mousethr(proc yield) {
    enum State { start, inside, dragging }
    pt startpos, lastpos;
    void delegate(ref State)[State] transitions=[
      State.inside: (ref State s) { lastpos=startpos=mouse.pos; },
      State.dragging: (ref State s) {
        pt offset=mouse.pos-lastpos;
        lastpos=mouse.pos;
        tl+=offset;
        foreach (constraint; constraints) constraint(tl);
      }
    ];
    transitions.addBidirectionalEdge({ return mouse.pressed; }, State.inside, State.dragging);
    transitions.addEdges(
      { return (mouse.pos in this)&&!mouse.pressed; }, State.start, State.inside,
      { return !(mouse.pos in this); }, State.inside, State.start
    );
    transitions.runState(yield);
  }
}

class BaseSliderWidget : StandardWidget {
  float pos=0;
  invariant { assert(pos>=0f && pos<=1f); }
  void delegate(ref float) limit;
  Draggable box;
  void mousethr(proc yield) { box.mousethr({ update; yield(); }); }
  void update() { }
  void constrain(ref pt p) {
    p.y=myArea.tl.y+myArea.height/2;
  }
  void area(Area area) {
    super.area(area);
    auto new_box=area.width(cast(int)(area.width*(0.1+pos*0.9))).width(-area.width/10);
    if (!box) box=new Draggable(0, 0, 0, 0);
    box.set(new_box);
  }
  Area area() { return super.area(); }
  void draw() {
    with (area.select(pt(cast(int)(area.width*0.9), 3), Center)) line(tl.tupleof, br.tupleof, Box=White, Fill=Black);
    if (box) with (box) line(tl.tupleof, br.tupleof, Box=White, Fill=White~Black~Black);
  }
}

class SplitWidget(bool vert=true) : GroupWidget {
  int[] percentages;
  mixin This!("super(myArea)");
  invariant {
    assert(percentages.length==children.length);
    assert(!percentages.length || (percentages /Reduce!("$+=$2") == 100));
  }
  void opCatAssign(Widget[] w) { super~=w; percentages~=0; }
  void add(Widget w, int p) { super~=w; percentages~=p; }
  void area(Area target) {
    primitives.drawFrame(target);
    auto length=vert?target.height:target.length;
    super.area=target;
    if (!percentages.length) return;
    auto offsets=new int[percentages.length];
    offsets[0]=0;
    foreach (id, ref entry; offsets[1..$]) entry=(percentages[id]*length)/100+offsets[id];
    foreach (id, child; children) {
      static if (vert) child.area=target.select(0, offsets[id]).height((percentages[id]*length)/100);
      else child.area=target.select(offsets[id], 0).width((percentages[id]*.length)/100);
    }
  }
  void draw() {
    Area meep=target; primitives.drawFrame(meep);
    foreach (child; children) child.draw();
  }
}

class FixedSplitWidget(bool vert=true) : Widget {
  Widget head, child;
  int offset;
  mixin This!("head, child, offset");
  void draw() { head.draw; child.draw; }
  void area(Area target) {
    static if (vert) {
      head.area=target.height(offset);
      child.area=target.height(-target.height+offset+1);
    } else {
      head.area=target.width(offset);
      child.area=target.width(-target.width+offset+1);
    }
  }
  Area area() { return null; }
}

class Proxy : Widget {
  Widget *target;
  this(Widget *w) { target=w; }
  mixin UseWidget!(target);
}

class Frame : StandardWidget {
  Widget child; mixin This!("child=null");
  void draw() {
    Area feep=myArea;
    primitives.drawFrame(feep);
    if (child) {
      child.area=feep.shrink(1);
      child.draw;
    }
  }
}

class AlignWidget : StandardWidget {
  Widget target;
  Align ment;
  this(Widget w, Align a) { target=w; ment=a; }
  void draw() { target.draw; }
  void area(Area a) {
    super.area=a;
    if (target) target.area=a.select(target.area.size, ment); // retarget
  }
}

class StackWidget : Widget {
  Widget[] targets;
  mixin This!("targets");
  void draw() { foreach (target; targets) if (target) target.draw; }
  void area(Area a) { foreach (target; targets) if (target) target.area=a; }
  Area area() { return targets[0].area(); }
}

T[] array(T)(T[] elems...) { return elems.dup; }

import tools.list;
class WindowManager {

  class Window : Widget {
    private Widget frame, headline, box;
    Widget child;
    StackThreadSched motion;
    this(Area area=null, Widget ch=null) {
      headline=new TextBox("Window", Center|Left);
      box=new Frame(); box.area=Area(pt(0, 0), pt(10, 10));
      this.child=ch;
      frame=new Frame(new StackWidget(array!(Widget)(
        new FixedSplitWidget!()(new Frame(headline), new Proxy(&child), 32),
        new AlignWidget(box, Bottom|Right)
      )));
      New(motion);
      if (area) frame.area=area;
    }
    mixin UseWidget!(frame);
  }
  
  list!(Window) windows;
  this() { mouse_sched ~= new Coroutine(&mousethr); }
  void draw() { windows.each((Window w) { w.draw(); }); }
  void mousethr(proc yield) {
    assert(mouse_sched.runsMe);
    enum State { start, title_inside, title_dragging, resize_inside, resize_dragging }
    pt lastpos;
    bool mouseInTitle() { with (windows) return length && top.headline.area && (mouse.pos in top.headline.area); }
    bool mouseInBox() { with (windows) return length && top.box.area && (mouse.pos in top.box.area); }
    void delegate(ref State)[State] transitions=[
      State.start: (ref State s) {
        size_t newActive=size_t.max;
        windows.each_reverse((size_t id, ref Window w, proc brk) {
          if (mouse.pos in w.area) {
            if (mouse.pressed) {
              newActive=id; // make current
              if (s==State.start && (w is windows.top)) w.motion();
            } else w.motion();
            brk();
          }
        });
        if (newActive!=size_t.max && newActive!=windows.length-1) {
          windows.push_back(windows.take(newActive));
        }
      },
      State.title_inside: (ref State s) { lastpos=mouse.pos; },
      State.title_dragging: (ref State s) {
        with (windows.top) area=area.move(mouse.pos-lastpos);
        lastpos=mouse.pos;
      },
      State.resize_inside: (ref State s) { lastpos=mouse.pos; },
      State.resize_dragging: (ref State s) {
        auto ns=mouse.pos-lastpos;
        with (windows.top) area=area.select(0, 0, area.width+ns.x, area.height+ns.y);
        lastpos=mouse.pos;
      }
    ];
    transitions.addBidirectionalEdges(
      { return mouseInTitle && !mouse.pressed; }, { return !mouseInTitle; }, State.start, State.title_inside,
      { return mouseInBox && !mouse.pressed; }, { return !mouseInBox; }, State.start, State.resize_inside,
      { return mouse.pressed; }, State.title_inside, State.title_dragging,
      { return mouse.pressed; }, State.resize_inside, State.resize_dragging
    );
    transitions.runState(yield);
  }
}

class Text : StandardWidget {
  Align alignment;
  this(string s, Align alignment=Left|Top) { text=primitives.font.render(s); this.alignment=alignment; }
  Area text;
  void draw() { myArea.select(text.size, alignment).blit(text); }
}

class TextBox : Text {
  this(string s, Align alignment=Left|Top) { super(s, alignment); }
  void draw() { screen.With(myArea) = cls; super.draw(); }
}

import std.c.time: time;
void countFPS() {
  static int last_time;
  static int fps=0;
  auto now=time(null);
  if (last_time!=now) {
    logln(fps, " fps.");
    fps=0;
    last_time=now;
  } else ++fps;
}

real transfer(real a, real b, real a2, real b2, real t) {
  real fp=(a2-a)*(b-a)/(a2-a+b-b2);
  fp+=a;
  float fac1=(fp-a)/(fp-a2), fac2=(b-fp)/(b2-fp);
  assert(abs(fac1-fac2)<0.001, format("Inequality: ", fac1, " != ", fac2));
  auto fac=fac1;
  auto steps=log(fac)/log(2.0);
  auto scale=pow(cast(real)2.0, steps*t);
  return fp-(fp-a)/scale;
}
creal transfer(creal a, creal b, creal a2, creal b2, real t) {
  return transfer(a.re, b.re, a2.re, b2.re, t)+transfer(a.im, b.im, a2.im, b2.im, t)*1.0i;
}

string filename;

import tools.time: getTime=time;
void main(string[] args) {
  New(primitives, "Vera.ttf");
  auto wm=new WindowManager;
  auto f1=new FractalWidget!(creal);
  screen(700, 700, SDL_RESIZABLE);
  flip=off;
  auto w1=wm.new Window(display.select(10, 10, 260, 229), f1);
  wm.windows~=w1;
  //wm.windows~=wm.new Window(display.select(10, 10, 160, 60), new BaseSliderWidget);
  auto start_from = f1.from, start_to = f1.to;
  
  if (args.length>6) {
    creal nfrom=args[1].atof() + args[2].atof()*1.0i, nto=args[3].atof() + args[4].atof()*1.0i;
    //float step=args[5].atof()/args[6].atof();
    int length = args[5].atoi();
    void renderStep(int which) {
      real step = (1.0*which)/(1.0*length);
      auto nffrom=transfer(start_from, start_to, nfrom, nto, step);
      auto nfto=transfer(start_to, start_from, nto, nfrom, step);
      auto num = toString(which);
      auto limit = toString(length);
      while (num.length < limit.length) num = "0"~num;
      filename = args[6] ~ num ~ ".bmp";
      if (filename.exists()) { logln("Skipping ", filename); return; }
      f1.zoom(nffrom, nfto);
      logln("new from ", nffrom, " to ", nfto, " step being ", step);
      while (filename.length) { cls(Black); wm.draw(); flip; idle(); events; countFPS(); }
    }
    for (int i = 0; i < length; ++i) renderStep(i);
    return;
  }
  
  long idletime=20;
  while (true) {
    cls(Black);
    wm.draw();
    flip;
    idle();
    events;
    countFPS();
    mouse_sched ();
  }
}
