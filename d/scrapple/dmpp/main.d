module main;
version(freetype) import SDL_ttf;
import std.math, std.string, std.stdio;
import tools.mersenne, tools.base;
import qd, SDL_ttf;

alias Mersenne Random;

int SDL_SaveBMP(SDL_Surface *surface, char []file) {
  return SDL_SaveBMP_RW(surface, SDL_RWFromFile(toStringz(file), "wb\0".ptr), 1);
}

struct rect(T, U) {
  T x, y;
  U w, h;
  string toString() { return format("rect (", x, ", ", y, ")[", w, ", ", h, "]"); }
}

bool maybe() { return ((rand&1)==0); }
T max(T)(T a, T b) { return (a>b?a:b); }

struct arguments {
  string[string] args;
  bool has(string s) { return (s in args)!=null; }
  string opIndex(string id) { return args[id]; }
  string need(string what) {
    if (!(what in args)) throw new Exception("Required parameter "~what~" not provided");
    return args[what];
  }
  void opCall(string[] args) {
    string last;
    void push(string what) { if (!last.length) return; this.args[last]=what; last=""; }
    foreach (arg; args) {
      if (!arg.length) continue;
      if (arg[0]=='-') { push(""); last=arg[1..$]; }
      else push(arg);
    }
    push("");
  }
}

struct point(T) { T x, y; }
struct pair(T) { T a, b; }

struct converter(T, U) {
  private rect!(T, T) origin;
  private rect!(U, U) target;
  private point!(real) ratio;
  void opCall(rect!(T,T) start, rect!(U,U) end) {
    origin=start; target=end;
    ratio.x=(target.w*1.0)/(origin.w*1.0);
    ratio.y=(target.h*1.0)/(origin.h*1.0);
  }
  complex!(U) toFn(point!(T) p) {
    complex!(U) np = void;
    np.re=cast(U)((p.x-origin.x)*ratio.x+target.x);
    np.im=cast(U)((p.y-origin.y)*ratio.y+target.y);
    return np;
  }
  point!(T) undo(complex!(U) p) {
    point!(T) up = void; /// undone point
    up.x=cast(T)((p.re-target.x)/ratio.x+origin.x);
    up.y=cast(T)((p.im-target.y)/ratio.y+origin.y);
    return up;
  }
  bool isIn(complex!(U) c) {
    static bool between(U v, U b, U t) { return ((v<b)&&(v>t)) || ((v<t)&&(v>b)); }
    return (between(c.re, target.x, target.x+target.w) && between(c.im, target.y, target.y+target.h));
  }
}

struct complex(T) {
  T re, im;
  static complex!(T) opCall(T re, T im) { complex!(T) result = void; result.re = re; result.im = im; return result; }
  complex!(T) opMul(complex!(T) b) {
    complex result = void;
    result.re=cast(T)(re*b.re-im*b.im);
    result.im=cast(T)(re*b.im+im*b.re);
    return result;
  }
  complex!(T) opAdd(complex!(T) b) {
    complex!(T) result = void;
    result.re=cast(T)(re+b.re); result.im=cast(T)(im+b.im);
    return result;
  }
  complex!(T) opDiv(complex!(T) b) {
    complex!(T) result = void;
    T sqs=cast(T)(b.re*b.re+b.im*b.im);
    result.re=cast(T)((re*b.re+im*b.im)/sqs); result.im=cast(T)((im*b.re-re*b.im)/sqs);
    return result;
  }
  void opAddAssign(complex!(T) b) {
    re=cast(T)(re+b.re); im=cast(T)(im+b.im);
  }
  void opMulAssign(complex!(T) b) {
    T tre=cast(T)(re*b.re-im*b.im);
    im=cast(T)(im*b.re+re*b.im);
    re=tre;
  }
}

alias float myval;
alias complex!(myval) mycomplex;

void swap(T)(inout T a, inout T b) { T c=a; a=b; b=c; }

struct FuncWrapper {
  myval a, b, c, d, e, f;
  void function(inout myval re, inout myval im) fn;
  myval[3] col;
  public myval p;
  static FuncWrapper opCall(myval _a, myval _b, myval _c, myval _d, myval _e, myval _f, void function(inout myval re, inout myval im) _fn, myval[] _col, myval _p) {
    FuncWrapper fw; with(fw) {
      fw.a=_a; fw.b=_b; fw.c=_c; fw.d=_d; fw.e=_e; fw.f=_f;
      fw.fn=_fn;
      col[]=_col[];
      p=_p;
    }
    return fw;
  }
  static FuncWrapper opCall(void function(inout myval re, inout myval im) _fn, uint seed, int factor=1) {
    auto rnd=new Random; rnd.seed(seed);
    myval getval() {
      auto v=rnd();
      myval res=(((v*1.0)/typeof(v).max));
      /*res=res>0.5?res*res:-1.0-res*res;
      res*=1.5;
      return res;*/
      return (res-0.5)*2.0;
    }
    myval[3] col; foreach (inout c; col) c=(rnd()*1.0)/(1.0*typeof(rnd()).max);
    static void normalize(inout myval r, inout myval g, inout myval b, myval to) {
      myval factor=to/(r*0.299+g*0.587+b*0.114);
      r*=factor; g*=factor; b*=factor;
    }
    static void boost(inout myval r, inout myval g, inout myval b, myval by) {
      static void _boost(inout myval lowest, inout myval highest, myval factor) {
        lowest=lowest*factor;
        highest=1-(1-highest)*factor;
      }
      /// w as in _walue_
      static ubyte max(myval[3] w) { ubyte m=0; if (w[1]>w[0]) { m=1; if (w[2]>w[1]) m=2; } else if (w[2]>w[0]) m=2; return m; }
      static ubyte min(myval[3] w) { ubyte m=0; if (w[1]<w[0]) { m=1; if (w[2]<w[1]) m=2; } else if (w[2]<w[0]) m=2; return m; }
      auto high=max([r,g,b]); auto low=min([r,g,b]);
      _boost(*[&r,&g,&b][min([r,g,b])], *[&r,&g,&b][max([r,g,b])], by);
    }
    normalize(col[0], col[1], col[2], 1.0);
    boost(col[0], col[1], col[2], 0.5);
    if (factor==-1) factor=(rnd()%2)+10;
    auto res=opCall(getval, getval, getval, getval, getval, getval, _fn, col, factor);
    return res;
  }
  void opCall(ref mycomplex p) {
    myval nre=a*p.re+b*p.im+c;
    myval nim=d*p.re+e*p.im+f;
    fn(nre, nim);
    p.re=nre; p.im=nim;
  }
}

void function(inout myval re, inout myval im) fns[];
char[][] fnames;
myval rsq(myval re, myval im) { return re*re+im*im; }
/// This is actually faster than doing a switch. I benchmarked it.
void setupFns() {
  fnames~="00-Linear";
  fns~=function void(inout myval re, inout myval im) {};
  
  fnames~="01-Sinusoidal";
  fns~=function void(inout myval re, inout myval im) { re=sin(re); im=sin(im); };
  
  fnames~="02-Spherical";
  fns~=function void(inout myval re, inout myval im) { myval rs=re*re+im*im; re/=rs; im/=rs; };
  
  fnames~="03-Swirl";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re, im)); myval a=atan2(im, re); a+=r; re=r*cos(a); im=r*sin(a); };
  
  fnames~="04-Horseshoe";
  fns~=function void(inout myval re, inout myval im)
  {
    myval a=atan2(re, im);
    myval c1=sin(a); myval c2=cos(a);
    myval nre=c1*re - c2*im;
    im=c2*re + c1*im;
    re=nre;
  };
  
  fnames~="05-Polar";
  fns~=function void(inout myval re, inout myval im)
  { myval r=1.0/sqrt(rsq(re, im)); re=atan2(im, re)/PI; im=r-1.0; };
    
  fnames~="06-Handkerchief";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); re=r*sin(a+r); im=r*cos(a-r); };
  
  fnames~="07-Heart";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); a*=r; re=r*sin(a); im=-r*cos(a); };
  
  fnames~="08-Disc";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re, im)); myval a=atan2(im, re); a/=PI; r*=PI; re=a*sin(r); im=a*cos(r); };
  
  fnames~="09-Spiral";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); re=(cos(a)+sin(r))/r; im=(sin(a)-cos(r))/r; };
  
  fnames~="10-Hyperbolic";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); re=sin(a)/r; im=cos(a)*r; };
  
  fnames~="11-Diamond";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); re=sin(a)*cos(r); im=cos(a)*sin(r); };
  
  fnames~="12-Ex";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); re=r*pow(sin(a+r), 3); im=r*pow(cos(a-r), 3); };
  
  fnames~="13-Julia";
  fns~=function void(inout myval re, inout myval im)
  { myval r=sqrt(rsq(re,im)); myval a=atan2(im, re); r=sqrt(r); a/=2.0; re=r*cos(a); im=r*sin(a); };
  
  writefln("Set up ", fns.length, " functions.");
}

int lookup(FuncWrapper f) {
  foreach (i, fn; fns) if (fn is f.fn) return i;
  writefln("Lookup failed. Throwing SIGFPE.");
  int e=0; e=1/e; throw new Exception("Lookup failed");
}

void logfn (FuncWrapper f) {
  writefln("Function ", fnames[lookup(f)], ":\n  x = p.x * ", f.a, " + p.y * ", f.b, " + ", f.c,
    ";\n  y = p.x * ", f.d, " + p.y * ", f.e, " + ", f.f, ";"c);
  writefln("Color: R", f.col[0], " G", f.col[1], " B", f.col[2]);
}

void logfn(FuncWrapper[] fs) {
  foreach (fw; fs) logfn(fw);
}

import std.c.stdlib:malloc, free;
final class fractal {
  alias rect!(myval, myval) myrect;
  static struct pixel { myval[3] col; }
  pixel[] buffer; int w, h;
  FuncWrapper[] fws;
  myrect range;
  converter!(ushort, myval) conv;
  ulong itercount;
  void cleanup() {
    foreach (inout col; buffer) with(col) { foreach (inout c; col) c=0; }
    conv(rect!(ushort, ushort)(cast(ushort)0, cast(ushort)0, cast(ushort)w, cast(ushort)h), range); // Init
    itercount=0;
  }
  this(int _w, int _h, FuncWrapper[] _fws, myrect _range) {
    w=_w; h=_h;
    buffer=(cast(pixel*)malloc(w*h*pixel.sizeof))[0..w*h];
    fws=_fws; range=_range;
    logfn(fws);
    cleanup;
  }
  ~this() { free(buffer.ptr); buffer=null; }
  bool addpixel(mycomplex cp, myval[3] pixcol) {
    if (!conv.isIn(cp)) return false;
    auto pp=conv.undo(cp);
    auto index=pp.x*h+pp.y;
    // if ((index<0)||(index>=buffer.length)) *(cast(int*)null)=0;
    for (int i = 0; i < 3; ++i) buffer[index].col[i] += pixcol[i];
    return true;
  }
  void iterate(ulong count) {
    /// Prescale the probability values to 0..maxint range
    if (!fws.length) return;
    ushort[] probability; ushort running_max=0;
    float psum=0.0; foreach (fw; fws) psum+=fw.p;
    // any rationale for this? float rndscale=(ushort.max*0.5)/psum;
    float rndscale=ushort.max/psum;
    ushort probsum=0;
    foreach (fw; fws) {
      running_max+=cast(ushort)(fw.p*rndscale);
      probability~=running_max; probsum+=running_max;
    }
    assert(probability[$-1] > ushort.max * 0.9); // guard against overflow
    mycomplex n;
    void resetpoint() {
      //n.re=0; n.im=0;
      n.re=((rand()*1.0)/typeof(rand()).max)*2.0-1.0;
      n.im=((rand()*1.0)/typeof(rand()).max)*2.0-1.0;
      //for (int i=0; i<20; ++i) fws[rnd.next%fws.length](n);
    }
    resetpoint;
    myval[3] col; foreach (inout c; col) c=0;
    /// Initialize the point
    int e=0;
    while (count) {
      /// 500000 is close to 524.288, which is 2^19
      if (!(count&524287)) writefln(count, "...");
      auto r = rand(), prerandom = cast(ushort) r; r >>= 16;
      // things are done twice to make full use of the uint
      if (prerandom >= probability[$-1]) continue; // highly unlikely
      int choice=0; while (probability[choice]<prerandom) choice++;
      auto func = fws[choice]; func(n);
      for (int i = 0; i < 3; ++i) col[i] = (col[i] + func.col[i]) / 2f;
      if (addpixel(n, col)) itercount++;
      prerandom = cast(ushort) r;
      if (prerandom >= probability[$-1]) continue; // still highly unlikely
      choice=0; while (probability[choice]<prerandom) choice++;
      func = fws[choice]; func(n);
      for (int i = 0; i < 3; ++i) col[i] = (col[i] + func.col[i]) / 2f;
      if (addpixel(n, col)) itercount++;
      count -= 2;
    }
  }
  Area render(config c, Area target=null) {
    auto dg={
      myval ipp=(itercount*1.0)/((buffer.length)*1.0); // iterations per pixel
      size_t i=0;
      for (ushort x=0; x<w; x+=c.dispscale) { if (!(x&255)) writefln("Column ", x);
        for (ushort y=0; y<h; y+=c.dispscale) {
          //myval tf(myval v) { return pow(log(v*c.brightness+1.0), c.gamma); } // Transform function
          myval tf(myval v) { return pow(log(v+1) / log(sqrt(2f))*c.brightness, c.gamma); } // Transform function
          myval factor=1.0/ipp; // Scaling factor
          static ubyte cap(uint v, ubyte c) { if (v>c) return c; return cast(ubyte)v; } // Self-explanatory
          ubyte[3] fcol; // "f" colors
          foreach (ix, cv; buffer[i].col) fcol[ix]=cap(cast(uint)(tf(cv*factor)*/*bness**/256.0), 255);
          auto col=rgb(fcol);
          pset(x/c.dispscale, y/c.dispscale, (c.invert?col.invert:col));
          i+=c.dispscale;
        }
        i+=h*(c.dispscale-1);
      }
    };
    if (target) screen.With(target) = dg();
    else screen.With(c.res[0]/c.dispscale, c.res[1]/c.dispscale) = { dg(); target = display; };
    return target;
  }
}

FuncWrapper[] seedgen(uint seed, char[] argstr) {
  FuncWrapper[] fws;
  auto rand=new Random; rand.seed(seed);
  auto amount=rand()%6;
  if (argstr.length) {
    foreach (fract; split(argstr, ",")) {
      while (fract[0]==' ') fract=fract[1..$]; while (fract[$-1]==' ') fract=fract[0..$-1];
      auto params=split(fract, " "); assert(params.length==7, "Must be seven params to fractal");
      float[6] coeffs; foreach (i, str; params[1..$]) coeffs[i]=atof(str);
      auto fn=FuncWrapper(fns[cast(uint)atoi(params[0])], rand());
      with (fn) { a=coeffs[0]; b=coeffs[1]; c=coeffs[2]; d=coeffs[3]; e=coeffs[4]; f=coeffs[5]; }
      fws~=fn;
    }
  } else for (int i=0; i<amount; ++i) fws~=FuncWrapper(fns[rand()%fns.length], rand());
  
  return fws;
}

char[] toHex(uint val) {
  static char[] hex="0123456789ABCDEF";
  char[] res; while (val>0) { res=hex[val%16]~res; val>>=4; }
  return res;
}

float distance(float a, float b) { return abs(a-b); }
float distance(FuncWrapper[] fa, FuncWrapper[] fb) {
  if (fa.length!=fb.length) return float.max;
  int same=0;
  auto maps=new int[fa.length];
  foreach (i, entry; fa) {
    bool hasMatch=false;
    foreach (id, f; fb) if (entry.fn is f.fn) {
      hasMatch=true;
      maps[i]=id;
    }
    if (hasMatch) same++;
  }
  if (same!=fa.length) return float.max;
  float res=0f;
  foreach (i, fn; fa) with (fn) foreach (id, value; [a, b, c, d, e, f]) with (fb[maps[i]]) {
    auto d=distance(value, [a, b, c, d, e, f][id]);
    res+=d/fa.length;
  }
  return res;
}

T min(T)(T a, T b) { if (a>b) return b; return a; }

Area CreateMosaic(config c, ushort xcount, ushort ycount, int ft, float threshold=2.2) {
  auto mosaic=Area(SDL_CreateRGBSurface(SDL_SWSURFACE, c.res[0]*xcount, c.res[1]*ycount, 32));
  version(freetype) auto font=new TTF_FontClass("ATFAntique-Regular.ttf", 10);
  auto range=rect!(myval, myval)(-1f, -1f, 2f, 2f);
  uint i=0;
  auto comparison=seedgen(ft, "");
  for (auto y=0; y<ycount; ++y) for (auto x=0; x<xcount; ++x) {
    writefln("X ", x, ", Y ", y);
    int curseed=void;
    FuncWrapper[] considered;
    writefln("Picking similar function set");
    float dist;
    do {
      curseed=c.seed+(i++);
      writefln(curseed);
      considered=seedgen(curseed, "");
      dist=distance(comparison, considered);
      if (dist<float.max) {
        dist/=considered.length;
        writefln("Distance: ", dist, " at ", curseed);
      }
    } while ((ft!=-1)&&(dist>threshold));
    writefln("Match found");
    auto curfract=new fractal(c.res[0], c.res[1], seedgen(curseed, ""), range);
    curfract.iterate(c.iter);
    auto curpiece=curfract.render(c);
    version(nofreetype) { }
    else {
      SDL_Color black; with(black) { r=0; g=0; b=0; }
      SDL_Color white; with(white) { r=g=b=255; }
      print(curpiece, Top|Left, Format("::", curseed));
    }
    mosaic.select(pt(x*c.res[0], y*c.res[1]), curpiece.size, Bottom|Right).blit(curpiece);
    SDL_FreeSurface(curpiece.surface);
    delete curfract;
  }
  return mosaic;
}

int info() {
  writefln(r"<D makes pretty pictures> by downs.
dmpp implements the fractal flame algorithm.
See also en.wikipedia.org/wiki/Fractal_Flame

You can use the following flags:
    -mode <0..2>
      0: interactive mode, supports zooming. This is the default.
      1: mosaic mode, generates a mosaic of fractals starting from a seed.
      2: batch mode
    -seed <integer>
      Initializes the PRNG. Defaults to 0.
    -o <filename>
      File to dump the image in.
    -size x:y
      The size of the target image. In mosaic mode, the size of a single part.
    -dscale <integer>
      The factor by which we downscale the image for display.
      Only relevant for interactive mode.
    -mosaic xcount:ycount
      Only relevant for -mode=1.
      Specifies the width and breadth of the mosaic in images.
    -iters <integer>
      How many iterations to calculate in batch mode before exiting.
    -invert
      Whether to invert the resulting image.
    -gamma <float>
      The gamma value. Controls the rendering function. Default is 1.
    -brightness <float>
      The brightness value. See gamma. Default is 0.1.
");
  return 0;
}

void MissingArgument(char[] c) { throw new Error("Missing argument: "~c); }

U[] forall(T, U)(T[] src, U function(T) fn) {
  U[] res; res.length=src.length;
  foreach (idx, inout elem; src) res[idx]=fn(elem);
  return res;
}

/// ascii to integer type
static T atoit(T)(char[] c) { return cast(T)atoi(c); }

int mosaic(config c) {
  char[] file="mosaic.bmp"; if (c.args.has("o")) file=c.args["o"];
  auto mosaic=forall(split(c.args.need("mosaic"), ":"), &atoit!(ushort)); assert(mosaic.length==2);
  int match=-1; if (c.args.has("similar")) match=cast(int)atoi(c.args["similar"]);
  SDL_SaveBMP(CreateMosaic(c, mosaic[0], mosaic[1], match).surface, file);
  return 0;
}

struct config {
  uint seed=0;
  int dispscale=1; int[] res; bool invert; float gamma=1.0; float brightness=0.1;
  FuncWrapper[] fns; fractal fract; rect!(myval, myval) range;
  ulong iter=100000;
  char[] file;
  arguments args;
  static config opCall(arguments arg) {
    config c; with (c) {
      range=rect!(myval, myval)(-1, -1, 2, 2);
      if (arg.has("dscale")) dispscale=cast(int)atoi(arg["dscale"]);
      if (arg.has("iters")) iter=cast(ulong)atoi(arg["iters"]);
      if (arg.has("gamma")) gamma=atof(arg["gamma"]);
      if (arg.has("brightness")) brightness=atof(arg["brightness"]);
      seed=cast(uint)atoi(arg.need("seed"));
      res=forall(split(arg.need("size"), ":"), &atoit!(int)); assert(res.length==2);
      file="screen.bmp"; if (arg.has("o")) file=arg["o"];
      invert=arg.has("invert");
      char[] pattern; if (arg.has("pattern")) pattern=arg["pattern"];
      fract=new fractal(res[0], res[1], seedgen(seed, pattern), range);
      args=arg;
    } return c;
  }
}

extern(C) {
  const SDLK_KP_DIVIDE=267, SDLK_KP_MULTIPLY=268,
    SDLK_KP_MINUS=269, SDLK_KP_PLUS=270;
}

int interactive(config c) {
  writefln(r"Switching to interactive mode
Press z to zoom in, or x to zoom out.
  Then drag to define the new rect.
Press i to invert.
NP PLUS/MINUS control gamma.
NP MULTIPLY/DIVIDE control brightness.
Press RMB to switch between display/calculating");
  screen(c.fract.w/c.dispscale, c.fract.h/c.dispscale);
  void flush() {
    print(0, 24, Bottom|Right, "Rendering");
    flip;
    c.fract.render(c, display);
  }
  bool running=true;
  mycomplex p1; mycomplex p2; /// rect to zoom into
  bool doCalc=true;
  enum zoom { inwards, outwards, none } zoom Zoom=zoom.none;
  while (running) {
    if (doCalc) {
      c.fract.iterate(c.iter);
      cls;
      print(0, 0, Bottom|Right, Format(c.fract.itercount, " iterations"));
    }
    flip;
        //case SDL_EventType.SDL_QUIT: running=false; break;
    // events((int key, bool pressed) {
    events;
    if (key.pressed('s')) {
      scope fc=c; fc.dispscale=1;
      with (fc) {
        auto full=fract.render(fc);
        SDL_SaveBMP(full.surface, file);
        SDL_FreeSurface(full.surface);
      }
    }
    if (key.pressed('x')) Zoom = zoom.outwards;
    if (key.pressed('z')) Zoom = zoom.inwards;
    if (key.pressed(SDLK_KP_PLUS)) { c.gamma*=0.75; writefln("Gamma is now ", c.gamma); flush; }
    if (key.pressed(SDLK_KP_MINUS)) { c.gamma/=0.75; writefln("Gamma is now ", c.gamma); flush; }
    if (key.pressed(SDLK_KP_MULTIPLY)) { c.brightness*=0.75; writefln("Brightness is now ", c.brightness); flush; }
    if (key.pressed(SDLK_KP_DIVIDE)) { c.brightness/=0.75; writefln("Brightness is now ", c.brightness); flush; }
    if (key.pressed('i')) { c.invert=!c.invert; flush; }
    if (mouse.clicked(Button.Right)) {
      if (doCalc) { doCalc=false; flush; }
      else {
        doCalc=true;
        print(0, 0, Bottom|Right, "Calculating .. ");
        flip;
      }
    } else if (mouse.clicked) {
      if (Zoom != zoom.none) {
        writefln("Click! ", mouse.pos);
        auto click=point!(ushort)(cast(ushort)(mouse.x*c.dispscale), cast(ushort)(mouse.y*c.dispscale));
        p1=c.fract.conv.toFn(click);
      }
    } else if (mouse.released) {
      if (Zoom != zoom.none) {
        auto x = mouse.x, y = mouse.y;
        writefln("Up! ", x, "-", y);
        auto up=point!(ushort)(cast(ushort)(x*c.dispscale), y=cast(ushort)(y*c.dispscale));
        p2=c.fract.conv.toFn(up);
        if (Zoom==zoom.inwards) {
          with (c.fract.range) { x=p1.re; y=p1.im; w=p2.re-p1.re; h=p2.im-p1.im; }
        }
        if (Zoom==zoom.outwards) {
          with (c.fract.range) {
            x-=w*((p1.re-x)/(p2.re-p1.re));
            y-=h*((p1.im-y)/(p2.im-p1.im));
            w*=w/(p2.re-p1.re);
            h*=h/(p2.im-p1.im);
          }
        }
        writefln("New range: ", c.fract.range);
        Zoom=zoom.none;
        c.fract.cleanup;
        c.fract.iterate(c.iter);
        flush;
      }
    }
  }
  return 0;
}

int batch(config c) {
  ulong iters=0;
  c.fract.iterate(c.iter);
  auto surf=c.fract.render(c);
  SDL_SaveBMP(surf.surface, c.file);
  SDL_FreeSurface(surf.surface);
  return 0;
}

import std.file: write;
int bindump(config c) {
  with (c) {
    fract.iterate(c.iter);
    write(args.need("o"), (cast(ubyte*)fract.buffer.ptr)[0..fract.buffer.length*fractal.pixel.sizeof]);
  }
  return 0;
}

int main(char[][] args) {
  arguments arg; arg(args);
  if (arg.has("?")||arg.has("h")||arg.has("help")) return info;
  setupFns;
  auto seed=0;
  int mode=0; if (arg.has("mode")) mode=cast(int)atoi(arg["mode"]);
  if ((mode<0)||(mode>3)) throw new Error("Invalid mode");
  writefln("Running in ", ["interactive", "mosaic", "batch", "binary dump"][mode], " mode.");
  [&interactive, &mosaic, &batch, &bindump][mode](config(arg));
  return 0;
}
