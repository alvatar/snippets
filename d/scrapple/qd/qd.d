module qd;
import tools.base, tools.compat;
alias tools.base.min min;
alias tools.base.max max;

template ctype(string S) {
  static if (S == "unsigned long")
    alias uint ctype;
  else static if (S == "long")
    alias int ctype;
  else static assert(false, "Unsupported C type "~S);
}

import tools.base;
extern(C) {
  struct SDL_Rect {
    short x, y;
    ushort w, h;
  }
  struct SDL_RWops {
    int function(SDL_RWops *context, int offset, int whence) seek;
    int function(SDL_RWops *context, void *ptr, int size, int maxnum) read;
    int function(SDL_RWops *context, void *ptr, int size, int maxnum) write;
    int function(SDL_RWops *context) close;
    uint type;
    union hidden {
      struct stdio { int autoclose; void *fp; }
      struct mem { ubyte *base, here, stop; }
      struct unknown { void *data; }
    }
  } 
  struct SDL_Color {
    ubyte r, g, b, a;
  }
  struct SDL_PixelFormat {
    //SDL_Palette *palette;
    void *palette;
    ubyte BitsPerPixel, BytesPerPixel, Rloss, Gloss, Bloss, Aloss, Rshift, Gshift, Bshift, Ashift;
    uint Rmask, Gmask, Bmask, Amask, colorkey; ubyte alpha;
  }
  struct SDL_Surface {
    uint flags;
    SDL_PixelFormat *format;
    int w, h;
    ushort pitch;
    void *pixels;
    int offset;
    void *hwdata;
    SDL_Rect clip_rect;
    uint unused;
    uint locked;
    void *map;
    uint format_version;
    int refcount;
  }
  uint SDL_MapRGBA(SDL_PixelFormat *format, ubyte r, ubyte g, ubyte b, ubyte a);
  void SDL_GetRGBA(uint pixel, SDL_PixelFormat *fmt, ubyte *r, ubyte *g, ubyte *b, ubyte *a);
  void SDL_GetRGB(uint pixel, SDL_PixelFormat *fmt, ubyte *r, ubyte *g, ubyte *b);
  int SDL_LockSurface(SDL_Surface *);
  void SDL_UnlockSurface(SDL_Surface *);
  SDL_Surface * SDL_SetVideoMode(int width, int height, int bpp, uint flags);
  SDL_Surface *SDL_CreateRGBSurface(
    uint flags, int width, int height, int depth,
    uint Rmask=0xff, uint Gmask=0xff00, uint Bmask=0xff0000, uint Amask=/*0xff000000*/0);
  void SDL_FreeSurface(SDL_Surface *);
  int SDL_Flip(SDL_Surface *);
  void SDL_UpdateRect (SDL_Surface *screen, int x=0, int y=0, uint w=0, uint h=0);
  int SDL_UpperBlit(SDL_Surface *src, SDL_Rect *srcrect, SDL_Surface *dst, SDL_Rect *dstrect);
  alias SDL_UpperBlit SDL_BlitSurface;
  int SDL_SetAlpha(SDL_Surface *surface, uint flags, ubyte alpha);
  int SDL_SetColorKey(SDL_Surface *surface, uint flag, uint key);
  int SDL_FillRect(SDL_Surface *dst, SDL_Rect *dstrect, uint color);
  SDL_RWops *SDL_RWFromFile(char *file, char *mode);
  SDL_RWops *SDL_RWFromMem(void *mem, int size);
  char *SDL_GetError();
  const uint SDL_SWSURFACE=0, SDL_HWSURFACE=1, SDL_ASYNCBLIT=4;
  const uint SDL_ANYFORMAT=0x10000000, SDL_HWPALETTE=0x20000000, SDL_DOUBLEBUF=0x40000000, SDL_FULLSCREEN=0x80000000;
  const uint SDL_OPENGL=0x2, SDL_OPENGLBLIT=0xa, SDL_RESIZABLE=0x10, SDL_NOFRAME=0x20;
  const uint SDL_SRCALPHA=0x00010000, SDL_SRCCOLORKEY=0x00001000;
  void SDL_Delay(uint ms);
  uint SDL_GetTicks();
  ubyte SDL_EventState(ubyte type, int state);

  enum SDLKey {
    Unknown = 0, First = 0, Return = 13,
    Escape = 27,
    Delete = 127,
    LCtrl = 306,
    a = 97, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z = 122, Zero = 48, Nine = 57,
    Up = 273, Down = 274, Right = 275, Left = 276,
    Home = 278, End = 279, PageUp = 280, PageDown = 281,
    KP_Multiply = 268, KP_Divide = 267, KP_Plus = 270, KP_Minus = 269,
    Minus = 45, Plus = 43, Backspace = 8,
    F1 = 282, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12
  }
  enum SDLMod {
    None = 0, LShift = 1, RShift = 2, Shift = 3,
    LCtrl = 0x40, RCtrl = 0x80, Ctrl = 0xc0,
    LAlt = 0x100, RAlt = 0x200, Alt = 0x300,
    LMeta = 0x400, RMeta = 0x800,
    Num = 0x1000, Caps = 0x2000, Mode = 0x4000,
    Reserved = 0x8000
  };

  struct SDL_keysym { ubyte scancode; SDLKey sym; SDLMod mod; ushort unicode; }
  void SDL_EnableUNICODE(int enable=1);
  enum SDL_EventType : ubyte {
    NoEvent=0, Active, KeyDown, KeyUp,
    MouseMotion, MouseButtonDown, MouseButtonUp, JoyAxisMotion,
    JoyBallMotion, JoyHatMotion, JoyButtonDown, JoyButtonUp,
    Quit, SysWMEvent, ReservedA, ReservedB, Resize, Expose
  }
  version(win32) { }
  else {
    alias uint Window; alias uint Atom;
    char* XGetAtomName(void* display, Atom atom);
    Atom XInternAtom(void* display, char* name, bool only_if_exists = true);
    int XConvertSelection(void* display, Atom selection, Atom target, Atom property, Window requester, int time);
    bool XCheckTypedWindowEvent(void* display, Window w, int type, XEvent* target);
    int XGetWindowProperty(void* display, Window w, Atom property,
      ctype!("long") offset, ctype!("long") length, bool del, Atom req_type, Atom* actual_type_return,
        int* actual_format_return, ctype!("unsigned long")* nitems_return, ctype!("unsigned long")* bytes_after_return,
          ubyte** prop_return);
    int XFree(void* data);
    void* XOpenDisplay(char* name = null);
    int XCloseDisplay(void* display);
    int XChangeProperty(void* display, Window w, Atom property, Atom type, int format, int mode, ubyte* data, int nelements);
    const XA_ATOM = 4, PropModeReplace = 0;
    void setupDragDrop() {
      Atom dndVersion = 4;
      auto disp = XOpenDisplay; scope(exit) XCloseDisplay(disp);
      XChangeProperty(disp, getWindow, XInternAtom(disp, "XdndAware"), XA_ATOM, 32, PropModeReplace, cast(ubyte*) &dndVersion, 1);
    }
    const SelectionNotify = 31, AnyPropertyType = 0;
    struct XClientMessageEvent {
      ctype!("unsigned long") serial;
      bool send_event;
      void* display;
      Window window;
      Atom type;
      int format;
      union Data { byte[20] b; short[10] s; int[5] l; }
      Data data;
    }
    struct XSelectionEvent {
      ctype!("unsigned long") serial;
      bool send_event;
      void* display;
      Window window;
      Atom selection, target, property; int time;
    }
    struct XEvent { int type; union { XClientMessageEvent xclient; XSelectionEvent xselect; } }
    alias int Status;
    Status XSendEvent(void* display, Window w, bool propagate, ctype!("long") mask, XEvent* send);
    struct SDL_SysWMinfo {
      ubyte major, minor, patch;
      int SDL_SYSWM_TYPE;
      union {
        struct _x11 {
          void* display;
          Window window;
        }
        _x11 x11;
      }
    }
    int SDL_GetWMInfo(SDL_SysWMinfo* info);
    Window getWindow() {
      char[1024] buffer;
      auto info = cast(SDL_SysWMinfo*) buffer.ptr;
      info.major = 1; info.minor = 2; info.patch = 13;
      SDL_GetWMInfo(info);
      return info.x11.window;
    }
  }
  union SDL_Event {
    SDL_EventType type;
    struct Active { SDL_EventType type, gain, state; }; Active active;
    struct Key { SDL_EventType type, which, state; SDL_keysym keysym; }; Key key;
    struct Motion { SDL_EventType type, which, state; ushort x, y; short xrel, yrel; }; Motion motion;
    struct Button { SDL_EventType type, which, button, state; ushort x, y; }; Button button;
    struct Jaxis { SDL_EventType type, which, axis; short value; }; Jaxis jaxis;
    struct Jball { SDL_EventType type, which, ball; short xrel, yrel; }; Jball jball;
    struct Jhat { SDL_EventType type, which, hat, value; }; Jhat jhat;
    struct Jbutton { SDL_EventType type, which, button, state; }; Jbutton jbutton;
    struct Resize { SDL_EventType type; int w, h; }; Resize resize;
    struct Expose { SDL_EventType type; }; Expose expose;
    struct Quit { SDL_EventType type; }; Quit quit;
    struct User { SDL_EventType type; int code; void *data1, data2; }; User user;
    version(win32) alias void SyswmMsg;
    else {
      struct SyswmMsg {
        ubyte major, minor, patch;
        uint subsystem;
        XEvent xevent;
      }
    }
    struct Syswm { SDL_EventType type; SyswmMsg *msg; }; Syswm syswm;
  }

  int SDL_PollEvent(SDL_Event *event);
  int SDL_SaveBMP_RW(SDL_Surface *surface, SDL_RWops *dst, int freedst);
  void SDL_QuitSubSystem(uint flags);
  const uint SDL_INIT_VIDEO = 0x00000020;
  int SDL_Init(uint flags);
  char* SDL_GetKeyName(SDLKey key);
  void SDL_WarpMouse(ushort x, ushort y);
}

static this() {
  SDL_Init(SDL_INIT_VIDEO);
  SDL_EnableUNICODE();
}

void SDL_SaveBMP(SDL_Surface *surf, string name) { SDL_SaveBMP_RW(surf, SDL_RWFromFile(toStringz(name), "wb"), 1); }

//SDL_Surface *display;
Area display;

void putpixel32(SDL_Surface *which, int x, int y, uint c) {
  uint *bufp = cast(uint *)which.pixels + y*which.pitch/4 + x;
  *bufp = c;
}

void putpixel32(int x, int y, rgb col) {
  putpixel32(display.surface, x, y, SDL_MapRGBA(display.surface.format, col.values[0], col.values[1], col.values[2], 0));
}

void getpixel32(SDL_Surface *which, int x, int y, ubyte[4]* col) {
  uint* bufp = cast(uint *)which.pixels + y*which.pitch/4 + x;
  SDL_GetRGBA(*bufp, which.format, col.ptr, col.ptr + 1, col.ptr + 2, col.ptr + 3);
  // we know we're 32-bit, so we can decode directly
  // presume RGBA order // this might actually be WRONG !!
  // ptr = cast(ubyte*) bufp;
}

void fastblend32(int FractionBits)(rgb to) {
  auto ds = display.surface;
  uint add = SDL_MapRGBA(ds.format, to.r, to.g, to.b, 0);
  const FractionMask =
     (255 >> FractionBits) +
    ((255 >> FractionBits) << 8) +
    ((255 >> FractionBits) << 16) +
    ((255 >> FractionBits) << 24); // construct shift mask
  add = (add >> FractionBits) & FractionMask;
  for (int y = 0; y < ds.h; ++y) {
    auto line = cast(uint*) ds.pixels + y * ds.pitch / 4;
    // enable autovec
    const Step = 16;
    int x;
    for (x = 0; x <= ds.w - Step; x += Step) {
      auto chunk = &line[x];
      for (int x2 = 0; x2 < Step; ++x2) {
        // fastblend
        chunk[x2] = chunk[x2] - ((chunk[x2] >> FractionBits) & FractionMask) + add;
      }
    }
    // rest
    for (; x < ds.w; ++x) {
      line[x] = line[x] - ((line[x] >> FractionBits) & FractionMask) + add;
    }
  }
}

rgb getpixel32(int x, int y) {
  ubyte[4] temp = void;
  uint* bufp = cast(uint*) display.surface.pixels + y*display.surface.pitch/4 + x;
  SDL_GetRGBA(*bufp, display.surface.format, &temp[0], &temp[1], &temp[2], &temp[3]);
  return *cast(rgb*) temp.ptr;
}

void getpixel24(SDL_Surface *which, int x, int y, ubyte[4] *col) {
  ubyte *bufp = cast(ubyte *)which.pixels + y*which.pitch + x*3;
  uint value=bufp[2] << 16 | bufp[1] << 8 | bufp[0];
  SDL_GetRGBA(value, which.format, col.ptr, col.ptr+1, col.ptr+2, col.ptr+3);
}

struct hsv {
  union {
    ubyte[3] values;
    alias values field;
    struct { ubyte h, s, v; }
  }
  static hsv opCall(T...)(T t) {
    hsv res = void;
    static if (T.length == 1 && is(T[0]==rgb)) {
      with (t[0]) with (res) {
        auto Min = min(r, g, b), Max = max(r, g, b), delta = Max - Min;
        v = Max;
        float res_h;
        if (Max) s = cast(ubyte) (255f*(1f*delta / Max));
        else {
          s = 0;
          res_h = 0;
          return res;
        }
        if (r == Max) res_h = (g - b)*1f / delta;
        else if (g == Max) res_h = 2f + (b - r)*1f / delta;
        else res_h = 4f + (r - g)*1f / delta;
        res_h *= 60f; if (res_h < 0) res_h += 360f;
        h = cast(ubyte) (res_h*255f/360f);
      }
    } else {
      static assert (T.length == 3, "Invalid parameter: "~T.stringof);
      res.h = cast(ubyte) t[0]; res.s = cast(ubyte) t[1]; res.v = cast(ubyte) t[2];
    }
    return res;
  }
}

struct rgb {
  union {
    ubyte[3] values;
    struct { ubyte r, g, b; }
    Repeat!(ubyte, 3) tuple;
  }
  SDL_Color toSDL() { return SDL_Color(r, g, b, 0); }
  uint map() {
    return SDL_MapRGBA(display.surface.format, r, g, b, 0);
  }
  rgb opCat(rgb other) {
    rgb res;
    foreach (id, ref v; res.values) v=cast(ubyte)((values[id]+other.values[id])/2);
    return res;
  }
  bool opEquals(rgb r) {
    return values == r.values;
  }
  rgb blend(R, S)(R other, S f) {
    static assert(is(typeof(R.r)) && is(typeof(R.g)) && is(typeof(R.b)), R.stringof~" cannot be blended with: needs r,g,b!");
    static if (is(S == ubyte)) {
      return rgb((r*(256-f)+other.r*f)/256, (g*(256-f)+other.g*f)/256, (b*(256-f)+other.b*f)/256);
    } else {
      static assert(is(S: real), " Cannot blend "~R.stringof~" with factor "~S.stringof);
      return rgb(r+(other.r-r)*f, g+(other.g-g)*f, b+(other.b-b)*f);
    }
  }
  rgb opMul(rgb other) {
    return rgb((r*other.r)/256, (g*other.g)/256, (b*other.b)/256);
  }
  rgb opMul(float f) {
    return rgb(cast(ubyte) (r*f), cast(ubyte) (g*f), cast(ubyte) (b*f));
  }
  rgb opMulAssign(rgb other) { *this = opMul(other); return *this; }
  rgb opAdd(rgb other) {
    rgb res = void;
    res.r = cast(ubyte) min(255, r + other.r);
    res.g = cast(ubyte) min(255, g + other.g);
    res.b = cast(ubyte) min(255, b + other.b);
    return res;
  }
  rgb invert() { return rgb(255-r, 255-g, 255-b); }
  static rgb opCall(T...)(T tuple) {
    rgb res = void;
    static if (T.length == 1) {
      static if (is(T[0] == hsv)) {
        with (tuple[0]) {
          if (!s) res=rgb(v, v, v);
          else {
            auto float_h = 6f * h / 256f, float_s = s / 255f, float_v = v / 255f;
            auto i = cast(int) float_h, f = float_h - i;
            auto p = float_v * (1f - float_s), q = float_v * (1f - float_s * f), t = float_v * (1f - float_s * (1f - f));
            switch (i) {
              case 0: res = rgb(float_v*255f, t*255f, p*255f); break;
              case 1: res = rgb(q*255f, float_v*255f, p*255f); break;
              case 2: res = rgb(p*255f, float_v*255f, t*255f); break;
              case 3: res = rgb(p*255f, q*255f, float_v*255f); break;
              case 4: res = rgb(t*255f, p*255f, float_v*255f); break;
              case 5: res = rgb(float_v*255f, p*255f, q*255f); break;
              default: fail("Oh noes.");
            }
          }
        }
      } else static if (is(T[0] == ubyte[3])) {
        res.r = tuple[0][0]; res.g = tuple[0][1]; res.b = tuple[0][2];
      } else static assert(false, "rgb: invalid parameter: "~T.stringof);
    } else {
      static assert (T.length == 3, "rgb: Invalid parameter: "~T.stringof);
      res.r = cast(ubyte) tuple[0]; res.g = cast(ubyte) tuple[1]; res.b = cast(ubyte) tuple[2];
    }
    return res;
  }
  string toString() { return Format("RGB ", values); }
}

int alpha;

void putpixel(int x, int y, uint col) {
  auto transformed=display.tl+pt(x, y);
  with (transformed) if (x<0 || y<0 || x!<display.surface.w || y!<display.surface.h) return;
  putpixel32(display.surface, transformed.tuple, col);
}

void blend(ubyte* rgba, ubyte nr, ubyte ng, ubyte nb, ubyte alpha) {
  rgba[0] = cast(ubyte) (rgba[0] + ((nr - rgba[0]) * alpha) / 255);
  rgba[1] = cast(ubyte) (rgba[1] + ((ng - rgba[1]) * alpha) / 255);
  rgba[2] = cast(ubyte) (rgba[2] + ((nb - rgba[2]) * alpha) / 255);
}

void putpixel(int x, int y, ubyte r, ubyte g, ubyte b) {
  putpixel(x, y, SDL_MapRGBA(display.surface.format, r, g, b, 0));
}
void putpixel(int x, int y, ubyte[3] rgb, ubyte a) {
  ubyte[4] old_rgba;
  getpixel32(display.surface, display.tl.x + x, display.tl.y + y, &old_rgba);
  blend(old_rgba.ptr, rgb[0], rgb[1], rgb[2], a);
  putpixel(x, y, SDL_MapRGBA(display.surface.format, old_rgba[0], old_rgba[1], old_rgba[2], 0));
}

const rgb White={[255, 255, 255]};
const rgb Black={[0, 0, 0]};
const rgb Red={[255, 0, 0]};
const rgb Green={[0, 255, 0]};
const rgb Blue={[0, 0, 255]};
const rgb Yellow={[255, 255, 0]};
const rgb Cyan={[0, 255, 255]};
const rgb Purple={[255, 0, 255]};
rgb color=White;
rgb back=Black;

template failfind(U, T...) {
  static if (T.length)
    static if (is(T[0] == U)) static assert(false, "Duplicate "~U.stringof~" found!");
    else const bool failfind=failfind!(U, T[1..$]);
  else
    const bool failfind=true;
}

template Select(U, T...) {
  static if(T.length)
    static if (is(U == T[0])) { static if (failfind!(U, T[1..$])) { }; const int Select = 0; }
    else
      static if (Select!(U, T[1..$]) != -1)
        const int Select = 1 + Select!(U, T[1..$]);
      else
        const int Select = -1;
  else
    const int Select = -1;
}

typedef rgb back_rgb;
back_rgb Back(rgb r) { return cast(back_rgb) r; }
back_rgb Back() { return cast(back_rgb) back; }
typedef rgb box_rgb;
box_rgb Box(rgb r) { return cast(box_rgb) r; }
box_rgb Box() { return cast(box_rgb) color; }
alias Back Fill;

bool doFlip=false;
//void flip() { if (!screen.offscreen) SDL_Flip(display.surface); }
void flip() { if (!screen.offscreen) SDL_UpdateRect(display.surface); }
void flip(bool target) { doFlip=target; }
scope class groupDraws {
  bool wasOn;
  this() { wasOn=doFlip; flip=false; }
  ~this() { if (wasOn) { flip=true; flip; } }
}

void execParams(T...)(T params) {
  const int bcol=Select!(back_rgb, T);
  static if (bcol != -1) back=cast(rgb) params[bcol];
  const int col=Select!(rgb, T);
  static if (col != -1) color=params[col];
  //else static if (bcol != -1) color=back;
  const int boxcol=Select!(box_rgb, T);
  static if (boxcol != -1) color=cast(rgb) params[boxcol];
}

void tintfill(int x1, int y1, int x2, int y2, rgb color) {
  SDL_LockSurface(display.surface);
  scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) flip; }
  // ubyte* c;
  ubyte[4] c;
  x1+=display.tl.x; x2+=display.tl.x;
  y1+=display.tl.y; y2+=display.tl.y;
  for (int x=x1; x<x2; ++x) {
    for (int y=y1; y<y2; ++y) {
      getpixel32(display.surface, x, y, &c);
      c[0]=cast(ubyte)((c[0]*178+color.r*77)>>8);
      c[1]=cast(ubyte)((c[1]*178+color.g*77)>>8);
      c[2]=cast(ubyte)((c[2]*178+color.b*77)>>8);
      putpixel32(display.surface, x, y, SDL_MapRGBA(display.surface.format, c[0], c[1], c[2], c[3]));
    }
  }
}

int lastx, lasty;

void pset(T...)(T params) {
  static if (T.length && is(T[0] == pt)) {
    auto x = params[0].x, y = params[0].y;
    const int RO = 1;
  } else {
    auto x = cast(int) params[0], y = cast(int) params[1];
    const int RO = 2;
  }
  lastx=cast(int) x; lasty=cast(int) y;
  SDL_LockSurface(display.surface);
  scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) flip; }
  execParams(params[RO..$]);
  if (x<0 || x!<screen.w || y<0 || y!<screen.h) return;
  with (color) putpixel32(
    display.surface, x, y,
    SDL_MapRGBA(display.surface.format, values[0], values[1], values[2], 0)
  );
}

rgb pget(T...)(T t) {
  static assert (T.length == 2);
  static assert (is(typeof(cast(int) t[0])) && is(typeof(cast(int) t[1])));
  auto x = cast(int) t[0], y = cast(int) t[1];
  SDL_LockSurface(display.surface);
  scope(exit) SDL_UnlockSurface(display.surface);
  // ubyte* c;
  ubyte[4] c;
  getpixel32(display.surface, (pt(x, y)+display.tl).tuple, &c);
  rgb res; res.values[]=c[0..3]; return res;
}

void bresenham(bool countUp=true, bool steep=false)(int x0, int y0, int x1, int y1) {
  auto deltax = x1 - x0, deltay = y1 - y0;
  static if (steep) {
    auto Δerror = cast(float)deltax / cast(float)deltay;
    auto var2 = x0;
    const string name="y";
  } else {
    auto Δerror = cast(float)deltay / cast(float)deltax;
    auto var2 = y0;
    const string name="x";
  }
  auto error = 0f;
  uint col=SDL_MapRGBA(display.surface.format, color.values[0], color.values[1], color.values[2], 0);
  for (auto var1 = mixin(name~'0'); var1 <= mixin(name~'1'); ++var1) {
    static if (steep) putpixel(var2, var1, col);
    else putpixel(var1, var2, col);
    error += Δerror;
    if (abs(error) >= 1f) { static if (countUp) { var2++; error -= 1f; } else { var2--; error += 1f; }}
  }
}

ubyte[256] square_map; // 0..255 == 0..1
static this() {
  for (int i = 0; i < 256; ++i) {
    float as_float = i / 256f;
    square_map[i] = cast(ubyte) (pow(as_float, 0.5f) * 256);
  }
}

void bresenham_aa(bool countUp=true, bool steep=false)(int x0, int y0, int x1, int y1) {
  auto deltax = x1 - x0, deltay = y1 - y0;
  static if (steep) {
    auto Δerror = cast(float)deltax / cast(float)deltay;
    auto var2 = x0;
    const string name="y";
  } else {
    auto Δerror = cast(float)deltay / cast(float)deltax;
    auto var2 = y0;
    const string name="x";
  }
  auto error = 0f;
  for (auto var1 = mixin(name~'0'); var1 <= mixin(name~'1'); ++var1) {
    // auto alpha = cast(ubyte) (error*255), anti_alpha = 255 - alpha;
    auto alpha = square_map[cast(ubyte) (error * 255)],
      anti_alpha = square_map[255 - cast(ubyte) (error * 255)];
    static if (steep) {
      if (countUp) {
        if (var2 < 1 || var1 < 0 || var2 !< screen.w + 1 || var1 !< screen.h) continue;
        putpixel(var2-1, var1, color.values, anti_alpha);
        if (var2 < display.width) putpixel(var2, var1, color.values, alpha);
      } else {
        if (var2 < -1 || var1 < 0 || var2 !< screen.w - 1 || var1 !< screen.h) continue;
        putpixel(var2+1, var1, color.values, alpha);
        if (var2 > 0) putpixel(var2, var1, color.values, anti_alpha);
      }
    } else {
      if (countUp) {
        if (var1 < 0 || var2 < 0 || var1 !< screen.w || var2 !< screen.h) continue;
        putpixel(var1, var2, color.values, anti_alpha);
        if (var2 < display.height) putpixel(var1, var2+1, color.values, alpha);
      } else {
        if (var1 < 0 || var2 < -1 || var1 !< screen.w || var2 !< screen.h) continue;
        putpixel(var1, var2+1, color.values, alpha);
        if (var2 > 0) putpixel(var1, var2, color.values, anti_alpha);
      }
    }
    error += Δerror;
    if (abs(error) >= 1f) { static if (countUp) { var2++; error -= 1f; } else { var2--; error += 1f; }}
  }
}

template _UnPoint(int curOffs, T...) {
  static if (T.length) {
    alias _UnPoint!(curOffs + 1, T[1..$]) rest;
    static if (is(T[0] == pt)) {
      alias Tuple!(int, int, rest.tuple) tuple;
      const string value = "%["~ctToString(curOffs)~"].x, %["~ctToString(curOffs)~"].y"
        ~ (rest.value.length?(", "~rest.value):rest.value);
    } else {
      alias Tuple!(T[0], rest.tuple) tuple;
      const string value = "%["~ctToString(curOffs)~"]"
        ~ (rest.value.length?(", "~rest.value):rest.value);
    }
  } else {
    alias Tuple!() tuple;
    const string value = "";
  }
}

template UnPoint(T...) { mixin _UnPoint!(0, T); }

bool hasnan(T...)(T t) {
  foreach (value; t)
    static if (is(typeof(value): real)) if (isnan(cast(real) value)) return true;
  return false;
}

void line(_T...)(_T _p) {
  if (hasnan(_p)) return;
  alias UnPoint!(_T).tuple T;
  auto p = mixin("stuple("~ctReplace(UnPoint!(_T).value, "%", "_p")~")");
  static if (T.length>=3 && (is(T[2]: long)||is(T[2]: real))) {
    _line(cast(int)p._0, cast(int)p._1, cast(int)p._2, cast(int)p._3, p.tupleof[4..$]);
    lastx=cast(int)p._2; lasty=cast(int)p._3;
  } else {
    _line(lastx, lasty, cast(int)p._0, cast(int)p._1, p.tupleof[2..$]);
    lastx=cast(int)p._0; lasty=cast(int)p._1;
  }
}

bool aa = true;

void _line(T...)(int x0, int y0, int x1, int y1, T p) {
  static int max(int a, int b) { return a>b?a:b; }
  static int min(int a, int b) { return a<b?a:b; }
  execParams(p);
  void updateRect() {
    SDL_UpdateRect(display.surface, min(x0, x1), min(y0, y1), max(x0, x1)-min(x0, x1), max(y0, y1)-min(y0, y1));
  }
  static if (Select!(back_rgb, T)!=-1) {
    SDL_LockSurface(display.surface);
    scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) updateRect; }
    SDL_Rect rect;
    with (rect) {
      x=cast(short) min(x0, x1); y=cast(short) min(y0, y1);
      w=cast(ushort) (max(x0, x1)-min(x0, x1));
      h=cast(ushort) (max(y0, y1)-min(y0, y1));
    }
    rect.x+=display.tl.x; rect.y+=display.tl.y;
    with (back) SDL_FillRect(display.surface, &rect, SDL_MapRGBA(display.surface.format, values[0], values[1], values[2], 0));
  }
  static if (Select!(box_rgb, T)!=-1) {
    SDL_LockSurface(display.surface);
    scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) updateRect; }
    uint col;
    auto box = p[Select!(box_rgb, T)];
    col = SDL_MapRGBA(display.surface.format, box.values[0], box.values[1], box.values[2], 0);
    if (x1 < x0) swap(x0, x1);
    if (y1 < y0) swap(y0, y1);
    hline(x0, y0, x1 - x0, col);
    hline(x0, y1, x1 - x0, col);
    vline(x0, y0, y1 - y0, col);
    vline(x1, y0, y1 - y0, col);
  }
  static if (Select!(box_rgb, T)+Select!(back_rgb, T)==-2) {
    SDL_LockSurface(display.surface);
    scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) updateRect; }
    bool steep = abs(y1 - y0) > abs(x1 - x0);
    void turn() { swap(x0, x1); swap(y0, y1); }
    if (steep) { if (y1 < y0) turn; }
    else { if (x1 < x0) turn; }
    bool stepUp=steep ? (x0 < x1) : (y0 < y1);
    mixin(IfBranch!(
      "aa", "$1", "_aa", "", "steep", "$2", "true", "false", "stepUp", "$3", "true", "false", "bresenham$1!($3, $2)(x0, y0, x1, y1); "
    ));
  }
}

template circle_bresenham_pass(bool first) {
  const string xy=(first?"x":"y");
  const string yx=(first?"y":"x");
  const string str="
    auto x="~(first?"radius":"0")~";
    auto y="~(first?"0":"radius")~";
    auto xchange=radius*radius*"~(first?"(1-2*radius)":"1")~";
    auto ychange=radius*radius*"~(first?"1":"(1-2*radius)")~";
    auto error=0;
    auto stopx="~(first?"y2square*radius":"0")~";
    auto stopy="~(first?"0":"x2square*radius")~";
    while (stopx"~(first?">=":"<=")~"stopy) {
      putpixel(cast(int) (cx+x), cast(int) (cy+y), col);
      putpixel(cast(int) (cx+x), cast(int) (cy-y), col);
      putpixel(cast(int) (cx-x), cast(int) (cy+y), col);
      putpixel(cast(int) (cx-x), cast(int) (cy-y), col);
      "~yx~"++;
      stop"~yx~"+="~xy~"2square;
      error+="~yx~"change;
      "~yx~"change+="~xy~"2square;
      if ((2*error+"~xy~"change)>0) {
        --"~xy~";
        stop"~xy~"-="~yx~"2square;
        error+="~xy~"change;
        "~xy~"change+="~yx~"2square;
      }
    }
  ";
}

void hline(pt p, int w, rgb col) {
  hline(p.x, p.y, w, SDL_MapRGBA(display.surface.format, col.tuple, 0));
}

void hline(int x, int y, int w, uint col) {
  if (alpha) {
    // ubyte* cur_col;
    ubyte[4] cur_col;
    auto color = (cast(ubyte*) &col)[0..4];
    for (int pos=x; pos<=x+w; ++pos) {
      getpixel32(display.surface, pos, y, &cur_col);
      color[0]=cast(ubyte)((cur_col[0]*alpha+color[0]*(255-alpha))>>8);
      color[1]=cast(ubyte)((cur_col[1]*alpha+color[1]*(255-alpha))>>8);
      color[2]=cast(ubyte)((cur_col[2]*alpha+color[2]*(255-alpha))>>8);
      putpixel32(display.surface, pos, y, SDL_MapRGBA(display.surface.format, color[0], color[1], color[2], color[3]));
    }
  } else {
    for (int pos=x; pos<=x+w; ++pos) putpixel(pos, y, col);
  }
}

void vline(int x, int y, int h, uint col) {
  for (int pos=y; pos<=y+h; ++pos) putpixel(x, pos, col);
}

void circle(T...)(T t) {
  static assert(T.length!<3, "Circle: Needs x, y and radius");
  int cx=cast(int)t[0], cy=cast(int)t[1]; float radius=cast(float)t[2];
  SDL_LockSurface(display.surface);
  scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) flip; }
  execParams(t[3..$]);
  if (radius!>0) return;
  static if (T.length>3 && is(T[3]: int)) cy=t[3];
  static if (Select!(back_rgb, T) != -1) {
    auto back_sdl=SDL_MapRGBA(display.surface.format, back.values[0], back.values[1], back.values[2], 0);
    for (int i=0; i<=radius; ++i) {
      ushort j=cast(ushort)(sqrt(cast(real)(radius*radius-i*i)));
      hline(cx-j, cy+i, 2*j, back_sdl);
      if (i) hline(cx-j, cy-i, 2*j, back_sdl);
    }
  }
  static if (Select!(back_rgb, T) == -1 || Select!(rgb, T) != -1) {
    auto x2square=2*radius*radius, y2square=x2square;
    uint col=SDL_MapRGBA(display.surface.format, color.values[0], color.values[1], color.values[2], 0);
    { mixin(circle_bresenham_pass!(true).str); }
    { mixin(circle_bresenham_pass!(false).str); }
  }
}

float distance(float x1, float y1, float x2, float y2) {
  auto x=x1-x2, y=y1-y2;
  return sqrt(x*x+y*y);
}

struct floodfill_node {
  int x, y;
  static floodfill_node opCall(int x, int y) {
    floodfill_node res;
    res.x=x; res.y=y;
    return res;
  }
}

void paint(T...)(int x, int y, T t) {
  SDL_LockSurface(display.surface);
  scope(exit) { SDL_UnlockSurface(display.surface); if (doFlip) flip; }
  execParams(t);
  bool border=true;
  if (Select!(back_rgb, T) == -1) {
    back=pget(x, y);
    border=false;
  }
  bool check(rgb r) {
    if (border) return (r != back) && (r != color);
    else return r == back;
  }
  if (back == color) throw new Exception("Having identical backgrounds and foregrounds will severely mess up floodfill.");
  alias floodfill_node node;
  node[] queue;
  queue ~= node(x, y);
  size_t count=0;
  uint col=SDL_MapRGBA(display.surface.format, color.values[0], color.values[1], color.values[2], 0);
  while (count<queue.length) {
    scope(exit) count++;
    with (queue[count]) {
      if (check(pget(x, y))) {
        int w=x, e=x;
        if (w<display.width) do w++; while ((w<display.width) && check(pget(w, y)));
        if (e>=0) do e--; while (e>=0 && check(pget(e, y)));
        for (int i=e+1; i<w; ++i) {
          putpixel(i, y, col);
          if (y && check(pget(i, y-1)) && ((i==w-1)||!check(pget(i+1, y-1)))) queue ~= node(i, y-1);
          if ((y < display.height-1) && check(pget(i, y+1)) && ((i==w-1)||!check(pget(i+1, y+1)))) queue ~= node(i, y+1);
        }
      }
    }
  }
}

bool opengl_mode;

struct screen {
  static {
    bool offscreen=false;
    ParameterTypeTuple!(typeof(&SDL_SetVideoMode))[3] sdl_flags;
    void opCall(int w, int h, typeof(sdl_flags) flags=0, bool osc=false) {
      sdl_flags=flags;
      screen.resize(w, h, osc);
    }
    void resize(int w, int h, bool osc = false) {
      if (display && display.surface && !offscreen && !osc) {
        SDL_FreeSurface(display.surface); // we're just changing size
      }
      if (opengl_mode && !offscreen) { display.width = w; display.height = h; return; }
      offscreen = osc;
      if (offscreen) {
        auto surf = SDL_CreateRGBSurface(sdl_flags, w, h, 32);
        display = Area(RefSurf(surf));
      } else {
        display=Area(RefSurf(SDL_SetVideoMode(w, h, 32, sdl_flags)));
        static if (is(typeof(&setupDragDrop))) setupDragDrop();
      }
    }
    void close() { SDL_QuitSubSystem(SDL_INIT_VIDEO); }
    int width() { return display.width; }
    int height() { return display.height; }
    alias width w; alias height h;
    struct WithAreaHolder(T...) {
      T t;
      Area opAssign(C)(lazy C callable) {
        void call() {
          static if (is(C==void)) callable();
          else static if (is(C==void delegate())) callable()();
          else static assert(false, "Cannot call: "~T.stringof);
        }
        static if (is(T == Tuple!(Area))) {
          auto backup=display; display=t[0]; scope(exit) display=backup;
          auto backup_flags=sdl_flags; scope(exit) sdl_flags=backup_flags;
          auto offs=offscreen; scope(exit) offscreen=offs;
          call(); return display;
        } else static if (is(T == Tuple!(int, int))) {
          auto backup=display, backup_flags=sdl_flags, old_osc=offscreen;
          scope(exit) { display=backup; sdl_flags=backup_flags; offscreen=old_osc; }
          screen(t[0], t[1], 0, true);
          call(); return display;
        } else static assert(false, "opAssign with "~T.stringof~" .. ?");
      }
    }
    WithAreaHolder!(T) With(T...)(T t) {
      WithAreaHolder!(T) res = void;
      foreach (id, value; t) res.t[id] = value;
      return res;
    }
  }
}

void cls(rgb fill=Black) { line(0, 0, display.width, display.height, Fill=fill); }

import tools.log;
void updateMouse(int x, int y, ubyte button, int pushed) {
  auto b = cast(Button) button;
  if (mouse.pos != pt(x, y)) mouse._moved = true;
  mouse.pos = pt(x, y);
  if (pushed == 1) {
    mouse.down(b);
  }
  if (pushed == -1) {
    mouse.up(b);
  }
}

enum Button : ubyte { Left=1, Middle, Right, Extra1, Extra2 }

struct _mouse {
  pt pos;
  int x() { return pos.x; }
  int y() { return pos.y; }
  bool pressed() { return pressed(Button.Left); }
  bool pressed(Button b) { return _pressed[b]; }
  bool pressed(Button b, Area a) { return pressed(b) && (pos in a); }
  bool pressed(Area a) { return pressed(Button.Left, a); }
  bool[256] _pressed, was_pressed, was_released;
  bool clicked() { return clicked(Button.Left); }
  bool clicked(Button which) { return was_pressed[which]; }
  bool clicked(Button which, Area a) { return (pos in a) && clicked(which); }
  bool clicked(Area a) { return clicked(Button.Left, a); }
  bool released() { return released(Button.Left); }
  bool released(Button which) { return was_released[which]; }
  bool released(Button which, Area a) { return (pos in a) && released(which); }
  bool released(Area a) { return released(Button.Left, a); }
  void down(Button b) { _pressed[b] = true; was_pressed[b] = true; }
  void up(Button b) { _pressed[b] = false; was_released[b] = true; }
  void events_reset() { foreach (ref b; was_pressed) b = false; foreach (ref b; was_released) b = false; }
  bool _moved;
  bool moved() { auto res = _moved; _moved = false; return res; }
}

_mouse mouse;

bool[wchar] keystate;

struct _key {
  bool[ushort] key_pressed;
  bool _pressed(bool PUSHED)(ushort which) {
    if (auto kp = which in key_pressed) {
      auto res = *kp;
      static if (!PUSHED) *kp = false;
      return res;
    } else return false;
  }
  alias _pressed!(false) pressed;
  alias _pressed!(true) pushed;
}

_key key;

void delegate() onResize;

int drop_type, drop_action;

string _text_dropped;
string textDropped() {
  auto res = _text_dropped;
  _text_dropped = null;
  return res;
}

bool accept_drop;

import tools.time;
static this() { SDL_EventState(SDL_EventType.SysWMEvent, true); }
/// parameters to key: which key, and if it's pressed or released
/// parameters to mouse: x, y, button [Left: 1, Mid: 2, Right: 3], change [1: pressed, -1: released]
void events(void delegate(ushort, bool) key=null, void delegate(int, int, ubyte, int) mouse=null, void delegate() quit=null) {
  .mouse.events_reset;
  SDL_Event evt;
  while (SDL_PollEvent(&evt)) {
    switch (evt.type) {
      case SDL_EventType.MouseMotion:
        with (evt.motion) {
          updateMouse(x, y, 0, 0);
          if (mouse) mouse(x, y, 0, 0);
        }
        break;
      case SDL_EventType.MouseButtonDown:
        with (evt.button) {
          updateMouse(x, y, button, 1);
          if (mouse) mouse(x, y, button, 1);
        }
        break;
      case SDL_EventType.MouseButtonUp:
        with (evt.button) {
          updateMouse(x, y, button, -1);
          if (mouse) mouse(x, y, button, -1);
        }
        break;
      case SDL_EventType.KeyDown:
        auto sym = evt.key.keysym.sym;
        logln("KeyDown: ", sym);
        .key.key_pressed[cast(ushort) sym] = true;
        if (key) key(cast(ushort) sym, true);
        break;
      case SDL_EventType.KeyUp:
        auto sym = evt.key.keysym.sym;
        .key.key_pressed[cast(ushort) sym] = false;
        if (key) key(cast(ushort) sym, false);
        break;
      case SDL_EventType.Resize:
        screen.resize(evt.resize.w, evt.resize.h);
        if (onResize) onResize();
        break;
      case SDL_EventType.Quit:
        if (quit) quit();
        else throw new Exception("Quit");
        break;
      case SDL_EventType.SysWMEvent:
        version(win32) break; // assert(false, "No system events support for win32 yet");
        else { // presume X11
          with (evt.syswm.msg.xevent) {
            Atom atom(string name) { return XInternAtom(xclient.display, toStringz(name)); }
            string atomName(Atom atom) { return Format(XGetAtomName(xclient.display, atom)); }
            if (type == 33) {
              // writefln("Type: ", xclient.type);
              auto name = atomName(xclient.type);
              // writefln("Name: ", name);
              if (name == "XdndEnter") {
                auto source = xclient.data.l[0];
                if (atomName(xclient.data.l[2]).startsWith("text/plain")) {
                  accept_drop = true;
                  // writefln("Drop accepted");
                }
                drop_type = xclient.data.l[2];
              }
              if (!accept_drop) break;
              if (name == "XdndPosition") {
                XEvent drop_response;
                drop_response.type = 33;
                with (drop_response.xclient) {
                  display = xclient.display;
                  window = xclient.data.l[0]; // wtf? see wine source code, but why?
                  type = "XdndStatus".atom();
                  format = 32;
                  data.l[0] = window;
                  data.l[1] = true;
                  data.l[2] = 0;
                  data.l[3] = 0;
                  drop_action = data.l[4] = xclient.data.l[4];
                }
                auto res = XSendEvent(xclient.display, xclient.data.l[0], false, 0, &drop_response);
              }
              if (name == "XdndDrop") {
                auto drop_timekey = xclient.data.l[2];
                auto res = XConvertSelection(xclient.display, "XdndSelection".atom(),
                  drop_type, "XdndTarget".atom(), getWindow, drop_timekey);
                auto start = sec();
                while (sec() - start < 1.0) {
                  XEvent event;
                  auto check_res = XCheckTypedWindowEvent(xclient.display, getWindow, SelectionNotify, &event);
                  if (check_res && event.xselect.selection == "XdndSelection".atom()) goto done;
                }
                return; // timeout
done:
                Atom act_type; uint bytes_left, items; int act_format; ubyte* data;
                XGetWindowProperty(xclient.display, getWindow, "XdndTarget".atom(), 0, -1, true,
                  AnyPropertyType, &act_type, &act_format, &items, &bytes_left, &data);
                items *= (act_format / 8);
                auto my_data = Format(cast(char*) data).dup;
                XFree(data);
                XEvent finished; finished.type = 33;
                with (finished.xclient) {
                  display = xclient.display;
                  window = getWindow();
                  type = "XdndFinished".atom();
                  format = 32;
                  data.l[0] = getWindow;
                  data.l[1] = true;
                  data.l[2] = drop_action;
                }
                XSendEvent(xclient.display, xclient.data.l[0], false, 0, &finished);
                // writefln("Sent XdndFinished");
                _text_dropped = my_data;
                accept_drop = false;
              }
            }
          }
        }
        break;
      case SDL_EventType.Active: break;
      default:
	break;
    }
  }
}

void events(void delegate(ushort) key, void delegate(int, int, ubyte, int) mouse=null, void delegate() quit=null) {
  events((ushort a, bool b) {
    if (b) key(a);
  }, mouse, quit);
}

void events(void delegate(ushort) key, void delegate(int, int) mouse, void delegate() quit=null) {
  events(key, (int x, int y, ubyte b, int p) { mouse(x, y); }, quit);
}

void events(void delegate(ushort, bool) key, void delegate(int, int) mouse, void delegate() quit=null) {
  events(key, (int x, int y, ubyte b, int p) { mouse(x, y); }, quit);
}

struct pt {
  union {
    struct {
      int x, y;
    }
    Repeat!(int, 2) tuple;
  }
  static pt opCall(int i, int k) { pt res; res.x = i; res.y = k; return res; }
  static pt opCall(int i) { pt res; res.x = res.y = i; return res; }
  pt opAdd(pt other) { return pt(x+other.x, y+other.y); }
  void opAddAssign(pt other) { x+=other.x; y+=other.y; }
  pt opSub(pt other) { return pt(x-other.x, y-other.y); }
  void opSubAssign(pt other) { x-=other.x; y-=other.y; }
  pt opDiv(pt other) { return pt(x/other.x, y/other.y); }
  pt opDiv(int v) { return pt(x/v, y/v); }
  pt opMod(pt other) { return pt(x%other.x, y%other.y); }
  pt opMul(pt other) { return pt(x*other.x, y*other.y); }
  pt opMul(float other) { return pt(cast(int) (x*other), cast(int) (y*other)); }
  alias opMul opMul_r;
  pt rotated(float angle) {
    auto c = cos(angle), s = sin(angle);
    return pt(cast(int) (x * c - y * s), cast(int) (x * s + y * c));
  }
  string toString() { return Format("pt(", x, ", ", y, ")"); }
  float length() {
    return sqrt(0f + x*x + y*y);
  }
  float length(float f) {
    auto factor = f / length;
    x = cast(int) (x * factor);
    y = cast(int) (y * factor);
    return f;
  }
  bool inside(pt tl, pt size) {
    return x > tl.x && y > tl.y && x < tl.x + size.x && y < tl.y + size.y;
  }
}

typedef int Align;
const Align Center=0, Left=1, Right=2, Top=4, Bottom=8;
string AlignToString(Align a) {
  return "Alignment ["~["Center", "Left", "Right", "Left+Right"][a&3]~
    ", "~["Center", "Top", "Bottom", "Top+Bottom"][(a>>2)&3]~"]";
}

Align alignto(pt from, pt to) {
  Align res;
  auto angle=atan2((to-from).y, (to-from).x);
  int index=cast(int)((angle/(2*PI))*9);
  while (index<0) index+=8;
  return [Left, Left|Bottom, Bottom, Bottom|Right, Right, Right|Top, Top, Top|Left, Left][index];
}

/*import tools.tests;
unittest {
  Assert("AlignmentFormattingTest", AlignToString(Left|Right|Top|Bottom)=="Alignment [Left+Right, Top+Bottom]");
  mustFail("InvalidAlignmentTest", validate(Left|Right|Top|Bottom));
}*/

void validate(Align a) {
  if (((a&Left)&&(a&Right)) || ((a&Top)&&(a&Bottom)))
    throw new Exception("Invalid alignment: "~AlignToString(a));
}

struct RefSurf {
  SDL_Surface* surface;
  int refs;
  static RefSurf* opCall(SDL_Surface* surf) {
    auto res = new RefSurf;
    res.surface = surf;
    return res;
  }
  void lock() {
    SDL_LockSurface(surface);
  }
  void unlock() {
    SDL_UnlockSurface(surface);
  }
}

class Area {
  RefSurf* surf;
  SDL_Surface* surface() { return surf.surface; }
  ~this() {
    if (!surf.surface) return;
    if (--surf.refs) return;
    SDL_FreeSurface(surf.surface);
  }
  pt tl, dimensions;
  Area dup() { return Area(tl, dimensions, surf); }
  pt br() { return tl+size; }
  pt bl() { return pt(tl.x, tl.y + size.y); }
  pt tr() { return pt(tl.x + size.x, tl.y); }
  static Area opCall(pt _tl, pt _size, RefSurf* _rs=null) {
    auto res=new Area;
    res.tl=_tl; res.dimensions=_size;
    if (!_rs) _rs = display.surf;
    _rs.refs ++;
    res.surf = _rs;
    return res;
  }
  static Area opCall(RefSurf* _rs) {
    auto res=new Area;
    res.tl=pt(0, 0);
    if (!_rs || !_rs.surface) throw new Exception("Invalid surface");
    res.dimensions=pt(_rs.surface.w, _rs.surface.h);
    res.surf = _rs;
    res.surf.refs ++;
    return res;
  }
  static Area opCall(SDL_Surface* surf) {
    return opCall(RefSurf(surf));
  }
  bool opIn_r(T)(T vt) { return (vt.x>=tl.x && vt.x<=(tl.x+size.x)) && (vt.y>=tl.y && vt.y<=(tl.y+size.y)); }
  Area select(int x, int y, int w=int.max, int h=int.max) {
    if (w==int.max) w=size.x - x;
    if (h==int.max) h=size.y - y;
    return Area(tl+pt(x, y), pt(w, h), surf);
  }
  Area size(int w, int h) { return Area(tl, pt(w, h), surf); }
  pt size() { return dimensions; }
  // move does not check if end coordinates are actually valid
  Area move(int x, int y) { return Area(tl+pt(x, y), size, surf); }
  Area move(pt p) { return Area(tl+p, size, surf); }
  creal locate(pt which, creal tl, creal br) {
    creal local=(1f*(which.x-this.tl.x))/width + ((1f*(which.y-this.tl.y))/height)*1i;
    return tl.re+(br.re-tl.re)*local.re + (tl.im+(br.im-tl.im)*local.im)*1i;
  }
  Area width(int w) {
    if (w<0) {
      auto new_size = pt(-w, size.y);
      tl = br - pt(-w, height);
      dimensions = new_size;
      return this;
    } else {
      dimensions = pt(w, size.y);
      return this;
    }
  }
  //Area width(float wp) { return width(cast(int)(width*wp)); }
  int width() { return dimensions.x; }
  Area height(int h) {
    if (h<0) {
      auto new_size = pt(size.x, -h);
      tl = br - pt(width, -h);
      dimensions = new_size;
      return this;
    } else {
      dimensions = pt(size.x, h);
      return this;
    }
  }
  //Area height(float hp) { return height(cast(int)(height*hp)); }
  int height() { return dimensions.y; }
  Area shrink(int s) { return select(s, s, width-2*s, height-2*s); }
  Area grow(int s) { return shrink(-s); }
  /// Select a sub-area in this area of size _size_
  Area select(pt size, Align where) {
    validate(where);
    pt tl2=([tl+pt((this.size.x-size.x)/2, 0), tl, tl+pt(this.size.x-size.x, 0)])[where&3];
    pt tl3=([tl2+pt(0, (this.size.y-size.y)/2), tl2, tl2+pt(0, this.size.y-size.y)])[where>>2];
    return Area(tl3, size, surf);
  }
  Area select(pt where, pt size, Align how) {
    return Area(tl+where+pt([-size.x/2, -size.x, 0][how&3], [-size.y/2, -size.y, 0][how>>2]), size, surf);
  }
  void blit(Area what) {
    auto srcrect=new SDL_Rect;
      with (what) (*srcrect)=SDL_Rect(cast(short) tl.x, cast(short) tl.y, cast(ushort) size.x, cast(ushort) size.y);
    auto dstrect=new SDL_Rect;
      (*dstrect)=SDL_Rect(cast(short) tl.x, cast(short) tl.y, cast(ushort) size.x, cast(ushort) size.y);
    SDL_BlitSurface(what.surface, srcrect, surface, dstrect);
  }
  void blit(Area what, Align how) {
    select(what.size, how).blit(what);
  }
  void pset(pt pos, ubyte r, ubyte g, ubyte b) {
    auto c = SDL_MapRGBA(surface.format, r, g, b, 0);
    .putpixel32(surface, (tl+pos).tuple, c);
  }
  string toString() {
    return Format("Area(", tl, ", size ", dimensions, ")");
  }
  bool opEquals(Area a) {
    if (!a) return false;
    if (!this) return false;
    return (surface==a.surface)&&(tl==a.tl)&&(dimensions==a.dimensions);
  }
}

union meep { rgb col; ubyte[4] ub; uint ui; }

void blitblend(Area dest, Area src, rgb tint, ubyte blend = 255) {
  if (dest.size != src.size) throw new Exception("can't blitblend different-sized areas");
  // clip dest to dest window
  if (dest.br.x < 0 || dest.br.y < 0) return; // out of surface, top left
  if (dest.tl.x !< dest.surface.w || dest.tl.y !< dest.surface.h) return; // out of surface, bottom right
  src = src.dup; dest = dest.dup;
  if (dest.tl.x < 0) {
    src.tl.x += -dest.tl.x; src.dimensions.x -= -dest.tl.x;
    dest.tl.x = 0;
  }
  if (dest.tl.y < 0) {
    src.tl.y += -dest.tl.y; src.dimensions.y -= -dest.tl.y;
    dest.tl.y = 0;
  }
  auto overhang_w = max(0, dest.br.x - dest.surface.w + 1);
  if (overhang_w) {
    src.dimensions.x -= overhang_w;
    dest.dimensions.x -= overhang_w;
  }
  auto overhang_h = max(0, dest.br.y - dest.surface.h + 1);
  if (overhang_h) {
    src.dimensions.y -= overhang_h;
    dest.dimensions.y -= overhang_h;
  }
  if (src.br.x < 0 || src.br.y < 0) throw new Exception("source surface: out of range (top/left)");
  if (src.tl.x !< src.surface.w || src.tl.y !< src.surface.h)
    throw new Exception("source surface: out of range (bottom/right)");
  auto w = src.dimensions.x, h = src.dimensions.y;
  auto srcxoffs = src.tl.x, srcyoffs = src.tl.y;
  auto destxoffs = dest.tl.x, destyoffs = dest.tl.y;
  for (int y = 0; y < h; ++y) {
    for (int x = 0; x < w; ++x) {
      meep src_rgba, dest_rgba; ubyte[4] a, b; // ubyte* a, b;
      getpixel32(src.surface, x + srcxoffs, y + srcyoffs, &a); src_rgba.ui = *cast(uint*) a.ptr;
      getpixel32(dest.surface, x + destxoffs, y + destyoffs, &b); dest_rgba.ui = *cast(uint*) b.ptr;
      src_rgba.col *= tint;
      src_rgba.ub[3] = cast(ubyte) ((src_rgba.ub[3] * blend) / 256);
      meep res;
      res.col = dest_rgba.col.blend(src_rgba.col, src_rgba.ub[3]);
      res.ub[3] = max(src_rgba.ub[3], dest_rgba.ub[3]);
      putpixel32(dest.surface, x + destxoffs, y + destyoffs, res.ui);
    }
  }
}
