module SDL_ttf;
import qd;

import tools.compat;

int deflt_size = 14;

struct fontsettings {
  bool bold, italic, underline;
  rgb color=Black; int size=-1;
}

import tools.base;

const SDL_Color SDL_White={255, 255, 255, 0};

TTF_FontClass[int] standard;
void initFont(int size = 14, string name="") {
  void[] data;
  if (name.length) data = name.read();
  else data = import("Vera.ttf");
  if (!standard.length) deflt_size = size;
  standard[size] = new TTF_FontClass(data, size);
}

int htoi(string s) {
  int res;
  foreach (ch; s) {
    if (ch >= '0' && ch <= '9') { res = res * 16 + (ch - '0'); continue; }
    if (ch >= 'a' && ch <= 'f') ch -= ('a' - 'A');
    if (ch >= 'A' && ch <= 'F') { res = res * 16 + (ch - 'A') + 10; continue; }
    throw new Exception("Not a hexadecimal string: "~s);
  }
  return res;
}

void prettyprint(T...)(T all) {
  static if (is(T[$-1] == fontsettings) && is(T[$-2]: string)) {
    auto param = all[0 .. $-2], text = all[$-2], deflt = all[$-1];
  } else static if (is(T[$-1]: string)) {
    auto param = all[0 .. $-1], text = all[$-1], deflt = Init!(fontsettings);
  } else static assert(false, "Invalid parameters for prettyprint: "~T.stringof);
  if (deflt.size == -1) deflt.size = deflt_size;
  string[] chunks;
  fontsettings[] sets;
  Stuple!(string, fontsettings)[] stack;
  fontsettings fs_top() {
    if (stack.length) {
      return stack[$-1]._1;
    } else return deflt;
  }
  fontsettings fs_pop(string compare) {
    if (stack[$-1]._0 != compare)
      throw new Exception("Mismatched closing tag: expected "~stack[$-1]._0~", got "~compare);
    return .pop(stack)._1;
  }
  void push(string text, string tag, void delegate(ref fontsettings fs) dg) {
    chunks ~= text;
    sets ~= fs_top();
    stack ~= stuple(tag, fs_top()); dg(stack[$-1]._1);
  }
  void pop(string text, string tag) {
    chunks ~= text;
    sets ~= fs_pop(tag);
  }
  void delegate(string, ref string) close_tag(string tag) {
    return stuple(&pop, tag) /apply/ (typeof(&pop) p, string tag, string pre, ref string post) { return p(pre, tag); };
  }
  void delegate(string, ref string) [string] tags = [
    "[b]"[]: (string pre, ref string post) {
      push(pre, "b", (ref fontsettings fs) { fs.bold = true; });
    }, "[/b]": close_tag("b"),
    "[i]": (string pre, ref string post) {
      push(pre, "i", (ref fontsettings fs) { fs.italic = true; });
    }, "[/i]": close_tag("i"),
    "[u]": (string pre, ref string post) {
      push(pre, "u", (ref fontsettings fs) { fs.underline = true; });
    }, "[/u]": close_tag("u"),
    "[color ": (string pre, ref string post) {
      string color; ptuple(color, post) = post.splitAt("]");
      //                            color.length != 3 || color.length != 6 -- see, it's shorter :)
      color = color.startsWith("#");
      if (!color || color.length != 3 /or/ 6)
        throw new Exception("Colors except #xxx and #xxxxxx not supported: "~color~".");
      if (color.length == 3) color = color[0]~"0"~color[1]~"0"~color[2]~"0";
      auto res = rgb(color[0 .. 2].htoi(), color[2 .. 4].htoi(), color[4 .. 6].htoi());
      push(pre, "color", (ref fontsettings fs) { fs.color = res; });
    }, "[/color]": close_tag("color"),
    "[size ": (string pre, ref string post) {
      int dir;
      if (post[0] == '+') dir = 1;
      else if (post[0] == '-') dir = -1;
      if (post[0] == '+' /or/ '-') post = post[1 .. $];
      string s; ptuple(s, post) = post.splitAt("]");
      auto n = s.atoi();
      push(pre, "size", (ref fontsettings fs) {
        if (!dir) fs.size = n;
        else fs.size += n * dir;
      });
    }, "[/size]": close_tag("size")
  ];
  text.glomp_parse(tags, (string s) {
    if (stack.length) throw new Exception("Unclosed tag: "~stack[$-1]._0);
    if (s.length) { chunks ~= s; sets ~= deflt; }
  });
  print(param, sets, chunks);
}

void _print(Area delegate(int w, int h) dg, int maxwidth, int delegate() linebreak, fontsettings[] settings, string[] strings) in {
    assert(settings.length == strings.length);
  } body {
  Area[] surfs;
  scope(exit) foreach (surf; surfs) delete surf;
  int width, height, fullwidth, fullheight;
  int[] heights;
  void newline(string str, bool lbcall = true) {
    if (lbcall && linebreak) { auto value = linebreak(); if (value) height = value; } // override
    if (width > fullwidth) fullwidth = width;
    fullheight += height;
    heights ~= height;
    // logln("newline -> ", fullwidth, ", ", fullheight, " <- ", width, ", ", height);
    // logln("Remain: ", str);
    height = width = 0;
    if (lbcall) surfs ~= null; // marker
  }
  foreach (i, str; strings) {
    auto s = settings[i];
    if (!(s.size in standard)) initFont(s.size);
redo:
    surfs ~= standard[s.size].render_linebreak(str, maxwidth, s);
    width += surfs[$-1].width;
    if (surfs[$-1].height > height) height = surfs[$-1].height;
    if (str.length) {
      newline(str);
      goto redo;
    }
  }
  newline("", false);
  //auto target = display.select(pt(x, y), pt(width, height), how);
  auto target = dg(fullwidth, fullheight);
  if (!target) return;
  auto mysurf = target.select(0, 0);
  foreach (surf; surfs) {
    if (!surf) {
      target = target.select(0, heights[0]); heights = heights[1 .. $];
      mysurf = target.select(0, 0);
    } else {
      mysurf.blit(surf, Top|Left);
      mysurf.width = surf.width - width; // yes this is correct.
      mysurf.tl.x += surf.width;
      width -= surf.width;
    }
  }
}

import tools.base;

template _SelectSet(TH /* TupleWrapper */, TYPES...) {
  static if (!TYPES.length) {
    alias Tuple!() values;
  } else {
    const value = Select!(TYPES[0], TH.Tuple);
    static if (value == -1) alias TH TH2;
    else alias TupleWrapper!(TH.Tuple[0..value], TH.Tuple[value+1 .. $]) TH2;
    static assert(Select!(TYPES[0], TH2.Tuple) == -1, "Duplicate "~TYPES[0].stringof~"!");
    alias Tuple!(value, _SelectSet!(TH, TYPES[1..$]).values) values;
  }
}

template SelectSet(TH, TYPES...) { alias _SelectSet!(TH, TYPES).values SelectSet; }

typedef int _linebreak;
_linebreak Linebreak(int width) { return cast(_linebreak) width; }

void print(T...)(T params) {
  static if (is(T[0]: int) && is(T[1]: int)) {
    Area fn(int w, int h, Align how) { return display.select(pt(params[0], params[1]), pt(w, h), how); }
    alias T[2 .. $] Rest; const offs = 2;
  } else static if (is(T[0] == pt)) {
    Area fn(int w, int h, Align how) { return display.select(params[0], pt(w, h), how); }
    alias T[1 .. $] Rest; const offs = 1;
  } else static if (is(T[0] == Area)) {
    Area fn(int w, int h, Align how) { return params[0].select(pt(w, h), how); }
    alias T[1 .. $] Rest; const offs = 1;
  } else static if (is(T[0] == Area delegate(int w, int h))) {
    Area fn(int w, int h, Align how) { return params[0](w, h); }
    alias T[1 .. $] Rest; const offs = 1;
  } else static assert(false, "print: "~T.stringof~": no position");
  
  alias SelectSet!(TupleWrapper!(TupleMap!(Unstatic, Rest)),
    string, Align, rgb, string[], fontsettings[], back_rgb, _linebreak, int delegate()) POS;
  fontsettings foo;
  foo.size = deflt_size;
  static assert(POS[0] != -1 ^ POS[3] != -1, "print needs string parameter xor string[] parameter");
  static if (POS[0] != -1) auto strings = ([params[offs+POS[0]]])[];
  else auto strings = params[offs+POS[3]];
  static if (POS[1] != -1) auto how = params[offs+POS[1]];
  else auto how = Center;
  static if (POS[2] != -1) foo.color = params[offs+POS[2]];
  else foo.color = White;
  static if (POS[4] != -1) auto fs_array = params[offs+POS[4]];
  else auto fs_array = ([foo])[];
  bool fill; rgb fillcol;
  static if (POS[5] != -1) { fill = true; fillcol = params[offs+POS[5]]; }
  int width = -1; int delegate() dg;
  static if (POS[6] != -1) {
    width = params[offs+POS[6]];
    static assert(POS[7] == POS[6] + 1, "Linebreak = num, { ... }?");
    dg = params[offs+POS[7]];
  }
  _print((int w, int h) {
    auto res = fn(w, h, how);
    if (fill) line(res.tl, res.br, Fill=fillcol);
    return res;
  }, width, dg, fs_array, strings);
}

class TTF_FontClass {
  static this() { if (!TTF_WasInit) TTF_Init; }
  static ~this() { if (TTF_WasInit) TTF_Quit; }
  private TTF_Font *font;
  int height() { return TTF_FontHeight(font); }
  int ascent() { return TTF_FontAscent(font); }
  int descent() { return TTF_FontDescent(font); }
  int lineskip() { return TTF_FontLineSkip(font); }
  static fontsettings Default;
  int getWidth(char[] text) {
    int w, h; TTF_SizeUTF8(font, toStringz(text), &w, &h);
    return w;
  }
  int curStyle;
  private SDL_Surface *_render(char[] text, fontsettings s=Default, int rendermode=2, SDL_Color *bg=null) {
    //logln("Rendering ", text);
    /// Make sure no two routines change font settings at the same time
    synchronized {
      int style=void;
      with (s) style=(bold?1:0) + (italic?2:0) + (underline?4:0);
      if (curStyle!=style) TTF_SetFontStyle(font, style);
      curStyle=style;
      /// Text mode: 0=Latin1, 1=UTF8, 2=Unicode
      switch(rendermode) {
        case 0: // Solid
          return TTF_RenderUTF8_Solid(font, toStringz(text), s.color.toSDL());
        case 1: // Shaded
          if (!bg) throw new Exception("Shaded selected but no background color given");
          return TTF_RenderUTF8_Shaded(font, toStringz(text), s.color.toSDL(), *bg);
        case 2: // Blended
          return TTF_RenderUTF8_Blended(font, toStringz(text), s.color.toSDL());
        default: throw new Exception("Invalid case");
      }
    }
    assert(false);
  }
  Area render(string text, fontsettings s=Default) {
    if (!text.length) return Area(SDL_CreateRGBSurface(0, 1, 1, 32));
    auto res=_render(text, s);
    if (!res) throw new Exception("Couldn't render "~'"'~text~'"'~": "~.toString(SDL_GetError));
    return Area(res);
  }
  Area render_linebreak(ref string text, int width, fontsettings s=Default) {
    if (width == -1) {
      scope(exit) text = null;
      return render(text, s);
    }
    auto start_text = text, len = text.length;
    scope(exit) text = start_text[text.length .. $];
    
    if (!text.length) return Area(SDL_CreateRGBSurface(0, 1, 1, 32));
    int change;
    retry:
    auto res=_render(text, s);
    if (!res) throw new Exception("Couldn't render "~'"'~text~'"'~": "~.toString(SDL_GetError));
    if (res.w > width) {
      SDL_FreeSurface(res);
      if (!change) change = start_text.length / 2 + 1;
      len = len - change; change = change / 2 + 1;
      text = start_text[0 .. len];
      goto retry;
    }
    if (change <= 2) return Area(res);
    len = len + change; change = change / 2 + 1;
    text = start_text[0 .. len];
    goto retry;
  }
  void[] file_buffer;
  this(void[] file, int ptsize) {
    file_buffer = file;
    font=TTF_OpenFontRW(SDL_RWFromMem(file.ptr, file.length), 1, ptsize);
    if (!font) throw new Exception("TTF_FontClass.this: Couldn't open font: "~.toString(SDL_GetError));
  }
  ~this() { TTF_CloseFont(font); }
}

extern(C) {
  alias void TTF_Font; /// Opaque struct
  // General
    // Activation
    int TTF_Init();
    int TTF_WasInit();
    void TTF_Quit();
    // Errors
    /// Just use SDL_GetError.
  // Management
    // Loading
    TTF_Font *TTF_OpenFontRW(SDL_RWops *src, int freesrc, int ptsize);
    TTF_Font *TTF_OpenFontIndex(char *file, int ptsize, long index);
    // Freeing
    void TTF_CloseFont(TTF_Font *font);
  // Attributes
    // Global Attributes
    void TTF_ByteSwappedUNICODE(int swapped);
    // Font Style
    int TTF_GetFontStyle(TTF_Font *font);
    void TTF_SetFontStyle(TTF_Font *font, int style);
    // Font Metrics
    int TTF_FontHeight(TTF_Font *font);
    int TTF_FontAscent(TTF_Font *font);
    int TTF_FontDescent(TTF_Font *font);
    int TTF_FontLineSkip(TTF_Font *font); /// Recommended pixel height of a rendered line
    // Face Attributes
    long TTF_FontFaces(TTF_Font *font);
    int TTF_FontFaceIsFixedWidth(TTF_Font *font);
    char *TTF_FontFaceFamilyName(TTF_Font *font);
    char *TTF_FontFaceStyleName(TTF_Font *font);
    // Glyph Metrics
    int TTF_GlyphMetrics(TTF_Font *font, ushort unichar,
                         int *minx, int *maxx,
                         int *miny, int *maxy,
                         int *advance
                        );
    // Text Metrics
    int TTF_SizeText(TTF_Font *font, char *text, int *w, int *h);
    int TTF_SizeUTF8(TTF_Font *font, char *text, int *w, int *h);
    int TTF_SizeUNICODE(TTF_Font *font, wchar *text, int *w, int *h);
  // Render
    // Solid
    SDL_Surface *TTF_RenderText_Solid(TTF_Font *font, char *text, SDL_Color fg);
    SDL_Surface *TTF_RenderUTF8_Solid(TTF_Font *font, char *text, SDL_Color fg);
    SDL_Surface *TTF_RenderUNICODE_Solid(TTF_Font *font, wchar *text, SDL_Color fg);
    SDL_Surface *TTF_RenderGlyph_Solid(TTF_Font *font, ushort unichar, SDL_Color fg);
    // Shaded
    SDL_Surface *TTF_RenderText_Shaded(TTF_Font *font, char *text, SDL_Color fg, SDL_Color bg);
    SDL_Surface *TTF_RenderUTF8_Shaded(TTF_Font *font, char *text, SDL_Color fg, SDL_Color bg);
    SDL_Surface *TTF_RenderUNICODE_Shaded(TTF_Font *font, wchar *text, SDL_Color fg, SDL_Color bg);
    SDL_Surface *TTF_RenderGlyph_Shaded(TTF_Font *font, ushort unichar, SDL_Color fg, SDL_Color bg);
    // Blended
    SDL_Surface *TTF_RenderText_Blended(TTF_Font *font, char *text, SDL_Color fg);
    SDL_Surface *TTF_RenderUTF8_Blended(TTF_Font *font, char *text, SDL_Color fg);
    SDL_Surface *TTF_RenderUNICODE_Blended(TTF_Font *font, wchar *text, SDL_Color fg);
    SDL_Surface *TTF_RenderGlyph_Blended(TTF_Font *font, ushort unichar, SDL_Color fg);
}
