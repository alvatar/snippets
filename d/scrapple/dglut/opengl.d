module dglut.opengl;
import dglut.vector;
import std.stdio;

extern(System) {
  typedef uint GLenum;
  typedef uint GLbitfield;
  typedef uint GLuint; // different on 64-bit?
  typedef GLenum glPrimitive;
  const glPrimitive GL_POINTS=0, GL_LINES=1, GL_LINE_LOOP=2, GL_LINE_STRIP=3, GL_TRIANGLES=4,
    GL_TRIANGLE_STRIP=5, GL_TRIANGLE_FAN=6, GL_QUADS=7, GL_QUAD_STRIP=8, GL_POLYGON=9;
  void glBegin(glPrimitive mode);
  void glEnd();
}

template Prepend (string S, T...) {
  static if (!T.length) alias Tuple!() Prepend;
  else {
    static if (is(typeof(T[0]): string)) alias Tuple!(S~T[0], Prepend!(S, T[1..$])) Prepend;
    else alias Tuple!(T[0], Prepend!(S, T[1..$])) Prepend;
  }
}

template Append(string S, T...) {
  static if (!T.length) alias Tuple!() Append;
  else {
    static if (is(typeof(T[0]): string)) alias Tuple!(T[0]~S, Append!(S, T[1..$])) Append;
    else alias Tuple!(T[0], Append!(S, T[1..$])) Append;
  }
}

template Find(string S, string T) {
  static if(S.length<T.length) { const size_t Find=size_t.max; }
  else {
    static if (S[0..T.length] == T) const size_t Find=0;
    else static if (Find!(S[1..$], T)==size_t.max) const size_t Find=size_t.max;
    else const size_t Find=Find!(S[1..$], T) + 1;
  }
}

template Split(string S, string SEP) {
  static if (Find!(S, SEP)==size_t.max) alias Tuple!(S) Split;
  else alias Tuple!(S[0..Find!(S, SEP)], Split!(S[Find!(S, SEP)+SEP.length .. $], SEP)) Split;
}

template StringCross(T, U) {
  static if (T.Tuple.length==1) alias Prepend!(T.Tuple[0], U.Tuple) StringCross;
  else alias Tuple!(Prepend!(T.Tuple[0], U.Tuple), StringCross!(TupleWrapper!(T.Tuple[1..$]), U)) StringCross;
}

template Expand(string E) {
  static if (!E.length) alias Tuple!("") Expand;
  else {
    static if (E[0]=='{') {
      static if(Find!(E, "}")==size_t.max) {
        pragma(msg, "You forgot a closing bracket in \""~E~"\"");
        static assert(false);
      }
      alias StringCross!(TupleWrapper!(Split!(E[1..Find!(E, "}")], "|")), TupleWrapper!(Expand!(E[Find!(E, "}")+1 .. $]))) Expand;
    } else {
      alias Prepend!(""~E[0], Expand!(E[1..$])) Expand;
    }
  }
}

template ConstEnum(T, string MOD, SYMBOLS...) {
  static assert(SYMBOLS.length>1);
  static assert(is(typeof(SYMBOLS[0]): long));
  static if (SYMBOLS.length>1) {
    static if (is(typeof(SYMBOLS[1]): long)) {
      static if (SYMBOLS.length>2) mixin ConstEnum!(T, MOD, SYMBOLS[1..$]);
    } else {
      mixin("T "~SYMBOLS[1]~"=SYMBOLS[0]; ");
      static if (SYMBOLS.length>2) mixin ConstEnum!(T, MOD, mixin("SYMBOLS[0]"~MOD), SYMBOLS[2..$]);
    }
  }
}

template PrimitiveScope(string NAME, string WHICH) {
  mixin("void "~NAME~"(void delegate()[] dgs...) {
    glBegin("~WHICH~"); scope(exit) glEnd();
    foreach (dg; dgs) dg();
  }");
}

template MultiMixin(alias T, int count, U...) {
  mixin T!(U[0..count]);
  static if (U.length>count) mixin MultiMixin!(T, count, U[count..$]);
}

mixin MultiMixin!(PrimitiveScope, 2,
  "Points", "GL_POINTS", "Lines", "GL_LINES",
  "LineLoop", "GL_LINE_LOOP", "LineStrip", "GL_LINE_STRIP",
  "Triangles", "GL_TRIANGLES", "TriangleStrip", "GL_TRIANGLE_STRIP",
  "TriangleFan", "GL_TRIANGLE_FAN", "Quads", "GL_QUADS",
  "QuadStrip", "GL_QUAD_STRIP", "Polygon", "GL_POLYGON"
);

string glTypeDecode(string s) {
  if (s=="b") return "byte";
  if (s=="d") return "double";
  if (s=="f") return "float";
  if (s=="i") return "int";
  if (s=="s") return "short";
  if (s=="ub") return "ubyte";
  if (s=="ui") return "uint";
  if (s=="us") return "ushort";
  assert(false);
}

string glTypeEncode(string s) {
  if (s[0]=='u') return s[0..2];
  return s[0..1];
}

template Repeat(int count, T) {
  static if (!count) alias Tuple!() Repeat;
  else alias Tuple!(T, Repeat!(count-1, T)) Repeat;
}

size_t find(string where, string what) {
  size_t counter=0;
  while (where.length>=what.length) {
    if (where[0..what.length]==what) return counter;
    where=where[1..$];
    ++counter;
  }
  return size_t.max;
}

string glFunctions(string name, string params, string types) {
  string res;
  while (types.length) {
    string type;
    if (find(types, "|")==size_t.max) { type=types; types=""; }
    else { type=types[0..find(types, "|")]; types=types[find(types, "|")+1..$]; }
    foreach (param; params) {
      
      res~="extern(System) void "~name~param~type~"(Repeat!("~param~", "~glTypeDecode(type)~")); ";
      res~="extern(System) void "~name~param~type~"v("~glTypeDecode(type)~" *); ";
      res~="void "~name~"(Repeat!("~param~", "~glTypeDecode(type)~") par) {
        "~name~param~type~"(par);
      }";
      res~="void "~name~"("~glTypeDecode(type)~"["~param~"] par) {
        "~name~param~type~"v(par.ptr);
      }";
    }
  }
  return res;
}

mixin(glFunctions("glVertex", "234", "d|f|i|s"));
mixin(glFunctions("glNormal", "3", "b|d|f|i|s"));
mixin(glFunctions("glTexCoord", "1234", "d|f|i|s"));
mixin(glFunctions("glColor", "34", "b|d|f|i|s|ub|ui|us"));
mixin(glFunctions("glRasterPos", "234", "d|f|i|s"));

void Vertex(T...)(T t) { static if (T.length==1) glVertex(t[0].parts); else glVertex(t); }
void Normal(T...)(T t) { static if (T.length==1) glNormal(t[0].parts); else glNormal(t); }
void TexCoord(T...)(T t) { static if (T.length==1) glTexCoord(t[0].parts); else glTexCoord(t); }
void Color(T...)(T t) { static if (T.length==1) glColor(t[0].parts); else glColor(t); }
void RasterPos(T...)(T t) { static if (T.length==1) glRasterPos(t[0].parts); else glRasterPos(t); }

extern(System) {
  void glPushMatrix();
  void glPopMatrix();
}

void MatrixScope(void delegate()[] dgs...) {
  glPushMatrix();
  scope(exit) glPopMatrix;
  foreach (dg; dgs) dg();
}

extern(System) {
  void glMatrixMode(MatrixModeType);
  typedef GLenum MatrixModeType;
  const MatrixModeType GL_MODELVIEW=0x1700, GL_PROJECTION=0x1701, GL_TEXTURE=0x1702;
}

// so as to prevent conflicts between Texture and class Texture later on
struct MatrixMode {
  static void Modelview() { glMatrixMode(GL_MODELVIEW); }
  static void Projection() { glMatrixMode(GL_PROJECTION); }
  static void Texture() { glMatrixMode(GL_TEXTURE); }
}

extern(System) {
  const GLenum GL_NONE=0;
  mixin ConstEnum!(GLenum, "+1", 0x0400,
    Expand!("GL_{FRONT|BACK}_{LEFT|RIGHT}"),
    Expand!("GL_{FRONT|BACK|LEFT|RIGHT}"),
    "GL_FRONT_AND_BACK",
    Expand!("GL_AUX{0|1|2|3}")
  );
  typedef GLenum LightParameter;
  mixin ConstEnum!(LightParameter, "+1", 0x1200,
    Prepend!("GL_",
      Tuple!(
        "AMBIENT", "DIFFUSE", "SPECULAR", "POSITION",
        Expand!("SPOT_{DIRECTION|EXPONENT|CUTOFF}"),
        Expand!("{CONSTANT|LINEAR|QUADRATIC}_ATTENUATION")
      )
    )
  );
  typedef GLenum MaterialParameter;
  mixin ConstEnum!(MaterialParameter, "+1", 0x1600,
    Prepend!("GL_", Tuple!("EMISSION", "SHININESS", "AMBIENT_AND_DIFFUSE", "COLOR_INDEXES"))
  );
  void glMaterialf(GLenum face, GLenum pname, float param);
  void glMaterialfv(GLenum face, GLenum pname, float *param);
  void glMateriali(GLenum face, GLenum pname, int param);
  void glMaterialiv(GLenum face, GLenum pname, int *param);
}

template MaterialNestedStruct(string NAME, string GLNAME) {
  mixin("struct "~NAME~" {
    static void ambient(float[4] col) { glMaterialfv("~GLNAME~", GL_AMBIENT, col.ptr); }
    static void diffuse(float[4] col) { glMaterialfv("~GLNAME~", GL_DIFFUSE, col.ptr); }
    static void ambient_and_diffuse(float[4] col) { glMaterialfv("~GLNAME~", GL_AMBIENT_AND_DIFFUSE, col.ptr); }
    static void specular(float[4] col) { glMaterialfv("~GLNAME~", GL_SPECULAR, col.ptr); }
    static void shininess(float s) { glMaterialf("~GLNAME~", GL_SHININESS, s); }
    static void emission(float[4] e) { glMaterialfv("~GLNAME~", GL_EMISSION, e.ptr); }
    static void emission(int[4] i) { glMaterialiv("~GLNAME~", GL_COLOR_INDEXES, i.ptr); }
  }");
}

struct Material {
  mixin MultiMixin!(MaterialNestedStruct, 2,  "Front", "GL_FRONT", "Back", "GL_BACK", "FrontAndBack", "GL_FRONT_AND_BACK");
}

extern(System) {
  void glTranslated(Repeat!(3, double) xyz);
  void glTranslatef(Repeat!(3, float) xyz);
}
void Translate(T...)(T what) {
  static if (is(T[0]==double) || is(T[0]==real)) glTranslated(what);
  else glTranslatef(what);
}

extern(System) {
  void glRotated(double angle, Repeat!(3, double) xyz);
  void glRotatef(float angle, Repeat!(3, float) xyz);
}

void Rotate(T...)(T what) {
  static if (is(T[0]==double) || is(T[0]==real)) glRotated(what);
  else glRotatef(what);
}

extern(System) {
  void glClear(GLbitfield mask);
  typedef GLbitfield AttribMask;
  mixin ConstEnum!(AttribMask, "*2", 1, Prepend!("GL_",
    Append!("_BIT",
      Expand!("{CURRENT|POINT|LINE|POLYGON|POLYGON_STIPPLE|PIXEL_MODE|LIGHTING|FOG|DEPTH_BUFFER|ACCUM_BUFFER|STENCIL_BUFFER"~
        "|VIEWPORT|TRANSFORM|ENABLE|COLOR_BUFFER|HINT|EVAL|LIST|TEXTURE|SCISSOR}"
      )
    )
  ));
  const AttribMask GL_ALL_ATTRIB_BITS=0xFFFFFFFF;
  void glClearColor(Repeat!(4, float) rgb);
  void glFlush();
  void glPushAttrib(GLbitfield);
  void glPopAttrib();
}

extern(System) {
  void glEnable(GLenum);
  void glDisable(GLenum);
  // sorry, but using templates would have been too much to ask from the compiler
  // (I tried; the resulting binary was 40M big)
  // yay for sed!
  // cat gl.h |sed -e "s@#define@const GLenum@" -e "s@ *0x\(.*\)@ = 0x\1;@" |less
  const GLenum GL_CURRENT_COLOR = 0x0B00;
  const GLenum GL_CURRENT_INDEX = 0x0B01;
  const GLenum GL_CURRENT_NORMAL = 0x0B02;
  const GLenum GL_CURRENT_TEXTURE_COORDS = 0x0B03;
  const GLenum GL_CURRENT_RASTER_COLOR = 0x0B04;
  const GLenum GL_CURRENT_RASTER_INDEX = 0x0B05;
  const GLenum GL_CURRENT_RASTER_TEXTURE_COORDS = 0x0B06;
  const GLenum GL_CURRENT_RASTER_POSITION = 0x0B07;
  const GLenum GL_CURRENT_RASTER_POSITION_VALID = 0x0B08;
  const GLenum GL_CURRENT_RASTER_DISTANCE = 0x0B09;
  const GLenum GL_POINT_SMOOTH = 0x0B10;
  const GLenum GL_POINT_SIZE = 0x0B11;
  const GLenum GL_SMOOTH_POINT_SIZE_RANGE = 0x0B12;
  const GLenum GL_SMOOTH_POINT_SIZE_GRANULARITY = 0x0B13;
  const GLenum GL_POINT_SIZE_RANGE = GL_SMOOTH_POINT_SIZE_RANGE;
  const GLenum GL_POINT_SIZE_GRANULARITY = GL_SMOOTH_POINT_SIZE_GRANULARITY;
  const GLenum GL_LINE_SMOOTH = 0x0B20;
  const GLenum GL_LINE_WIDTH = 0x0B21;
  const GLenum GL_SMOOTH_LINE_WIDTH_RANGE = 0x0B22;
  const GLenum GL_SMOOTH_LINE_WIDTH_GRANULARITY = 0x0B23;
  const GLenum GL_LINE_WIDTH_RANGE = GL_SMOOTH_LINE_WIDTH_RANGE;
  const GLenum GL_LINE_WIDTH_GRANULARITY = GL_SMOOTH_LINE_WIDTH_GRANULARITY;
  const GLenum GL_LINE_STIPPLE = 0x0B24;
  const GLenum GL_LINE_STIPPLE_PATTERN = 0x0B25;
  const GLenum GL_LINE_STIPPLE_REPEAT = 0x0B26;
  const GLenum GL_LIST_MODE = 0x0B30;
  const GLenum GL_MAX_LIST_NESTING = 0x0B31;
  const GLenum GL_LIST_BASE = 0x0B32;
  const GLenum GL_LIST_INDEX = 0x0B33;
  const GLenum GL_POLYGON_MODE = 0x0B40;
  const GLenum GL_POLYGON_SMOOTH = 0x0B41;
  const GLenum GL_POLYGON_STIPPLE = 0x0B42;
  const GLenum GL_EDGE_FLAG = 0x0B43;
  const GLenum GL_CULL_FACE = 0x0B44;
  const GLenum GL_CULL_FACE_MODE = 0x0B45;
  const GLenum GL_FRONT_FACE = 0x0B46;
  const GLenum GL_LIGHTING = 0x0B50;
  const GLenum GL_LIGHT_MODEL_LOCAL_VIEWER = 0x0B51;
  const GLenum GL_LIGHT_MODEL_TWO_SIDE = 0x0B52;
  const GLenum GL_LIGHT_MODEL_AMBIENT = 0x0B53;
  const GLenum GL_SHADE_MODEL = 0x0B54;
  const GLenum GL_COLOR_MATERIAL_FACE = 0x0B55;
  const GLenum GL_COLOR_MATERIAL_PARAMETER = 0x0B56;
  const GLenum GL_COLOR_MATERIAL = 0x0B57;
  const GLenum GL_FOG = 0x0B60;
  const GLenum GL_FOG_INDEX = 0x0B61;
  const GLenum GL_FOG_DENSITY = 0x0B62;
  const GLenum GL_FOG_START = 0x0B63;
  const GLenum GL_FOG_END = 0x0B64;
  const GLenum GL_FOG_MODE = 0x0B65;
  const GLenum GL_FOG_COLOR = 0x0B66;
  const GLenum GL_DEPTH_RANGE = 0x0B70;
  const GLenum GL_DEPTH_TEST = 0x0B71;
  const GLenum GL_DEPTH_WRITEMASK = 0x0B72;
  const GLenum GL_DEPTH_CLEAR_VALUE = 0x0B73;
  const GLenum GL_DEPTH_FUNC = 0x0B74;
  const GLenum GL_ACCUM_CLEAR_VALUE = 0x0B80;
  const GLenum GL_STENCIL_TEST = 0x0B90;
  const GLenum GL_STENCIL_CLEAR_VALUE = 0x0B91;
  const GLenum GL_STENCIL_FUNC = 0x0B92;
  const GLenum GL_STENCIL_VALUE_MASK = 0x0B93;
  const GLenum GL_STENCIL_FAIL = 0x0B94;
  const GLenum GL_STENCIL_PASS_DEPTH_FAIL = 0x0B95;
  const GLenum GL_STENCIL_PASS_DEPTH_PASS = 0x0B96;
  const GLenum GL_STENCIL_REF = 0x0B97;
  const GLenum GL_STENCIL_WRITEMASK = 0x0B98;
  const GLenum GL_MATRIX_MODE = 0x0BA0;
  const GLenum GL_NORMALIZE = 0x0BA1;
  const GLenum GL_VIEWPORT = 0x0BA2;
  const GLenum GL_MODELVIEW_STACK_DEPTH = 0x0BA3;
  const GLenum GL_PROJECTION_STACK_DEPTH = 0x0BA4;
  const GLenum GL_TEXTURE_STACK_DEPTH = 0x0BA5;
  const GLenum GL_MODELVIEW_MATRIX = 0x0BA6;
  const GLenum GL_PROJECTION_MATRIX = 0x0BA7;
  const GLenum GL_TEXTURE_MATRIX = 0x0BA8;
  const GLenum GL_ATTRIB_STACK_DEPTH = 0x0BB0;
  const GLenum GL_CLIENT_ATTRIB_STACK_DEPTH = 0x0BB1;
  const GLenum GL_ALPHA_TEST = 0x0BC0;
  const GLenum GL_ALPHA_TEST_FUNC = 0x0BC1;
  const GLenum GL_ALPHA_TEST_REF = 0x0BC2;
  const GLenum GL_DITHER = 0x0BD0;
  const GLenum GL_BLEND_DST = 0x0BE0;
  const GLenum GL_BLEND_SRC = 0x0BE1;
  const GLenum GL_BLEND = 0x0BE2;
  const GLenum GL_LOGIC_OP_MODE = 0x0BF0;
  const GLenum GL_INDEX_LOGIC_OP = 0x0BF1;
  const GLenum GL_LOGIC_OP = GL_INDEX_LOGIC_OP;
  const GLenum GL_COLOR_LOGIC_OP = 0x0BF2;
  const GLenum GL_AUX_BUFFERS = 0x0C00;
  const GLenum GL_DRAW_BUFFER = 0x0C01;
  const GLenum GL_READ_BUFFER = 0x0C02;
  const GLenum GL_SCISSOR_BOX = 0x0C10;
  const GLenum GL_SCISSOR_TEST = 0x0C11;
  const GLenum GL_INDEX_CLEAR_VALUE = 0x0C20;
  const GLenum GL_INDEX_WRITEMASK = 0x0C21;
  const GLenum GL_COLOR_CLEAR_VALUE = 0x0C22;
  const GLenum GL_COLOR_WRITEMASK = 0x0C23;
  const GLenum GL_INDEX_MODE = 0x0C30;
  const GLenum GL_RGBA_MODE = 0x0C31;
  const GLenum GL_DOUBLEBUFFER = 0x0C32;
  const GLenum GL_STEREO = 0x0C33;
  const GLenum GL_RENDER_MODE = 0x0C40;
  const GLenum GL_PERSPECTIVE_CORRECTION_HINT = 0x0C50;
  const GLenum GL_POINT_SMOOTH_HINT = 0x0C51;
  const GLenum GL_LINE_SMOOTH_HINT = 0x0C52;
  const GLenum GL_POLYGON_SMOOTH_HINT = 0x0C53;
  const GLenum GL_FOG_HINT = 0x0C54;
  const GLenum GL_TEXTURE_GEN_S = 0x0C60;
  const GLenum GL_TEXTURE_GEN_T = 0x0C61;
  const GLenum GL_TEXTURE_GEN_R = 0x0C62;
  const GLenum GL_TEXTURE_GEN_Q = 0x0C63;
  const GLenum GL_PIXEL_MAP_I_TO_I_SIZE = 0x0CB0;
  const GLenum GL_PIXEL_MAP_S_TO_S_SIZE = 0x0CB1;
  const GLenum GL_PIXEL_MAP_I_TO_R_SIZE = 0x0CB2;
  const GLenum GL_PIXEL_MAP_I_TO_G_SIZE = 0x0CB3;
  const GLenum GL_PIXEL_MAP_I_TO_B_SIZE = 0x0CB4;
  const GLenum GL_PIXEL_MAP_I_TO_A_SIZE = 0x0CB5;
  const GLenum GL_PIXEL_MAP_R_TO_R_SIZE = 0x0CB6;
  const GLenum GL_PIXEL_MAP_G_TO_G_SIZE = 0x0CB7;
  const GLenum GL_PIXEL_MAP_B_TO_B_SIZE = 0x0CB8;
  const GLenum GL_PIXEL_MAP_A_TO_A_SIZE = 0x0CB9;
  const GLenum GL_UNPACK_SWAP_BYTES = 0x0CF0;
  const GLenum GL_UNPACK_LSB_FIRST = 0x0CF1;
  const GLenum GL_UNPACK_ROW_LENGTH = 0x0CF2;
  const GLenum GL_UNPACK_SKIP_ROWS = 0x0CF3;
  const GLenum GL_UNPACK_SKIP_PIXELS = 0x0CF4;
  const GLenum GL_UNPACK_ALIGNMENT = 0x0CF5;
  const GLenum GL_PACK_SWAP_BYTES = 0x0D00;
  const GLenum GL_PACK_LSB_FIRST = 0x0D01;
  const GLenum GL_PACK_ROW_LENGTH = 0x0D02;
  const GLenum GL_PACK_SKIP_ROWS = 0x0D03;
  const GLenum GL_PACK_SKIP_PIXELS = 0x0D04;
  const GLenum GL_PACK_ALIGNMENT = 0x0D05;
  const GLenum GL_MAP_COLOR = 0x0D10;
  const GLenum GL_MAP_STENCIL = 0x0D11;
  const GLenum GL_INDEX_SHIFT = 0x0D12;
  const GLenum GL_INDEX_OFFSET = 0x0D13;
  const GLenum GL_RED_SCALE = 0x0D14;
  const GLenum GL_RED_BIAS = 0x0D15;
  const GLenum GL_ZOOM_X = 0x0D16;
  const GLenum GL_ZOOM_Y = 0x0D17;
  const GLenum GL_GREEN_SCALE = 0x0D18;
  const GLenum GL_GREEN_BIAS = 0x0D19;
  const GLenum GL_BLUE_SCALE = 0x0D1A;
  const GLenum GL_BLUE_BIAS = 0x0D1B;
  const GLenum GL_ALPHA_SCALE = 0x0D1C;
  const GLenum GL_ALPHA_BIAS = 0x0D1D;
  const GLenum GL_DEPTH_SCALE = 0x0D1E;
  const GLenum GL_DEPTH_BIAS = 0x0D1F;
  const GLenum GL_MAX_EVAL_ORDER = 0x0D30;
  const GLenum GL_MAX_LIGHTS = 0x0D31;
  const GLenum GL_MAX_CLIP_PLANES = 0x0D32;
  const GLenum GL_MAX_TEXTURE_SIZE = 0x0D33;
  const GLenum GL_MAX_PIXEL_MAP_TABLE = 0x0D34;
  const GLenum GL_MAX_ATTRIB_STACK_DEPTH = 0x0D35;
  const GLenum GL_MAX_MODELVIEW_STACK_DEPTH = 0x0D36;
  const GLenum GL_MAX_NAME_STACK_DEPTH = 0x0D37;
  const GLenum GL_MAX_PROJECTION_STACK_DEPTH = 0x0D38;
  const GLenum GL_MAX_TEXTURE_STACK_DEPTH = 0x0D39;
  const GLenum GL_MAX_VIEWPORT_DIMS = 0x0D3A;
  const GLenum GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = 0x0D3B;
  const GLenum GL_SUBPIXEL_BITS = 0x0D50;
  const GLenum GL_INDEX_BITS = 0x0D51;
  const GLenum GL_RED_BITS = 0x0D52;
  const GLenum GL_GREEN_BITS = 0x0D53;
  const GLenum GL_BLUE_BITS = 0x0D54;
  const GLenum GL_ALPHA_BITS = 0x0D55;
  const GLenum GL_DEPTH_BITS = 0x0D56;
  const GLenum GL_STENCIL_BITS = 0x0D57;
  const GLenum GL_ACCUM_RED_BITS = 0x0D58;
  const GLenum GL_ACCUM_GREEN_BITS = 0x0D59;
  const GLenum GL_ACCUM_BLUE_BITS = 0x0D5A;
  const GLenum GL_ACCUM_ALPHA_BITS = 0x0D5B;
  const GLenum GL_NAME_STACK_DEPTH = 0x0D70;
  const GLenum GL_AUTO_NORMAL = 0x0D80;
  const GLenum GL_MAP1_COLOR_4 = 0x0D90;
  const GLenum GL_MAP1_INDEX = 0x0D91;
  const GLenum GL_MAP1_NORMAL = 0x0D92;
  const GLenum GL_MAP1_TEXTURE_COORD_1 = 0x0D93;
  const GLenum GL_MAP1_TEXTURE_COORD_2 = 0x0D94;
  const GLenum GL_MAP1_TEXTURE_COORD_3 = 0x0D95;
  const GLenum GL_MAP1_TEXTURE_COORD_4 = 0x0D96;
  const GLenum GL_MAP1_VERTEX_3 = 0x0D97;
  const GLenum GL_MAP1_VERTEX_4 = 0x0D98;
  const GLenum GL_MAP2_COLOR_4 = 0x0DB0;
  const GLenum GL_MAP2_INDEX = 0x0DB1;
  const GLenum GL_MAP2_NORMAL = 0x0DB2;
  const GLenum GL_MAP2_TEXTURE_COORD_1 = 0x0DB3;
  const GLenum GL_MAP2_TEXTURE_COORD_2 = 0x0DB4;
  const GLenum GL_MAP2_TEXTURE_COORD_3 = 0x0DB5;
  const GLenum GL_MAP2_TEXTURE_COORD_4 = 0x0DB6;
  const GLenum GL_MAP2_VERTEX_3 = 0x0DB7;
  const GLenum GL_MAP2_VERTEX_4 = 0x0DB8;
  const GLenum GL_MAP1_GRID_DOMAIN = 0x0DD0;
  const GLenum GL_MAP1_GRID_SEGMENTS = 0x0DD1;
  const GLenum GL_MAP2_GRID_DOMAIN = 0x0DD2;
  const GLenum GL_MAP2_GRID_SEGMENTS = 0x0DD3;
  const GLenum GL_TEXTURE_1D = 0x0DE0;
  const GLenum GL_TEXTURE_2D = 0x0DE1;
  const GLenum GL_FEEDBACK_BUFFER_POINTER = 0x0DF0;
  const GLenum GL_FEEDBACK_BUFFER_SIZE = 0x0DF1;
  const GLenum GL_FEEDBACK_BUFFER_TYPE = 0x0DF2;
  const GLenum GL_SELECTION_BUFFER_POINTER = 0x0DF3;
  const GLenum GL_SELECTION_BUFFER_SIZE = 0x0DF4;
  const GLenum GL_POLYGON_OFFSET_UNITS = 0x2A00;
  const GLenum GL_POLYGON_OFFSET_POINT = 0x2A01;
  const GLenum GL_POLYGON_OFFSET_LINE = 0x2A02;
  const GLenum GL_POLYGON_OFFSET_FILL = 0x8037;
  const GLenum GL_POLYGON_OFFSET_FACTOR = 0x8038;
  const GLenum GL_TEXTURE_BINDING_1D = 0x8068;
  const GLenum GL_TEXTURE_BINDING_2D = 0x8069;
  const GLenum GL_TEXTURE_BINDING_3D = 0x806A;
  const GLenum GL_VERTEX_ARRAY = 0x8074;
  const GLenum GL_NORMAL_ARRAY = 0x8075;
  const GLenum GL_COLOR_ARRAY = 0x8076;
  const GLenum GL_INDEX_ARRAY = 0x8077;
  const GLenum GL_TEXTURE_COORD_ARRAY = 0x8078;
  const GLenum GL_EDGE_FLAG_ARRAY = 0x8079;
  const GLenum GL_VERTEX_ARRAY_SIZE = 0x807A;
  const GLenum GL_VERTEX_ARRAY_TYPE = 0x807B;
  const GLenum GL_VERTEX_ARRAY_STRIDE = 0x807C;
  const GLenum GL_NORMAL_ARRAY_TYPE = 0x807E;
  const GLenum GL_NORMAL_ARRAY_STRIDE = 0x807F;
  const GLenum GL_COLOR_ARRAY_SIZE = 0x8081;
  const GLenum GL_COLOR_ARRAY_TYPE = 0x8082;
  const GLenum GL_COLOR_ARRAY_STRIDE = 0x8083;
  const GLenum GL_INDEX_ARRAY_TYPE = 0x8085;
  const GLenum GL_INDEX_ARRAY_STRIDE = 0x8086;
  const GLenum GL_TEXTURE_COORD_ARRAY_SIZE = 0x8088;
  const GLenum GL_TEXTURE_COORD_ARRAY_TYPE = 0x8089;
  const GLenum GL_TEXTURE_COORD_ARRAY_STRIDE = 0x808A;
  const GLenum GL_EDGE_FLAG_ARRAY_STRIDE = 0x808C;
  const GLenum GL_TEXTURE_WIDTH = 0x1000;
  const GLenum GL_TEXTURE_HEIGHT = 0x1001;
  const GLenum GL_TEXTURE_INTERNAL_FORMAT = 0x1003;
  const GLenum GL_TEXTURE_COMPONENTS = GL_TEXTURE_INTERNAL_FORMAT;
  const GLenum GL_TEXTURE_BORDER_COLOR = 0x1004;
  const GLenum GL_TEXTURE_BORDER = 0x1005;
  const GLenum GL_TEXTURE_RED_SIZE = 0x805C;
  const GLenum GL_TEXTURE_GREEN_SIZE = 0x805D;
  const GLenum GL_TEXTURE_BLUE_SIZE = 0x805E;
  const GLenum GL_TEXTURE_ALPHA_SIZE = 0x805F;
  const GLenum GL_TEXTURE_LUMINANCE_SIZE = 0x8060;
  const GLenum GL_TEXTURE_INTENSITY_SIZE = 0x8061;
  const GLenum GL_TEXTURE_PRIORITY = 0x8066;
  const GLenum GL_TEXTURE_RESIDENT = 0x8067;
  const GLenum GL_DONT_CARE = 0x1100;
  const GLenum GL_FASTEST = 0x1101;
  const GLenum GL_NICEST = 0x1102;
  typedef GLenum LightSource;
  const LightSource GL_LIGHT0=0x4000,
    GL_LIGHT1=GL_LIGHT0+1, GL_LIGHT2=GL_LIGHT1+1,
    GL_LIGHT3=GL_LIGHT2+1, GL_LIGHT4=GL_LIGHT3+1,
    GL_LIGHT5=GL_LIGHT4+1, GL_LIGHT6=GL_LIGHT5+1,
    GL_LIGHT7=GL_LIGHT6+1;
}

extern(System) {
  void glLightf(LightSource, GLenum pname, float);
  void glLightfv(LightSource, GLenum pname, float *);
  void glLighti(LightSource, GLenum pname, int);
  void glLightiv(LightSource, GLenum pname, int *);
}
template LightProperty(string name, GLenum GL, int params) {
  mixin("void "~name~"("~(params?"float["~params.stringof~"]":"float")~" par) {
    glLightf"~(params?"v":"")~"(index, GL, par"~(params?".ptr":"")~");
  }");
}
struct Light {
  LightSource index;
  void position(float[3] par, bool point=true) {
    float[4] fullpar; fullpar[0..3]=par;
    if (point) fullpar[3]=1f; else fullpar[3]=0f;
    glLightfv(index, GL_POSITION, fullpar.ptr);
  }
  void enable() { glEnable(index); }
  mixin MultiMixin!(LightProperty, 3,
    "ambient", GL_AMBIENT, 4,             "diffuse", GL_DIFFUSE, 4,
    "specular", GL_SPECULAR, 4,           "spot_direction", GL_SPOT_DIRECTION, 3,
    "spot_exponent", GL_SPOT_EXPONENT, 0, "spot_cutoff", GL_SPOT_CUTOFF, 0,
    "constant_attenuation", GL_CONSTANT_ATTENUATION, 0,
    "linear_attenuation", GL_LINEAR_ATTENUATION, 0,
    "quadratic_attenuation", GL_QUADRATIC_ATTENUATION, 0
  );
  static Light opIndex(size_t which) {
    assert(which<8, "Allowed number of light sources exceeded!");
    return Light(cast(LightSource)(GL_LIGHT0+which));
  }
}

extern(System) {
  typedef GLenum ErrorCode;
  mixin ConstEnum!(ErrorCode, "+1", Prepend!(
      "GL_", 0, "NO_ERROR", 0x0500, Expand!("INVALID_{ENUM|VALUE|OPERATION}"),
      Expand!("STACK_{OVER|UNDER}FLOW"), "OUT_OF_MEMORY",
      0x8031, "TABLE_TOO_LARGE"
    )
  );
  char *gluErrorString(ErrorCode);
  ErrorCode glGetError();
}

string glErrorToString(ErrorCode err) {
  auto res=gluErrorString(err), cur=res;
  while (*cur) cur++;
  return res[0..(cur-res)];
}

import std.string: toString;
void glChecked(string label, void delegate()[] dgs...) {
  foreach (idx, dg; dgs) {
    if (auto err=glGetError())
      if (!idx) throw new Exception("OpenGL error before checked section"~(label.length?" \""~label~"\"":"")~": "~glErrorToString(err));
      else throw new Exception("OpenGL error in "~(label.length?"\""~label~"\", ":"")~"command "~toString(idx)~": "~glErrorToString(err));
    dg();
  }
  if (auto err=glGetError()) {
    if (label.length) throw new Exception("OpenGL error in \""~label~"\""~(dgs.length>1?", last command":"")~": "~glErrorToString(err));
    else throw new Exception("OpenGL error in"~(dgs.length>1?" last":"")~" command: "~glErrorToString(err));
  }
}

extern(System) {
  void glGenTextures(int n, uint *textures);
  bool glIsTexture(uint texture);
  void glBindTexture(GLenum target, uint texture);
  void glDeleteTextures(int n, uint *textures);
  
  void glTexImage2D(GLenum target, int level, PixelFormat internalFormat, int width, int height, int border, PixelFormat, DataType, void *pixels);
  void gluBuild2DMipmaps(GLenum target, PixelFormat internalFormat, int width, int height, PixelFormat, DataType, void *pixels);
  void glTexParameterf(GLenum target, GLenum pname, float param);
  void glTexParameterfv(GLenum target, GLenum pname, float *param);
  void glTexParameteri(GLenum target, GLenum pname, int param);
  void glTexParameteriv(GLenum target, GLenum pname, int *param);
  typedef GLenum PixelFormat;
  mixin ConstEnum!(PixelFormat, "+1", 0x1900, Prepend!("GL_",
      "COLOR_INDEX", "STENCIL_INDEX", "DEPTH_COMPONENT",
      "RED", "GREEN", "BLUE", "ALPHA",
      "RGB", "RGBA", "LUMINANCE", "LUMINANCE_ALPHA"
    )
  );
  typedef GLenum DataType;
  mixin ConstEnum!(DataType, "+1", 0x1400, Prepend!("GL_",
      "BYTE", "UNSIGNED_BYTE", "SHORT", "UNSIGNED_SHORT",
      "INT", "UNSIGNED_INT", "FLOAT", Expand!("{2|3|4}_BYTES"),
      "DOUBLE"
    )
  );
  typedef GLenum TextureParameterName;
  mixin ConstEnum!(TextureParameterName, "+1", 0x2800, Prepend!("GL_TEXTURE_", "MAG_FILTER", "MIN_FILTER", "WRAP_S", "WRAP_T"));
  const GLenum GL_NEAREST=0x2600, GL_LINEAR=0x2601, GL_CLAMP=0x2900, GL_REPEAT=0x2901;
  mixin ConstEnum!(GLenum, "+1", 0x2700, Prepend!("GL_",
    Append!("_NEAREST", Expand!("{NEAREST|LINEAR}_MIPMAP")),
    Append!("_LINEAR", Expand!("{NEAREST|LINEAR}_MIPMAP"))
  ));

}

extern(System) {
  typedef GLenum StringName;
  void glGetDoublev(GLenum, double *);
  char *glGetString(StringName);
  void glGetFloatv(GLenum, float *);
  void glGetIntegerv(GLenum, int *);
  
  const StringName GL_VENDOR=0x1F00, GL_RENDERER=0x1F01, GL_VERSION=0x1F02, GL_EXTENSIONS=0x1F03;
}

class Texture  {
  uint me;
  int width, height;
  this(size_t w, size_t h, bool mipmap, ubyte[] data) {
    width=w; height=h;
    glChecked("GenTextures", glGenTextures(1, &me));
    bind;
    if (mipmap) glChecked("TexMinFilterMipmap", glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST));
    else glChecked("TexMinFilter", glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
    glChecked("TexParams", glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR),
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT),
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    );
    if (mipmap) {
      glChecked("TexBuildMipmaps", gluBuild2DMipmaps(GL_TEXTURE_2D, 4, width, height, GL_RGBA, GL_UNSIGNED_BYTE, data.length?data.ptr:null));
    } else {
      glChecked("TexGenImage", glTexImage2D(GL_TEXTURE_2D, 0, 4, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data.length?data.ptr:null));
    }
  }
  this(size_t w, size_t h, bool mipmap=false) { ubyte[] empty; this(w, h, mipmap, empty); }
  this(size_t w, size_t h, bool mipmap, vec3f delegate(float u, float v) dg) {
    width=w; height=h;
    ubyte[] data;
    if (dg) {
      data=new ubyte[width*height*4];
      size_t idx=0;
      for (int y=0; y<height; ++y) {
        for (int x=0; x<width; ++x) {
          auto col=dg(x/(1f*width), y/(1f/height));
          data[idx+0]=cast(ubyte)(col.x*255);
          data[idx+1]=cast(ubyte)(col.y*255);
          data[idx+2]=cast(ubyte)(col.z*255);
          data[idx+3]=0;
          idx += 4;
        }
      }
    }
    this(width, height, mipmap, data);
  }
  static void enable() {  glEnable(GL_TEXTURE_2D); }
  static void disable() { glDisable(GL_TEXTURE_2D); }
  void free() { glDeleteTextures(1, &me); me=0; }
  ~this() { free; }
  
  static void bind(uint what) { glBindTexture(GL_TEXTURE_2D, what); }
  static uint bound() { uint res; glGetIntegerv(GL_TEXTURE_BINDING_2D, cast(int*)&res); return res; }
  void bind() { bind(me); }
  void With(void delegate()[] dgs...) {
    auto old=bound(); scope(exit) bind(old);
    bind;
    foreach (dg; dgs) dg();
  }
}

extern(System) {
  void glViewport(int x, int y, int width, int height);
}
