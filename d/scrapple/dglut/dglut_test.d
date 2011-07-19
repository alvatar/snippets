module dglut.dglut_test;

import dglut.opengl, dglut.vector, std.stdio, std.math;

import std.c.stdlib: exit;
extern(System) void key(char key, int x, int y) {
  if (key == 'q') exit(0);
}

extern(System) {
  void glutInit(int *argc, char **argv);
  void glutInitDisplayMode(uint mode);
  void glutInitWindowSize(int, int);
  int glutCreateWindow(char *title);
  void gluOrtho2D(Repeat!(4, double) lrbt);
  void glutKeyboardFunc(void function(char, int, int));
  void glutDisplayFunc(void function());
  void glutIdleFunc(void function());
  void glutMainLoop();
  void gluLookAt(Repeat!(3, double) eye, Repeat!(3, double) center, Repeat!(3, double) up);
  void gluPerspective(double fovy, double aspect, double znear, double zfar);
  void glutSwapBuffers();
}

mixin ConstEnum!(ushort, "*2", 1, Prepend!(
  "GLUT_",
    Expand!("{INDEX|DOUBLE|ACCUM|ALPHA|DEPTH|STENCIL|UNUSED|MULTISAMPLE|STEREO|LUMINANCE}")
  )
);
const ushort GLUT_RGB=0, GLUT_RGBA=0, GLUT_SINGLE=0;

void setupLights() {
  glEnable(GL_LIGHTING);
  with (Light[0]) {
    enable;
    position(lightpos.parts, true);
    ambient=[0, 0, 0, 1];
    diffuse=[1, 1, 1, 1];
    specular=[1, 1, 1, 1];
  }
  /*with (Light[1]) {
    enable;
    position([10, 50, 70], true);
    ambient=[0f, 0f, 0.2f, 1f];
    diffuse=[2, 1, 1, 1];
    specular=[1, 1, 1, 1];
  }*/
}

import std.string: toStringz;
import dglut.ext;
FrameBuffer test; Texture target;
float[32] cameraMatrix, lightMatrix; // projection, view 4x4

vec3f campos, lightpos;
void main(string[] args) {
  int c_argc=args.length;
  char*[] c_args;
  foreach (arg; args) c_args~=toStringz(arg);
  c_args~=null;
  glutInit(&c_argc, c_args.ptr);
  glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH);
  glutInitWindowSize(400, 400);
  auto win = glutCreateWindow("Triangle\0".ptr);
  
  campos=vec3f(0, 5, -10);
  lightpos=vec3f(100, -100, 10);
  
  writefln(Extensions.igrep("framebuffer_object"));
  target=new Texture(1024, 1024);
  glChecked("CreateFramebuffer", test=new FrameBuffer, test.attach(GL_COLOR_ATTACHMENT0_EXT, GL_RGBA, target));
  writefln(test, " created - ", test.width, ":", test.height);
  
  MatrixMode.Projection;
  glLoadIdentity;
  gluPerspective(45, 1.0, 0.1, 100);
  MatrixMode.Modelview;
  Texture.enable;
  glEnable(GL_DEPTH_TEST);

  glutDisplayFunc=&display;
  glutIdleFunc=&display;
  glutKeyboardFunc=&key;
  glutMainLoop;
  return 0;
}

const FONT=cast(void*) 0x804a018;
const CHAR_W=8, CHAR_H=13, CHAR_DESCENT=3, LINE_SEP=2;

extern(System) {
  void glutBitmapCharacter(void *font, int ch);
  void glLoadIdentity();
}

void times(size_t t, void delegate()[] dgs...) { while (t--) foreach (dg; dgs) dg(); }

extern(System) void display() { d_display_wrap(); }
void d_display_wrap() { try glChecked("d_display", d_display); catch (Exception e) { writefln("Error in display: ", e); throw e; } }

import std.c.time, std.random;

void d_display() {
  static int fps;
  static typeof(time(null)) last;
  ++fps;
  if (last!=time(null)) {
    last=time(null);
    writefln(fps, " fps");
    fps=0;
  }
  glLoadIdentity;
  static float f=0;
  f+=1;
  Translate(0, 0, -5);
  Translate(sin(f*0.01), 0, 0);
  Rotate(f*0.2, 0, 1, 0);
  Rotate(f*0.1, 1, 1, 0);

  glChecked("RenderToFramebuffer", MatrixScope(test.Using(renderScene(f))));
  glChecked("RenderToScreen", renderScene(f));
  
  glFlush;
  glutSwapBuffers();
}

void renderScene(float t) {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  setupLights();
  auto points=[
    vec3f(-1, -1, -1), vec3f(1, -1, -1), vec3f(-1, 1, -1), vec3f(1, 1, -1),
    vec3f(-1, -1, 1), vec3f(1, -1, 1), vec3f(-1, 1, 1), vec3f(1, 1, 1)
  ];
  alias Vector!(vec3f, 3) Triangle;
  
  Triangle[] subdivide(Triangle[] from) {
    auto res=new Triangle[from.length*4];
    foreach (index, tri; from) with (tri) {
      //if ((index&127) == 0) writefln(index, "/", from.length);
      auto xy=(x+y)/2, yz=(y+z)/2, zx=(z+x)/2;
      res[index*4..index*4+4] = [Triangle(x, xy, zx), Triangle(y, yz, xy), Triangle(z, zx, yz), Triangle(xy, yz, zx)];
    }
    return res;
  }
  
  void deform(ref vec3f vec) {
    vec.normalize;
    vec.length=sin(vec.x*sin(t*0.01)*16)*0.2 + cos(vec.y*vec.z*cos(t*0.01)*16)*0.2+1;
  }
  static Triangle[] cube;
  Triangle[] face(vec3f a, vec3f b, vec3f c, vec3f d) { return [Triangle(a, b, c), Triangle(c, d, a)][]; }
  if (!cube.length) {
    cube=face(points[0], points[2], points[3], points[1])
      ~ face(points[4], points[5], points[7], points[6])
      ~ face(points[0], points[1], points[5], points[4])
      ~ face(points[3], points[2], points[6], points[7])
      ~ face(points[0], points[4], points[6], points[2])
      ~ face(points[1], points[3], points[7], points[5])
      ;
    times(5, { cube=cube.subdivide(); });
  }
  auto normals=new vec3f[cube.length];
  foreach (id, ref tri; cube) with (tri) {
    foreach (ref vec; parts) deform(vec);
    normals[id]=(y-x).cross(z-x).normalized;
  }
  
  static Texture tex;
  glChecked("DisplayGenerateTexture", {
    if (!tex) tex=new Texture(64, 64, true, (float x, float y) { return vec3f(sin(x)*2+2, y, (x-0.5)*(y-0.5)); });
  });
  with (Material.FrontAndBack) {
    //specular=[.3f, .3f, .3f, 1f];
    emission=[0f, 0f, 0f, 1f];
  }
  glChecked("DisplayBindTextue", tex.bind);
  glChecked("DisplayRenderShape", Triangles({
    foreach (id, t; cube) {
      Normal(normals[id]);
      TexCoord(0, 0); Vertex(t.x);
      TexCoord(0, 1); Vertex(t.y);
      TexCoord(1, 0); Vertex(t.z);
    }
  }));
  //writefln(cube.length, " triangles rendered");
}
