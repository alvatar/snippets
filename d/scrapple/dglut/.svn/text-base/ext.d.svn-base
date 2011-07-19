module dglut.ext;
import dglut.opengl;
import std.string: toString, toStringz, split;
public import dglut.set;

template glStringFunction(string name, StringName glname) {
  mixin("string "~name~"() { string res; glChecked(res=toString(glGetString(glname))); return res; }");
}
mixin MultiMixin!(glStringFunction, 2, "Vendor", GL_VENDOR, "Renderer", GL_RENDERER, "Version", GL_VERSION);
Set!(string) Extensions() {
  string ext;
  glChecked(ext=toString(glGetString(GL_EXTENSIONS)));
  Set!(string) res;
  foreach (e; ext.split(" ")) if (!e.length) continue; else res.set(e);
  return res;
}

version(Win32) { }
else {
  extern(C) void *glXGetProcAddressARB(char *procName);
}

import std.traits: ReturnType, ParameterTypeTuple;
template SysFunc(C) { extern(System) alias ReturnType!(C) function(ParameterTypeTuple!(C)) SysFunc; }

struct ExtFunc(C, string name) {
  static {
    SysFunc!(C) load() {
      static if (is(typeof(&glXGetProcAddressARB))) {
        return cast(SysFunc!(C)) glXGetProcAddressARB(toStringz("gl"~name));
      } else static assert(false, "Platform not supported (yet)");
    }
    SysFunc!(C) fn;
    ReturnType!(C) opCall(ParameterTypeTuple!(C) params) {
      if (!fn) fn=cast(typeof(fn)) load();
      if (!fn) throw new Exception("Extension function \""~name~"\" not found in lookup table!");
      return fn(params);
    }
  }
}

const GLenum GL_FRAMEBUFFER_EXT=0x8D40, GL_RENDERBUFFER_EXT=0x8D41;
const GLenum GL_FRAMEBUFFER_BINDING_EXT=0x8CA6, GL_RENDERBUFFER_BINDING_EXT=0x8CA7,
  GL_MAX_COLOR_ATTACHMENTS_EXT=0x8CDF, GL_MAX_RENDERBUFFER_SIZE_EXT=0x84E8;
typedef GLenum Attachment;
mixin ConstEnum!(Attachment, "+1",
  0x8CE0, Expand!("GL_COLOR_ATTACHMENT{0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15}_EXT"),
  0x8D00, "GL_DEPTH_ATTACHMENT_EXT", 0x8D20, "GL_STENCIL_ATTACHMENT_EXT"
);


class glExtension(string name) {
  static { bool checked, _supported; }
  this() {
    if (!checked) {
      _supported=name in Extensions;
      checked=true;
    }
    if (!_supported) throw new Exception("Cannot create "~(cast(Object) this).toString~": "~name~" not supported!");
  }
  static bool supported() { if (!checked) { checked=true; _supported=name in Extensions; } return _supported; }
}

mixin ConstEnum!(GLenum, "+1", 0x8CD5, Prepend!("GL_FRAMEBUFFER_", Append!("_EXT",
    "COMPLETE", Expand!("INCOMPLETE_{ATTACHMENT|MISSING_ATTACHMENT}"),
    0x8CD9, Expand!("INCOMPLETE_{DIMENSIONS|FORMATS|DRAW_BUFFER_EXT|READ_BUFFER_EXT}"), "UNSUPPORTED"
  ))
);

const GLenum GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT=0x8CD0,
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT=0x8CD1;

class FrameBuffer : glExtension!("GL_EXT_framebuffer_object") {
  void checkState() {
    auto old=bound(); scope(exit) bind(old); bind;
    alias ExtFunc!(void function(GLenum, GLenum, GLenum, int *), "GetFramebufferAttachmentParameterivEXT") getpar;
    foreach (where, obj; attachedObjects) {
      int res, res2;
      getpar(GL_FRAMEBUFFER_EXT, where, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT, &res);
      getpar(GL_FRAMEBUFFER_EXT, where, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT, &res2);
      if (!res || !res2) throw new Exception("Weird .. no draw buffer attached?");
    }
    
    auto res=ExtFunc!(GLenum function(GLenum target), "CheckFramebufferStatusEXT")(GL_FRAMEBUFFER_EXT);
    if (res==GL_FRAMEBUFFER_COMPLETE_EXT) return;
    string[GLenum] decode=[0x8CD6: "Incomplete (attachment)"[], 0x8CD7: "Incomplete (missing attachment)",
      0x8CD9: "Incomplete (dimensions)", 0x8CDA: "Incomplete (formats)",
      0x8CDB: "Incomplete (draw buffer)", 0x8CDC: "Incomplete (read buffer)",
      0x8CDD: "Unsupported"];
    if (!(res in decode)) throw new Exception("Unknown framebuffer error!");
    throw new Exception("Framebuffer error: "~decode[res]);
  }
  uint fb;
  this() { ExtFunc!(void function(int, uint *), "GenFramebuffersEXT")(1, &fb); }
  void free() {
    ExtFunc!(void function(int, uint *), "DeleteFramebuffersEXT")(1, &fb);
    fb=0;
  }
  ~this() { free; }
  
  static void bind(uint whar) { ExtFunc!(void function(GLenum, uint), "BindFramebufferEXT")(GL_FRAMEBUFFER_EXT, whar); }
  static uint bound() { uint res; glGetIntegerv(GL_FRAMEBUFFER_BINDING_EXT, cast(int*)&res); return res; }
  bool isBound() { return bound==fb; }
  void bind() { bind(fb); }
  void Using(void delegate()[] dgs...) {
    glPushAttrib(GL_VIEWPORT_BIT); scope(exit) glPopAttrib;
    glViewport(0, 0, width, height);
    auto old=bound(); scope(exit) bind(old); bind;
    checkState;
    foreach (dg; dgs) dg();
  }
  void With(void delegate()[] dgs...) {
    auto old=bound(); scope(exit) bind(old); bind;
    foreach (dg; dgs) dg();
  }
  
  Texture[Attachment] attachedObjects; // for the sake of the GC
  int width, height;
  
  alias ExtFunc!(void function(GLenum, Attachment, GLenum, uint), "FramebufferRenderbufferEXT") FramebufferRenderbuffer;
  alias ExtFunc!(void function(GLenum target, Attachment, GLenum target, uint texture, int level), "FramebufferTexture2DEXT")
    glfbtex2d;
  void attach(Attachment where, PixelFormat component, Texture tex, int level=0) {
    auto old=bound(); scope(exit) bind(old); bind;
    if (width || height) {
      if (width!=tex.width || height!=tex.height)
        throw new Exception("All textures attached to a frame buffer must be the same size!");
    } else { width=tex.width; height=tex.height; }
    glChecked("FrameBufBindTexture", {
      auto prev=tex.bound; scope(exit) tex.bind(prev);
      tex.bind;
      glChecked("FrameBufAttachTexture", glfbtex2d(GL_FRAMEBUFFER_EXT, where, GL_TEXTURE_2D, tex.me, level));
    });
  }
  void detach(Attachment where, int level=0) {
    glfbtex2d(GL_FRAMEBUFFER_EXT, where, GL_TEXTURE_2D, 0, level);
    width=0; height=0;
  }
}
