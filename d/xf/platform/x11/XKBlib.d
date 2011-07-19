module xf.platform.x11.XKBlib;

import xf.platform.x11.Xlib;

extern(C) bool XkbSetDetectableAutoRepeat(Display*, int, int*);