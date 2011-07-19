#!/bin/sh
xfbuild +noop +obudha +xstd buddhabrot.d -I../.. -L-lSDL -L-lXrandr
rm -R .deps .objs
xfbuild +noop +ocellular +xstd cellular.d -I../.. -J. -L-lSDL -L-lXrandr
rm -R .deps .objs
