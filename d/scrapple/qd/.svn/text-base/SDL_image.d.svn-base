module SDL_image;

import qd, tools.compat;
extern(C) {
  SDL_Surface *IMG_Load_RW(SDL_RWops *src, int freesrc);
  int SDL_SaveBMP_RW(SDL_Surface *what, SDL_RWops *dst, int freedst);
}

class Image : Area {
  import tools.log;
  this(void[] data) {
    tl=pt(0, 0);
    auto ops=SDL_RWFromMem(data.ptr, data.length);
    surf = RefSurf(IMG_Load_RW(ops, true));
    if (!surf.surface) throw new Exception("Could not interpret data");
    surf.refs ++;
    dimensions = pt(surface.w, surface.h);
  }
  private this() { }
  ~this() { if (surf.surface && !--surf.refs) SDL_FreeSurface(surface); }
  Image map(string NEWSIZE, string SRCPOS, T...)(T t) {
    auto res = mixin("SDL_CreateRGBSurface(0, "~NEWSIZE~", 32)");
    ubyte[4] pixel;
    auto surf = surface();
    switch (surface.format.BytesPerPixel) {
      case 3:
        for (int y=0; y<res.h; ++y) for (int x=0; x<res.w; ++x) {
          mixin("getpixel24(surf, "~SRCPOS~", &pixel); ");
          putpixel32(res, x, y, SDL_MapRGBA(res.format, pixel[0], pixel[1], pixel[2], 0));
        }
        break;
      case 4:
        for (int y=0; y<res.h; ++y) for (int x=0; x<res.w; ++x) {
          mixin("getpixel32(surf, "~SRCPOS~", &pixel); ");
          putpixel32(res, x, y, SDL_MapRGBA(res.format, pixel[0], pixel[1], pixel[2], pixel[3]));
        }
      break;
      default: throw new Exception("invalid depth");
    }
    auto img = new Image;
    img.surf = RefSurf(res);
    img.surf.refs ++;
    img.dimensions = mixin("pt("~NEWSIZE~")");
    return img;
  }
  alias map!("t[0], t[1]", "(x*surface.w)/t[0], (y*surface.h)/t[1]", int, int) nearscale;
  alias map!("dimensions.x, dimensions.y", "x, dimensions.y - 1 - y") xflip;
  alias map!("dimensions.x, dimensions.y", "dimensions.x - 1 - x, y") yflip;
  alias map!("dimensions.y, dimensions.x", "y, x") flip;
  Image rot_right() { return flip().yflip(); }
  Image rot_left() { return flip().xflip(); }
}

void SaveBMP(Area area, string name) {
  if (-1==SDL_SaveBMP_RW(area.surface, SDL_RWFromFile(toStringz(name), toStringz("wb")), 1)) throw new Exception("Couldn't save "~name);;
}
