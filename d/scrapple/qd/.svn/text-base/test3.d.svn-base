module test3;

import qd, SDL_image, std.file, std.stdio, std.math;

void main(string[] args) {
  screen(640, 480);
  int width = screen.width, height = screen.height;
  foreach (idx, arg; args[1..$]) {
    writefln("> ", arg);
    Image img, scaled;
    try {
      img=new Image(read(arg));
      scaled=img.nearscale(width, height);
    } catch (Exception e) continue;
    SDL_SetAlpha(scaled.surface, SDL_SRCALPHA, 255/(idx+1));
    float limit=1f / pow(1f + idx, /*0.707*/0.6); //(idx/sqrt(args.length*1f)+1f);
    writefln(limit, " > ", 0.05);
    if (limit < 0.05) break;
    for (int x=0; x<width; ++x) {
      for (int y=0; y<height; ++y) {
        ubyte[4] current;
        getpixel32(display.surface, x, y, &current);
        ubyte[4] newOne;
        switch (scaled.surface.format.BytesPerPixel) {
          case 4: getpixel32(scaled.surface, (x*scaled.width)/width, (y*scaled.height)/height, &newOne); break;
          case 3: getpixel24(scaled.surface, (x*scaled.width)/width, (y*scaled.height)/height, &newOne); break;
          default: writefln("Agh!"); goto agh;
        }
        float[3] dists=[current[0]*1f-newOne[0]*1f, current[1]*1f-newOne[1]*1f, current[2]*1f-newOne[2]*1f];
        float dist=sqrt(dists[0]*dists[0]+dists[1]*dists[1]+dists[2]*dists[2]);
        dist/=sqrt(3f*255*255);
        void blend(ubyte[4] *what, ubyte[4] to, float f) {
          foreach (id, ref e; *what) e=cast(ubyte)(e*(1f-f)+to[id]*f);
        }
        if (dist<=limit) {
          blend(&current, newOne, dist);
          pset(pt(x, y), rgb(current[0], current[1], current[2]));
        }
      }
    }
    agh:
    events; flip;
  }
  writefln("Save as result.bmp");
  SaveBMP(display, "result.bmp");
  writefln("Done, looping");
  while (true) { flip; events; }
}
