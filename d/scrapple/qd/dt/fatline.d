module dt.fatline;

import qd, tools.vector, tools.base, tools.log, tools.compat;

bool _polygons_overlap_4(vec2f[4] poly_1, vec2f[4] poly_2) {
  int next_1(int i) { if (i == poly_1.length - 1) return 0; else return i+1; }
  foreach (i, pt; poly_1) {
    auto pt2 = poly_1[next_1(i)];
    auto d = pt2 - pt, perp = vec2f(d.y, -d.x);
    bool check(vec2f v) { return perp.dot(v) > 0; }
    bool side = check(poly_1[next_1(next_1(i))] - pt); // check which side poly_1 is on
    bool separating = true;
    foreach (other_pt; poly_2) {
      // if any point of the other polygon is on the _same_ side, this is not a separating line. Abort.
      if (check(other_pt - pt) == side) {
        separating = false;
        break;
      }
    }
    if (separating) return false;
  }
  return true; // no separating side found; see http://gpwiki.org/index.php/Polygon_Collision
}

bool polygons_overlap_4(vec2f[4] poly_1, vec2f[4] poly_2) {
  return _polygons_overlap_4(poly_1, poly_2) && _polygons_overlap_4(poly_2, poly_1);
}

class FatLine {
  vec2f[] pts, ds, vs;
  float width;
  this(vec2f[] pts, typeof(color) c, float w) {
    color = c;
    width = w;
    this.pts = pts.dup;
    for (int i = 0; i < pts.length - 1; ++i) {
      auto d = pts[i+1] - pts[i], v = vec2f(d.y, -d.x).normalized();
      ds ~= d; vs ~= v;
    }
  }
  bool overlap(vec2f tl, vec2f br) {
    auto tr = vec2f(br.x, tl.y), bl = vec2f(tl.x, br.y);
    auto square = [tl, tr, br, bl];
    for (int i = 0; i < pts.length - 1; ++i) {
      // construct polygon for line
      auto line_start = pts[i], line_end = pts[i+1];
      auto d = line_end - line_start, d_norm = d.normalized(), d_extend = d_norm * width;
      auto offs = vec2f(d_norm.y, -d_norm.x) * width;
      auto start_extend = line_start - d_extend, end_extend = line_end + d_extend;
      auto line = [start_extend + offs, start_extend - offs, end_extend - offs, end_extend + offs];
      if (polygons_overlap_4(line, square)) return true;
    }
    return false;
  }
  float normal_dist(vec2f pt) {
    auto smallest_sqr = float.infinity, smallest = float.nan;
    vec2f start_diff, end_diff = pt - pts[0];
    for (int i = 0; i < pts.length - 1; ++i) {
      start_diff = end_diff; end_diff = pt - pts[i+1];
      auto dist2 = ds[i].dot(start_diff) > 0, dist3 = ds[i].dot(end_diff) < 0;
      if (dist2 && dist3) {
        auto whee = vs[i].dot(start_diff), whee_sq = whee * whee;
        if (whee_sq < smallest_sqr) {
          smallest_sqr = whee_sq;
          smallest = abs(whee);
        }
      } else {
        auto d1 = start_diff.lensq(), d2 = end_diff.lensq();
        if (smallest_sqr > d1) { smallest_sqr = d1; smallest = float.nan; }
        if (smallest_sqr > d2) { smallest_sqr = d2; smallest = float.nan; }
      }
    }
    if (!isnan(smallest)) return smallest;
    else return sqrt(smallest_sqr);
  }
  bool circle_collides(vec2f pos, float radius) {
    return normal_dist(pos) <= width + radius;
  }
  alias normal_dist opCall;
  rgb delegate(float) color;
  void fill_quad(vec2f tl, vec2f br) {
    for (int x = cast(int) tl.x; x < br.x; ++x)
      for (int y = cast(int) tl.y; y < br.y; ++y)
        pset(x, y, color(opCall(vec2f(x, y))));
  }
  const MAXSIZE = 4;
  void quad_descent(vec2f tl, vec2f br, float[4] dists = [float.nan, 0f, 0f, 0f]) {
    line(tl.tuple, br.tuple, Box=Blue);
    // logln("line from ", tl, " .. ", br);
    bool maxdepth = ((br.x-tl.x) < MAXSIZE) || ((br.y-tl.y) < MAXSIZE);
    if (isnan(dists[0])) {
      dists[] = [opCall(tl), opCall(vec2f(br.x, tl.y)), opCall(vec2f(tl.x, br.y)), opCall(br)];
    }
    void try_desc(vec2f new_tl, vec2f new_br, float[4] dists) {
      if (!overlap(new_tl, new_br)) {
        line(new_tl.tuple, new_br.tuple, Fill=color(dists[0]), Box=Blue);
        return;
      }
      if (maxdepth) fill_quad(new_tl, new_br);
      else quad_descent(new_tl, new_br, dists);
    }
    if (maxdepth) fill_quad(tl, br);
    else {
      auto half = (tl + br) / 2;
      auto new_dists = [opCall(vec2f(half.x, tl.y)), opCall(vec2f(tl.x, half.y)), opCall(vec2f(br.x, half.y)), opCall(vec2f(half.x, br.y))];
      auto center = opCall(half);
      try_desc(tl, half, [dists[0], new_dists[0], new_dists[1], center]);
      try_desc(vec2f(half.x, tl.y), vec2f(br.x, half.y), [new_dists[0], dists[1], center, new_dists[2]]);
      try_desc(vec2f(tl.x, half.y), vec2f(half.x, br.y), [new_dists[1], center, dists[2], new_dists[3]]);
      try_desc(half, br, [center, new_dists[2], new_dists[3], dists[3]]);
    }
  }
  void render() {
    quad_descent(vec2f(0, 0), vec2f(screen.w, screen.h));
  }
}

import tools.time;
void delegate() gen_fps() {
  return stuple(sec(), 0) /apply/ (ref double s, ref int c) {
    auto t = sec();
    if (t > s + 1f) {
      writefln(c, " FPS"); s = t; c = 0;
    } else ++c;
  };
}

/*import tools.functional, std.math, tools.mersenne;
void main() {
  screen(1024, 768);
  auto start = vec2f(20, 20);
  auto end = vec2f(100, 200);
  auto current = ([start, end]).dup;
  auto fps = gen_fps(), df = new FatLine(current, 20);
  void update_mouse() {
    if (mouse.clicked) current ~= vec2f(mouse.pos.x, mouse.pos.y);
    df = new FatLine(current, df.width);
  }
  void jiggle() {
    foreach (ref v; current) v += vec2f(randf() - .5f, randf() - .5f);
    df = new FatLine(current, df.width);
  }
  typeof(White) color(float d) {
    if (abs(d) > df.width) return Black.blend(White, 0.1);
    else if (abs(d) > df.width - 2) return Black.blend(White, 0.1).blend(White, (df.width - abs(d)) / 2f);
    else if (abs(d) > df.width - 4) return White.blend(Black, (df.width - 2 - abs(d)) / 2f);
    else if (abs(d) > df.width - 6) return Black.blend(White, (df.width - 4 - abs(d)) / 2f);
    else return White;
  }
  while (true) {
    quad_descent(vec2f(0, 0), vec2f(screen.w, screen.h));
    flip;
    events;
    // jiggle();
    update_mouse();
    fps();
  }
}
*/