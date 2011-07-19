module splines;
import std.stdio, qd, tools.base;

float blend(float a, float b, float x) {
  return a*(1f-x) + b*x;
}

float delegate(float) bezier2(float _v1, float _v2, float _v3, float _v4) {
  return (float v1, float v2, float v3, float v4, float x) {
    auto t1 = blend(v1, v2, x), t2 = blend(v2, v3, x), t3 = blend(v3, v4, x);
    auto u1 = blend(t1, t2, x), u2 = blend(t2, t3, x);
    return blend(u1, u2, x);
  } /fix/ stuple(_v1, _v2, _v3, _v4);
}

float delegate(float) multibezier(float[] values) {
  float[] controls;
  auto vals = values; vals ~= vals[$-1];
  float prev; bool first = true;
  while (vals.length > 2) {
    float nv;
    if (first) nv = vals[0]; else nv = vals[0] + (vals[0] - prev)*0.5;
    first = false;
    prev = nv; controls ~= nv;
    vals.take();
  }
  float[] controls_2;
  foreach_reverse (i, val; values) {
    if (!i) break;
    float nc2 = values[i] + (values[i] - controls[i-1])*0.5;
    controls_2 = nc2 ~ controls_2;
  }
  controls_2 = values[0] ~ controls_2;
  controls ~= vals[$-1];
  // logln("Off ", values, ": controls ", controls, " - controls2 ", controls_2);
  return (float[] v, float[] c, float[] c_2, float x) {
    int i = cast(int) x;
    auto start = v[i], end = v[i+1];
    return bezier2(start, c[i], c_2[i], end)(x-i);
  } /fix/ stuple(values, controls, controls_2);
}

/*
void main() {
  screen(640, 480);
  float[][] points;
  points ~= [100, 100];
  points ~= [200, 100];
  points ~= [200, 200];
  points ~= [100, 200];
  points ~= [0, 200];
  points ~= [200, 300];
  points ~= [300, 200];
  foreach (point; points) point[0]+=200;
  
  flip=false;

  auto cols = [Red, Green, Blue, Red~Green, Green~Blue, Red~Blue, White, Red];
  foreach (i, point; points) circle(point[0], point[1], 3, cols[i]);
  foreach (i, point; points[1..$])
    line(points[i][0], points[i][1], point[0], point[1], White);
  int i;
  float[] prev_c;
  float[] xs, ys;
  foreach (point; points) { xs ~= point[0]; ys ~= point[1]; }
  auto mbx = multibezier(xs), mby = multibezier(ys);
  for (float f=0; f<points.length - 1; f += 0.001) {
    pset(cast(int) mbx(f), cast(int) mby(f), cols[cast(int) f].blend(cols[cast(int) f + 1], f - cast(int) f));
  }
  flip;
  while (true) events;
}
*/
