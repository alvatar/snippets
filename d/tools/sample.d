module tools.sample;

import tools.base;

// ported from ruby's statistics2
double pnorm(double f) {
  const b = [1.570796288, 0.03706987906, -0.8364353589e-3,
    -0.2250947176e-3, 0.6841218299e-5, 0.5824238515e-5,
    -0.104527497e-5, 0.8360937017e-7, -0.3231081277e-8,
    0.3657763036e-10, 0.6936233982e-12];
  if (f < 0 || f !< 1)
    throw new Exception("f <= 0 or >= 1  in pnorm()!");
  if (f == 0.5) return 0;
  if (f > 0.5) return -pnorm(1 - f);
  auto w3 = -std.math.log(4 * f * (1 - f));
  auto sum = b[0];
  foreach (i, value; b[1 .. $])
    sum += value * pow(w3, i+1);
  return -sqrt(sum * w3);
}

// see http://www.evanmiller.org/how-not-to-sort-by-average-rating.html
double ci_lower_bound(double pos, int total, float power) {
  if (!total) return 0;
  auto
    z = pnorm(1 - power / 2),
    phat = pos / total;
  // guh
  return (
    phat + z * (
      z/(2*total) /* ± */
    - sqrt((phat*(1-phat)+z*z/(4*total))/total)
    )
  )/(1+z*z/total);
}

float measure(T)(T t) {
  static if (is(typeof(t.length))) return t.length;
  else return cast(float) t;
}

T simplesample(T)(int min, int max, float error, out int samples, T delegate() dg) {
  T running_average = dg();
  float running_variance = 0;
  samples = 1;
  static float sqr(float f) { return f*f; }
  void step() {
    auto nsamples = samples + 1, newsample = dg();
    running_variance = running_variance * ((samples - 1) * 1f / samples) + sqr(measure(newsample - running_average)) / samples;
    running_average = running_average * (samples * 1f / nsamples) + newsample / nsamples;
    samples = nsamples;
  }
  for (int i = 0; i < min; ++i) step;
  // ad-hoc
  while (running_variance > sqr(error * samples) && samples < max)
    step;
  return running_average;
}
