module primes;

import tools.base, qd, std.math;
bool isPrimeNaïve(int i, int start = 2) {
  auto sqi = sqrt(cast(float) i);
  for (int k = start; k <= sqi; ++k) {
    if ((i%k) == 0) return false;
  }
  return true;
}

bool isPrime(int i) {
  static int[] list;
  if (!list.length) {
    int cur = 2;
    while (list.length < 200) {
      if (isPrimeNaïve(cur)) list ~= cur;
      cur++;
    }
  }
  auto sqi = sqrt(cast(float) i);
  foreach (smallprime; list) {
    if (smallprime>sqi) return true; // bound for divisors
    if ((i%smallprime) == 0) return false;
  }
  return isPrimeNaïve(i, list[$-1]+1);
}


import tools.array2d;
void main() {
  screen(640, 480);
  cls(White);
  const scale = 1;
  int limit = 480*480*scale*scale;
  int current;
  auto field = array2d!(ubyte)(480, 480);
  Stuple!(int, int) pos = stuple((field.width/2)*scale, (field.height/2)*scale);
  void up () { pos._0 --; ; } void down () { pos._0 ++; }
  void left () { pos._1 --; } void right () { pos._1 ++; }
  void check () { current++; if (isPrime(current)) field[pos.x/scale, pos.y/scale] = field[pos.x/scale, pos.y/scale] + 1; }
  int step;
  flip=false;
  while (current<limit) {
    Range[++step].each={ right; check; };
    Range[step].each={ up; check; };
    Range[++step].each={ left; check; };
    Range[step].each={ down; check; };
  }
  for (int x = 80; x < 560; ++x) for (int y = 0; y < 480; ++y) {
    float value = field[x-80, y] / (scale*scale*1.0);
    pset(x, y, White.blend(Black, value));
  }
  while (true) { events; flip; }
}
