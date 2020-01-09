#include <iostream>
#include <cmath>
using namespace std;
int main(){

  double sx = 10000, sy = 10000, ex = -10000, ey = -10000;
  double lenx = 780, leny = 620;

  double alpha = .923169, fx = 258.501, fy = 258.498,
    cx = 323.92,  cy = 240.827;

  const double ialpha = 1 / alpha;
  const double inside = 2 * tan(alpha / 2);
  for (int j = 0; j < leny; j++) {
    for (int i = 0; i < lenx; i++) {
      double ux = (static_cast<double>(i) - lenx * 0.5) / fx;
      double uy = (static_cast<double>(j) - leny * 0.5) / fy;
      double ru = sqrt(ux*ux + uy*uy);
      double rd = atan(ru * inside) * ialpha;
      double conv;
      if (ru > 1e-5) {
        conv = rd / ru;
      } else {
        conv = 1;
      }
      double px = cx + ux * conv * fx;
      double py = cy + uy * conv * fy;
      sx = std::min(sx, px); ex = std::max(ex, px);
      sy = std::min(sy, py); ey = std::max(ey, py);
    }
  }

  std::cout << "values are: " << sx << ' ' << ex << ' ' << sy << ' ' << ey << std::endl;
}
