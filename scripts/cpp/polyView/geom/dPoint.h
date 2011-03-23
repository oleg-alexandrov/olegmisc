#ifndef DPOINT_H
#define DPOINT_H

struct dPoint{
  double x, y;
  dPoint(): x(0), y(0){}
  dPoint(double x_in, double y_in): x(x_in), y(y_in){}
};


bool operator<   (dPoint P, dPoint Q);
bool greaterThan (dPoint P, dPoint Q);

#endif
