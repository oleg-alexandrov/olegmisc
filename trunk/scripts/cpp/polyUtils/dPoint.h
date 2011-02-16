#ifndef DPOINT_H
#define DPOINT_H

struct dPoint{
  double x, y;
};

bool operator< (dPoint P, dPoint Q){
  return ( P.x < Q.x ) || (P.x == Q.x && P.y < Q.y);
}

#endif
