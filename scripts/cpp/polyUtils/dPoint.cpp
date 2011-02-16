#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "dPoint.h"

bool operator< (dPoint P, dPoint Q){
  return ( P.x < Q.x ) || (P.x == Q.x && P.y < Q.y);
}

bool greaterThan (dPoint P, dPoint Q){
  return ( P.x > Q.x ) || (P.x == Q.x && P.y > Q.y);
}
