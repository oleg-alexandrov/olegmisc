#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "edgeUtils.h"

using namespace std;

bool utils::edgeIntersectsHorizontalEdge(// Input: arbitrary edge
                                         double x0, double y0,
                                         double x1, double y1,
                                         // Input: horizontal edge
                                         double begx, double endx,
                                         double yval
                                         ){

  // See if an arbitrary edge intersects a horizontal
  // edge. Care has be taken to avoid as much
  // as possible floating point operations which
  // can be problematic in cases close to
  // degeneracy.

  if (y0 > yval && y1 > yval) return false;
  if (y0 < yval && y1 < yval) return false;

  double diff = y1 - y0;

  if (diff == 0.0){ // The two edges are collinear
    assert(y0 == yval && y1 == yval);
    return (
            max(min(x0, x1), min(begx, endx)) <=
            min(max(x0, x1), max(begx, endx))
            );
  }
  

  // Find the intersection of the two edges.
  // (1 - t)*(x0, y0) + t*(x1, y1) has yval as y coordinate.
  double t = (yval - y0)/diff;
  assert(0.0 <= t && t <= 1.0);
  double x = (1 - t)*x0 + t*x1;
  return ( min(x0, x1) <= x && x <= max(x0, x1) );

  // Note: The answer above could be incorrect
  // if the two edges intersect exactly at one of the
  // two endpoints (begx, yval) or (endx, yval).
  // In that case, if all inputs are integer,
  // a more careful analysis could be done.
}

void utils::cutEdge(double x0, double y0, double x1, double y1,
                    double nx, double ny, double H,
                    double & cutx, double & cuty){

  // Find the point at which the horizontal or vertical line
  // nx*x + ny*y = H intersects the edge (x0, y0) --> (x1, y1).
  // We assume that by now we already know that the edge
  // intersects this line.

  // To do: This needs careful reading.

  double dot0 = nx*x0 + ny*y0;
  double dot1 = nx*x1 + ny*y1;
  
  assert( (dot0 <= H && dot1 >= H) || (dot0 >= H && dot1 <= H ) );
  assert( dot0 != dot1 );

  // Find t such that (1-t)*(x0, y0) + t*(x1, y1) intersect the
  // cutting line
  
  double t = (H - dot0)/(dot1 - dot0);
  t = max(t, 0.0); t = min(t, 1.0); // extra precautions
  cutx = (1-t)*x0 + t*x1;
  cuty = (1-t)*y0 + t*y1;

  // Cut symmetrically in x0 and x1 to avoid problems later
  t = (H - dot1)/(dot0 - dot1);
  t = max(t, 0.0); t = min(t, 1.0); // extra precautions
  double cutx2 = (1-t)*x1 + t*x0;
  double cuty2 = (1-t)*y1 + t*y0;

  cutx = 0.5*cutx + 0.5*cutx2;
  cuty = 0.5*cuty + 0.5*cuty2;
  
  // The above formulas have floating point errors. Use the fact that
  // the cutting line is either vertical or horizontal to recover
  // precisely one of the two coordinates above.
  if (nx == 0){
    cuty = H/ny;
  }else if (ny == 0){
    cutx = H/nx;
  }
  
  return;
}

