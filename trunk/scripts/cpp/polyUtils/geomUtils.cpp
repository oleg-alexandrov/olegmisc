#include <cmath>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include <cfloat>
#include <cstring>
#include <cassert>
#include "geomUtils.h"
using namespace std;
using namespace utils;
void utils::snapPolyLineTo45DegAngles(bool isClosedPolyLine,
                                      int numVerts, double * xv, double * yv){

  // Given a polygonal line, transform it such that all edges
  // in it make an angle multiple of 45 degrees with the x axis.

  if (numVerts <= 0) return;
  
  // The vectors corresponding to angles multiple of 45 degree
  int numAngles = 8;
  double xs[numAngles], ys[numAngles];
  for (int a = 0; a < numAngles; a++){
    double theta = a*45*M_PI/180;
    xs[a] = cos(theta);
    ys[a] = sin(theta);
  }

  // Snap first vertex to int grid
  xv[0] = round(xv[0]);
  yv[0] = round(yv[0]);

  for (int v = 0; v < numVerts - 1; v++){

    bool snap2ndClosest = false; // snap to closest, not second closest
    snapOneEdgeTo45(numAngles, xs, ys, snap2ndClosest,  // inputs
                    xv[v], yv[v], xv[v + 1], yv[v + 1]  // in-out
                    );
    
  }

  if (!isClosedPolyLine || numVerts < 3) return;

  // The poly line is closed. After vertex n - 1 we have vertex 0.

  for (int attempt = 0; attempt < 2; attempt++){
    
    double x0 = xv[0],            y0 = yv[0];
    double x1 = xv[numVerts - 2], y1 = yv[numVerts - 2];
    double x2 = xv[numVerts - 1], y2 = yv[numVerts - 1];
    
    bool snap2ndClosest = (attempt != 0); 
    snapOneEdgeTo45(numAngles, xs, ys, snap2ndClosest,          // inputs
                    x0, y0, xv[numVerts - 1], yv[numVerts - 1]  // in-out
                    );

    double x3 = xv[numVerts - 1], y3 = yv[numVerts - 1];
    
    // To do: better  variable names above, more comments, and move
     // the code below to a subroutine.
    // Find the intersection of the lines
    // (x0, y0) --> (x3, y3) and (x1, y1) --> (x2, y2).
    double det = ( (x3-x0)*(y2-y1) - (y3-y0)*(x2-x1) );
    double top = ( (x1-x0)*(y2-y1) - (y1-y0)*(x2-x1) );
    bool success = (det != 0 || top == 0);
    if (det != 0){
      double t = top/det;
      xv[numVerts - 1] = round( 2*( t*(x3-x0) + x0 ) )/2.0; // round to half-int grid
      yv[numVerts - 1] = round( 2*( t*(y3-y0) + y0 ) )/2.0;
    }else{
      xv[numVerts - 1] = x2;
      yv[numVerts - 1] = y2;
    }

    if (success) break;
    
  }
  
  // It is possible that the last edge and the edge before it 
  // are 45-degree edges and intersect off-grid. If that's the case,
  // go backwards from last edge to first and fix all the intersections
  // to be on grid.
  assert(numVerts >= 3);
  double shiftx = 0.0, shifty = 0.0;
  for (int v = numVerts; v >= 0; v--){
    int v0 = v%numVerts;
    double x0 = xv[v0], x3 = xv[v-1], x1 = xv[v - 1], x2 = xv[v-2];
    double y0 = yv[v0], y3 = yv[v-1], y1 = yv[v - 1], y2 = yv[v-2];
    
    // Apply the shift from the previous snap operation
    x0 += shiftx; x3 += shiftx;
    y0 += shifty; y3 += shifty;
    xv[v0] = x0;
    yv[v0] = y0;

    bool snap2ndClosest = false;
    snapOneEdgeTo45(numAngles, xs, ys, snap2ndClosest,  // inputs
                   x0, y0, x3, y3                       // in-out
                   );

    // Find the intersection of the lines
    // (x0, y0) --> (x3, y3) and (x1, y1) --> (x2, y2).
    double det = ( (x3-x0)*(y2-y1) - (y3-y0)*(x2-x1) );
    double top = ( (x1-x0)*(y2-y1) - (y1-y0)*(x2-x1) );
    double t = top/det;
    double xi = round( 2*( t*(x3-x0) + x0 ) )/2.0;
    double yi = round( 2*( t*(y3-y0) + y0 ) )/2.0;
    if (det != 0 &&  xi == round(xi) && yi == round(yi) ){
      // Finally arrived at a point at which all vertices
      // are on grid
      xv[v-1] = xi;
      yv[v-1] = yi;
      break;
    }

    shiftx = x3 - x1;
    shifty = y3 - y1;
  }

  // Validate
//  for (int v = 0; v < numVerts; v++) cout << xv[v] << ' ' << yv[v] << endl;

  for (int v = 0; v < numVerts; v++){
    double dx = xv[(v+1)%numVerts] - xv[v];
    double dy = yv[(v+1)%numVerts] - yv[v];
    if ( xv[v] != round(xv[v]) ||
         yv[v] != round(yv[v]) ||
         !( dx == 0 || dy == 0 || abs(dx) == abs(dy) ) ){
      cerr << "Error: Expecting integer vertices with 45 degree angles."  << endl;
      cerr << "Instead, got the vector (" << dx << ", " << dy << ") " 
           << "with starting point " << xv[v] << ' ' << yv[v] << endl;
      //assert(false);
    }
  }
  
  return;
}

void utils::snapOneEdgeTo45(int numAngles, double* xs, double* ys,
                            bool snap2ndClosest, 
                            double & x0, double & y0,
                            double & x1, double & y1){

  
  double dx = x1 - x0, dy = y1 - y0;
  double len = distance(0, 0, dx, dy);
  if (len == 0.0) return;

  dx /= len;
  dy /= len;

  // Find the closest angle multiple of 45 degrees from (dx, dy)
  int minAngle   = 0;
  double minDist = DBL_MAX;
  for (int a = 0; a < numAngles; a++){
    double dist = distance(dx, dy, xs[a], ys[a]);
    if (dist <= minDist){
      minDist  = dist;
      minAngle = a;
    }
  }

  // We prefer to snap to the second closest angle if for some reason
  // we know that snapping to the closest angle does not work.
  if (snap2ndClosest){
    int minAngle2   = 0;
    double minDist2 = DBL_MAX;

    for (int a = 0; a < numAngles; a++){
      double dist = distance(dx, dy, xs[a], ys[a]);
      if (dist <= minDist2 && a != minAngle){
        minDist2  = dist;
        minAngle2 = a;
      }
    }
    minAngle = minAngle2;
  }
  
  // Snap to integer coordinates in the direction of minAngle 
  double factor =  abs(xs[minAngle]) + abs(ys[minAngle]); // 1 or sqrt(2)
  len = factor*round(len/factor);
  x1 = x0 + round( len*xs[minAngle] );
  y1 = y0 + round( len*ys[minAngle] );

  return;
}

void utils::minDistFromPtToSeg(//inputs
                               double xin, double yin,
                               double x0, double y0,
                               double x1, double y1,
                               // outputs
                               double & xout, double & yout,
                               double & minDist
                               ){

  // Given the point (xin, yin) and the segment going from (x0, y0) to
  // (x1, y1), find the point (xout, yout) on this segment (not on its
  // continuation) closest to (xin, yin).
  
  double a = (x1  - x0)*(x1 - x0) + (y1  - y0)*(y1 - y0);
  double b = (xin - x0)*(x1 - x0) + (yin - y0)*(y1 - y0);

  double t;
  if (a == 0.0) t = 0.0;
  else          t = b/a;
  t = max(t, 0.0);
  t = min(t, 1.0);

  xout = x0 + t*(x1 - x0);
  yout = y0 + t*(y1 - y0);

  minDist = sqrt ( (xin  - xout)*(xin - xout) + (yin  - yout)*(yin - yout) );

  return;
}



void utils::searchForColor(std::string lineStr, // input, not a reference on purpose
                           std::string & color  // output
                           ){
  
//   const char * xgraph_colors[] = 
//     {"black", "white", "red", "blue", "green", "violet",
//      "orange", "yellow", "pink", "cyan", "lightGray",
//      "darkGray", "fuchsia", "aqua", "navy", "gold"};

  const char * xgraph_colors[] = 
    {"black", "white", "red", "blue", "green", "violet", // 0,  ..., 5
     "orange", "yellow", "pink", "cyan", "#A2B5CD",      // 6,  ..., 10
     "#6C7B8B", "#FF00FF", "#00CDCD", "navy", "gold"     // 11, ..., 15
    };

  char       * line  = (char*)lineStr.c_str();
  const char * col   = "color";
  char       * start = strstr(line, col);

  if (start == NULL) return;
  if (strlen(start) <= strlen(col)) return;
  start += strlen(col);

  // Strip the equal sign, quotes, etc.
  for (int i = 0; i < (int)strlen(start); i++){

    if (start[i] == '"' || start[i] == '=' || start[i] == '\''){
      start[i] = ' ';
    }
    
  }

  const char *delimiter = " \t";
  char * pch = strtok (start, delimiter);
  if (pch == NULL) return;

  color = string(pch);

  int numColors = sizeof(xgraph_colors)/sizeof(char*);
  
  // If the color is given as a number, per xgraph's conventions
  // (e.g., red is color 2), convert that number to the color name.
  if ('0' <= pch[0] && pch[0] <= '9'){
    int colorIndex = atoi(pch)%numColors;
    color = string( xgraph_colors[colorIndex] );
  }
  
  return;
}

bool utils::searchForAnnotation(std::string lineStr, anno & annotation){

  // Search for annotations, which have the form:
  // anno xval yval label
  // Return true on success.

  istringstream iss (lineStr);
  string an, label;
  double x, y;

  if ( ( !(iss >> an >> x >> y) ) || an != "anno" ){
    return false;
  }

  getline(iss, label); // Everything else goes to the label
  
  annotation.x     = x;
  annotation.y     = y;
  annotation.label = label;

  return true;
}

void utils::searchForLayer(std::string   lineStr, // input
                           std::string & layer    // output
                           ){

  layer = "";
  
  // We are searching for ";" followed by zero or more spaces,
  // followed by something like "65:0"
  char * line = (char *) lineStr.c_str();

  char * start1 = strstr(line, ";");
  if (start1 == NULL) return;
  start1++; // Move beyond ";"
  if (*start1 == '\0') return;

  int l1 = atoi(start1);

  char * start2 = strstr(start1, ":");
  if (start2 == NULL) return;
  start2++; // Move beyond ":"
  if (*start2 == '\0') return;
  
  int l2 = atoi(start2);

  ostringstream strout;
  strout << l1 << ':' << l2;
  layer = strout.str();

  return;
}

double utils::signedPolyArea(int numV, const double* xv, const double* yv){

  // Subtract the first vertex when computing the area to handle more
  // accurately polygons very far from the origin.
  
  double area = 0.0;
  
  for (int vIter = 0; vIter < numV; vIter++){

    int vNext = (vIter + 1)%numV;
    area += (xv[vIter] - xv[0])*(yv[vNext] - yv[0]) -
      (xv[vNext] - xv[0])*(yv[vIter] - yv[0]);
    
  }

  area /= 2.0;

  return area;
}

void utils::expandBoxToGivenRatio(// inputs
                                  double aspectRatio, 
                                  // inputs/outputs
                                  double & xll,  double & yll,
                                  double & widx, double & widy){

  // Expand the given box to have the aspect ratio equal to the number aspectRatio.
  assert(widx > 0.0 && widy > 0.0 && aspectRatio > 0.0);
  double nwidx = widx, nwidy = widy;
  if (widy/widx <= aspectRatio) nwidy = widx*aspectRatio;
  else                          nwidx = widy/aspectRatio;

  // Sanity checks
  double tol = 1.0e-3;
  bool check = ( nwidx >= widx*(1 - tol) && nwidy >= widy*(1 - tol)
                 && abs(nwidy/nwidx - aspectRatio) < tol*aspectRatio );
  if (!check){
    cout << "ERROR!" << endl;
    cout << "widx widy are "   << widx  << ' ' << widy  << endl;
    cout << "nwidx nwidy are " << nwidx << ' ' << nwidy << endl;
    cout << "Aspect ratio is " << aspectRatio << endl;
    cout << "|nwidy/nwidx - aspectRatio| = " << abs(nwidy/nwidx - aspectRatio) << endl;
    cout << "Max allowed error is " << tol*aspectRatio << endl;
  }
  assert(check);

  // Make the new bounding box have the same center as the old one
  xll += widx/2.0 - nwidx/2.0;
  yll += widy/2.0 - nwidy/2.0;

  // Overwrite the previous box
  widx = nwidx; 
  widy = nwidy;

  return;
}
