#ifndef EDGE_UTILS_H
#define EDGE_UTILS_H

namespace utils {
  bool edgeIntersectsBox(// Input: arbitrary edge
                         double bx, double by,
                         double ex, double ey,
                         // Input: Box
                         double xl, double yl,
                         double xh, double yh
                         );
  bool edgeIntersectsHorizontalEdge(// Input: arbitrary edge
                                    double x0, double y0,
                                    double x1, double y1,
                                    // Input: horizontal edge
                                    double begx, double endx,
                                    double yval
                                    );
  bool isPointOnEdge(double x0, double y0, double x1, double y1,
                     double x, double y);
  bool collinearEdgesIntersect(// Input: first edge
                                    double ax0, double ay0,
                                    double ax1, double ay1,
                                    // Input: second edge
                                    double bx0, double by0,
                                    double bx1, double by1,
                                    // Output: intersection
                                    // if it exists
                                    double & x, double & y
                                    );
  bool edgesIntersect(// Input: first edge
                      double ax0, double ay0,
                      double ax1, double ay1,
                      // Input: second edge
                      double bx0, double by0,
                      double bx1, double by1,
                      // Output: intersection if it exists
                      double & x, double & y
                      );
  void cutEdge(double x0, double y0, double x1, double y1,
               double nx, double ny, double H,
               double & cutx, double & cuty);
  
}

#endif

