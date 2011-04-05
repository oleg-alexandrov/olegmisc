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
  void cutEdge(double x0, double y0, double x1, double y1,
               double nx, double ny, double H,
               double & cutx, double & cuty);
  
}

#endif

