#ifndef EDGE_UTILS_H
#define EDGE_UTILS_H

namespace utils {
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

