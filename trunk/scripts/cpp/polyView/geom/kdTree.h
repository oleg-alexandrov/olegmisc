#ifndef KDTREE_H
#define KDTREE_H

// A naive (but hopefully correct) implementation of kd-trees.

#include <vector>

struct Point{
  double x, y;
  Point(){}
  Point(double x_in, double y_in): x(x_in), y(y_in){}
};
  
struct Node{
  Node * left;
  Node * right;
  Point P;
  double sep;
  bool isVert;
  bool isPoint; // If the current node holds a point or is just a separator between points
};

class kdTree{

public:
  kdTree();
  void formTree(std::vector<Point> & P // P will be reordered inside this function
                );

private:
  void formTreeInternal(Point * P, int numPts, Node * root);
  void reset();
  Node * getNewNode();
  std::vector<Node> m_nodePool; // Will get nodes from this pool (for performance reasons)
  int m_freeNodeIndex;
  Node * m_root;
    
};
  
#endif
