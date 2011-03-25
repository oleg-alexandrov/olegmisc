#ifndef KDTREE_H
#define KDTREE_H

// A naive (but hopefully correct) implementation of kd-trees.

#include <vector>

struct Point{
  double x, y;
  Point(): x(0), y(0){}
  Point(double x_in, double y_in): x(x_in), y(y_in){}
};

inline bool leftLessThan (Point P, Point Q){ return P.x < Q.x; }
inline bool botLessThan  (Point P, Point Q){ return P.y < Q.y; }

struct Node{
  Node * left;
  Node * right;
  Point  P;
  bool   isLeftRightSplit;
  Node(): left(NULL), right(NULL), isLeftRightSplit(false){}
};

class kdTree{

public:
  kdTree();
  void formTree(// Pts will be reordered but otherwise unchanged inside this function
                std::vector<Point> & Pts 
                );
  void getPointsInBox(double xl, double yl, double xh, double yh, // input box
                      std::vector<Point> & outPts);

private:

  void formTreeInternal(Point * Pts, int numPts, bool isLeftRightSplit, Node *&  root);

  void getPointsInBoxInternal(//Inputs
                              double xl, double yl, double xh, double yh,
                              Node * root,
                              // Outputs
                              std::vector<Point> & outPts);
  void reset();
  Node * getNewNode();
  std::vector<Node> m_nodePool; // Will get nodes from this pool (for performance reasons)
  int m_freeNodeIndex;
  Node * m_root;
    
};
  
#endif
