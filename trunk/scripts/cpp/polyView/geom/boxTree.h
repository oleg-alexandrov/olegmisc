#ifndef BOXTREE_H
#define BOXTREE_H

// A tree for storing boxes for fast access.

#include <vector>
#include <cfloat> // defines DBL_MAX

struct Box{
  double xl, yl, xh, yh;
  Box(): xl(0.0), yl(0.0), xh(0.0), yh(0.0){}
  Box(double xl_in, double yl_in, double xh_in, double yh_in):
    xl(xl_in), yl(yl_in), xh(xh_in), yh(yh_in){}
};


inline bool leftLessThan (Box P, Box Q){
  return (P.xl + P.xh) < (Q.xl + Q.xh); // x midpoint comparison
}

inline bool botLessThan (Box P, Box Q){
  return (P.yl + P.yh) < (Q.yl + Q.yh); // y midpoint comparison
}

struct boxNode{
  boxNode * left;
  boxNode * right;
  Box       B;
  bool      isLeftRightSplit;
  double    maxInLeftChild, minInRightChild;
  boxNode(): left(NULL), right(NULL), isLeftRightSplit(false),
             maxInLeftChild(-DBL_MAX), minInRightChild(DBL_MAX){}
};


class boxTree{

public:
  boxTree();
  void formTree(// Boxes will be reordered but otherwise
                // unchanged inside this function
                std::vector<Box> & Boxes 
                );
  
  void getBoxesInBox(double xl, double yl, double xh, double yh, // input box
                     std::vector<Box> & outBoxes);

  
private:

  void formTreeInternal(Box * Boxes, int numBoxes,
                        bool isLeftRightSplit,
                        boxNode *&  root
                        );

  
  void getBoxesInBoxInternal(//Inputs
                             double xl, double yl, double xh, double yh,
                             boxNode * root,
                             // Outputs
                             std::vector<Box> & outBoxes);
  
  void reset();
  boxNode * getNewboxNode();
  
  // Will get nodes from this pool (for performance reasons)
  std::vector<boxNode> m_nodePool; 

  int m_freeNodeIndex;
  boxNode * m_root;
    
};


#endif
