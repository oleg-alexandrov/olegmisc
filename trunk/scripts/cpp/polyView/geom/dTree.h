#ifndef DTREE_H
#define DTREE_H

// A tree for storing boxes for fast access. 

#include <vector>
#include <cfloat> // defines DBL_MAX
#include "geomUtils.h"

template <typename Box>
inline bool leftLessThan (Box P, Box Q){
  return (P.xl + P.xh) < (Q.xl + Q.xh); // x midpoint comparison
}

template <typename Box>
inline bool botLessThan (Box P, Box Q){
  return (P.yl + P.yh) < (Q.yl + Q.yh); // y midpoint comparison
}

template <typename Box>
inline bool lexLessThan(Box P, Box Q){
  if (P.xl < Q.xl) return true; if (P.xl > Q.xl) return false;
  if (P.yl < Q.yl) return true; if (P.yl > Q.yl) return false;
  if (P.xh < Q.xh) return true; if (P.xh > Q.xh) return false;
  if (P.yh < Q.yh) return true; if (P.yh > Q.yh) return false;
  return false;
}

template <typename Box>
inline bool operator==(Box P, Box Q){
  return P.xl == Q.xl && P.xh == Q.xh && P.yl == Q.yl && P.yh == Q.yh;
}

template <typename Box>
inline bool operator!=(Box P, Box Q){
  return ! (P == Q);
}

template <typename Box>
struct boxNode{
  boxNode<Box> * left;
  boxNode<Box> * right;
  Box       Rect;
  bool      isLeftRightSplit;
  double    maxInLeftChild, minInRightChild;
  boxNode<Box>(): left(NULL), right(NULL), isLeftRightSplit(false),
                  maxInLeftChild(-DBL_MAX), minInRightChild(DBL_MAX){}
};

template <typename Box>
class boxTree{

public:
  boxTree();
  void formTree(// Boxes will be reordered but otherwise
                // unchanged inside this function
                std::vector<Box> & Boxes 
                );
  
  void getBoxesInRegion(double xl, double yl, double xh, double yh, // Inputs
                        std::vector<Box> & outBoxes                 // Outputs
                        );

  boxNode<Box> * getTreeRoot(){
    return m_root;
  }

private:

  void formTreeInternal(Box * Boxes, int numBoxes,
                        bool isLeftRightSplit,
                        boxNode<Box> *&  root
                        );

  
  void getBoxesInRegionInternal(//Inputs
                                double xl, double yl, double xh, double yh,
                                boxNode<Box> * root,
                                // Outputs
                                std::vector<Box> & outBoxes);
  
  void reset();
  boxNode<Box> * getNewboxNode();
  
  // Will get nodes from this pool (for performance reasons)
  std::vector< boxNode<Box> > m_nodePool; 

  int m_freeNodeIndex;
  boxNode<Box> * m_root;
    
};

template <typename Box>
boxTree<Box>::boxTree(){
  reset();
  return;
}

template <typename Box>
void boxTree<Box>::reset(){
  m_freeNodeIndex = 0;
  m_root          = NULL;
  m_nodePool.clear();
  return;
}

template <typename Box>
boxNode<Box> * boxTree<Box>::getNewboxNode(){
  // Get a node from the pool
  boxNode<Box> * ptr = vecPtr(m_nodePool) + m_freeNodeIndex;
  assert( m_freeNodeIndex < (int)m_nodePool.size() );
  m_freeNodeIndex++;
  return ptr;  
}

template <typename Box>
void boxTree<Box>::formTree(// Boxes will be reordered but otherwise
                            // unchanged inside this function
                            std::vector<Box> & Boxes 
                            ){
  
  reset();
  int numBoxes = Boxes.size();
  m_nodePool.resize(numBoxes);

  bool isLeftRightSplit = true;
  formTreeInternal(vecPtr(Boxes), numBoxes, isLeftRightSplit, m_root);
  return;
}

template <typename Box>
void boxTree<Box>::formTreeInternal(Box * Boxes, int numBoxes,
                                    bool isLeftRightSplit,
                                    boxNode<Box> *&  root
                                    ){
  

  // To do: Implement this without recursion.

  // To do: No need to store the box B in the tree. Store just a
  // pointer to B as sorting the left and right halves does not change
  // the midpoint box B.

  // To do: No need even for the tree, it can be stored in-place in
  // Boxes, in the same way as an in-place heap is stored.
  
  assert(numBoxes >= 0);
  
  if (numBoxes == 0){
    root = NULL;
    return; 
  }
  
  root = getNewboxNode();
  root->isLeftRightSplit = isLeftRightSplit;
  
  if (isLeftRightSplit){ // Left-most boxes go in the left child
    sort(Boxes, Boxes + numBoxes, leftLessThan<Box>);
  }else{               // Bottom-most boxes go in the left child
    sort(Boxes, Boxes + numBoxes, botLessThan<Box>);
  }

  int mid = numBoxes/2;
  assert( 0 <= mid && mid < numBoxes);
  
  root->Rect = Boxes[mid]; // Must happen after sorting

  // A box is not a point, it has non-zero width. A box whose midpoint
  // is to the left of the root box midpoint, may still overlap with
  // the region to the right of the root box midpoint. As such, when
  // searching for boxes in the tree, we will need to consider how far
  // to the right the left subtree goes, and how far to the left the
  // right subtree goes.
  
  double maxInLeftChild = -DBL_MAX, minInRightChild = DBL_MAX;
  if (isLeftRightSplit){
    
    for (int s = 0; s < mid; s++)
      if (Boxes[s].xh >= maxInLeftChild) maxInLeftChild = Boxes[s].xh;
    
    for (int s = mid + 1; s < numBoxes; s++)
      if (Boxes[s].xl <= minInRightChild) minInRightChild = Boxes[s].xl;
    
  }else{
    
    for (int s = 0; s < mid; s++)
      if (Boxes[s].yh >= maxInLeftChild) maxInLeftChild = Boxes[s].yh;
    
    for (int s = mid + 1; s < numBoxes; s++)
      if (Boxes[s].yl <= minInRightChild) minInRightChild = Boxes[s].yl;
    
  }

  root->maxInLeftChild  = maxInLeftChild;
  root->minInRightChild = minInRightChild;
  
  // At the next split we will split perpendicularly to the direction
  // of the current split
  formTreeInternal( Boxes,            mid,
                    !isLeftRightSplit, root->left  );
  formTreeInternal( Boxes + mid + 1,  numBoxes - mid - 1,
                    !isLeftRightSplit, root->right );

  return;
}

template <typename Box>
void boxTree<Box>::getBoxesInRegion(// Inputs
                                    double xl, double yl, double xh, double yh,
                                    // Output
                                    std::vector<Box> & outBoxes
                                    ){

  outBoxes.clear();
  if (xl > xh || yl > yh || m_root == NULL) return;
  getBoxesInRegionInternal(xl, yl, xh, yh, m_root, // Inputs
                           outBoxes                // Outputs
                           );
  return;
}

template <typename Box>
void boxTree<Box>::getBoxesInRegionInternal(// Inputs
                                            double xl, double yl,
                                            double xh, double yh,
                                            boxNode<Box> * root,
                                            // Outputs
                                            std::vector<Box> & outBoxes
                                            ){

  assert (root != NULL);

  const Box & B = root->Rect; // alias

  if (boxesIntersect(B.xl, B.yl, B.xh, B.yh, xl, yl, xh, yh))
    outBoxes.push_back(B);
  
  if (root->isLeftRightSplit){
    
    if (root->left != NULL  && xl <= root->maxInLeftChild)
      getBoxesInRegionInternal(xl, yl, xh, yh, root->left,  outBoxes);
    if (root->right != NULL && xh >= root->minInRightChild)
      getBoxesInRegionInternal(xl, yl, xh, yh, root->right, outBoxes);
    
  }else{
    
    if (root->left != NULL  && yl <= root->maxInLeftChild)
      getBoxesInRegionInternal(xl, yl, xh, yh, root->left,  outBoxes);
    if (root->right != NULL && yh >= root->minInRightChild)
      getBoxesInRegionInternal(xl, yl, xh, yh, root->right, outBoxes);
    
  }
    
  return;
}

class edgeTree{
  
public:
  
  void putPolyEdgesInTree(const dPoly & poly);

  void findPolyEdgesInBox(// inputs
                          double xl, double yl,
                          double xh, double yh,
                          // outputs
                          std::vector<utils::seg> & edgesInBox
                          );
  
  void findClosestEdgeToPoint(// inputs
                              double x0, double y0,
                              // outputs
                              utils::seg & closestEdge,
                              double     & closestDist,
                              // The location on the closest
                              // edge where closestDist is achieved
                              double & closestX, double & closestY
                              );
  
private:

  inline void edgeToBox(//inputs
                        double bx, double by, double ex, double ey,
                        // outputs
                        utils::dRectWithId & R
                        ){
    // Given an edge, compute its bounding box and the info necessary info
    // to reverse this transformation, see boxToEdge.
    int id = 0;
    if (bx > ex){ std::swap(bx, ex); id |= 1; } // id = id | 01
    if (by > ey){ std::swap(by, ey); id |= 2; } // id = id | 10
    R = utils::dRectWithId(bx, by, ex, ey, id);
    return;
  }
  
  inline void boxToEdge(// inputs
                        const utils::dRectWithId & R, 
                        // outputs
                        double & bx, double & by, double & ex, double & ey
                        ){
    // Recover the edge based on the bounding box and id. The inverse of edgeToBox.
    bx = R.xl; by = R.yl; ex = R.xh; ey = R.yh;
    int id = R.id;
    if (id & 1) std::swap (bx, ex);
    if (id & 2) std::swap (by, ey);
  }
  
  void findClosestEdgeToPointInternal(// inputs
                                      double x0, double y0,
                                      boxNode<utils::dRectWithId> * root,
                                      // outputs
                                      utils::seg & closestEdge,
                                      double     & closestDist
                                      );
  // Internal data structures
  boxTree<utils::dRectWithId>      m_boxTree;
  std::vector<utils::dRectWithId>  m_allEdges; 
  std::vector<utils::dRectWithId>  m_boxesInRegion;
  
};


#endif
