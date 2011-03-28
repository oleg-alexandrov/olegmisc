#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "boxTree.h"
#include "geomUtils.h"

using namespace std;
using namespace utils;

boxTree::boxTree(){
  reset();
  return;
}

void boxTree::reset(){
  m_freeNodeIndex = 0;
  m_root          = NULL;
  m_nodePool.clear();
  return;
}

boxNode * boxTree::getNewboxNode(){
  // Get a node from the pool
  boxNode * ptr = vecPtr(m_nodePool) + m_freeNodeIndex;
  assert( m_freeNodeIndex < (int)m_nodePool.size() );
  m_freeNodeIndex++;
  return ptr;  
}


void boxTree::formTree(// Boxes will be reordered but otherwise
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


void boxTree::formTreeInternal(Box * Boxes, int numBoxes,
                               bool isLeftRightSplit,
                               boxNode *&  root
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
    sort(Boxes, Boxes + numBoxes, leftLessThan);
  }else{               // Bottom-most boxes go in the left child
    sort(Boxes, Boxes + numBoxes, botLessThan);
  }

  int mid = numBoxes/2;
  assert( 0 <= mid && mid < numBoxes);
  
  root->B = Boxes[mid]; // Must happen after sorting

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

#if 0

void boxTree::getBoxesInBox(double xl, double yl, double xh, double yh, // input box
                            std::vector<Box> & outBoxes){

  outBoxes.clear();
  if (xl > xh || yl > yh) return;
  getBoxesInBoxInternal(xl, yl, xh, yh, m_root, // inputs
                         outBoxes                  // outputs
                         );
  return;
}

void boxTree::getBoxesInBoxInternal(//Inputs
                                    double xl, double yl, double xh, double yh,
                                    boxNode * root,
                                    // Outputs
                                    std::vector<Box> & outBoxes){

  // To do: Can this be done without recursion?
  
  if (root == NULL) return;

  const Box & B = root->B; // alias
  
  if (xl <= B.x && B.x <= xh && yl <= B.y && B.y <= yh) outBoxes.push_back(B);
  
  if (root->isLeftRightSplit){
    if (xl <= B.x) getBoxesInBoxInternal(xl, yl, xh, yh, root->left,  outBoxes);
    if (xh >= B.x) getBoxesInBoxInternal(xl, yl, xh, yh, root->right, outBoxes);
  }else{
    if (yl <= B.y) getBoxesInBoxInternal(xl, yl, xh, yh, root->left,  outBoxes);
    if (yh >= B.y) getBoxesInBoxInternal(xl, yl, xh, yh, root->right, outBoxes);
  }
    
  return;
}
#endif
