#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "kdTree.h"
#include "baseUtils.h"
#include "geomUtils.h"

using namespace std;
using namespace utils;

kdTree::kdTree(){
  reset();
  return;
}

void kdTree::reset(){
  m_freeNodeIndex = 0;
  m_root          = NULL;
  m_nodePool.clear();
  return;
}


Node * kdTree::getNewNode(){
  // Get a node from the pool
  Node * ptr = vecPtr(m_nodePool) + m_freeNodeIndex;
  assert( m_freeNodeIndex < (int)m_nodePool.size() );
  m_freeNodeIndex++;
  return ptr;  
}

void kdTree::formTreeOfPoints(// Pts will be reordered but otherwise
                              // unchanged inside this function
                              std::vector<Point> & Pts 
                              ){

  reset();
  int numPts = Pts.size();
  m_nodePool.resize(numPts);

  bool isLeftRightSplit = true;
  formTreeOfPointsInternal(vecPtr(Pts), numPts, isLeftRightSplit, m_root);
  return;
}


void kdTree::formTreeOfPointsInternal(Point * Pts, int numPts, bool isLeftRightSplit,
                                      Node *&  root){

  // To do: Implement this without recursion.
  // To do: No need to store the point P in the tree. Store just a pointer to P
  // as sorting the left and right halves does not change the midpoint P.
  // To do: No need even for the tree, it can be stored in-place in Pts,
  // in the same way as an in-place heap is stored.
  
  assert(numPts >= 0);
  
  if (numPts == 0){
    root = NULL;
    return; 
  }
  
  root = getNewNode();
  root->isLeftRightSplit = isLeftRightSplit;
  
  if (isLeftRightSplit){ // Split points into left and right halves
    sort(Pts, Pts + numPts, leftLessThan);
  }else{               // Split points into bottom and top halves 
    sort(Pts, Pts + numPts, botLessThan);
  }

  int mid = numPts/2;
  assert( 0 <= mid && mid < numPts);
  
  root->P = Pts[mid]; // Must happen after sorting

  // At the next split we will split perpendicularly to the direction
  // of the current split.
  formTreeOfPointsInternal( Pts,            mid,
                            !isLeftRightSplit, root->left
                            );
  formTreeOfPointsInternal( Pts + mid + 1,  numPts - mid - 1,
                            !isLeftRightSplit, root->right
                            );

}

void kdTree::getPointsInBox(double xl, double yl, double xh, double yh, // input box
                            std::vector<Point> & outPts){

  outPts.clear();
  if (xl > xh || yl > yh) return;
  getPointsInBoxInternal(xl, yl, xh, yh, m_root, // inputs
                         outPts                  // outputs
                         );
  return;
}

void kdTree::getPointsInBoxInternal(//Inputs
                                    double xl, double yl, double xh, double yh,
                                    Node * root,
                                    // Outputs
                                    std::vector<Point> & outPts){

  // To do: Can this be done without recursion?
  
  if (root == NULL) return;

  const Point & P = root->P; // alias
  
  if (xl <= P.x && P.x <= xh && yl <= P.y && P.y <= yh) outPts.push_back(P);
  
  if (root->isLeftRightSplit){
    if (xl <= P.x) getPointsInBoxInternal(xl, yl, xh, yh, root->left,  outPts);
    if (xh >= P.x) getPointsInBoxInternal(xl, yl, xh, yh, root->right, outPts);
  }else{
    if (yl <= P.y) getPointsInBoxInternal(xl, yl, xh, yh, root->left,  outPts);
    if (yh >= P.y) getPointsInBoxInternal(xl, yl, xh, yh, root->right, outPts);
  }
    
  return;
}
