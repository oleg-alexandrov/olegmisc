#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>
#include "kdTree.h"
#include "geomUtils.h"

using namespace std;
using namespace utils;

#if 0
    kdTree();
    Node * formTree(Point * P, int numPts);
    Node * getNewNode();
    
    std::vector<Node> m_nodePool; // Will get nodes from this pool (for performance reasons)
    int m_freeNodeIndex;
    Node * m_root;

#endif

kdTree::kdTree(){
  reset();
}

void kdTree::reset(){
  m_freeNodeIndex = 0;
  m_root          = NULL;
  m_nodePool.clear();
}


Node * kdTree::getNewNode(){
  Node * ptr = vecPtr(m_nodePool) + m_freeNodeIndex;
  m_freeNodeIndex++;
  assert( m_freeNodeIndex < (int)m_nodePool.size() );
  return ptr;  
}

void kdTree::formTree(std::vector<Point> & P // P will be reordered inside this function
                      ){

  reset();
  int numPts = P.size();
  m_nodePool.resize(2*numPts + 2);
  m_root = getNewNode();
  formTreeInternal(vecPtr(P), numPts, m_root);
}


void kdTree::formTreeInternal(Point * P, int numPts, Node * root){
  
  
}
