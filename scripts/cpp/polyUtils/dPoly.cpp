#include <vector>
#include <algorithm>
#include <iostream>
#include <cassert>
#include "read_write_xg.h"

#include "dPoly.h"
using namespace std;
using namespace utils;

void dPoly::bdBox(double & xll, double & yll, double & xur, double & yur)
  const{

  if (m_totalNumVerts <= 0){
    xll = DBL_MAX, xur = -DBL_MAX;
    yll = DBL_MAX, yur = -DBL_MAX;
    return;
  }
    
  xll = *std::min_element( vecPtr(m_xv), vecPtr(m_xv) + m_totalNumVerts );
  yll = *std::min_element( vecPtr(m_yv), vecPtr(m_yv) + m_totalNumVerts );
  xur = *std::max_element( vecPtr(m_xv), vecPtr(m_xv) + m_totalNumVerts );
  yur = *std::max_element( vecPtr(m_yv), vecPtr(m_yv) + m_totalNumVerts );

  return;
};

void dPoly::appendPolygon(int numVerts,
                            const double * xv,
                            const double * yv,
                            const std::string & color,
                            const std::string & layer
                            ){

  if (numVerts <= 0) return;
  
  m_numPolys      += 1;
  m_totalNumVerts += numVerts;
  
  m_numVerts.push_back(numVerts);
  m_colors.push_back(color);
  m_layers.push_back(layer);
  for (int s = 0; s < numVerts; s++){
    m_xv.push_back(xv[s]);
    m_yv.push_back(yv[s]);
  }

  return;
}

void dPoly::clipPoly(// inputs
                       double clip_xll, double clip_yll,
                       double clip_xur, double clip_yur,
                       dPoly & clippedPoly // output
                       ){

  assert(this != &clippedPoly); // source and destination must be different
  
  clippedPoly.reset();
  clippedPoly.set_isPointCloud(m_isPointCloud);
  
  const double * xv                     = get_xv();
  const double * yv                     = get_yv();
  const int    * numVerts               = get_numVerts();
  int numPolys                          = get_numPolys();
  const std::vector<std::string> colors = get_colors();
  const std::vector<std::string> layers = get_layers();
  
  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){
      
    if (pIter > 0) start += numVerts[pIter - 1];
      
    std::string color = colors[pIter];
    std::string layer = layers[pIter];
      
    std::vector<double> cxv, cyv;
    std::vector<int> cpoly;
    cxv.clear(); cyv.clear(); cpoly.clear();
    
    if (m_isPointCloud){

      // To cut a point cloud to a box all is needed is to select
      // which points are in the box
      for (int vIter = 0; vIter < numVerts[pIter]; vIter++){

        double x = xv[start + vIter];
        double y = yv[start + vIter];
        if (x >= clip_xll && x <= clip_xur &&
            y >= clip_yll && y <= clip_yur
            ){
          cxv.push_back(x);
          cyv.push_back(y);
        }
        
      }

      cpoly.push_back( cxv.size() );
      
    }else{

      cutPoly(1, numVerts + pIter, xv + start, yv + start,
              clip_xll, clip_yll, clip_xur, clip_yur,
              cxv, cyv, cpoly // outputs
              );

    }
    
    int cstart = 0;
    for (int cIter = 0; cIter < (int)cpoly.size(); cIter++){
        
      if (cIter > 0) cstart += cpoly[cIter - 1];
      int cSize = cpoly[cIter];
      clippedPoly.appendPolygon(cSize,
                                vecPtr(cxv) + cstart,
                                vecPtr(cyv) + cstart,
                                color, layer
                                );

    }

  }

  // Cutting inherits the annotations at the vertices of the uncut
  // polygons which are in the cutting box.
  vector<anno> annotations, annoInBox;

  for (int annoType = 0; annoType < 2; annoType++){
    
    if (annoType == 0){
      get_annotations(annotations);
    }else{
      compAnnoAtVerts();
      getAnnoAtVerts(annotations);
    }   
    
    annoInBox.clear();
    for (int s = 0; s < (int)annotations.size(); s++){
      const anno & A = annotations[s];
      
      if (clip_xll <= A.x && A.x <= clip_xur &&
          clip_yll <= A.y && A.y <= clip_yur
          ){
        annoInBox.push_back(A);
      }
      
    }

    if (annoType == 0){
      clippedPoly.set_annotations(annoInBox);
    }else{
      clippedPoly.setAnnoAtVerts(annoInBox);
    }

  }
  
  return;
} 

void dPoly::appendPolygons(const dPoly & poly){

  const double * xv        = poly.get_xv();
  const double * yv        = poly.get_yv();
  const int    * numVerts  = poly.get_numVerts();
  int numPolys             = poly.get_numPolys();
  vector<string> colors    = poly.get_colors();
  vector<string> layers    = poly.get_layers();
  vector<anno> annotations;  poly.get_annotations(annotations);
  
  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){
      
    if (pIter > 0) start += numVerts[pIter - 1];
      
    string color = colors   [pIter];
    string layer = layers   [pIter];
    int pSize    = numVerts [pIter];
    
    appendPolygon(pSize, xv + start, yv + start, color, layer);
    
  }

  for (int s = 0; s < (int)annotations.size(); s++){
    addAnno(annotations[s]);
  }
  
  return;
}

void dPoly::setAnnoAtVerts(const std::vector<anno> & annotations){
  m_annotationsAtVerts = annotations;
}

void dPoly::getAnnoAtVerts(std::vector<anno> & annotations){
  annotations = m_annotationsAtVerts;
}

void dPoly::compAnnoAtVerts(){

  m_annotationsAtVerts.clear();

  const double * xv = get_xv();
  const double * yv = get_yv();

  int start = 0;
  for (int pIter = 0; pIter < m_numPolys; pIter++){
      
    if (pIter > 0) start += m_numVerts[pIter - 1];

    for (int v = 0; v < m_numVerts[pIter]; v++){

      anno A;
      A.x     = xv[start + v];
      A.y     = yv[start + v];
      A.label = num2str(v); 
      m_annotationsAtVerts.push_back(A);
    }

  }

  return;
}
