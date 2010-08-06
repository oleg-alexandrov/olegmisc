#include <vector>
#include <algorithm>
#include <iostream>
#include <cassert>
#include <cfloat>
#include <cassert>
#include <cstring>
#include <string>
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

void dPoly::findClosestPointAndDist(// inputs
                                    double x0, double y0,
                                    // outputs
                                    double & min_x, double & min_y,
                                    double & min_dist
                                    ){

  min_x = x0; min_y = y0; min_dist = DBL_MAX;
  
  for (int s = 0; s < m_totalNumVerts; s++){

    double dist = distance(x0, y0, m_xv[s], m_yv[s]);
    if (dist <= min_dist){
      min_dist = dist;
      min_x    = m_xv[s];
      min_y    = m_yv[s];
    }
    
  }

  return;
}

void dPoly::findClosestPolyIndex(//inputs
                                 double x0, double y0,
                                 // outputs
                                 int & minIndex, double & minDist
                                 ){

  // Given a set of polygons and a point, find the index of the polygon
  // closest to the point. Return that closest distance as well.

  minIndex = -1;
  minDist  = DBL_MAX;
  
  int start = 0;
  double xval, yval;
  for (int pIter = 0; pIter < m_numPolys; pIter++){
      
    if (pIter > 0) start += m_numVerts[pIter - 1];
      
    for (int vIter = 0; vIter < m_numVerts[pIter]; vIter++){

      int beg = start + vIter;
      int end = start + (vIter + 1)%m_numVerts[pIter];

      double dist = DBL_MAX;
      minDistFromPtToSeg(// inputs
                         x0, y0, m_xv[beg], m_yv[beg], m_xv[end], m_yv[end],
                         // outputs
                         xval, yval, dist
                         );

      if (dist <= minDist){
        minIndex = pIter;
        minDist  = dist;
      }

    }
    
  }

  return;
}

void dPoly::erasePoly(int polyIndex){

  assert(0 <= polyIndex && polyIndex < m_numPolys);

  int start = 0;
  for (int pIter = 0; pIter < polyIndex; pIter++){
    start += m_numVerts[pIter]; 
  }

  m_xv.erase(m_xv.begin() + start,
             m_xv.begin() + start + m_numVerts[polyIndex]);
  
  m_yv.erase(m_yv.begin() + start,
             m_yv.begin() + start + m_numVerts[polyIndex]);

  m_totalNumVerts -= m_numVerts[polyIndex];
  m_numPolys      -= 1;
  
  m_colors.erase(m_colors.begin()     + polyIndex);
  m_layers.erase(m_layers.begin()     + polyIndex);
  m_numVerts.erase(m_numVerts.begin() + polyIndex); // better be last
  m_annotationsAtVerts.clear();

  return;
}

bool dPoly::readPoly(const char * filename){

  reset();
  
  ifstream fh(filename);
  if( !fh ){
    cerr << "Could not open " << filename << endl;
    return false;
  }

  string line;
  string color = "yellow"; // default color
  
  while( getline(fh, line) ) {

    cout << "Line is " << line << endl;

    // Convert to lowercase
    transform(line.begin(), line.end(), line.begin(), ::tolower);

    // If the current line has a color, store it in 'color'.
    // Else keep both 'line' and 'color' unchanged.
    searchForColor((char*)line.c_str(), color); // Fix this hackish approach
    
    istringstream iss(line);
    string val;
    if ( (iss >> val) && (val == "next") ){
      cout << "Found a next statement in: '" << line << "'" << endl;
      // Put here logic for closing the polygon
      continue;
    }

    // Extract the coordinates and the layer
    istringstream iss2 (line);
    double x, y;
    string layer = "";
    if ( !(iss2 >> x >> y) ) continue;
    
    // This line has valid coordinates, which we stored in x and y.
    // Now find the layer as well.
    searchForLayer(line, layer);
    cout << "Extracted: " << x << ' ' << y << ' ' << layer << endl;

  }
  
  return true;
}
