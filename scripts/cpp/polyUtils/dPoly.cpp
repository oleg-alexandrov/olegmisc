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

  for (int annoType = 0; annoType < 3; annoType++){
    
    if (annoType == 0){
      get_annotations(annotations);
    }else if (annoType == 1){
      get_vertIndexAnno(annotations);
    }else{
      get_layerAnno(annotations);
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
    }else if (annoType == 1){
      clippedPoly.set_vertIndexAnno(annoInBox);
    }else{
      clippedPoly.set_layerAnno(annoInBox);
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

void dPoly::get_annotations (std::vector<anno> & annotations) const {
  annotations =  m_annotations;
}

void dPoly::set_annotations(const std::vector<anno> & A){
  m_annotations = A;
}

void dPoly::set_vertIndexAnno(const std::vector<anno> & annotations){
  m_vertIndexAnno = annotations;
}

void dPoly::get_vertIndexAnno(std::vector<anno> & annotations) const{
  annotations = m_vertIndexAnno;
}

void dPoly::set_layerAnno(const std::vector<anno> & annotations){
  m_layerAnno = annotations;
}

void dPoly::get_layerAnno(std::vector<anno> & annotations) const{
  annotations = m_layerAnno;
}

void dPoly::setColor(std::string color){

  for (int s = 0; s < (int)m_colors.size(); s++){
    m_colors[s] = color;
  }
  
}

void dPoly::compVertIndexAnno(){

  m_vertIndexAnno.clear();
  
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
      m_vertIndexAnno.push_back(A);
    }

  }

  return;
}

void dPoly::compLayerAnno(){

  m_layerAnno.clear();
  
  const double * xv = get_xv();
  const double * yv = get_yv();

  int start = 0;
  for (int pIter = 0; pIter < m_numPolys; pIter++){
      
    if (pIter > 0) start += m_numVerts[pIter - 1];

    for (int v = 0; v < m_numVerts[pIter]; v++){

      anno A;
      int vn = (v+1)%m_numVerts[pIter];
      
      A.x     = (xv[start + v] + xv[start + vn])/2.0; // put anno at midpt
      A.y     = (yv[start + v] + yv[start + vn])/2.0; // put anno at midpt
      A.label = m_layers[pIter]; 
      m_layerAnno.push_back(A);
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
  m_vertIndexAnno.clear();
  m_layerAnno.clear();

  return;
}

bool dPoly::readPoly(const char * filename,
                     // If isPointCloud is true, treat each point as a
                     // singleton polygon
                     bool isPointCloud 
                     ){

  // Note: Need to skip any characters after the comment characters, which
  // are "#" and "!". Otherwise the string "! color = x" would read "x" as
  // a color.
  
  reset();
  
  m_isPointCloud = isPointCloud;
  
  ifstream fh(filename);
  if( !fh ){
    cerr << "Could not open " << filename << endl;
    return false;
  }

  // The current polygon has vertices in the range [beg, end)
  int beg = 0, end = 0;
  
  anno annotation;
  string layer, line;
  string color = "yellow"; // default color for polygons
  
  while( getline(fh, line) ) {
    
    bool isLastLine = ( fh.peek() == EOF );

    // Convert to lowercase
    transform(line.begin(), line.end(), line.begin(), ::tolower);

    // If the current line has a color, store it in 'color'.
    // Else keep 'color' unchanged.
    searchForColor(line, color);
    
    if ( searchForAnnotation(line, annotation) ){
      m_annotations.push_back(annotation);
    }
    
    // Extract the coordinates of the current vertex and the layer
    // The format we expect is: x y ; layerNo (e.g., 2.3 -1.2 ; 5:16)
    istringstream iss_xy (line);
    double x, y;
    if ( iss_xy >> x >> y ){

      // This line has valid coordinates, which we read in x and y.
      m_xv.push_back(x);
      m_yv.push_back(y);
      end++;
      
      if (end == beg + 1){
        // Find the layer for the current point only if this point
        // is the first point in the polygon
        searchForLayer(line, layer);
      }
      
    }
    
    // If this is the last line in the file, or if we encountered a
    // "next" statement, or if we treat a polygon as just a set of
    // points (point cloud) then close the current polygon and start a
    // new one.
    istringstream iss_next(line);
    string val;
    if ( isLastLine                               ||
         ( (iss_next >> val) && (val == "next") ) ||
         isPointCloud
         ){

      if (beg < end){

        // The current polygon is non-empty

        if (beg < end - 1              &&
            m_xv[beg] == m_xv[end - 1] &&
            m_yv[beg] == m_yv[end - 1]){
          // The first vertex equals to the last vertex in the current
          // polygon. Then don't store the last vertex.
          assert( end == (int)m_xv.size() && end == (int)m_yv.size() );
          end--;
          m_xv.resize(end);
          m_yv.resize(end);
        }
        
        m_layers.push_back(layer);
        m_colors.push_back(color);

        m_numPolys++;
        m_numVerts.push_back(end - beg);
        m_totalNumVerts = end;
        
        // Start a new polygon
        beg = end;
        
      }
      
    } // End processing the current polygon in the list of polygons

  } // End reading the file and processing all polygons
  
  return true; // success
  
}
