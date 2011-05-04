#include <vector>
#include <algorithm>
#include <iostream>
#include <cassert>
#include <cfloat>
#include <cassert>
#include <cstring>
#include <string>
#include "cutPoly.h"
#include "dPoly.h"
using namespace std;
using namespace utils;

void dPoly::reset(){
  m_isPointCloud  = false;
  m_numPolys      = 0;
  m_totalNumVerts = 0;
  m_numVerts.clear();
  m_xv.clear();
  m_yv.clear();
  m_isPolyClosed.clear();
  m_colors.clear();
  m_layers.clear();
  m_annotations.clear();
  m_vertIndexAnno.clear();
  m_layerAnno.clear();
}

void dPoly::bdBox(double & xll, double & yll, double & xur, double & yur)
  const{

  // The bounding box of all polygons
  
  if (m_totalNumVerts <= 0){
    xll = DBL_MAX/4.0, xur = -DBL_MAX/4.0; // Use 1/4.0 to avoid overflow when ...
    yll = DBL_MAX/4.0, yur = -DBL_MAX/4.0; // ... finding width and height
    return;
  }
    
  xll = *min_element( vecPtr(m_xv), vecPtr(m_xv) + m_totalNumVerts );
  yll = *min_element( vecPtr(m_yv), vecPtr(m_yv) + m_totalNumVerts );
  xur = *max_element( vecPtr(m_xv), vecPtr(m_xv) + m_totalNumVerts );
  yur = *max_element( vecPtr(m_yv), vecPtr(m_yv) + m_totalNumVerts );

  return;
};

void dPoly::bdBoxes(std::vector<double> & xll, std::vector<double> & yll,
                    std::vector<double> & xur, std::vector<double> & yur) const{

  // Bounding boxes of individual polygons
  
  xll.clear(); yll.clear(); xur.clear(); yur.clear();
  
  int start = 0;
  for (int pIter = 0; pIter < m_numPolys; pIter++){
      
    if (pIter > 0) start += m_numVerts[pIter - 1];

    int numV = m_numVerts[pIter];

    double x0, y0, x1, y1;
    if (numV <= 0){
      x0 = DBL_MAX/4.0, x1 = -DBL_MAX/4.0; // Use 1/4.0 to avoid overflow when ...
      y0 = DBL_MAX/4.0, y1 = -DBL_MAX/4.0; // ... finding width and height
    }else{
      const double * px = vecPtr(m_xv) + start;
      const double * py = vecPtr(m_yv) + start;
      x0 = *min_element( px, px + numV ); x1 = *max_element( px, px + numV );
      y0 = *min_element( py, py + numV ); y1 = *max_element( py, py + numV );
    }
    xll.push_back(x0); xur.push_back(x1);
    yll.push_back(y0); yur.push_back(y1);
    
  }
  
  return;
};

void dPoly::appendPolygon(int numVerts,
                          const double * xv,
                          const double * yv,
                          bool isPolyClosed,
                          const std::string & color,
                          const std::string & layer
                          ){

  if (numVerts <= 0) return;
  
  m_numPolys      += 1;
  m_totalNumVerts += numVerts;
  
  m_numVerts.push_back(numVerts);
  m_isPolyClosed.push_back(isPolyClosed);
  m_colors.push_back(color);
  m_layers.push_back(layer);
  for (int s = 0; s < numVerts; s++){
    m_xv.push_back(xv[s]);
    m_yv.push_back(yv[s]);
  }

  return;
}

void dPoly::get_annoByType(std::vector<anno> & annotations, int annoType){
  
  if (annoType == 0){
    get_annotations(annotations);
  }else if (annoType == 1){
    get_vertIndexAnno(annotations);
  }else{
    get_layerAnno(annotations);
  }

  return;
}

void dPoly::set_annoByType(const std::vector<anno> & annotations, int annoType){

  if (annoType == 0){
    set_annotations(annotations);
  }else if (annoType == 1){
    set_vertIndexAnno(annotations);
  }else{
    set_layerAnno(annotations);
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
  
  const double * xv               = get_xv();
  const double * yv               = get_yv();
  const int    * numVerts         = get_numVerts();
  int numPolys                    = get_numPolys();
  const vector<bool> isPolyClosed = get_isPolyClosed();
  const vector<string> colors     = get_colors();
  const vector<string> layers     = get_layers();
  
  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){
      
    if (pIter > 0) start += numVerts[pIter - 1];
      
    int  isClosed = isPolyClosed [pIter];
    string color  = colors       [pIter];
    string layer  = layers       [pIter];
      
    vector<double> cutXv, cutYv;
    vector<int> cutNumVerts;
    cutXv.clear(); cutYv.clear(); cutNumVerts.clear();
    
    if (m_isPointCloud){

      // To cut a point cloud to a box all is needed is to select
      // which points are in the box
      for (int vIter = 0; vIter < numVerts[pIter]; vIter++){
        double x = xv[start + vIter];
        double y = yv[start + vIter];
        if (x >= clip_xll && x <= clip_xur &&
            y >= clip_yll && y <= clip_yur
            ){
          cutXv.push_back(x);
          cutYv.push_back(y);
        }
      }
      cutNumVerts.push_back( cutXv.size() );
      
    }else if (isClosed){

      cutPoly(1, numVerts + pIter, xv + start, yv + start,
              clip_xll, clip_yll, clip_xur, clip_yur, 
              cutXv, cutYv, cutNumVerts // outputs
              );
      
    }else{
      
      cutPolyLine(numVerts[pIter], xv + start, yv + start,
                  clip_xll, clip_yll, clip_xur, clip_yur, 
                  cutXv, cutYv, cutNumVerts // outputs
                  );
      
    }
    
    int cstart = 0;
    for (int cIter = 0; cIter < (int)cutNumVerts.size(); cIter++){
        
      if (cIter > 0) cstart += cutNumVerts[cIter - 1];
      int cSize = cutNumVerts[cIter];
      clippedPoly.appendPolygon(cSize,
                                vecPtr(cutXv) + cstart,
                                vecPtr(cutYv) + cstart,
                                isClosed, color, layer
                                );

    }
    
  }

  // Cutting inherits the annotations at the vertices of the uncut
  // polygons which are in the cutting box.
  vector<anno> annotations, annoInBox;

  for (int annoType = 0; annoType < 3; annoType++){
    
    get_annoByType(annotations, annoType);

    annoInBox.clear();
    for (int s = 0; s < (int)annotations.size(); s++){
      const anno & A = annotations[s];
      if (clip_xll <= A.x && A.x <= clip_xur && clip_yll <= A.y && A.y <= clip_yur){
        annoInBox.push_back(A);
      }
    }
    
    clippedPoly.set_annoByType(annoInBox, annoType);

  }
  
  return;
} 

void dPoly::shift(double shift_x, double shift_y){

  for (int i = 0; i < (int)m_xv.size(); i++){
    m_xv[i] += shift_x;
    m_yv[i] += shift_y;
  }

  vector<anno> annotations;
  for (int annoType = 0; annoType < 3; annoType++){
    get_annoByType(annotations, annoType);
    for (int i = 0; i < (int)annotations.size(); i++){
      anno & A = annotations[i]; // alias
      A.x += shift_x;
      A.y += shift_y;
    }
    set_annoByType(annotations, annoType);
  }
  
  return;
}

void dPoly::rotate(double angle){ // The angle is given in degrees

  double a = angle*M_PI/180.0, c = cos(a), s= sin(a);

  if (angle == round(angle) && int(angle)%90 == 0 ){
    // The special case of angle multiple of 90 degrees
    c = round(c), s = round(s);
  }
  
  for (int i = 0; i < (int)m_xv.size(); i++){
    double tmpx = c*m_xv[i] - s*m_yv[i];
    double tmpy = s*m_xv[i] + c*m_yv[i];
    m_xv[i] = tmpx;
    m_yv[i] = tmpy;
  }

  vector<anno> annotations;
  for (int annoType = 0; annoType < 3; annoType++){
    get_annoByType(annotations, annoType);
    for (int i = 0; i < (int)annotations.size(); i++){
      anno & A = annotations[i]; // alias
      double tmpx = c*A.x - s*A.y;
      double tmpy = s*A.x + c*A.y;
      A.x = tmpx;
      A.y = tmpy;
    }
    set_annoByType(annotations, annoType);
  }

  return;
}

void dPoly::scale(double scale){ // The angle is given in degrees

  for (int i = 0; i < (int)m_xv.size(); i++){
    m_xv[i] *= scale;
    m_yv[i] *= scale;
  }

  vector<anno> annotations;
  for (int annoType = 0; annoType < 3; annoType++){
    get_annoByType(annotations, annoType);
    for (int i = 0; i < (int)annotations.size(); i++){
      anno & A = annotations[i]; // alias
      A.x *= scale;
      A.y *= scale;
    }
    set_annoByType(annotations, annoType);
  }

  return;
}

void dPoly::appendPolygons(const dPoly & poly){

  const double * xv         = poly.get_xv();
  const double * yv         = poly.get_yv();
  const int    * numVerts   = poly.get_numVerts();
  int numPolys              = poly.get_numPolys();
  vector<bool> isPolyClosed = poly.get_isPolyClosed();
  vector<string> colors     = poly.get_colors();
  vector<string> layers     = poly.get_layers();
  vector<anno> annotations;  poly.get_annotations(annotations);
  
  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){
      
    if (pIter > 0) start += numVerts[pIter - 1];
      
    bool isClosed = isPolyClosed [pIter];
    string color  = colors       [pIter];
    string layer  = layers       [pIter];
    int pSize     = numVerts     [pIter];
    
    appendPolygon(pSize, xv + start, yv + start, isClosed, color, layer);
    
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

void dPoly::set_color(std::string color){

  m_colors.resize(m_numPolys);
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

void dPoly::findClosestPolyVertex(// inputs
                                  double x0, double y0,
                                  // outputs
                                  double & min_x, double & min_y,
                                  double & min_dist
                                  ) const {

  // Given a point and a set of polygons, find the polygon vertex
  // closest to the given point. Return the closest vertex and the
  // distance to it from the given point. Return DBL_MAX if the
  // polygon is empty.
  
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

void dPoly::findClosestPolyEdge(//inputs
                                 double x0, double y0,
                                 // outputs
                                 int & minIndex,
                                 double & minX, double & minY, double & minDist
                                 ) const{

  // Given a point and a set of polygons, find the polygon edge
  // closest to the given point and the location on the edge where the
  // smallest distance is achieved. Return the index of the polygon
  // where the closest distance is achieved, as well as the point at
  // which that distance is achieved and the smallest distance itself.
  
  minIndex = -1;
  minX     = DBL_MAX, minY = DBL_MAX;
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

      if (dist < minDist){
        minIndex = pIter;
        minX     = xval;
        minY     = yval;
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
  
  m_isPolyClosed.erase(m_isPolyClosed.begin() + polyIndex);
  m_colors.erase(m_colors.begin()             + polyIndex);
  m_layers.erase(m_layers.begin()             + polyIndex);
  m_numVerts.erase(m_numVerts.begin()         + polyIndex); // better be last
  m_vertIndexAnno.clear();
  m_layerAnno.clear();

  return;
}

namespace dPoly_local_functions{
  struct ptAndIndex{
    dPoint point;
    double area;
    int    index;
  };
  bool greaterThanPtIndex (ptAndIndex P, ptAndIndex Q){

    if (greaterThan(P.point, Q.point) ) return true;
    if (greaterThan(Q.point, P.point) ) return false;

    return (P.area > Q.area);
    
  }
}

void dPoly::sortFromLargestToSmallest(){

  // Sort the polygons so that if polygon A is inside of polygon B, then
  // polygon B shows up before polygon A after sorting.

  // Use the fact that if A is inside of B, then the dimensions/area of A
  // are no more than those of B.

  using namespace dPoly_local_functions;

  // Find the bounding boxes of polygons
  vector<double> xll, yll, xur, yur;
  bdBoxes(xll, yll, xur, yur);

  int numPolys = xll.size();

  vector<ptAndIndex> boxDims;
  int start = 0;
  boxDims.resize(numPolys);
  for (int s = 0; s < numPolys; s++){
    
    if (s > 0) start += m_numVerts[s - 1];
    int numV = m_numVerts[s];
    
    boxDims[s].point = dPoint( xur[s] - xll[s], yur[s] - yll[s] );
    boxDims[s].area  = abs(signedPolyArea(numV,
                                          vecPtr(m_xv) + start, vecPtr(m_yv) + start)
                           );
    boxDims[s].index = s;
  }

  // Sort the bounding boxes, this will tell us how to sort the polygons
  sort(boxDims.begin(), boxDims.end(), greaterThanPtIndex );

  // Sort the polygons using auxiliary storage

  vector<double> l_xv           = m_xv;
  vector<double> l_yv           = m_yv;
  vector<int>    l_numVerts     = m_numVerts;
  vector<bool>   l_isPolyClosed = m_isPolyClosed;
  vector<string> l_colors       = m_colors;
  vector<string> l_layers       = m_layers;

  for (int s = 0; s < numPolys; s++){
    int index          = boxDims[s].index;
    m_numVerts     [s] = l_numVerts     [index];
    m_isPolyClosed [s] = l_isPolyClosed [index];
    m_colors       [s] = l_colors       [index];
    m_layers       [s] = l_layers       [index];
  }
  
  start = 0;
  for (int s = 0; s < numPolys; s++){

    if (s > 0) start += m_numVerts[s - 1];

    int index = boxDims[s].index;
    int start2 = 0;
    for (int t = 0; t < index; t++) start2 += l_numVerts[t];

    for (int t = 0; t < m_numVerts[s]; t++){
      m_xv[start + t] = l_xv[start2 + t];
      m_yv[start + t] = l_yv[start2 + t];
    }
    
  }
  
}

void dPoly::sortBySizeAndMaybeAddBigFgPoly(// inputs
                                           double bigXll, double bigYll,
                                           double bigXur, double bigYur
                                           ){
  
  // Sort the polygons from largest to smallest by size. If the
  // largest one is going clockwise, it is a hole. In this case, add a
  // large box going counter-clockwise so that we see the hole as a
  // hole in this box. This is important only when polygons are filled
  // (and holes are of background color).

  sortFromLargestToSmallest();

  if (get_numPolys() <= 0) return;

  const double         * xv       = get_xv();
  const double         * yv       = get_yv();
  const int            * numVerts = get_numVerts();
  const vector<string> & colors   = get_colors();
  const vector<string> & layers   = get_layers();

  double signedArea = signedPolyArea(numVerts[0], xv, yv);

  if (signedArea >= 0) return; // Outer poly is correctly oriented

  double xll, yll, xur, yur;
  bdBox(xll, yll, xur, yur);
  bigXll = min(xll, bigXll); bigXur = max(xur, bigXur);
  bigYll = min(yll, bigYll); bigYur = max(yur, bigYur);

  // Add the extra term to ensure we get a box strictly bigger than
  // the polygons and the big window (this will help with sorting below).
  double extra = max(abs(bigXur - bigXll), abs(bigYur - bigYll)) + 10.0;
  bigXll -= extra; bigXur += extra;
  bigYll -= extra; bigYur += extra;

  double boxX[4], boxY[4];
  boxX[0] = bigXll; boxX[1] = bigXur; boxX[2] = bigXur; boxX[3] = bigXll;
  boxY[0] = bigYll; boxY[1] = bigYll; boxY[2] = bigYur; boxY[3] = bigYur;

  bool isPolyClosed = true;
  appendPolygon(4, boxX, boxY, isPolyClosed, colors[0], layers[0]);

  // Reorder the updated set of polygons
  sortFromLargestToSmallest();

  return;
}

void dPoly::enforce45(){

  // Enforce that polygon vertices are integers and the angles are 45x. 
  
  int start = 0;
  for (int pIter = 0; pIter < m_numPolys; pIter++){
      
    if (pIter > 0) start += m_numVerts[pIter - 1];
    
    bool isClosedPolyLine = true;
    int numV    = m_numVerts[pIter];
    double * px = vecPtr(m_xv) + start;
    double * py = vecPtr(m_yv) + start;
    snapPolyLineTo45DegAngles(isClosedPolyLine, numV, px, py);
    
  }
  
  return;
};

bool dPoly::readPoly(std::string filename,
                     // If isPointCloud is true, treat each point as a
                     // singleton polygon
                     bool isPointCloud 
                     ){

  reset();
  
  m_isPointCloud = isPointCloud;
  
  ifstream fh(filename.c_str());
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

    char * linePtr = (char*)line.c_str(); // To do: Avoid this casting hack.

    // Replace comma with space, to be able to use comma as separator
    for (int s = 0; s < (int)strlen(linePtr); s++){
      if (linePtr[s] == ',') linePtr[s] = ' ';
    }

    // Ignore any text after the comment character, which is '#' or '!'
    for (int s = 0; s < (int)strlen(linePtr); s++){
      if (linePtr[s] == '#' || linePtr[s] == '!'){
        for (int t = s; t < (int)strlen(linePtr); t++){
          linePtr[t] = '\0';
        }
        break;
      }
    }
    
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
    bool isLastVertexOfCurrPoly = ( isLastLine                               ||
                                    ( (iss_next >> val) && (val == "next") ) ||
                                    isPointCloud
                                    );
    bool isCurrPolyNonEmpty = (beg < end);
    if (isLastVertexOfCurrPoly && isCurrPolyNonEmpty){
      
      assert( end == (int)m_xv.size() && end == (int)m_yv.size() );
      
      if (beg < end - 1              &&
          m_xv[beg] == m_xv[end - 1] &&
          m_yv[beg] == m_yv[end - 1]){
        // The first vertex equals to the last vertex in the current
        // polygon. That means that this is a true polygon rather
        // than a polygonal line. Don't store the last
        // vertex.
        end--;
        m_xv.resize(end);
        m_yv.resize(end);
        m_isPolyClosed.push_back(true);
      }else{
        m_isPolyClosed.push_back(false);
      }
      
      m_layers.push_back(layer);
      m_colors.push_back(color);
      
      m_numPolys++;
      m_numVerts.push_back(end - beg);
      m_totalNumVerts = end;
      
      // Start a new polygon
      beg = end;
      
    } // End processing the current polygon in the list of polygons

  } // End reading the file and processing all polygons
  
  return true; // success
  
}

void dPoly::writePoly(std::string filename, std::string defaultColor){

  ofstream outfile(filename.c_str());
  outfile.precision(16);

  int vertCount = 0, annoCount = 0, numAnno = m_annotations.size();
  string prevColor = defaultColor, currColor = "";
    
  for (int j = 0; j < m_numPolys; j++){ // Iterate over polygons

    if ( m_numVerts[j] <= 0 ) continue; // skip empty polygons

    bool isPolyClosed;
    if ((int)m_isPolyClosed.size() <= j ) isPolyClosed = true;
    else isPolyClosed = m_isPolyClosed[j];

    if ( (int)m_colors.size() <= j ) currColor = defaultColor;
    else                             currColor = m_colors[j];

    if (j == 0 || prevColor != currColor){
      outfile << "color = " << currColor << endl; 
    }
    prevColor = currColor;

    string layer;
    if ((int)m_layers.size() <= j ) layer = "";
    else layer = m_layers[j];
    
    for (int i = 0; i < m_numVerts[j]; i++){ // Iterate over vertices of current poly
      
      outfile <<  m_xv[vertCount] << " " << m_yv[vertCount];
      if (layer != "") outfile << " ; " << layer;
      outfile << endl;
      vertCount++;

      // Put one annotation for each vertex, if possible
      if (annoCount < numAnno){
        m_annotations[annoCount].appendTo(outfile);
        annoCount++;
      }
      
    }
    
    // Print the first element again at the end (so that polygons are
    // closed).
    assert(m_numVerts[j] > 0);
    if ( !m_isPointCloud && isPolyClosed){
      int firstVert = vertCount - m_numVerts[j];
      outfile << m_xv[firstVert] << " " << m_yv[firstVert];
      if (layer != "") outfile << " ; " << layer;
      outfile << endl;
    }
    
    if ( !m_isPointCloud ) outfile << "NEXT" << endl;
  }

  // Write the remaining annotations
  for (int a = annoCount; a < numAnno; a++){
    m_annotations[a].appendTo(outfile);
  }

  outfile.close();
}

bool dPoly::read_pol_or_cnt_format(std::string filename,
                                   std::string type, 
                                   bool isPointCloud 
                                   ){

  // Read in two very simple and related polygon formats.
  
  assert(type == "pol" || type == "cnt");

  string color = "yellow";
  string layer = "";

  reset();
  m_isPointCloud = isPointCloud;

  ifstream fh(filename.c_str());
  if( !fh ){
    cerr << "Could not open " << filename << endl;
    return false;
  }
 
  // Bypass lines starting with comments
  while (1){
    char c = fh.peek();
    if (c != '#' && c != '!') break;
    string line;
    getline(fh, line);
  }  

  double tmp;
  if (type == "pol" && !(fh >> tmp >> tmp >> tmp >> tmp) ) return false;

  while (1){
   
    int numVerts = 0;
   
    if (type == "pol"){
     if (! (fh >> tmp >> numVerts) ) return true;  // no more vertices
     if (! (fh >> tmp >> tmp) )      return false; // invalid format
    }else{
     if (! (fh >> numVerts) ) return true;         // no more vertices
    }
    
    m_numPolys++;
    m_numVerts.push_back(numVerts);
    m_totalNumVerts += numVerts;
    m_isPolyClosed.push_back(true);
    m_colors.push_back(color);
    m_layers.push_back(layer);
  
    for (int s = 0; s < numVerts; s++){
      double x, y;
      if (! (fh >> x >> y) ) return false;
      m_xv.push_back(x);
      m_yv.push_back(y); 
    }

    int l = m_totalNumVerts;
    if (l > 0 && numVerts >= 2             && 
        m_xv[l - 1] == m_xv[l - numVerts] && 
        m_yv[l - 1] == m_yv[l - numVerts]
	){
	// Remove last repeated vertex
	m_xv.pop_back();
	m_yv.pop_back();
	m_totalNumVerts--;
        m_numVerts[m_numVerts.size() - 1]--;
     }
  }

   return true;
}

void dPoly::set_pointCloud(const std::vector<dPoint> & P, std::string color,
                           std::string layer){

  // Form a dPoly structure from a set of points
  reset();
  m_isPointCloud = true;
  for (int s = 0; s < (int)P.size(); s++){
    m_totalNumVerts++;
    m_numPolys++;
    m_numVerts.push_back(1);
    m_layers.push_back(layer);
    m_isPolyClosed.push_back(true);
    m_colors.push_back(color);
    m_xv.push_back(P[s].x);
    m_yv.push_back(P[s].y);
  }

  return;
}
