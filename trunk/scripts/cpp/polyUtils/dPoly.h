#ifndef XG_POLY_H
#define XG_POLY_H

#include <vector>
#include <algorithm>
#include <cfloat> // defines DBL_MAX
#include "read_write_xg.h"
#include <vector>
#include "cutPoly.h"

// A class holding a set of polygons in double precision
class dPoly{

public:

  dPoly(){
    reset();
  }

  void set_isPointCloud(bool isPointCloud){ m_isPointCloud = isPointCloud; }
  bool isPointCloud() { return m_isPointCloud;}
  
  bool readPoly(const char * filename);
  
  bool read_poly(const char * filename, bool isPointCloud){
    m_isPointCloud = isPointCloud;
    bool success = utils::read_xg(filename, isPointCloud,
                                  m_xv, m_yv, m_numVerts, m_numPolys,
                                  m_totalNumVerts, m_colors, m_layers,
                                  m_annotations);
    return success;
  }

  void write_poly(const char *filename,
                  char *color = "yellow", double scale = 1.0
                  ){
    utils::write_xg(filename, m_isPointCloud,
                    m_xv, m_yv, m_numVerts, m_numPolys,
                    m_totalNumVerts,
                    m_colors, color, scale, m_layers, m_annotations);
  }
  
  void reset(){
    m_isPointCloud  = false;
    m_numPolys      = 0;
    m_totalNumVerts = 0;
    m_numVerts.clear();
    m_xv.clear();
    m_yv.clear();
    m_colors.clear();
    m_layers.clear();
    m_annotations.clear();
    m_vertIndexAnno.clear();
    m_layerAnno.clear();
  }

  void appendPolygon(int numVerts,
                     const double * xv,
                     const double * yv,
                     const std::string & color = "yellow",
                     const std::string & layer = ""
                     );

  void appendPolygons(const dPoly & poly);
  
  void clipPoly(// inputs
                double clip_xll, double clip_yll,
                double clip_xur, double clip_yur,
                dPoly & clippedPoly // output
                );

  const int    * get_numVerts          () const { return utils::vecPtr(m_numVerts); }
  const double * get_xv                () const { return utils::vecPtr(m_xv);       }
  const double * get_yv                () const { return utils::vecPtr(m_yv);       }
  int get_numPolys                     () const { return m_numPolys;                }
  int get_totalNumVerts                () const { return m_totalNumVerts;           }
  std::vector<std::string> get_colors  () const { return m_colors;                  }
  std::vector<std::string> get_layers  () const { return m_layers;                  }

  void setColor(std::string color);
  
  // Annotations
  void get_annotations   (std::vector<anno> & annotations) const;
  void get_layerAnno     (std::vector<anno> & annotations) const;
  void get_vertIndexAnno (std::vector<anno> & annotations) const;
  void set_annotations   (const std::vector<anno> & A);
  void set_layerAnno     (const std::vector<anno> & annotations);
  void set_vertIndexAnno (const std::vector<anno> & annotations);

  void addAnno(const anno & A){m_annotations.push_back(A); }
  void compVertIndexAnno();
  void compLayerAnno();

  void bdBox(double & xll, double & yll, double & xur, double & yur) const;
  
  void findClosestPointAndDist(// inputs
                               double x0, double y0,
                               // outputs
                               double & min_x, double & min_y,
                               double & min_dist
                               );
  
  void findClosestPolyIndex(//inputs
                            double x0, double y0,
                            // outputs
                            int & minIndex, double & minDist
                            );
  
  void erasePoly(int polyIndex);
  
private:
  // m_isPointCloud tells if the poly is an unstructured set of points
  bool                     m_isPointCloud; 

  std::vector<double>      m_xv;
  std::vector<double>      m_yv; 
  std::vector<int>         m_numVerts;
  int                      m_numPolys;
  int                      m_totalNumVerts;
  std::vector<std::string> m_colors;
  std::vector<std::string> m_layers;
  std::vector<anno>        m_annotations;
  std::vector<anno>        m_vertIndexAnno; // Anno showing vertex index
  std::vector<anno>        m_layerAnno;     // Anno showing layer number
  
};

#endif
