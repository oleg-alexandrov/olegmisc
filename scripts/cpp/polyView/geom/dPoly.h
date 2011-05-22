#ifndef DPOLY_H
#define DPOLY_H

#include <vector>
#include <algorithm>
#include <cfloat> // defines DBL_MAX
#include <vector>
#include "baseUtils.h"
#include "geomUtils.h"

// A class holding a set of polygons in double precision
class dPoly{

public:

  dPoly(){
    reset();
  }

  void reset();
  
  bool read_pol_or_cnt_format(std::string filename,
                              std::string type,
                              bool isPointCloud = false
                              );
  
  bool readPoly(std::string filename,
                bool isPointCloud = false
                );
  
  void writePoly(std::string filename, std::string defaultColor = "yellow");
  
  void appendPolygon(int numVerts,
                     const double * xv,
                     const double * yv,
                     bool isPolyClosed,
                     const std::string & color,
                     const std::string & layer
                     );
  
  void appendPolygons(const dPoly & poly);
  
  void appendRectangle(double xl, double yl, double xh, double yh,
                       bool isPolyClosed,
                       const std::string & color, const std::string & layer
                       );

  void setRectangle(double xl, double yl, double xh, double yh,
                    bool isPolyClosed,
                    const std::string & color, const std::string & layer
                    );

  void clipPoly(// inputs
                double clip_xll, double clip_yll,
                double clip_xur, double clip_yur,
                dPoly & clippedPoly // output
                );
  
  void shift(double shift_x, double shift_y);
  void rotate(double angle);
  void scale(double scale);
  
  const int    * get_numVerts         () const { return utils::vecPtr(m_numVerts); }
  const double * get_xv               () const { return utils::vecPtr(m_xv);       }
  const double * get_yv               () const { return utils::vecPtr(m_yv);       }
  int get_numPolys                    () const { return m_numPolys;                }
  int get_totalNumVerts               () const { return m_totalNumVerts;           }
  std::vector<char> get_isPolyClosed  () const { return m_isPolyClosed;            }
  std::vector<std::string> get_colors () const { return m_colors;                  }
  std::vector<std::string> get_layers () const { return m_layers;                  }
  
  void set_color(std::string color);

  void set_isPolyClosed(bool isPolyClosed);
  
  void set_isPointCloud(bool isPointCloud){ m_isPointCloud = isPointCloud; }
  bool isPointCloud() { return m_isPointCloud;}
  
  void set_pointCloud(const std::vector<dPoint> & P, std::string color,
                      std::string layer);
  // Annotations
  void get_annotations (std::vector<anno> & annotations) const;
  void get_layerAnno(std::vector<anno> & annotations) const;
  void get_vertIndexAnno(std::vector<anno> & annotations) const;
  void set_annotations(const std::vector<anno> & A);
  void set_layerAnno(const std::vector<anno> & annotations);
  void set_vertIndexAnno(const std::vector<anno> & annotations);
  
  void addAnno(const anno & A){m_annotations.push_back(A); }
  void compVertIndexAnno();
  void compLayerAnno();
  
  void bdBox(double & xll, double & yll, double & xur, double & yur) const;
  
  void bdBoxes(std::vector<double> & xll, std::vector<double> & yll,
               std::vector<double> & xur, std::vector<double> & yur) const;
  
  void setPolygon(int numVerts,
                  const double * xv,
                  const double * yv,
                  bool isPolyClosed,
                  const std::string & color,
                  const std::string & layer
                  );
  
  void findClosestPolyVertex(// inputs
                             double x0, double y0,
                             // outputs
                             double & min_x, double & min_y,
                             double & min_dist
                             ) const;
  
  void findClosestPolyEdge(//inputs
                           double x0, double y0,
                           // outputs
                           int & minIndex,
                           double & minX, double & minY, double & minDist
                           ) const;
  
  void erasePoly(int polyIndex);
  void sortFromLargestToSmallest();

  void sortBySizeAndMaybeAddBigContainingRect(// inputs
                                              double bigXll, double bigYll,
                                              double bigXur, double bigYur
                                              );

  void enforce45();
  
private:

  void get_annoByType(std::vector<anno> & annotations, int annoType);
  void set_annoByType(const std::vector<anno> & annotations, int annoType);

  // If isPointCloud is true, treat each point as a set of unconnected points
  bool                     m_isPointCloud; 

  std::vector<double>      m_xv;
  std::vector<double>      m_yv; 
  std::vector<int>         m_numVerts;
  int                      m_numPolys;
  int                      m_totalNumVerts;
  std::vector<char>        m_isPolyClosed;
  std::vector<std::string> m_colors;
  std::vector<std::string> m_layers;
  std::vector<anno>        m_annotations;
  std::vector<anno>        m_vertIndexAnno; // Anno showing vertex index
  std::vector<anno>        m_layerAnno;     // Anno showing layer number
  
};

#endif
