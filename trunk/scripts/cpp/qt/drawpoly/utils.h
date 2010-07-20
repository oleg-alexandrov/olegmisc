#ifndef UTILS_H
#define UTILS_H
#include <cmath>
#include <iostream>
#include "../../polyUtils/dPoly.h"

namespace utils{

  void extractWindowDims(// inputs
                         int numArgs, char ** args,
                         // outputs
                         int & windowWidX, int & windowWidY
                         );

  void parseCmdOptionsLoadData(//inputs
                               int argc, char** argv, char * progName,
                               int yFactor,
                               // outputs
                               int & windowWidX, int & windowWidY,
                               std::vector<dPoly>  & polyVec, 
                               std::vector<bool>     & plotPointsOnlyVec
                               );

  inline void printUsage(char * progName){
    
    std::cout << "Usage: " << progName << " [ -geo 1000x800 ] file_1.xg ... "
              << "[ -p ] file_N.xg " << std::endl;

  }
  
  inline int iround(double x){ return (int)round(x); }
  inline int iceil (double x){ return (int)ceil( x); }
  inline int ifloor(double x){ return (int)floor(x); }
  inline int isign (double x){
    if (x > 0) return  1;
    if (x < 0) return -1;
    return 0;
  }

  
  class dRect{
    
  public:
    dRect(double xll = 0, double yll = 0, double xur = 0, double yur = 0): 
      m_xll(xll), m_yll(yll), m_xur(xur), m_yur(yur) {}

    double left()   const { return m_xll; }
    double top()    const { return m_yll; }
    double right()  const { return m_xur; }
    double bottom() const { return m_yur; }
    double width()  const { return m_xur - m_xll;}
    double height() const { return m_yur - m_yll;}

  private:

    double m_xll;
    double m_yll;
    double m_xur;
    double m_yur;
  };

  inline void normalize(dRect & R){
    double left  = std::min(R.left(), R.right());
    double right = std::max(R.left(), R.right());
    double top   = std::min(R.top(), R.bottom());
    double bot   = std::max(R.top(), R.bottom());
    R            = dRect(left, top, right, bot);
  }
  
  
}

#endif
