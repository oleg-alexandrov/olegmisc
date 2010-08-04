#ifndef DRAWPOLY_H
#define DRAWPOLY_H

#include <qwidget.h>
#include <qpixmap.h>
#include <qevent.h>
#include <vector>
#include "utils.h"
#include "../../polyUtils/dPoly.h"

class drawPoly : public QWidget{
    Q_OBJECT
public:
    drawPoly( QWidget *parent, 
              const std::vector<std::string> & polyFilesVec,
              const std::vector<bool>        & plotPointsOnlyVec
              );
  
  void zoomIn();
  void zoomOut();
  void shiftRight();
  void shiftLeft();
  void shiftUp();
  void shiftDown();
  void toggleAnno();
  void toggleVertIndices();
  void toggleFilled();
  void resetView();
  void cutToHlt();
  void undoLast();
  void readAllPolys();
  void openPoly();
  void saveOnePoly();
  void saveMultiplePoly();
  void togglePE();
  void changeOrder();
  // actions
  
public slots:

protected:

  void paintEvent( QPaintEvent*);
  void mousePressEvent( QMouseEvent *E);
  void mouseMoveEvent( QMouseEvent *E);
  void keyPressEvent( QKeyEvent *K );
  void mouseReleaseEvent ( QMouseEvent * E );
  void wheelEvent(QWheelEvent *E);
  void contextMenuEvent(QContextMenuEvent *E);

public slots:
  void createPoly();
  void deletePoly();
  void saveMark();
  void toggleNmScale();

private:
  void drawMark(int x0, int y0, QColor color, int lineWidth,
                QPainter * paint);
  void setupDisplayOrder(int                 numPolys, 
                         std::vector<bool> & plotPointsOnlyVec,
                         bool              & changeDisplayOrder,
                         std::vector<int>  & polyVecOrder);
  void drawCurrPolyLine(QPainter * paint);
  void drawPolyLine();
  void addPolyVert(int px, int py);
  double pixelToWorldDist(int pd);
  void createHighlight(// inputs are in pixels
                       int pxll, int pyll, int pxur, int pyur
                       );
  void printCurrCoords(const ButtonState & state, // input
                       int & currX, int  & currY  // in-out
                       );
  void readOnePoly(// inputs
                   std::string & filename,
                   bool plotPointsOnly,
                   // output
                   dPoly & poly           
                   );
  bool isClosestGridPtFree(std::vector< std::vector<int> > & Grid,
                           int x, int y);
  void initScreenGrid(std::vector< std::vector<int> > & Grid);
  bool isPolyZeroDim(const QPointArray & pa);
  void drawRect(const utils::dRect & R, int lineWidth,
                QPainter * paint);
  void centerViewAtPoint(double x, double y);
  void drawOneVertex(int x0, int y0, QColor color, int lineWidth,
                     int drawVertIndex, QPainter * paint);
  void wipeRubberBand(QRect & R);

  void showPoly( QPainter *paint );
  void resetTransformSettings();
  void pixelToWorldCoords(int px, int py,
                          double & wx, double & wy);
  void worldToPixelCoords(double wx, double wy,
                          int & px,  int & py);
  
  static void expandBoxToGivenRatio(// inputs
                                    double screenRatio, 
                                    // inputs/outputs
                                    double & xll,  double & yll,
                                    double & widx, double & widy);
  
  static void setUpViewBox(// inputs
                           const std::vector<dPoly> & polyVec,
                           // outputs
                           double & xll, double & yll,
                           double &widx, double & widy);
  
  void setStandardCursor();
  void setPolyDrawCursor();
  
  double m_zoomFactor, m_shiftX, m_shiftY;
  int m_mousePrsX,  m_mousePrsY, m_mouseRelX,  m_mouseRelY;
  int m_screenXll,  m_screenYll, m_screenWidX, m_screenWidY;
  double m_viewXll, m_viewYll,   m_viewWidX,   m_viewWidY;
  double m_prevClickedX, m_prevClickedY;
  double m_screenRatio;
  
  // Transform the polygons to the device coordinate system
  double m_pixelSize, m_padX, m_padY;
  
  std::vector<dPoly> m_polyVec;
  std::vector< std::vector<dPoly> > m_polyVecStack; // Used for undo
  
  std::vector<std::string> m_polyFilesVec;
  std::vector<bool>    m_plotPointsOnlyVec;
  
  int m_yFactor; // To compensate for Qt's origin in the upper-left corner

  QRect m_rubberBandRect;
  bool m_resetView;
  bool m_prevClickExists;
  
  // For double buffering
  QPixmap m_cache;

  QRect   m_rubberBand;

  bool m_showAnnotations;
  bool m_showFilledPolys;
  
  std::vector<utils::dRect> m_highlights;

  std::vector<int> m_actions;

  static const int m_polyChanged  = 1;
  static const int m_createHlt    = 2;

  int m_showEdges, m_showPoints, m_showPointsEdges, m_toggleShowPointsEdges;
  bool m_changeDisplayOrder, m_showVertIndices;

  bool m_createPoly;
  std::vector<double> m_currPolyX, m_currPolyY;
  std::vector<double> m_markX, m_markY;

  bool m_zoomToMouseSelection;
  
  double m_menuX, m_menuY;

  // If the current point on the polygon being created is closer than
  // this distance (in pixels) from the first point of the polygon, we
  // will assume we arrived back to the first point so we finished
  // creating the polygon.
  int m_pixelTol;

  std::vector<int> m_polyVecOrder;

  // To print points at the nm scale as opposed to the database unit
  // (dbu) scale.
  bool m_useNmScale;
  double m_nmScale;
  std::string m_nmScaleFile;
  
};

#endif // DRAWPOLY_H

