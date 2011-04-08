#ifndef POLYVIEW_H
#define POLYVIEW_H

#include <qwidget.h>
#include <qpixmap.h>
#include <qevent.h>
#include <vector>
#include "utils.h"
#include "../geom/dPoly.h"
#include "../geom/geomUtils.h"

struct cmdLineOptions;

class polyView : public QWidget{
    Q_OBJECT
public:
    polyView(QWidget *parent, const cmdLineOptions & options);
  
  void runCmd(std::string cmd);
  
public slots:

  // File menu
  void openPoly();
  void saveOnePoly();
  void overwriteMultiplePolys();
  void saveAsMultiplePolys();
  void saveMultiplePoly(bool overwrite);
  void shiftPolys();
  void rotatePolys();
  void scalePolys();

  // View menu
  void zoomOut();
  void zoomIn();
  void shiftLeft();
  void shiftRight();
  void shiftUp();
  void shiftDown();
  void resetView();
  void changeOrder();
  void toggleAnno();
  void toggleFilled();
  void toggleShowPolyDiff();
  void plotNextDiff();
  void plotPrevDiff();
  void plotDiff(int dir);
  void togglePE();
  void toggleVertIndexAnno();
  void toggleLayerAnno();

  // Edit menu
  void undoLast();
  void cutToHlt();
  void create45DegreeIntPoly();
  void createArbitraryPoly();

  // Transform menu
  void enforce45();

  // Options menu
  void setLineWidth();

  // Right-click menu
  void saveMark();
  void toggleNmScale();
  void deletePoly();

protected:

  void paintEvent( QPaintEvent*);
  void popUp(std::string msg);
  bool getValuesFromGui(std::string title, std::string description,
                        std::vector<double> & values);
  void mousePressEvent( QMouseEvent *E);
  void mouseMoveEvent( QMouseEvent *E);
  void keyPressEvent( QKeyEvent *K );
  void mouseReleaseEvent ( QMouseEvent * E );
  void wheelEvent(QWheelEvent *E);
  void contextMenuEvent(QContextMenuEvent *E);

private:
  void readAllPolys();
  void printCmd(std::string cmd, const std::vector<double> & vals);
  void printCmd(std::string cmd, double xll, double yll,
                double widX, double widY);
  void printCmd(std::string cmd);
  void shiftPolys(std::vector<double> & shifts);
  void rotatePolys(std::vector<double> & angle);
  void scalePolys(std::vector<double> & scale);
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
  void createHighlightWithPixelInputs(int pxll, int pyll, int pxur, int pyur
                                      );
  
  void createHighlightWithRealInputs(double xll, double yll,
                                     double xur, double yur
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
  
  static void setUpViewBox(// inputs
                           const std::vector<dPoly> & polyVec,
                           // outputs
                           double & xll, double & yll,
                           double &widx, double & widy);
  
  void setStandardCursor();
  void setPolyDrawCursor();
  
  void sortBySizeAndMaybeAddBigFgPoly(// inputs
                                      double viewXll, double viewYll,
                                      double viewXur, double viewYur,
                                      // input-output
                                      dPoly& poly
                                      );

  void plotDistBwPolyClips( QPainter *paint );
  
  double m_zoomFactor, m_shiftX, m_shiftY;
  int m_mousePrsX,  m_mousePrsY, m_mouseRelX,  m_mouseRelY;
  int m_screenXll,  m_screenYll, m_screenWidX, m_screenWidY;
  double m_viewXll, m_viewYll,   m_viewWidX,   m_viewWidY;
  double m_prevClickedX, m_prevClickedY;
  double m_screenRatio, m_pixelSize;
  
  // Polygons, plot as points or not, and file names.
  // These three vectors must always have the same size.
  std::vector<dPoly>       m_polyVec;
  std::vector<bool>        m_plotPointsOnlyVec;
  std::vector<std::string> m_polyFilesVec;

  // Used for undo
  std::vector< std::vector<dPoly> > m_polyVecStack;
  std::vector<int> m_actions;
  std::vector<bool> m_resetViewOnUndo;

  int m_lineWidth;
  
  bool m_useCmdLineColors;
  std::vector<std::string> m_cmdLineColors;
  bool                     m_plotAsLines;
  bool                     m_noClosedPolys;
  
  QRect m_rubberBandRect;
  bool m_resetView;
  bool m_prevClickExists;
  
  // For double buffering
  QPixmap m_cache;

  QRect   m_rubberBand;

  bool m_showAnnotations;
  bool m_showFilledPolys;
  
  std::vector<utils::dRect> m_highlights;

  int m_polyChanged, m_createHlt;

  int m_showEdges, m_showPoints, m_showPointsEdges, m_toggleShowPointsEdges;
  bool m_changeDisplayOrder, m_showVertIndexAnno, m_showLayerAnno;

  bool m_createPoly, m_snapPolyTo45DegreeIntGrid;
  std::vector<double> m_currPolyX, m_currPolyY;
  std::vector<double> m_markX, m_markY;

  bool m_zoomToMouseSelection, m_viewChanged;
  
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

  // For plotting in diff mode
  bool m_polyDiffMode;
  std::vector<dPoly> m_polyVecBk;
  std::vector<bool>  m_plotPointsOnlyVecBk;
  std::vector<std::string> m_polyFilesVecBk;
  std::vector<utils::segDist> m_distVec; // distances b/w polys to diff
  std::vector<double> m_segX, m_segY;    // segment to plot
  
  int m_indexOfDistToPlot;
  
};

#endif // POLYVIEW_H

