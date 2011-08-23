#ifndef POLYVIEW_H
#define POLYVIEW_H

#include <qwidget.h>
#include <qpixmap.h>
#include <qevent.h>
//Added by qt3to4:
#include <QContextMenuEvent>
#include <Q3PointArray>
#include <QWheelEvent>
#include <QPaintEvent>
#include <QMouseEvent>
#include <QKeyEvent>
#include <vector>
#include "utils.h"
#include "../geom/dPoly.h"
#include "../geom/geomUtils.h"
#include "chooseFilesDlg.h"

class cmdLineOptions;

class polyView : public QWidget{
    Q_OBJECT
public:
    polyView(QWidget *parent, const cmdLineOptions & options);
  
  void runCmd(std::string cmd);
  
public slots:

  // File menu
  void chooseFilesToShow();
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
  void plotDiff(int direction);
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
  void backupPolysForUndo(bool resetViewOnUndo);

  // Options menu
  void setLineWidth();
  void setBgColor();

  // Right-click menu
  void saveMark();
  void toggleNmScale();
  void toggleEditMode();
  void toggleAlignMode();
  void turnOnMoveVertices();
  void turnOnMovePolys();
  void insertVertex();
  void deleteVertex();
  void deletePoly();
  void copyPoly();
  void pastePoly();
  void reversePoly();
  void align_rotate90();
  void align_rotate180();
  void align_rotate270();
  void align_flip_against_y_axis();
  void align_flip_against_x_axis();
  void performAlignmentOfClosePolys();
  
protected:

  void paintEvent(QPaintEvent *);
  void resizeEvent(QResizeEvent*);
  void popUp(std::string msg);
  bool getStringFromGui(std::string title, std::string description,
                        std::string & data // output
                        );
  bool getRealValuesFromGui(std::string title, std::string description,
                            std::vector<double> & values);
  void setBgFgColorsFromPrefs();
  void mousePressEvent( QMouseEvent *E);
  void mouseMoveEvent( QMouseEvent *E);
  void keyPressEvent( QKeyEvent *K );
  void mouseReleaseEvent ( QMouseEvent * E );
  bool isShiftLeftMouse(QMouseEvent * E);
  void wheelEvent(QWheelEvent *E);
  void contextMenuEvent(QContextMenuEvent *E);

private slots:
 void showFilesChosenByUser();
  
private:
  void readAllPolys();
  void refreshPixmap();
  void printCmd(std::string cmd, const std::vector<double> & vals);
  void printCmd(std::string cmd, double xll, double yll,
                double widX, double widY);
  void printCmd(std::string cmd);
  void shiftPolys(std::vector<double> & shifts);
  void rotatePolys(std::vector<double> & angle);
  void scalePolys(std::vector<double> & scale);
  void drawMark(int x0, int y0, QColor color, int lineWidth,
                QPainter * paint);
  void setupDisplayOrder(// Inputs
                         int                 numPolys,
                         // Input-output
                         bool              & changeDisplayOrder,
                         std::vector<int>  & polyVecOrder
                         );
  void drawPolyLine(const std::vector<double> & polyX,
                    const std::vector<double> & polyY,
                    QPainter * paint);
  void addPolyVert(int px, int py);
  void appendToPolyVec(const dPoly & P);
  double pixelToWorldDist(int pd);
  void createHighlightWithPixelInputs(int pxll, int pyll, int pxur, int pyur
                                      );
  
  void createHighlightWithRealInputs(double xll, double yll,
                                     double xur, double yur
                                     );

  void printCurrCoords(const Qt::ButtonState & state, // input
                       int & currX, int  & currY      // in-out
                       );
  bool readOnePoly(// inputs
                   std::string   & filename,
                   bool            plotPointsOnly,
                   closedPolyInfo  isPolyClosed,
                   // output
                   dPoly & poly           
                   );
  bool isClosestGridPtFree(std::vector< std::vector<int> > & Grid,
                           int x, int y);
  void initTextOnScreenGrid(std::vector< std::vector<int> > & Grid);
  bool isPolyZeroDim(const Q3PointArray & pa);
  void centerViewAtPoint(double x, double y);
  void drawOneVertex(int x0, int y0, QColor color, int lineWidth,
                     int drawVertIndex, QPainter * paint);
  void updateRubberBand(QRect & R);

  void displayData( QPainter *paint );
  void plotDPoly(bool plotPoints, bool plotEdges,
                 bool plotFilled, bool showAnno,
                 int lineWidth,
                 int drawVertIndex, // 0 is a good choice here
                 // An empty grid is a good choice if not text is present
                 std::vector< std::vector<int> > & textOnScreenGrid,
                 QPainter *paint,
                 dPoly currPoly // Make a local copy on purpose
                 );
  void resetTransformSettings();
  void pixelToWorldCoords(int px, int py,
                          double & wx, double & wy);
  void worldToPixelCoords(double wx, double wy,
                          int & px,  int & py);
  
  
  void setStandardCursor();
  void setPolyDrawCursor();
  
  void plotDistBwPolyClips( QPainter *paint );
  
  double m_zoomFactor, m_shiftX, m_shiftY;
  int m_mousePrsX,  m_mousePrsY, m_mouseRelX,  m_mouseRelY;
  int m_screenXll,  m_screenYll, m_screenWidX, m_screenWidY;
  double m_viewXll, m_viewYll,   m_viewWidX,   m_viewWidY;
  double m_prevClickedX, m_prevClickedY;
  double m_screenRatio, m_pixelSize;
  
  // Polygons
  std::vector<dPoly>       m_polyVec;

  std::vector<polyOptions> m_polyOptionsVec; // options for exiting polygons
  polyOptions m_prefs;                       // options for future polygons

  // Used for undo
  std::vector< std::vector<dPoly> > m_polyVecStack;
  std::vector< std::vector<polyOptions> > m_polyOptionsVecStack;
  
  std::vector<int> m_actions;
  std::vector<char> m_resetViewOnUndo;

  bool m_resetView;
  bool m_prevClickExists;
  bool m_firstPaintEvent;
  
  // Use double buffering: draw to a pixmap first, refresh it only
  // if really necessary, and display it when paintEvent is called.
  QPixmap m_pixmap;

  std::vector<QPoint> m_snappedPoints, m_nonSnappedPoints;
  int m_smallLen;
  
  QRect   m_emptyRubberBand;
  QRect   m_rubberBand;

  bool m_showAnnotations;
  bool m_showFilledPolys;
  
  std::vector<dPoly> m_highlights;

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

  // To print point coordinates at the nm scale as opposed to the database unit
  // (dbu) scale.
  bool m_useNmScale;
  double m_nmScale;
  std::string m_nmScaleFile;

  // For plotting in diff mode
  bool                        m_polyDiffMode;
  std::vector<dPoly>          m_polyVecBk;
  std::vector<polyOptions>    m_polyOptionsVecBk;
  std::vector<utils::segDist> m_distVec;       // distances b/w polys to diff
  std::vector<double>         m_segX, m_segY;  // segment to plot
  int                         m_indexOfDistToPlot;

  // Choose which files to show or not in the GUI
  chooseFilesDlg        m_chooseFilesDlg;
  std::set<std::string> m_filesNotToShow;

  // Edit mode
  bool   m_editMode;
  bool   m_moveVertices;
  bool   m_movePolys;
  bool   m_movingVertsOrPolysNow;
  bool   m_deletingPolyNow;
  int    m_toggleShowPointsEdgesBk;
  int    m_polyVecIndex;
  int    m_polyIndexInCurrPoly;
  int    m_vertIndexInCurrPoly;
  double m_mousePressWorldX, m_mousePressWorldY;
  dPoly  m_polyBeforeShift;
  dPoly  m_copiedPoly;
  double m_copyPosX, m_copyPosY;
  
  // Align mode (align one file with another file via linear transform)
  bool m_alignMode;
  bool m_aligningPolysNow;
  
};

#endif // POLYVIEW_H

