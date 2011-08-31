#include <qwidget.h>
#include <QContextMenuEvent>
#include <QFileDialog>
#include <QKeyEvent>
#include <Q3PopupMenu>
#include <Q3PointArray>
#include <QMouseEvent>
#include <QPaintEvent>
#include <QWheelEvent>
#include <QStylePainter>
#include <QStyleOptionFocusRect>
#include <QTableWidget>
#include <cassert>
#include <cfloat>    // defines DBL_MAX
#include <cmath>
#include <cstdlib>
#include <iomanip>   // required for use of setw()
#include <iostream>
#include <qapplication.h>
#include <qcursor.h>
#include <qdir.h>
#include <qinputdialog.h>
#include <qpainter.h>
#include "polyView.h"
#include <qmessagebox.h>
#include "../geom/polyUtils.h"
#include "utils.h"

using namespace std;
using namespace utils;

// To do: handle colors correctly (convert dark-gray to darkGray, etc.).
// To do: In the geom directory, put everything in a namespace, say called 'pv'.
//        Here too. Clean up, modularize, and structure the code more.
// To do: Fix other "To do" mentioned in the code.
// To do: The viewer does not render correctly in fill mode overlapping polygons
//        with each polygon having holes. A fix would require a thorough analysis
//        which would identify which hole belongs to which polygon.
// To do: Replace cmdLineOptions directly with polyOptionsVec.
// To do: Make font size a preference.

polyView::polyView(QWidget *parent, const cmdLineOptions & options): QWidget(parent){

  // Choose which files to show in the GUI
  QObject::connect(m_chooseFilesDlg.getFilesTable(),
                   SIGNAL(cellClicked(int, int)),
                   this,
                   SLOT(showFilesChosenByUser())
                   );
  
  // Preferences per polygon file. The element in the vector
  // m_polyOptionsVec below is not associated with any polygon
  // file. Set it apart, it will be used for new polygons.
  m_polyOptionsVec = options.polyOptionsVec;
  assert(m_polyOptionsVec.size() >= 1);
  m_prefs = m_polyOptionsVec.back(); m_polyOptionsVec.pop_back();
  m_prefs.plotAsPoints = false; // most likely the user wants to see edges not points
  setBgFgColorsFromPrefs(); // must be called after m_prefs is set
  
  setStandardCursor();

  // int
  m_screenXll  = 0; m_screenYll  = 0;
  m_screenWidX = 0; m_screenWidY = 0;

  // double
  m_viewXll  = 0.0; m_viewYll  = 0.0;
  m_viewWidX = 0.0; m_viewWidY = 0.0;

  m_resetView       = true;
  m_prevClickExists = false;
  m_firstPaintEvent = true;
  
  m_showAnnotations    = true;
  m_showVertIndexAnno  = false;
  m_showLayerAnno      = false;
  m_showFilledPolys    = false;
  m_changeDisplayOrder = false;
  
  m_emptyRubberBand = QRect(-10, -10, 0, 0); // off-screen rubberband
  m_rubberBand      = m_emptyRubberBand;

  m_showEdges             = 1;
  m_showPointsEdges       = 2;
  m_showPoints            = 3;
  m_toggleShowPointsEdges = m_showEdges;

  // These are constants
  m_polyChanged  = 1;
  m_createHlt    = 2;
  
  m_createPoly                = false;
  m_snapPolyTo45DegreeIntGrid = false;
  m_currPolyX.clear(); m_currPolyY.clear();

  m_zoomToMouseSelection = false;
  m_viewChanged          = false;

  m_zoomFactor = 1.0;
  m_mousePrsX = 0; m_mousePrsY = 0;
  m_mouseRelX = 0; m_mouseRelY = 0;
  
  // Points closer than this are in some situations considered equal
  m_pixelTol = 5;
  
  m_useNmScale  = false;
  m_nmScale     = 1.0;
#ifdef SCALE_FILE
  m_nmScaleFile = SCALE_FILE;
#else
  m_nmScaleFile = "scale.txt";
#endif

  // Used for undo
  m_polyVecStack.clear();
  m_polyOptionsVecStack.clear();
  m_actions.clear();
  m_resetViewOnUndo.clear();
  
  // Show poly diff mode
  m_polyDiffMode = false;
  m_polyVecBk.clear();
  m_polyOptionsVecBk.clear();
  m_distVec.clear(); // distances b/w polys to diff
  m_indexOfDistToPlot = -1;

  // Plot the point wheres the mouse clicked (with or without snapping
  // to the closest vertex).
  m_snappedPoints.clear();
  m_nonSnappedPoints.clear();
  m_smallLen = 2; // Used for plotting the points

  // Edit mode
  m_editMode                = false;
  m_moveVertices            = false;
  m_moveEdges               = false;
  m_movePolys               = false;
  m_toggleShowPointsEdgesBk = m_showEdges;
  m_polyVecIndex            = -1;
  m_polyIndexInCurrPoly     = -1;
  m_vertIndexInCurrPoly     = -1;
  m_copyPosX                = 0.0;
  m_copyPosY                = 0.0;
  
  // Align mode
  m_alignMode = false;
  
  resetTransformSettings();

  // This statement must be the last
  readAllPolys(); // To do: avoid global variables here

  return;
}

void polyView::displayData( QPainter *paint ){

  // To do: this function needs modularization and some cleanup.

  assert( m_polyVec.size() == m_polyOptionsVec.size() );

  // Dimensions of the plotting window in pixels exluding any window
  // frame/menu bar/status bar
  QRect v       = this->geometry();
  m_screenXll   = v.left();
  m_screenYll   = v.top();
  m_screenWidX  = v.width();
  m_screenWidY  = v.height();
  //cout << "geom is: " << m_screenXll << ' ' << m_screenYll << ' '
  //     << m_screenWidX << ' ' << m_screenWidY << endl;
  
  m_screenRatio = double(m_screenWidY)/double(m_screenWidX);

  if (m_resetView){
    setUpViewBox(// inputs
                 m_polyVec,
                 // outputs
                 m_viewXll, m_viewYll, m_viewWidX, m_viewWidY
                 );
    m_resetView = false;
  }

  //  This is necessary when the screen is resized
  expandBoxToGivenRatio(m_screenRatio,                               // input
                        m_viewXll, m_viewYll, m_viewWidX, m_viewWidY // in/out
                        );

  // Create the new view
  double xll, yll, xur, yur, widx, widy;
    
  if (m_zoomToMouseSelection){
    
    // Form a new view based on the rectangle selected with the mouse.
    // The call to pixelToWorldCoords uses the existing view internally.
    pixelToWorldCoords(m_mousePrsX, m_mousePrsY, xll, yur); // upper-left  rect corner
    pixelToWorldCoords(m_mouseRelX, m_mouseRelY, xur, yll); // lower-right rect corner
    widx = xur - xll;
    widy = yur - yll;

  }else if (m_viewChanged){

    // Modify the view for given shift or zoom
    xll  = m_viewXll + m_viewWidX*( (1 - m_zoomFactor)/2.0 + m_shiftX );
    yll  = m_viewYll + m_viewWidY*( (1 - m_zoomFactor)/2.0 + m_shiftY );
    widx = m_viewWidX*m_zoomFactor;
    widy = m_viewWidY*m_zoomFactor;
    
    resetTransformSettings(); // Wipe the zoom and shift data 
  
  }

  if (m_zoomToMouseSelection || m_viewChanged){
    
    // If the view becomes too small, don't accept it
    if (xll + widx <= xll || yll + widy <= yll){
      cerr << "Cannot zoom to requested view."  << endl;
    }else{
      // Enlarge this rectangle if necessary to keep the aspect ratio.
      expandBoxToGivenRatio(//inputs
                            m_screenRatio,  
                            // input/outputs
                            xll, yll, widx, widy
                            );
      
      // Overwrite the view
      m_viewXll = xll; m_viewWidX = widx;
      m_viewYll = yll; m_viewWidY = widy;
    }
    printCmd("view", m_viewXll, m_viewYll, m_viewWidX, m_viewWidY);
    m_zoomToMouseSelection = false;
    m_viewChanged          = false;
  }

  // The two ratios below will always be the same. Take the maximum
  // for robustness to floating point errors.
  m_pixelSize = max(m_viewWidX/m_screenWidX, m_viewWidY/m_screenWidY);
  
  // Use a grid to not draw text too densely as that's slow
  vector< vector<int> > textOnScreenGrid; 
  initTextOnScreenGrid(textOnScreenGrid);
  
  // Plot the polygons

  // Will draw a vertex with a shape dependent on this index
  int drawVertIndex = -1; 
  
  setupDisplayOrder(m_polyVec.size(),                    //inputs
                    m_changeDisplayOrder, m_polyVecOrder // inputs-outputs
                    );
  
  // Draw the polygons
  for (int vi  = 0; vi < (int)m_polyVec.size(); vi++){

    int vecIter = m_polyVecOrder[vi];

    // Skip the files the user does not want to see
    string fileName = m_polyOptionsVec[vecIter].polyFileName;
    if (m_filesNotToShow.find(fileName) != m_filesNotToShow.end()) continue;
      
    int lineWidth = m_polyOptionsVec[vecIter].lineWidth;

    // Note: plotFilled, plotEdges, and plotPoints are not mutually exclusive.
    bool plotFilled = m_polyOptionsVec[vecIter].isPolyFilled || m_showFilledPolys;
    bool plotEdges  = (!m_polyOptionsVec[vecIter].plotAsPoints) &&
      (m_toggleShowPointsEdges != m_showPoints);
    bool plotPoints = m_polyOptionsVec[vecIter].plotAsPoints  ||
      ( m_toggleShowPointsEdges == m_showPoints )             ||
      ( m_toggleShowPointsEdges == m_showPointsEdges);
    
    if (plotPoints) drawVertIndex++;

    bool showAnno = true;
    plotDPoly(plotPoints, plotEdges, plotFilled, showAnno, lineWidth,  
              drawVertIndex,    // 0 is a good choice here
              textOnScreenGrid, // empty grid is fine here  
              paint,  
              m_polyVec[vecIter]
              );
    
  } // End iterating over sets of polygons

  // Plot the highlights
  bool plotPoints = false, plotEdges = true, plotFilled = false;
  drawVertIndex = 0;
  for (int h = 0; h < (int)m_highlights.size(); h++){
    m_highlights[h].set_color(m_prefs.fgColor.c_str());
    bool showAnno = false;
    plotDPoly(plotPoints, plotEdges, plotFilled, showAnno, m_prefs.lineWidth,  
              drawVertIndex,    // 0 is a good choice here
              textOnScreenGrid, // empty grid is fine here  
              paint,  
              m_highlights[h]
              );
  }
  
  // This draws the polygon being created if in that mode
  drawPolyLine(m_currPolyX, m_currPolyY, paint);

  // Draw the mark if there
  if (m_markX.size() > 0){
    int x0, y0;
    worldToPixelCoords(m_markX[0], m_markY[0], // inputs
                       x0, y0                  // outputs
                       );
    drawMark(x0, y0, QColor(m_prefs.fgColor.c_str()), m_prefs.lineWidth, paint);
  }

  // If in diff mode
  plotDistBwPolyClips(paint);

  // Wipe this temporary data now that the display changed
  m_snappedPoints.clear();
  m_nonSnappedPoints.clear(); 
  
  return;
}

void polyView::plotDPoly(bool plotPoints, bool plotEdges,
                         bool plotFilled, bool showAnno,
                         int lineWidth,
                         int drawVertIndex, // 0 is a good choice here
                         // An empty grid is a good choice if not text is present
                         std::vector< std::vector<int> > & textOnScreenGrid,
                         QPainter *paint,
                         dPoly currPoly // Make a local copy on purpose
                         ){

  // Plot a given dPoly with given options.
  
  // Note: Having annotations at vertices can make the display
  // slow for large polygons.
  // The operations below must happen before cutting,
  // as cutting will inherit the result computed here.
  if (m_showVertIndexAnno){
    currPoly.compVertIndexAnno();
  }else if (m_showLayerAnno){
    currPoly.compLayerAnno();
  }
    
  // When polys are filled, plot largest polys first
  if (plotFilled)
    currPoly.sortBySizeAndMaybeAddBigContainingRect(m_viewXll,  m_viewYll,
                                                    m_viewXll + m_viewWidX,
                                                    m_viewYll + m_viewWidY
                                                    );

  // Clip the polygon a bit beyond the viewing window, as to not see
  // the edges where the cut took place. It is a bit tricky to
  // decide how much the extra should be.
  double tol    = 1e-12;
  double extra  = 2*m_pixelSize*lineWidth;
  double extraX = extra + tol*max(abs(m_viewXll), abs(m_viewXll + m_viewWidX));
  double extraY = extra + tol*max(abs(m_viewYll), abs(m_viewYll + m_viewWidY));
    
  dPoly clippedPoly;
  currPoly.clipPoly(//inputs
                    m_viewXll - extraX,
                    m_viewYll - extraY,
                    m_viewXll + m_viewWidX + extraX,
                    m_viewYll + m_viewWidY + extraY,
                    // output
                    clippedPoly
                    );

  const double * xv               = clippedPoly.get_xv();
  const double * yv               = clippedPoly.get_yv();
  const int    * numVerts         = clippedPoly.get_numVerts();
  int numPolys                    = clippedPoly.get_numPolys();
  const vector<char> isPolyClosed = clippedPoly.get_isPolyClosed();
  const vector<string> colors     = clippedPoly.get_colors();
  //int numVerts                  = clippedPoly.get_totalNumVerts();
    
  vector<anno> annotations;
  annotations.clear();
  if (showAnno){
    if (m_showVertIndexAnno){
      clippedPoly.get_vertIndexAnno(annotations);
    }else if (m_showLayerAnno){
      clippedPoly.get_layerAnno(annotations);
    }else if (m_showAnnotations){
      clippedPoly.get_annotations(annotations);
    }
  }
  
  int start = 0;
  for (int pIter = 0; pIter < numPolys; pIter++){

    if (pIter > 0) start += numVerts[pIter - 1];

    // Change the poly file color if it is the background color or invalid
    QColor color = QColor( colors[pIter].c_str() );
    if ( color == backgroundColor() || color == QColor::Invalid){
      if ( backgroundColor() != QColor("white") ){
        color = QColor("white");
      }else{
        color = QColor("black");
      }
    }
      
    int pSize = numVerts[pIter];

    // Determine the orientation of polygons
    double signedArea = 0.0;
    if (plotFilled && isPolyClosed[pIter]){
      signedArea = signedPolyArea(pSize, xv + start, yv + start);
    }
      
    Q3PointArray pa(pSize);
    for (int vIter = 0; vIter < pSize; vIter++){

      int x0, y0;
      worldToPixelCoords(xv[start + vIter], yv[start + vIter], // inputs
                         x0, y0                                // outputs
                         );
      pa[vIter] = QPoint(x0, y0);

      // Qt's built in points are too small. Instead of drawing a point
      // draw a small shape. 
      if ( plotPoints                                          &&
           x0 > m_screenXll && x0 < m_screenXll + m_screenWidX && 
           y0 > m_screenYll && y0 < m_screenYll + m_screenWidY
           ){
        drawOneVertex(x0, y0, color, lineWidth, drawVertIndex, paint);
      }
    }
      
    if (plotEdges){

      if (plotFilled && isPolyClosed[pIter]){
        if (signedArea >= 0.0) paint->setBrush( color );
        else                   paint->setBrush( backgroundColor() ); 
        paint->setPen( Qt::NoPen );
      }else {
        paint->setBrush( Qt::NoBrush );
        paint->setPen( QPen(color, lineWidth) );
      }

      if ( pa.size() >= 1 && isPolyZeroDim(pa) ){
        // Treat the case of polygons which are made up of just one point
        int l_drawVertIndex = -1;
        drawOneVertex(pa[0].x(), pa[0].y(), color, lineWidth, l_drawVertIndex,
                      paint);
      }else if (isPolyClosed[pIter]){
        paint->drawPolygon( pa );
      }else{
        paint->drawPolyline( pa ); // don't join the last vertex to the first
      }
        
    }
  }

  // Plot the annotations
  int numAnno = annotations.size();
  for (int aIter = 0; aIter < numAnno; aIter++){
    const anno & A = annotations[aIter];
    int x0, y0;
    worldToPixelCoords(A.x, A.y, // inputs
                       x0, y0    // outputs
                       );
    paint->setPen( QPen(QColor("gold"), lineWidth) );
    if (isClosestGridPtFree(textOnScreenGrid, x0, y0)){
      paint->drawText(x0, y0, (A.label).c_str());
    }
      
  } // End placing annotations
  
  return;
}

void polyView::zoomIn(){
  m_zoomFactor  = 0.5;
  m_viewChanged = true;
  refreshPixmap();
}

void polyView::zoomOut(){
  m_zoomFactor  = 2.0;
  m_viewChanged = true;
  refreshPixmap();
}

void polyView::shiftRight(){
  m_shiftX      = 0.25;
  m_viewChanged = true;
  refreshPixmap();
}

void polyView::shiftLeft(){
  m_shiftX      = -0.25;
  m_viewChanged = true;
  refreshPixmap();
}

void polyView::shiftUp(){
  m_shiftY      = 0.25;
  m_viewChanged = true;
  refreshPixmap();
}

void polyView::shiftDown(){
  m_shiftY      = -0.25;
  m_viewChanged = true;
  refreshPixmap();
}

void polyView::centerViewAtPoint(double x, double y){
  m_viewXll     = x - m_viewWidX/2.0;
  m_viewYll     = y - m_viewWidY/2.0;
  m_viewChanged = true;
}

void polyView::resetView(){
  m_resetView   = true;
  m_viewChanged = true;
  refreshPixmap();
}


void polyView::resetTransformSettings(){
  m_zoomFactor = 1.0;
  m_shiftX     = 0.0; m_shiftY = 0.0;
}

void polyView::mousePressEvent( QMouseEvent *E){

  const QPoint Q = E->pos();
  m_mousePrsX = Q.x();
  m_mousePrsY = Q.y();

#if 0
  cout << "Mouse pressed at "
       << m_mousePrsX << ' ' << m_mousePrsY << endl;
#endif

  pixelToWorldCoords(m_mousePrsX, m_mousePrsY,              // inputs
                     m_mousePressWorldX, m_mousePressWorldY // outputs
                     ); 

  m_rubberBand = m_emptyRubberBand;

  // This must happen before m_movingVertsOrPolysNow is declared.
  m_deletingPolyNow = ( m_editMode && ( E->modifiers() & Qt::AltModifier ) &&
                        ( E->modifiers() & Qt::ShiftModifier ) );

  m_aligningPolysNow = ( m_alignMode && isShiftLeftMouse(E) && !m_createPoly );
  if (m_aligningPolysNow){
    backupPolysForUndo(false);
    assert(m_polyVec.size() >= 1);
    m_polyBeforeShift = m_polyVec[0];
    m_T.reset();
  }
  
  m_movingVertsOrPolysNow = ( m_editMode && isShiftLeftMouse(E) &&
                              !m_createPoly && ! m_deletingPolyNow);
  if (m_movingVertsOrPolysNow){

    backupPolysForUndo(false);

    double min_x, min_y, min_dist;
    if (m_moveVertices){
      findClosestPolyVertex(// inputs
                            m_mousePressWorldX, m_mousePressWorldY, m_polyVec,
                            // outputs
                            m_polyVecIndex,
                            m_polyIndexInCurrPoly,
                            m_vertIndexInCurrPoly,
                            min_x, min_y, min_dist
                            );
    }else if (m_moveEdges || m_movePolys){
      findClosestPolyEdge(// inputs
                          m_mousePressWorldX, m_mousePressWorldY, m_polyVec,  
                          // outputs
                          m_polyVecIndex,
                          m_polyIndexInCurrPoly,
                          m_vertIndexInCurrPoly,
                          min_x, min_y, min_dist
                          );
      if (m_polyVecIndex >= 0) m_polyBeforeShift = m_polyVec[m_polyVecIndex];
    }
    
    return;
  }

  return;
}

void polyView::mouseMoveEvent( QMouseEvent *E){

  const QPoint Q = E->pos();
  int x = Q.x();
  int y = Q.y();

  double wx, wy;
  pixelToWorldCoords(x, y, wx, wy);
  
  if (m_aligningPolysNow){
    m_polyVec[0] = m_polyBeforeShift;
    m_polyVec[0].applyTransform(1, 0, 0, 1,
                                wx - m_mousePressWorldX, 
                                wy - m_mousePressWorldY,
                                m_T
                                );
    refreshPixmap();
    return;
  }
  
  if (m_movingVertsOrPolysNow){
    if (m_polyVecIndex        < 0 ||
        m_polyIndexInCurrPoly < 0 ||
        m_vertIndexInCurrPoly < 0) return;
    if (m_moveVertices){
      m_polyVec[m_polyVecIndex].changeVertexValue(m_polyIndexInCurrPoly,
                                                  m_vertIndexInCurrPoly,
                                                  wx, wy
                                                  );
    }else if ( (m_moveEdges || m_movePolys) && m_polyVecIndex >= 0){
      m_polyVec[m_polyVecIndex] = m_polyBeforeShift;
      if (m_moveEdges){
        m_polyVec[m_polyVecIndex].shiftEdge(m_polyIndexInCurrPoly,
                                            m_vertIndexInCurrPoly,
                                            wx - m_mousePressWorldX, 
                                            wy - m_mousePressWorldY 
                                            );
      }else if (m_movePolys){
        m_polyVec[m_polyVecIndex].shiftOnePoly(m_polyIndexInCurrPoly,
                                               wx - m_mousePressWorldX, 
                                               wy - m_mousePressWorldY 
                                               );
      }
    }
    refreshPixmap(); // To do: Need to update just a small region, not the whole screen
    return;
  }

  // Standard Qt rubberband trick (kind of confusing as to how it works).
  updateRubberBand(m_rubberBand);
  m_rubberBand = QRect( min(m_mousePrsX, x), min(m_mousePrsY, y),
                        abs(x - m_mousePrsX), abs(y - m_mousePrsY) );
  updateRubberBand(m_rubberBand);
  
  return;
}

void polyView::mouseReleaseEvent ( QMouseEvent * E ){

  const QPoint Q = E->pos();
  m_mouseRelX = Q.x();
  m_mouseRelY = Q.y();
#if 0
  cout << "Mouse pressed at "
       << m_mousePrsX << ' ' << m_mousePrsY << endl;
  cout << "Mouse released at "
       << m_mouseRelX << ' ' << m_mouseRelY << endl;
#endif

  if ( m_editMode && m_deletingPolyNow ){
    // To do: consolidate this with the other call to this function.
    // See if can pass the relevant variables as input arguments.
    pixelToWorldCoords(m_mouseRelX, m_mouseRelY, m_menuX, m_menuY); 
    deletePoly();
    return;
  }

  if (m_aligningPolysNow){
    m_T.print();
    m_totalT = composeTransforms(m_T, m_totalT);
  }
  
  if (m_aligningPolysNow || m_movingVertsOrPolysNow){
    refreshPixmap();
    return;
  }

  // Wipe the rubberband
  updateRubberBand(m_rubberBand); 
  m_rubberBand = m_emptyRubberBand;
  updateRubberBand(m_rubberBand);

  if ( E->modifiers() & Qt::ControlModifier ){
    // Draw a  highlight with control + left mouse button
    // ending at the current point
    createHighlightWithPixelInputs(m_mousePrsX, m_mousePrsY,
                                   m_mouseRelX, m_mouseRelY);
    refreshPixmap();
    return;
  }

  int tol = 5; 
  // Any selection smaller than 'tol' number of pixels will be ignored
  // as perhaps the user moved the mouse unintentionally between press
  // and release.
  if       (m_mouseRelX > m_mousePrsX + tol &&
            m_mouseRelY > m_mousePrsY + tol){
    
    m_zoomToMouseSelection = true; // Will zoom to the region selected with the mouse
    refreshPixmap(); 
    return;
    
  }else if (m_mouseRelX + tol < m_mousePrsX &&
            m_mouseRelY + tol < m_mousePrsY ){
    
    zoomOut();
    return;
    
  }else if (abs(m_mouseRelX - m_mousePrsX) <= tol &&
            abs(m_mouseRelY - m_mousePrsY) <= tol){

  
    if (m_createPoly){
      addPolyVert(m_mouseRelX, m_mouseRelY);
      return;
    }
    
    printCurrCoords(E->state(),              // input
                    m_mouseRelX, m_mouseRelY // in-out
                    );
    return;
  }
  
  return;
}

bool polyView::isShiftLeftMouse(QMouseEvent * E){
  // This does not work in mouseReleaseEvent. 
  return ( E->buttons() & Qt::LeftButton  ) && ( E->modifiers() & Qt::ShiftModifier );
}

void polyView::wheelEvent(QWheelEvent *E){

  int delta = E->delta();

  if (E->state() == Qt::ControlModifier){

    // The control button was pressed. Zoom in/out around the current point.

    int pixelx = E->x();
    int pixely = E->y();
    double x, y;
    pixelToWorldCoords(pixelx, pixely, x, y);
    centerViewAtPoint(x, y);
    
    if (delta > 0){
      zoomIn();
    }else if (delta < 0){
      zoomOut();
    }
    
  }else{

    // Shift wheel goes left and right. Without shift we go up and down.
    if (E->state() == Qt::ShiftModifier){
      if (delta > 0){
        shiftLeft();
      }else if (delta < 0){
        shiftRight();
      }
    }else{
      if (delta > 0){
        shiftUp();
      }else if (delta < 0){
        shiftDown();
      }
    }
  }
  
  E->accept();
}

void polyView::keyPressEvent( QKeyEvent *K ){

  switch ( K->key() ) {
  case Qt::Key_Minus:
    zoomOut();
    break;
  case Qt::Key_Plus:
    zoomIn();
    break;
  case Qt::Key_Equal:
    zoomIn();
    break;
  case Qt::Key_Right:
    shiftRight();
    break;
  case Qt::Key_Left:
    shiftLeft();
    break;
  case Qt::Key_Up:
    shiftUp();
    break;
  case Qt::Key_Down:
    shiftDown();
    break;
  }
  
}

void polyView::contextMenuEvent(QContextMenuEvent *E){

  int x = E->x(), y = E->y();
  pixelToWorldCoords(x, y, m_menuX, m_menuY);

  Q3PopupMenu menu(this);

  int id = 1;

  if (!m_alignMode){
    menu.insertItem("Edit mode", this, SLOT(toggleEditMode()), 0, id);
    menu.setItemChecked(id, m_editMode);
    id++;
  }
  
  if (m_editMode){

    menu.insertItem("Move vertices (Shift-Mouse)", this,
                    SLOT(turnOnMoveVertices()), 0, id);
    menu.setItemChecked(id, m_moveVertices);
    id++;

    menu.insertItem("Move edges (Shift-Mouse)", this,
                    SLOT(turnOnMoveEdges()), 0, id);
    menu.setItemChecked(id, m_moveEdges);
    id++;

    menu.insertItem("Move polygons (Shift-Mouse)", this,
                    SLOT(turnOnMovePolys()), 0, id);
    menu.setItemChecked(id, m_movePolys);
    id++;
    
    menu.insertItem("Insert vertex on edge", this, SLOT(insertVertex()));
    menu.insertItem("Delete vertex",         this, SLOT(deleteVertex()));
    menu.insertItem("Copy polygon",          this, SLOT(copyPoly()));
    menu.insertItem("Paste polygon",         this, SLOT(pastePoly()));
    menu.insertItem("Reverse orientation",   this, SLOT(reversePoly()));
    menu.insertItem("Delete polygon (Alt-Shift-Mouse)", this, SLOT(deletePoly()));
      
    menu.addSeparator();
    
  }else{
  
    menu.insertItem("Align mode", this, SLOT(toggleAlignMode()), 0, id);
    menu.setItemChecked(id, m_alignMode);
    id++;
    
  }

  if (m_alignMode){
    menu.insertItem("Rotate  90 degrees",  this, SLOT(align_rotate90()));
    menu.insertItem("Rotate 180 degrees",  this, SLOT(align_rotate180()));
    menu.insertItem("Rotate 270 degrees",  this, SLOT(align_rotate270()));
    menu.insertItem("Flip against x axis", this, SLOT(align_flip_against_x_axis()));
    menu.insertItem("Flip against y axis", this, SLOT(align_flip_against_y_axis()));
    menu.insertItem("Guess alignment", this,
                    SLOT(performAlignmentOfClosePolys()));

    menu.addSeparator();
  }
  
  menu.insertItem("Create 45-degree integer polygon", this,
                  SLOT(create45DegreeIntPoly()));
  menu.insertItem("Create arbitrary polygon", this,
                  SLOT(createArbitraryPoly()));
  
  menu.insertItem("Save mark at point", this, SLOT(saveMark()));
  
  if (!m_editMode){
    menu.insertItem("Use nm scale", this, SLOT(toggleNmScale()), 0, id);
    menu.setItemChecked(id, m_useNmScale);
    id++;
  }

  menu.exec(E->globalPos());

  return;
}

void polyView::copyPoly(){

  if (!m_editMode) return;
    
  m_copyPosX = m_menuX;
  m_copyPosY = m_menuY;

  double min_x, min_y, min_dist;
  int polyVecIndex, polyIndexInCurrPoly, vertIndexInCurrPoly;
  findClosestPolyEdge(// inputs
                      m_copyPosX, m_copyPosY, m_polyVec,  
                      // outputs
                      polyVecIndex,
                      polyIndexInCurrPoly,
                      vertIndexInCurrPoly,
                      min_x, min_y, min_dist
                      );
  if (polyVecIndex < 0 || polyIndexInCurrPoly < 0) return;
  
  m_polyVec[polyVecIndex].extractOnePoly(polyIndexInCurrPoly,  // input
                                         m_copiedPoly          // output
                                         );
  
  return;
}

void polyView::pastePoly(){

  if (!m_editMode) return;

  dPoly P = m_copiedPoly;
  P.shift(m_menuX - m_copyPosX, m_menuY - m_copyPosY);
  appendToPolyVec(P);
  refreshPixmap();

  return;
}

void polyView::reversePoly(){

  if (!m_editMode) return;
    
  m_copyPosX = m_menuX;
  m_copyPosY = m_menuY;

  double min_x, min_y, min_dist;
  int polyVecIndex, polyIndexInCurrPoly, vertIndexInCurrPoly;
  findClosestPolyEdge(// inputs
                      m_copyPosX, m_copyPosY, m_polyVec,  
                      // outputs
                      polyVecIndex,
                      polyIndexInCurrPoly,
                      vertIndexInCurrPoly,
                      min_x, min_y, min_dist
                      );
  if (polyVecIndex < 0 || polyIndexInCurrPoly < 0) return;
  
  backupPolysForUndo(false);

  m_polyVec[polyVecIndex].reverseOnePoly(polyIndexInCurrPoly);

  refreshPixmap();
  
  return;
}

void polyView::refreshPixmap(){

  // Draw the data onto the pixmap instead of the screen
  // directly. Later we'll display the pixmap without redrawing
  // whenever possible for reasons of speed.
  
  m_pixmap = QPixmap(size());
  m_pixmap.fill(this, 0, 0);
  
  QPainter paint(&m_pixmap);
  paint.initFrom(this);

  QFont F;
  F.setPointSize(m_prefs.fontSize);
  //F.setStyleStrategy(QFont::NoAntialias);
  paint.setFont(F);

  displayData( &paint );
  update();

  return;
}

void polyView::paintEvent(QPaintEvent *){

  if (m_firstPaintEvent){
    // This will be called the very first time the display is
    // initialized. There must be a better way.
    m_firstPaintEvent = false;
    refreshPixmap();
  }
  
  QStylePainter paint(this);
  paint.drawPixmap(0, 0, m_pixmap);

  QColor fgColor = QColor(m_prefs.fgColor.c_str());
  paint.setPen(fgColor);
  paint.drawRect(m_rubberBand.normalized().adjusted(0, 0, -1, -1));

  // Plot the mouse clicks (snapped or not snapped to closest vertex).
  // Do it here since those are temporary, they are supposed
  // to go away when the display changes such as on zoom.
  paint.setPen( QPen(fgColor, m_prefs.lineWidth) );
  paint.setBrush( Qt::NoBrush );
  for (int p = 0; p < (int)m_snappedPoints.size(); p++){
    const QPoint & P = m_snappedPoints[p];
    paint.drawEllipse(P.x() - m_smallLen, P.y() - m_smallLen,
                      2*m_smallLen, 2*m_smallLen
                      );
  }
  for (int p = 0; p < (int)m_nonSnappedPoints.size(); p++){
    const QPoint & P = m_nonSnappedPoints[p];
    paint.drawRect(P.x() - m_smallLen, P.y() - m_smallLen,
                   2*m_smallLen, 2*m_smallLen
                   );
  }

  // Draw the current polygon being plotted
  drawPolyLine(m_currPolyX, m_currPolyY, &paint);
  
  return;
}

void polyView::resizeEvent(QResizeEvent*){
  refreshPixmap();
  return;
}

void polyView::popUp(std::string msg){
  QMessageBox msgBox;
  msgBox.setText(msg.c_str());
  msgBox.exec();
  return;
}

bool polyView::getStringFromGui(std::string title, std::string description,
                                std::string & data // output
                                ){

  data = "";

  bool ok = false;
  QString text = QInputDialog::getText(title.c_str(), description.c_str(),
                                       QLineEdit::Normal, QString::null, &ok, this );

  if (ok) data = text.toStdString();

  return ok;
}

bool polyView::getRealValuesFromGui(std::string title, std::string description,
                                    std::vector<double> & values){

  values.clear();
  string data;
  bool ok = getStringFromGui(title, description,
                             data // output
                             );
  
  data = replaceAll(data, ",", " ");
  istringstream ts(data);
  double val;
  while (ts >> val) values.push_back(val);

  return ok;
}

void polyView::setLineWidth(){

  vector<double> linewidth;
  if ( getRealValuesFromGui("Line width", "Enter line width", linewidth) &&
       !linewidth.empty() && linewidth[0] >= 1.0 ){

    int lw = (int) round(linewidth[0]);

    for (int polyIter = 0; polyIter < (int)m_polyOptionsVec.size(); polyIter++){
      m_polyOptionsVec[polyIter].lineWidth = lw;
    }
    m_prefs.lineWidth = lw;
    
    refreshPixmap();
  
  }else{
    popUp("The line width must be a positive integer");
  }
  return;
}

void polyView::setBgColor(){

  string data = "";
  bool ok = getStringFromGui("Background", "Enter background",
                             data // output
                             );

  istringstream in(data);
  string bgColor = "";
  if (ok && in >> bgColor && QColor(bgColor.c_str()) != QColor::Invalid){
    m_prefs.bgColor = bgColor;
    setBgFgColorsFromPrefs();
    refreshPixmap();
  }else{
    popUp("Invalid background color");
  }
  return;
}

void polyView::setBgFgColorsFromPrefs(){

  // Set the background. Watch for invalid colors.
  string bgColor   = m_prefs.bgColor;
  QColor qtBgColor = QColor(bgColor.c_str());
  if (qtBgColor == QColor::Invalid){
    bgColor   = "black";
    qtBgColor = QColor(bgColor.c_str()); // fallback color
  }
  setBackgroundColor(qtBgColor);

  string fgColor = m_prefs.fgColor;
  if ( QColor(fgColor.c_str()) == QColor::Invalid ){
    fgColor = "white";
  }

  // Make sure bg and fg have different colors
  if (bgColor == fgColor){
    if (bgColor == "black") fgColor = "white";
    else                    fgColor = "black";
  }

  // Update the preferences after doing the logic above
  m_prefs.bgColor = bgColor;
  m_prefs.fgColor = fgColor;

  // While unnecessary, also update the preferences for each
  // polygon file, for consistency with other preferences per file.
  for (int s = 0; s < (int)m_polyOptionsVec.size(); s++){
    m_polyOptionsVec[s].bgColor = bgColor; 
    m_polyOptionsVec[s].fgColor = fgColor; 
  }

  return;
}


void polyView::shiftPolys(){

  vector<double> shifts;
  if ( getRealValuesFromGui("Translate polygons", "Enter shift_x and shift_y", shifts) ){
    shiftPolys(shifts);
  }
  return;
}

void polyView::rotatePolys(){

  vector<double> angle;
  if ( getRealValuesFromGui("Rotate polygons", "Enter rotation angle in degrees", angle) ){
    rotatePolys(angle);
  }
  return;
}

void polyView::scalePolys(){

  vector<double> scale;
  if ( getRealValuesFromGui("Scale polygons", "Enter scale factor", scale) ){
    scalePolys(scale);
  }
  return;
}

void polyView::shiftPolys(std::vector<double> & shifts){

  if (shifts.size() < 2){
    popUp("Invalid shift_x and shift_y values");
    return;
  }
  shifts.resize(2);
  
  backupPolysForUndo(true);
  
  printCmd("shift", shifts);
  for (int vi  = 0; vi < (int)m_polyVec.size(); vi++){
    m_polyVec[vi].shift(shifts[0], shifts[1]);
  }
  resetView();
  
  return;
}

void polyView::rotatePolys(std::vector<double> & angle){

  if (angle.size() < 1){
    popUp("Invalid rotation angle");
    return;
  }
  angle.resize(1);
  
  backupPolysForUndo(true);

  printCmd("rotate", angle);
  for (int vi  = 0; vi < (int)m_polyVec.size(); vi++){
    m_polyVec[vi].rotate(angle[0]);
  }
  resetView();
  
  return;
}

void polyView::scalePolys(std::vector<double> & scale){

  if (scale.size() < 1){
    popUp("Invalid scale factor");
    return;
  }
  scale.resize(1);
  
  backupPolysForUndo(true);

  printCmd("scale", scale);
  for (int vi  = 0; vi < (int)m_polyVec.size(); vi++){
    m_polyVec[vi].scale(scale[0]);
  }
  resetView();
  
  return;
}

void polyView::transformPolys(std::vector<double> & M){

  if (M.size() < 6){
    popUp("Invalid linear transform");
    return;
  }

  bool resetViewOnUndo = !m_alignMode;
  
  backupPolysForUndo(resetViewOnUndo);

  int end = m_polyVec.size();
  if (m_alignMode) end = min(end, 1);
    
  for (int vi = 0; vi < end; vi++){
    m_polyVec[vi].applyTransform(M[0], M[1], M[2], M[3], M[4], M[5], m_T);
  }

  if (m_alignMode) m_totalT = composeTransforms(m_T, m_totalT);
  
  m_T.print();

  if (resetViewOnUndo) resetView();
  else                 refreshPixmap();
  
  return;
}

void polyView::addPolyVert(int px, int py){

  // Add a point to the polygon being drawn or stop drawing and append
  // the drawn polygon to the list of polygons.

  double wx, wy;
  pixelToWorldCoords(px, py, wx, wy);

  double wtol = pixelToWorldDist(m_pixelTol);
  int pSize   = m_currPolyX.size();
  
  if (pSize <= 0 ||
      distance(m_currPolyX[0], m_currPolyY[0], wx, wy) > wtol
      ){

    // We did not arrive at the starting point of the polygon being
    // drawn. Add the current point.
    
    m_currPolyX.push_back(wx);
    m_currPolyY.push_back(wy);
    pSize = m_currPolyX.size();
    if (m_snapPolyTo45DegreeIntGrid){
      bool isClosedPolyLine = false;
      snapPolyLineTo45DegAngles(isClosedPolyLine, pSize,
                                vecPtr(m_currPolyX), vecPtr(m_currPolyY));
    }

    // This will call paintEvent which will draw the current poly line
    update();

    return;
  }

  // We arrived at the starting point of the polygon being drawn. Stop
  // adding points and append the current polygon.
  
  if (m_snapPolyTo45DegreeIntGrid){
    bool isClosedPolyLine = true;
    snapPolyLineTo45DegAngles(isClosedPolyLine, pSize,
                              vecPtr(m_currPolyX), vecPtr(m_currPolyY));
  }
  
  // Get the layer and color from the closest existing polygon
  double minX = DBL_MAX, minY = DBL_MAX, minDist = DBL_MAX;
  int minVecIndex, minPolyIndex, minVertIndex;
  findClosestPolyEdge(// inputs
                      m_currPolyX[0], m_currPolyY[0],
                      m_polyVec,  
                      // outputs
                      minVecIndex, minPolyIndex, minVertIndex, 
                      minX, minY, minDist
                      );
  string color, layer;
  if (minVecIndex >= 0 && minPolyIndex >= 0){
    const vector<string> & layers = m_polyVec[minVecIndex].get_layers();
    const vector<string> & colors = m_polyVec[minVecIndex].get_colors();
    color = colors.at(minPolyIndex);
    layer = layers.at(minPolyIndex);
  }else{
    // No other polygons to borrow layer and color info from. Just use
    // some defaults then.
    color = m_prefs.cmdLineColor;
    layer = "";
  }

  // Form the new polygon
  dPoly P;
  bool isPolyClosed = (m_prefs.isPolyClosed != forceNonClosedPoly);
  P.reset();
  P.appendPolygon(pSize, vecPtr(m_currPolyX), vecPtr(m_currPolyY),
                  isPolyClosed, color, layer);

  appendToPolyVec(P);

  // Reset
  m_createPoly = false;
  m_currPolyX.clear();
  m_currPolyY.clear();
  setStandardCursor();
  refreshPixmap();
    
  return;
}

void polyView::appendToPolyVec(const dPoly & P){

  backupPolysForUndo(false);

  // Append the new polygon to the list of polygons. If we have several
  // clips already, append it to the last clip. If we have no clips,
  // create a new clip.
  if (m_polyVec.size() == 0){
    m_polyVec.push_back(P);
    m_polyOptionsVec.push_back(m_prefs);
    string fileName = "poly" + num2str(m_polyVec.size() - 1) + ".xg";
    m_polyOptionsVec.back().polyFileName = fileName;
  }else{
    m_polyVec.back().appendPolygons(P);
  }

  return;
}

void polyView::drawPolyLine(const std::vector<double> & polyX,
                            const std::vector<double> & polyY,
                            QPainter * paint){

  dPoly  polyLine;
  bool   isPolyClosed = false;
  string color        = m_prefs.fgColor.c_str();
  string layer        = "";
  polyLine.setPolygon(polyX.size(), vecPtr(polyX), vecPtr(polyY),
                      isPolyClosed, color, layer
                      );

  bool plotPoints = false, plotEdges = true, plotFilled = false;
  int drawVertIndex = 0;
  vector< vector<int> > textOnScreenGrid;
  textOnScreenGrid.clear();
  bool showAnno = false;
  plotDPoly(plotPoints, plotEdges, plotFilled, showAnno, m_prefs.lineWidth,  
            drawVertIndex,    // 0 is a good choice here
            textOnScreenGrid, // empty grid is fine here  
            paint,  
            polyLine
            );
  
  return;
}

void polyView::createHighlightWithPixelInputs(int pxll, int pyll, int pxur, int pyur
                                              ){
  
  double xll, yll, xur, yur;
  pixelToWorldCoords(pxll, pyll, // inputs
                     xll, yll    // outputs
                     );
  pixelToWorldCoords(pxur, pyur, // inputs
                     xur, yur    // outputs
                     );

  createHighlightWithRealInputs(xll, yll, xur, yur);

  return;
}

void polyView::createHighlightWithRealInputs(double xll, double yll,
                                             double xur, double yur
                                             ){
  
  dPoly R;
  bool isPolyClosed = true;
  string color = m_prefs.fgColor, layer = "";
  R.setRectangle(min(xll, xur), min(yll, yur), max(xll, xur), max(yll, yur),
                 isPolyClosed, color, layer);
  m_highlights.push_back(R);
  m_actions.push_back(m_createHlt);
  
  return;
}

void polyView::printCurrCoords(const Qt::ButtonState & state, // input
                               int & currX, int  & currY      // in-out
                               ){
  
  // Snap or not the current point to the closest polygon vertex
  // and print its coordinates.

  double s;
  string unit;
  if (m_useNmScale){
    s    = m_nmScale;
    unit = " (nm):";
  }else{
    s    = 1.0;
    unit = ":     ";
  }
  
  int prec = 6, wid = prec + 6;
  cout.precision(prec);
  cout.setf(ios::floatfield);
        
  double wx, wy;
  pixelToWorldCoords(currX, currY, wx, wy);
  int len = 2*m_smallLen + 2*m_prefs.lineWidth; // big enough

  if (state == (int)Qt::LeftButton){
      
    // Snap to the closest vertex with the left mouse button.
    
    double min_x, min_y, min_dist;
    int polyVecIndex, polyIndexInCurrPoly, vertIndexInCurrPoly;
    findClosestPolyVertex(// inputs
                          wx, wy, m_polyVec,
                          // outputs
                          polyVecIndex,
                          polyIndexInCurrPoly,
                          vertIndexInCurrPoly,
                          min_x, min_y, min_dist
                          );
    wx = min_x; wy = min_y;
    worldToPixelCoords(wx, wy,      // inputs
                       currX, currY // outputs
                       );
    
    // Save the point to plot. Call update to paint the point.
    m_snappedPoints.push_back(QPoint(currX, currY));
    update(currX - len, currY - len, 2*len, 2*len);

  }else if (state == ( (int)Qt::LeftButton | (int)Qt::ShiftModifier )
            ||
            state == ((int)Qt::MidButton)
            ){
      
    // Don't snap with the shift-left button or the middle button.
    
    // Save the point to plot. Call update to paint the point.
    m_nonSnappedPoints.push_back(QPoint(currX, currY));
    int len = 2*m_smallLen + 2*m_prefs.lineWidth; // big enough
    update(currX - len, currY - len, 2*len, 2*len);
      
  }
  
  cout << "Point" << unit << " ("
       << setw(wid) << s*wx << ", "
       << setw(wid) << s*wy << ")";
  if (m_prevClickExists){
    cout  << " dist from prev: ("
          << setw(wid) << s*(wx - m_prevClickedX) << ", "
          << setw(wid) << s*(wy - m_prevClickedY)
          << ") Euclidean: "
          << setw(wid) << s*sqrt( (wx - m_prevClickedX)*(wx - m_prevClickedX)
                                  + 
                                  (wy - m_prevClickedY)*(wy - m_prevClickedY)
                                  );
  }
  cout << endl;
  
  m_prevClickExists = true;
  m_prevClickedX    = wx;
  m_prevClickedY    = wy;

  return;
}


void polyView::updateRubberBand(QRect & R){
  
  QRect rect = R.normalized();
  update(rect.left(), rect.top(),    rect.width(), 1             );
  update(rect.left(), rect.top(),    1,            rect.height() );
  update(rect.left(), rect.bottom(), rect.width(), 1             );
  update(rect.right(), rect.top(),   1,            rect.height() );
  
  return;
}

void polyView::pixelToWorldCoords(int px, int py,
                                  double & wx, double & wy){

  // Compensate for the Qt's origin being in the upper-left corner
  // instead of the lower-left corner.
  py = m_screenWidY - py;

  wx = px*m_pixelSize + m_viewXll;
  wy = py*m_pixelSize + m_viewYll;

}

void polyView::worldToPixelCoords(double wx, double wy,
                                  int & px,  int & py){

  px = iround((wx - m_viewXll)/m_pixelSize);
  py = iround((wy - m_viewYll)/m_pixelSize);
  
  // Compensate for the Qt's origin being in the upper-left corner
  // instead of the lower-left corner.
  py = m_screenWidY - py;
  
}

void polyView::drawOneVertex(int x0, int y0, QColor color, int lineWidth,
                             int drawVertIndex, QPainter * paint){

  // Draw a vertex as a small shape (a circle, rectangle, triangle)
  
  // Use variable size shapes to distinguish better points on top of
  // each other
  int len = 3*(drawVertIndex+1); 

  paint->setPen( QPen(color, lineWidth) );

  int numTypes = 4;
  if (drawVertIndex < 0){
    
    // This will be reached only for the case when a polygon
    // is so small that it collapses into a point.
    len = lineWidth;
    paint->setBrush( color );
    paint->drawRect(x0 - len, y0 - len, 2*len, 2*len);
    
  } else if (drawVertIndex%numTypes == 0){
    
    // Draw a small empty ellipse
    paint->setBrush( Qt::NoBrush );
    paint->drawEllipse(x0 - len, y0 - len, 2*len, 2*len);
    
  }else if (drawVertIndex%numTypes == 1){
    
    // Draw an empty square
    paint->setBrush( Qt::NoBrush );
    paint->drawRect(x0 - len, y0 - len, 2*len, 2*len);
    
  }else if (drawVertIndex%numTypes == 2){
    
    // Draw an empty triangle
    paint->setBrush( Qt::NoBrush );
    paint->drawLine(x0 - len, y0 - len, x0 + len, y0 - len);
    paint->drawLine(x0 - len, y0 - len, x0 + 0,   y0 + len);
    paint->drawLine(x0 + len, y0 - len, x0 + 0,   y0 + len);
    
  }else{
    
    // Draw an empty reversed triangle
    paint->setBrush( Qt::NoBrush );
    paint->drawLine(x0 - len, y0 + len, x0 + len, y0 + len);
    paint->drawLine(x0 - len, y0 + len, x0 + 0,   y0 - len);
    paint->drawLine(x0 + len, y0 + len, x0 + 0,   y0 - len);
    
  }
  
  return;
}

void polyView::drawMark(int x0, int y0, QColor color, int lineWidth,
                        QPainter * paint){
  
  int len = 6;

  paint->setBrush( Qt::NoBrush );
  paint->setPen( QPen(color, lineWidth) );

  // Draw a cross
  paint->drawLine(x0 - len, y0 - len, x0 + len, y0 + len);
  paint->drawLine(x0 - len, y0 + len, x0 + len, y0 - len);
  
}

void polyView::toggleAnno(){
  m_showAnnotations   = !m_showAnnotations;
  m_showVertIndexAnno = false;
  m_showLayerAnno     = false;
  refreshPixmap();
}

void polyView::toggleVertIndexAnno(){
  m_showVertIndexAnno = !m_showVertIndexAnno;
  m_showAnnotations   = false;
  m_showLayerAnno     = false;
  refreshPixmap();
}

void polyView::toggleLayerAnno(){
  m_showLayerAnno     = !m_showLayerAnno;
  m_showAnnotations   = false;
  m_showVertIndexAnno = false;
  refreshPixmap();
}

void polyView::toggleFilled(){
  m_showFilledPolys = !m_showFilledPolys;
  refreshPixmap();
}

void polyView::toggleShowPolyDiff(){

  // Show the differences of two polygons as points
  
  printCmd("poly_diff");

  if (m_polyDiffMode){
    // Turn off diff mode
    m_polyDiffMode      = false;
    m_polyVec           = m_polyVecBk;
    m_polyOptionsVec    = m_polyOptionsVecBk;

    // See polyView::plotDiff() for explanation.
    m_distVec.clear();
    m_indexOfDistToPlot = -1; 

    refreshPixmap();
    return;
  }

  // Turn on diff mode
  
  assert( m_polyVec.size() == m_polyOptionsVec.size() );
  
  if (m_polyVec.size() < 2){
    popUp("Must have two polygon files to diff");
    return;
  }

  if (m_polyVec.size() > 2){
    cout << "Showing the differences of the first two polygon files "
         << "and ignoring the rest" << endl;
  }

  m_polyDiffMode = true;

  // Back up the current settings before entering poly diff mode
  // to be able to restore them later.
  m_polyVecBk        = m_polyVec;
  m_polyOptionsVecBk = m_polyOptionsVec;

  m_polyVec.resize(4);
  m_polyOptionsVec.resize(4);

  string color1 = "red", color2 = "blue", layer1 = "", layer2 = "";
  
  dPoly & P = m_polyVec[0]; // alias
  dPoly & Q = m_polyVec[1]; // alias
  vector<dPoint>  vP, vQ;

  findPolyDiff(P, Q,  // inputs
               vP, vQ // outputs
               );

  cout << "Changing the polygons colors to " << color1 << " and "
       << color2 << " in show poly diff mode"<< endl;
  
  P.set_color(color1);
  Q.set_color(color2);
  
  m_polyVec[2].set_pointCloud(vP, color1, layer1);
  m_polyVec[3].set_pointCloud(vQ, color2, layer2);

  m_polyOptionsVec[2].plotAsPoints = true;
  m_polyOptionsVec[3].plotAsPoints = true;

  m_polyOptionsVec[2].polyFileName = "diff1.xg";
  m_polyOptionsVec[3].polyFileName = "diff2.xg";
  
  refreshPixmap();
}

void polyView::plotNextDiff(){
  plotDiff(1);
}

void polyView::plotPrevDiff(){
  plotDiff(-1);
}

void polyView::plotDiff(int direction){

  // For every vertex in m_polyVec[0], find the distance to the
  // closest point on the closest edge of m_polyVec[1], and the
  // segment achieving this shortest distance. Do the same with the
  // polygons reversed. We sort all these distances in decreasing
  // order and store them in m_distVec. The user will navigate over
  // these segments to see how different the polygon clips are.

  if ( !m_polyDiffMode ) return;

  // The current segment to plot
  m_segX.clear(); m_segY.clear();

  assert(direction == 1 || direction == -1);
  
  if (m_distVec.size() == 0 ){
    assert( m_polyVec.size() >= 2);
    findDistanceBwPolys(// inputs
                        m_polyVec[0], m_polyVec[1], 
                        // outputs
                        m_distVec
                        );
  }

  if (m_indexOfDistToPlot < 0){
    if (direction > 0) m_indexOfDistToPlot = -1;
    else               m_indexOfDistToPlot = 0;
  }
  m_indexOfDistToPlot += direction;

  int len = m_distVec.size();
  if (len > 0){
    m_indexOfDistToPlot = (m_indexOfDistToPlot + len) % len;
  }else{
    m_indexOfDistToPlot = -1; // Nothing to plot
  }
    
  if (m_indexOfDistToPlot < 0 || m_indexOfDistToPlot >= len) return;

  // The segment to plot
  segDist S = m_distVec[m_indexOfDistToPlot];
  m_segX.push_back(S.begx); m_segX.push_back(S.endx);
  m_segY.push_back(S.begy); m_segY.push_back(S.endy);

  cout << "Distance: " << S.dist << endl;

  // Set up the view so that it is centered at the midpoint of this
  // segment.
  double midX = (m_segX[0] + m_segX[1])/2.0;
  double midY = (m_segY[0] + m_segY[1])/2.0;
  m_viewXll  = midX - m_viewWidX/2.0;
  m_viewYll  = midY - m_viewWidY/2.0;
  
  refreshPixmap(); // Will call plotDistBwPolyClips(...)
}


void polyView::plotDistBwPolyClips( QPainter *paint ){

  // This is to be called in poly diff mode only. Plot the current
  // segment/distance between clips of polygons. See
  // polyView::plotDiff() for more info.
  
  if ( !m_polyDiffMode ) return;
  drawPolyLine(m_segX, m_segY, paint);
  return;
}

void polyView::toggleEditMode(){
  
  m_editMode = !m_editMode;

  if (m_editMode){
    m_alignMode               = false;
    m_moveVertices            = true;
    m_moveEdges               = false; 
    m_movePolys               = false;
    m_toggleShowPointsEdgesBk = m_toggleShowPointsEdges;
    m_toggleShowPointsEdges   = m_showPointsEdges;
  }else{
    m_moveVertices            = false;
    m_moveEdges               = false; 
    m_movePolys               = false;
    m_toggleShowPointsEdges   = m_toggleShowPointsEdgesBk;
  }
  
  refreshPixmap();
  return;
}

void polyView::toggleAlignMode(){
  
  m_alignMode = !m_alignMode;

  if (m_alignMode){
    
    if (m_polyVec.size() < 2){
      popUp("Must have two polygon files to align");
      m_alignMode = false;
      return;
    }

    m_editMode = false;
    m_totalT.reset();
  }else{
    cout << "\nCombined transform:" << endl;
    m_totalT.print();
    cout << endl;
  }
  
  refreshPixmap();
  return;
}

void polyView::align_rotate90(){
  assert(m_alignMode);
  backupPolysForUndo(false);
  m_polyVec[0].applyTransformAroundBdBoxCenter(0, -1, 1, 0, m_T);
  m_T.print();
  m_totalT = composeTransforms(m_T, m_totalT);
  refreshPixmap();
}

void polyView::align_rotate180(){
  assert(m_alignMode);
  backupPolysForUndo(false);
  m_polyVec[0].applyTransformAroundBdBoxCenter(-1, 0, 0, -1, m_T);
  m_T.print();
  m_totalT = composeTransforms(m_T, m_totalT);
  refreshPixmap();
}

void polyView::align_rotate270(){
  assert(m_alignMode);
  backupPolysForUndo(false);
  m_polyVec[0].applyTransformAroundBdBoxCenter(0, 1, -1, 0, m_T);
  m_T.print();
  m_totalT = composeTransforms(m_T, m_totalT);
  refreshPixmap();
}

void polyView::align_flip_against_y_axis(){
  assert(m_alignMode);
  backupPolysForUndo(false);
  m_polyVec[0].applyTransformAroundBdBoxCenter(-1, 0, 0, 1, m_T);
  m_T.print();
  m_totalT = composeTransforms(m_T, m_totalT);
  refreshPixmap();
}

void polyView::align_flip_against_x_axis(){
  assert(m_alignMode);
  backupPolysForUndo(false);
  m_polyVec[0].applyTransformAroundBdBoxCenter(1, 0, 0, -1, m_T);
  m_T.print();
  m_totalT = composeTransforms(m_T, m_totalT);
  refreshPixmap();
}

void polyView::performAlignmentOfClosePolys(){
  assert(m_alignMode);
  backupPolysForUndo(false);
  assert(m_polyVec.size() >= 2);
  utils::alignPoly1ToPoly2(m_polyVec[0], m_polyVec[1], m_T);
  m_T.print();
  m_totalT = composeTransforms(m_T, m_totalT);
  refreshPixmap();
}

void polyView::turnOnMoveVertices(){
  if (!m_editMode) return;
  m_moveVertices = true;
  m_moveEdges    = false;
  m_movePolys    = false;
}

void polyView::turnOnMoveEdges(){
  if (!m_editMode) return;
  m_moveVertices = false;
  m_moveEdges    = true;
  m_movePolys    = false;
}

void polyView::turnOnMovePolys(){
  if (!m_editMode) return;
  m_moveVertices = false;
  m_moveEdges    = false;
  m_movePolys    = true;
}

void polyView::create45DegreeIntPoly(){

  // This flag will change the behavior of mouseReleaseEvent() so that
  // we can start adding points to the polygon with the mouse.
  m_createPoly                = true;

  m_snapPolyTo45DegreeIntGrid = true;
  
  setPolyDrawCursor();
}

void polyView::createArbitraryPoly(){

  // This flag will change the behavior of mouseReleaseEvent() so that
  // we can start adding points to the polygon with the mouse.
  m_createPoly                = true;
  
  m_snapPolyTo45DegreeIntGrid = false;
  
  setPolyDrawCursor();
}

void polyView::insertVertex(){

  if (!m_editMode) return;

  if (m_polyVec.size() == 0) return;
  
  double min_x, min_y, min_dist;
  findClosestPolyEdge(// inputs
                      m_menuX, m_menuY, m_polyVec,  
                      // outputs
                      m_polyVecIndex,
                      m_polyIndexInCurrPoly,
                      m_vertIndexInCurrPoly,
                      min_x, min_y, min_dist
                      );
  
  if (m_polyVecIndex        < 0 ||
      m_polyIndexInCurrPoly < 0 ||
      m_vertIndexInCurrPoly < 0) return;
  
  backupPolysForUndo(false);

  // Need +1 below as we insert AFTER current vertex.
  m_polyVec[m_polyVecIndex].insertVertex(m_polyIndexInCurrPoly,
                                         m_vertIndexInCurrPoly + 1,
                                         min_x, min_y
                                         );
  
  refreshPixmap();
  return;
}

void polyView::deleteVertex(){

  if (!m_editMode) return;

  if (m_polyVec.size() == 0) return;
  
  double min_x, min_y, min_dist;
  findClosestPolyVertex(// inputs
                        m_menuX, m_menuY, m_polyVec,
                        // outputs
                        m_polyVecIndex,
                        m_polyIndexInCurrPoly,
                        m_vertIndexInCurrPoly,
                        min_x, min_y, min_dist
                        );
  
  if (m_polyVecIndex        < 0 ||
      m_polyIndexInCurrPoly < 0 ||
      m_vertIndexInCurrPoly < 0) return;
  
  backupPolysForUndo(false);

  m_polyVec[m_polyVecIndex].eraseVertex(m_polyIndexInCurrPoly,
                                        m_vertIndexInCurrPoly
                                        );
  
  refreshPixmap();
  return;
}

void polyView::deletePoly(){

  if (!m_editMode) return;
    
  if (m_polyVec.size() == 0) return;
  
  backupPolysForUndo(false);

  int minVecIndex, minPolyIndex, minVertIndex;
  double minX = DBL_MAX, minY = DBL_MAX, minDist = DBL_MAX;
  findClosestPolyEdge(// inputs
                      m_menuX, m_menuY,
                      m_polyVec,  
                      // outputs
                      minVecIndex, minPolyIndex, minVertIndex, 
                      minX, minY, minDist
                      );
  
  if (minVecIndex >= 0 && minPolyIndex >= 0){
    m_polyVec[minVecIndex].erasePoly(minPolyIndex);
  }

  refreshPixmap();
  
  return;
}

void polyView::saveMark(){

  // When saving the mark don't overwrite existing marks
  int markIndex = 0;
  string markFile;
  while(1){
    markIndex++;
    markFile = "mark" + num2str(markIndex) + ".xg";
    ifstream mark(markFile.c_str());
    if (!mark) break;
  }
  
  cout << "Saving the mark to " << markFile << endl;
  ofstream mark(markFile.c_str());
  mark << "color = white" << endl;
  mark << m_menuX << ' ' << m_menuY << endl;
  mark << "NEXT" << endl;
  mark.close();

  // Plot the mark
  m_markX.resize(1); m_markX[0] = m_menuX;
  m_markY.resize(1); m_markY[0] = m_menuY;
  refreshPixmap();
}

void polyView::toggleNmScale(){

  m_useNmScale = !m_useNmScale;
  if (!m_useNmScale){
    cout << "Using the dbu scale" << endl;
    return;
  }
  
  ifstream scaleHandle(m_nmScaleFile.c_str());
  if (!scaleHandle){
    cerr << "File " << m_nmScaleFile << " does not exist" << endl;
    m_useNmScale = false;
    return;
  }

  string dummy;
  if (! (scaleHandle >> dummy >> m_nmScale) ){
    cerr << "Could not read the nm scale factor from "
         << m_nmScaleFile << endl;
    cerr << "Syntax is: scale 1.5" << endl;
    m_useNmScale = false;
    return;
  }

  if (m_nmScale <= 0.0){
    cerr << "The nm scale factor must be greater than 0" << endl;
    m_useNmScale = false;
    return;
  }
  
  cout << "Using the nm scale factor " << m_nmScale << endl;

  return;
}

void polyView::cutToHlt(){
  
  // Cut to the last highlight
  int numH = m_highlights.size();
  if ( numH == 0){
    cout << "No highlights" << endl;
    return;
  }
  
  backupPolysForUndo(false);

  dPoly H = m_highlights[numH - 1];
  assert(H.get_totalNumVerts() == 4);
  double xl, yl, xh, yh;
  H.bdBox(xl, yl, xh, yh);
  printCmd( "clip", xl, yl, xh - xl, yh - yl );
    
  dPoly clippedPoly;
  for (int vecIter = 0; vecIter < (int)m_polyVec.size(); vecIter++){

    m_polyVec[vecIter].clipPoly(xl, yl, xh, yh, //inputs
                                clippedPoly     // output
                                );

    m_polyVec[vecIter] = clippedPoly;    
  }

  m_highlights.resize(numH - 1);
  
  refreshPixmap();
  return;
}

void polyView::enforce45(){
  
  // Enforce that polygon vertices are integers and the angles are 45x. 

  printCmd("enforce45");
    
  backupPolysForUndo(false);

  for (int vecIter = 0; vecIter < (int)m_polyVec.size(); vecIter++){
    m_polyVec[vecIter].enforce45();
  }

  refreshPixmap();
  return;
}

void polyView::backupPolysForUndo(bool resetViewOnUndo){
  // So that we can undo later
  m_polyVecStack.push_back(m_polyVec);
  m_polyOptionsVecStack.push_back(m_polyOptionsVec); 
  m_actions.push_back(m_polyChanged);
  m_resetViewOnUndo.push_back(resetViewOnUndo);
}

void polyView::undoLast(){

  int numActions = m_actions.size();
  if (numActions == 0){
    cout << "No actions to undo" << endl;
    return;
  }

  int lastAction = m_actions[numActions - 1];
  m_actions.resize(numActions - 1);

  if (lastAction == m_createHlt){

    int numH = m_highlights.size();
    if (numH == 0){
      cout << "No highlights to undo" << endl;
      return;
    }
    m_highlights.resize(numH - 1);
    refreshPixmap();
    
  }else if (lastAction == m_polyChanged){
  
    int numCopies = m_polyVecStack.size();
    if (numCopies == 0){
      cout << "No poly operations to undo" << endl;
      return;
    }

    m_polyVec        = m_polyVecStack.back();        m_polyVecStack.pop_back();
    m_polyOptionsVec = m_polyOptionsVecStack.back(); m_polyOptionsVecStack.pop_back();

    bool resetViewOnUndo = m_resetViewOnUndo[numCopies - 1];
    m_resetViewOnUndo.resize(numCopies - 1);
    if (resetViewOnUndo) resetView();

    refreshPixmap();
  }

  return;
}

void polyView::readAllPolys(){

  int numFiles = m_polyOptionsVec.size();
  m_polyVec.resize(numFiles);

  string missingFiles = "";
  int numMissing = 0;
  
  for (int fileIter = 0; fileIter < numFiles; fileIter++){
    
    bool success = readOnePoly(// inputs
                               m_polyOptionsVec[fileIter].polyFileName,
                               m_polyOptionsVec[fileIter].plotAsPoints,
                               m_polyOptionsVec[fileIter].isPolyClosed,
                               // output
                               m_polyVec[fileIter]
                               );
    if (!success){
      missingFiles += " " + m_polyOptionsVec[fileIter].polyFileName;
      numMissing++;
    }
    
    if (m_polyOptionsVec[fileIter].useCmdLineColor){
      m_polyVec[fileIter].set_color(m_polyOptionsVec[fileIter].cmdLineColor);
    }
    
  }


  if (numMissing >= 1){
    string suffix = ""; if (numMissing > 1) suffix = "s";
    popUp("Warning: Could not read file" + suffix + ":" + missingFiles);
  }
  
  return;
}

void polyView::chooseFilesToShow(){
  m_chooseFilesDlg.chooseFiles(m_polyOptionsVec, // Input
                               m_filesNotToShow  // In-out
                               );
  // User's choice is processed in showFilesChosenByUser.
  return;
}

void polyView::showFilesChosenByUser(){
  
  // Process user's choice from chooseFilesToShow().

  QTableWidget * filesTable = m_chooseFilesDlg.getFilesTable();
  int numFiles = filesTable->rowCount();
  int numCols  = filesTable->columnCount();
  
  for (int fileIter = 0; fileIter < numFiles; fileIter++){

    QTableWidgetItem *item = filesTable->item(fileIter, numCols - 1);
    bool isChecked  = (item->checkState() != Qt::Unchecked);
    string fileName = ((item->data(0)).toString()).toStdString();
    
    if (!isChecked) m_filesNotToShow.insert(fileName);
    else            m_filesNotToShow.erase (fileName);
    
  }

  refreshPixmap();
  
  return;
}

void polyView::openPoly(){

  QString s = QFileDialog::getOpenFileName(QDir::currentDirPath(),
                                           "(*.xg *.ly* *.pol)",
                                           this,
                                           "open file dialog"
                                           "Choose a file"
                                           );

  if (s.length() == 0) return;
  
  string fileName = s.toStdString();
  
  assert ( (int)m_polyVec.size() == (int)m_polyOptionsVec.size() );

  m_polyOptionsVec.push_back(m_prefs);
  m_polyOptionsVec.back().polyFileName = fileName;
  
  dPoly poly;
  bool success = readOnePoly(// inputs
                             m_polyOptionsVec.back().polyFileName,
                             m_polyOptionsVec.back().plotAsPoints,
                             m_polyOptionsVec.back().isPolyClosed,
                             // output
                             poly
                             );

  if (!success){
    popUp("Warning: Could not read file: " + m_polyOptionsVec.back().polyFileName);
  }

  if (m_polyOptionsVec.back().useCmdLineColor){
    poly.set_color(m_polyOptionsVec.back().cmdLineColor);
  }
  m_polyVec.push_back(poly);

  resetView();
  
  return;
}

bool polyView::readOnePoly(// inputs
                           std::string   & filename,
                           bool            plotPointsOnly,
                           closedPolyInfo  isPolyClosed,
                           // output
                           dPoly & poly           
                           ){

  poly.reset();
  
  string type = getFilenameExtension(filename);
  
  if (type == "pol" || type == "cnt"){
     if ( poly.read_pol_or_cnt_format(filename, type, plotPointsOnly) ) return true;
     string msg = string("Invalid .") + type + " format for " + filename
       + ". Trying to read it in .xg format.";
     cerr << msg << endl;
  }
  
  if ( ! poly.readPoly(filename, plotPointsOnly) ){
    return false;
  }

  bool isClosed;
  if (isPolyClosed == forceClosedPoly){
    isClosed = true;
    poly.set_isPolyClosed(isClosed);
  }else if (isPolyClosed == forceNonClosedPoly){
    isClosed = false;
    poly.set_isPolyClosed(isClosed);
  } // else use the isClosed info from file
  
  return true;
}

  
void polyView::saveOnePoly(){

  string fileName = "out_poly.xg";

  dPoly poly;

  for (int polyIter = 0; polyIter < (int)m_polyVec.size(); polyIter++){
    poly.appendPolygons(m_polyVec[polyIter]); 
  }

  poly.writePoly(fileName);
  cout << "Polygon saved to " << fileName << endl;

  return;
}

void polyView::overwriteMultiplePolys(){
  bool overwrite = true;
  saveMultiplePoly(overwrite);
}

void polyView::saveAsMultiplePolys(){
  bool overwrite = false;
  saveMultiplePoly(overwrite);
}

void polyView::saveMultiplePoly(bool overwrite){

  string allFiles = "";
  for (int polyIter = 0; polyIter < (int)m_polyVec.size(); polyIter++){

    dPoly poly = m_polyVec[polyIter];
    
    string fileName = m_polyOptionsVec[polyIter].polyFileName;
    if (!overwrite) fileName = inFileToOutFile(fileName);

    poly.writePoly(fileName.c_str());
    allFiles += " " + fileName;
  }

  if ((int)m_polyVec.size() > 0){
    cout << "Polygons saved to" << allFiles << endl;
  }

  return;
}

void polyView::togglePE(){

  m_toggleShowPointsEdges = m_toggleShowPointsEdges%3 + 1;
  refreshPixmap();
  
}

void polyView::changeOrder(){

  m_changeDisplayOrder = true;
  refreshPixmap();
  
}

bool polyView::isPolyZeroDim(const Q3PointArray & pa){

  int numPts = pa.size();
  for (int s = 1; s < numPts; s++){
    if (pa[0] != pa[s]) return false;
  }
  
  return true;
}

void polyView::initTextOnScreenGrid(std::vector< std::vector<int> > & Grid){

  // Split the screen into numGridPts x numGridPts rectangles.  For
  // performance reasons, will not allow more than one string of text
  // do be displayed in one rectangle. As such, if a polygon has a lot
  // of text (annotations), the more you zoom in the more of the
  // annotations you will see.
  int numGridPts = 20; 
  
  Grid.resize(numGridPts);
  for (int s = 0; s < (int)Grid.size(); s++){
    Grid[s].resize(numGridPts);
    for (int t = 0; t < (int)Grid[s].size(); t++){
      Grid[s][t] = 0; // All points start free
    }
  }
}

bool polyView::isClosestGridPtFree(std::vector< std::vector<int> > & Grid,
                                   int x, int y){

  int numGridPts = Grid.size();
  if (numGridPts <= 0) return false;
  
  // Take a point on the screen and snap it to the closest grid point
  int sx = (int)round ((numGridPts - 1)*(double(x - m_screenXll)/double(m_screenWidX)));
  sx = max(sx, 0); sx = min(sx, numGridPts - 1);
  
  int sy = (int)round ((numGridPts - 1)*(double(y - m_screenYll)/double(m_screenWidY)));
  sy = max(sy, 0); sy = min(sy, numGridPts - 1);

  int maxAllow = 2; // Allow at most this many text labels around one grid point
  if (Grid[sx][sy] <= maxAllow - 1){
    Grid[sx][sy]++;
    return true;
  }else{
    return false;
  }
  
  return false;
}

double polyView::pixelToWorldDist(int pd){

  double x0, x1, y0, y1;
  pixelToWorldCoords(0,  0, x0, y0);
  pixelToWorldCoords(pd, 0, x1, y1);

  return abs(x0 - x1);
  
}

void polyView::setStandardCursor(){
  QCursor C(Qt::ArrowCursor);
  setCursor(C);
}

void polyView::setPolyDrawCursor(){
  QCursor C(Qt::CrossCursor);
  setCursor(C);
}

void polyView::setupDisplayOrder(// Inputs
                                 int                 numPolys,
                                 // Input-output
                                 bool              & changeDisplayOrder,
                                 std::vector<int>  & polyVecOrder
                                 ){


  // Decide the order in which polygons are displayed. 

  if ((int)polyVecOrder.size() != numPolys){

    // Default order
    polyVecOrder.resize(numPolys);
    for (int c = 0; c < numPolys; c++){
      polyVecOrder[c] = c;
    }

  }else if (changeDisplayOrder && numPolys >= 1){

    changeDisplayOrder = false;

    // Cycle left
    int bk = polyVecOrder[0];
    for (int c = 1; c < numPolys; c++) polyVecOrder[c-1] = polyVecOrder[c];
    polyVecOrder[numPolys - 1] = bk;
    
    
  }

  return;
}

void polyView::printCmd(std::string cmd, const std::vector<double> & vals){

  ostringstream S;
  int prec = 16;
  S.precision(prec);
  S << cmd;
  for (int p = 0; p < (int)vals.size(); p++){
    S << ' ' << vals[p];
  }
  S << endl;
  
  cout << S.str();

  return;
}

void polyView::printCmd(std::string cmd, double xll, double yll,
                        double widX, double widY){

  ostringstream S;
  int prec = 16;
  S.precision(prec);
  S << cmd << ' ' << xll << ' ' << yll << ' ' << widX << ' ' << widY << endl;
  cout << S.str();

  return;
}
                                               
void polyView::printCmd(std::string cmd){

  ostringstream S;
  int prec = 16;
  S.precision(prec);
  S << cmd << endl;
  cout << S.str();

  return;
}

void polyView::runCmd(std::string cmd){

  string cmdName = "";
  vector<double> vals; vals.clear();
  double val;
  
  istringstream in(cmd);
  if (in >> cmdName){

    while (in >> val) vals.push_back(val);

    // Commands with no arguments
    if (cmdName == "enforce45"){ enforce45();          return; }
    if (cmdName == "poly_diff"){ toggleShowPolyDiff(); return; }
  
    // Process a command with four numbers are input arguments
    if (cmdName == "view"){

      if (vals.size() >= 4){
        double xll = vals[0], yll = vals[1], widx = vals[2], widy = vals[3];
        if (xll + widx > xll && yll + widy > yll){
          m_viewXll = xll; m_viewWidX = widx;
          m_viewYll = yll; m_viewWidY = widy;
          m_viewChanged = true;
          refreshPixmap();
          return;
        }
      }
      cerr << "Invalid view command: " << cmd << endl;
      return;
      
    }else if (cmdName == "clip"){
      
      if (vals.size() >= 4){
        double xll = vals[0], yll = vals[1], widx = vals[2], widy = vals[3];
        if (xll + widx > xll && yll + widy > yll){
          createHighlightWithRealInputs(xll, yll, xll + widx, yll + widy);
          cutToHlt();
          return;
        }
      }
      cerr << "Invalid clip command: " << cmd << endl;
      return;
      
    }else if (cmdName == "shift"){
      if (vals.size() >= 2){
        shiftPolys(vals);
        return;
      }
      cerr << "Invalid shift command: " << cmd << endl;
      return;
      
    }else if (cmdName == "rotate"){
      if (vals.size() >= 1){
        rotatePolys(vals);
        return;
      }
      cerr << "Invalid rotate command: " << cmd << endl;
      return;
    }else if (cmdName == "scale"){
      if (vals.size() >= 1){
        scalePolys(vals);
        return;
      }
      cerr << "Invalid scale command: " << cmd << endl;
      return;
    }else if (cmdName == "transform"){
      if (vals.size() >= 6){
        transformPolys(vals);
        return;
      }
      cerr << "Invalid transform command: " << cmd << endl;
      return;
    }
    
  }
  
  cerr << "Invalid command: " << cmd << endl;
  
  return;
}

