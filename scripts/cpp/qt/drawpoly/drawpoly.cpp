#include <qwidget.h>
#include <cassert>
#include <cfloat>    // defines DBL_MAX
#include <cmath>
#include <cstdlib>
#include <iomanip>   // required for use of setw()
#include <iostream>
#include <qapplication.h>
#include <qfiledialog.h>
#include <qcursor.h>
#include <qpopupmenu.h>
#include <qdir.h>
#include <qpainter.h>
#include "drawpoly.h"
using namespace std;
using namespace utils;

// To do: handle colors correctly (convert dark-gray to darkGray, etc.).
// To do: Make the two numbers below into members or put them in a
// namespace.
// To do: Do the same with other constants.
const int drawPoly::m_polyChanged;
const int drawPoly::m_createHlt;

drawPoly::drawPoly( QWidget *parent,
                    bool useCmdLineColors,
                    const std::vector<std::string> & cmdLineColors,
                    const std::vector<std::string> & polyFilesVec,
                    const std::vector<bool>        & plotPointsOnlyVec,
                    bool                             plotAsLines,
                    bool                             noClosedPolys
                    ): QWidget(parent){

  setStandardCursor();

  m_useCmdLineColors  = useCmdLineColors;
  m_cmdLineColors     = cmdLineColors;
  m_polyFilesVec      = polyFilesVec;
  m_plotPointsOnlyVec = plotPointsOnlyVec;
  m_plotAsLines       = plotAsLines;
  m_noClosedPolys     = noClosedPolys;
  
  // int
  m_screenXll  = 0; m_screenYll  = 0;
  m_screenWidX = 0; m_screenWidY = 0;

  // double
  m_viewXll  = 0.0; m_viewYll  = 0.0;
  m_viewWidX = 0.0; m_viewWidY = 0.0;

  m_resetView       = true;
  m_prevClickExists = false;
  
  m_showAnnotations    = true;
  m_showVertIndexAnno  = false;
  m_showLayerAnno      = false;
  m_showFilledPolys    = false;
  m_changeDisplayOrder = false;
  
  m_rubberBand = QRect(0, 0, 0, 0); // initial rubberband

  m_showEdges             = 1;
  m_showPointsEdges       = 2;
  m_showPoints            = 3;
  m_toggleShowPointsEdges = m_showEdges;

  m_createPoly                = false;
  m_snapPolyTo45DegreeIntGrid = false;
  m_currPolyX.clear(); m_currPolyY.clear();

  m_zoomToMouseSelection = false;
  m_viewChanged          = false;
  
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
  
  resetTransformSettings();

  m_polyVecBk    = vector<dPoly>();
  m_polyDiffMode = false;
  
  // This statement must be the last
  readAllPolys(); // To do: avoid global variables here

  return;
}

void drawPoly::showPoly( QPainter *paint ){

  // To do: this function needs modularization

  // Dimensions of the plotting window in pixels exluding any window
  // frame/menu bar/status bar
  QRect v       = this->rect();
  m_screenXll   = v.left();
  m_screenYll   = v.top();
  m_screenWidX  = v.width();
  m_screenWidY  = v.height();
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

  }else if(m_viewChanged){

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
  m_pixelSize = max(m_screenWidX/m_viewWidX, m_screenWidY/m_viewWidY);
  //assert( abs(m_pixelSize - m_screenWidX/m_viewWidX) < 1.0e-5*m_pixelSize );
  //assert( abs(m_pixelSize - m_screenWidY/m_viewWidY) < 1.0e-5*m_pixelSize );
  
  // Use a grid to not draw text too densely as that's slow
  vector< vector<int> > Grid; 
  initScreenGrid(Grid);
  
  // Plot the polygons
  int lineWidth = 1;
  QFont F;
  int fontSize = 12;
  F.setPointSize(fontSize);
  //F.setStyleStrategy(QFont::ForceOutline);
  //F.setStyleStrategy(QFont::PreferBitmap);
  //F.setStyleStrategy(QFont::NoAntialias);
  paint->setFont(F);

  // Will draw a vertex with a shape dependent on this index
  int drawVertIndex = -1; 
  
  setupDisplayOrder(//inputs
                    m_polyVec.size(), m_plotPointsOnlyVec,
                    // inputs-outputs
                    m_changeDisplayOrder, m_polyVecOrder        
                    );
  
  // Draw the polygons
  for (int vi  = 0; vi < (int)m_polyVec.size(); vi++){

    int vecIter = m_polyVecOrder[vi];

    bool plotPointsOnly = m_plotPointsOnlyVec[vecIter];
    if (plotPointsOnly                               ||
        m_toggleShowPointsEdges == m_showPoints      ||
        m_toggleShowPointsEdges == m_showPointsEdges 
        ) drawVertIndex++;
    
    // Note: Having annotations at vertices can make the display
    // slow for large polygons.
    // The operations below must happen before cutting,
    // as cutting will inherit the result computed here.
    if (m_showVertIndexAnno){
      m_polyVec[vecIter].compVertIndexAnno();
    }else if (m_showLayerAnno){
      m_polyVec[vecIter].compLayerAnno();
    }
    
    dPoly currPoly = m_polyVec[vecIter]; // local copy which we can modify
    
    // When polys are filled, plot largest polys first
    if (m_showFilledPolys) sortBySizeAndMaybeAddBigFgPoly(//inputs
                                                          m_viewXll,  m_viewYll,
                                                          m_viewXll + m_viewWidX,
                                                          m_viewYll + m_viewWidY,
                                                          // input-output
                                                          currPoly 
                                                          );
    
    dPoly clippedPoly;
    currPoly.clipPoly(//inputs
                      m_viewXll,  m_viewYll,
                      m_viewXll + m_viewWidX,
                      m_viewYll + m_viewWidY,
                      // output
                      clippedPoly
                      );

    const double * xv           = clippedPoly.get_xv();
    const double * yv           = clippedPoly.get_yv();
    const int    * numVerts     = clippedPoly.get_numVerts();
    int numPolys                = clippedPoly.get_numPolys();
    const vector<string> colors = clippedPoly.get_colors();
    //int numVerts              = clippedPoly.get_totalNumVerts();
    
    vector<anno> annotations;
    annotations.clear();
    if (m_showVertIndexAnno){
      clippedPoly.get_vertIndexAnno(annotations);
    }else if (m_showLayerAnno){
      clippedPoly.get_layerAnno(annotations);
    }else if (m_showAnnotations){
      clippedPoly.get_annotations(annotations);
    }
    
    int start = 0;
    for (int pIter = 0; pIter < numPolys; pIter++){

      // Change the poly file color if it is the background color or invalid
      QColor color = QColor( colors[pIter] );
      if ( color == backgroundColor() || color == QColor::Invalid){
        if ( backgroundColor() != QColor("white") ){
          color = QColor("white");
        }else{
          color = QColor("black");
        }
      }
      
      if (pIter > 0) start += numVerts[pIter - 1];

      int pSize = numVerts[pIter];

      // Determine the orientation of polygons
      double signedArea = 0.0;
      if (m_showFilledPolys){
        signedArea = signedPolyArea(pSize, xv + start, yv + start);
      }
      
      QPointArray pa(pSize);
      for (int vIter = 0; vIter < pSize; vIter++){

        int x0, y0;
        worldToPixelCoords(xv[start + vIter], yv[start + vIter], // inputs
                           x0, y0                                // outputs
                           );
        pa[vIter] = QPoint(x0, y0);

        // Qt's built in points are too small. Instead of drawing a point
        // draw a small shape. 
        if ( ( plotPointsOnly                               ||
               m_toggleShowPointsEdges == m_showPoints      ||
               m_toggleShowPointsEdges == m_showPointsEdges
               )
             &&
             x0 > m_screenXll && x0 < m_screenXll + m_screenWidX && 
             y0 > m_screenYll && y0 < m_screenYll + m_screenWidY
             ){
          drawOneVertex(x0, y0, color, lineWidth, drawVertIndex, paint);
        }
      }
      
      if (!plotPointsOnly && m_toggleShowPointsEdges != m_showPoints){

        if (m_showFilledPolys){
          if (signedArea >= 0.0) paint->setBrush( color );
          else                   paint->setBrush( backgroundColor() ); 
          paint->setPen( NoPen );
        }else {
          paint->setBrush( NoBrush );
          paint->setPen( QPen(color, lineWidth) );
        }

        if ( pa.size() >= 1 && isPolyZeroDim(pa) && !m_plotAsLines){
          // Treat the case of polygons which are made up of just one point 
          drawOneVertex(pa[0].x(), pa[0].y(), color, lineWidth, drawVertIndex,
                        paint);
        }else if (!m_noClosedPolys){
          paint->drawPolygon( pa );
        }else{
          paint->drawPolyline( pa ); // don't join the last vertex to the first
        }
        
      }
      
    } // End plotting the current set of polygons

    // Plot the annotations
    int numAnno = annotations.size();
    for (int aIter = 0; aIter < numAnno; aIter++){
      const anno & A = annotations[aIter];
      int x0, y0;
      worldToPixelCoords(A.x, A.y, // inputs
                         x0, y0    // outputs
                         );
      paint->setPen( QPen("gold", lineWidth) );
      if (isClosestGridPtFree(Grid, x0, y0)){
        paint->drawText(x0, y0, A.label);
      }
      
    } // End placing annotations
    
  } // End iterating over sets of polygons

  // Plot the highlights
  for (int h = 0; h < (int)m_highlights.size(); h++){
    drawRect(m_highlights[h], lineWidth, paint);
  }
  
  // This draws the polygon being created if in that mode
  drawCurrPolyLine(paint);

  // Draw the mark if there
  if (m_markX.size() > 0){
    int x0, y0;
    worldToPixelCoords(m_markX[0], m_markY[0], // inputs
                       x0, y0                  // outputs
                       );
    drawMark(x0, y0, "white", lineWidth, paint);
  }
  
  return;
}

void drawPoly::zoomIn(){
  m_zoomFactor  = 0.5;
  m_viewChanged = true;
  update();
}

void drawPoly::zoomOut(){
  m_zoomFactor  = 2.0;
  m_viewChanged = true;
  update();
}

void drawPoly::shiftRight(){
  m_shiftX      = 0.25;
  m_viewChanged = true;
  update();
}

void drawPoly::shiftLeft(){
  m_shiftX      = -0.25;
  m_viewChanged = true;
  update();
}

void drawPoly::shiftUp(){
  m_shiftY      = 0.25;
  m_viewChanged = true;
  update();
}

void drawPoly::shiftDown(){
  m_shiftY      = -0.25;
  m_viewChanged = true;
  update();
}

void drawPoly::centerViewAtPoint(double x, double y){
  m_viewXll     = x - m_viewWidX/2.0;
  m_viewYll     = y - m_viewWidY/2.0;
  m_viewChanged = true;
}

void drawPoly::resetView(){
  m_resetView   = true;
  m_viewChanged = true;
  update();
}


void drawPoly::resetTransformSettings(){
  m_zoomFactor = 1.0;
  m_shiftX     = 0.0; m_shiftY = 0.0;
}

void drawPoly::mousePressEvent( QMouseEvent *E){
  
  const QPoint Q = E->pos();
  m_mousePrsX = Q.x();
  m_mousePrsY = Q.y();
#if 0
  cout << "Mouse pressed at "
       << m_mousePrsX << ' ' << m_mousePrsY << endl;
#endif

  m_rubberBand = QRect(m_mousePrsX, m_mousePrsY, 0, 0); // initial rubberband
}

void drawPoly::mouseMoveEvent( QMouseEvent *E){

  const QPoint Q = E->pos();
  int x = Q.x();
  int y = Q.y();
  
  QPainter painter(this);
  painter.setPen(Qt::white);
  painter.setBrush( NoBrush );
  
  wipeRubberBand(m_rubberBand);
  m_rubberBand = QRect( min(m_mousePrsX, x), min(m_mousePrsY, y),
                        abs(x - m_mousePrsX), abs(y - m_mousePrsY) );
  painter.drawRect(m_rubberBand);
  
}

void drawPoly::mouseReleaseEvent ( QMouseEvent * E ){

  const QPoint Q = E->pos();
  m_mouseRelX = Q.x();
  m_mouseRelY = Q.y();
#if 0
  cout << "Mouse pressed at "
       << m_mousePrsX << ' ' << m_mousePrsY << endl;
  cout << "Mouse released at "
       << m_mouseRelX << ' ' << m_mouseRelY << endl;
#endif

  wipeRubberBand(m_rubberBand); // Wipe any rubberband artifacts

  if (E->state() == (Qt::LeftButton | Qt::AltButton) ){
    // To do: consolidate this with the other call to this function.
    // See if can pass the relevant variables as input arguments.
    pixelToWorldCoords(m_mouseRelX, m_mouseRelY, m_menuX, m_menuY); 
    deletePoly();
  }
  
  if (E->state() == (Qt::LeftButton | Qt::ControlButton) ){
    // Draw a  highlight with control + left mouse button
    // ending at the current point
    createHighlightWithPixelInputs(m_mousePrsX, m_mousePrsY, m_mouseRelX, m_mouseRelY);
    update();
    return;
  }

  int tol = 5; 
  // Any selection smaller than 'tol' number of pixels will be ignored
  // as perhaps the user moved the mouse unintentionally between press
  // and release.
  if       (m_mouseRelX > m_mousePrsX + tol &&
            m_mouseRelY > m_mousePrsY + tol){
    
    m_zoomToMouseSelection = true; // Will zoom to the region selected with the mouse
    update(); 
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


void drawPoly::wheelEvent(QWheelEvent *E){

  int delta = E->delta();

  if (E->state() == Qt::ControlButton){

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
    if (E->state() == Qt::ShiftButton){
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

void drawPoly::keyPressEvent( QKeyEvent *K ){

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
  case Qt::Key_Q:
    QApplication::exit(); 
    break;
  }
  
}

void drawPoly::contextMenuEvent(QContextMenuEvent *E){

  int x = E->x(), y = E->y();
  pixelToWorldCoords(x, y, m_menuX, m_menuY);
  
  QPopupMenu menu(this);
  menu.insertItem("Save mark at point", this, SLOT(saveMark()));
  int id = 1;
  menu.insertItem("Use nm scale", this, SLOT(toggleNmScale()), 0, id);
  menu.setItemChecked(id, m_useNmScale);
  menu.insertItem("Create 45-degree integer polygon", this,
                  SLOT(create45DegreeIntPoly()));
  menu.insertItem("Create arbitrary polygon", this,
                  SLOT(createArbitraryPoly()));
  menu.insertItem("Delete polygon", this, SLOT(deletePoly()));
  menu.exec(E->globalPos());
}

void drawPoly::paintEvent( QPaintEvent*){

  // This function will be called by update()
  
  // Instead of drawing on the screen right away, draw onto
  // a cache, then display the cache. We'll use the cache
  // later to avoid repainting when the view does not change.
  QRect R = this->rect();
  QSize expandedSize = R.size().expandedTo(m_cache.size());
  m_cache.resize(expandedSize);
  m_cache.fill(this, R.topLeft());

  QPainter paint( &m_cache, this );
  paint.translate(-R.x(), -R.y());
  showPoly( &paint );

  // Copy the buffer to the screen
  bitBlt( this, R.x(), R.y(), &m_cache, 0, 0, R.width(), R.height() );

  return;
}

void drawPoly::addPolyVert(int px, int py){

  // Add a point to the polygon being drawn or stop drawing.

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
    
    QPainter paint(this);
    drawCurrPolyLine(&paint);

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
  double minDist   = DBL_MAX;
  int minVecIndex  = -1;
  int minPolyIndex = -1;
  findClosestPolyAndDist(// inputs
                         m_currPolyX[0], m_currPolyY[0],
                         m_polyVec,  
                         // outputs
                         minVecIndex, minPolyIndex,  
                         minDist
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
    color = "green";
    layer = "";
  }

  // Form the new polygon
  dPoly P;
  P.reset();
  P.appendPolygon(pSize, vecPtr(m_currPolyX), vecPtr(m_currPolyY), color, layer);
      
  // So that we can undo later
  m_polyVecStack.push_back(m_polyVec); 
  m_actions.push_back(m_polyChanged);

  // Append the new polygon to the list of polygons. If we have several
  // clips already, append it to the last clip. If we have no clips,
  // create a new clip.
  if (m_polyVec.size() == 0){
      
    m_polyVec.push_back(P);
    m_plotPointsOnlyVec.push_back(false);
    string fileName = "poly" + num2str(m_polyFilesVec.size()) + ".xg";
    m_polyFilesVec.push_back(fileName);
      
  }else{
    m_polyVec[m_polyVec.size() - 1].appendPolygons(P);
  }
    
  // Reset
  m_createPoly = false;
  m_currPolyX.clear();
  m_currPolyY.clear();
  setStandardCursor();
  update();
    
  return;
}

void drawPoly::drawCurrPolyLine(QPainter * paint){

  int pSize = m_currPolyX.size();
  if (pSize == 0){
    return;
  }
  
  int lineWidth = 1;
  char * color  = "white";
  paint->setBrush( NoBrush );
  paint->setPen( QPen(color, lineWidth) );

  // To do: The block below better become its own function
  // which can draw points, poly lines, and polygons
  QPointArray pa(pSize);
  for (int vIter = 0; vIter < pSize; vIter++){
      
    int x0, y0;
    worldToPixelCoords(m_currPolyX[vIter], m_currPolyY[vIter], // inputs
                       x0, y0                                  // outputs
                       );
    pa[vIter] = QPoint(x0, y0);

    if (vIter == 0){
      // Emphasize the starting point of the polygon
      paint->drawRect(x0 - m_pixelTol,  y0 - m_pixelTol,
                      2*m_pixelTol, 2*m_pixelTol); 
    }
  }

  paint->drawPolyline( pa );

  return;
}

void drawPoly::createHighlightWithPixelInputs(int pxll, int pyll, int pxur, int pyur
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

void drawPoly::createHighlightWithRealInputs(double xll, double yll,
                                             double xur, double yur
                                             ){
  
  // To do: Use dPoly instead of dRect so that we can plot
  // highlights exactly in the same way as we plot polygons.
  dRect R( min(xll, xur), min(yll, yur), max(xll, xur), max(yll, yur) );
  m_highlights.push_back(R);
  m_actions.push_back(m_createHlt);
  
  return;
}

void drawPoly::printCurrCoords(const ButtonState & state, // input
                               int & currX, int  & currY  // in-out
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

  QPainter paint(this);
  int len = 3, lineWidth = 1;
  paint.setPen( QPen("white", lineWidth) );
  paint.setBrush( NoBrush );

  // Snap to the closest vertex with the left mouse button
  if (state == Qt::LeftButton){
      
    double min_x, min_y, min_dist;
    findClosestPointAndDist(wx, wy, m_polyVec,     // inputs
                            min_x, min_y, min_dist // outputs
                            );
    wx = min_x; wy = min_y;
    worldToPixelCoords(wx, wy,      // inputs
                       currX, currY // outputs
                       );

    paint.drawEllipse(currX - len, currY - len, 2*len, 2*len);
    
  }else if (state == (Qt::LeftButton | Qt::ShiftButton)
            ||
            state == (Qt::MidButton)
            ){
      
    // Don't snap with the shift-left button or the middle button
    paint.drawRect(currX - len, currY - len, 2*len, 2*len);
      
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


void drawPoly::wipeRubberBand(QRect & R){
  
  // Wipe the current rubberband by overwriting the region it occupies
  // (a set of four segments forming a rectangle) with the cached
  // version of image before the rubberband was drawn.

  int left  = min(R.left(), R.right());
  int right = max(R.left(), R.right());
  int top   = min(R.top(), R.bottom());
  int bot   = max(R.top(), R.bottom());
  int wd    = abs(R.width());
  int ht    = abs(R.height());
  int px    = 1; 

  QPainter paint(this);
  paint.drawPixmap (left,  top, m_cache, left,  top, wd, px);
  paint.drawPixmap (left,  top, m_cache, left,  top, px, ht);
  paint.drawPixmap (left,  bot, m_cache, left,  bot, wd, px);
  paint.drawPixmap (right, top, m_cache, right, top, px, ht);

  R = QRect(0, 0, 0, 0); // wipe
  
  return;
}

void drawPoly::pixelToWorldCoords(int px, int py,
                                  double & wx, double & wy){

  // Compensate for the Qt's origin being in the upper-left corner
  // instead of the lower-left corner.
  py = m_screenWidY - py;

  wx = px/m_pixelSize + m_viewXll;
  wy = py/m_pixelSize + m_viewYll;

}

void drawPoly::worldToPixelCoords(double wx, double wy,
                                  int & px,  int & py){

  px = iround((wx - m_viewXll)*m_pixelSize);
  py = iround((wy - m_viewYll)*m_pixelSize);
  
  // Compensate for the Qt's origin being in the upper-left corner
  // instead of the lower-left corner.
  py = m_screenWidY - py;
  
}

void drawPoly::setUpViewBox(// inputs
                            const std::vector<dPoly> & polyVec,
                            // outputs
                            double & xll, double & yll,
                            double &widx, double & widy){

  // To do: Move this to utilities
  
  // Given a set of polygons, set up a box containing these polygons.

  double xur, yur; // local variables
  
  // Start with the poly bounding box
  double big = DBL_MAX;
  xll = big; yll = big; xur = -big; yur = -big;
  for (int p = 0; p < (int)polyVec.size(); p++){
    double xll0, yll0, xur0, yur0;
    polyVec[p].bdBox(xll0, yll0, xur0, yur0);
    xll = min(xll, xll0); xur = max(xur, xur0);
    yll = min(yll, yll0); yur = max(yur, yur0);
  }

  // Treat the case of empty polygons
  if (xur < xll || yur < yll){
    xll = 0.0; yll = -1000.0; xur = 1000.0; yur = 0.0;
  }

  // Treat the case when the polygons are degenerate
  if (xur == xll){ xll -= 0.5; xur += 0.5; }
  if (yur == yll){ yll -= 0.5; yur += 0.5; }
    
  widx = xur - xll; assert(widx > 0.0);
  widy = yur - yll; assert(widy > 0.0);

  //   cout << "Bd box ll corner and widths are "
  //        << xll  << ' ' << yll << ' ' << widx << ' ' << widy << endl;

  // Expand the box slightly for plotting purposes
  double factor = 0.05;
  xll -= widx*factor; xur += widx*factor; widx *= 1.0 + 2*factor;
  yll -= widy*factor; yur += widy*factor; widy *= 1.0 + 2*factor;
  
  return;
  
}


void drawPoly::drawOneVertex(int x0, int y0, QColor color, int lineWidth,
                             int drawVertIndex, QPainter * paint){

  // Draw a vertex as a small shape (a circle, rectangle, triangle)
  
  drawVertIndex = max(0, drawVertIndex);

  // Use variable size shapes to distinguish better points on top of
  // each other
  int len = 3*(drawVertIndex+1); 

  paint->setPen( QPen(color, lineWidth) );

  int numTypes = 4;
  if (drawVertIndex%numTypes == 0){
    
    // Draw a small filled ellipse
    paint->setBrush( NoBrush );
    paint->drawEllipse(x0 - len, y0 - len, 2*len, 2*len);
    
  }else if (drawVertIndex%numTypes == 1){
    
    // Draw an empty square
    paint->setBrush( NoBrush );
    paint->drawRect(x0 - len, y0 - len, 2*len, 2*len);
    
  }else if (drawVertIndex%numTypes == 2){
    
    // Draw an empty triangle
    paint->setBrush( NoBrush );
    paint->drawLine(x0 - len, y0 - len, x0 + len, y0 - len);
    paint->drawLine(x0 - len, y0 - len, x0 + 0,   y0 + len);
    paint->drawLine(x0 + len, y0 - len, x0 + 0,   y0 + len);
    
  }else{
    
    // Draw an empty reversed triangle
    paint->setBrush( NoBrush );
    paint->drawLine(x0 - len, y0 + len, x0 + len, y0 + len);
    paint->drawLine(x0 - len, y0 + len, x0 + 0,   y0 - len);
    paint->drawLine(x0 + len, y0 + len, x0 + 0,   y0 - len);
#if 0
    // Draw a star
    paint->setBrush( NoBrush );
    paint->drawLine(x0 - len, y0 - len, x0 + len, y0 + len);
    paint->drawLine(x0 - len, y0 + len, x0 + len, y0 - len);
    paint->drawLine(x0 - len, y0, x0 + len, y0);
    paint->drawLine(x0, y0 - len, x0, y0 + len);
#endif  
  }
  
  return;
}

void drawPoly::drawMark(int x0, int y0, QColor color, int lineWidth,
                        QPainter * paint){
  
  int len = 6;

  paint->setBrush( NoBrush );
  paint->setPen( QPen(color, lineWidth) );

  // Draw a cross
  paint->drawLine(x0 - len, y0 - len, x0 + len, y0 + len);
  paint->drawLine(x0 - len, y0 + len, x0 + len, y0 - len);
  
}

void drawPoly::toggleAnno(){
  m_showAnnotations   = !m_showAnnotations;
  m_showVertIndexAnno = false;
  m_showLayerAnno     = false;
  update();
}

void drawPoly::toggleVertIndexAnno(){
  m_showVertIndexAnno = !m_showVertIndexAnno;
  m_showAnnotations   = false;
  m_showLayerAnno     = false;
  update();
}

void drawPoly::toggleLayerAnno(){
  m_showLayerAnno     = !m_showLayerAnno;
  m_showAnnotations   = false;
  m_showVertIndexAnno = false;
  update();
}

void drawPoly::toggleFilled(){
  m_showFilledPolys = !m_showFilledPolys;
  update();
}

void drawPoly::toggleShowPolyDiff(){

  // Show the differences of two polygons as points
  
  if (m_polyDiffMode){
    m_polyDiffMode = false;
    m_polyVec      = m_polyVecBk;
    update();
    return;
  }

  if (m_polyVec.size() < 2){
    cerr << "Error: Must have at least two polygons to compare" << endl;
    return;
  }
  
  m_polyDiffMode = true;
  m_polyVecBk    = m_polyVec;
  m_polyVec.resize(2);

  dPoly & P = m_polyVec[0]; // alias
  dPoly & Q = m_polyVec[1]; // alias
  vector<dPoint>  vP, vQ;

  findPolyDiff(P, Q,  // inputs
               vP, vQ // outputs
               );

  update();
}

void drawPoly::create45DegreeIntPoly(){

  // This flag will change the behavior of mouseReleaseEvent() so that
  // we can start adding points to the polygon with the mouse.
  m_createPoly                = true;

  m_snapPolyTo45DegreeIntGrid = true;
  
  setPolyDrawCursor();
}

void drawPoly::createArbitraryPoly(){

  // This flag will change the behavior of mouseReleaseEvent() so that
  // we can start adding points to the polygon with the mouse.
  m_createPoly                = true;
  
  m_snapPolyTo45DegreeIntGrid = false;
  
  setPolyDrawCursor();
}

void drawPoly::deletePoly(){

  if (m_polyVec.size() == 0) return;
  
  // So that we can undo later
  m_polyVecStack.push_back(m_polyVec); 
  m_actions.push_back(m_polyChanged);

  double minDist   = DBL_MAX;
  int minVecIndex  = -1;
  int minPolyIndex = -1;
  findClosestPolyAndDist(// inputs
                         m_menuX, m_menuY,
                         m_polyVec,  
                         // outputs
                         minVecIndex, minPolyIndex,  
                         minDist
                         );
  
  if (minVecIndex >= 0 && minPolyIndex >= 0){
    m_polyVec[minVecIndex].erasePoly(minPolyIndex);
  }

  update();
  
  return;
}

void drawPoly::saveMark(){

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
  update();
}

void drawPoly::toggleNmScale(){

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

// actions

void drawPoly::drawRect(const utils::dRect & R, int lineWidth,
                        QPainter * paint){

  int numV = 4;
  double xv[] = { R.left(), R.right(), R.right(),  R.left()   };
  double yv[] = { R.top(),  R.top(),   R.bottom(), R.bottom() };
  
  QPointArray pa(numV);
  for (int vIter = 0; vIter < numV; vIter++){
    int x0, y0;
    worldToPixelCoords(xv[vIter], yv[vIter], // inputs
                       x0, y0                // outputs
                       );
    pa[vIter] = QPoint(x0, y0);
  }
  paint->setBrush( NoBrush );
  paint->setPen( QPen("white", lineWidth) );
  paint->drawPolygon( pa );

  return;
}

void drawPoly::cutToHlt(){
  
  // Cut to the last highlight
  int numH = m_highlights.size();
  if ( numH == 0){
    cout << "No highlights" << endl;
    return;
  }
  
  // So that we can undo later
  m_polyVecStack.push_back(m_polyVec);
  m_actions.push_back(m_polyChanged);
  
  dRect H = m_highlights[numH - 1];

  printCmd( "clip", H.left(), H.top(), H.right() - H.left(), H.bottom() - H.top() );
    
  dPoly clippedPoly;
  for (int vecIter = 0; vecIter < (int)m_polyVec.size(); vecIter++){

    m_polyVec[vecIter].clipPoly(//inuts
                                H.left(), H.top(),
                                H.right(), H.bottom(),
                                // output
                                clippedPoly
                                );

    m_polyVec[vecIter] = clippedPoly;    
  }

  m_highlights.resize(numH - 1);
  
  update();
  
}

void drawPoly::enforce45(){
  
  // Enforce that polygon vertices are integers and the angles are 45x. 

  // So that we can undo later
  m_polyVecStack.push_back(m_polyVec);
  m_actions.push_back(m_polyChanged);
  
  printCmd("enforce45");
    
  for (int vecIter = 0; vecIter < (int)m_polyVec.size(); vecIter++){
    m_polyVec[vecIter].enforce45();
  }

  update();
  return;
}

void drawPoly::undoLast(){

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
    update();
    
  }else if (lastAction == m_polyChanged){
  
    int numCopies = m_polyVecStack.size();
    if (numCopies == 0){
      cout << "No cutting to undo" << endl;
      return;
    }

    m_polyVec = m_polyVecStack[numCopies - 1];
    m_polyVecStack.resize(numCopies - 1);
    update();

  }

  return;
}

void drawPoly::readAllPolys(){

  int numFiles = m_polyFilesVec.size();
  m_polyVec.resize(numFiles);
  
  for (int fileIter = 0; fileIter < numFiles; fileIter++){
    
    readOnePoly(// inputs
                m_polyFilesVec[fileIter], m_plotPointsOnlyVec[fileIter],
                // output
                m_polyVec[fileIter]
                );

    if (m_useCmdLineColors){
      m_polyVec[fileIter].setColor(m_cmdLineColors[fileIter]);
    }
    
  }

  return;
}

void drawPoly::openPoly(){

  QString s = QFileDialog::getOpenFileName(
                                           QDir::currentDirPath(),
                                           "(*.xg *.ly* *.pol)",
                                           this,
                                           "open file dialog"
                                           "Choose a file" );

  if (s.length() == 0) return;
  
  string fileName = string(s.data());

  bool plotPointsOnly = false;

  int numFiles = m_polyFilesVec.size();
  m_polyFilesVec.resize      (numFiles+1);
  m_plotPointsOnlyVec.resize (numFiles+1);
  m_polyVec.resize           (numFiles+1);

  m_polyFilesVec[numFiles]      = fileName;
  m_plotPointsOnlyVec[numFiles] = plotPointsOnly;
  readOnePoly(// inputs
              m_polyFilesVec[numFiles], m_plotPointsOnlyVec[numFiles],
              // output
              m_polyVec[numFiles]
              );

  resetView();

  return;
}

void drawPoly::readOnePoly(// inputs
                           std::string & filename,
                           bool plotPointsOnly,
                           // output
                           dPoly & poly           
                           ){
  
  string type = getFilenameExtension(filename);
  
  if (type == "pol" || type == "cnt"){
     if ( poly.read_pol_or_cnt_format(filename, type, plotPointsOnly) ) return;
     cerr << "Invalid ." << type << " format for " << filename << ". Trying to read it in .xg format." << endl;
  }
  
  if ( ! poly.readPoly(filename, plotPointsOnly) ){
    exit(1);
  }
  
  return;
}

  
void drawPoly::saveOnePoly(){

  if (m_polyVec.size() == 0){
    cerr << "No polygons to save" << endl;
  }

  char * fileName = "out_poly.xg";

  dPoly poly;

  for (int polyIter = 0; polyIter < (int)m_polyVec.size(); polyIter++){
    poly.appendPolygons(m_polyVec[polyIter]); 
  }

  poly.writePoly(fileName);
  cout << "Polygon saved to " << fileName << endl;

  return;
}

void drawPoly::overwriteMultiplePolys(){
  bool overwrite = true;
  saveMultiplePoly(overwrite);
}

void drawPoly::saveAsMultiplePolys(){
  bool overwrite = false;
  saveMultiplePoly(overwrite);
}

void drawPoly::saveMultiplePoly(bool overwrite){

  string allFiles = "";
  for (int polyIter = 0; polyIter < (int)m_polyVec.size(); polyIter++){

    dPoly poly = m_polyVec[polyIter];
    
    string fileName;
    if (overwrite){
      fileName = m_polyFilesVec[polyIter];
    }else{
      fileName = inFileToOutFile(m_polyFilesVec[polyIter]);
    }
    
    poly.writePoly(fileName.c_str());
    allFiles += " " + fileName;
  }

  if ((int)m_polyVec.size() > 0){
    cout << "Polygons saved to" << allFiles << endl;
  }

  return;
}

void drawPoly::togglePE(){

  m_toggleShowPointsEdges = m_toggleShowPointsEdges%3 + 1;
  update();
  
}

void drawPoly::changeOrder(){

  m_changeDisplayOrder = true;
  update();
  
}

bool drawPoly::isPolyZeroDim(const QPointArray & pa){

  int numPts = pa.size();
  for (int s = 1; s < numPts; s++){
    if (pa[0] != pa[s]) return false;
  }
  
  return true;
}

void drawPoly::initScreenGrid(std::vector< std::vector<int> > & Grid){

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

bool drawPoly::isClosestGridPtFree(std::vector< std::vector<int> > & Grid,
                                   int x, int y){

  int numGridPts = Grid.size();
  
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

double drawPoly::pixelToWorldDist(int pd){

  double x0, x1, y0, y1;
  pixelToWorldCoords(0,  0, x0, y0);
  pixelToWorldCoords(pd, 0, x1, y1);

  return abs(x0 - x1);
  
}

void drawPoly::setStandardCursor(){
  QCursor C(Qt::ArrowCursor);
  setCursor(C);
}

void drawPoly::setPolyDrawCursor(){
  QCursor C(Qt::CrossCursor);
  setCursor(C);
}

void drawPoly::setupDisplayOrder(int                 numPolys, 
                                 std::vector<bool> & plotPointsOnlyVec,
                                 bool              & changeDisplayOrder,
                                 std::vector<int>  & polyVecOrder){


  // Decide the order in which polygons are displayed. 

  if ((int)polyVecOrder.size() != numPolys){

    // Default order
    polyVecOrder.resize(numPolys);
    for (int c = 0; c < numPolys; c++){
      polyVecOrder[c] = c;
    }
    
  }else if (changeDisplayOrder && numPolys >= 1){

    changeDisplayOrder = false;

    assert((int)plotPointsOnlyVec.size() == numPolys);

    bool hasNonPointPolys = false;
    for (int c = 0; c < numPolys; c++){
      if (!plotPointsOnlyVec[c]){
        hasNonPointPolys = true;
        break;
      }
    }
    if (!hasNonPointPolys){
      return;
    }
    
    // Cycle left
    bool firstCycle = true;
    while (
           // We want the last polygon (the one displayed on top)
           // to not be made up of points only.
           firstCycle || plotPointsOnlyVec[polyVecOrder[numPolys - 1]]
           ){
      
      firstCycle = false;
      
      int bk = polyVecOrder[0];
      for (int c = 1; c < numPolys; c++){
        polyVecOrder[c-1] = polyVecOrder[c];
      }
      polyVecOrder[numPolys - 1] = bk;
      
    }
    
  }

  return;
}

void drawPoly::printCmd(std::string cmd, double xll, double yll,
                        double widX, double widY){

  ostringstream S;
  int prec = 16;
  S.precision(prec);
  S << cmd << ' ' << xll << ' ' << yll << ' ' << widX << ' ' << widY << endl;
  cout << S.str();

  return;
}
                                               
void drawPoly::printCmd(std::string cmd){

  ostringstream S;
  int prec = 16;
  S.precision(prec);
  S << cmd << endl;
  cout << S.str();

  return;
}

void drawPoly::runCmd(std::string cmd){

  string cmdName;

  // Process a command with no input arguments
  istringstream in0(cmd);
  if ( in0 >> cmdName && cmdName == "enforce45"){
    enforce45();
    return;
  }

  // Process a command with four numbers are input arguments
  istringstream in(cmd);
  double xll, yll, widx, widy;
  if ( in >> cmdName >> xll >> yll >> widx >> widy){
      
    if (cmdName == "view"){
      
      if (xll + widx > xll && yll + widy > yll){
        m_viewXll = xll; m_viewWidX = widx;
        m_viewYll = yll; m_viewWidY = widy;
        m_viewChanged = true;
        update();
      }else{
        cerr << "Invalid view request" << endl;
      }
      
    }else if (cmdName == "clip"){
      
      if (xll + widx > xll && yll + widy > yll){
        createHighlightWithRealInputs(xll, yll, xll + widx, yll + widy);
        cutToHlt();
      }else{
        cerr << "Invalid clip request" << endl;
      }
      
    }
    
  }else{
    cerr << "Invalid command: " << cmd << endl;
  }
  
  return;
}

void drawPoly::sortBySizeAndMaybeAddBigFgPoly(// inputs
                                              double viewXll, double viewYll,
                                              double viewXur, double viewYur,
                                              // input-output
                                              dPoly& poly
                                              ){

  // Sort the polygons from largest to smallest by size. If the
  // largest one is going clockwise, it is a hole. In that case, add a
  // large box going counter-clockwise so that we see the hole as a
  // hole in this box. This is important only when polygons are filled
  // (and holes are of background color).

  // We use the view box (what we currently see on the screen) as
  // reference to the size of the box to add.
  
  poly.sortFromLargestToSmallest();

  if (poly.get_numPolys() <= 0) return;

  const double         * xv       = poly.get_xv();
  const double         * yv       = poly.get_yv();
  const int            * numVerts = poly.get_numVerts();
  const vector<string> & colors   = poly.get_colors();
  const vector<string> & layers   = poly.get_layers();

  double signedArea = signedPolyArea(numVerts[0], xv, yv);

  if (signedArea >= 0) return; // Outer poly is correctly oriented

  double xll, yll, xur, yur;
  poly.bdBox(xll, yll, xur, yur);
  xll = min(xll, viewXll); xur = max(xur, viewXur);
  yll = min(yll, viewYll); yur = max(yur, viewYur);

  // Add the extra term to ensure we get a box strictly bigger than
  // the polygons and the view (this will help with sorting below).
  double extra = max(abs(xur - xll), abs(yur - yll)) + 10.0;
  xll -= extra; xur += extra;
  yll -= extra; yur += extra;

  double boxX[4], boxY[4];
  boxX[0] = xll; boxX[1] = xur; boxX[2] = xur; boxX[3] = xll;
  boxY[0] = yll; boxY[1] = yll; boxY[2] = yur; boxY[3] = yur;
  poly.appendPolygon(4, boxX, boxY, colors[0], layers[0]);

  // Reorder the updated set of polygons
  poly.sortFromLargestToSmallest();

  return;
}

