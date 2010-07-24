#include <cassert>
#include <cfloat>    // defines DBL_MAX
#include <cmath>
#include <cstdlib>
#include <iomanip>   // required for use of setw()
#include <iostream>
#include <qapplication.h>
#include <qfiledialog.h>
#include <qdir.h>
#include <qpainter.h>
#include "drawpoly.h"
using namespace std;
using namespace utils;

const int drawPoly::m_cutToHlt;
const int drawPoly::m_createHlt;

drawPoly::drawPoly( QWidget *parent, 
                    const std::vector<std::string> & polyFilesVec,
                    const std::vector<bool>        & plotPointsOnlyVec
                    ):
  QWidget(parent){

  m_polyFilesVec      = polyFilesVec;
  m_plotPointsOnlyVec = plotPointsOnlyVec;
  
  m_yFactor = -1; // To compensate for Qt's origin in the upper-left corner
  
  // int
  m_screenXll  = 0; m_screenYll  = 0;
  m_screenWidX = 0; m_screenWidY = 0;

  // double
  m_viewXll  = 0.0; m_viewYll  = 0.0;
  m_viewWidX = 0.0; m_viewWidY = 0.0;

  m_resetView       = true;
  m_prevClickExists = false;
  
  m_showAnnotations    = true;
  m_showVertIndices    = false;
  m_showFilledPolys    = false;
  m_showInReverseOrder = false;
  
  m_rubberBand = QRect( 0, 0, 0, 0); // initial rubberband

  m_showEdges             = 1;
  m_showPointsEdges       = 2;
  m_showPoints            = 3;
  m_toggleShowPointsEdges = m_showEdges;

  m_addPoly = false;
  
  resetTransformSettings();
  initOpenPoly();
}

void drawPoly::showPoly( QPainter *paint ){

  // Dimensions of the plotting window in pixels exluding any window
  // frame/menu bar/status bar
  QRect v       = this->rect();
  m_screenXll   = v.left();
  m_screenYll   = v.top();
  m_screenWidX  = v.width();
  m_screenWidY  = v.height();
  m_screenRatio = double(m_screenWidY)/double(m_screenWidX);

  // To have the polygon show up a bit inside the screen use some padding.
  // This must be set early as converting from screen to world coordinates
  // depends on this.
  m_padX = 0.0; m_padY = m_screenRatio*m_padX; // Units are pixels

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
  
//   paint->setWindow(m_screenXll,  m_screenYll,
//                    m_screenWidX, m_screenWidY
//                    );
//   paint->setViewport(m_screenXll,  m_screenYll,
//                      m_screenWidX, m_screenWidY
//                      );

  // Create the new view
  double xll, yll, xur, yur, widx, widy;
    
  if (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    
    // Form a new view based on the rectangle selected with the mouse.
    // Enlarge this rectangle if necessary to keep the aspect ratio.

    // The call to pixelToWorldCoords uses the existing view internally
    pixelToWorldCoords(m_mousePrsX, m_mousePrsY, xll, yll);
    pixelToWorldCoords(m_mouseRelX, m_mouseRelY, xur, yur);
    widx = xur - xll;
    widy = yur - yll;

  }else{

    // Modify the view for given shift or zoom
    xll  = m_viewXll + m_viewWidX*( (1 - m_zoomFactor)/2.0 + m_shiftX );
    yll  = m_viewYll + m_viewWidY*( (1 - m_zoomFactor)/2.0 + m_shiftY );
    widx = m_viewWidX*m_zoomFactor;
    widy = m_viewWidY*m_zoomFactor;
    
  }

  // If the view becomes too small, don't accept it
  if (widx == 0 || widy == 0){
    cerr << "Cannot zoom in more" << endl;
  }else{
    expandBoxToGivenRatio(//inputs
                          m_screenRatio,  
                          // input/outputs
                          xll, yll, widx, widy
                          );
    
    // Overwrite the view
    m_viewXll = xll; m_viewWidX = widx;
    m_viewYll = yll; m_viewWidY = widy;
  }

  // Having computed the new view reset the numbers used to manipulate
  // it so that we can start fresh with new manipulations next time
  // (but starting from the newly computed view).
  resetTransformSettings();
  
  m_pixelSize = (m_screenWidX - 2*m_padX)/m_viewWidX;
  assert( abs(m_pixelSize - (m_screenWidY - 2*m_padY)/m_viewWidY)
          < 1.0e-5*m_pixelSize );
  
  vector< vector<int> > Grid; // To not draw text too densely as that's slow
  initScreenGrid(Grid);
  
  // Plot the polygons
  int lineWidth = 1;
  QFont F;
  int fontSize = 12;
  F.setPointSize(fontSize);
  //F.setStyleStrategy(QFont::ForceOutline);
  //F.setStyleStrategy(QFont::PreferBitmap);
  // F.setStyleStrategy(QFont::NoAntialias);
  paint->setFont(F);

  int drawVertIndex = -1; // Will draw a vertex with a shape dependent on this
  
  // Draw the polygons
  for (int vi  = 0; vi < (int)m_polyVec.size(); vi++){

    int vecIter = vi;
    if (m_showInReverseOrder) vecIter = (int)m_polyVec.size() - 1 - vi;
    
    bool plotPointsOnly = m_plotPointsOnlyVec[vecIter];
    if (plotPointsOnly                               ||
        m_toggleShowPointsEdges == m_showPoints      ||
        m_toggleShowPointsEdges == m_showPointsEdges 
        ) drawVertIndex++;
    
    if (m_showVertIndices){
      // Note: Having annotations at vertices can make the display slow for large
      // polygons.
      m_polyVec[vecIter].compAnnoAtVerts();
    }
    
    dPoly clipPoly;
    m_polyVec[vecIter].clipPoly(//inuts
                                m_viewXll,  m_viewYll,
                                m_viewXll + m_viewWidX,
                                m_viewYll + m_viewWidY,
                                // output
                                clipPoly
                                );
    
    const double * xv           = clipPoly.get_xv();
    const double * yv           = clipPoly.get_yv();
    const int    * numVerts     = clipPoly.get_numVerts();
    int numPolys                = clipPoly.get_numPolys();
    const vector<string> colors = clipPoly.get_colors();
    //int numVerts              = clipPoly.get_totalNumVerts();
    
    vector<anno> annotations;
    annotations.clear();
    if (m_showVertIndices){
      clipPoly.getAnnoAtVerts(annotations);
    }else if (m_showAnnotations){
      clipPoly.get_annotations(annotations);
    }
    
    int start = 0;
    for (int pIter = 0; pIter < numPolys; pIter++){

      // To do: Don't assume the bg is black, rather check what it is
      string strColor = colors[pIter];
      if (strColor == "black"){ // Avoid conflicts with the background
        strColor = "white";
      }
      QColor color = QColor( strColor.c_str() );

      if (pIter > 0) start += numVerts[pIter - 1];

      int pSize = numVerts[pIter];
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
          paint->setBrush( color );
          paint->setPen( NoPen );
        }else {
          paint->setBrush( NoBrush );
          paint->setPen( QPen(color, lineWidth) );
        }

        if ( pa.size() >= 1 && isPolyZeroDim(pa) ){
          // Treat the case of polygons which are made up of just one point 
          drawOneVertex(pa[0].x(), pa[0].y(), color, lineWidth, drawVertIndex,
                        paint);
        }else{
          paint->drawPolygon( pa );
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
  
  // Plot the view box
  dRect R(m_viewXll, m_viewYll,
          m_viewXll + m_viewWidX, m_viewYll + m_viewWidY);
  drawRect(R, lineWidth, paint);
    
   return;
}

void drawPoly::zoomIn(){
  resetTransformSettings();
  m_zoomFactor = 0.5;
  update();
}

void drawPoly::zoomOut(){
  resetTransformSettings();
  m_zoomFactor = 2.0;
  update();
}

void drawPoly::shiftRight(){
  resetTransformSettings();
  m_shiftX = 0.25;
  update();
}

void drawPoly::shiftLeft(){
  resetTransformSettings();
  m_shiftX = -0.25;
  update();
}

void drawPoly::shiftUp(){
  resetTransformSettings();
  m_shiftY = -0.25;
  update();
}

void drawPoly::shiftDown(){
  resetTransformSettings();
  m_shiftY = 0.25;
  update();
}

void drawPoly::centerViewAtPoint(double x, double y){
  m_viewXll = x - m_viewWidX/2.0;
  m_viewYll = y - m_viewWidY/2.0;
}

void drawPoly::resetView(){
  resetTransformSettings();
  m_resetView = true;
  update();
}


void drawPoly::resetTransformSettings(){
  m_zoomFactor = 1.0;
  m_shiftX     = 0.0; m_shiftY    = 0.0;
  m_mousePrsX  = 0;   m_mousePrsY = 0;
  m_mouseRelX  = 0;   m_mouseRelY = 0;
}

void drawPoly::mousePressEvent( QMouseEvent *E){
  
  const QPoint Q = E->pos();
  m_mousePrsX = Q.x();
  m_mousePrsY = Q.y();
  
  m_rubberBand = QRect( m_mousePrsX, m_mousePrsY, 0, 0); // initial rubberband
//   cout << "Mouse pressed at "
//        << m_mousePrsX << ' ' << m_mousePrsY << endl;

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

  if (E->state() == (Qt::LeftButton | Qt::ControlButton) ){
    // Draw a  highlight with control + left mouse button
    // ending at the current point
    createHighlight(m_mousePrsX, m_mousePrsY, m_mouseRelX, m_mouseRelY);
    resetTransformSettings(); // To not zoom around the mosue release coords
    update();
    return;
  }

  // Any selection smaller than this will be ignored as perhaps the
  // user moved the mouse unintentionally between press and release.
  int tol = 5; 
  if       (m_mouseRelX > m_mousePrsX + tol &&
            m_mouseRelY > m_mousePrsY + tol){
    update(); // Will zoom to the region selected with the mouse
    return;
  }else if (m_mouseRelX + tol < m_mousePrsX &&
            m_mouseRelY + tol < m_mousePrsY ){
    zoomOut();
    return;
  }else if (abs(m_mouseRelX - m_mousePrsX) <= tol &&
            abs(m_mouseRelY - m_mousePrsY) <= tol){
    printCurrCoords(E);
    return;
  }    
   
  return;
}

void drawPoly::createHighlight(// inputs are in pixels
                               int pxll, int pyll, int pxur, int pyur
                               ){
    
  double xll, yll, xur, yur;
  pixelToWorldCoords(pxll, pyll, // inputs
                     xll, yll    // outputs
                     );
  pixelToWorldCoords(pxur, pyur, // inputs
                     xur, yur    // outputs
                     );
  
  dRect R( min(xll, xur), min(yll, yur), max(xll, xur), max(yll, yur) );
  m_highlights.push_back(R);
  m_actions.push_back(m_createHlt);
  
  return;
}

void drawPoly::printCurrCoords(QMouseEvent * E){
  
  // Print the physcal coordinates of the point the mouse was released at

  int prec = 6, wid = prec + 6;
  cout.precision(prec);
  cout.setf(ios::floatfield);
        
  double wx, wy;
  pixelToWorldCoords(m_mouseRelX, m_mouseRelY, wx, wy);

  QPainter paint(this);
  int len = 3, lineWidth = 1;
  paint.setPen( QPen("white", lineWidth) );
  paint.setBrush( NoBrush );

  // Snap to the closest vertex with the left mouse button
  if (E->state() == Qt::LeftButton){
      
    double min_x, min_y, min_dist;
    findClosestPointAndDist(wx, wy, m_polyVec,     // inputs
                            min_x, min_y, min_dist // outputs
                            );
    wx = min_x; wy = min_y;
    worldToPixelCoords(wx, wy,                  // inputs
                       m_mouseRelX, m_mouseRelY // outputs
                       );

    paint.drawEllipse(m_mouseRelX - len, m_mouseRelY - len, 2*len, 2*len);
    
  }else if (E->state() == (Qt::LeftButton | Qt::ShiftButton)
            ||
            E->state() == (Qt::MidButton)
            ){
      
    // Don't snap with the shift-left button or the middle button
    paint.drawRect(m_mouseRelX - len, m_mouseRelY - len, 2*len, 2*len);
      
  }

  cout << "Point: ("
       << setw(wid) << wx << ", "
       << setw(wid) << wy*m_yFactor << ")";
  if (m_prevClickExists){
    cout  << " dist from prev: ("
          << setw(wid) << wx - m_prevClickedX << ", "
          << setw(wid) << (wy - m_prevClickedY)*m_yFactor
          << ") Euclidean: "
          << setw(wid) << sqrt( (wx - m_prevClickedX)*(wx - m_prevClickedX)
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

void drawPoly::mouseMoveEvent( QMouseEvent *E){

  const QPoint Q = E->pos();
  int x = Q.x();
  int y = Q.y();
  //cout << "Mouse moved to " << x << ' ' << y << endl;
  
  QPainter painter(this);
  painter.setPen(Qt::white);
  painter.setBrush( NoBrush );
  
  wipeRubberBand(m_rubberBand);
  m_rubberBand = QRect( min(m_mousePrsX, x), min(m_mousePrsY, y),
                        abs(x - m_mousePrsX), abs(y - m_mousePrsY) );
  painter.drawRect(m_rubberBand);
  
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
  
    if (delta > 0){
      shiftUp();
    }else if (delta < 0){
      shiftDown();
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

void drawPoly::pixelToWorldCoords(int px, int py,
                                  double & wx, double & wy){

  wx = (px - m_padX)/m_pixelSize + m_viewXll;
  wy = (py - m_padY)/m_pixelSize + m_viewYll;
  
}

void drawPoly::worldToPixelCoords(double wx, double wy,
                                  int & px,  int & py){

  px = iround((wx - m_viewXll)*m_pixelSize + m_padX);
  py = iround((wy - m_viewYll)*m_pixelSize + m_padY);
  
}

void drawPoly::paintEvent( QPaintEvent * E){

  // Instead of drawing on the screen right away, draw onto
  // a cache, then display the cache. We'll use the cache
  // later to avoid repainting when the view does not change.
  m_screenRect = E->rect();
  QSize expandedSize = m_screenRect.size().expandedTo(m_cache.size());
  m_cache.resize(expandedSize);
  m_cache.fill(this, m_screenRect.topLeft());

  QPainter paint( &m_cache, this );
  paint.translate(-m_screenRect.x(), -m_screenRect.y());
  showPoly( &paint );

  // Copy the buffer to the screen
  bitBlt(this, m_screenRect.x(), m_screenRect.y(), &m_cache, 0, 0,
         m_screenRect.width(), m_screenRect.height()
         );

  return;
}

void drawPoly::expandBoxToGivenRatio(// inputs
                                     double screenRatio, 
                                     // inputs/outputs
                                     double & xll,  double & yll,
                                     double & widx, double & widy){
                           
  // Expand the given box to have the same aspect ratio as the screen.
  assert(widx > 0.0 && widy > 0.0 && screenRatio > 0.0);
  double nwidx = widx, nwidy = widy;
  if (widy/widx <= screenRatio) nwidy = widx*screenRatio;
  else                          nwidx = widy/screenRatio;

  // Sanity checks
  double tol = 1.0e-3;
  bool check = ( nwidx >= widx*(1 - tol) && nwidy >= widy*(1 - tol)
                 && abs(nwidy/nwidx - screenRatio) < tol*screenRatio );
  if (!check){
    cout << "ERROR!" << endl;
    cout << "widx widy are "   << widx  << ' ' << widy  << endl;
    cout << "nwidx nwidy are " << nwidx << ' ' << nwidy << endl;
    cout << "Screen ratio is " << screenRatio << endl;
    cout << "|nwidy/nwidx - screenRatio| = " << abs(nwidy/nwidx - screenRatio) << endl;
    cout << "Max allowed error is " << 1.0e-5*screenRatio << endl;
  }
  assert(check);

  // Make the new bounding box have the same center as the old one
  xll += widx/2.0 - nwidx/2.0;
  yll += widy/2.0 - nwidy/2.0;

  // Overwrite the previous box
  widx = nwidx; 
  widy = nwidy;

  return;
}

void drawPoly::setUpViewBox(// inputs
                            const std::vector<dPoly> & polyVec,
                            // outputs
                            double & xll, double & yll,
                            double &widx, double & widy){

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
    xll = 0.0; yll = 0.0; xur = 1.0; yur = 1.0;
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

  drawVertIndex = max(0, drawVertIndex);

  int len = 3*(drawVertIndex+1); // To distinguish better points on top of each other
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

void drawPoly::toggleAnno(){
  m_showAnnotations = !m_showAnnotations;
  m_showVertIndices = false; // To be able to show the annotations
  update();
}

void drawPoly::toggleVertIndices(){
  m_showVertIndices = !m_showVertIndices;
  m_showAnnotations = false;
  update();
}

void drawPoly::toggleFilled(){
  m_showFilledPolys = !m_showFilledPolys;
  update();
}

void drawPoly::toggleAddPoly(){
  

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
  
  m_polyVecStack.push_back(m_polyVec); // So that we can undo later
  
  dRect H = m_highlights[numH - 1];

  dPoly clipedPoly;
  for (int vecIter = 0; vecIter < (int)m_polyVec.size(); vecIter++){

    m_polyVec[vecIter].clipPoly(//inuts
                                H.left(), H.top(),
                                H.right(), H.bottom(),
                                // output
                                clipedPoly
                                );

    m_polyVec[vecIter] = clipedPoly;    
  }

  m_highlights.resize(numH - 1);
  m_actions.push_back(m_cutToHlt);
  
  update();
  
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
    
  }else if (lastAction == m_cutToHlt){
  
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

void drawPoly::initOpenPoly(){

  int numFiles = m_polyFilesVec.size();
  m_polyVec.resize(numFiles);
  
  for (int fileIter = 0; fileIter < numFiles; fileIter++){
    
    readOnePoly(// inputs
                m_polyFilesVec[fileIter], m_plotPointsOnlyVec[fileIter],
                // output
                m_polyVec[fileIter]
                );
    
  }

  return;
}

void drawPoly::openPoly(){

  QString s = QFileDialog::getOpenFileName(
                                           QDir::currentDirPath(),
                                           "(*.xg *.ly*)",
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
  
  if ( ! poly.read_poly(filename.c_str(), plotPointsOnly) ){
    exit(1);
  }
  
  // Flip the polygons to compensate for Qt's origin
  // being in the upper-right corner
  double * xv = (double*)poly.get_xv();
  double * yv = (double*)poly.get_yv();
  int numV    = poly.get_totalNumVerts();
  for (int s = 0; s < numV; s++){
    xv[s] = xv[s];
    yv[s] = m_yFactor*yv[s];
  }
  
  // Flip the annotations as well
  vector<anno> annotations;
  poly.get_annotations(annotations);
  for (int s = 0; s < (int)annotations.size(); s++){
    annotations[s].y *= m_yFactor;
  }
  poly.set_annotations(annotations);
  
  return;
}

  
void drawPoly::savePoly(){

  if (m_polyVec.size() == 0){
    cerr << "No polygons to save" << endl;
  }

  char * fileName = "out.xg";
  cout << "Will save to " << fileName << endl;

  dPoly poly;

  for (int polyIter = 0; polyIter < (int)m_polyVec.size(); polyIter++){
    poly.appendPolygons(m_polyVec[polyIter]); 
  }

  // To do: the operation below should be done inside of the dPoly class
  double * yv  = (double*)poly.get_yv(); 
  int numVerts = poly.get_totalNumVerts();
  for (int s = 0; s < numVerts; s++){
    yv[s] *= m_yFactor; // To compensate for Qt's origin in the ul corner
  }
  
  vector<anno> annotations; poly.get_annotations(annotations);
  for (int s = 0; s < (int)annotations.size(); s++){
    annotations[s].y *= m_yFactor;
  }
  poly.set_annotations(annotations);

  poly.write_poly(fileName);

  return;
}

void drawPoly::togglePE(){

  m_toggleShowPointsEdges = m_toggleShowPointsEdges%3 + 1;
  update();
  
}

void drawPoly::toggleOrder(){

  m_showInReverseOrder = !m_showInReverseOrder;
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

