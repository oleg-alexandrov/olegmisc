#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include <qapplication.h>
#include <qbitmap.h>
#include <qdatetime.h>
#include <qimage.h>
#include <qlabel.h>
#include <qmainwindow.h>
#include <qmenubar.h>
#include <qmessagebox.h>
#include <qpainter.h>
#include <qprinter.h>
#include <qprogressdialog.h>
#include <qstatusbar.h>
#include <qtimer.h>
#include <qevent.h>
#include "drawpoly.h"
#include <iomanip>   // required for use of setw()
#include <cfloat>    // defines DBL_MAX
using namespace std;

inline int iround(double x){ return (int)round(x); }
inline int iceil (double x){ return (int)ceil( x); }
inline int ifloor(double x){ return (int)floor(x); }
inline int isign (double x){
  if (x > 0) return  1;
  if (x < 0) return -1;
  return 0;
}

drawPoly::drawPoly( QWidget *parent, const char *name,
                    const std::vector<xg_poly> & polyVec,
                    const std::vector<bool>    & plotVertsOnlyVec,
                    int yFactor
                    ):
  QWidget(parent, name), m_yFactor(yFactor) {

  m_polyVec          = polyVec;
  m_plotVertsOnlyVec = plotVertsOnlyVec;
  
  resetTransformSettings();

  // int
  m_screenXll  = 0; m_screenYll  = 0;
  m_screenWidX = 0; m_screenWidY = 0;

  // double
  m_viewXll         = 0.0; m_viewYll  = 0.0;
  m_viewWidX        = 0.0; m_viewWidY = 0.0;
  m_resetView       = true;
  m_prevClickExists = false;
  m_showAnnotations = true;
  
  m_rubberBand      = QRect( 0, 0, 0, 0); // initial rubberband
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

//   cout << "Mouse pressed at "
//        << m_mousePrsX << ' ' << m_mousePrsY << endl;
//   cout << "Mouse released at "
//        << m_mouseRelX << ' ' << m_mouseRelY << endl;

  // Wipe the previous rubberband, if non-empty
  if (m_rubberBand.width() > 0 || m_rubberBand.height() > 0){
    QPainter painter(this);
    wipeRubberBand(&painter, m_rubberBand);
  }
  m_rubberBand = QRect( m_mouseRelX, m_mouseRelY, 0, 0); //empty rect
  
  // Pressed mid-button enables left/right/up/down navigation
  const ButtonState button = E->button();
  if (button == Qt::MidButton){

    if (abs(m_mouseRelX - m_mousePrsX) > abs(m_mouseRelY - m_mousePrsY) ){

      if ( m_mouseRelX - m_mousePrsX > 0){
        shiftLeft();
      }else{
        shiftRight();
      }
      
    }else{
      
      if ( m_mouseRelY - m_mousePrsY > 0){
        shiftUp();
      }else{
        shiftDown();
      }
      
    }
    return;
  }

  // Zoom to selection if the mouse went down and right,
  // zoom out if the mouse went up and left, and print
  // the current coordinates otherwise.
  if       (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    update(); // Will zoom to the region selected with the mouse
    return;
  }else if (m_mouseRelX < m_mousePrsX && m_mouseRelY < m_mousePrsY ){
    zoomOut();
    return;
  }else if (m_mouseRelX == m_mousePrsX && m_mouseRelY == m_mousePrsY){

    // Print the physcal coordinates of the point the mouse was clicked at
    int prec = 6, wid = prec + 6;
    cout.precision(prec);
    
    double wx, wy;
    pixelToWorldCoords(m_mouseRelX, m_mouseRelY, wx, wy);
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
  
  return;
}

void drawPoly::wipeRubberBand(QPainter * paint, QRect & rubberBand){
  
  // Wipe the current rubberband by overwriting the region it occupies
  // (a set of four segments forming a rectangle) with the cached
  // version of image before the rubberband was drawn.
  QRect R   = rubberBand;
  int left  = min(R.left(), R.right());
  int right = max(R.left(), R.right());
  int top   = min(R.top(), R.bottom());
  int bot   = max(R.top(), R.bottom());
  int wd    = R.width();
  int ht    = R.height();
  int px    = 1; 
  paint->drawPixmap (left,  top, m_cache, left,  top, wd, px);
  paint->drawPixmap (left,  top, m_cache, left,  top, px, ht);
  paint->drawPixmap (left,  bot, m_cache, left,  bot, wd, px);
  paint->drawPixmap (right, top, m_cache, right, top, px, ht);

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

  // Wipe the previous rubberband
  wipeRubberBand(&painter, m_rubberBand);
  
  // Create the new rubberband
  QRect rubberBand( m_mousePrsX, m_mousePrsY,
                    x - m_mousePrsX, y - m_mousePrsY );
  painter.drawRect(rubberBand);
  m_rubberBand = rubberBand; // Save this for the future
  
}

void drawPoly::wheelEvent(QWheelEvent *event){

  int delta = event->delta();

  ButtonState state = event->state();

  if (state == Qt::ControlButton){

    // The control button was pressed. Zoom in/out around the current point.

    int pixelx = event->x();
    int pixely = event->y();
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
  
  event->accept();
}

void drawPoly::keyPressEvent( QKeyEvent *k ){

  switch ( k->key() ) {
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
  double nwidx = widx, nwidy = widy;
  if (widy/widx <= screenRatio) nwidy = widx*screenRatio;
  else                          nwidx = widy/screenRatio;
  assert(nwidx >= widx && nwidy >= widy
         && abs(nwidy/nwidx - screenRatio) < 1.0e-5*screenRatio);

  // Make the new bounding box have the same center as the old one
  xll += widx/2.0 - nwidx/2.0;
  yll += widy/2.0 - nwidy/2.0;

  // Overwrite the previous box
  widx = nwidx; 
  widy = nwidy;

  return;
}

void drawPoly::setUpViewBox(// inputs
                            const std::vector<xg_poly> & polyVec,
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
  
  expandBoxToGivenRatio(m_screenRatio,                               // input
                        m_viewXll, m_viewYll, m_viewWidX, m_viewWidY // in/out
                        );
  
//   paint->setWindow(m_screenXll,  m_screenYll,
//                    m_screenWidX, m_screenWidY
//                    );
//   paint->setViewport(m_screenXll,  m_screenYll,
//                      m_screenWidX, m_screenWidY
//                      );

  if (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    
    // Form a new view based on the rectangle selected with the mouse.
    // Enlarge this rectangle if necessary to keep the aspect ratio.

    // The call to pixelToWorldCoords uses the existing view internally
    double xll, yll, xur, yur;
    pixelToWorldCoords(m_mousePrsX, m_mousePrsY, xll, yll);
    pixelToWorldCoords(m_mouseRelX, m_mouseRelY, xur, yur);
    
    double widx = xur - xll;
    double widy = yur - yll;

    expandBoxToGivenRatio(//inputs
                          m_screenRatio,  
                          // input/outputs
                          xll, yll, widx, widy
                          );

    // Overwrite the view
    m_viewXll = xll; m_viewWidX = widx;
    m_viewYll = yll; m_viewWidY = widy;
    
  }else{

    // Modify the view for given shift or zoom
    m_viewXll  += m_viewWidX*( (1 - m_zoomFactor)/2.0 + m_shiftX );
    m_viewYll  += m_viewWidY*( (1 - m_zoomFactor)/2.0 + m_shiftY );
    m_viewWidX *= m_zoomFactor;
    m_viewWidY *= m_zoomFactor;
    
  }

  // Having computed the new view reset the numbers used to manipulate
  // it so that we can start fresh with new manipulations next time
  // (but starting from the newly computed view).
  resetTransformSettings();
  
  m_pixelSize = (m_screenWidX - 2*m_padX)/m_viewWidX;
  assert( abs(m_pixelSize - (m_screenWidY - 2*m_padY)/m_viewWidY)
           < 1.0e-5*m_pixelSize );

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
  for (int vecIter  = 0; vecIter < (int)m_polyVec.size(); vecIter++){

    bool plotVertsOnly = m_plotVertsOnlyVec[vecIter];
    if (plotVertsOnly) drawVertIndex++;

    xg_poly clipPoly;
    m_polyVec[vecIter].clipPoly(//inuts
                                m_viewXll,  m_viewYll,
                                m_viewXll + m_viewWidX,
                                m_viewYll + m_viewWidY,
                                // output
                                clipPoly
                                );
    
    const double * xv              = clipPoly.get_xv();
    const double * yv              = clipPoly.get_yv();
    const int    * numVerts        = clipPoly.get_numVerts();
    int numPolys                   = clipPoly.get_numPolys();
    const vector<string> colors    = clipPoly.get_colors();
    const vector<anno> annotations = clipPoly.get_annotations();
    //int numVerts                 = clipPoly.get_totalNumVerts();

    int start = 0;
    for (int pIter = 0; pIter < numPolys; pIter++){
    
      QColor color = QColor( colors[pIter].c_str() );

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
        if (plotVertsOnly &&
            x0 > m_screenXll && x0 < m_screenXll + m_screenWidX && 
            y0 > m_screenYll && y0 < m_screenYll + m_screenWidY
            ){
          drawOneVertex(x0, y0, color, lineWidth, drawVertIndex, paint);
        }
      }
      
      if (!plotVertsOnly){
        paint->setBrush( NoBrush );
        paint->setPen( QPen(color, lineWidth) );
        paint->drawPolygon( pa );
      }
      
    }

    // Plot the annotations
    if (m_showAnnotations){
      
      int numAnno = annotations.size();
      if (numAnno > 2000){
        //cout << "Too many annotations, zoom in to see them" << endl;
      }else{
        for (int aIter = 0; aIter < numAnno; aIter++){
          const anno & A = annotations[aIter];
          int x0, y0;
          worldToPixelCoords(A.x, A.y, // inputs
                             x0, y0    // outputs
                             );
          paint->setPen( QPen("gold", lineWidth) );
          paint->drawText(x0, y0, A.label);
        }
      }
    }
    
  }

  // Plot the view box
  int numV = 4;
  double xv[] = {m_viewXll, m_viewXll + m_viewWidX,
                 m_viewXll + m_viewWidX, m_viewXll};
  double yv[] = {m_viewYll, m_viewYll, m_viewYll + m_viewWidY,
                 m_viewYll + m_viewWidY};
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
   
//    cout << "view is: " << m_viewXll << ' ' << m_viewYll
//         << ' ' << m_viewWidX << ' ' << m_viewWidY << endl;

   return;
}


void drawPoly::drawOneVertex(int x0, int y0, QColor color, int lineWidth,
                             int drawVertIndex, QPainter * paint){

  int len = 6;
  paint->setPen( QPen(color, lineWidth) );

  int numTypes = 4;
  if (drawVertIndex%numTypes == 0){
    
    // Draw a small filled ellipse
    paint->setBrush( color );
    paint->drawEllipse(x0 - len/2, y0 - len/2, len, len);
    
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
    len = int(len*1.5 + 0.5);
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
  update();
}
