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
#include "../../polyUtils/geomclipandmerge.h"
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
  m_screenXll  = 0;   m_screenYll  = 0;
  m_screenWidX = 0;   m_screenWidY = 0;

  // double
  m_viewXll      = 0.0; m_viewYll  = 0.0;
  m_viewWidX     = 0.0; m_viewWidY = 0.0;
  m_firstPaintInstance = true;
  m_prevClickExists    = false;
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
    
  if       (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    update(); // Will zoom to the region selected with the mouse
  }else if (m_mouseRelX < m_mousePrsX && m_mouseRelY < m_mousePrsY ){
    zoomOut();
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

  }
  
  return;
}

void drawPoly::mouseMoveEvent( QMouseEvent *){
#if 0
  cout << "Mouse moved" << endl;
  QPainter painter(this);
  painter.setRasterOp(Qt::XorROP);
  painter.setPen(Qt::white);
  //painter.setRasterOp(Qt::NotROP);
  //painter.drawRect(m_rubberBandRect.normalize());
  QRect rect = m_rubberBandRect.normalize();
  update(rect.left(), rect.top(), rect.width(), 1);                      
  update(rect.left(), rect.top(), 1, rect.height());                     
  update(rect.left(), rect.bottom(), rect.width(), 1);                   
  update(rect.right(), rect.top(), 1, rect.height());                    
#endif
}

void drawPoly::wheelEvent(QWheelEvent *event){
  
  int delta = event->delta();

  if (delta > 0){
    shiftUp();
  }else if (delta < 0){
    shiftDown();
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

void drawPoly::paintEvent( QPaintEvent * ){
  QPainter paint( this );
  showPoly( &paint );
}

void drawPoly::expandBoxToGivenRatio(// inputs
                                     double screenRatio, 
                                     // inputs/outputs
                                     double & xll,  double & yll,
                                     double & widx, double & widy){
                           
  // Expand the bounding box to have the same aspect ratio as the screen.
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
                            double screenRatio, 
                            const std::vector<xg_poly> & polyVec,
                            // outputs
                            double & xll, double & yll,
                            double &widx, double & widy){

  // Given a set of polygons, set up a box containing these polygons
  // with the same aspect ratio as the screen

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
  
  expandBoxToGivenRatio(screenRatio,             // input
                        xll, yll, widx, widy     // in/out
                        );
 
  xur = xll + widx;
  yur = yll + widy;

  return;
  
}

void drawPoly::showPoly( QPainter *paint ){

  //paint->setRasterOp(Qt::XorROP);
               
  // Screen dimensions
  QRect v       = paint->viewport();
  m_screenXll   = v.left();
  m_screenYll   = v.top();
  m_screenWidX  = v.width();
  m_screenWidY  = v.height();
  m_screenRatio = double(m_screenWidY)/double(m_screenWidX);
  //cout << "Screen is " << m_screenXll << ' ' << m_screenYll << ' '
  //     << m_screenWidX << ' ' << m_screenWidY << endl;

  // To have the polygon show up a bit inside the screen use some padding
  m_padX = 0.0; m_padY = m_screenRatio*m_padX; // Units are pixels

  if (m_firstPaintInstance){
    setUpViewBox(// inputs
                 m_screenRatio, m_polyVec,
                 // outputs
                 m_viewXll, m_viewYll, m_viewWidX, m_viewWidY
                 );
    m_firstPaintInstance = false;
  }
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
  F.setPointSize(fontSize);  //F.setPixelSize(fontSize);
  paint->setFont(F);

  // Draw the polygons
  for (int vecIter  = 0; vecIter < (int)m_polyVec.size(); vecIter++){

    bool plotVertsOnly = m_plotVertsOnlyVec[vecIter];
    
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
      paint->setPen( QPen(color, lineWidth) );

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
        // draw a small circle.
        int w = 2;
        paint->setBrush( color );
        if (plotVertsOnly){
          paint->drawEllipse(x0 - w, y0 - w, 2*w, 2*w);
        }
        
      }

      paint->setBrush( NoBrush );        // do not fill
      if (!plotVertsOnly){
        paint->drawPolygon( pa );
      }
      
    }

    // Plot the annotations
    for (int aIter = 0; aIter < (int)annotations.size(); aIter++){
      const anno & A = annotations[aIter];
      int x0, y0;
      worldToPixelCoords(A.x, A.y, // inputs
                         x0, y0    // outputs
                         );
      paint->setPen( QPen("gold", lineWidth) );
      paint->drawText(x0, y0, A.label);
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
   paint->setPen( QPen("white", lineWidth) );
   paint->drawPolygon( pa );
   
//    cout << "view is: " << m_viewXll << ' ' << m_viewYll
//         << ' ' << m_viewWidX << ' ' << m_viewWidY << endl;

   return;
}


