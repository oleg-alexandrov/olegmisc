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
                    int yFactor
                    ):
  QWidget(parent, name), m_yFactor(yFactor) {

  m_polyVec = polyVec;
  
  resetTransformSettings();

  // int
  m_screenXll  = 0;   m_screenYll  = 0;
  m_screenWidX = 0;   m_screenWidY = 0;
  m_windowXll  = 0;   m_windowYll  = 0;
  m_windowWidX = 0;   m_windowWidY = 0;

  // double
  m_undefined    = 1.0e+100;
  m_viewXll      = 0.0; m_viewYll  = 0.0;
  m_viewWidX     = 0.0; m_viewWidY = 0.0;
  m_prevClickedX = m_undefined;
  m_prevClickedY = m_undefined;
}

void drawPoly::resetTransformSettings(){
  m_scale     = 1.0;
  m_shiftX    = 0.0; m_shiftY    = 0.0;
  m_mousePrsX = 0;   m_mousePrsY = 0;
  m_mouseRelX = 0;   m_mouseRelY = 0;
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
    if (m_prevClickedX != m_undefined || m_prevClickedY != m_undefined){
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
    
    m_prevClickedX = wx;
    m_prevClickedY = wy;

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
  m_scale = 2.0;
  update();
}

void drawPoly::zoomOut(){
  resetTransformSettings();
  m_scale = 0.5;
  update();
}

void drawPoly::shiftRight(){
  resetTransformSettings();
  m_shiftX = -min(m_screenWidX, m_screenWidY)/4.0;
  update();
}

void drawPoly::shiftLeft(){
  resetTransformSettings();
  m_shiftX = min(m_screenWidX, m_screenWidY)/4.0;
  update();
}

void drawPoly::shiftUp(){
  resetTransformSettings();
  m_shiftY = min(m_screenWidX, m_screenWidY)/4.0;
  update();
}

void drawPoly::shiftDown(){
  resetTransformSettings();
  m_shiftY = -min(m_screenWidX, m_screenWidY)/4.0;
  update();
}

void drawPoly::pixelToWorldCoords(double px, double py,
                                  double & wx, double & wy){

  wx = m_windowWidX*double(px - m_viewXll)/double(m_viewWidX) + m_windowXll;
  wy = m_windowWidY*double(py - m_viewYll)/double(m_viewWidY) + m_windowYll;
}


void drawPoly::paintEvent( QPaintEvent * ){
  QPainter paint( this );
  showPoly( &paint );
}

void drawPoly::showPoly( QPainter *paint ){

  //paint->setRasterOp(Qt::XorROP);
  
               
  // Screen dimensions
  QRect v = paint->viewport();
  m_screenXll  = v.left();
  m_screenYll  = v.top();
  m_screenWidX = v.width();
  m_screenWidY = v.height();
  cout << "Screen is " << m_screenXll << ' ' << m_screenYll << ' '
       << m_screenWidX << ' ' << m_screenWidY << endl;

  // Poly dimensions
  double big = DBL_MAX;
  double xll = big, yll = big, xur = -big, yur = -big;
  for (int p = 0; p < (int)m_polyVec.size(); p++){
    double xll0, yll0, xur0, yur0;
    m_polyVec[p].bdBox(xll0, yll0, xur0, yur0);
    xll = min(xll, xll0); xur = max(xur, xur0);
    yll = min(yll, yll0); yur = max(yur, yur0);
  }
  if (xur < xll || yur < yll){
    // All polygons are empty
    xll = 0.0; yll = 0.0; xur = 1.0; yur = 1.0;
  }
  
  double widx = xur - xll; assert(widx >= 0.0);
  if (widx == 0.0){ xll = 0.0; widx = 1.0; }
  double widy = yur - yll; assert(widy >= 0.0);
  if (widy == 0.0){ yll = 0.0; widy = 1.0; }

  cout << "Bd box is "
       << xll  << ' ' << yll << ' ' << xur << ' ' << yur << endl;
  
  double tol = 0.1; // percentage by which to pad the view
  int blen   = (int)ceil(max(widx, widy));
  double pad = tol*blen;
  double len = min(m_screenWidX, m_screenWidY);
  m_windowXll  = iround(xll + 0.5*(widx - blen) - pad);
  m_windowYll  = iround(yll + 0.5*(widy - blen) - pad);
  m_windowWidX = iround(blen + 2*pad);
  m_windowWidY = iround(blen + 2*pad);
  paint->setWindow(m_windowXll,  m_windowYll,
                   m_windowWidX, m_windowWidY
                   );
  
//   cout << "Window is " << m_windowXll << ' ' << m_windowYll << ' '
//        << m_windowWidX << ' ' << m_windowWidY << endl;

  if (m_viewWidX == 0 || m_viewWidY == 0){
    
    // This should be reached only when the window is created
    m_viewXll  = m_screenXll + 0.5*(m_screenWidX - len);
    m_viewYll  = m_screenYll + 0.5*(m_screenWidY - len);
    m_viewWidX = len;
    m_viewWidY = len;

  }else if (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    
    // Zoom in to selected highlight
    
    double mlen = max(m_mouseRelX - m_mousePrsX,
                   m_mouseRelY - m_mousePrsY
                   );
    assert(mlen > 0);
    double vlen = min(m_viewWidX, m_viewWidY);
    assert(vlen > 0);
    double slen = min(m_screenWidX, m_screenWidY);
    assert(slen > 0);
    
    double scale = double(slen)/double(mlen);
    
    m_viewXll  = (m_viewXll - m_mousePrsX)*scale;
    m_viewYll  = (m_viewYll - m_mousePrsY)*scale;
    m_viewWidX = max(vlen*scale, 2.0);
    m_viewWidY = max(vlen*scale, 2.0);
    
  }else{
    
    // shift or zoom
    
    double len = min(m_viewWidX, m_viewWidY);
    assert(len > 0);
    m_viewXll  = m_viewXll*m_scale
      + (m_screenXll + m_screenWidX/2.0)*(1.0 - m_scale) + m_shiftX;
    m_viewYll  = m_viewYll*m_scale
      + (m_screenYll + m_screenWidY/2.0)*(1.0 - m_scale) + m_shiftY;
    m_viewWidX = len * m_scale;
    m_viewWidY = len * m_scale;
    
  }

  // Having computed the new view reset the numbers used to manipulate
  // it so that we can start fresh with new manipulations next time
  // (but starting from the newly computed view).
  resetTransformSettings();

  paint->setViewport(ifloor(m_viewXll), ifloor(m_viewYll),
                     iceil(m_viewWidX), iceil(m_viewWidY)
                     );
  cout << "Viewpoint is " << m_viewXll << ' ' << m_viewYll << ' '
       << m_viewWidX << ' ' << m_viewWidY << endl;

  paint->setBrush( NoBrush );        // do not fill

  // Clip the polygons to a window slightly bigger than what is seen
  // on the screen to avoid a Qt bug with zoom in.
  double clipPad = 50; // pixels
  double clip_xll, clip_yll, clip_xur, clip_yur;
  pixelToWorldCoords(m_screenXll - clipPad, m_screenYll - clipPad,
                     clip_xll, clip_yll);
  pixelToWorldCoords(m_screenXll + m_screenWidX + clipPad,
                     m_screenYll + m_screenWidY + clipPad,
                     clip_xur, clip_yur);

  // If the physical box filling the screen is less than
  // smallBoxSize, then snapping to integer is not accurate
  // enough. Snap to a fraction of integer.
  double smallBoxSize = 1000.0;
  double boxSize = min(clip_xur - clip_xll, clip_yur - clip_yll);
  assert(boxSize > 0.0);
  double snapScale = 1.0;
  while (boxSize/snapScale < smallBoxSize){
    snapScale *= 0.1;
  }
  paint->setWindow(iround(m_windowXll/snapScale),
                   iround(m_windowYll/snapScale),
                   iround(m_windowWidX/snapScale),
                   iround(m_windowWidY/snapScale)
                   );


  // Plot the polygons
  int lineWidth = 1;
  for (int vecIter  = 0; vecIter < (int)m_polyVec.size(); vecIter++){
    
    xg_poly clipPoly;
    m_polyVec[vecIter].clipPoly(//inuts
                                clip_xll, clip_yll, clip_xur, clip_yur,
                                // output
                                clipPoly
                                );
    
    const double * xv           = clipPoly.get_xv();
    const double * yv           = clipPoly.get_yv();
    const int    * numVerts     = clipPoly.get_numVerts();
    int numPolys                = clipPoly.get_numPolys();
    const vector<string> colors = clipPoly.get_colors();
    //int numVerts              = clipPoly.get_totalNumVerts();

    int start = 0;
    for (int pIter = 0; pIter < numPolys; pIter++){
    
      if (pIter > 0) start += numVerts[pIter - 1];
      QColor color = QColor( colors[pIter].c_str() );
      paint->setPen( QPen(color, lineWidth) );

      int pSize = numVerts[pIter];
      QPointArray pa(pSize);
      for (int vIter = 0; vIter < pSize; vIter++){
        int x0 = iround(xv[start + vIter]/snapScale);
        int y0 = iround(yv[start + vIter]/snapScale);
        pa[vIter] = QPoint(x0, y0);
        
//         QFont F;
//         //int fs = max(10, min(iround(2000/boxSize), 1000));
//         int fs = 100;
//         cout << "Font size is " << fs << endl;
//         //F.setPixelSize(fs);
//         F.setPointSize(fs);
//         paint->setFont(F);
//         paint->drawText(x0, y0, 100, 100, Qt::AlignCenter, "z");
      }

      paint->drawPolygon( pa );
    }

  }

  cout << "Clip box: " << clip_xll << ' ' << m_yFactor*clip_yll
       << ' ' << clip_xur - clip_xll << ' ' << clip_yur - clip_yll << endl;

  // Plot the clipping box
  int numV = 4;
  double xv[] = {clip_xll, clip_xur, clip_xur, clip_xll};
  double yv[] = {clip_yll, clip_yll, clip_yur, clip_yur};
  QPointArray pa(numV);
  for (int vIter = 0; vIter < numV; vIter++){
    int x0 = iround(xv[vIter]/snapScale);
    int y0 = iround(yv[vIter]/snapScale);
    pa[vIter] = QPoint(x0, y0);
  }
  paint->setPen( QPen("white", lineWidth) );
  paint->drawPolygon( pa );

  return;
}


