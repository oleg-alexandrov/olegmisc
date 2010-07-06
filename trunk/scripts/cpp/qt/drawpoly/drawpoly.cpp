#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include "drawpoly.h"
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
using namespace std;

inline int iround(double x){ return (int)round(x); }
inline int iceil (double x){ return (int)ceil( x); }
inline int ifloor(double x){ return (int)floor(x); }

drawPoly::drawPoly( QWidget *parent, const char *name,
                    const std::vector<xg_poly> & polyVec,
                    int yFactor
                    ):
  QWidget(parent, name), m_yFactor(yFactor) {

  m_polyVec = polyVec;
  
  resetTransformSettings();
  
  m_screenXll    = 0; m_screenYll  = 0;
  m_screenWidX   = 0; m_screenWidY = 0;
  m_viewXll      = 0; m_viewYll    = 0;
  m_viewWidX     = 0; m_viewWidY   = 0;
  m_worldXll     = 0; m_worldYll   = 0;
  m_worldWidX    = 0; m_worldWidY  = 0;
  m_undefined    = 1.0e+100;
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

  double wx, wy;
  pixelToWorldCoords(m_mousePrsX, m_mousePrsY, wx, wy);
  cout << "Point: " << wx << ' ' << wy*m_yFactor;
  if (m_prevClickedX != m_undefined || m_prevClickedY != m_undefined){
   cout  << " dist from prev: " << wx - m_prevClickedX << ' '
         << (wy - m_prevClickedY)*m_yFactor
         << " Euclidean: "
         << sqrt( (wx - m_prevClickedX)*(wx - m_prevClickedX) + 
                  (wy - m_prevClickedY)*(wy - m_prevClickedY)
                  );
  }
  cout << endl;
    
  m_prevClickedX = wx;
  m_prevClickedY = wy;
}

void drawPoly::mouseReleaseEvent ( QMouseEvent * E ){

  const QPoint Q = E->pos();
  m_mouseRelX = Q.x();
  m_mouseRelY = Q.y();
//   cout << "Mouse pressed at "
//        << m_mousePrsX << ' ' << m_mousePrsY << endl;
//   cout << "Mouse released at "
//        << m_mouseRelX << ' ' << m_mouseRelY << endl;

  if       (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    update();
  }else if (m_mouseRelX < m_mousePrsX && m_mouseRelY < m_mousePrsY ){
    zoomOut();
  }
  
}

void drawPoly::mouseMoveEvent( QMouseEvent *){
  //cout << "Mouse moved" << endl;
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

void drawPoly::pixelToWorldCoords(int px, int py, double & wx, double & wy){

  wx = m_worldWidX*double(px - m_viewXll)/double(m_viewWidX) + m_worldXll;
  wy = m_worldWidY*double(py - m_viewYll)/double(m_viewWidY) + m_worldYll;
}


void drawPoly::paintEvent( QPaintEvent * ){
  QPainter paint( this );
  showPoly( &paint );
}

void drawPoly::showPoly( QPainter *paint ){

  // Screen dimensions
  QRect v = paint->viewport();
  m_screenXll  = v.left();
  m_screenYll  = v.top();
  m_screenWidX = v.width();
  m_screenWidY = v.height();
  //   cout << "Screen is " << m_screenXll << ' ' << m_screenYll << ' '
  //        << m_screenWidX << ' ' << m_screenWidY << endl;

  // Poly dimensions
  double big = 1e+100;
  double xll = big, yll = big, xur = -big, yur = -big;
  for (int p = 0; p < (int)m_polyVec.size(); p++){
    double xll0, yll0, xur0, yur0;
    m_polyVec[p].bdBox(xll0, yll0, xur0, yur0);
    xll = min(xll, xll0); xur = max(xur, xur0);
    yll = min(yll, yll0); yur = max(yur, yur0);
  }

  double widx = xur - xll; assert(widx >= 0.0);
  if (widx == 0.0){ xll = 0.0; widx = 1.0; }
  double widy = yur - yll; assert(widy >= 0.0);
  if (widy == 0.0){ yll = 0.0; widy = 1.0; }

  cout << "Bd box is "
       << xll  << ' ' << yll << ' ' << xur << ' ' << yur << endl;
  
  double tol = 0.05; // percentage by which to pad the view
  int blen   = (int)ceil(max(widx, widy));
  double pad = tol*blen;
  int len = min(m_screenWidX, m_screenWidY);
    paint->setWindow(iround(xll + 0.5*(widx - blen) - pad),
                     iround(yll + 0.5*(widy - blen) - pad),
                     iround(blen + 2*pad), iround(blen + 2*pad)
                     );
  
  if (m_viewWidX == 0 || m_viewWidY == 0){
    
    // This should be reached only when the window is created
    m_viewXll  = m_screenXll + iround(0.5*(m_screenWidX - len));
    m_viewYll  = m_screenYll + iround(0.5*(m_screenWidY - len));
    m_viewWidX = len;
    m_viewWidY = len;

  }else if (m_mouseRelX > m_mousePrsX && m_mouseRelY > m_mousePrsY){
    
    // Zoom in to selected highlight
    
    int mlen = max(m_mouseRelX - m_mousePrsX,
                   m_mouseRelY - m_mousePrsY
                   );
    assert(mlen > 0);
    int vlen = min(m_viewWidX, m_viewWidY);
    assert(vlen > 0);
    int slen = min(m_screenWidX, m_screenWidY);
    assert(slen > 0);
    
    double scale = double(slen)/double(mlen);
    
    m_viewXll  = ifloor ( (m_viewXll - m_mousePrsX)*scale );
    m_viewYll  = ifloor ( (m_viewYll - m_mousePrsY)*scale );
    m_viewWidX = max( iceil ( vlen*scale ), 2);
    m_viewWidY = max( iceil ( vlen*scale ), 2);
    
  }else{
    
    // shift or zoom
    
    int len = min(m_viewWidX, m_viewWidY);
    assert(len > 0);
    m_viewXll  = ifloor( m_viewXll*m_scale
                         + (m_screenXll + m_screenWidX/2.0)*(1.0 - m_scale) 
                         + m_shiftX);
    m_viewYll  = ifloor( m_viewYll*m_scale
                         + (m_screenYll + m_screenWidY/2.0)*(1.0 - m_scale)
                         + m_shiftY);
    m_viewWidX = iceil( len * m_scale );
    m_viewWidY = iceil( len * m_scale );
    
  }

  resetTransformSettings();

  paint->setViewport(m_viewXll, m_viewYll, m_viewWidX, m_viewWidY);
  cout << "Viewpoint is " << m_viewXll << ' ' << m_viewYll << ' '
       << m_viewWidX << ' ' << m_viewWidY << endl;


  QRect R = paint->window();
  cout << "Window is " << R.left() << ' ' << R.top() << ' '
       << R.width() << ' ' << R.height() << endl;

  m_worldXll  = R.left();
  m_worldYll  = R.top();
  m_worldWidX = R.width();
  m_worldWidY = R.height();
  //assert( m_worldWidX == m_worldWidY);
  
  paint->setBrush( NoBrush );        // do not fill
  //paint.setBrush( QColor("red") );
  //paint->setRenderHint(QPainter::Antialiasing); // Qt4
  //paint->setRenderHint(QPainter::SmoothPixmapTransform);
  // Also for anti-alising of text

  // Plot the polygons
  for (int clipIter  = 0; clipIter < (int)m_polyVec.size(); clipIter++){
    
    const double * xv           = m_polyVec[clipIter].get_xv();
    const double * yv           = m_polyVec[clipIter].get_yv();
    const int    * numVerts     = m_polyVec[clipIter].get_numVerts();
    int numPolys                = m_polyVec[clipIter].get_numPolys();
    const vector<string> colors = m_polyVec[clipIter].get_colors();
    //int numVerts              = m_polyVec[clipIter].get_totalNumVerts();

    int start = 0;
    for (int pIter = 0; pIter < numPolys; pIter++){
    
      if (pIter > 0) start += numVerts[pIter - 1];
    
      int pSize = numVerts[pIter];
      //cout << "Poly size is " << pSize << endl;
    
      QPointArray pa(pSize);
    
      for (int vIter = 0; vIter < pSize; vIter++){
        pa[vIter] = QPoint(iround(xv[start + vIter]),
                           iround(yv[start + vIter])
                           );
//         QBrush brush2( Qt::yellow, Qt::SolidPattern );
//         paint->setBrush( brush2 ); 
//         paint->drawEllipse(pa[vIter].x()-1, pa[vIter].y()+1, 2, 2);
//         paint->setBrush( NoBrush );
      }

      QColor color = QColor(colors[pIter].c_str());
      int lineWidth = 1;
      paint->setPen( QPen(color, lineWidth) );
      paint->drawPolygon( pa );
      //paint->setPen( QPen(color, 5) );
      //paint->drawPoints( pa );

    }

  }
  
  return;
}


