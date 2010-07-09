#ifndef DRAWPOLY_H
#define DRAWPOLY_H

#include <qpopupmenu.h>
#include <qmainwindow.h>
#include <qintdict.h>
#include <qcanvas.h>
#include <qwidget.h>
#include <qdatetime.h>
#include <vector>
#include "utils.h"
#include "../../polyUtils/xg_poly.h"

class drawPoly : public QWidget
{
    Q_OBJECT
public:
    drawPoly( QWidget *parent, const char *name,
              const std::vector<xg_poly> & polyVec,
              int yFactor
              );
  void zoomIn();
  void zoomOut();
  void shiftRight();
  void shiftLeft();
  void shiftUp();
  void shiftDown();
  
public slots:

protected:

  void paintEvent( QPaintEvent * );
  void mousePressEvent( QMouseEvent *E);
  void mouseMoveEvent( QMouseEvent *);
  void keyPressEvent( QKeyEvent *k );
  void mouseReleaseEvent ( QMouseEvent * E );
  void wheelEvent(QWheelEvent *event);

private slots:

private:

  void showPoly( QPainter *paint );
  void resetTransformSettings();
  void pixelToWorldCoords(double   px, double   py,
                          double & wx, double & wy);
  static void expandBoxToGivenRatio(// inputs
                                    double screenRatio, 
                                    // inputs/outputs
                                    double & xll,  double & yll,
                                    double & widx, double & widy);
  
  static void setUpViewBox(// inputs
                           double screenRatio, 
                           const std::vector<xg_poly> & polyVec,
                           // outputs
                           double & xll, double & yll,
                           double &widx, double & widy);
  
  double m_zoomFactor, m_shiftX, m_shiftY;
  int m_mousePrsX,  m_mousePrsY, m_mouseRelX,  m_mouseRelY;
  int m_screenXll,  m_screenYll, m_screenWidX, m_screenWidY;
  double m_viewXll, m_viewYll,   m_viewWidX,   m_viewWidY;
  double m_prevClickedX, m_prevClickedY;
  double m_screenRatio;
  
  // Transform the polygons to the device coordinate system
  double m_pixelSize, m_padX, m_padY;
  
  std::vector<xg_poly> m_polyVec;
  int m_yFactor; // To compensate for Qt's origin in the upper-left corner

  QRect m_rubberBandRect;
  bool m_firstPaintInstance;
  bool m_prevClickExists;
  
};

#endif // DRAWPOLY_H

