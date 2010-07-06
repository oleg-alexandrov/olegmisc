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
#include "../../xg_poly.h"

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
  void resetTransformSettings();
  void pixelToWorldCoords(int px, int py, double & wx, double & wy);
  
public slots:

protected:

  void paintEvent( QPaintEvent * );
  void mousePressEvent( QMouseEvent *E);
  void mouseMoveEvent( QMouseEvent *);
  void keyPressEvent( QKeyEvent *k );
  void mouseReleaseEvent ( QMouseEvent * E );
  void wheelEvent(QWheelEvent *event);

private slots:
void showPoly( QPainter *paint );

private:
  
  double m_scale , m_shiftX,    m_shiftY;
  int m_mousePrsX, m_mousePrsY, m_mouseRelX,  m_mouseRelY;
  int m_viewXll,   m_viewYll,   m_viewWidX,   m_viewWidY;
  int m_worldXll,  m_worldYll,  m_worldWidX,  m_worldWidY;
  int m_screenXll, m_screenYll, m_screenWidX, m_screenWidY;
  double m_prevClickedX, m_prevClickedY, m_undefined;
  
  std::vector<xg_poly> m_polyVec; // put back the reference!
  int m_yFactor; // To compensate for Qt's origin in the upper-left corner

};

#endif // DRAWPOLY_H

