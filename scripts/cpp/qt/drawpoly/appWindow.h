#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <qpopupmenu.h>
#include <qmainwindow.h>
#include <qintdict.h>
#include <qcanvas.h>
#include "drawpoly.h"
class xg_poly;


class appWindow : public QMainWindow {
  Q_OBJECT

public:
  appWindow(QWidget*, QWidget* parent, const char* name,
            const std::vector<xg_poly> & polyVec,
            const std::vector<bool>    & plotVertsOnlyVec,
            int yFactor,
          int widX, int widY, WFlags f=0);
  ~appWindow();
            public slots:
            void help();

             private slots:
             void createMenus();
  void zoomIn();
  void zoomOut();
  void shiftRight();
  void shiftLeft();
  void shiftUp();
  void shiftDown();
  
private:
  drawPoly * m_poly;
  QPopupMenu* options;
  QPrinter* printer;
  int dbf_id;
  
};

#endif
