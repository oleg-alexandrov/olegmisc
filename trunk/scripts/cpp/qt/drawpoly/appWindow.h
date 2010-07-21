#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <qpopupmenu.h>
#include <qmainwindow.h>
#include <qintdict.h>
#include <qcanvas.h>
#include "drawpoly.h"
class dPoly;


class appWindow : public QMainWindow {
  Q_OBJECT

public:
  appWindow(QWidget* parent, const char* name,
            const std::vector<dPoly> & polyVec,
            const std::vector<bool>    & plotPointsOnlyVec,
            int yFactor,
            int widX, int widY, WFlags f=0);
  ~appWindow();
  
public slots:
void help();
  
private slots:
  void createMenus       ();
  void zoomIn            ();
  void zoomOut           ();
  void shiftRight        ();
  void shiftLeft         ();
  void shiftUp           ();
  void shiftDown         ();
  void resetView         ();
  void toggleAnno        ();
  void toggleVertIndices ();
  void toggleFilled      ();
  void cutToHlt          ();
  void undoLast          ();
  void savePoly          ();
  void togglePE          ();
  void toggleOrder       ();
  
private:
  drawPoly   * m_poly;
  QPopupMenu * options;
  QPrinter   * printer;
  int dbf_id;
  std::string m_progName;
};

#endif
