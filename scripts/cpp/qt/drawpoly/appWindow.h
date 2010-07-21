#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <qpopupmenu.h>
#include <qmainwindow.h>
#include <qintdict.h>
#include <qcanvas.h>
#include "drawpoly.h"

class appWindow : public QMainWindow {
  Q_OBJECT

public:
  appWindow(QWidget* parent, const char* progName,
            const std::vector<std::string> & polyFilesVec,
            const std::vector<bool>        & plotPointsOnlyVec);
  ~appWindow();
  
public slots:
void help();
  
private slots:
  void createMenus();
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
  int dbf_id;
  std::string m_progName;
};

#endif
