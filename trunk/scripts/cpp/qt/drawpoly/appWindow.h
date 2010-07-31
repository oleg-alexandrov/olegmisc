#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <qmainwindow.h>
#include <string>
#include <vector>
class drawPoly;

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
  void openPoly          ();
  void saveOnePoly       ();
  void saveMultiplePoly  ();
  void togglePE          ();
  void toggleOrder       ();
  void createPoly        ();
  void deletePoly        ();
  // actions
  
private:
  drawPoly   * m_poly;
  std::string  m_progName;
};

#endif
