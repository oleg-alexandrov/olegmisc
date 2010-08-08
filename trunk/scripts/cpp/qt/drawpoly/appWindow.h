#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <qmainwindow.h>
#include <qlineedit.h>
#include <string>
#include <vector>
class drawPoly;

class cmdLine : public QLineEdit {
  Q_OBJECT
  
public:
  cmdLine(QWidget* parent);
  virtual ~cmdLine();
};

class appWindow : public QMainWindow {
  Q_OBJECT

public:
  appWindow(QWidget* parent, const char* progName,
            const std::vector<std::string> & polyFilesVec,
            const std::vector<bool>        & plotPointsOnlyVec,
            int windowWidX, int windowWidY);
  ~appWindow();
  
public slots:
  void help();
  
private slots:
  QMenuBar* createMenus();
  void zoomIn              ();
  void zoomOut             ();
  void shiftRight          ();
  void shiftLeft           ();
  void shiftUp             ();
  void shiftDown ();
  void resetView           ();
  void toggleAnno          ();
  void toggleVertIndexAnno ();
  void toggleLayerAnno     ();
  void toggleFilled        ();
  void cutToHlt            ();
  void undoLast            ();
  void openPoly            ();
  void saveOnePoly         ();
  void saveMultiplePoly    ();
  void togglePE            ();
  void changeOrder         ();
  void createPoly          ();
  void deletePoly          ();
  void procCmdLine();
  // actions
  
private:
  void insertCmdFromHist();
  
  drawPoly    * m_poly;
  cmdLine     * m_cmdLine;
  std::string   m_progName;
  std::vector<std::string> m_cmdHist;
  int m_histPos;
};


#endif
