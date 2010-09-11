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
            bool useCmdLineColors, 
            const std::vector<std::string> & cmdLineColors,
            const std::vector<std::string> & polyFilesVec,
            const std::vector<bool>        & plotPointsOnlyVec,
            bool                             plotAsLines,
            int windowWidX, int windowWidY
            );
  ~appWindow();
  
public slots:
  void help();
  
private slots:
  QMenuBar* createMenus();
  void procCmdLine();
  void shiftUp();
  void shiftDown();
  
private:
  void insertCmdFromHist();
  
  drawPoly    * m_poly;
  cmdLine     * m_cmdLine;
  std::string   m_progName;
  std::vector<std::string> m_cmdHist;
  int m_histPos;
};


#endif
