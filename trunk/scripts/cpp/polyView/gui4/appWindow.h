#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <q3mainwindow.h>
#include <qlineedit.h>
//Added by qt3to4:
#include <QEvent>
#include <string>
#include <vector>
class polyView;
struct cmdLineOptions;

class cmdLine : public QLineEdit {
  Q_OBJECT
  
public:
  cmdLine(QWidget* parent);
  virtual ~cmdLine();
};

class appWindow : public Q3MainWindow {
  Q_OBJECT

public:
  appWindow(QWidget* parent, std::string progName,
            const cmdLineOptions & options, 
            int windowWidX, int windowWidY
            );
  ~appWindow();
  
public slots:
  void help();

protected:
  bool eventFilter(QObject *obj, QEvent *ev);

private slots:
  QMenuBar* createMenus();
  void procCmdLine();
  void shiftUp ();
  void shiftDown ();
  
private:
  void insertCmdFromHist();
  
  polyView    * m_poly;
  cmdLine     * m_cmdLine;
  std::string   m_progName;
  std::vector<std::string> m_cmdHist;
  int m_histPos;
};


#endif
