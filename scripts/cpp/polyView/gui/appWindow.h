#ifndef EXAMPLE_H
#define EXAMPLE_H

#include <qmainwindow.h>
#include <qlineedit.h>
#include <QEvent>
#include <string>
#include <vector>
class polyView;
class QCloseEvent;
class cmdLineOptions;
class QTextEdit;

class cmdLine : public QLineEdit {
  Q_OBJECT
  
public:
  cmdLine(QWidget* parent);
  virtual ~cmdLine();
};

class docWindow: public QMainWindow{
  Q_OBJECT

public:
  docWindow(QWidget * parent = NULL);
  virtual ~docWindow();
  void setText(const std::string & docText);
private:
  QTextEdit * m_textArea;
};

class appWindow : public QMainWindow {
  Q_OBJECT

public:
  appWindow(QWidget* parent, std::string progName,
            const cmdLineOptions & options, 
            int windowWidX, int windowWidY
            );
  ~appWindow();
  
protected:
  bool eventFilter(QObject *obj, QEvent *event);

private slots:
  void createMenusAndMainWidget(const cmdLineOptions & opt);
  void showDoc();
  void about();
  void procCmdLine();
  void shiftUp();
  void shiftDown();
  void forceQuit();
  
private:
  void closeEvent(QCloseEvent *);
  void insertCmdFromHist();
  
  polyView       * m_poly;
  cmdLine        * m_cmdLine;
  std::string      m_progName;
  std::vector<std::string> m_cmdHist;
  int m_histPos;
  docWindow m_docWindow;
};


#endif
