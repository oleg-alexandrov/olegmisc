#include <qapplication.h>
#include <Q3PopupMenu>
#include <qlabel.h>
#include <QtGui/QMainWindow>
#include <qmenubar.h>
#include <qmessagebox.h>
#include <qstatusbar.h>
#include <qlayout.h>
#include <QMenu>
#include <QEvent>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include "appWindow.h"
#include "polyView.h"
#include "utils.h"

using namespace std;

cmdLine::cmdLine(QWidget* parent): QLineEdit(parent){}
cmdLine::~cmdLine(){}

appWindow::appWindow(QWidget* parent, std::string progName,
                     const cmdLineOptions & options, 
                     int windowWidX, int windowWidY
                     ):
  QMainWindow(parent, progName.c_str()){

  installEventFilter(this);

  m_progName = progName;
  resize(windowWidX, windowWidY);

  // Central widget
  m_poly = new polyView (this, options);
  m_poly->setBackgroundColor (QColor("black"));
  m_poly->setFocusPolicy(Qt::StrongFocus);
  m_poly->setFocus();
  setCentralWidget(m_poly);

  // Command line
  m_cmdLine = new cmdLine(this);
  m_cmdLine->setAlignment(Qt::AlignLeft);
  m_cmdLine->setFocusPolicy(Qt::StrongFocus);
  connect( m_cmdLine, SIGNAL( returnPressed() ),
           this, SLOT( procCmdLine() ) );

  QStatusBar * status = statusBar();
  QRect Rp = status->rect();
  m_cmdLine->setGeometry(Rp);
  status->addWidget(m_cmdLine, 1);
  m_cmdHist.clear();
  m_histPos = 0;

  // Menus (must be created after the other widgets were initialized)
  createMenus();

}

void appWindow::forceQuit(){
  exit(0); // A fix for an older buggy version of Qt
}
                     
bool appWindow::eventFilter(QObject *obj, QEvent *event){

  if (obj == m_poly) {
    // Avoid repainting on these events
    if (event->type() == QEvent::FocusIn          ||
        event->type() == QEvent::FocusOut         ||
        event->type() == QEvent::WindowDeactivate ||
        event->type() == QEvent::Leave
        ){
      return true;
    }
  }

  //cout << "Other event: " << (int)event->type() << endl;
  return QWidget::eventFilter(obj, event);
}

appWindow::~appWindow(){

  if (m_poly != NULL){ delete m_poly; m_poly = NULL; }
  
}

void appWindow::procCmdLine(){
  string cmd = m_cmdLine->text().toStdString();
  m_cmdHist.push_back(cmd);
  m_poly->runCmd(cmd);
  m_cmdLine->setText("");
  m_histPos = m_cmdHist.size();
  //m_poly->setFocus();
}

void appWindow::insertCmdFromHist(){

  int numHistItems = m_cmdHist.size();
  m_histPos = min(m_histPos, numHistItems);
  m_histPos = max(0, m_histPos);
  
  if (m_histPos < numHistItems){
    m_cmdLine->setText(m_cmdHist[m_histPos].c_str());
  }else{
    m_cmdLine->setText("");
  }

  return;
}

void appWindow::shiftUp (){

  if (m_poly->hasFocus()){
    m_poly->shiftUp ();
  }else if(m_cmdLine->hasFocus()){
    m_histPos--;
    insertCmdFromHist();
  }
  
}

void appWindow::shiftDown (){

  if (m_poly->hasFocus()){
    m_poly->shiftDown ();
  }else if(m_cmdLine->hasFocus()){
    m_histPos++;
    insertCmdFromHist();
  }
  
}

QMenuBar* appWindow::createMenus(){

  QMenuBar* menu = menuBar();

  Q3PopupMenu* file = new Q3PopupMenu( menu );
  menu->insertItem("&File", file);
  file->insertItem("Open", m_poly, SLOT(openPoly()), Qt::CTRL+Qt::Key_O);
  file->insertItem("Save as one clip", m_poly, SLOT(saveOnePoly()),
                   Qt::CTRL+Qt::Key_S);
  file->insertItem("Save as multiple clips", m_poly,
                   SLOT(saveAsMultiplePolys()), Qt::ALT+Qt::Key_S);
  file->insertItem("Overwrite current clips", m_poly,
                   SLOT(overwriteMultiplePolys()), Qt::CTRL+Qt::Key_W);
  file->insertItem("Exit", this, SLOT(forceQuit()), Qt::Key_Q);

  Q3PopupMenu* view = new Q3PopupMenu( menu );
  menu->insertItem("&View", view);
  //view->insertSeparator();
  view->insertItem("Zoom out",             m_poly, SLOT(zoomOut()),      Qt::Key_Minus);
  view->insertItem("Zoom in",              m_poly, SLOT(zoomIn()),       Qt::Key_Equal);
  view->insertItem("Move left",            m_poly, SLOT(shiftLeft()),    Qt::Key_Left);
  view->insertItem("Move right",           m_poly, SLOT(shiftRight()),   Qt::Key_Right);
  view->insertItem("Move up",              this,   SLOT(shiftUp()),      Qt::Key_Up);
  view->insertItem("Move down",            this,   SLOT(shiftDown()),    Qt::Key_Down);
  view->insertItem("Reset view",           m_poly, SLOT(resetView()),    Qt::Key_R);
  view->insertItem("Change display order", m_poly, SLOT(changeOrder()),  Qt::Key_O);
  view->insertItem("Toggle annotations",   m_poly, SLOT(toggleAnno()),   Qt::Key_A);
  view->insertItem("Toggle filled",        m_poly, SLOT(toggleFilled()), Qt::Key_F);
  view->insertItem("Toggle points display",
                   m_poly, SLOT(togglePE()),          Qt::Key_P);
  view->insertItem("Toggle show vertex indices",
                   m_poly, SLOT(toggleVertIndexAnno()), Qt::Key_V);
  view->insertItem("Toggle show layers", m_poly, SLOT(toggleLayerAnno()), Qt::Key_L);

  Q3PopupMenu* edit = new Q3PopupMenu( menu );
  menu->insertItem("&Edit", edit);
  edit->insertItem("Undo",             m_poly, SLOT(undoLast()), Qt::Key_Z);
  edit->insertItem("Cut to highlight", m_poly, SLOT(cutToHlt()), Qt::Key_C);
  edit->insertItem("Create poly with int vertices and 45x angles",
                   m_poly, SLOT(create45DegreeIntPoly()), Qt::Key_N);
  edit->insertItem("Create arbitrary polygon",
                   m_poly, SLOT(createArbitraryPoly()), Qt::CTRL+Qt::Key_N);

  Q3PopupMenu* transform = new Q3PopupMenu( menu );
  menu->insertItem("&Transform", transform);
  transform->insertItem("Enforce int vertices and 45x angles", m_poly, SLOT(enforce45()),
                        Qt::CTRL+Qt::Key_4);
  transform->insertItem("Translate polygons", m_poly, SLOT(shiftPolys()),
                        Qt::CTRL+Qt::Key_T);
  transform->insertItem("Rotate polygons", m_poly, SLOT(rotatePolys()),
                        Qt::CTRL+Qt::Key_R);
  transform->insertItem("Scale polygons", m_poly, SLOT(scalePolys()),
                        Qt::CTRL+Qt::Key_X);

  Q3PopupMenu* diff = new Q3PopupMenu( menu );
  menu->insertItem("&Diff", diff);
  diff->insertItem("Toggle show poly diff", m_poly, SLOT(toggleShowPolyDiff()), Qt::Key_D);
  diff->insertItem("Show next diff", m_poly, SLOT(plotNextDiff()), Qt::Key_K);
  diff->insertItem("Show prev diff", m_poly, SLOT(plotPrevDiff()), Qt::Key_J);

  Q3PopupMenu* options = new Q3PopupMenu( menu );
  menu->insertItem("&Options", options);
  options->insertItem("Line width", m_poly, SLOT(setLineWidth()));

  Q3PopupMenu* help = new Q3PopupMenu( menu );
  menu->insertItem("&Help", help);
  help->insertItem("About", this, SLOT(help()));

  return menu;
}

void appWindow::help(){

  string aboutStr = string("About ") + m_progName;
  static QMessageBox* about
    = new QMessageBox( aboutStr.c_str(),
                       "© Oleg Alexandrov        ", // extra space to make window bigger
                       QMessageBox::NoIcon, 1, 0, 0, this, 0, FALSE );
  about->setButtonText( 1, "OK" );
  about->show();

  return;
}



