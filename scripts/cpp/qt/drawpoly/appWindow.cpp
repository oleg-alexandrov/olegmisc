#include <qapplication.h>
#include <qlabel.h>
#include <qmainwindow.h>
#include <qmenubar.h>
#include <qmessagebox.h>
#include <qstatusbar.h>
#include <qlayout.h>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include "appWindow.h"
#include "drawpoly.h"

using namespace std;

cmdLine::cmdLine(QWidget* parent): QLineEdit(parent){}
cmdLine::~cmdLine(){}

appWindow::appWindow(QWidget* parent, const char* progName,
                     const std::vector<std::string> & polyFilesVec,
                     const std::vector<bool>        & plotPointsOnlyVec,
                     int windowWidX, int windowWidY):
  QMainWindow(parent, progName){

  m_progName = progName;
  resize(windowWidX, windowWidY);

  createMenus();
  
  m_poly = new drawPoly (this, polyFilesVec, plotPointsOnlyVec);
  m_poly->setBackgroundColor (QColor("black"));
  m_poly->setFocusPolicy(QWidget::StrongFocus);
  m_poly->setFocus();
  setCentralWidget(m_poly);
  
  m_cmdLine = new cmdLine(this);
  m_cmdLine->setAlignment(Qt::AlignLeft);
  m_cmdLine->setFocusPolicy(QWidget::StrongFocus);
  connect( m_cmdLine, SIGNAL( returnPressed() ),
           this, SLOT( procCmdLine() ) );

  QStatusBar * status = statusBar();
  QRect Rp = status->rect();
  m_cmdLine->setGeometry(Rp);
  status->addWidget(m_cmdLine, 1);

  m_cmdHist.clear();
  m_histPos = 0;
}

appWindow::~appWindow(){

  if (m_poly != NULL){ delete m_poly; m_poly = NULL; }
  
}

void appWindow::procCmdLine(){
  string cmd = m_cmdLine->text().data();
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

void appWindow::zoomOut             (){ m_poly->zoomOut             (); }
void appWindow::zoomIn              (){ m_poly->zoomIn              (); }
void appWindow::shiftRight          (){ m_poly->shiftRight          (); }
void appWindow::shiftLeft           (){ m_poly->shiftLeft           (); }
void appWindow::resetView           (){ m_poly->resetView           (); }
void appWindow::toggleAnno          (){ m_poly->toggleAnno          (); }
void appWindow::toggleVertIndexAnno (){ m_poly->toggleVertIndexAnno (); }
void appWindow::toggleLayerAnno     (){ m_poly->toggleLayerAnno     (); }
void appWindow::toggleFilled        (){ m_poly->toggleFilled        (); }
void appWindow::cutToHlt            (){ m_poly->cutToHlt            (); }
void appWindow::undoLast            (){ m_poly->undoLast            (); }
void appWindow::openPoly            (){ m_poly->openPoly            (); }
void appWindow::saveOnePoly         (){ m_poly->saveOnePoly         (); }
void appWindow::saveMultiplePoly    (){ m_poly->saveMultiplePoly    (); }
void appWindow::togglePE            (){ m_poly->togglePE            (); }
void appWindow::changeOrder         (){ m_poly->changeOrder         (); }
void appWindow::createPoly          (){ m_poly->createPoly          (); }
void appWindow::deletePoly          (){ m_poly->deletePoly          (); }
// actions


QMenuBar* appWindow::createMenus(){
  
  QMenuBar* menu = menuBar();

  QPopupMenu* file = new QPopupMenu( menu );
  menu->insertItem("&File", file);
  file->insertItem("Open", this, SLOT(openPoly()), Qt::CTRL+Key_O);
  file->insertItem("Save as one polygon", this, SLOT(saveOnePoly()),
                   Qt::CTRL+Key_S);
  file->insertItem("Save as multiple polygons", this,
                   SLOT(saveMultiplePoly()), Qt::ALT+Key_S);
  file->insertItem("Exit", qApp, SLOT(quit()), Key_Q);

  QPopupMenu* edit = new QPopupMenu( menu );
  menu->insertItem("&Edit", edit);
  edit->insertItem("Undo",             this, SLOT(undoLast()), Key_Z);
  edit->insertItem("Cut to highlight", this, SLOT(cutToHlt()), Key_C);
  edit->insertItem("Create polygon",   this, SLOT(createPoly()), Key_N);

  QPopupMenu* view = new QPopupMenu( menu );
  menu->insertItem("&View", view);
  //view->insertSeparator();
  view->insertItem("Zoom out",              this, SLOT(zoomOut()),           Key_Minus);
  view->insertItem("Zoom in",               this, SLOT(zoomIn()),            Key_Equal);
  view->insertItem("Move left",             this, SLOT(shiftLeft()),         Key_Left);
  view->insertItem("Move right",            this, SLOT(shiftRight()),        Key_Right);
  view->insertItem("Move up",               this, SLOT(shiftUp()),           Key_Up);
  view->insertItem("Move down",             this, SLOT(shiftDown()),         Key_Down);
  view->insertItem("Reset view",            this, SLOT(resetView()),         Key_R);
  view->insertItem("Change display order",  this, SLOT(changeOrder()),       Key_O);
  view->insertItem("Toggle annotations",    this, SLOT(toggleAnno()),        Key_A);
  view->insertItem("Toggle filled",         this, SLOT(toggleFilled()),      Key_F);
  view->insertItem("Toggle points display", this, SLOT(togglePE()),          Key_P);
  view->insertItem("Toggle show vertex indices", this, SLOT(toggleVertIndexAnno()), Key_V);
  view->insertItem("Toggle show layers", this, SLOT(toggleLayerAnno()), Key_L);

  QPopupMenu* help = new QPopupMenu( menu );
  menu->insertItem("&Help", help);
  help->insertItem("About", this, SLOT(help()));

  return menu;
}

void appWindow::help(){

  string aboutStr = string("About ") + m_progName;
  static QMessageBox* about
    = new QMessageBox( aboutStr.c_str(),
                       "© 2010 Oleg Alexandrov  ",
                       QMessageBox::NoIcon, 1, 0, 0, this, 0, FALSE );
  about->setButtonText( 1, "OK" );
  about->show();

  return;
}



