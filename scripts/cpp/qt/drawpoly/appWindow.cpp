#include <qapplication.h>
#include <qlabel.h>
#include <qmainwindow.h>
#include <qmenubar.h>
#include <qmessagebox.h>
#include <cstdlib>
#include <iostream>
#include <cmath>
#include "appWindow.h"
#include "drawpoly.h"

using namespace std;

appWindow::appWindow(QWidget* parent, const char* progName,
                     const std::vector<std::string> & polyFilesVec,
                     const std::vector<bool>        & plotPointsOnlyVec):
  QMainWindow(parent, progName){

  m_progName = progName;
  
  createMenus();
  statusBar();

  m_poly = new drawPoly (this, polyFilesVec, plotPointsOnlyVec);
  m_poly->setBackgroundColor (QColor("black"));
  setCentralWidget(m_poly);

}

appWindow::~appWindow(){

  if (m_poly != NULL){ delete m_poly; m_poly = NULL; }
  
}

void appWindow::zoomOut           (){ m_poly->zoomOut           (); }
void appWindow::zoomIn            (){ m_poly->zoomIn            (); }
void appWindow::shiftRight        (){ m_poly->shiftRight        (); }
void appWindow::shiftLeft         (){ m_poly->shiftLeft         (); }
void appWindow::shiftUp           (){ m_poly->shiftUp           (); }
void appWindow::shiftDown         (){ m_poly->shiftDown         (); }
void appWindow::resetView         (){ m_poly->resetView         (); }
void appWindow::toggleAnno        (){ m_poly->toggleAnno        (); }
void appWindow::toggleVertIndices (){ m_poly->toggleVertIndices (); }
void appWindow::toggleFilled      (){ m_poly->toggleFilled      (); }
void appWindow::cutToHlt          (){ m_poly->cutToHlt          (); }
void appWindow::undoLast          (){ m_poly->undoLast          (); }
void appWindow::openPoly          (){ m_poly->openPoly          (); }
void appWindow::savePoly          (){ m_poly->savePoly          (); }
void appWindow::togglePE          (){ m_poly->togglePE          (); }
void appWindow::toggleOrder       (){ m_poly->toggleOrder       (); }

void appWindow::createMenus(){
  
  QMenuBar* menu = menuBar();

  QPopupMenu* file = new QPopupMenu( menu );
  menu->insertItem("File", file);
  file->insertItem("Open", this, SLOT(openPoly()), CTRL+Key_O);
  file->insertItem("Save", this, SLOT(savePoly()), CTRL+Key_S);
  file->insertItem("Exit", qApp, SLOT(quit()), Key_Q);

  QPopupMenu* edit = new QPopupMenu( menu );
  menu->insertItem("Edit", edit);
  edit->insertItem("Cut to highlight", this, SLOT(cutToHlt()), Key_C);
  edit->insertItem("Undo",             this, SLOT(undoLast()), Key_Z);

  QPopupMenu* view = new QPopupMenu( menu );
  menu->insertItem("View", view);
  //view->insertSeparator();
  view->insertItem("Zoom out",              this, SLOT(zoomOut()),           Key_Minus);
  view->insertItem("Zoom in",               this, SLOT(zoomIn()),            Key_Equal);
  view->insertItem("Move left",             this, SLOT(shiftLeft()),         Key_Left);
  view->insertItem("Move right",            this, SLOT(shiftRight()),        Key_Right);
  view->insertItem("Move up",               this, SLOT(shiftUp()),           Key_Up);
  view->insertItem("Move down",             this, SLOT(shiftDown()),         Key_Down);
  view->insertItem("Reset view",            this, SLOT(resetView()),         Key_R);
  view->insertItem("Toggle annotations",    this, SLOT(toggleAnno()),        Key_A);
  view->insertItem("Toggle display order",  this, SLOT(toggleOrder()),       Key_O);
  view->insertItem("Toggle filled",         this, SLOT(toggleFilled()),      Key_F);
  view->insertItem("Toggle points display", this, SLOT(togglePE()),          Key_P);
  view->insertItem("Toggle vertex indices", this, SLOT(toggleVertIndices()), Key_V);

  QPopupMenu* help = new QPopupMenu( menu );
  help->insertItem("About", this, SLOT(help()));
  menu->insertItem("Help", help);

  return;
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




