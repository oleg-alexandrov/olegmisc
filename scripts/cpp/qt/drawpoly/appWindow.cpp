#include <qdatetime.h>
#include <qmainwindow.h>
#include <qstatusbar.h>
#include <qmessagebox.h>
#include <qmenubar.h>
#include <qapplication.h>
#include <qpainter.h>
#include <qprinter.h>
#include <qlabel.h>
#include <qimage.h>
#include <qprogressdialog.h>
#include <stdlib.h>
#include <iostream>
#include <cmath>
#include "appWindow.h"
#include "drawpoly.h"

using namespace std;

appWindow::appWindow(QWidget* parent, const char* name,
                     const std::vector<xg_poly> & polyVec,
                     const std::vector<bool>    & plotPointsOnlyVec,
                     int yFactor,
                     int widX, int widY, WFlags f):
  QMainWindow(parent, name, f){

  createMenus();

  m_poly = new drawPoly (this, name, polyVec, plotPointsOnlyVec, yFactor);
  setCentralWidget(m_poly);
  
  m_poly->resize( widX, widY );
  m_poly->setBackgroundColor (QColor("black"));
  m_poly->setCaption(name);
  m_poly->show();

}

appWindow::~appWindow(){

  if (m_poly != NULL){ delete m_poly; m_poly = NULL; }
  
}

void appWindow::zoomOut    (){ m_poly->zoomOut    (); }
void appWindow::zoomIn     (){ m_poly->zoomIn     (); }
void appWindow::shiftRight (){ m_poly->shiftRight (); }
void appWindow::shiftLeft  (){ m_poly->shiftLeft  (); }
void appWindow::shiftUp    (){ m_poly->shiftUp    (); }
void appWindow::shiftDown  (){ m_poly->shiftDown  (); }
void appWindow::resetView  (){ m_poly->resetView  (); }
void appWindow::toggleAnno (){ m_poly->toggleAnno (); }
void appWindow::cutToHlt   (){ m_poly->cutToHlt   (); }
void appWindow::undoLast   (){ m_poly->undoLast   (); }
void appWindow::savePoly   (){ m_poly->savePoly   (); }
void appWindow::togglePE   (){ m_poly->togglePE   (); }

void appWindow::createMenus(){
  
  QMenuBar* menu = menuBar();

  QPopupMenu* file = new QPopupMenu( menu );
  menu->insertItem("&File", file);
  file->insertItem("Save", this, SLOT(savePoly()), Key_S);
  file->insertItem("E&xit", qApp, SLOT(quit()), Key_Q);

  QPopupMenu* edit = new QPopupMenu( menu );
  menu->insertItem("&Edit", edit);
  edit->insertItem("Cut to hlt", this, SLOT(cutToHlt()), Key_C);
  edit->insertItem("Undo",       this, SLOT(undoLast()), Key_Z);

  QPopupMenu* view = new QPopupMenu( menu );
  menu->insertItem("&View", view);
  //view->insertSeparator();
  view->insertItem("Zoom out",      this, SLOT(zoomOut()),    Key_Minus);
  view->insertItem("Zoom in",       this, SLOT(zoomIn()),     Key_Equal);
  view->insertItem("Move left",     this, SLOT(shiftLeft()),  Key_Left);
  view->insertItem("Move right",    this, SLOT(shiftRight()), Key_Right);
  view->insertItem("Move up",       this, SLOT(shiftUp()),    Key_Up);
  view->insertItem("Move down",     this, SLOT(shiftDown()),  Key_Down);
  view->insertItem("Reset view",    this, SLOT(resetView()),  Key_R);
  view->insertItem("Toggle anno",   this, SLOT(toggleAnno()), Key_A);
  view->insertItem("Toggle points", this, SLOT(togglePE()),   Key_P);

  QPopupMenu* help = new QPopupMenu( menu );
  help->insertItem("&About", this, SLOT(help()));
  help->setItemChecked(dbf_id, TRUE);
  menu->insertItem("&Help",help);

  statusBar();
}

void appWindow::help(){
  
  static QMessageBox* about
    = new QMessageBox( "About polyViewer",
                       " © 2010 Oleg Alexandrov  ",
                       QMessageBox::NoIcon, 1, 0, 0, this, 0, FALSE );
  about->setButtonText( 1, "OK" );
  about->show();
}




