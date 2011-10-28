#include <cassert>
#include <iostream>
#include <QDialogButtonBox>
#include <QLabel>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QWidget>
#include "chooseFilesDlg.h"
#include "utils.h"
using namespace std;

// Allow the user to choose which files to hide/show in the GUI.
// User's choice will be processed by polyView::showFilesChosenByUser().
chooseFilesDlg::chooseFilesDlg(QWidget * parent): QDialog(parent){

  setWindowModality(Qt::ApplicationModal); 

  int spacing = 6;
  
  QVBoxLayout * vBoxLayout = new QVBoxLayout(this);
  vBoxLayout->setSpacing(spacing);
  vBoxLayout->setAlignment(Qt::AlignLeft);

  // Label
  QLabel * label = new QLabel(chooseFilesDlg::selectFilesTag(), this);

  // The layout having the file names. It will be filled in dynamically later.
  m_filesTable = new QTableWidget();

  QDialogButtonBox * submitBox = new QDialogButtonBox(this);
  submitBox->setOrientation(Qt::Horizontal);
  submitBox->setStandardButtons(QDialogButtonBox::Ok);

  vBoxLayout->addWidget(label);
  vBoxLayout->addWidget(m_filesTable);
  vBoxLayout->addWidget(submitBox);

  connect(submitBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(submitBox, SIGNAL(rejected()), this, SLOT(reject()));

  return;
}

chooseFilesDlg::~chooseFilesDlg(){}

void chooseFilesDlg::chooseFiles(const std::vector<polyOptions> & optionsVec){

  // See the top of this file for documentation.
  
  int numFiles = optionsVec.size();
  int numCols = 1;
  m_filesTable->setRowCount(numFiles);
  m_filesTable->setColumnCount(numCols);

  for (int fileIter = 0; fileIter < numFiles; fileIter++){
    string fileName         = optionsVec[fileIter].polyFileName;
     QTableWidgetItem *item = new QTableWidgetItem(fileName.c_str());
     m_filesTable->setItem(fileIter, numCols - 1, item);
  }

  QStringList rowNamesList;
  for (int fileIter = 0; fileIter < numFiles; fileIter++) rowNamesList << "";
  m_filesTable->setVerticalHeaderLabels(rowNamesList);

  QStringList colNamesList; 
  for (int colIter = 0; colIter < numCols; colIter++) colNamesList << "Invert selection";
  m_filesTable->setHorizontalHeaderLabels(colNamesList);
  QTableWidgetItem * hs = m_filesTable->horizontalHeaderItem(0);
  hs->setBackground(QBrush(QColor("lightgray")));
  
  m_filesTable->setSelectionMode(QTableWidget::MultiSelection);
  string style = string("QTableWidget::indicator:unchecked ")
    + "{background-color:white; border: 1px solid black;}";
  m_filesTable->setStyleSheet(style.c_str());
  m_filesTable->resizeColumnsToContents();
  m_filesTable->resizeRowsToContents();
  
  exec(); // Pop-up the filled in dialog

  // The processing of user's choice happens in polyView::showFilesChosenByUser()
  
  return;
}

