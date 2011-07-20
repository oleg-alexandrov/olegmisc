#include <cassert>
#include <iostream>
#include <QDialogButtonBox>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QRadioButton>
#include <QButtonGroup>
#include <QSize>
#include <QSplitter>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QWidget>
#include <QtGui>
#include "chooseFilesDlg.h"
#include "utils.h"
using namespace std;

// Allow the user to choose which files to show or not to show in the GUI.

chooseFilesDlg::chooseFilesDlg(QWidget * parent): QDialog(parent){

  setWindowModality(Qt::ApplicationModal); 

  int spacing = 6;
  
  QVBoxLayout * vBoxLayout = new QVBoxLayout(this);
  vBoxLayout->setSpacing(spacing);
  vBoxLayout->setAlignment(Qt::AlignLeft);

  // Label
  QLabel * label = new QLabel("Choose files to show", this);

  // The layout having the file names. It will be filled in dynamically later.
  m_filesTable = new QTableWidget();

  QDialogButtonBox * submitBox = new QDialogButtonBox(this);
  submitBox->setOrientation(Qt::Horizontal);
  submitBox->setStandardButtons(QDialogButtonBox::Cancel   |
                                QDialogButtonBox::NoButton |
                                QDialogButtonBox::Ok
                                );

  vBoxLayout->addWidget(label);
  vBoxLayout->addWidget(m_filesTable);
  vBoxLayout->addWidget(submitBox);

  connect(submitBox, SIGNAL(accepted()), this, SLOT(accept()));
  connect(submitBox, SIGNAL(rejected()), this, SLOT(reject()));

  return;
}

chooseFilesDlg::~chooseFilesDlg(){}

bool chooseFilesDlg::chooseFiles(const std::vector<polyOptions> & optionsVec,   // In
                                 std::set<std::string>          & filesNotToShow// In-out
                                 ){

  // See the top of this file for documentation.
  
  int numFiles = optionsVec.size();
  int numCols = 1;
  m_filesTable->setRowCount(numFiles);
  m_filesTable->setColumnCount(numCols);
  
  for (int fileIter = 0; fileIter < numFiles; fileIter++){
    
    string fileName        = optionsVec[fileIter].polyFileName;
    QTableWidgetItem *item = new QTableWidgetItem(fileName.c_str());
    
    if (filesNotToShow.find(fileName) != filesNotToShow.end()){
      item->setCheckState(Qt::Unchecked);
    }else{
      item->setCheckState(Qt::Checked);
    }
    
    m_filesTable->setItem(fileIter, numCols - 1, item);
  }

  QStringList rowNamesList;
  for (int fileIter = 0; fileIter < numFiles; fileIter++) rowNamesList << "";
  m_filesTable->setVerticalHeaderLabels(rowNamesList);

  QStringList colNamesList; 
  for (int colIter = 0; colIter < numCols; colIter++) colNamesList << "";
  m_filesTable->setHorizontalHeaderLabels(colNamesList);
  
  m_filesTable->setSelectionMode(QTableWidget::NoSelection);
  string style = string("QTableWidget::indicator:unchecked ")
    + "{background-color:white; border: 1px solid black;}";
  m_filesTable->setStyleSheet(style.c_str());
  m_filesTable->resizeColumnsToContents();
  m_filesTable->resizeRowsToContents();
  
  int ans = exec(); // Pop-up the filled in dialog
  if (ans == QDialog::Rejected) return false;

  // Process user's choice
  for (int fileIter = 0; fileIter < numFiles; fileIter++){
    
    QTableWidgetItem *item = m_filesTable->item(fileIter, numCols - 1);
    bool isChecked  = (item->checkState() != Qt::Unchecked);
    string fileName = ((item->data(0)).toString()).toStdString();
    
    if (!isChecked) filesNotToShow.insert(fileName);
    else            filesNotToShow.erase (fileName);
  }
  
  return true;
}
