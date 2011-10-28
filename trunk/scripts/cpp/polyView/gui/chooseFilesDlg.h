#ifndef CHOOSE_FILES_DLG_H
#define CHOOSE_FILES_DLG_H
#include <set>
#include <string>
#include <vector>
#include <QDialog>

class QWidget;
class QTableWidget;
class polyOptions;

class chooseFilesDlg: public QDialog{
  Q_OBJECT
  
public:
  chooseFilesDlg(QWidget * parent = NULL);
  ~chooseFilesDlg();
  void chooseFiles(const std::vector<polyOptions> & optionsVec);
  QTableWidget * getFilesTable(){ return m_filesTable; }
  static QString selectFilesTag(){ return "Select files to hide/show"; }
private:
  QTableWidget * m_filesTable;

private slots:
};

#endif // CHOOSE_FILES_DLG_H
