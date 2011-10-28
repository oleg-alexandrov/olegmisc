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
  void chooseFiles(const std::vector<polyOptions> & optionsVec, // In
                   std::set<std::string>          & filesToHide // In-out
                   );
  QTableWidget * getFilesTable(){ return m_filesTable; }
  
private:
  QTableWidget * m_filesTable;

private slots:
};

#endif // CHOOSE_FILES_DLG_H
