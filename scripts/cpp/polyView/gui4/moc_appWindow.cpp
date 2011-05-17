/****************************************************************************
** Meta object code from reading C++ file 'appWindow.h'
**
** Created: Sun May 15 22:59:06 2011
**      by: The Qt Meta Object Compiler version 59 (Qt 4.2.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "appWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'appWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.2.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

static const uint qt_meta_data_cmdLine[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets

       0        // eod
};

static const char qt_meta_stringdata_cmdLine[] = {
    "cmdLine\0"
};

const QMetaObject cmdLine::staticMetaObject = {
    { &QLineEdit::staticMetaObject, qt_meta_stringdata_cmdLine,
      qt_meta_data_cmdLine, 0 }
};

const QMetaObject *cmdLine::metaObject() const
{
    return &staticMetaObject;
}

void *cmdLine::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_cmdLine))
	return static_cast<void*>(const_cast< cmdLine*>(this));
    return QLineEdit::qt_metacast(_clname);
}

int cmdLine::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QLineEdit::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_appWindow[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      11,   10,   10,   10, 0x0a,
      28,   10,   18,   10, 0x08,
      42,   10,   10,   10, 0x08,
      56,   10,   10,   10, 0x08,
      66,   10,   10,   10, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appWindow[] = {
    "appWindow\0\0help()\0QMenuBar*\0createMenus()\0"
    "procCmdLine()\0shiftUp()\0shiftDown()\0"
};

const QMetaObject appWindow::staticMetaObject = {
    { &Q3MainWindow::staticMetaObject, qt_meta_stringdata_appWindow,
      qt_meta_data_appWindow, 0 }
};

const QMetaObject *appWindow::metaObject() const
{
    return &staticMetaObject;
}

void *appWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appWindow))
	return static_cast<void*>(const_cast< appWindow*>(this));
    return Q3MainWindow::qt_metacast(_clname);
}

int appWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = Q3MainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: help(); break;
        case 1: { QMenuBar* _r = createMenus();
            if (_a[0]) *reinterpret_cast< QMenuBar**>(_a[0]) = _r; }  break;
        case 2: procCmdLine(); break;
        case 3: shiftUp(); break;
        case 4: shiftDown(); break;
        }
        _id -= 5;
    }
    return _id;
}
