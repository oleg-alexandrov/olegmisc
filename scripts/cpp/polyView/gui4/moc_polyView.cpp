/****************************************************************************
** Meta object code from reading C++ file 'polyView.h'
**
** Created: Fri May 13 23:31:40 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "polyView.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'polyView.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_polyView[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
      34,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x0a,
      21,    9,    9,    9, 0x0a,
      35,    9,    9,    9, 0x0a,
      60,    9,    9,    9, 0x0a,
      92,   82,    9,    9, 0x0a,
     115,    9,    9,    9, 0x0a,
     128,    9,    9,    9, 0x0a,
     142,    9,    9,    9, 0x0a,
     155,    9,    9,    9, 0x0a,
     165,    9,    9,    9, 0x0a,
     174,    9,    9,    9, 0x0a,
     186,    9,    9,    9, 0x0a,
     199,    9,    9,    9, 0x0a,
     209,    9,    9,    9, 0x0a,
     221,    9,    9,    9, 0x0a,
     233,    9,    9,    9, 0x0a,
     247,    9,    9,    9, 0x0a,
     260,    9,    9,    9, 0x0a,
     275,    9,    9,    9, 0x0a,
     296,    9,    9,    9, 0x0a,
     311,    9,    9,    9, 0x0a,
     336,  326,    9,    9, 0x0a,
     350,    9,    9,    9, 0x0a,
     361,    9,    9,    9, 0x0a,
     383,    9,    9,    9, 0x0a,
     401,    9,    9,    9, 0x0a,
     412,    9,    9,    9, 0x0a,
     423,    9,    9,    9, 0x0a,
     447,    9,    9,    9, 0x0a,
     469,    9,    9,    9, 0x0a,
     481,    9,    9,    9, 0x0a,
     496,    9,    9,    9, 0x0a,
     507,    9,    9,    9, 0x0a,
     523,    9,    9,    9, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_polyView[] = {
    "polyView\0\0openPoly()\0saveOnePoly()\0"
    "overwriteMultiplePolys()\0saveAsMultiplePolys()\0"
    "overwrite\0saveMultiplePoly(bool)\0"
    "shiftPolys()\0rotatePolys()\0scalePolys()\0"
    "zoomOut()\0zoomIn()\0shiftLeft()\0"
    "shiftRight()\0shiftUp()\0shiftDown()\0"
    "resetView()\0changeOrder()\0toggleAnno()\0"
    "toggleFilled()\0toggleShowPolyDiff()\0"
    "plotNextDiff()\0plotPrevDiff()\0direction\0"
    "plotDiff(int)\0togglePE()\0toggleVertIndexAnno()\0"
    "toggleLayerAnno()\0undoLast()\0cutToHlt()\0"
    "create45DegreeIntPoly()\0createArbitraryPoly()\0"
    "enforce45()\0setLineWidth()\0saveMark()\0"
    "toggleNmScale()\0deletePoly()\0"
};

const QMetaObject polyView::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_polyView,
      qt_meta_data_polyView, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &polyView::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *polyView::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *polyView::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_polyView))
        return static_cast<void*>(const_cast< polyView*>(this));
    return QWidget::qt_metacast(_clname);
}

int polyView::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: openPoly(); break;
        case 1: saveOnePoly(); break;
        case 2: overwriteMultiplePolys(); break;
        case 3: saveAsMultiplePolys(); break;
        case 4: saveMultiplePoly((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 5: shiftPolys(); break;
        case 6: rotatePolys(); break;
        case 7: scalePolys(); break;
        case 8: zoomOut(); break;
        case 9: zoomIn(); break;
        case 10: shiftLeft(); break;
        case 11: shiftRight(); break;
        case 12: shiftUp(); break;
        case 13: shiftDown(); break;
        case 14: resetView(); break;
        case 15: changeOrder(); break;
        case 16: toggleAnno(); break;
        case 17: toggleFilled(); break;
        case 18: toggleShowPolyDiff(); break;
        case 19: plotNextDiff(); break;
        case 20: plotPrevDiff(); break;
        case 21: plotDiff((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 22: togglePE(); break;
        case 23: toggleVertIndexAnno(); break;
        case 24: toggleLayerAnno(); break;
        case 25: undoLast(); break;
        case 26: cutToHlt(); break;
        case 27: create45DegreeIntPoly(); break;
        case 28: createArbitraryPoly(); break;
        case 29: enforce45(); break;
        case 30: setLineWidth(); break;
        case 31: saveMark(); break;
        case 32: toggleNmScale(); break;
        case 33: deletePoly(); break;
        default: ;
        }
        _id -= 34;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
