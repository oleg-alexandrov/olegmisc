/****************************************************************************
** appWindow meta object code from reading C++ file 'appWindow.h'
**
** Created: Mon Jul 5 17:11:31 2010
**      by: The Qt MOC ($Id: qt/moc_yacc.cpp   3.3.6   edited Mar 8 17:43 $)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#undef QT_NO_COMPAT
#include "appWindow.h"
#include <qmetaobject.h>
#include <qapplication.h>

#include <private/qucomextra_p.h>
#if !defined(Q_MOC_OUTPUT_REVISION) || (Q_MOC_OUTPUT_REVISION != 26)
#error "This file was generated using the moc from 3.3.6. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

const char *appWindow::className() const
{
    return "appWindow";
}

QMetaObject *appWindow::metaObj = 0;
static QMetaObjectCleanUp cleanUp_appWindow( "appWindow", &appWindow::staticMetaObject );

#ifndef QT_NO_TRANSLATION
QString appWindow::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "appWindow", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString appWindow::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "appWindow", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* appWindow::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QMainWindow::staticMetaObject();
    static const QUMethod slot_0 = {"help", 0, 0 };
    static const QUMethod slot_1 = {"createMenus", 0, 0 };
    static const QUMethod slot_2 = {"zoomIn", 0, 0 };
    static const QUMethod slot_3 = {"zoomOut", 0, 0 };
    static const QUMethod slot_4 = {"shiftRight", 0, 0 };
    static const QUMethod slot_5 = {"shiftLeft", 0, 0 };
    static const QUMethod slot_6 = {"shiftUp", 0, 0 };
    static const QUMethod slot_7 = {"shiftDown", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "help()", &slot_0, QMetaData::Public },
	{ "createMenus()", &slot_1, QMetaData::Private },
	{ "zoomIn()", &slot_2, QMetaData::Private },
	{ "zoomOut()", &slot_3, QMetaData::Private },
	{ "shiftRight()", &slot_4, QMetaData::Private },
	{ "shiftLeft()", &slot_5, QMetaData::Private },
	{ "shiftUp()", &slot_6, QMetaData::Private },
	{ "shiftDown()", &slot_7, QMetaData::Private }
    };
    metaObj = QMetaObject::new_metaobject(
	"appWindow", parentObject,
	slot_tbl, 8,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_appWindow.setMetaObject( metaObj );
    return metaObj;
}

void* appWindow::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "appWindow" ) )
	return this;
    return QMainWindow::qt_cast( clname );
}

bool appWindow::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: help(); break;
    case 1: createMenus(); break;
    case 2: zoomIn(); break;
    case 3: zoomOut(); break;
    case 4: shiftRight(); break;
    case 5: shiftLeft(); break;
    case 6: shiftUp(); break;
    case 7: shiftDown(); break;
    default:
	return QMainWindow::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool appWindow::qt_emit( int _id, QUObject* _o )
{
    return QMainWindow::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool appWindow::qt_property( int id, int f, QVariant* v)
{
    return QMainWindow::qt_property( id, f, v);
}

bool appWindow::qt_static_property( QObject* , int , int , QVariant* ){ return FALSE; }
#endif // QT_NO_PROPERTIES
