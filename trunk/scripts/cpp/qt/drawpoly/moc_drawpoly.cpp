/****************************************************************************
** drawPoly meta object code from reading C++ file 'drawpoly.h'
**
** Created: Tue Jul 6 20:39:16 2010
**      by: The Qt MOC ($Id: qt/moc_yacc.cpp   3.3.6   edited Mar 8 17:43 $)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#undef QT_NO_COMPAT
#include "drawpoly.h"
#include <qmetaobject.h>
#include <qapplication.h>

#include <private/qucomextra_p.h>
#if !defined(Q_MOC_OUTPUT_REVISION) || (Q_MOC_OUTPUT_REVISION != 26)
#error "This file was generated using the moc from 3.3.6. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

const char *drawPoly::className() const
{
    return "drawPoly";
}

QMetaObject *drawPoly::metaObj = 0;
static QMetaObjectCleanUp cleanUp_drawPoly( "drawPoly", &drawPoly::staticMetaObject );

#ifndef QT_NO_TRANSLATION
QString drawPoly::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "drawPoly", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString drawPoly::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "drawPoly", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* drawPoly::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QWidget::staticMetaObject();
    static const QUParameter param_slot_0[] = {
	{ "paint", &static_QUType_ptr, "QPainter", QUParameter::In }
    };
    static const QUMethod slot_0 = {"showPoly", 1, param_slot_0 };
    static const QMetaData slot_tbl[] = {
	{ "showPoly(QPainter*)", &slot_0, QMetaData::Private }
    };
    metaObj = QMetaObject::new_metaobject(
	"drawPoly", parentObject,
	slot_tbl, 1,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_drawPoly.setMetaObject( metaObj );
    return metaObj;
}

void* drawPoly::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "drawPoly" ) )
	return this;
    return QWidget::qt_cast( clname );
}

bool drawPoly::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: showPoly((QPainter*)static_QUType_ptr.get(_o+1)); break;
    default:
	return QWidget::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool drawPoly::qt_emit( int _id, QUObject* _o )
{
    return QWidget::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool drawPoly::qt_property( int id, int f, QVariant* v)
{
    return QWidget::qt_property( id, f, v);
}

bool drawPoly::qt_static_property( QObject* , int , int , QVariant* ){ return FALSE; }
#endif // QT_NO_PROPERTIES
