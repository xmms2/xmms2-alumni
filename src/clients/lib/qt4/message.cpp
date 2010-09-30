/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2010 XMMS2 Team
 *
 *  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 */

#include <xmmsclient/xmmsclientqt4/message.h>

#include <QFile>
#include <QStringList>
#include <qendian.h>

#include <xmmsc/xmmsv.h>
#include <xmmsclient/xmmsclientqt4/propdict.h>
#include <xmmsclient/xmmsclientqt4/client.h>
#include <xmmsclient/xmmsclientqt4/result.h>
#include <xmmsclient/xmmsclientqt4/coll.h>

// #include "compat_p.h"

namespace XMMSQt
{

	Message::Message (quint32 object, quint32 cmd, const QVariantList &list)
	{
		m_object = object;
		m_cmd = cmd;
		m_headercomplete = false;
		m_stream = new QDataStream (&m_bytearray, QIODevice::ReadWrite);
		m_list = list;
	}

	bool
	Message::processHeader (const QByteArray &b)
	{
		if (b.size () != 16) {
			qWarning ("Didn't get 16 bytes from parent!");
			return false;
		}

		QDataStream r (b);
		if (!headerComplete ()) {
			r >> m_object;
			r >> m_cmd;
			r >> m_cookie;
			r >> m_length;
			m_headercomplete = true;
			DBGIPC ("header is complete! the message is %d", m_length);
			return true;
		}
		return false;
	}


	bool
	Message::process (QIODevice *r)
	{
		if (m_bytearray.size () < m_length) {
			int readsize = m_length - m_bytearray.size ();
			QByteArray b = r->read (readsize);
			DBGIPC ("read %d wanted %d", b.size (), readsize);
			m_bytearray += b;
		}

		if (m_bytearray.size () == m_length) {
	#ifdef __DEBUG_IPC_TO_FILE__
			QFile fp("/tmp/dbg");
			fp.open (QIODevice::WriteOnly);
			fp.write (m_bytearray);
			fp.close ();
	#endif
			return true;
		} else {
			return false;
		}
	}

	bool
	Message::put (const QVariant &v, QDataStream *stream) const
	{
		switch (v.type ())
		{
			case QVariant::Int:
			case QVariant::UInt:
				return put (v.toInt (), stream);

			case QVariant::String:
				return put (v.toString (), stream);

			case QVariant::ByteArray:
				return put (v.toByteArray (), stream);

			case QVariant::List:
				return put (v.toList (), stream);

			case QVariant::StringList:
				return put (v.toStringList (), stream);

			case QVariant::Map:
				return put (v.toMap (), stream);

			default:
				return false;
		}
	}

	//TODO: XMMSV_TYPE_NONE
	//TODO: XMMSV_TYPE_ERROR

	bool
	Message::put (const qint32 val, QDataStream *stream) const
	{
		*stream << (qint32)XMMSV_TYPE_INT32;
		*stream << val;
		return true;
	}

	bool
	Message::put (const QString &s, QDataStream *stream) const
	{
		QByteArray b = s.toUtf8 ();
		b.append ('\0');

		*stream << (qint32)XMMSV_TYPE_STRING;
		*stream << (qint32)b.size();
		stream->writeRawData (b.data(), b.size());
		return true;
	}

	// TODO: XMMSV_TYPE_COLL

	bool
	Message::put (const QByteArray &b, QDataStream *stream) const
	{
		*stream << (qint32)XMMSV_TYPE_BIN;
		*stream << (qint32)b.size ();
		stream->writeRawData (b.data (), b.size ());
		return true;
	}

	bool
	Message::put (const QVariantList &list, QDataStream *stream) const
	{
		*stream << (qint32)XMMSV_TYPE_LIST;
		*stream << (qint32)list.size();
		foreach (QVariant v, list) {
			if (!put (v, stream))
				return false;
		}
		return true;
	}

	bool
	Message::put (const QStringList &list, QDataStream *stream) const
	{
		*stream << (qint32)XMMSV_TYPE_LIST;
		*stream << (qint32)list.size();
		foreach (QString s, list) {
			if (!put (s, stream))
				return false;
		}
		return true;
	}

	bool
	Message::put (const QVariantMap &m, QDataStream *stream) const
	{
		*stream << (quint32)XMMSV_TYPE_LIST;
		*stream << (quint32)m.size ();
		QVariantMap::const_iterator i = m.constBegin ();
		while (i != m.constEnd ())
		{
			if (!put (i.key (), stream))
				return false;
			if (!put (i.value (), stream))
				return false;
			++i;
		}
		return true;
	}

	// TODO: XMMSV_TYPE_END

	QByteArray
	Message::finish (quint32 cookie) const
	{
		QByteArray retarray;
		QDataStream ret (&retarray, QIODevice::WriteOnly);
		ret.setByteOrder (QDataStream::BigEndian);

		QByteArray params;
		QDataStream p_stream (&params, QIODevice::ReadWrite);

		put (m_list, &p_stream);

		ret << m_object;
		ret << m_cmd;
		ret << cookie;
		ret << params.size ();
		ret.writeRawData (params.data (), params.size ());

		return retarray;
	}


	Coll::Coll *
	Message::getColl ()
	{
		qint32 type;
		*m_stream >> type;
		if (type != XMMSV_TYPE_COLL) {
			qWarning ("wanted type %d but got %d", XMMSV_TYPE_COLL, type);
			return 0;
		}

		quint32 t;
		*m_stream >> t;
		Coll::Coll *ret = new Coll::Coll ((Coll::Type) t);

		/* attr list */
		quint32 len;
		*m_stream >> len;
		qDebug ("num of attrs: %d", len);
		QMap<QString, QString> attr;
		for (quint32 i = 0; i < len; i += 2)
		{
			QString key = getString (false);
			QString val = getString (false);
			qDebug ("got attribute %s=%s", qPrintable (key), qPrintable (val));
			attr[key] = val;
		}
		ret->setAttributeList (attr);

		/* idlist */
		len = 0;
		*m_stream >> len;
		QList<quint32> idlist;
		for (quint32 i = 0; i < len; i++)
		{
			quint32 val;
			*m_stream >> val;
			idlist.append (val);
		}
		ret->setIdList (idlist);

		/* operand list */
		len = 0;
		*m_stream >> len;
		QList<Coll::Coll *> oplst;
		for (quint32 i = 0; i < len; i ++)
		{
			oplst.append (getColl ());
		}
		ret->setOperandList (oplst);

		return ret;
	}

	QVariant
	Message::getValue ()
	{
		qint32 type;
		*m_stream >> type;
		switch (type) {
			case XMMSV_TYPE_INT32:
				qint32 i;
				*m_stream >> i;
				return QVariant (i);
			case XMMSV_TYPE_STRING:
				{
					QString s = getString (false);
					return QVariant (s);
				}
			// For DrM and later
			case XMMSV_TYPE_DICT:
				{
					return QVariant (getDict (type));
				}
			case XMMSV_TYPE_LIST:
			case XMMSV_TYPE_COLL:
			// FIXME
				qWarning ("Message::getValue not yet supported for LIST and" \
				          "COLL");
				break;
			default:
				qWarning ("Message::getValue(): Type not handled: %u", type);
				return QVariant ();
		}
		return QVariant ();
	}

	quint32
	Message::getUInt32 ()
	{
		qint32 type;
		*m_stream >> type;
		if (type != XMMSV_TYPE_UINT32 && type != XMMSV_TYPE_INT32) {
			qWarning ("wanted type %d but got %d", XMMSV_TYPE_UINT32,
			          type);
			return 0;
		}
		quint32 r;
		*m_stream >> r;
		return r;
	}

	QVariantMap
	Message::getDict (qint32 type) {
		if (type == TYPE_UNDEFINED) {
			*m_stream >> type;
		}

		switch (type) {
			case XMMSV_TYPE_DICT:
			{
				QVariantMap ret;
				// Dict and PropDict have different serialization forms
				quint32 size;
				*m_stream >> size;
				for (quint32 i = 0; i < size; i ++)
				{
					QString key = getString (false);
					QVariant value = getValue ();
					ret[key] = value;
				}
				return ret;
			}
			break;
			default:
				qWarning ("Called Message::getDict with unsupported type: %i",
				          type);
				break;
		}
		return QVariantMap ();
	}

	PropDict
	Message::getPropDict (qint32 type)
	{
		if (type == TYPE_UNDEFINED) {
			*m_stream >> type;
		}

		switch (type) {
			case XMMSV_TYPE_DICT:
				return PropDict (getDict (type));
			default:
				qWarning ("Called Message::getPropDict with unsupported " \
				          "type: %i", type);
		}
		return PropDict ();
	}

	QVariantList
	Message::getList (const bool &checktype)
	{
		if (checktype) {
			qint32 type;
			*m_stream >> type;
			if (type != XMMSV_TYPE_LIST) {
				qWarning ("wanted type %d but got %d", XMMSV_TYPE_LIST,
				          type);
				return QList<QVariant> ();
			}
		}

		qint32 size;
		*m_stream >> size;

		QVariantList ret;
		for (int i = 0; i < size; i ++)
		{
			ret.append (getValue ());
		}

		return ret;
	}

	qint32
	Message::getInt32 ()
	{
		qint32 type;
		*m_stream >> type;
		if (type != XMMSV_TYPE_INT32) {
			qWarning ("wanted type %d but got %d", XMMSV_TYPE_INT32,
			          type);
			return 0;
		}
		qint32 r;
		*m_stream >> r;
		return r;
	}

	qreal
	Message::getReal ()
	{
		qWarning ("getting qReal, not supported");
		return 0;
	}

	QString
	Message::getString (const bool &checktype)
	{
		if (checktype) {
			qint32 type;
			*m_stream >> type;
			if (type != XMMSV_TYPE_STRING) {
				qWarning ("wanted type %d but got %d",
				          XMMSV_TYPE_STRING, type);
				return QString ();
			}
		}
		quint32 len;
		*m_stream >> len;
		if (len > fullLength ()) {
			qWarning ("broken lenght, wanted %d but we only have %d", len,
			          fullLength ());
			return QString ();
		}
		char *str = (char *) malloc (len + 1);
		m_stream->readRawData (str, len);
		QString r = QString::fromUtf8 (str);
		delete str;

		return r;
	}

	QByteArray
	Message::getBindata ()
	{
		qint32 type;
		*m_stream >> type;
		if (type != XMMSV_TYPE_BIN) {
			qWarning ("wanted type %d but got %d", XMMSV_TYPE_BIN,
			          type);
			return QByteArray ();
		}

		quint32 len;
		*m_stream >> len;
		if (len > fullLength ()) {
			qWarning ("broken lenght, wanted %d but we only have %d", len,
			          fullLength ());
			return QByteArray ();
		}
		char *buf = (char *) malloc (len);
		m_stream->readRawData (buf, len);
		QByteArray r (buf, len);
		return r;
	}

}
