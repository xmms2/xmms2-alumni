/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2011 XMMS2 Team
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

#ifndef __XMMS_RESULT_H__
#define __XMMS_RESULT_H__

#include <QObject>
#include <QPointer>
#include <QVariant>
#include <QList>
#include <QMap>
#include <QByteArray>

#include "message.h"

#include "propdict.h"

#undef __DEBUG_RESULT__

#ifdef __DEBUG_RESULT__
#define DBGRES(fmt, ...) qDebug("DEBUG RESULT: " fmt, ## __VA_ARGS__)
#else
#define DBGRES(fmt, ...)
#endif

namespace XMMSQt
{

	class Client;

	class ResultBase : public QObject
	{
		Q_OBJECT
		public:
			ResultBase (const ResultBase &src) : QObject (src.parent ())
			{
				m_cookie = src.cookie ();
				m_object = src.object ();
				m_slot = src.slot ();
				m_errobject = src.m_errobject;
				m_errslot = src.m_errslot;
				m_client = src.client ();
				m_message = src.message ();
				m_restartsignal = src.m_restartsignal;
				m_broadcast = src.m_broadcast;
				m_apierr = src.m_apierr;
			};

			ResultBase (Client *, int cookie);

//			Result () : QObject () {};

			ResultBase (QString apierr = "Wrong use of class Result");

			ResultBase &
			operator= (const ResultBase &src)
			{
				m_cookie = src.cookie ();
				m_object = src.object ();
				m_slot = src.slot ();
				m_errobject = src.m_errobject;
				m_errslot = src.m_errslot;
				m_client = src.client ();
				m_message = src.message ();
				m_restartsignal = src.m_restartsignal;
				m_broadcast = src.m_broadcast;
				m_apierr = src.m_apierr;
				return *this;
			};

			void operator() (QObject *object, const char *slot);
			void operator() (QObject *object, const char *slot,
			                 const char *errslot);
			void operator() (QObject *object, const char *slot,
			                 QObject *errobject, const char *errslot);

			int cookie () const { return m_cookie; };
			void setCookie (int c) {
				m_cookie = c;
			};

			const char *slot () const {
				return m_slot;
			};

			QObject *object () const {
				return m_object;
			};

			Client *client () const {
				return m_client;
			};

			Message message () const {
				return m_message;
			};

			void setRestartSignal (quint32 rsignal)
			{
				DBGRES ("got restart signal %d", rsignal);
				m_restartsignal = rsignal;
			};

			void setBroadcast (bool b)
			{
				m_broadcast = b;
			};

			void exec (const Message &);

			virtual void execResult (const QVariant &v)
			{
				qWarning ("execResult doesn't work yet");
			}

		protected:
			int m_cookie;

			const char *m_slot;
			QPointer<QObject> m_object;

			const char *m_errslot;
			QPointer<QObject> m_errobject;

			Client *m_client;
			Message m_message;

			QString m_apierr;

			void setSlots (QObject *object, const char *slot,
			               QObject *errobject, const char *errslot);

			quint32 m_restartsignal;
			bool m_broadcast;
	};

	typedef ResultBase Result;

	// Normally, Templates and the Qt metaobject approach dont mix very well
	// So I try to seperate QT metacalls to a base class and the templates to
	// a derived class
	// It's a 'Hack and Pray' approach and might not work

	template < typename T >
	class ResultT : public ResultBase
	{
		public:
			ResultT< T > (const ResultBase &src) : ResultBase (src) {}

			void execResult (const QVariant &v)
			{
				if (m_object && m_slot)
				{
					QByteArray slotname (QMetaObject::normalizedSignature (m_slot));
					qDebug (slotname);
					qint32 methidx = m_object->metaObject ()->indexOfMethod (slotname.constData ());

					if (methidx == -1 ) {
#ifndef __clang__ // currently clang does not like this
						qWarning ("Class %s has no slot '%s'",
						          m_object->metaObject ()->className (),
						          slot.constData ());
#endif
						return;
					}

					if (!v.canConvert< T >()) {
						qWarning ("Cannot convert type");
						return;
					}

					const T val = v.value<T>();

					qDebug ("TODO");

				}

				if (v.canConvert< T >())
				{
					const T val = v.value<T>();
				}
				else
				{
					qDebug ("Wrong resulttype");
				}
			}
	};

	template <>
	class ResultT<void> : public ResultBase
	{
		public:
			ResultT< void > (const ResultBase &src) : ResultBase (src) {}

			void execResult (const QVariant &v)
			{
				//Q_ASSERT (v.type () == QVariant::Invalid);
			}
	};

}


// TODO: somehow generate this too:

Q_DECLARE_METATYPE (QList<qint32>)
Q_DECLARE_METATYPE (QList<QVariantMap>)
// Qt doesn't like ',' in Q_DECLARE_METATYPE
typedef QMap<QString, qint32> QIntMap;
Q_DECLARE_METATYPE (QIntMap)
typedef QMap<QString, QString> QStringMap;
Q_DECLARE_METATYPE (QStringMap)
typedef QMap<QString, QVariantMap> QVariantVariantMap;
Q_DECLARE_METATYPE (QVariantVariantMap)


#endif
