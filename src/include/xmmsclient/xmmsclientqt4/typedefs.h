#ifndef __XMMSCLIENTPP_TYPEDEFS_H__
#define __XMMSCLIENTPP_TYPEDEFS_H__

#include <xmmsc/xmmsc_idnumbers.h>

namespace XMMSQt
{
        namespace Coll {
                class Coll;
        }
		class Message;
}

namespace XMMSQt
{
	typedef xmmsc_coll_namespace_t CollNamespace;
	static const CollNamespace COLLECTION_ALL         = XMMS_COLLECTION_NS_ALL;
	static const CollNamespace COLLECTION_COLLECTIONS = XMMS_COLLECTION_NS_COLLECTIONS;
	static const CollNamespace COLLECTION_PLAYLISTS   = XMMS_COLLECTION_NS_PLAYLISTS;
}

#endif // XMMSCLIENTPP_TYPEDEFS_H
