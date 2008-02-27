/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
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

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient++/client.h>
#include <xmmsclient/xmmsclient++/playlist.h>
#include <xmmsclient/xmmsclient++/exceptions.h>
#include <xmmsclient/xmmsclient++/signal.h>
#include <xmmsclient/xmmsclient++/helpers.h>
#include <xmmsclient/xmmsclient++/mainloop.h>

#include <boost/bind.hpp>

#include <string>
#include <list>

namespace Xmms
{

	const std::string Playlist::DEFAULT_PLAYLIST = XMMS_ACTIVE_PLAYLIST;


	Playlist::~Playlist()
	{
	}

	StringListResult Playlist::list() const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_list, conn_ ) );
		return StringListResult( res, ml_ );
	}

	VoidResult Playlist::create( const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_create, conn_, playlist.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::load( const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_load, conn_, playlist.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addRecursive( const std::string& url,
	                                   const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_radd, conn_,
		                       playlist.c_str(), url.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addRecursiveEncoded( const std::string& url,
	                                          const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_radd_encoded, conn_,
		                       playlist.c_str(), url.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addUrl( const std::string& url,
	                             const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_add_url, conn_,
		                       playlist.c_str(), url.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addUrl( const std::string& url,
	                             const std::list< std::string >& args,
	                             const std::string& playlist ) const
	{
		std::vector< const char* > cargs;
		fillCharArray( args, cargs );

		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_add_args, conn_,
		                       playlist.c_str(), url.c_str(),
		                       args.size(), &cargs[0] ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addUrlEncoded( const std::string& url,
	                                    const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_add_encoded, conn_,
		                       playlist.c_str(), url.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addId( unsigned int id,
	                            const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_add_id, conn_,
		                       playlist.c_str(), id ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::addCollection( const Coll::Coll& collection,
	                                    const std::list< std::string >& order,
	                                    const std::string& playlist ) const
	{
		std::vector< const char* > corder;
		fillCharArray( order, corder );

		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_add_collection, conn_,
		                       playlist.c_str(), collection.getColl(),
		                       &corder[0] ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::clear( const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_clear, conn_, playlist.c_str() ) );
		return VoidResult( res, ml_ );
	}

	UintResult Playlist::currentPos( const std::string& playlist ) const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playlist_current_pos, conn_,
		                       playlist.c_str() ) );
		return UintResult( res, ml_ );
	}

	StringResult Playlist::currentActive() const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_current_active, conn_ ) );
		return StringResult( res, ml_ );
	}

	VoidResult Playlist::insertUrl( int pos, const std::string& url,
	                                const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_insert_url, conn_,
		                       playlist.c_str(), pos, url.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::insertUrl( int pos, const std::string& url,
	                                const std::list< std::string >& args,
	                                const std::string& playlist ) const
	{
		std::vector< const char* > cargs;
		fillCharArray( args, cargs );

		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_insert_args, conn_,
		                       playlist.c_str(), pos, url.c_str(),
		                       args.size(), &cargs[0] ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::insertUrlEncoded( int pos, const std::string& url,
	                                       const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_insert_encoded, conn_,
		                       playlist.c_str(), pos, url.c_str() ) );
		return VoidResult( res, ml_ );
	}


	VoidResult Playlist::insertId( int pos, unsigned int id,
	                               const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_insert_id, conn_,
		                       playlist.c_str(), pos, id ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::insertCollection( int pos, const Coll::Coll& collection,
	                                       const std::list< std::string >& order,
	                                       const std::string& playlist ) const
	{
		std::vector< const char* > corder;
		fillCharArray( order, corder );

		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_insert_collection, conn_,
		                       playlist.c_str(), pos, collection.getColl(),
		                       &corder[0] ) );
		return VoidResult( res, ml_ );
	}

	UintListResult Playlist::listEntries( const std::string& playlist ) const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playlist_list_entries, conn_,
		                       playlist.c_str() ) );
		return UintListResult( res, ml_ );
	}

	VoidResult Playlist::moveEntry( unsigned int curpos, unsigned int newpos,
	                                const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_move_entry, conn_,
		                       playlist.c_str(), curpos, newpos ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::removeEntry( unsigned int pos,
	                                  const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_remove_entry, conn_,
		                       playlist.c_str(), pos ) );
		return VoidResult( res, ml_ );
	}

	UintResult Playlist::setNext( unsigned int pos ) const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playlist_set_next, conn_, pos ) );
		return UintResult( res, ml_ );
	}

	UintResult Playlist::setNextRel( signed int pos ) const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playlist_set_next_rel, conn_, pos ) );
		return UintResult( res, ml_ );
	}

	VoidResult Playlist::shuffle( const std::string& playlist ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_shuffle, conn_, playlist.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playlist::sort( const std::list<std::string>& properties,
	                           const std::string& playlist ) const
	{
		const char** props = c_stringList( properties );
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_playlist_sort, conn_,
		                       playlist.c_str(), props ) );
		delete [] props;
		return VoidResult( res, ml_ );
	}

	DictSignal Playlist::broadcastChanged() const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_broadcast_playlist_changed, conn_ ) );
		return DictSignal( res, ml_ );
	}

	UintSignal Playlist::broadcastCurrentPos() const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_broadcast_playlist_current_pos, conn_ ) );
		return UintSignal( res, ml_ );
	}

	StringSignal Playlist::broadcastLoaded() const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_broadcast_playlist_loaded, conn_ ) );
		return StringSignal( res, ml_ );
	}

	Playlist::Playlist( xmmsc_connection_t*& conn, bool& connected,
	                    MainloopInterface*& ml ) :
		conn_( conn ), connected_( connected ), ml_( ml )
	{
	}

}
