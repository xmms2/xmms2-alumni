/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2007 XMMS2 Team
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

#ifndef XMMSCLIENTPP_MEDIALIB_H
#define XMMSCLIENTPP_MEDIALIB_H

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient++/mainloop.h>
#include <xmmsclient/xmmsclient++/result.h>

#include <string>
#include <list>

namespace Xmms
{

	class Client;

	/** @class Medialib medialib.h "xmmsclient/xmmsclient++/medialib.h"
	 *  @brief This class controls the medialib.
	 */
	class Medialib
	{

		public:

			/** Destructor. */
			~Medialib();

			/** Add a URL to the medialib.
			 *  If you want to add multiple files you should call pathImport.
			 *
			 *  @param url URL to add to the medialib.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult addEntry( const std::string& url ) const;

			/** Add a URL with arguments to the medialib.
			 *  If you want to add multiple files you should call pathImport.
			 *
			 *  @param url URL to add to the medialib.
			 *  @param args List of strings used as arguments.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult addEntry( const std::string& url,
			                     const std::list< std::string >& args ) const;

			/** Add a URL to the medialib.
			 *  Same as #addEntry but takes a encoded URL instead.
			 *
			 *  @param url URL to add to the medialib.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult addEntryEncoded( const std::string& url ) const;

			/** Remove a custom field in the medialib associated with an entry.
			 *
			 *  The optional @c source parameter will default to
			 *  client/@<clientname@> if not provided.
			 *
			 *  @param id Entry ID.
			 *  @param key Field key to remove.
			 *  @param source Source for the entry. (<b>optional</b>)
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult entryPropertyRemove( unsigned int id,
			                                const std::string& key,
			                                const std::string& source = "" ) const;

			/** Associate a value with a medialib entry.
			 *
			 *  The optional @c source parameter will default to
			 *  client/@<clientname@> if not provided.
			 *
			 *  @param id Entry ID.
			 *  @param key Field key to set.
			 *  @param value to set.
			 *  @param source Source for the value. (<b>optional</b>)
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult entryPropertySet( unsigned int id,
			                             const std::string& key,
			                             const std::string& value,
			                             const std::string& source = "" ) const;

			/**
			 * @overload
			 * @note It takes a int instead of string
			 */
			VoidResult entryPropertySet( unsigned int id,
			                             const std::string& key,
			                             int value,
			                             const std::string& source = "" ) const;

			/** Search for a entry (URL) in the medialib db
			 *  and return its ID number.
			 *
			 *  @param url The URL to search for.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 *  
			 *  @return ID number
			 */
			UintResult getID( const std::string& url ) const;

			/** Retrieve information about a entry from the medialib.
			 *
			 *  @param id ID of the entry.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */  
			PropDictResult getInfo( unsigned int id ) const;

			/** Import all files recursively from the 
			 *  directory passed as argument.
			 *
			 *  @param path Directory to import.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult pathImport( const std::string& path ) const;

			/** Import all files recursively from the 
			 *  directory passed as argument.
			 *
			 *  same as #pathImport but takes a encoded path instead.
			 *
			 *  @param path Directory to import.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult pathImportEncoded( const std::string& path ) const;

			/** Rehash the medialib. 
			 *  This will check data in the medialib still 
			 *  is the same as the data in files.
			 *
			 *  @param id The ID to rehash. (<b>optional</b>, 
			 *                               default is to rehash all)
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult rehash( unsigned int id = 0 ) const;

			/** Remove an entry from the medialib.
			 *  
			 *  @param id ID of the entry to remove.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult removeEntry( unsigned int id ) const;

			/** Make a SQL query to the server medialib.
			 *  
			 *  @param query SQL query.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 *
			 *  @return List of @link Dict Dicts@endlink
			 */
			DictListResult select( const std::string& query ) const;

			/** Request the medialib entry added broadcast.
			 *
			 *  This will be called if a new entry is added to
			 *  the medialib serverside.
			 * 
			 *  @param slot Function pointer to a function taking a
			 *              const unsigned int& and returning a bool.
			 *  @param error Function pointer to an error callback
			 *               function. (<b>optional</b>)
			 *
			 *  @throw connection_error If the client isn't connected.
			 */
			UintSignal broadcastEntryAdded() const;

			/** Request the medialib entry changed broadcast.
			 *
			 *  This will be called if a entry changes on the serverside.
			 *  The argument will be an medialib id. 
			 *  
			 *  @param slot Function pointer to a function taking a
			 *              const unsigned int& and returning a bool.
			 *  @param error Function pointer to an error callback
			 *               function. (<b>optional</b>)
			 *
			 *  @throw connection_error If the client isn't connected.
			 */
			UintSignal broadcastEntryChanged() const;

		/** @cond */
		private:
			friend class Client;
			Medialib( xmmsc_connection_t*& conn, bool& connected,
			          MainloopInterface*& ml );

			Medialib( const Medialib& src );
			Medialib& operator=( const Medialib& src );

			xmmsc_connection_t*& conn_;
			bool& connected_;
			MainloopInterface*& ml_;
		/** @endcond */

	};

}

#endif // XMMSCLIENTPP_MEDIALIB_H
