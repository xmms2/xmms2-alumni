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

#ifndef XMMSCLIENTPP_VISUALIZATION_H
#define XMMSCLIENTPP_VISUALIZATION_H

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient++/mainloop.h>
#include <xmmsclient/xmmsclient++/helpers.h>
#include <xmmsclient/xmmsclient++/result.h>

#include <string>
#include <map>

namespace Xmms
{

	class Client;

	/** @class Visualization visualization.h "xmmsclient/xmmsclient++/visualization.h"
	 *  @brief This class represents a visualization instance.
	 */

	class Visualization
	{
		public:

			/** Destructor.  Destroy the object to shutdown a visualization instance.
			 */
			virtual ~Visualization();

			/** Initialize the visualization connection.
			 *
			 *  @return The success status (see the server error in case of failure).
			 */
			bool start() const;

			/** Set the value for a given visualization property.
			 *
			 *  @param key The name of the property.
			 *  @param value The new value for the property.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult setProperty( const std::string& key,
			                        const std::string& value ) const;

			/** Set the values for multiple visualization properties.
			 *
			 *  @param pairs Property key/value pairs.
			 *
			 *  @throw connection_error If the client isn't connected.
			 *  @throw mainloop_running_error If a mainloop is running -
			 *  sync functions can't be called when mainloop is running. This
			 *  is only thrown if the programmer is careless or doesn't know
			 *  what he/she's doing. (logic_error)
			 *  @throw result_error If the operation failed.
			 */
			VoidResult setProperties( const
			                          std::map< std::string, std::string >&
			                          pairs ) const;

			/** Fetches the next available data chunk.
			 *
			 *  If drawtime >= 0, the data is returned as soon as
			 *  currenttime >= (playtime - drawtime); data is thrown
			 *  away if playtime < currenttime, but not if playtime <
			 *  currenttime - drawtime.
			 *
			 *  If drawtime < 0, the data is returned as soon as
			 *  available, and no old data is thrown away.
			 *
			 *  The process will sleep until new data is available, or
			 *  the blocking time limit is reached. But if data is
			 *  found, it could still wait until it is current (see
			 *  drawtime).
			 *
			 *  Note: the size read can be less than expected (for
			 *  example, on song end). Check it!
			 *
			 *  @param buffer Buffer where the data is stored.
			 *  @param drawtime Expected time needed to process the
			 *                  data in milliseconds after collecting it.
			 *  @param blocking Time limit in milliseconds to wait for data.
			 *  @return The size read on success, -1 on failure
			 *          (server killed!) and 0 if no data is available
			 *          yet (retry later). If a signal is received
			 *          while waiting for data, 0 is returned, even
			 *          before time ran out.
			 */
			int getChunk( short *buffer, int drawtime,
			              unsigned int blocking ) const;

		/** @cond */
		private:
			friend class Client;
			Visualization( xmmsc_connection_t*& conn, bool& connected,
			               MainloopInterface*& ml );

			xmmsc_connection_t*& conn_;
			bool& connected_;
			MainloopInterface*& ml_;

			int vis_;

		/** @endcond */
	};

}

#endif // XMMSCLIENTPP_VISUALIZATION_H
