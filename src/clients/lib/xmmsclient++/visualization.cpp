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

#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient++/visualization.h>
#include <xmmsclient/xmmsclient++/mainloop.h>
#include <xmmsclient/xmmsclient++/helpers.h>

#include <string>
#include <map>

namespace Xmms
{

	Visualization::~Visualization()
	{
		xmmsc_visualization_shutdown( conn_, vis_ );
	}

	bool Visualization::start() const
	{
		return xmmsc_visualization_start( conn_, vis_ );
	}

	VoidResult Visualization::setProperty( const std::string& key,
	                                       const std::string& value ) const
	{
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_visualization_property_set, conn_, vis_,
		                       key.c_str(), value.c_str() ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Visualization::setProperties( const
	                                         std::map< std::string, std::string >&
	                                         pairs ) const
	{
		const char** props = c_stringMapList( pairs );
		xmmsc_result_t* res =
		    call( connected_,
		          boost::bind( xmmsc_visualization_properties_set, conn_, vis_,
		                       props ) );
		delete [] props;
		return VoidResult( res, ml_ );
	}

	int Visualization::getChunk( short *buffer, int drawtime,
	                             unsigned int blocking ) const
	{
		return xmmsc_visualization_chunk_get( conn_, vis_, buffer, drawtime,
		                                      blocking );
	}

	Visualization::Visualization( xmmsc_connection_t*& conn, bool& connected,
	                              MainloopInterface*& ml ) :
		conn_( conn ), connected_( connected ), ml_( ml )
	{
		xmmsc_result_t* res = xmmsc_visualization_init( conn );
		xmmsc_result_wait( res );
		if (xmmsc_result_iserror( res ) ) {
			puts( xmmsc_result_get_error( res ) );
			exit( EXIT_FAILURE );
		}
		vis_ = xmmsc_visualization_init_handle( res );
	}

}

