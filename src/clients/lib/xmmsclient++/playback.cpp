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
#include <xmmsclient/xmmsclient++/client.h>
#include <xmmsclient/xmmsclient++/playback.h>
#include <xmmsclient/xmmsclient++/signal.h>
#include <xmmsclient/xmmsclient++/helpers.h>
#include <xmmsclient/xmmsclient++/mainloop.h>
#include <xmmsclient/xmmsclient++/typedefs.h>
#include <xmmsclient/xmmsclient++/dict.h>

#include <boost/bind.hpp>

#include <iostream>
#include <string>

namespace Xmms
{
	
	Playback::~Playback()
	{
	}

	VoidResult Playback::tickle() const
	{
		xmmsc_result_t* res =
			call( connected_, boost::bind( xmmsc_playback_tickle, conn_ ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::stop() const
	{
		xmmsc_result_t* res =
			call( connected_, boost::bind( xmmsc_playback_stop, conn_ ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::pause() const
	{
		xmmsc_result_t* res =
			call( connected_, boost::bind( xmmsc_playback_pause, conn_ ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::start() const
	{
		xmmsc_result_t* res =
			call( connected_, boost::bind( xmmsc_playback_start, conn_ ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::seekMs(unsigned int milliseconds) const
	{
		xmmsc_result_t* res =
			call( connected_,
			      boost::bind( xmmsc_playback_seek_ms, conn_, milliseconds ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::seekMsRel(int milliseconds) const
	{
		xmmsc_result_t* res =
			call( connected_,
			      boost::bind( xmmsc_playback_seek_ms_rel,
			                   conn_, milliseconds ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::seekSamples(unsigned int samples) const
	{
		xmmsc_result_t* res =
			call( connected_,
			      boost::bind( xmmsc_playback_seek_samples, conn_, samples ) );
		return VoidResult( res, ml_ );
	}

	VoidResult Playback::seekSamplesRel(int samples) const
	{
		xmmsc_result_t* res =
			call( connected_,
			      boost::bind( xmmsc_playback_seek_samples_rel,
			                   conn_, samples ) );
		return VoidResult( res, ml_ );
	}

	UintResult Playback::currentID() const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playback_current_id, conn_ ) );
		return UintResult( res, ml_ );
	}

	StatusResult Playback::getStatus() const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playback_status, conn_ ) );

		return StatusResult( res, ml_ );
	}

	UintResult Playback::getPlaytime() const
	{
		xmmsc_result_t* res = 
		    call( connected_, boost::bind( xmmsc_playback_playtime, conn_ ) );
		return UintResult( res, ml_ );
	}

	VoidResult Playback::volumeSet(const std::string& channel,
	                               unsigned int volume) const
	{
		xmmsc_result_t* res =
			call( connected_,
			      boost::bind( xmmsc_playback_volume_set, conn_,
			                   channel.c_str(), volume ) );
		return VoidResult( res, ml_ );
	}

	DictResult Playback::volumeGet() const
	{
		xmmsc_result_t* res = 
		    call( connected_,
		          boost::bind( xmmsc_playback_volume_get, conn_ ) );
		return DictResult( res, ml_ );
	}

	Playback::Playback( xmmsc_connection_t*& conn, bool& connected,
	                    MainloopInterface*& ml ) :
		conn_( conn ), connected_( connected ), ml_( ml )
	{
	}

}
