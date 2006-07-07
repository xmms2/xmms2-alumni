#include <xmmsclient/xmmsclient.h>
#include <xmmsclient/xmmsclient++/listener.h>

namespace Xmms
{

	bool
	ListenerInterface::operator==( const ListenerInterface& rhs ) const
	{
		return ( getFileDescriptor() == rhs.getFileDescriptor() );
	}



	Listener::Listener( const Listener& src )
		: ListenerInterface(), conn_( src.conn_ )
	{
	}

	Listener& Listener::operator=( const Listener& src )
	{
		conn_ = src.conn_;
		return *this;
	}

	Listener::~Listener()
	{
	}

	int32_t
	Listener::getFileDescriptor() const
	{
		return xmmsc_io_fd_get( conn_ );
	}

	bool
	Listener::listenIn() const
	{
		return true;
	}

	bool
	Listener::listenOut() const
	{
		return xmmsc_io_want_out( conn_ );
	}

	void
	Listener::handleIn()
	{
		xmmsc_io_in_handle( conn_ );
	}

	void
	Listener::handleOut()
	{
		xmmsc_io_out_handle( conn_ );
	}

	Listener::Listener( xmmsc_connection_t*& conn ) :
		conn_( conn )
	{
	}

}
