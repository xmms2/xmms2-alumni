#ifndef XMMSCLIENTPP_LIST_H
#define XMMSCLIENTPP_LIST_H

#include <xmmsclient/xmmsclient.h>
#include <boost/any.hpp>

#include <xmmsclient/xmmsclient++/dict.h>
#include <xmmsclient/xmmsclient++/typedefs.h>
#include <xmmsclient/xmmsclient++/exceptions.h>
#include <string>

#include <iostream>

namespace Xmms
{

	/** @class SuperList list.h "xmmsclient/xmmsclient++/list.h"
	 *  @brief Superclass for List classes.
	 */
	class SuperList
	{

		public:

			/** Constructor.
			 *
			 *  @param result xmmsc_result_t* returned by one of the
			 *         libxmmsclient functions, must be a list.
			 *
			 *  @throw result_error If the result was in error state.
			 *  @throw not_list_error If the result is not a list.
			 *
			 *  @note You must unref the result you feed to this class.
			 */
			SuperList( xmmsc_result_t* result );

			/** Copy-constructor.
			 */
			SuperList( const SuperList& list );

			/** Copy assignment operator.
			 */
			virtual SuperList& operator=( const SuperList& list );

			/** Destructor.
			 */
			virtual ~SuperList();

			/** Return to first entry in list.
			 *
			 *  @todo Should probably throw on error?
			 */
			virtual void first() const;

			/** Skip to next entry in list.
			 *
			 *  Advances to next list entry. May advance outside of list,
			 *  so isValid should be used to determine if end of list was
			 *  reached.
			 *
			 *  @todo Throw on error?
			 */
			virtual void operator++() const;

			/** Check if current listnode is inside list boundary.
			 */
			virtual bool isValid() const;

		protected:
			xmmsc_result_t* result_;

	};

	/** @class List list.h "xmmsclient/xmmsclient++/list.h"
	 *  @brief This class acts as a wrapper for list type results.
	 *  This is actually a virtual class and is specialized with T being
	 *  - std::string
	 *  - int
	 *  - unsigned int
	 *  - Dict
	 *
	 *  If any other type is used, a compile-time error should occur.
	 */
	template< typename T >
	class List : public SuperList
	{

		public:

			/** Constructor
			 *  @see SuperList#SuperList.
			 */
			List( xmmsc_result_t* result ) :
				SuperList( result )
			{
			}

			/** Copy-constructor.
			 */
			List( const List<T>& list ) :
				SuperList( list )
			{
			}

			/** Copy assignment operator.
			 */
			List<T>& operator=( const List<T>& list )
			{
				SuperList::operator=( list );
				return *this;
			}

			/** Destructor.
			 */
			virtual ~List()
			{
			}

			/** Operator *.
			 *  Used to get the underlying value from the list.
			 */
			const T operator*() const
			{
				return constructContents();
			}

			/** Operator ->.
			 *  Used to call a function of the underlying class
			 *  (only applicable for std::string and Dict).
			 *  Same as (*list).function();
			 */
			const T operator->() const
			{
				return constructContents();
			}
			
		/** @cond */
		private:

			virtual T constructContents() const = 0;
		/** @endcond */

	};

	/** @cond */
	template<>
	class List< int > : public SuperList
	{

		public:
			List( xmmsc_result_t* result ) :
				SuperList( result )
			{

				if( xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_INT32 &&
				    xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_NONE ) {

					// SuperList constructor refs the result so we'll unref
					xmmsc_result_unref( result );
					throw wrong_type_error( "Expected list of ints" );

				}

			}

			List( const List<int>& list ) :
				SuperList( list )
			{
			}

			List<int>& operator=( const List<int>& list )
			{   
				SuperList::operator=( list );
				return *this;
			}

			virtual ~List()
			{
			}

			const int operator*() const
			{
				return constructContents();
			}

			const int operator->() const
			{
				return constructContents();
			}

		private:

			virtual int constructContents() const
			{

				if( !isValid() ) {
					throw out_of_range( "List out of range or empty list" );
				}

				int temp = 0;
				if( !xmmsc_result_get_int( result_, &temp ) ) {
					// throw something
				}
				return temp;
				
			}

	};

	template<>
	class List< unsigned int > : public SuperList
	{

		public:
			List( xmmsc_result_t* result ) :
				SuperList( result )
			{

				if( xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_UINT32 &&
				    xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_NONE ) {

					// SuperList constructor refs the result so we'll unref
					xmmsc_result_unref( result );
					throw wrong_type_error( "Expected list of unsigned ints" );
				}

			}

			List( const List<unsigned int>& list ) :
				SuperList( list )
			{
			}

			List<unsigned int>& operator=( const List<unsigned int>& list )
			{
				SuperList::operator=( list );
				return *this;
			}

			virtual ~List()
			{
			}

			const unsigned int operator*() const
			{
				return constructContents();
			}

			const unsigned int operator->()
			{
				return constructContents();
			}

		private:

			virtual unsigned int constructContents() const
			{

				if( !isValid() ) {
					throw out_of_range( "List out of range or empty list" );
				}

				unsigned int temp = 0;
				if( !xmmsc_result_get_uint( result_, &temp ) ) {
					// throw something
				}
				return temp;
				
			}

	};

	template<>
	class List< std::string > : public SuperList
	{

		public:
			List( xmmsc_result_t* result ) :
				SuperList( result )
			{

				if( xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_STRING &&
				    xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_NONE ) {
					// SuperList constructor refs the result so we'll unref
					xmmsc_result_unref( result );
					throw wrong_type_error( "Expected list of strings" );
				}

			}

			List( const List<std::string>& list ) :
				SuperList( list )
			{
			}

			List<std::string>& operator=( const List<std::string>& list )
			{
				SuperList::operator=( list );
				return *this;
			}

			virtual ~List()
			{
			}

			const std::string operator*() const
			{
				return constructContents();
			}

			const std::string operator->() const
			{
				return constructContents();
			}

		private:

			virtual std::string constructContents() const
			{

				if( !isValid() ) {
					throw out_of_range( "List out of range or empty list" );
				}

				char* temp = 0;
				if( !xmmsc_result_get_string( result_, &temp ) ) {
					// throw something
				}
				return std::string( temp );
				
			}

	};

	template<>
	class List< Dict > : public SuperList
	{

		public:
			List( xmmsc_result_t* result ) try :
				SuperList( result )
			{
				// checking the type here is a bit useless since
				// Dict constructor checks it but we must catch it and
				// unref the result which SuperList refs or we leak.
			}
			catch( Xmms::not_dict_error& e )
			{
				if( xmmsc_result_get_type( result ) !=
				    XMMS_OBJECT_CMD_ARG_NONE ) {

					xmmsc_result_unref( result );
					throw;

				}
			}

			List( const List<Dict>& list ) :
				SuperList( list )
			{
			}

			List<Dict>& operator=( const List<Dict>& list )
			{
				SuperList::operator=( list );
				return *this;
			}

			virtual ~List()
			{
			}

			const Dict operator*() const
			{
				return constructContents();
			}

			const Dict operator->()
			{
				return constructContents();
			}

		private:

			virtual Dict constructContents() const
			{

				if( !isValid() ) {
					throw out_of_range( "List out of range or empty list" );
				}

				return Dict( result_ );

			}

	};
	/** @endcond */
}

#endif // XMMSCLIENTPP_LIST_H
