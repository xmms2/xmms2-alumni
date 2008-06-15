#include "perl_xmmsclient.h"

MODULE = Audio::XMMSClient::Collection	PACKAGE = Audio::XMMSClient::Collection	PREFIX = xmmsc_coll_

=head1 NAME

Audio::XMMSClient::Collection - Media collections for Audio::XMMSClient

=head1 SYNOPSIS

  use Audio::XMMSClient;

  my $coll = Audio::XMMSClient::Collection->new('match', {
          artist => 'Solche',
  });

=head1 METHODS

=head2 new

=over 4

=item Arguments: $type, \%attributes?

=item Arguments: $type, %attributes?

=item Return Value: $collection

=back

  my $coll = Audio::XMMSClient::Collection->new('match', {
          artist => 'Solche',
  });

Create a new collection instance of type C<$type>. Also sets the
collections C<%attributes>, if given.

=cut

xmmsc_coll_t *
xmmsc_coll_new (class, type, ...)
		xmmsc_coll_type_t type
	PREINIT:
		int i, nargs;
		HV *args;
		HE *iter;
	CODE:
		RETVAL = xmmsc_coll_new (type);

		nargs = items - 2;
		if (nargs == 1) {
			if (!SvOK (ST (2)) || !SvROK (ST (2)) || !(SvTYPE (SvRV (ST (2))) == SVt_PVHV))
				croak ("expected hash reference or hash");

			args = (HV *)SvRV (ST (2));

			hv_iterinit (args);
			while ((iter = hv_iternext (args))) {
				xmmsc_coll_attribute_set (RETVAL, HePV (iter, PL_na), SvPV_nolen (HeVAL (iter)));
			}
		}
		else {
			if (nargs % 2 != 0)
				croak ("expected even number of attributes/values");

			for (i = 2; i <= nargs; i += 2) {
				xmmsc_coll_attribute_set (RETVAL, SvPV_nolen (ST (i)), SvPV_nolen (ST (i+1)));
			}
		}
	OUTPUT:
		RETVAL

=head2 parse

=over 4

=item Arguments: $pattern

=item Return Value: $collection

=back

  my $coll = Audio::XMMSClient::Collection->parse("artist:Solche +compilation");

Try to parse the given C<$pattern> to produce a collection structure.

=cut

xmmsc_coll_t *
xmmsc_coll_parse (class, const char *pattern)
	PREINIT:
		int ret;
	CODE:
		ret = xmmsc_coll_parse (pattern, &RETVAL);
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;
	OUTPUT:
		RETVAL

void
DESTROY (coll)
		xmmsc_coll_t *coll
	CODE:
		xmmsc_coll_unref (coll);

=head2 set_idlist

=over 4

=item Arguments: @ids

=item Return Value: none

=back

  $coll->set_idlist(qw/1 42 1337/);

Set the list of ids in the given collection. Note that the idlist is only
relevant for idlist collections.

=cut

void
xmmsc_coll_set_idlist (coll, ...)
		xmmsc_coll_t *coll
	PREINIT:
		int i;
		unsigned int *ids;
	INIT:
		ids = (unsigned int *)malloc (sizeof (unsigned int) * items);

		for (i = 0; i < items - 1; i++) {
			ids[i] = SvUV (ST (i + 1));
			if (ids[i] == 0) {
				free (ids);
				croak("0 is an invalid mlib id");
			}
		}

		ids[items - 1] = 0;
	C_ARGS:
		coll, ids
	CLEANUP:
		free (ids);

=head2 add_operand

=over 4

=item Arguments: $operand

=item Return Value: none

=back

  $coll->add_operand($other_coll);

Add the C<$operand> to a given collection.

=cut

void
xmmsc_coll_add_operand (coll, op)
		xmmsc_coll_t *coll
		xmmsc_coll_t *op

=head2 remove_operand

=over 4

=item Arguments: $operand

=item Return Value: none

=back

  $coll->remove_operand($other_coll);

Remove all the occurences of the C<$operand> in the given collection.

=cut

void
xmmsc_coll_remove_operand (coll, op)
		xmmsc_coll_t *coll
		xmmsc_coll_t *op

=head2 idlist_append

=over 4

=item Arguments: $id

=item Return Value: $success

=back

  my $success = $coll->idlist_append(5);

Append an C<$id> to the idlist.

=cut

int
xmmsc_coll_idlist_append (coll, id)
		xmmsc_coll_t *coll
		unsigned int id
	INIT:
		if (id == 0) {
			croak ("0 is an invalid mlib id");
		}

=head2 idlist_insert

=over 4

=item Arguments: $index, $id

=item Return Value: $success

=back

  my $success = $coll->idlist_insert(42, 2);

Insert an C<$id> at a given C<$index> in the idlist.

=cut

int
xmmsc_coll_idlist_insert (coll, index, id)
		xmmsc_coll_t *coll
		unsigned int index
		unsigned int id
	INIT:
		if (index > xmmsc_coll_idlist_get_size (coll)) {
			croak ("inserting id after end of idlist");
		}

		if (id == 0) {
			croak ("0 is an invalid mlib id");
		}

=head2 idlist_move

=over 4

=item Arguments: $from, $to

=item Return Value: $success

=back

  my $success = $coll->idlist_move(0, 3);

Move a value of the idlist to a new position.

=cut

int
xmmsc_coll_idlist_move (coll, from, to)
		xmmsc_coll_t *coll
		unsigned int from
		unsigned int to
	PREINIT:
		size_t idlist_len;
	INIT:
		idlist_len = xmmsc_coll_idlist_get_size (coll);

		if (from > idlist_len) {
			croak ("trying to move id from after the idlists end");
		}

		if (to >= idlist_len) {
			croak ("trying to move id to after the idlists end");
		}

=head2 idlist_clear

=over 4

=item Arguments: none

=item Return Value: $success

=back

  my $success = $coll->idlist_clear;

Empties the idlist.

=cut

int
xmmsc_coll_idlist_clear (coll)
		xmmsc_coll_t *coll

=head2 idlist_get_index

=over 4

=item Arguments: $index

=item Return Value: $value | undef

=back

  my $value = $coll->idlist_get_index(2);

Retrieves the value at the given C<$index> in the idlist.

=cut

NO_OUTPUT int
xmmsc_coll_idlist_get_index (xmmsc_coll_t *coll, unsigned int index, OUTLIST uint32_t val)
	INIT:
		PERL_UNUSED_VAR (targ);

		if (index > xmmsc_coll_idlist_get_size (coll)) {
			croak ("trying to get an id from behind the idlists end");
		}
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;

=head2 idlist_set_index

=over 4

=item Arguments: $index, $value

=item Return Value: $success

=back

  my $success = $coll->idlist_set_index(3, 1);

Sets the C<$value> at the given C<$index> in the idlist.

=cut

int
xmmsc_coll_idlist_set_index (coll, index, val)
		xmmsc_coll_t *coll
		unsigned int index
		uint32_t val
	PREINIT:
		size_t idlist_len;
	INIT:
		idlist_len = xmmsc_coll_idlist_get_size (coll);
		if (idlist_len == 0 || index > idlist_len - 1) {
			croak ("trying to set an id after the end of the idlist");
		}

=head2 idlist_get_size

=over 4

=item Arguments: none

=item Return Value: $size

=back

  my $size = $coll->idlist_get_size;

Get the size of the idlist.

=cut

size_t
xmmsc_coll_idlist_get_size (coll)
		xmmsc_coll_t *coll

=head2 get_type

=over 4

=item Arguments: none

=item Return Value: $type

=back

  my $type = $coll->get_type;

Return the type of the collection. Valid types are "reference", "union",
"intersection", "complement", "has", "equals", "match", "smaller", "greater",
"idlist", "queue", "partyshuffle".

=cut

xmmsc_coll_type_t
xmmsc_coll_get_type (coll)
		xmmsc_coll_t *coll

=head2 get_idlist

=over 4

=item Arguments: none

=item Return Value: @ids

=back

  my @ids = $coll->get_idlist;

Return the list of ids stored in the collection. Note that this must not be
confused with the content of the collection, which must be queried using
L<Audio::XMMSClient/coll_query_ids>.

=cut

void
xmmsc_coll_get_idlist (coll)
		xmmsc_coll_t *coll
	PREINIT:
		uint32_t *ret;
		size_t size;
		unsigned int i = 0;
	PPCODE:
		ret = xmmsc_coll_get_idlist (coll);

		if (ret == NULL)
			XSRETURN_UNDEF;

		size = xmmsc_coll_idlist_get_size (coll);
		EXTEND (sp, size);

		while (ret[i] != 0) {
			PUSHs (sv_2mortal (newSVuv (ret[i])));
			++i;
		}

=head2 operands

=over 4

=item Arguments: none

=item Return Value: @operands

=back

  my @operands = $coll->operands;

Get a list of operands of the collection.

=cut

void
operands (coll)
		xmmsc_coll_t *coll
	ALIAS:
		operand_list = 1
	PREINIT:
		xmmsc_coll_t *op;
	PPCODE:
		PERL_UNUSED_VAR (ix);

		for (xmmsc_coll_operand_list_first (coll);
		     xmmsc_coll_operand_list_entry (coll, &op);
		     xmmsc_coll_operand_list_next (coll)) {
			xmmsc_coll_ref (op);
			XPUSHs (sv_2mortal (perl_xmmsclient_new_sv_from_ptr (op, "Audio::XMMSClient::Collection")));
		}

=head2 operand_list_first

=over 4

=item Arguments: none

=item Return Value: $success

=back

  my $success = $coll->operand_list_first;

Move the internal pointer of the operand list to the first operand.

=cut

int
xmmsc_coll_operand_list_first (coll)
		xmmsc_coll_t *coll

=head2 operand_list_valid

=over 4

=item Arguments: none

=item Return Value: $is_valid

=back

  my $is_valid = $coll->operand_list_valid;

Checks if the internal pointer points to a valid operand of the list.

=cut

int
xmmsc_coll_operand_list_valid (coll)
		xmmsc_coll_t *coll

=head2 operand_list_entry

=over 4

=item Arguments: none

=item Return Value: $collection | undef

=back

  my $op = $coll->operand_list_entry;

Return a collection representing the current operand in the list.

=cut

NO_OUTPUT int
xmmsc_coll_operand_list_entry (xmmsc_coll_t *coll, OUTLIST xmmsc_coll_t *op)
	INIT:
		PERL_UNUSED_VAR (targ);
	POSTCALL:
		xmmsc_coll_ref (op);

		if (RETVAL == 0)
			XSRETURN_UNDEF;

=head2 operand_list_next

=over 4

=item Arguments: none

=item Return Value: $success

=back

  my $success = $coll->operand_list_next;

Move forward the internal pointer of the operand list.

=cut

int
xmmsc_coll_operand_list_next (coll)
		xmmsc_coll_t *coll

=head2 operand_list_save

=over 4

=item Arguments: none

=item Return Value: $success

=back

  my $success = $coll->operand_list_save;

Save the position of the operand iterator, to be restored later by calling
L</operand_list_restore>.

Note that the iterator is not tested for consistency, so you better be careful
if the list of operands was manipulated since the iterator was saved!

=cut

int
xmmsc_coll_operand_list_save (coll)
		xmmsc_coll_t *coll

=head2 operand_list_restore

=over 4

=item Arguments: none

=item Return Value: $success

=back

  my $success = $coll->operand_list_restore;

Restore the position of the operand iterator, previously saved by calling
L</operand_list_save>.

Note that the iterator is not tested for consistency, so you better be careful
if the list of operands was manipulated since the iterator was saved!

=cut

int
xmmsc_coll_operand_list_restore (coll)
		xmmsc_coll_t *coll

=head2 attribute_set

=over 4

=item Arguments: $key, $value

=item Return Value: none

=back

  $coll->attribute_set(field => 'artist');

Set an attribute C<$key> to C<$value> in the given collection.

=cut

void
xmmsc_coll_attribute_set (coll, key, value)
		xmmsc_coll_t *coll
		const char *key
		const char *value

=head2 attribute_remove

=over 4

=item Arguments: $key

=item Return Value: $success

=back 

  my $success = $coll->attribute_remove;

Remove an attribute C<$key> from the given collection. The return value
indicated whether the attribute was found (and removed).

=cut

int
xmmsc_coll_attribute_remove (coll, key)
		xmmsc_coll_t *coll
		const char *key

=head2 attribute_get

=over 4

=item Arguments: $key

=item Return Value: $value | undef

=back

  my $value = $coll->attribute_get('field');

Retrieve the C<$value> of the attribute C<$key> of the given collection. 

=cut

NO_OUTPUT int
xmmsc_coll_attribute_get (xmmsc_coll_t *coll, const char *key, OUTLIST char *val)
	INIT:
		PERL_UNUSED_VAR (targ);
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;

=head2 attribute_list

=over 4

=item Arguments: none

=item Return Value: %attributes

=back

  my %attributes = $coll->attributes_list;

Get a hash of all C<%attributes> of the given collection.

=cut

void
xmmsc_coll_attribute_list (xmmsc_coll_t *coll)
	PREINIT:
		const char *key;
		const char *value;
	PPCODE:
		xmmsc_coll_attribute_list_first (coll);

		while (xmmsc_coll_attribute_list_valid (coll)) {
			xmmsc_coll_attribute_list_entry (coll, &key, &value);

			EXTEND (sp, 2);
			mPUSHp (key, strlen (key));
			mPUSHp (value, strlen (value));

			xmmsc_coll_attribute_list_next (coll);
		}

=head2 universe

=over 4

=item Arguments: none

=item Return Value: $collection

=back

  my $universe = Audio::XMMSClient::Collection->universe;

Returns a collection referencing the whole media library, i.e. the "All Media"
collection.

=cut

xmmsc_coll_t *
xmmsc_coll_universe (class="optional")
	C_ARGS:
		/* void */

=head1 AUTHOR

Florian Ragwitz <rafl@debian.org>

=head1 SEE ALSO

L<Audio::XMMSClient>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2006-2008, Florian Ragwitz

This library is free software; you can redistribute it and/or modify it under
the same terms as Perl itself, either Perl version 5.8.8 or, at your option,
any later version of Perl 5 you may have available.

=cut

BOOT:
	PERL_UNUSED_VAR (items);
