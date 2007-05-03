#include "perl_xmmsclient.h"

MODULE = Audio::XMMSClient::Collection	PACKAGE = Audio::XMMSClient::Collection	PREFIX = xmmsc_coll_

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

NO_OUTPUT int
xmmsc_coll_parse (class, const char *pattern, OUTLIST xmmsc_coll_t *coll)
	INIT:
		PERL_UNUSED_VAR (targ);
	C_ARGS:
		pattern, &coll
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;

void
DESTROY (coll)
		xmmsc_coll_t *coll
	CODE:
		xmmsc_coll_unref (coll);

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
		}

		ids[items - 1] = 0;
	C_ARGS:
		coll, ids
	CLEANUP:
		free (ids);

void
xmmsc_coll_add_operand (coll, op)
		xmmsc_coll_t *coll
		xmmsc_coll_t *op

void
xmmsc_coll_remove_operand (coll, op)
		xmmsc_coll_t *coll
		xmmsc_coll_t *op

int
xmmsc_coll_idlist_append (coll, id)
		xmmsc_coll_t *coll
		unsigned int id

int
xmmsc_coll_idlist_insert (coll, index, id)
		xmmsc_coll_t *coll
		unsigned int index
		unsigned int id

int
xmmsc_coll_idlist_move (coll, from, to)
		xmmsc_coll_t *coll
		unsigned int from
		unsigned int to

int
xmmsc_coll_idlist_clear (coll)
		xmmsc_coll_t *coll

NO_OUTPUT int
xmmsc_coll_idlist_get_index (xmmsc_coll_t *coll, unsigned int index, OUTLIST uint32_t val)
	INIT:
		PERL_UNUSED_VAR (targ);
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;

int
xmmsc_coll_idlist_set_index (coll, index, val)
		xmmsc_coll_t *coll
		unsigned int index
		uint32_t val

size_t
xmmsc_coll_idlist_get_size (coll)
		xmmsc_coll_t *coll

xmmsc_coll_type_t
xmmsc_coll_get_type (coll)
		xmmsc_coll_t *coll

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
			XPUSHs (perl_xmmsclient_new_sv_from_ptr (op, "Audio::XMMSClient::Collection"));
		}

int
xmmsc_coll_operand_list_first (coll)
		xmmsc_coll_t *coll

int
xmmsc_coll_operand_list_valid (coll)
		xmmsc_coll_t *coll

NO_OUTPUT int
xmmsc_coll_operand_list_entry (xmmsc_coll_t *coll, OUTLIST xmmsc_coll_t *op)
	INIT:
		PERL_UNUSED_VAR (targ);
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;

int
xmmsc_coll_operand_list_next (coll)
		xmmsc_coll_t *coll

int
xmmsc_coll_operand_list_save (coll)
		xmmsc_coll_t *coll

int
xmmsc_coll_operand_list_restore (coll)
		xmmsc_coll_t *coll

void
xmmsc_coll_attribute_set (coll, key, value)
		xmmsc_coll_t *coll
		const char *key
		const char *value

int
xmmsc_coll_attribute_remove (coll, key)
		xmmsc_coll_t *coll
		const char *key

NO_OUTPUT int
xmmsc_coll_attribute_get (xmmsc_coll_t *coll, const char *key, OUTLIST char *val)
	INIT:
		PERL_UNUSED_VAR (targ);
	POSTCALL:
		if (RETVAL == 0)
			XSRETURN_UNDEF;

xmmsc_coll_t *
xmmsc_coll_universe (class="optional")
	C_ARGS:
		/* void */

BOOT:
	PERL_UNUSED_VAR (items);
