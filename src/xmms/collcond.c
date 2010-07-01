/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2009 XMMS2 Team
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

#include "xmmspriv/xmms_collcond.h"
#include "xmmspriv/xmms_utils.h"
#include "xmms/xmms_log.h"

#include <string.h>

static int is_universe (xmmsv_coll_t *coll)
{
	char *target_name;

	xmmsv_coll_attribute_get (coll, "reference", &target_name);

	return (target_name != NULL && strcmp (target_name, "All Media") == 0);
}

static s4_condition_t *handle_filter_operands (xmms_coll_dag_t *dag, xmmsv_list_iter_t *it,
		s4_condition_t *cond, s4_sourcepref_t *sp)
{
	xmmsv_t *v;
	xmmsv_coll_t *coll;
	GList *operands = g_list_prepend (NULL, cond);

	for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
		xmmsv_list_iter_entry (it, &v);
		xmmsv_get_coll (v, &coll);

		if (!is_universe (coll)) {
			cond = xmms_coll_to_cond (dag, coll, sp);
			operands = g_list_prepend (operands, cond);
		}
	}

	return s4_cond_new_combiner (S4_COMBINE_AND, operands);
}

static int is_filter (xmmsv_coll_type_t type)
{
	int ret = 0;
	switch (type) {
		case XMMS_COLLECTION_TYPE_EQUALS:
		case XMMS_COLLECTION_TYPE_MATCH:
		case XMMS_COLLECTION_TYPE_GREATER:
		case XMMS_COLLECTION_TYPE_SMALLER:
		case XMMS_COLLECTION_TYPE_HAS:
			ret = 1;
			break;
		default:
			break;
	}

	return ret;
}

static int idlist_filter (s4_val_t *value, s4_condition_t *cond)
{
	int32_t ival;
	GHashTable *id_table = s4_cond_get_funcdata (cond);

	if (!s4_val_get_int (value, &ival))
		return 1;

	return g_hash_table_lookup (id_table, GINT_TO_POINTER (ival)) == NULL;
}

s4_condition_t *xmms_coll_to_cond (xmms_coll_dag_t *dag, xmmsv_coll_t *coll, s4_sourcepref_t *sp)
{
	xmmsv_list_iter_t *it;
	xmmsv_t *v;
	xmmsv_coll_t *c;
	char *key, *val;
	int i;
	int case_sens;
	int32_t ival;
	s4_condition_t *cond = NULL;
	GList *operands = NULL;
	s4_val_t *sval = NULL;
	GHashTable *id_table;
	int flags = 0;

	xmmsv_coll_attribute_get (coll, "case-sensitive", &val);
	case_sens = (val != NULL && strcmp (val, "true") == 0);

	if (case_sens)
		flags |= S4_COND_CASESENS;

	xmmsv_get_list_iter (xmmsv_coll_operands_get (coll), &it);

	switch (xmmsv_coll_get_type (coll)) {
		case XMMS_COLLECTION_TYPE_UNION:
			for (i = 0; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it), i++) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);

				operands = g_list_prepend (operands, xmms_coll_to_cond (dag, c, sp));
			}

			cond = s4_cond_new_combiner (S4_COMBINE_OR, operands);
			break;

		case XMMS_COLLECTION_TYPE_INTERSECTION:
			for (i = 0; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it), i++) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);

				operands = g_list_prepend (operands, xmms_coll_to_cond (dag, c, sp));
			}
			cond = s4_cond_new_combiner (S4_COMBINE_AND, operands);
			break;

		case XMMS_COLLECTION_TYPE_COMPLEMENT:
			for (i = 0; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it), i++) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);

				operands = g_list_prepend (operands, xmms_coll_to_cond (dag, c, sp));
			}
			cond = s4_cond_new_combiner (S4_COMBINE_NOT, operands);
			break;

		case XMMS_COLLECTION_TYPE_EQUALS:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			if (xmms_is_int (val, &ival)) {
				sval = s4_val_new_int (ival);
			} else {
				sval = s4_val_new_string (val);
			}

			cond = s4_cond_new_filter (S4_FILTER_EQUAL, key, sval, sp, flags);
			break;

		case XMMS_COLLECTION_TYPE_MATCH:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			if (xmms_is_int (val, &ival)) {
				sval = s4_val_new_int (ival);
			} else {
				sval = s4_val_new_string (val);
			}

			cond = s4_cond_new_filter (S4_FILTER_MATCH, key, sval, sp, flags);
			break;

		case XMMS_COLLECTION_TYPE_SMALLER:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			if (xmms_is_int (val, &ival)) {
				sval = s4_val_new_int (ival);
			} else {
				sval = s4_val_new_string (val);
			}

			cond = s4_cond_new_filter (S4_FILTER_SMALLER, key, sval, sp, flags);
			break;

		case XMMS_COLLECTION_TYPE_GREATER:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			if (xmms_is_int (val, &ival)) {
				sval = s4_val_new_int (ival);
			} else {
				sval = s4_val_new_string (val);
			}

			cond = s4_cond_new_filter (S4_FILTER_GREATER, key, sval, sp, flags);
			break;

		case XMMS_COLLECTION_TYPE_REFERENCE:
			if (is_universe (coll)) {
				sval = s4_val_new_string ("song");
				cond = s4_cond_new_filter (S4_FILTER_EQUAL, "type", sval, sp, flags);
			} else {
				xmmsv_coll_attribute_get (coll, "reference", &key);
				xmmsv_coll_attribute_get (coll, "namespace", &val);
				c = xmms_collection_get_pointer (dag, key,
						xmms_collection_get_namespace_id (val));
				cond = xmms_coll_to_cond (dag, c, sp);
			}
			break;

		case XMMS_COLLECTION_TYPE_IDLIST:
			id_table = g_hash_table_new (NULL, NULL);

			for (i = 0; xmmsv_coll_idlist_get_index (coll, i, &ival); i++) {
				g_hash_table_insert (id_table, GINT_TO_POINTER (ival), GINT_TO_POINTER (1));
			}

			cond = s4_cond_new_custom_filter (idlist_filter, id_table,
					(free_func_t)g_hash_table_destroy, "song_id", sp, S4_COND_PARENT);
			break;

		case XMMS_COLLECTION_TYPE_HAS:
			xmmsv_coll_attribute_get (coll, "field", &key);

			cond = s4_cond_new_filter (S4_FILTER_EXISTS, key, NULL, sp, flags);
			break;

		default:
			XMMS_DBG ("Do not support %i\n", xmmsv_coll_get_type (coll));
			break;
	}

	if (sval != NULL)
		s4_val_free (sval);

	if (is_filter (xmmsv_coll_get_type (coll))) {
		cond = handle_filter_operands (dag, it, cond, sp);
	}

	return cond;
}
