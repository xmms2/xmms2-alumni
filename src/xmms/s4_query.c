#include <s4.h>
#include <xmmsclient/xmmsclient.h>
#include <string.h>
#include <stdlib.h>
//#include "xmmspriv/s4_query.h"
#include "xmmspriv/xmms_utils.h"
#include <xmmsc/xmmsv.h>
#include <xmmspriv/xmms_collection.h>


/**
 *
 * @defgroup Query Query
 * @ingroup S4
 * @brief Handles collection queries in S4
 *
 * @{
 */

/*
static s4_set_t *universe (s4_t *s4) {
	s4_entry_t *entry = s4_entry_get_s (s4, "type", "song");
	s4_set_t *ret = s4_entry_contained (s4, entry);
	s4_entry_free (entry);
	return ret;
}
*/

static int is_universe (xmmsv_coll_t *coll)
{
	char *target_name;

	xmmsv_coll_attribute_get (coll, "reference", &target_name);

	return (target_name != NULL && strcmp (target_name, "All Media") == 0);
}

/*
static s4_set_t *handle_filter_operands (s4_t *s4, xmms_coll_dag_t *dag, xmmsv_list_iter_t *it, s4_set_t *set)
{
	xmmsv_t *v;
	xmmsv_coll_t *coll;
	s4_set_t *sa, *sb;

	for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
		xmmsv_list_iter_entry (it, &v);
		xmmsv_get_coll (v, &coll);

		if (!is_universe (coll)) {
			sb = s4_query (s4, dag, coll);
			sa = set;
			set = s4_set_intersection (set, sb);
			s4_set_free (sa);
			s4_set_free (sb);
		}
	}

	return set;
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
*/

int idlist_filter (s4_val_t *value, s4_condition_t *cond)
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
	uint32_t *idlist;
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
			idlist = xmmsv_coll_get_idlist (coll);
			id_table = g_hash_table_new (NULL, NULL);

			for (i = 0; idlist[i] != 0; i++) {
				g_hash_table_insert (id_table, GINT_TO_POINTER (idlist[i]), GINT_TO_POINTER (1));
			}

			cond = s4_cond_new_custom_filter (idlist_filter, id_table,
					(free_func_t)g_hash_table_destroy, "song_id", sp, S4_COND_PARENT);
			break;

		case XMMS_COLLECTION_TYPE_HAS:
			xmmsv_coll_attribute_get (coll, "field", &key);

			cond = s4_cond_new_filter (S4_FILTER_EXISTS, key, NULL, sp, flags);
			break;

		default:
			printf ("Do not support %i\n", xmmsv_coll_get_type (coll));
			break;
	}

	if (sval != NULL)
		s4_val_free (sval);

	/*
	if (is_filter (xmmsv_coll_get_type (coll))) {
		ret = handle_filter_operands (s4, dag, it, ret);
	}
	*/


	return cond;
}

/**
 * Query the database with the given collection
 *
 * @param s4 The database handle
 * @param dag The collection dag
 * @param coll The collection to query
 * @return A set with all the entries matching
 */
#if 0
s4_set_t *s4_query (s4_t *s4, xmms_coll_dag_t *dag, xmmsv_coll_t *coll)
{
	s4_set_t *ret, *sa, *sb;
	xmmsv_list_iter_t *it;
	xmmsv_t *v;
	xmmsv_coll_t *c;
	char *key, *val;
	s4_entry_t *entry;
	int32_t ival;
	uint32_t *idlist;
	int first, i;
	int case_sens;

	xmmsv_coll_attribute_get (coll, "case-sensitive", &val);
	case_sens = (val != NULL && strcmp (val, "true") == 0);

	xmmsv_get_list_iter (xmmsv_coll_operands_get (coll), &it);

	ret = NULL;

	switch (xmmsv_coll_get_type (coll)) {
		case XMMS_COLLECTION_TYPE_UNION:
			for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);
				sa = ret;
				sb = s4_query (s4, dag, c);
				ret = s4_set_union (sa, sb);
				s4_set_free (sa);
				s4_set_free (sb);
			}
			break;

		case XMMS_COLLECTION_TYPE_INTERSECTION:
			first = 1;
			for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);
				sb = s4_query (s4, dag, c);

				if (first) {
					ret = sb;
					first = 0;
				} else {
					sa = ret;
					ret = s4_set_intersection (sa, sb);
					s4_set_free (sa);
					s4_set_free (sb);
				}
			}
			break;

		case XMMS_COLLECTION_TYPE_COMPLEMENT:
			if (xmmsv_list_iter_valid (it)) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);
				sa = s4_query (s4, dag, c);
			} else {
				sa = NULL;
			}
			sb = universe (s4);
			ret = s4_set_complement (sa, sb);
			s4_set_free (sa);
			s4_set_free (sb);
			break;

		case XMMS_COLLECTION_TYPE_EQUALS:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			/* id gets special treatment */
			if (strcmp (key, "id") == 0) {
				entry = s4_entry_get_i (s4, "song_id", atoi (val));

				/* Check if it exists */
				sa = s4_entry_contains (s4, entry);

				if (s4_set_size (sa) > 0) {
					ret = s4_set_new (0);
					s4_set_insert (ret, entry);
				}
				s4_set_free (sa);
			} else {
				if (case_sens) {
					sa = s4_set_new (0);
					entry = s4_entry_get_s (s4, key, val);
					s4_set_insert (sa, entry);
				} else {
					sa = s4_entry_get_entries (s4, key, val);
				}
				s4_set_reset (sa);

				while ((entry = s4_set_next (sa)) != NULL) {
					sb = s4_entry_contained (s4, entry);
					if (ret != NULL) {
						s4_set_t *tmp = ret;
						ret = s4_set_union (tmp, sb);
						s4_set_free (tmp);
						s4_set_free (sb);
					} else {
						ret = sb;
					}
				}

				s4_set_free (sa);

				/* If it is an int we should search for integer entries too */
				if (xmms_is_int (val, &ival)) {
					entry = s4_entry_get_i (s4, key, ival);
					sa = ret;
					sb = s4_entry_contained (s4, entry);
					ret = s4_set_union (sa, sb);
					s4_set_free (sa);
					s4_set_free (sb);
					s4_entry_free (entry);
				}
			}
			break;

		case XMMS_COLLECTION_TYPE_MATCH:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			/* Get all string entries with key=key */
			entry = s4_entry_get_i (s4, key, INT32_MIN);
			s4_entry_fillin (s4, entry);
			entry->key_i = -entry->key_i;
			sa = s4_entry_greater (s4, entry, 1);

			/* Find all entries matching val */
			ret = s4_entry_match (s4, sa, val, case_sens);

			s4_set_free (sa);
			s4_entry_free (entry);

			break;

		case XMMS_COLLECTION_TYPE_SMALLER:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			/* We have to treat id a little differently */
			if (strcmp (key, "id") == 0) {
				int ival = atoi (val);
				/* TODO: Make s4_entry_smaller work with the reverse tree too */
				sa = universe (s4);
				ret = s4_set_new (0);

				while ((entry = s4_set_next (sa)) != NULL) {
					if (entry->val_i < ival) {
						s4_set_insert (ret, entry);
					} else {
						break;
					}
				}
			} else {
				entry = s4_entry_get_i (s4, key, atoi(val));
				ret = s4_entry_smaller (s4, entry, 0);
				s4_entry_free (entry);
			}
			break;
		case XMMS_COLLECTION_TYPE_GREATER:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			/* We have to treat id a little differently */
			if (strcmp (key, "id") == 0) {
				int ival = atoi (val);
				/* TODO: Make s4_entry_greater work with the reverse tree too */
				sa = universe (s4);
				ret = s4_set_new (0);

				while ((entry = s4_set_next (sa)) != NULL) {
					if (entry->val_i > ival) {
						s4_set_insert (ret, entry);
					}
				}
			} else {
				entry = s4_entry_get_i (s4, key, atoi(val));
				ret = s4_entry_greater (s4, entry, 0);
				s4_entry_free (entry);
			}

			break;

		case XMMS_COLLECTION_TYPE_REFERENCE:
			if (is_universe (coll)) {
				ret = universe (s4);
			} else {
				xmmsv_coll_attribute_get (coll, "reference", &key);
				xmmsv_coll_attribute_get (coll, "namespace", &val);
				c = xmms_collection_get_pointer (dag, key,
						xmms_collection_get_namespace_id (val));
				ret = s4_query (s4, dag, c);
			}
			break;

		case XMMS_COLLECTION_TYPE_IDLIST:
			idlist = xmmsv_coll_get_idlist (coll);

			ret = s4_set_new (0);

			for (i = 0; idlist[i] != 0; i++) {
				s4_entry_t entry;
				entry.type = ENTRY_INT;
				entry.key_s = strdup ("song_id");
				entry.key_i = 0;
				entry.val_s = NULL;
				entry.val_i = idlist[i];
				entry.src_s = strdup ("server");
				entry.src_i = 0;

				s4_entry_fillin (s4, &entry);

				s4_set_insert (ret, &entry);
			}
			break;

		case XMMS_COLLECTION_TYPE_HAS:
			xmmsv_coll_attribute_get (coll, "field", &key);
			entry = s4_entry_get_i (s4, key, INT32_MIN);
			s4_entry_fillin (s4, entry);

			/* Find all integer entries with key=key and values greater
			 * than INT32_MIN.
			 */
			sa = s4_entry_greater (s4, entry, 0);

			/* Integer entries has a negative key_i, we make it positive
			 * and search for all string entries with key=key and values
			 * greater than INT32_MIN.
			 */
			entry->key_i = -entry->key_i;
			sb = s4_entry_greater (s4, entry, 0);

			ret = s4_set_union (sa, sb);

			s4_set_free (sa);
			s4_set_free (sb);
			s4_entry_free (entry);

			break;

		default:
			printf ("Do not support %i\n", xmmsv_coll_get_type (coll));
			break;
	}

	if (is_filter (xmmsv_coll_get_type (coll))) {
		ret = handle_filter_operands (s4, dag, it, ret);
	}


	return ret;
}
#endif
/**
 * @}
 */
