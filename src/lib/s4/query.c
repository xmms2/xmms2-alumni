#include <s4.h>
#include "src/s4_be.h"
#include <xmmsclient/xmmsclient.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "s4_query.h"


/**
 *
 * @defgroup Query Query
 * @ingroup S4
 * @brief Handles collection queries in S4
 *
 * @{
 */

static s4_set_t *universe (s4_t *s4) {
	s4_entry_t *entry = s4_entry_get_s (s4, "type", "song");
	s4_set_t *ret = s4_entry_contained (s4, entry);
	s4_entry_free (entry);
	return ret;
}

static int is_int (const char *str, int *val)
{
	int ret = 0;
	char *end;

	if (!isspace (*str)) {
		*val = strtol (str, &end, 10);
		if (*end == '\0')
			ret = 1;
	}

	return ret;
}


/**
 * Query the database with the given collection
 *
 * @param s4 The database handle
 * @param dag The collection dag
 * @param coll The collection to query
 * @return A set with all the entries matching
 */
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

			entry = s4_entry_get_s (s4, key, val);
			ret = s4_entry_contained (s4, entry);
			s4_entry_free (entry);

			/* If it is an int we should search for integer entries too */
			if (is_int (val, &ival)) {
				entry = s4_entry_get_i (s4, key, ival);
				sa = ret;
				sb = s4_entry_contained (s4, entry);
				ret = s4_set_union (sa, sb);
				s4_set_free (sa);
				s4_set_free (sb);
				s4_entry_free (entry);
			}
			break;

		case XMMS_COLLECTION_TYPE_MATCH:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			GList *tmp, *list = s4be_st_match (s4->be, val);
			tmp = list;

			while (list != NULL) {
				entry = s4_entry_get_s (s4, key, list->data);
				sa = ret;
				sb = s4_entry_contained (s4, entry);
				ret = s4_set_union (sa, sb);
				s4_set_free (sa);
				s4_set_free (sb);
				s4_entry_free (entry);

				free (list->data);
				list = g_list_next (list);
			}

			g_list_free (tmp);

			break;

		case XMMS_COLLECTION_TYPE_SMALLER:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			entry = s4_entry_get_i (s4, key, atoi(val));
			ret = s4_entry_smaller (s4, entry);
			s4_entry_free (entry);

			break;
		case XMMS_COLLECTION_TYPE_GREATER:
			xmmsv_coll_attribute_get (coll, "field", &key);
			xmmsv_coll_attribute_get (coll, "value", &val);

			entry = s4_entry_get_i (s4, key, atoi(val));
			ret = s4_entry_greater (s4, entry);
			s4_entry_free (entry);

			break;

		case XMMS_COLLECTION_TYPE_REFERENCE:
			xmmsv_coll_attribute_get (coll, "reference", &key);
			xmmsv_coll_attribute_get (coll, "namespace", &val);

			if (key != NULL && strcmp (key, "All Media") == 0) {
				ret = universe (s4);
			} else {
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
			sa = s4_entry_greater (s4, entry);

			/* Integer entries has a negative key_i, we make it positive
			 * and search for all string entries with key=key and values
			 * greater than INT32_MIN.
			 */
			entry->key_i = -entry->key_i;
			sb = s4_entry_greater (s4, entry);

			ret = s4_set_union (sa, sb);
			s4_set_free (sa);
			s4_set_free (sb);

			break;

		default:
			printf ("Do not support %i\n", xmmsv_coll_get_type (coll));
			break;
	}

	return ret;
}

/**
 * @}
 */
