#include "s4.h"
#include "s4_be.h"
#include <xmmsclient/xmmsclient.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>


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

s4_set_t *_coll_to_set (s4_t *s4, xmmsv_coll_t *coll)
{
	s4_set_t *ret, *sa, *sb;
	xmmsv_list_iter_t *it;
	xmmsv_t *v;
	xmmsv_coll_t *c;
	const char *key, *val;
	s4_entry_t *entry;
	int32_t ival;

	xmmsv_get_list_iter (xmmsv_coll_operands_get (coll), &it);

	ret = NULL;

	switch (xmmsv_coll_get_type (coll)) {
		case XMMS_COLLECTION_TYPE_UNION:
			for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);
				sa = ret;
				sb = _coll_to_set (s4, c);
				ret = s4_set_union (sa, sb);
			}
			break;

		case XMMS_COLLECTION_TYPE_INTERSECTION:
			for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);
				sb = _coll_to_set (s4, c);

				if (ret != NULL) {
					sa = ret;
					ret = s4_set_intersection (sa, sb);
				}
				else
					ret = sb;
			}
			break;

		case XMMS_COLLECTION_TYPE_EQUALS:
			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "field", &v);
			xmmsv_get_string (v, &key);

			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "value", &v);
			xmmsv_get_string (v, &val);

			entry = s4_entry_get_s (s4, key, val);
			ret = s4_entry_contained (s4, entry);
			s4_entry_free (entry);

			/* If it is an int we should search for integer entries too */
			if (is_int (val, &ival)) {
				entry = s4_entry_get_i (s4, key, ival);
				ret = s4_set_union (ret, s4_entry_contained (s4, entry));
				s4_entry_free (entry);
			}
			break;

		case XMMS_COLLECTION_TYPE_MATCH:
			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "field", &v);
			xmmsv_get_string (v, &key);

			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "value", &v);
			xmmsv_get_string (v, &val);

			/* Get rid of prefix and suffix *, they break the regexp */
			char *pat = strdup (val + 1);
			pat[strlen (pat) - 1] = '\0';

			xmmsv_t *list = s4be_st_regexp (s4->be, pat);
			free (pat);
			xmmsv_t *str;
			xmmsv_list_iter_t *it;
			const char *s;

			xmmsv_get_list_iter (list, &it);
			while (xmmsv_list_iter_valid (it)) {
				xmmsv_list_iter_entry (it, &str);
				xmmsv_get_string (str, &s);

				entry = s4_entry_get_s (s4, key, s);
				sa = ret;
				sb = s4_entry_contained (s4, entry);
				ret = s4_set_union (sa, sb);
				s4_entry_free (entry);

				xmmsv_list_iter_next (it);
			}

			xmmsv_list_clear (list);
			xmmsv_unref (list);
			break;

		case XMMS_COLLECTION_TYPE_SMALLER:
			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "field", &v);
			xmmsv_get_string (v, &key);

			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "value", &v);
			xmmsv_get_string (v, &val);

			entry = s4_entry_get_i (s4, key, atoi(val));
			ret = s4_entry_smaller (s4, entry);
			s4_entry_free (entry);

			break;
		case XMMS_COLLECTION_TYPE_GREATER:
			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "field", &v);
			xmmsv_get_string (v, &key);

			xmmsv_dict_get (xmmsv_coll_attributes_get (coll), "value", &v);
			xmmsv_get_string (v, &val);

			entry = s4_entry_get_i (s4, key, atoi(val));
			ret = s4_entry_greater (s4, entry);
			s4_entry_free (entry);

			break;

		default:
			printf ("Do not support %i\n", xmmsv_coll_get_type (coll));
			break;
	}

	return ret;
}


s4_set_t *s4_query (s4_t *s4, const char *query)
{
	xmmsv_coll_t *coll;
	s4_set_t *ret;

	if (!xmmsv_coll_parse (query, &coll)) {
		printf ("The query '%s' could not be parsed\n", query);
		return NULL;
	}

	ret = _coll_to_set (s4, coll);
	xmmsv_coll_unref (coll);

	return ret;
}
