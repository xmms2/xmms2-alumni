#include "s4.h"
#include <xmmsclient/xmmsclient.h>
#include <string.h>


s4_set_t *_coll_to_set (s4_t *s4, xmmsv_coll_t *coll)
{
	s4_set_t *ret, *sa, *sb;
	xmmsv_list_iter_t *it;
	xmmsv_t *v;
	xmmsv_coll_t *c;
	xmmsv_dict_iter_t *dt;

	const char *foo, *bar, *key, *val;
	s4_entry_t *entry;

	xmmsv_get_list_iter (xmmsv_coll_operands_get (coll), &it);
	xmmsv_get_dict_iter (xmmsv_coll_attributes_get (coll), &dt);

	ret = NULL;

	switch (xmmsv_coll_get_type (coll)) {
		case XMMS_COLLECTION_TYPE_UNION:
			for (; xmmsv_list_iter_valid (it); xmmsv_list_iter_next (it)) {
				xmmsv_list_iter_entry (it, &v);
				xmmsv_get_coll (v, &c);
				sa = ret;
				sb = _coll_to_set (s4, c);
				ret = s4_set_union (sa, sb);
				s4_set_free (sa);
				s4_set_free (sb);
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
					s4_set_free (sa);
					s4_set_free (sb);
				}
				else
					ret = sb;
			}
			break;

		case XMMS_COLLECTION_TYPE_EQUALS:
			for (; xmmsv_dict_iter_valid (dt); xmmsv_dict_iter_next (dt)) {
				xmmsv_dict_iter_pair (dt, &foo, &v);
				xmmsv_get_string (v, &bar);

				if (strcmp (foo, "field") == 0)
					key = bar;
				else if (strcmp (foo, "value") == 0)
					val = bar;

			}
			printf ("Looking for (%s, %s)\n", key, val);
			entry = s4_entry_get_s (s4, key, val);
			ret = s4_entry_contained (s4, entry);
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

	if (!xmmsv_coll_parse (query, &coll)) {
		printf ("The query '%s' could not be parsed\n", query);
		return NULL;
	}

	return _coll_to_set (s4, coll);
}
