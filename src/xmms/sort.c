#include <glib.h>
#include "xmmsc/xmmsv.h"
#include "xmms/xmms_log.h"


int
xmms_sort_compare_values (xmmsv_t *a, xmmsv_t *b)
{
	const char *sa, *sb;
	int ia, ib;
	int ret = 0;

	if (a == NULL || b == NULL) {
		return (a == NULL) * -1 + (b == NULL) * 1;
	}
	if (xmmsv_get_string (a, &sa) && xmmsv_get_string (b, &sb)) {
		char *ca = g_utf8_casefold (sa, -1);
		char *cb = g_utf8_casefold (sb, -1);
		ret = g_utf8_collate (ca, cb);
		g_free (ca);
		g_free (cb);
	} else if (xmmsv_get_int (a, &ia) && xmmsv_get_int (b, &ib)) {
		ret = (ia < ib) * -1 + (ia > ib);
	} else {
		XMMS_DBG ("Trying to compare values of different type");
	}

	return ret;
}

static int
compare_entries (gconstpointer a, gconstpointer b, gpointer data)
{
	xmmsv_t *dict_a = (xmmsv_t*)a, *dict_b = (xmmsv_t*)b;
	xmmsv_t *order = data, *val_a, *val_b, *order_val;
	xmmsv_list_iter_t *it;
	const char *order_str;
	int ret = 0;
	int neg;

	for (xmmsv_get_list_iter (order, &it)
			; xmmsv_list_iter_valid (it) && !ret
			; xmmsv_list_iter_next (it)) {
		xmmsv_list_iter_entry (it, &order_val);
		xmmsv_get_string (order_val, &order_str);

		if (*order_str == '-') {
			neg = -1;
			order_str++;
		} else
			neg = 1;

		if (!xmmsv_dict_get (dict_a, order_str, &val_a))
			val_a = NULL;
		if (!xmmsv_dict_get (dict_b, order_str, &val_b))
			val_b = NULL;

		ret = xmms_sort_compare_values (val_a, val_b) * neg;
	}

	return ret;
}


/**
 * Sort a list of dicts
 */
GList*
xmms_sort_list (GList *list, xmmsv_t *order)
{
	return g_list_sort_with_data (list, compare_entries, order);
}
