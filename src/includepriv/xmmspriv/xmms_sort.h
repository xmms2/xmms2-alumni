#ifndef __XMMS_PRIV_SORT_H__
#define __XMMS_PRIV_SORT_H__

int xmms_sort_compare_values (xmmsv_t *a, xmmsv_t *b);
GList *xmms_sort_list (GList *list, xmmsv_t *order);

#endif
