#include <stdlib.h>
#include "s4.h"



static int set_compare (s4_set_t *a, s4_set_t *b)
{
	int ret;
	if (a == NULL)
		return 1;
	if (b == NULL)
		return -1;

	ret = a->entry.key_i - a->entry.key_i;

	if (!ret)
		ret = a->entry.val_i - b->entry.val_i;

	return 0;
}

static void _set_free (s4_set_t *set)
{
	s4_entry_free_strings (&set->entry);

	free (set);
}


s4_set_t *s4_set_intersection (s4_set_t *a, s4_set_t *b)
{
	s4_set_t *ret, *cur, *tmp;
	int c;

	cur = ret = NULL;

	while (a != NULL && b != NULL) {
		c = set_compare (a, b);
		if (c > 0) {
			tmp = b->next;
			_set_free (b);
			b = tmp;
		} else if (c < 0) {
			tmp = a->next;
			_set_free (a);
			a = tmp;
		} else {
			if (cur == NULL) {
				ret = cur = a;
			} else {
				cur->next = a;
				cur = cur->next;
			}

			cur->next = NULL;

			a = a->next;
			tmp = b->next;
			_set_free (b);
			b = tmp;
		}
	}

	return ret;
}


s4_set_t *s4_set_union (s4_set_t *a, s4_set_t *b)
{
	s4_set_t *ret, *cur, *tmp;
	int c;

	cur = ret = NULL;

	while (a != NULL || b != NULL) {
		c = set_compare (a, b);

		if (c > 0) {
			cur->next = b;
			cur = b;

			tmp = b->next;
			_set_free (b);
			b = tmp;
		} else if (c < 0) {
			cur->next = a;
			cur = a;

			tmp = a->next;
			_set_free (a);
			a = tmp;
		} else {
			cur->next = a;
			cur = a;

			tmp = a->next;
			_set_free (a);
			a = tmp;
			tmp = b->next;
			_set_free (b);
			b = tmp;
		}

		if (ret == NULL)
			ret = cur;
		cur->next = NULL;
	}

	return ret;
}


s4_set_t *s4_set_next (s4_set_t *set)
{
	s4_set_t *ret = NULL;

	if (set != NULL) {
		ret = set->next;
		_set_free (set);
	}

	return ret;
}


void s4_set__set_free (s4_set_t *set)
{
	s4_set_t *tmp;
	while (set != NULL) {
		tmp = set->next;
		_set_free (set);
		set = tmp;
	}
}
