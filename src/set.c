#include <stdlib.h>
#include "s4.h"



static int set_compare (s4_set_t *a, s4_set_t *b)
{
	int ret;
	if (a == NULL)
		return 1;
	if (b == NULL)
		return -1;

	ret = (a->entry.key_i < b->entry.key_i)?-1:
		(a->entry.key_i > b->entry.key_i);

	if (!ret)
		ret = (a->entry.val_i < b->entry.val_i)?-1:
			(a->entry.val_i > b->entry.val_i);

	return ret;
}

static void _set_free (s4_set_t *set)
{
	s4_entry_free_strings (&set->entry);

	free (set);
}


s4_set_t *s4_set_intersection (s4_set_t *a, s4_set_t *b)
{
	s4_set_t *ret, *cur;
	int c;

	cur = ret = NULL;

	while (a != NULL && b != NULL) {
		c = set_compare (a, b);

		if (c > 0) {
			b = b->next;
		} else if (c < 0) {
			a = a->next;
		} else {
			if (cur == NULL) {
				ret = cur = malloc (sizeof (s4_set_t));
			} else {
				cur->next = malloc (sizeof (s4_set_t));
				cur = cur->next;
			}

			cur->next = NULL;
			cur->entry = a->entry;

			a = a->next;
			b = b->next;
		}
	}

	return ret;
}


s4_set_t *s4_set_union (s4_set_t *a, s4_set_t *b)
{
	s4_set_t *ret, *cur;
	int c;

	cur = ret = NULL;

	while (a != NULL || b != NULL) {
		c = set_compare (a, b);

		if (ret == NULL)
			cur = ret = malloc (sizeof (s4_set_t));
		else {
			cur->next = malloc (sizeof (s4_set_t));
			cur = cur->next;
		}
		cur->next = NULL;

		if (c > 0) {
			cur->entry = b->entry;
			b = b->next;
		} else if (c < 0) {
			cur->entry = a->entry;
			a = a->next;
		} else {
			cur->entry = a->entry;
			a = a->next;
			b = b->next;
		}
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


void s4_set_free (s4_set_t *set)
{
	s4_set_t *tmp;
	while (set != NULL) {
		tmp = set->next;
		_set_free (set);
		set = tmp;
	}
}
