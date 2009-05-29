#include <stdlib.h>
#include "intpair.h"

int set_compare (set_t *a, set_t *b)
{
	int ret;
	if (a == NULL)
		return 1;
	if (b == NULL)
		return -1;

	ret = a->pair.key - b->pair.key;

	if (!ret)
		ret = a->pair.val - b->pair.val;

	return ret;
}

set_t *set_intersection (set_t *a, set_t *b)
{
	set_t *ret, *cur;
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
				ret = cur = malloc (sizeof (set_t));
			} else {
				cur->next = malloc (sizeof (set_t));
				cur = cur->next;
			}
			cur->next = NULL;
			cur->pair.val = a->pair.val;
			cur->pair.key = a->pair.key;
			
			a = a->next;
			b = b->next;
		}
	}

	return ret;
}


set_t *set_union (set_t *a, set_t *b)
{
	set_t *ret, *cur;
	int c;

	cur = ret = NULL;

	while (a != NULL || b != NULL) {
		c = set_compare (a, b);

		if (cur == NULL) {
			ret = cur = malloc (sizeof (set_t));
		} else {
			cur->next = malloc (sizeof (set_t));
			cur = cur->next;
		}

		cur->next = NULL;

		if (c > 0) {
			cur->pair.val = b->pair.val;
			cur->pair.key = b->pair.key;
			b = b->next;
		} else if (c < 0) {
			cur->pair.val = a->pair.val;
			cur->pair.key = a->pair.key;
			a = a->next;
		} else {
			cur->pair.val = a->pair.val;
			cur->pair.key = a->pair.key;
			a = a->next;
			b = b->next;
		}
	}

	return ret;
}

void set_free (set_t *set)
{
	set_t *tmp;
	while (set != NULL) {
		tmp = set->next;
		free (set);
		set = tmp;
	}
}
