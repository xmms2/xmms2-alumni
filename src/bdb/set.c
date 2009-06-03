#include <stdlib.h>
#include "s4.h"


int set_compare (s4_set_t *a, s4_set_t *b)
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
			cur->pair.val = a->pair.val;
			cur->pair.key = a->pair.key;
			
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

		if (cur == NULL) {
			ret = cur = malloc (sizeof (s4_set_t));
		} else {
			cur->next = malloc (sizeof (s4_set_t));
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

void s4_set_free (s4_set_t *set)
{
	s4_set_t *tmp;
	while (set != NULL) {
		tmp = set->next;
		free (set);
		set = tmp;
	}
}
