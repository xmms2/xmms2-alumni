#ifndef _SET_H
#define _SET_H

#include "intpair.h"

typedef struct set_St {
	struct set_St *next;
	intpair_t pair;
} set_t;


set_t *set_intersection (set_t *a, set_t *b);
set_t *set_union (set_t *a, set_t *b);
void set_free (set_t *set);

#endif /* _SET_H */
