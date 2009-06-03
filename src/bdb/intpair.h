#ifndef _INTPAIR_H
#define _INTPAIR_H

#include "s4.h"


int intpair_compare (DB *db, const DBT *key1, const DBT *key2);
int intpair_add_property (s4_t *s4, intpair_t a, intpair_t b);
int intpair_remove_property (s4_t *s4, intpair_t a, intpair_t b);
s4_set_t *intpair_has_this (s4_t *s4, intpair_t pair);
s4_set_t *intpair_this_has (s4_t *s4, intpair_t pair);

#endif /* _INTPAIR_H */
