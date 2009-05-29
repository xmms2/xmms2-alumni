#ifndef _STRSTORE_H
#define _STRSTORE_H

#include "s4.h"

int32_t strstore_str_to_int (s4_t *s4, const char *str);
char   *strstore_int_to_str (s4_t *s4, int32_t off);

int strstore_ref_str (s4_t *s4, const char *str);
int strstore_unref_str (s4_t * s4, const char *str);

#endif /* _STRSTORE_H */
