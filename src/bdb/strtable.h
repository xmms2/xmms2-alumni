#ifndef _STRTABLE_H
#define _STRTABLE_H

int strtab_associate (DB *db, const DBT *key, const DBT *data, DBT *result);
int strtab_ref (s4_t *s4, const char *str);
int strtab_unref (s4_t *s4, const char *str);
int strtab_lookup (s4_t *s4, const char *str);
const char *strtab_reverse (s4_t *s4, int id);

#endif
