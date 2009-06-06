#ifndef _S4_BE_H
#define _S4_BE_H

#include "s4.h"


s4be_t *s4be_open (const char *filename);
int s4be_close (s4be_t *be);

int s4be_st_ref (s4be_t *be, const char *str);
int s4be_st_unref (s4be_t *be, const char *str);
int s4be_st_lookup (s4be_t *be, const char *str);
char *s4be_st_reverse (s4be_t *be, int str_id);

int s4be_ip_add (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop);
int s4be_ip_del (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop);
s4_set_t *s4be_ip_has_this (s4be_t *be, s4_entry_t *entry);
s4_set_t *s4be_ip_this_has (s4be_t *be, s4_entry_t *entry);

#endif /* _S4_BE_H */
