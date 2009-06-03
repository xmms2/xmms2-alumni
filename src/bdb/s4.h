#ifndef _S4_H
#define _S4_H

#include <db.h>

typedef struct {
	DB *str_db;
	DB *str_rev_db;

	DB *pair_db;
	DB *pair_rev_db;
} s4_t;


typedef struct {
	int key;
	int val;
} intpair_t;


/* s4.c */
s4_t *s4_open (const char *filename);
int s4_close (s4_t *db);


/* set.c */
typedef struct s4_set_St {
	struct s4_set_St *next;
	intpair_t pair;
} s4_set_t;

s4_set_t *s4_set_intersection (s4_set_t *a, s4_set_t *b);
s4_set_t *s4_set_union (s4_set_t *a, s4_set_t *b);
void s4_set_free (s4_set_t *set);


/* entry.c */
struct s4_entry_St {
	intpair_t pair;
};
typedef struct s4_entry_St s4_entry_t;

s4_entry_t s4_entry_get_i (s4_t *s4, const char *key, int val);
s4_entry_t s4_entry_get_s (s4_t *s4, const char *key, const char *val);

int s4_entry_add_i (s4_t *s4, s4_entry_t entry, const char *key, int val);
int s4_entry_add_s (s4_t *s4, s4_entry_t entry, const char *key, const char *val);

s4_set_t *s4_entry_contains (s4_t *s4, s4_entry_t entry);
s4_set_t *s4_entry_contained(s4_t *s4, s4_entry_t entry);



#endif /* _S4_H */
