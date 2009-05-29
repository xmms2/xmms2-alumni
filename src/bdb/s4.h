#ifndef _S4_H
#define _S4_H

#include <db.h>
typedef struct {
	DB *str_db;
	DB *str_rev_db;

	DB *pair_db;
	DB *pair_rev_db;
} s4_t;

s4_t *s4_open (const char *filename);
int s4_close (s4_t *db);

#endif /* _S4_H */
