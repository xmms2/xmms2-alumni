#ifndef _BE_H
#define _BE_H

#include <db.h>

extern DB_TXN *tid;

struct s4be_St {
	DB *str_db;
	DB *str_rev_db;

	DB *pair_db;
	DB *pair_rev_db;

	DB_ENV *env;
};

int intpair_compare (DB *db, const DBT *key1, const DBT *key2);
int strtab_associate (DB *db, const DBT *key, const DBT *data, DBT *result);

int txn_begin (s4be_t *be);
int txn_commit ();
int txn_abort ();


#endif /* _BE_H */
