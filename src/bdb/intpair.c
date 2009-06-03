#include <stdlib.h>
#include <string.h>
#include "intpair.h"


int intpair_compare (DB *db, const DBT *key1, const DBT *key2)
{
	intpair_t *a, *b;
	int ret;

	a = key1->data;
	b = key2->data;

	ret = a->key - b->key;
	if (!ret)
		ret = a->val - b->val;


	if (ret > 0)
		ret = 1;
	if (ret < 0)
		ret = -1;

	return ret;
}


static void setup_dbts (DBT *key, DBT *data, intpair_t *pair_a, intpair_t *pair_b)
{
	memset (key, 0, sizeof (DBT));
	memset(data,0, sizeof (DBT));

	key->data = pair_a;
	key->size = sizeof (intpair_t);
	data->data = pair_b;
	data->ulen = sizeof (intpair_t);
	data->flags = DB_DBT_USERMEM;
}


int intpair_add_property (s4_t *s4,
		intpair_t pair_a,
		intpair_t pair_b)
{
	DBT key, data;
	int ret;
	
	setup_dbts (&key, &data, &pair_a, &pair_b);
	data.size = sizeof (intpair_t);

	if ((ret = s4->pair_db->put (s4->pair_db, NULL, &key, &data, 0)) ||
	    (ret = s4->pair_rev_db->put (s4->pair_rev_db, NULL, &data, &key, 0))) {
		printf ("Error in intpair_add_property\n");
		return -1;
	}

	return 0;
}


int intpair_remove_property (s4_t *s4,
		intpair_t pair_a,
		intpair_t pair_b)
{
	DBT key, data;
	DBC *cursor;
	int ret;

	setup_dbts (&key, &data, &pair_a, &pair_b);
	data.size = sizeof (intpair_t);

	ret = s4->pair_db->cursor (s4->pair_db, NULL, &cursor, DB_WRITECURSOR);
	ret = cursor->get (cursor, &key, &data, DB_GET_BOTH);
	ret = cursor->del (cursor, 0);
	cursor->close (cursor);

	ret = s4->pair_rev_db->cursor (s4->pair_rev_db, NULL, &cursor, DB_WRITECURSOR);
	ret = cursor->get (cursor, &data, &key, DB_GET_BOTH);
	ret = cursor->del (cursor, 0);
	cursor->close (cursor);

	return 0;
}


static s4_set_t *db_get_set (DB *db, intpair_t pair_a)
{
	s4_set_t *root, *cur, *prev;
	DBT key, data;
	DBC *cursor;
	intpair_t pair_b;
	int ret;

	setup_dbts (&key, &data, &pair_a, &pair_b);

	ret = db->cursor (db, NULL, &cursor, 0);
	ret = cursor->get (cursor, &key, &data, DB_SET);

	root = prev = cur = NULL;
	while (!ret) {
		prev = cur;
		cur = malloc (sizeof (s4_set_t) + sizeof (intpair_t));
		cur->next = NULL;
		if (prev != NULL) {
			prev->next = cur;
		} else {
			root = cur;
		}

		cur->pair.key = pair_b.key;
		cur->pair.val = pair_b.val;

		setup_dbts (&key, &data, &pair_a, &pair_b);
		ret = cursor->get (cursor, &key, &data, DB_NEXT_DUP);
	}

	cursor->close (cursor);

	return root;
}


s4_set_t *intpair_has_this (s4_t *s4, intpair_t pair)
{
	return db_get_set (s4->pair_rev_db, pair);
}


s4_set_t *intpair_this_has (s4_t *s4, intpair_t pair)
{
	return db_get_set (s4->pair_db, pair);
}
