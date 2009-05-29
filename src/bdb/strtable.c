#include "s4.h"
#include <string.h>

typedef struct string_val_St {
	int id;
	int ref_count;
} string_val_t;


static void setup_dbts (DBT *key, DBT *data,
		const char *str, string_val_t *val)
{
	memset (key, 0, sizeof (DBT));
	memset (data, 0, sizeof (DBT));

	key->size = strlen (str) + 1;
	key->data = (void*)str;

	data->data = val;
	data->flags = DB_DBT_USERMEM;
}

int strtab_associate (DB *db, const DBT *key, const DBT *data, DBT *result)
{
	/* We only want the id as a secondary key, not the ref count */
	memset (result, 0, sizeof (DBT));
	result->size = sizeof (int);
	result->data = data->data;
	return 0;
}

int strtab_ref (s4_t *s4, const char *str)
{
	DBT key, data;
	int ret;
	static int id = 0;
	string_val_t strval;

	setup_dbts (&key, &data, str, &strval);
	data.ulen = sizeof (string_val_t);

	ret = s4->str_db->get (s4->str_db, NULL, &key, &data, 0);

	if (ret == DB_NOTFOUND) {
		strval.id = ++id;
		strval.ref_count = 1;
	} else if (!ret) {
		strval.ref_count++;
	} else {
		printf("Error\n");
		return -1;
		/* Error handling */
	}

	setup_dbts (&key, &data, str, &strval);
	data.size = sizeof (string_val_t);

	ret = s4->str_db->put (s4->str_db, NULL, &key, &data, 0);

	if (ret) {
		printf("Error2\n");
		return -1;
		/* More error handling */
	}

	return strval.ref_count;
}

int strtab_unref (s4_t *s4, const char *str)
{
	DBT key, data;
	int ret;
	string_val_t strval;


	setup_dbts (&key, &data, str, &strval);
	data.ulen = sizeof (string_val_t);

	ret = s4->str_db->get (s4->str_db, NULL, &key, &data, 0);

	if (ret) {
		/* Error handling */
		printf("Error3\n");
		return -1;
	} else if (strval.ref_count <= 1) {
		ret = s4->str_db->del (s4->str_db, NULL, &key, 0);
		if (ret) {
			printf("Error4\n");
			return -1;
			/* Error handling */
		}
		strval.ref_count = 0;
	} else {
		strval.ref_count--;
		setup_dbts (&key, &data, str, &strval);
		data.size = sizeof (string_val_t);
		s4->str_db->put (s4->str_db, NULL, &key, &data, 0);
	}

	return strval.ref_count;
}

int strtab_lookup (s4_t *s4, const char *str)
{
	DBT key, data;
	int ret;
	string_val_t strval;

	memset (&key, 0, sizeof (key));
	memset (&data, 0, sizeof (data));

	key.data = (void*)str;
	key.size = strlen (str) + 1;

	data.data = &strval;
	data.ulen = sizeof (string_val_t);
	data.flags = DB_DBT_USERMEM;
	
	ret = s4->str_db->get (s4->str_db, NULL, &key, &data, 0);

	if (ret) {
		/* Error handling */
		return -1;
	}

	return strval.id;
}

const char *strtab_reverse (s4_t *s4, int id)
{
	DBT key, data, pkey;
	int ret;

	memset (&key, 0, sizeof (key));
	memset (&pkey, 0, sizeof (pkey));
	memset (&data, 0, sizeof (data));

	key.data = &id;
	key.size = sizeof (int);

	ret = s4->str_rev_db->pget (s4->str_rev_db, NULL, &key, &pkey, &data, 0);

	if (ret) {
		/* Error handling */
		return NULL;
	}

	return pkey.data;
}
