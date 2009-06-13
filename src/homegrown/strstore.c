#include "pat.h"
#include <string.h>
#include <stdlib.h>


#define STR_MAGIC 0xafa7beef

typedef struct str_info_St {
	int32_t magic;
	int32_t refs;
} str_info_t;


/**
 * Look up a string and return the associated int
 *
 * @param s4 The database handle
 * @param str The string to look up
 * @return The int, or 0 if it can not find it
 */
int32_t s4be_st_lookup (s4be_t *s4, const char *str)
{
	int32_t ret;
	pat_key_t key;
	key.data = str;
	key.key_len = (strlen(str) + 1) * 8;

	ret = pat_lookup (s4, S4_STRING_STORE, &key);

	if (ret == -1)
		ret = 0;

	return ret;
}


/**
 * Find the string associated with the int
 *
 * @param s4 The database handle
 * @param node The int
 * @return The string
 */
char *s4be_st_reverse (s4be_t *s4, int32_t node)
{
	char *ret;
	be_rlock (s4);

	ret = strdup (S4_PNT(s4, pat_node_to_key (s4, node), char));

	be_unlock (s4);
	return ret;
}


/**
 * Add a reference to the string
 *
 * @param s4 The database handle
 * @param str The string to reference
 * @return The integer associated with the string
 */
int s4be_st_ref (s4be_t *s4, const char *str)
{
	pat_key_t key;
	int32_t node;
	int len = strlen (str) + 1;
	str_info_t *info;
	char *data;


	be_wlock (s4);

	key.data = str;
	key.key_len = (strlen(str) + 1) * 8;
	node = pat_lookup (s4, S4_STRING_STORE, &key);

	if (node != -1) {
		data = S4_PNT (s4, pat_node_to_key (s4, node), char);
		info = (str_info_t*)(data + len);

		info->refs++;
		be_unlock (s4);
		return node;
	}

	data = malloc (len + sizeof(str_info_t));
	strcpy (data, str);
	info = (str_info_t*)(data + len);
	info->magic = STR_MAGIC;
	info->refs = 1;

	key.data = data;
	key.data_len = len + sizeof(str_info_t);
	key.key_len = len * 8;
	node = pat_insert (s4, S4_STRING_STORE, &key);

	be_unlock (s4);
	free (data);

	return node;
}


/**
 * Remove a reference from a string
 *
 * @param s4 The database handle
 * @param str The string to unref
 * @return -1 if the string does not exist, the refcount otherwise
 */
int s4be_st_unref (s4be_t * s4, const char *str)
{
	int32_t node;
	str_info_t *info;
	char *data;
	int len = strlen (str) + 1;
	pat_key_t key;

	be_wlock (s4);

	key.data = str;
	key.key_len = len * 8;
	node = pat_lookup (s4, S4_STRING_STORE, &key);

	if (node == -1) {
		be_unlock (s4);
		return -1;
	}

	data = S4_PNT (s4, pat_node_to_key (s4, node), char);
	info = (str_info_t*)(data + len);
	info->refs--;

	if (info->refs == 0) {
		pat_remove (s4, S4_STRING_STORE, &key);
		be_unlock (s4);
		return 0;
	}

	be_unlock (s4);
	return info->refs;
}


/* Try to read the key and return it in pkey
 * Return 1 if it succeded, 0 otherwise
 */
int _copy_key (s4be_t *db, int32_t key, pat_key_t *pkey)
{
	int len;
	char *data;
	str_info_t *info;

	if (key < 0 || key > db->size)
		return 0;

	data = S4_PNT (db, key, char);
	for (len = 0; data[len] && len < (db->size - key); len++);
	len++;

	info = S4_PNT (db, key + len, str_info_t);
	if (len >= (db->size - key - sizeof (str_info_t)) || info->magic != STR_MAGIC)
		return 0;

	pkey->data = data;
	pkey->key_len = len * 8;
	pkey->data_len = len + sizeof (str_info_t);

	return 1;
}


/* Called when the database is recovering. We walk through all
 * the nodes in the string-trie and check if the key makes sense.
 * If it does we insert it into the new db.
 */
int _st_recover (s4be_t *old, s4be_t *rec)
{
	int32_t node = pat_first (old, S4_STRING_STORE);
	int32_t key;
	pat_key_t pkey;

	while (node != -1) {
		key = pat_node_to_key (old, node);

		if (_copy_key (old, key, &pkey)) {
			pat_insert (rec, S4_STRING_STORE, &pkey);
		}

		node = pat_next (old, node);
	}

	return 0;
}
