#include "strstore.h"
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
 * @return The int, or -1 if it can not find it
 */
int32_t strstore_str_to_int (s4_t *s4, const char *str)
{
	pat_key_t key;
	key.data = str;
	key.key_len = (strlen(str) + 1) * 8;

	return pat_lookup (s4, S4_STRING_STORE, &key);
}


/**
 * Find the string associated with the int
 *
 * @param s4 The database handle
 * @param node The int
 * @return The string
 */
char *strstore_int_to_str (s4_t *s4, int32_t node)
{
	return S4_PNT(s4, pat_node_to_key (s4, node), char);
}


/**
 * Add a reference to the string
 *
 * @param s4 The database handle
 * @param str The string to reference
 * @return 0 if the string already exist, 1 otherwise
 */
int strstore_ref_str (s4_t *s4, const char *str)
{
	pat_key_t key;
	int32_t node;
	int len = strlen (str) + 1;
	str_info_t *info;
	char *data;

	node = strstore_str_to_int (s4, str);

	if (node != -1) {
		data = strstore_int_to_str (s4, node);
		info = (str_info_t*)(data + len);

		info->refs++;
		return 0;
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

	free (data);

	return 1;
}


/**
 * Remove a reference from a string
 *
 * @param s4 The database handle
 * @param str The string to unref
 * @return -1 if the string does not exist, 0 otherwise
 */
int strstore_unref_str (s4_t * s4, const char *str)
{
	int32_t node;
	char *data;
	str_info_t *info;
	int len = strlen (str) + 1;
	pat_key_t key;

	key.data = str;
	key.key_len = (strlen(str) + 1) * 8;

	node = pat_lookup (s4, S4_STRING_STORE, &key);

	if (node == -1) {
		return -1;
	}

	data = strstore_int_to_str (s4, node);
	info = (str_info_t*)(data + len);
	info->refs--;

	if (info->refs == 0) {
		pat_remove (s4, S4_STRING_STORE, &key);
		return 0;
	}

	return info->refs;
}
