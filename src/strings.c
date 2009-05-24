/*
 * The s4 string store
 */

#include "s4.h"
#include <string.h>

#define MAX(a, b) (((a) > (b))?((a)):((b)))

#define GET_ROOT(s, t) (*(S4_PNT(s, t, int32_t)))
#define SET_ROOT(s, t, r) (GET_ROOT(s, t) = (r))

/* The structure used for the patricie trie nodes */
typedef union pat_node_St {
	struct {
		uint32_t pos;
		int32_t left, right;
	} internal;
	struct {
		uint32_t len;
		int32_t key;
		int32_t magic;
	} leaf;
} pat_node_t;


typedef struct pat_key_St {
	const void *data;
	int32_t key_len, data_len;
} pat_key_t;



static inline int is_leaf (pat_node_t *pn)
{
	return pn->leaf.magic == -1;
}


static inline int bit_set (const char *key, int bit)
{
	int i = bit >> 3;
	int j = bit & 7;
	char c = key[i];
	return (c >> j) & 1;
}


static inline int string_diff (s4_t *s4, pat_key_t *key,
		const char *sb, uint32_t lenb)
{
	int i, diff, ret;
	int lena = key->key_len;
	const char *sa = key->data;

	for (i = 0, diff = 0; i*8 < lena && i*8 < lenb; i++) {
		diff = sa[i] ^ sb[i];
		if (diff)
			break;
	}

	for (ret = i * 8; diff && !(diff & 1); diff >>= 1, ret++);

	if (ret >= lena && lena == lenb) {
		ret = (lena == lenb)?-1:MAX(lena, lenb);
	}

	return ret;
}


static inline int nodes_equal (s4_t *s4, pat_key_t *key, int32_t node)
{
	pat_node_t *pn = S4_PNT(s4, node, pat_node_t);
	const char *nkey = S4_PNT(s4, pn->leaf.key, char);

	return pn->leaf.len == key->key_len &&
		string_diff(s4, key, nkey, pn->leaf.len) == -1;
}


int32_t pat_walk (s4_t *s4, int32_t trie, pat_key_t *key)
{
	int32_t node = GET_ROOT(s4, trie);
	pat_node_t *pn;

	while (node != -1) {
		pn = S4_PNT(s4, node, pat_node_t);

		if (is_leaf (pn))
			break;

		if (pn->internal.pos < key->key_len) {
			if (bit_set(key->data, pn->internal.pos)) {
				node = pn->internal.right;
			} else {
				node = pn->internal.left;
			}
		} else {
			node = pn->internal.left;
		}
	}

	return node;
}
void pat_insert_internal (s4_t *s4, int32_t trie, pat_key_t *key,
		int pos, int32_t node)
{
	int32_t internal = s4_alloc (s4, sizeof(pat_node_t));
	int32_t prev = -1;
	int32_t cur = GET_ROOT(s4, trie);
	pat_node_t *pn = S4_PNT(s4, cur, pat_node_t);

	/* Look for the internal node before the new one */
	while (!is_leaf(pn) && pn->internal.pos < pos) {
		prev = cur;
		cur = (bit_set (key->data, pn->internal.pos))?
			pn->internal.right:pn->internal.left;
		pn = S4_PNT(s4, cur, pat_node_t);
	}

	/* Insert the internal node */
	if (prev == -1) {
		SET_ROOT(s4, trie, internal);
		prev = cur;
	} else {
		pn = S4_PNT(s4, prev, pat_node_t); 
		if (bit_set (key->data, pn->internal.pos)) {
			prev = pn->internal.right;
			pn->internal.right = internal;
		} else {
			prev = pn->internal.left;
			pn->internal.left = internal;
		}
	}

	/* Add the leaf to the internal node */
	pn = S4_PNT(s4, internal, pat_node_t);
	pn->internal.pos = pos;
	if (bit_set (key->data, pos)) {
		pn->internal.right = node;
		pn->internal.left = prev;
	} else {
		pn->internal.left = node;
		pn->internal.right = prev;
	}
}



int32_t pat_lookup (s4_t *s4, int32_t trie, pat_key_t *key)
{
	int32_t node = pat_walk (s4, trie, key);

	if (!nodes_equal (s4, key, node))
		return -1;

	return node;
}


int32_t pat_insert (s4_t *s4, int32_t trie, pat_key_t *key_s)
{
	int32_t key, node, comp;
	int diff;
	pat_node_t *pn;

	/* Check if the node already exist */
	comp = pat_walk (s4, trie, key_s);
	if (nodes_equal (s4, key_s, comp)) {
		return -1;
	}

	/* Copy the key into the database */
	key = s4_alloc (s4, key_s->data_len);
	memcpy (S4_PNT(s4, key, char), key_s->data, key_s->data_len);

	/* Allocate and setup the node */
	node = s4_alloc (s4, sizeof(pat_node_t));
	pn = S4_PNT(s4, node, pat_node_t);
	pn->leaf.key = key;
	pn->leaf.len = key_s->key_len;
	pn->leaf.magic = -1;

	/* If there is no root, we are the root */
	if (comp == -1) {
		SET_ROOT(s4, trie, node);
		return node;
	}

	pn = S4_PNT(s4, comp, pat_node_t);
	const char *sb = S4_PNT(s4, pn->leaf.key, char);
	diff = string_diff(s4, key_s, sb, pn->leaf.len);

	pat_insert_internal (s4, trie, key_s, diff, node);

	return node;
}


int32_t pat_insert_string (s4_t *s4, const char *string)
{
	pat_key_t key;

	key.data = string;
	key.data_len = strlen (string) + 1;
	key.key_len = key.data_len * 8;

	return pat_insert (s4, S4_STRING_STORE, &key);
}

int32_t pat_lookup_string (s4_t *s4, char *string)
{
	pat_key_t key;

	key.data = string;
	key.data_len = strlen (string) + 1;
	key.key_len = key.data_len * 8;

	return pat_lookup (s4, S4_STRING_STORE, &key);
}
