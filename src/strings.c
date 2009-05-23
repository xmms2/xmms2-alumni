/*
 * The s4 string store
 */

#include "s4.h"
#include <string.h>

#define MAX(a, b) (((a) > (b))?((a)):((b)))


/* The structure used for the patricie trie nodes */
typedef struct pat_node_St {
	union {
		struct {
			uint32_t pos;
			int32_t left, right;
		} internal;
		struct {
			uint32_t len;
			int32_t key;
			int32_t magic;
		} leaf;
	} u;
} pat_node_t;


int is_leaf (pat_node_t *pn)
{
	return pn->u.leaf.magic == -1;
}


int bit_set (const char *key, int bit)
{
	int i = bit / 8;
	int j = bit % 8;
	char c = key[i];
	return (c >> j) & 1;
}

/* Walks the trie */
int32_t pat_walk (s4_t *s4, const char *key, uint32_t key_len)
{
	int32_t node = s4_get_string_store (s4);
	pat_node_t *pn;

	while (node != -1) {
		pn = S4_PNT(s4, node, pat_node_t);

		if (is_leaf (pn))
			break;

		if (pn->u.internal.pos < key_len) {
			if (bit_set(key, pn->u.internal.pos)) {
				node = pn->u.internal.right;
			} else {
				node = pn->u.internal.left;
			}
		} else {
			node = pn->u.internal.left;
		}
	}

	return node;
}



int string_diff (s4_t *s4, const char  *sa, uint32_t lena, const char *sb, uint32_t lenb)
{
	int i, diff, ret;

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

void pat_insert_internal (s4_t *s4, const char *key, uint32_t len, int pos, int32_t node)
{
	int32_t internal = s4_alloc (s4, sizeof(pat_node_t));
	int32_t prev = -1;
	int32_t cur = s4_get_string_store (s4);
	pat_node_t *pn = S4_PNT(s4, cur, pat_node_t);

	/* Look for the internal node before the new one */
	while (!is_leaf(pn) && pn->u.internal.pos < pos) {
		prev = cur;
		cur = (bit_set (key, pn->u.internal.pos))?
			pn->u.internal.right:pn->u.internal.left;
		pn = S4_PNT(s4, cur, pat_node_t);
	}

	/* Insert the internal node */
	if (prev == -1) {
		s4_set_string_store (s4, internal);
		prev = cur;
	} else {
		pn = S4_PNT(s4, prev, pat_node_t); 
		if (bit_set (key, pn->u.internal.pos)) {
			prev = pn->u.internal.right;
			pn->u.internal.right = internal;
		} else {
			prev = pn->u.internal.left;
			pn->u.internal.left = internal;
		}
	}

	/* Add the leaf to the internal node */
	pn = S4_PNT(s4, internal, pat_node_t);
	pn->u.internal.pos = pos;
	if (bit_set (key, pos)) {
		pn->u.internal.right = node;
		pn->u.internal.left = prev;
	} else {
		pn->u.internal.left = node;
		pn->u.internal.right = prev;
	}
}


int32_t pat_lookup (s4_t *s4, const char *key, uint32_t key_len)
{
	int32_t node = pat_walk (s4, key, key_len);
	pat_node_t *pn = S4_PNT(s4, node, pat_node_t);
	const char *nkey = S4_PNT(s4, pn->u.leaf.key, char);

	if (pn->u.leaf.len != key_len || string_diff(s4, key, key_len, nkey, pn->u.leaf.len) != -1)
		return -1;

	return node;
}


int32_t pat_insert (s4_t *s4, const char *key_string, uint32_t key_len)
{
	/* Check if it already exist */
	if (pat_lookup (s4, key_string, key_len) != -1) {
		return -1;
	}
	int32_t key = s4_alloc (s4, (key_len + 7) / 8);

	memcpy (S4_PNT(s4, key, char), key_string, (key_len + 7) / 8);

	int32_t node = s4_alloc (s4, sizeof(pat_node_t));
	int32_t comp = pat_walk (s4, key_string, key_len);
	int diff;
	pat_node_t *pn = S4_PNT(s4, node, pat_node_t);

	pn->u.leaf.key = key;
	pn->u.leaf.len = key_len;
	pn->u.leaf.magic = -1;

	if (comp == -1) {
		s4_set_string_store(s4, node);
		return node;
	}

	pn = S4_PNT(s4, comp, pat_node_t);
	const char *sb = S4_PNT(s4, pn->u.leaf.key, char);
	diff = string_diff(s4, key_string, key_len, sb, pn->u.leaf.len);

	pat_insert_internal (s4, key_string, key_len, diff, node);

	return node;
}


int32_t pat_insert_string (s4_t *s4, const char *string)
{
	uint32_t len = strlen (string) + 1;

	return pat_insert (s4, string, len * 8);
}

int32_t pat_lookup_string (s4_t *s4, char *string)
{
	uint32_t len = strlen (string) + 1;

	return pat_lookup (s4, string, len * 8);
}
