#include "s4_be.h"
#include "be.h"
#include "pat.h"
#include <stdlib.h>

#define INTLIST_MAGIC 0xf00dbabe

typedef struct int_pair_St {
	int key;
	int val;

	int32_t list;
} int_pair_t;

typedef struct int_list_St {
	int32_t next;
	int key;
	int val;
	int magic;
} int_list_t;

static int _cmp_list (int_list_t *a, int_list_t *b)
{
	int ret;

	ret = a->key - b->key;
	if (!ret)
		ret = a->val - b->val;

	return ret;
}

static int _list_insert (s4be_t *be, int32_t list, int32_t new)
{
	int_list_t *l, *this, *prev;

	prev = NULL;
	l = S4_PNT (be, list, int_list_t);
	this = S4_PNT (be, new, int_list_t);

	while (list != -1) {
		int c = _cmp_list (this, l);
		if (c < 0) {
			this->next = list;

			if (prev == NULL)
				return 1;

			prev->next = new;
			break;
		} else if (c == 0) {
			return 0;
		}

		prev = l;
		list = l->next;
		l = S4_PNT (be, list, int_list_t);
	}

	if (list == -1) {
		if (prev == NULL)
			return 1;
		prev->next = new;
	}

	return 0;
}

static int _add_entry (s4be_t *be, int32_t trie,
		int32_t key_a, int32_t val_a,
		int32_t key_b, int32_t val_b)
{
	int_pair_t pair, *ppair;
	int32_t off, list;
	int_list_t *plist;
	pat_key_t key;

	list = be_alloc (be, sizeof (int_list_t));
	plist = S4_PNT (be, list, int_list_t);
	plist->key = key_b;
	plist->val = val_b;
	plist->magic = INTLIST_MAGIC;
	plist->next = -1;

	pair.key = key_a;
	pair.val = val_a;
	pair.list = list;

	key.data = &pair;
	key.data_len = sizeof(int_pair_t);
	key.key_len = sizeof(int) * 2 * 8;

	off = pat_lookup (be, trie, &key);

	if (off == -1) {
		off = pat_insert (be, trie, &key);
	} else {
		ppair = S4_PNT (be, pat_node_to_key (be, off), int_pair_t);
		if (_list_insert (be, ppair->list, list))
			ppair->list = list;
	}

	return 0;
}

int s4be_ip_add (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop)
{
	be_wlock (be);
	_add_entry (be, S4_INT_STORE,
			entry->key_i, entry->val_i,
			prop->key_i, prop->val_i);
	_add_entry (be, S4_REV_STORE,
			prop->key_i, prop->val_i,
			entry->key_i, entry->val_i);
	be_unlock (be);

	return 0;
}

int s4be_ip_del (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop)
{
}

static s4_set_t *_list_to_set (s4be_t *be, int32_t trie, s4_entry_t *entry)
{
	pat_key_t key;
	int_pair_t pair;
	int32_t off;
	int32_t list;
	s4_set_t *cur = NULL, *ret = NULL;
	int_list_t *l;

	pair.key = entry->key_i;
	pair.val = entry->val_i;

	key.data = &pair;
	key.data_len = sizeof (int_pair_t);
	key.key_len = sizeof (int) * 2 * 8;

	off = pat_lookup (be, trie, &key);

	if (off == -1)
		return NULL;

	list = (S4_PNT (be, pat_node_to_key (be, off), int_pair_t))->list;

	while (list != -1) {
		l = S4_PNT (be, list, int_list_t);
		if (cur != NULL) {
			cur->next = malloc (sizeof (s4_set_t));
			cur = cur->next;
		} else {
			ret = cur = malloc (sizeof (s4_set_t));
		}
		cur->next = NULL;
		cur->entry.key_s = cur->entry.val_s = 0;
		cur->entry.key_i = l->key;
		cur->entry.val_i = l->val;

		if (l->key < 0)
			cur->entry.type = ENTRY_INT;
		else
			cur->entry.type = ENTRY_STR;

		list = l->next;
	}

	return ret;
}

s4_set_t *s4be_ip_has_this (s4be_t *be, s4_entry_t *entry)
{
	s4_set_t *ret;
	be_rlock (be);
	ret = _list_to_set (be, S4_REV_STORE, entry);
	be_unlock (be);

	return ret;
}

s4_set_t *s4be_ip_this_has (s4be_t *be, s4_entry_t *entry)
{
	s4_set_t *ret;
	be_rlock (be);
	ret = _list_to_set (be, S4_INT_STORE, entry);
	be_unlock (be);

	return ret;
}


void _keyval_save (s4be_t *old, s4be_t *new,
		int32_t key_old, int32_t val_old,
		int32_t *key_new, int32_t *val_new)
{
	if (key_old < 0) {
		*key_new = -s4be_st_lookup (new,
				S4_PNT (old, pat_node_to_key (old, -key_old), char));
		*val_new = val_old;
	} else {
		*key_new = s4be_st_lookup (new,
				S4_PNT (old, pat_node_to_key (old, key_old), char));
		*val_new = s4be_st_lookup (new,
				S4_PNT (old, pat_node_to_key (old, val_old), char));
	}
}


/* This function is called if the database wasn't synced
 * last time it was opened. That means we can't assume anything
 * about the state of the data.
 */
int _ip_recover (s4be_t *old, s4be_t *rec)
{
	int32_t node;
	int32_t list;
	int_pair_t *pair;
	int_list_t *plist;
	int32_t ka, kb, va, vb;

	for (node = pat_first (old, S4_INT_STORE);
			node != -1;
			node = pat_next (old, node)) {
		pair = S4_PNT (old, pat_node_to_key (old, node), int_pair_t);
		for (list = pair->list; list != -1 && list < old->size;) {
			plist = S4_PNT (old, list, int_list_t);

			if (plist->magic != INTLIST_MAGIC)
				break;

			_keyval_save (old, rec, pair->key, pair->val, &ka, &va);
			_keyval_save (old, rec, plist->key, plist->val, &kb, &vb);

			if (ka != -1 && va != -1 && kb != -1 && vb != -1) {
				printf ("saving (%i %i), (%i %i)\n", ka, va, kb, vb);
				_add_entry (rec, S4_INT_STORE, ka, va, kb, vb);
				_add_entry (rec, S4_REV_STORE, kb, vb, ka, va);
			}

			list = plist->next;
		}
	}

	return 0;
}
