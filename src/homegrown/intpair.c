#include "s4_be.h"
#include "be.h"
#include "pat.h"
#include <stdlib.h>

typedef struct int_pair_St {
	int key;
	int val;

	int32_t list;
} int_pair_t;

typedef struct int_list_St {
	int32_t next;
	int key;
	int val;
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

static int _add_entry (s4be_t *be, int32_t trie, s4_entry_t *a, s4_entry_t *b)
{
	int_pair_t pair, *ppair;
	int32_t off, list;
	int_list_t *plist;
	pat_key_t key;

	list = be_alloc (be, sizeof (int_list_t));
	plist = S4_PNT (be, list, int_list_t);
	plist->key = b->key_i;
	plist->val = b->val_i;
	plist->next = -1;

	pair.key = a->key_i;
	pair.val = a->val_i;
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
	_add_entry (be, S4_INT_STORE, entry, prop);
	_add_entry (be, S4_REV_STORE, prop, entry);
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
