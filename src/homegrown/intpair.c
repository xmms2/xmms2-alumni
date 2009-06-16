#include "s4_be.h"
#include "be.h"
#include "bpt.h"
#include <stdlib.h>

#define INTLIST_MAGIC 0xf00dbabe

typedef struct int_pair_St {
	int key;
	int val;

	pat_trie_t trie;
//	int32_t list;
} int_pair_t;

/*
typedef struct int_list_St {
	int32_t next;
	int key;
	int val;
	int magic;
} int_list_t;
*/


#if 0
/* Compare a and b,
 * return <0 if a<b, 0 if a=b and >0 if a>b
 */
static int _cmp_list (int_list_t *a, int_list_t *b)
{
	int ret;

	ret = a->key - b->key;
	if (!ret)
		ret = a->val - b->val;

	return ret;
}


/* Insert new into the list
 * Returns -1 if it already exists,
 * 1 if the root needs to be changed and 0 otherwise
 */
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
			return -1;
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

#endif
/* Add the entry (key_a, val_a) with property (key_b, val_b) */
static int _add_entry (s4be_t *be, int32_t trie,
		int32_t key_a, int32_t val_a,
		int32_t key_b, int32_t val_b)
{
	int_pair_t pair, *ppair;
	int32_t off, list;
	//int_list_t *plist;
	pat_key_t key;

	/*
	list = be_alloc (be, sizeof (int_list_t));
	plist = S4_PNT (be, list, int_list_t);
	plist->key = key_b;
	plist->val = val_b;
	plist->magic = INTLIST_MAGIC;
	plist->next = -1;
	*/

	memset (&pair, -1, sizeof (int_pair_t));
	pair.key = key_a;
	pair.val = val_a;

	key.data = &pair;
	key.data_len = sizeof(int_pair_t);
	key.key_len = sizeof(int) * 2 * 8;

	off = pat_lookup (be, trie, &key);

	if (off == -1) {
		off = pat_insert (be, trie, &key);
	}

	off = pat_node_to_key (be, off);

	pair.key = key_b;
	pair.val = val_b;
	key.data = &pair;
	key.data_len = sizeof(int) * 2;
	key.key_len = sizeof(int) * 2 * 8;

	if (pat_lookup (be, off + sizeof (int) * 2, &key) == -1)
		pat_insert (be, off + sizeof (int) * 2, &key);
	else
		return -1;

	return 0;
}


/**
 * Add the binding entry->prop to the database
 *
 * @param be The database handle
 * @param entry The entry to get a new property
 * @param prop The property to be set
 * @return 0 on success, -1 otherwise
 */
int s4be_ip_add (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop)
{
	int ret;
	bpt_record_t a, b;

	b.key_b = a.key_a = entry->key_i;
	b.val_b = a.val_a = entry->val_i;
	a.key_b = b.key_a = prop->key_i;
	a.val_b = b.val_a = prop->val_i;

	be_wlock (be);
/*	ret =_add_entry (be, S4_INT_STORE,
			entry->key_i, entry->val_i,
			prop->key_i, prop->val_i);
	if (!ret)
		_add_entry (be, S4_REV_STORE,
				prop->key_i, prop->val_i,
				entry->key_i, entry->val_i);*/
	ret = bpt_insert (be, S4_INT_STORE, a);
	bpt_insert (be, S4_REV_STORE, b);

	be_unlock (be);

	return ret;
}


/**
 * Remove the binding entry->prop
 *
 * TODO: Implement it
 * @return 0 on success, -1 otherwise
 */
int s4be_ip_del (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop)
{
	return -1;
}


void *_node_to_set (s4be_t *be, int32_t key, void *next)
{
	int_pair_t *pair = S4_PNT (be, key, int_pair_t);
	s4_set_t *set = malloc (sizeof (s4_set_t));

	set->entry.key_s = set->entry.val_s = NULL;
	set->entry.key_i = pair->key;
	set->entry.val_i = pair->val;
	set->next = next;

	return set;
}


/* Get the list for the entry and convert it to a set and return it */
static s4_set_t *_list_to_set (s4be_t *be, int32_t trie, s4_entry_t *entry)
{
	pat_key_t key;
	int_pair_t pair;
	int32_t off;
	int32_t list;
	s4_set_t *cur = NULL, *ret = NULL;
//	int_list_t *l;

	pair.key = entry->key_i;
	pair.val = entry->val_i;

	key.data = &pair;
	key.data_len = sizeof (int_pair_t);
	key.key_len = sizeof (int) * 2 * 8;

	off = pat_lookup (be, trie, &key);

	if (off == -1)
		return NULL;

	off = pat_node_to_key (be, off);

	ret = pat_fold (be, off + sizeof(int) * 2, NULL, _node_to_set);
/*
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
*/
	return ret;
}


/**
 * Get all the entries that has the property entry
 *
 * @param be The database handle
 * @param entry The property
 * @return A set with all the entries
 */
s4_set_t *s4be_ip_has_this (s4be_t *be, s4_entry_t *entry)
{
	s4_set_t *ret;
	be_rlock (be);
	ret = bpt_get_set (be, S4_REV_STORE, entry->key_i, entry->val_i);
	be_unlock (be);

	return ret;
}


/**
 * Get all the properties that this entry has
 *
 * @param be The database handle
 * @param entry The entry
 * @return A set with all the properties
 */
s4_set_t *s4be_ip_this_has (s4be_t *be, s4_entry_t *entry)
{
	s4_set_t *ret;
	be_rlock (be);
	ret = bpt_get_set (be, S4_INT_STORE, entry->key_i, entry->val_i);
	be_unlock (be);

	return ret;
}


/* Check if the key/val pair given is good, if they are
 * we save the translated key/val pair into key_new/val_new
 */
static void _keyval_save (s4be_t *old, s4be_t *new,
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


/* Try to recover the database
 * We walk through all the pairs and their lists. We do a check on
 * every key/val pair to make sure they are good, if they are we
 * save the pair into the new database
 */
int _ip_recover (s4be_t *old, s4be_t *rec)
{
	/*int32_t node, list;
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
				_add_entry (rec, S4_INT_STORE, ka, va, kb, vb);
				_add_entry (rec, S4_REV_STORE, kb, vb, ka, va);
			}

			list = plist->next;
		}
	}*/

	return 0;
}
