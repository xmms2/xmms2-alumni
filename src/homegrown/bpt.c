/*
 * B+ tree
 */

#include "be.h"
#include "bpt.h"
#include <stdlib.h>
#include <stdio.h>


#define LEAF_MAGIC 0x12345678
#define INT_MAGIC  0x87654321

/* The node size, it must be odd or
 * I'm pretty sure something will blow up
 * Lowest size possible = 3
 * Tweak until performance is good
 */
#define SIZE 63


/* Internal node */
typedef struct bpt_node_St {
	int32_t magic;
	int32_t key_count;
	int32_t parent;
	int32_t next;

	bpt_record_t keys[SIZE];
	int32_t pointers[0];
} bpt_node_t;


static int32_t _bpt_insert_internal (s4be_t *be, int32_t node,
		int32_t child, bpt_record_t record);


/* Handy debug function */
static int _print_tree (s4be_t *be, int32_t root, int depth)
{
	bpt_node_t *p = S4_PNT (be, root, bpt_node_t);
	int i,j;

	if (root == -1) {
		printf ("(null)\n");
		return 0;
	}


	for (i = 0; i < p->key_count; i++) {
		if (p->magic != LEAF_MAGIC)
			_print_tree (be, p->pointers[i], depth + 1);

		for (j = 0; j < depth; j++)
			printf ("-");
		printf ("%.2i (%i %i %i %i)\n", i, p->keys[i].key_a, p->keys[i].val_a,
				p->keys[i].key_b, p->keys[i].val_b);

	}
	if (p->magic != LEAF_MAGIC)
		_print_tree (be, p->pointers[i], depth + 1);

	if (depth == 0)
		printf("\n");

	return 0;
}


/* Create a new leaf node */
static int32_t _bpt_create_leaf (s4be_t *be)
{
	int32_t ret = be_alloc (be, sizeof (bpt_node_t));
	bpt_node_t *p = S4_PNT (be, ret, bpt_node_t);

	p->parent = -1;
	p->magic = LEAF_MAGIC;
	p->key_count = 0;
	p->next = -1;
	
	return ret;
}


/* Create a new internal node */
static int32_t _bpt_create_internal (s4be_t *be)
{
	int32_t ret = be_alloc (be, sizeof (bpt_node_t) +
			sizeof(int32_t) * (SIZE + 1));
	bpt_node_t *p = S4_PNT (be, ret, bpt_node_t);

	p->parent = -1;
	p->magic = INT_MAGIC;
	p->key_count = 0;
	p->next = -1;
	
	return ret;
}

/* Compare to records,
 * return <0 if a<b, 0 if a=b and >0 if a>b
 */
static int _bpt_comp (bpt_record_t a, bpt_record_t b)
{
	int ret = a.key_a - b.key_a;
	if (!ret)
		ret = a.val_a - b.val_a;
	if (!ret)
		ret = a.key_b - b.key_b;
	if (!ret)
		ret = a.val_b - b.val_b;

	return ret;
}


/* Search the node n for the first key bigger or equal to r */
static int _bpt_search (bpt_node_t *n, bpt_record_t r)
{
	int lower = 0;
	int upper = n->key_count;

	while ((upper - lower) > 0) {
		int gap = (upper - lower) / 2;
		int c = _bpt_comp (r, n->keys[lower + gap]);

		if (c < 0) {
			upper = lower + gap;
		} else if (c > 0) {
			lower += gap + 1;
		} else {
			return (lower + gap);
		}
	}

	return upper;
}


/* Searches for the leaf that might contain record */
static int32_t _bpt_find_leaf (s4be_t *be, int32_t bpt, bpt_record_t record)
{
	int32_t cur = *S4_PNT (be, bpt, int32_t);
	bpt_node_t *i = S4_PNT (be, cur, bpt_node_t);
	int index = 0;

	while (cur != -1 && i->magic != LEAF_MAGIC) {
		index = _bpt_search (i, record);
		if (index < i->key_count && _bpt_comp (record, i->keys[index]) == 0)
			index++;

		cur = i->pointers[index];
		i = S4_PNT (be, cur, bpt_node_t);
	}

	return cur;
}


/* Helper function for _bpt_split
 * Copies half of the keys (and pointers if it's an internal node)
 * over in the new node.
 */
static void _bpt_move_keys (s4be_t *be, bpt_node_t *pl, bpt_node_t *pn,
		int32_t index, int32_t new, int32_t child, bpt_record_t record)
{
	int i, j;
	int foo = (index > (SIZE / 2))?1:0;
	int leaf = child == -1;
	bpt_node_t *pc;

	for (j = 0, i = SIZE / 2 + foo; j < (SIZE/2 + 1); j++) {
		if (i == index && foo) {
			index = -1;
			pn->keys[j] = record;
			if (!leaf)
				pn->pointers[j + 1] = child;
		} else {
			pn->keys[j] = pl->keys[i++];
			if (!leaf)
				pn->pointers[j + 1] = pl->pointers[i];
		}
		if (!leaf) {
			pc = S4_PNT (be, pn->pointers[j + 1], bpt_node_t);
			pc->parent = new;
		}
	}

	if (!foo) {
		for (i = SIZE / 2; i > index; i--) {
			pl->keys[i] = pl->keys[i - 1];
			if (!leaf)
				pl->pointers[i + 1] = pl->pointers[i];
		}

		pl->keys[index] = record;
		if (!leaf)
			pl->pointers[index + 1] = child;
	}

	if (!leaf) {
		pn->pointers[0] = pl->pointers[SIZE / 2 + 1];
		pc = S4_PNT (be, pn->pointers[0], bpt_node_t);
		pc->parent = new;
	}

	pl->key_count = SIZE / 2 + ((leaf)?1:0);
	pn->key_count = SIZE / 2 + 1;
}

/* Static: split a node
 *
 * child should be -1 if node is a leaf
 *
 * Return -1 on error, 0 on success or a positive number
 * that is the new root.
 */
static int32_t _bpt_split (s4be_t *be, int32_t node,
		bpt_record_t record, int32_t child)
{
	bpt_node_t *pn, *pl;
	int index;
	int32_t new;
	int leaf = child == -1;

	pl = S4_PNT (be, node, bpt_node_t);
	index = _bpt_search (pl, record);
	if (index < pl->key_count && _bpt_comp (pl->keys[index], record) == 0) {
		return -1;
	}

	new = (leaf) ? (_bpt_create_leaf (be)) : (_bpt_create_internal (be));
	pl = S4_PNT (be, node, bpt_node_t);
	pn = S4_PNT (be, new, bpt_node_t);

	pn->parent = pl->parent;
	pn->next = pl->next;
	pl->next = new;

	_bpt_move_keys (be, pl, pn, index, new, child, record);

	if (pl->parent == -1) {
		int32_t parent = _bpt_create_internal(be);
		bpt_node_t *pp = S4_PNT (be, parent, bpt_node_t);
		pp->pointers[0] = node;
		pl = S4_PNT (be, node, bpt_node_t);
		pn = S4_PNT (be, new, bpt_node_t);
		pl->parent = pn->parent = parent;
	}

	 return _bpt_insert_internal (be, pl->parent, new,
			 leaf?(pn->keys[0]):(pl->keys[SIZE/2]));
}


/* Insert a key into an internal node
 * Return 0 on success, the node if key_count == 0
 */
static int32_t _bpt_insert_internal (s4be_t *be, int32_t node,
		int32_t child, bpt_record_t record)
{
	bpt_node_t *pn = S4_PNT (be, node, bpt_node_t);
	int ret = 0;

	if (pn->key_count < SIZE) {
		int index = _bpt_search (pn, record);
		int i;

		for (i = pn->key_count; i > index; i--) {
			pn->keys[i] = pn->keys[i - 1];
			pn->pointers[i + 1] = pn->pointers[i];
		}

		pn->keys[index] = record;
		pn->pointers[index + 1] = child;
		pn->key_count++;

		if (pn->key_count == 1) {
			ret = node;
		}
	} else {
		ret = _bpt_split (be, node, record, child);
	}

	return ret;
}


/**
 * Insert a new record into the tree
 *
 * @param be The database handle
 * @param bpt The tree to insert into
 * @param record The record to insert
 * @return 0 on success, -1 on error
 */
int bpt_insert (s4be_t *be, int32_t bpt, bpt_record_t record)
{
	int32_t leaf = _bpt_find_leaf (be, bpt, record);
	bpt_node_t *pl = S4_PNT (be, leaf, bpt_node_t);

	if (leaf == -1) {
		/* There are no root, we create one */
		leaf = _bpt_create_leaf (be);
		pl = S4_PNT (be, leaf, bpt_node_t);
		pl->key_count = 1;
		pl->keys[0] = record;
		*S4_PNT (be, bpt, int32_t) = leaf;

	} else if (pl->key_count < SIZE) {
		/* There's room for more keys in this leaf, we simpy add it */
		int index = _bpt_search (pl, record);
		int i;

		/* Check if it already exists */
		if (index < pl->key_count && _bpt_comp (pl->keys[index], record) == 0) {
			return -1;
		}

		for (i = pl->key_count; i > index; i--)
			pl->keys[i] = pl->keys[i - 1];

		pl->keys[index] = record;
		pl->key_count++;
	} else {
		/* The leaf is full, we need to split it */
		if ((leaf = _bpt_split (be, leaf, record, -1)) != 0) {
			if (leaf == -1) {
				return -1;
			}

			*S4_PNT (be, bpt, int32_t) = leaf;
		}
	}

	return 0;
}


/**
 * Return a set with all the entries that has main key/val pair = key/val
 *
 * @param be The database handle
 * @param bpt The tree
 * @param key The main key
 * @param val The main val
 * @return A set with all the entries, or NULL if there are none
 */
s4_set_t *bpt_get_set (s4be_t *be, int32_t bpt, int32_t key, int32_t val)
{
	bpt_record_t rec;
	bpt_node_t *pl;
	int32_t leaf;
	int index;
	s4_set_t *root, *cur;
	root = cur = NULL;

	rec.key_a = key;
	rec.val_a = val;
	rec.key_b = rec.val_b = INT32_MIN;

	leaf = _bpt_find_leaf (be, bpt, rec);
	pl = S4_PNT (be, leaf, bpt_node_t);

	index = _bpt_search (pl, rec);

	if (leaf != -1 && index == pl->key_count) {
		leaf = pl->next;
		pl = S4_PNT (be, leaf, bpt_node_t);
		index = 0;
	}

	while (leaf != -1 &&
			(pl->keys[index].key_a < key ||
			 (pl->keys[index].key_a == key && pl->keys[index].val_a <= val))) {
		if (_bpt_comp (rec, pl->keys[index]) <= 0) {
			if (cur == NULL) {
				root = cur = malloc (sizeof (s4_set_t));
			} else {
				cur->next = malloc (sizeof (s4_set_t));
				cur = cur->next;
			}

			cur->next = NULL;
			cur->entry.key_s = cur->entry.val_s = NULL;
			cur->entry.key_i = pl->keys[index].key_b;
			cur->entry.val_i = pl->keys[index].val_b;
		}

		if (++index >= pl->key_count) {
			leaf = pl->next;
			pl = S4_PNT (be, leaf, bpt_node_t);
			index = 0;
		}
	}

	return root;
}
