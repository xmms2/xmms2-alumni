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


/* Prototypes */
static int32_t _bpt_insert_internal (s4be_t *be, int32_t node,
		int32_t child, bpt_record_t record);
static void _bpt_underflow (s4be_t *be, int32_t bpt, int32_t node);


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

static void _bpt_set_root (s4be_t *be, int32_t bpt, int32_t root)
{
	int32_t *r = S4_PNT (be, bpt, int32_t);
	*r = root;
}

static int32_t _bpt_get_root (s4be_t *be, int32_t bpt)
{
	return *S4_PNT (be, bpt, int32_t);
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
	int ret = 0;
	ret = (a.key_a < b.key_a)?-1:(a.key_a > b.key_a);
	if (!ret)
		ret = (a.val_a < b.val_a)?-1:(a.val_a > b.val_a);
	if (!ret)
		ret = (a.key_b < b.key_b)?-1:(a.key_b > b.key_b);
	if (!ret)
		ret = (a.val_b < b.val_b)?-1:(a.val_b > b.val_b);

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
	int32_t cur = _bpt_get_root(be, bpt);
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
	int foo = index > (SIZE / 2);
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

	pl->key_count = SIZE / 2 + leaf;
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


/* A helper function for _bpt_left_sibling */
static int32_t _bpt_left_helper (s4be_t *be, int depth,
		int32_t node, bpt_record_t key)
{
	if (node == -1)
		return -1;

	bpt_node_t *pnode = S4_PNT (be, node, bpt_node_t);
	int index = _bpt_search (pnode, key);
	int32_t ret;

	if (index == 0 && _bpt_comp (key, pnode->keys[0]) != 0)
		return _bpt_left_helper (be, depth + 1, pnode->parent, key);
	else if (index != 0 && _bpt_comp (key, pnode->keys[index]) != 0)
		index--;

	if (depth == 0)
		ret = pnode->pointers[index];
	else
		ret = _bpt_left_helper (be, depth - 1, pnode->pointers[index], key);

	return ret;
}


/* Return the left sibling of node, or -1 if it doesn't exist */
static int32_t _bpt_left_sibling (s4be_t *be, int32_t node)
{
	bpt_node_t *pnode = S4_PNT (be, node, bpt_node_t);
	return _bpt_left_helper (be, 0, pnode->parent, pnode->keys[0]);
}


/* Replace the old key with the new key in the parent (or the parents parent) */
static bpt_record_t _bpt_update_parent (s4be_t *be, int32_t parent,
		bpt_record_t old, bpt_record_t new)
{
	bpt_node_t *node = S4_PNT (be, parent, bpt_node_t);
	int index;
	bpt_record_t ret;

	if (parent == -1)
		return old;

	index = _bpt_search (node, old);

	if (index == node->key_count || _bpt_comp (old, node->keys[index]) != 0)
		index--;

	if (index < 0) {
		ret = _bpt_update_parent (be, node->parent, old, new);
	} else {
		ret = node->keys[index];
		node->keys[index] = new;
	}

	return ret;
}


/* Remove the given key (or the one closest) from the internal node */
static bpt_record_t _bpt_remove_internal (s4be_t *be, int32_t bpt,
		int32_t node, bpt_record_t key)
{
	bpt_node_t *pnode = S4_PNT (be, node, bpt_node_t);
	int index = _bpt_search (pnode, key);
	bpt_record_t ret;
	int i;

	if (index == pnode->key_count || _bpt_comp (key, pnode->keys[index]) != 0)
		index--;

	if (index < 0) {
		ret = pnode->keys[0];
		for (i = 1; i < pnode->key_count; i++)
			pnode->keys[i - 1] = pnode->keys[i];
		for (i = 1; i <= pnode->key_count; i++)
			pnode->pointers[i - 1] = pnode->pointers[i];

		ret =  _bpt_update_parent (be, pnode->parent, key, ret);
	} else {
		ret = pnode->keys[index];

		for (i = index; i < pnode->key_count; i++) {
			pnode->keys[i] = pnode->keys[i + 1];
			pnode->pointers[i + 1] = pnode->pointers[i + 2];
		}
	}

	pnode->key_count--;

	if (pnode->key_count < SIZE/2 && pnode->parent != -1)
		_bpt_underflow (be, bpt, node);
	else if (pnode->key_count == 0) {
		_bpt_set_root (be, bpt, pnode->pointers[0]);
		pnode = S4_PNT (be, pnode->pointers[0], bpt_node_t);
		pnode->parent = -1;
		be_free (be, node);
	}

	return ret;
}


/* Merge to nodes and delete one of them */
static void _bpt_merge_nodes (s4be_t *be, int32_t bpt, int leaf,
		bpt_node_t *plo, bpt_node_t *phi,
		int32_t lo, int32_t hi)
{
	int i;
	int add = !leaf;
	bpt_node_t *pc;

	for (i = 0; i < phi->key_count; i++)
		plo->keys[i + plo->key_count + add] = phi->keys[i];
	for (i = 0; i <= phi->key_count && !leaf; i++) {
		plo->pointers[i + plo->key_count + 1] = phi->pointers[i];
		pc = S4_PNT (be, phi->pointers[i], bpt_node_t);
		pc->parent = lo;
	}

	plo->key_count += phi->key_count + add;
	plo->next = phi->next;

	plo->keys[plo->key_count - phi->key_count - add] =
	   	_bpt_remove_internal (be, bpt, phi->parent, phi->keys[0]);

	be_free (be, hi);
}


/* Take keys from one node and put them in the other so that there
 * is the same amount of keys in both (+- 1).
 */
static void _bpt_blend_nodes (s4be_t *be, int leaf,
		bpt_node_t *plo, bpt_node_t *phi,
		int32_t lo, int32_t hi)
{
	int diff = (phi->key_count - plo->key_count) / 2;
	int i;
	int add = !leaf;
	bpt_node_t *pc;

	if (diff > 0) {
		plo->keys[plo->key_count] = _bpt_update_parent (be, phi->parent,
				phi->keys[0], phi->keys[diff - add]);

		/* Copy keys and pointers from high to low node */
		for (i = 0; i < (diff - 1); i++)
			plo->keys[plo->key_count + i + 1] = phi->keys[i + leaf];
		for (i = 0; i < diff && !leaf; i++) {
			plo->pointers[plo->key_count + i + 1] = phi->pointers[i];
			pc = S4_PNT (be, phi->pointers[i], bpt_node_t);
			pc->parent = lo;
		}
		/* Move keys and pointers inside the high node */
		for (i = diff; i < phi->key_count; i++)
			phi->keys[i - diff] = phi->keys[i];
		for (i = diff; i <= phi->key_count && !leaf; i++)
			phi->pointers[i - diff] = phi->pointers[i];

		phi->key_count -= diff;
		plo->key_count += diff;
	} else if (diff < 0) {
		diff = -diff;

		/* Move the keys and pointers in the high node to make room */
		for (i = phi->key_count - 1; i >= 0; i--)
			phi->keys[i + diff] = phi->keys[i];
		for (i = phi->key_count; i >= 0 && !leaf; i--)
			phi->pointers[i + diff] = phi->pointers[i];

		/* Copy keys and pointers from low to high */
		for (i = 0; i < (diff - add); i++)
			phi->keys[i] = plo->keys[i + plo->key_count - (diff - add)];
		for (i = 0; i < diff && !leaf; i++) {
			phi->pointers[i] = plo->pointers[i + plo->key_count - diff + 1];
			pc = S4_PNT (be, phi->pointers[i], bpt_node_t);
			pc->parent = hi;
		}

		phi->keys[diff - add] = _bpt_update_parent (be, phi->parent,
				phi->keys[diff], plo->keys[plo->key_count - diff]);

		phi->key_count += diff;
		plo->key_count -= diff;
	}
}


/* Deal with underflow in a node */
static void _bpt_underflow (s4be_t *be, int32_t bpt, int32_t node)
{
	bpt_node_t *pnode = S4_PNT (be, node, bpt_node_t);
	int leaf = pnode->magic == LEAF_MAGIC;
	int32_t right = pnode->next;
	int32_t left = _bpt_left_sibling (be, node);
	int32_t hi, lo;
	bpt_node_t *pr = S4_PNT (be, right, bpt_node_t);
	bpt_node_t *pl = S4_PNT (be, left , bpt_node_t);
	bpt_node_t *phi, *plo;

	if (right == -1 || (left != -1 && pl->key_count > pr->key_count)) {
		plo = pl;
		phi = pnode;
		hi = node;
		lo = left;
	} else {
		phi = pr;
		plo = pnode;
		hi = right;
		lo = node;
	}

	if ((plo->key_count + phi->key_count) < (SIZE + leaf))
		_bpt_merge_nodes (be, bpt, leaf, plo, phi, lo, hi);
	else
		_bpt_blend_nodes (be, leaf, plo, phi, lo, hi);
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
		_bpt_set_root (be, bpt, leaf);

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
			_bpt_set_root (be, bpt, leaf);
		}
	}

	return 0;
}


/**
 * Remove the record given from the tree
 *
 * @param be The database handle
 * @param bpt The tree
 * @param record The record to remove
 * @return 0 on success, -1 on error.
 */
int bpt_remove (s4be_t *be, int32_t bpt, bpt_record_t record)
{
	int leaf = _bpt_find_leaf (be, bpt, record);
	bpt_node_t *pl = S4_PNT (be, leaf, bpt_node_t);
	int index, i;

	if (leaf == -1)
		return -1;

	index = _bpt_search (pl, record);

	if (index >= pl->key_count || _bpt_comp (pl->keys[index], record) != 0)
		return -1;

	if (index == 0)
		_bpt_update_parent (be, pl->parent, pl->keys[0], pl->keys[1]);

	pl->key_count--;
	for (i = index; i < pl->key_count; i++)
		pl->keys[i] = pl->keys[i + 1];

	if (pl->key_count <= SIZE / 2 && pl->parent != -1)
		_bpt_underflow (be, bpt, leaf);
	else if (pl->key_count == 0) {
		be_free (be, leaf);
		_bpt_set_root (be, bpt, -1);
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
