/*
 * B+ tree
 */

#include "be.h"
#include "bpt.h"
#include <stdlib.h>


#define LEAF_MAGIC 0x12345678
#define INT_MAGIC  0x87654321

#define SIZE 3


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


/* TODO: use binary search? */
static int _bpt_search (bpt_node_t *n, bpt_record_t r)
{
	int i;
	for (i = 0; i < n->key_count && _bpt_comp (r, n->keys[i]) >= 0; i++);
	return i;
}


/* Searches for the leaf that might contain record */
static int32_t _bpt_find_leaf (s4be_t *be, int32_t bpt, bpt_record_t record)
{
	int32_t cur = *S4_PNT (be, bpt, int32_t);
	bpt_node_t *i;
	int index = 0;

	while (cur != -1) {
		i = S4_PNT (be, cur, bpt_node_t);

		if (i->magic == LEAF_MAGIC)
			break;

		int j;


		index = _bpt_search (i, record);
		//printf ("r : %i %i %i %i - %i\n", record.key_a, record.val_a,
		//		record.key_b, record.val_b, _bpt_comp(record, i->keys[index]));
		//printf ("fl: %i %i - %i %i\n", i->key_count, index, cur, i->parent);
		for (j = 0; j < i->key_count;j++)
		{
			//printf("%i %i %i %i - %i\n", i->keys[j].key_a,i->keys[j].val_a,
			//		i->keys[j].key_b,i->keys[j].val_b,
			//		_bpt_comp(record, i->keys[j]));
		}

		cur = i->pointers[index];
	}

	return cur;
}


/* Split an internal node
 *
 * Return -1 on error, 0 on success or a positive number
 * that is the new root.
 */
static int32_t _bpt_split_internal (s4be_t *be, int32_t node,
		bpt_record_t record, int32_t child)
{
	bpt_node_t *pn, *pl, *pc;
	int index;
	int32_t new;

	pl = S4_PNT (be, node, bpt_node_t);
	index = _bpt_search (pl, record);

	new = _bpt_create_internal (be);
	pl = S4_PNT (be, node, bpt_node_t);
	pn = S4_PNT (be, new, bpt_node_t);

	pn->parent = pl->parent;
	pn->next = pl->next;
	pl->next = new;

	int i, j;
	int foo = (index > (SIZE / 2))?1:0;

	for (j = 0, i = SIZE / 2 + foo; i < SIZE; i++, j++) {
		if (i == index && foo) {
			pn->keys[j++] = record;
			pn->pointers[j] = child;
		}
		pn->keys[j] = pl->keys[i];
		pn->pointers[j + 1] = pl->pointers[i + 1];

		pc = S4_PNT (be, pn->pointers[j + 1], bpt_node_t);
		pc->parent = new;
	}

	if (index == SIZE) {
		pn->keys[SIZE/2] = record;
		pn->pointers[SIZE/2 + 1] = child;
	}
	if (foo) {
		pc = S4_PNT (be, child, bpt_node_t);
		pc->parent = new;
	}


	if (!foo) {
		for (i = SIZE / 2; i > index; i--) {
			pl->keys[i] = pl->keys[i - 1];
			pl->pointers[i + 1] = pl->pointers[i];
		}

		pl->pointers[index + 1] = child;
		pl->keys[index] = record;
	}

	pn->pointers[0] = pl->pointers[SIZE / 2 + 1];
	pc = S4_PNT (be, pn->pointers[0], bpt_node_t);
	pc->parent = new;

	pl->key_count = SIZE / 2;
	pn->key_count = SIZE / 2 + 1;

	if (pl->parent == -1) {
		int32_t parent = _bpt_create_internal(be);
		bpt_node_t *pp = S4_PNT (be, parent, bpt_node_t);
		pp->pointers[0] = node;
		pl->parent = pn->parent = parent;
	}

	 return _bpt_insert_internal (be, pl->parent, new, pl->keys[SIZE/2]);
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

		if (pn->key_count == 1)
			ret = node;
	} else {
		ret = _bpt_split_internal (be, node, record, child);
	}

	return ret;
}


int32_t _bpt_split_leaf (s4be_t *be, int32_t node, bpt_record_t record)
{
	bpt_node_t *pn, *pl;
	int index;
	int32_t new;

	pl = S4_PNT (be, node, bpt_node_t);
	index = _bpt_search (pl, record);
	if (index < pl->key_count && _bpt_comp (pl->keys[index], record) == 0)
		return -1;

	new = _bpt_create_leaf (be);
	pl = S4_PNT (be, node, bpt_node_t);
	pn = S4_PNT (be, new, bpt_node_t);

	pn->parent = pl->parent;
	pn->next = pl->next;
	pl->next = new;

	int i, j;
	int foo = (index > (SIZE / 2))?1:0;

	for (i = SIZE / 2 + foo, j = 0; i < SIZE; i++, j++)
	{
		if (i == index && foo)
			pn->keys[j++] = record;
		pn->keys[j] = pl->keys[i];
	}
	if (index == SIZE)
		pn->keys[j] = record;

	if (!foo) {
		for (i = SIZE / 2 + 1; i > index; i--)
			pl->keys[i] = pl->keys[i - 1];
		pl->keys[index] = record;
	}

	pl->key_count = pn->key_count = SIZE / 2 + 1;

	if (pl->parent == -1) {
		int32_t parent = _bpt_create_internal(be);
		bpt_node_t *pp = S4_PNT (be, parent, bpt_node_t);
		pp->pointers[0] = node;
		pl->parent = pn->parent = parent;
	}

	 return _bpt_insert_internal (be, pl->parent, new, pn->keys[0]);
}

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
		if (_bpt_comp (pl->keys[index], record) == 0)
			return -1;

		for (i = pl->key_count; i > index; i--)
			pl->keys[i] = pl->keys[i - 1];

		pl->keys[index] = record;
		pl->key_count++;
	} else {
		/* The leaf is full, we need to split it */
		if ((leaf = _bpt_split_leaf (be, leaf, record)) != 0) {
			if (leaf == -1) {
				printf ("returning -1\n");
				return -1;
			}

			*S4_PNT (be, bpt, int32_t) = leaf;
		}
	}

	return 0;
}

s4_set_t *bpt_get_set (s4be_t *be, int32_t bpt, int32_t key, int32_t val)
{
	bpt_record_t rec;
	bpt_node_t *pl;
	int32_t leaf;
	int index;
	s4_set_t *root, *cur;

	rec.key_a = key;
	rec.val_a = val;
	rec.key_b = rec.val_b = INT32_MIN;

	leaf = _bpt_find_leaf (be, bpt, rec);

	if (leaf == -1)
		return NULL;

	pl = S4_PNT (be, leaf, bpt_node_t);
	index = _bpt_search (pl, rec);

	//printf ("l: %i\n", pl->keys[0].key_a);

	if (index == pl->key_count) {
		leaf = pl->next;
		pl = S4_PNT (be, leaf, bpt_node_t);
		index = 0;

		if (leaf == -1)
			return NULL;
	}

	root = cur = NULL;

	//printf ("(%i %i) - (%i %i)(%i %i) - %i %i\n", key, val,
	//		pl->keys[index].key_a, pl->keys[index].val_a,
	//		pl->keys[index].key_b, pl->keys[index].val_b,
	//		index, pl->key_count);

	while (index < pl->key_count && 
			(pl->keys[index].key_a <= key ||
		   	(pl->keys[index].key_a == key && pl->keys[index].val_a <= val))) {
		if (pl->keys[index].key_a == key && pl->keys[index].val_a == val) {
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

		index++;
		if (index >= pl->key_count) {
			leaf = pl->next;
			if (leaf == -1)
				break;
			pl = S4_PNT (be, leaf, bpt_node_t);
			index = 0;
		}
		//printf ("(%i %i) - (%i %i)(%i %i) - %i %i\n", key, val,
		//		pl->keys[index].key_a, pl->keys[index].val_a,
		//		pl->keys[index].key_b, pl->keys[index].val_b,
		//		index, pl->key_count);
	}

	return root;
}
