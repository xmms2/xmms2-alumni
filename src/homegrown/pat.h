#ifndef _PAT_H
#define _PAT_H

#include "be.h"

typedef struct pat_key_St {
	const void *data;
	int32_t key_len;
	int32_t data_len;
} pat_key_t;


int32_t pat_lookup (s4be_t *s4, int32_t trie, pat_key_t *key);
int32_t pat_insert (s4be_t *s4, int32_t trie, pat_key_t *key);
int     pat_remove (s4be_t *s4, int32_t trie, pat_key_t *key);
int32_t pat_node_to_key (s4be_t *s4, int32_t node);

#endif /* _PAT_H */
