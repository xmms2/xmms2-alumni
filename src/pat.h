#ifndef _PAT_H
#define _PAT_H

#include "s4.h"

typedef struct pat_key_St {
	const void *data;
	int32_t key_len;
	int32_t data_len;
} pat_key_t;


int32_t pat_lookup (s4_t *s4, int32_t trie, pat_key_t *key);
int32_t pat_insert (s4_t *s4, int32_t trie, pat_key_t *key);

#endif /* _PAT_H */
