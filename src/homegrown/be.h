#ifndef _BE_H
#define _BE_H

#include <stdint.h>
#include <pthread.h>
#include "s4_be.h"
#include "s4.h"
#include "pat.h"
#include "bpt.h"

struct s4be_St {
	int fd;
	void *map;
	int size;
	pthread_rwlock_t rwlock;
};


#define S4_PNT(s, i, t) ((t*)((char*)(s)->map + (i)))

#define S4_STRING_STORE 0
#define S4_INT_STORE (sizeof (pat_trie_t))
#define S4_REV_STORE (sizeof (pat_trie_t) + sizeof (bpt_t))

int32_t be_alloc (s4be_t *s4, int n);
void be_free (s4be_t *s4, int32_t off);

void be_rlock (s4be_t *s4);
void be_unlock (s4be_t *s4);
void be_wlock (s4be_t *s4);

int _st_recover (s4be_t *old, s4be_t *rec);
int _ip_recover (s4be_t *old, s4be_t *rec);

#endif /* _BE_H */
