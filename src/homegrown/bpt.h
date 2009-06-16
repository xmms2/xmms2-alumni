#ifndef _BPT_H
#define _BPT_H

#include "s4.h"

typedef struct bpt_record_St {
	int32_t key_a, val_a;
	int32_t key_b, val_b;
} bpt_record_t;


int bpt_insert (s4be_t *be, int32_t bpt, bpt_record_t record);
s4_set_t *bpt_get_set (s4be_t *be, int32_t bpt, int32_t key, int32_t val);

#endif /* _BPT_H */
