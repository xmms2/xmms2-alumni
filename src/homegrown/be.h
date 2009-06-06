#ifndef _BE_H
#define _BE_H

#include <stdint.h>
#include "s4_be.h"
#include "s4.h"

struct s4be_St {
	int fd;
	void *map;
	int size;
};


#define S4_PNT(s, i, t) ((t*)((char*)(s)->map + (i)))

#define S4_STRING_STORE 0
#define S4_INT_STORE 4
#define S4_REV_STORE 8

int32_t be_alloc (s4be_t *s4, int n);
void be_free (s4be_t *s4, int32_t off);

#endif /* _BE_H */
