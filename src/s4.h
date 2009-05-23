#ifndef _S4_H
#define _S4_H

#include <stdint.h>

typedef struct s4_St {
	int fd;
	void *map;
	int size;
} s4_t;

#define S4_PNT(s, i, t) ((t*)((char*)(s)->map + (i)))

s4_t *s4_open (const char *filename);
int s4_close (s4_t *s4);

int32_t s4_alloc (s4_t *s4, int n);
void s4_free (s4_t *s4, int32_t off);

int32_t s4_get_string_store (s4_t *s4);
void s4_set_string_store (s4_t *s4, int32_t ss);

#endif /* _S4_H */
