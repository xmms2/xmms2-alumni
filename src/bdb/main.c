#include "s4.h"
#include "strtable.h"
#include "intpair.h"
#include "set.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main (int argc, char *argv[])
{
	s4_t *s4;

	s4 = s4_open ("test.db");

	if (s4 == NULL) {
		printf("Could not open database\n");
		exit(0);
	}

	intpair_t a, b;
	set_t *set, *set_a, *set_b;

	a.key = 1;
	a.val = 1;

	b.key = 3;
	b.val = 2;
	intpair_add_property (s4, a, b);
	b.key = 2;
	b.val = 4;
	intpair_add_property (s4, a, b);
	b.key = 6;
	b.val = 3;
	intpair_add_property (s4, a, b);
	b.key = 260;
	b.val = 2;
	intpair_add_property (s4, a, b);

	set_a = intpair_this_has (s4, a);
	set_b = intpair_this_has (s4, a);

	set = set_union (set_a, set_b);

	while (set != NULL) {
		printf ("Found (%i, %i)\n", set->pair.key, set->pair.val);
		set = set->next;
	}

	set_free (set);
	set_free (set_a);
	set_free (set_b);

	s4_close (s4);

	return 0;
}
