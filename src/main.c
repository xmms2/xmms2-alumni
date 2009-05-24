/* Just to test the s4 lib */

#include "s4.h"
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
	char buffer[1024];
	s4_t *s4 = s4_open("s4.db");

	printf("Testing patricia trie string insertion\n\n");

	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "done", 4))
			break;

		buffer[strlen(buffer) - 1] = '\0';

		pat_insert_string (s4, buffer);
	}
	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "done", 4))
			break;
		buffer[strlen(buffer) - 1] = '\0';

		pat_lookup_string(s4, buffer);
	//	printf("%i\n", pat_lookup_string (s4, buffer));
	}

	s4_close (s4);

	return 0;
}
