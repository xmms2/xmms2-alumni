/* Just to test the s4 lib */

#include "s4.h"
#include "strstore.h"
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
	char buffer[1024];
	s4_t *s4 = s4_open("s4.db");
	int ret;

	printf("Testing patricia trie string insertion\n\n");

	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "DONE", 4))
			break;

		buffer[strlen(buffer) - 1] = '\0';

		strstore_ref_str (s4, buffer);
	}
	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "DONE", 4))
			break;
		buffer[strlen(buffer) - 1] = '\0';

		ret = strstore_unref_str (s4, buffer);
		printf("u: %d\n", ret);
	}
	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "DONE", 4))
			break;
		buffer[strlen(buffer) - 1] = '\0';

		ret = strstore_str_to_int (s4, buffer);
		printf("%i\n", ret);
	}

	s4_close (s4);

	return 0;
}
