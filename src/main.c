#include "s4.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *info[] = {
	"artist",
	"title",
	"album",
	NULL
};

int in_info (const char *str)
{
	int i;
	for (i = 0; info[i] != NULL; i++)
		if (strcmp (str, info[i]) == 0)
			return 1;

	return 0;
}

int main (int argc, char *argv[])
{
	s4_t *s4;
	char buffer[2048];
	char *key, *val;
	int id;
	s4_set_t *set, *props;
	s4_entry_t *entry, *prop;

	s4 = s4_open ("medialib");

	if (s4 == NULL) {
		printf("Could not open database\n");
		exit(0);
	}

	if (argc > 1) {
		FILE *file = fopen (argv[1], "r");

		while (fgets (buffer, 2048, file) != NULL) {
			buffer[strlen (buffer) - 1] = 0;
			key = strtok (buffer, "|");
			if (key != NULL)
				id = atoi (key);

			key = strtok (NULL, "|");
			val = strtok (NULL, "|");

			if (key != NULL && val != NULL) {
				entry = s4_entry_get_i (s4, "song_id", id);
				prop = s4_entry_get_s (s4, key, val);

				if (s4_entry_add (s4, entry, prop))
					printf ("Error inserting %i (%s %s)\n",
							id, key, val);

				s4_entry_free (entry);
				s4_entry_free (prop);
			}
		}

		fclose (file);
	} else {
		while (fgets (buffer, 2048, stdin) != NULL) {
			buffer[strlen (buffer) - 1] = 0;

			set = s4_query (s4, buffer);
			while (set != NULL) {
				s4_entry_fillin (s4, &set->entry);
				printf ("Found (%s, %i)\n", set->entry.key_s, set->entry.val_i);

				props = s4_entry_contains (s4, &set->entry);

				while (props != NULL) {
					s4_entry_fillin (s4, &props->entry);
					if (in_info (props->entry.key_s))
						printf ("  %s: %s\n", props->entry.key_s, props->entry.val_s);
					props = s4_set_next (props);
				}

				set = s4_set_next (set);
			}
		}
	}

	s4_close (s4);

	return 0;
}
