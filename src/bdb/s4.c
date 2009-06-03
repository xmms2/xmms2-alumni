#include "s4.h"
#include "intpair.h"
#include "strtable.h"
#include <stdlib.h>
#include <db.h>
#include <string.h>


int string_open (DB **db, const char *filename, const char *table)
{
	int ret;
	char buffer[128];

	strcpy (buffer, filename);
	strncat (buffer, table, 128);

	ret = db_create (db, NULL, 0);

	if (!(ret = db_create (db, NULL, 0)) &&
	    !(ret = (*db)->open (*db, NULL, buffer, NULL, DB_BTREE, DB_CREATE, 0)));

	return ret;
}


int pair_open (DB **db, const char *filename, const char *table)
{
	int ret;
	char buffer[128];

	strcpy (buffer, filename);
	strncat (buffer, table, 128);

	if (!(ret = db_create (db, NULL, 0)) &&
	    !(ret = (*db)->set_dup_compare (*db, intpair_compare)) &&
	    !(ret = (*db)->set_flags (*db, DB_DUPSORT)) &&
		!(ret = (*db)->open (*db, NULL, buffer, NULL, DB_BTREE, DB_CREATE, 0)));

	return ret;
}


s4_t *s4_open (const char *filename)
{
	s4_t *s4 = malloc (sizeof (s4_t));
	int ret;

	memset (s4, 0, sizeof (s4_t));

	if ((ret = string_open (&s4->str_db, filename, "_string.db")))
		goto cleanup;
	if ((ret = string_open (&s4->str_rev_db, filename, "_string_rev.db")))
		goto cleanup;

	if ((ret = s4->str_db->associate (s4->str_db, NULL, s4->str_rev_db,
					strtab_associate, 0)))
		goto cleanup;

	if ((ret = pair_open (&s4->pair_db, filename, "_pair.db")))
		goto cleanup;
	if ((ret = pair_open (&s4->pair_rev_db, filename, "_pair_rev.db")))
		goto cleanup;


	return s4;

cleanup:
	printf("Something went wrong\n");
	s4_close (s4);
	return NULL;
}


int s4_close (s4_t *s4)
{
	if (s4->str_db) {
		s4->str_db->close (s4->str_db, 0);
	}
	if (s4->str_rev_db) {
		s4->str_rev_db->close (s4->str_rev_db, 0);
	}
	if (s4->pair_db) {
		s4->pair_db->close (s4->pair_db, 0);
	}
	if (s4->pair_rev_db) {
		s4->pair_rev_db->close (s4->pair_rev_db, 0);
	}
	free (s4);
	return 0;
}
