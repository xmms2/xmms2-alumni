#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main (int argc, char *argv[])
{
	DB *db;
	DBT key, data;
	int ret;
	int foo = 0;
	char buffer[1024];

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));

	ret = db_create(&db, NULL, 0);
	if (ret) {
		printf("Error in db_create\n");
		exit(0);
	}

	ret = db->open(db, NULL, "bdb.db", NULL, DB_BTREE, DB_CREATE, 0);
	if (ret) {
		printf("Error in db->open\n");
		exit(0);
	}

	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "DONE", 4))
			break;
		buffer[strlen(buffer) - 1] = '\0';
		
		key.data = buffer;
		key.size = strlen(buffer) + 1;

		data.data = &foo;
		data.size = sizeof(int);

		db->put(db, NULL, &key, &data, DB_NOOVERWRITE);
		foo++;
	}
	while (1) {
		if (fgets(buffer, 1024, stdin) == NULL)
			break;
		if (!strncmp(buffer, "DONE", 4))
			break;
		buffer[strlen(buffer) - 1] = '\0';

		key.data = buffer;
		key.size = strlen(buffer) + 1;

		data.data = &foo;
		data.ulen = sizeof(int);
		data.flags = DB_DBT_USERMEM;

		foo = -1;
		db->get(db, NULL, &key, &data, 0);

//		printf("(%i) %s\n", foo, buffer);
	}


	db->close(db, 0);

	return 0;
}
