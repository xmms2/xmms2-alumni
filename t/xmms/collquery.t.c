#include "tap.h"
#include <glib.h>
#include <xmmsclient/xmmsclient.h>
#include <sqlite3.h>
#include <stdlib.h>
#include <stdarg.h>
#include "xmmspriv/xmms_collection.h"
#include "xmmspriv/xmms_collquery.h"
#include "test_helper_configuration.h"

sqlite3 *
test_collquery_get_db_handle () {
	sqlite3 *db;

	if (sqlite3_open (TEST_DIR "/xmms/collquery.db", &db) != SQLITE_OK) {
		printf ("# failed to open db: `%s'\n", sqlite3_errmsg (db));
		exit (EXIT_FAILURE);
	}

	return db;
}

int
test_collquery_add_results (void *user_data, int col_num, gchar **vals, gchar **cols) {
	int i;
	GHashTable *fields;
	GList **ret = (GList **)user_data;

	fields = g_hash_table_new (g_str_hash, g_str_equal);

	for (i = 0; i < col_num; i++) {
		g_hash_table_insert (fields, g_strdup (cols[i]), g_strdup (vals[i]));
	}

	*ret = g_list_append (*ret, fields);

	return 0;
}

void
test_collquery_strlist_to_glist (gchar **arr, GList **list) {
	gchar **i;

	if (!arr) {
		return;
	}

	for (i = arr; *i; i++) {
		*list = g_list_append (*list, *i);
	}
}

GList *
test_collquery_get_results (xmmsc_coll_t *coll, guint limit_start, guint limit_len, gchar **order, gchar **fetch, gchar **group) {
	GList *ret = NULL, *order_list = NULL, *fetch_list = NULL, *group_list = NULL;
	sqlite3 *db;
	GString *query;
	gchar *err_msg;
	xmms_coll_dag_t *dag;
	int query_success;

	dag = xmms_collection_init (NULL);
	test_collquery_strlist_to_glist (order, &order_list);
	test_collquery_strlist_to_glist (fetch, &fetch_list);
	test_collquery_strlist_to_glist (group, &group_list);

	query = xmms_collection_get_query (dag, coll, limit_start, limit_len, order_list, fetch_list, group_list);

	g_list_free (order_list);
	g_list_free (fetch_list);
	g_list_free (group_list);

	db = test_collquery_get_db_handle ();

	query_success = sqlite3_exec (db, query->str, test_collquery_add_results, &ret, &err_msg);
	OK(!query_success, "query successful");
	if (query_success && err_msg) {
		printf ("# sql message: `%s'\n", err_msg);
		printf ("# query was: `%s'\n", query->str);
	}

	g_string_free (query, TRUE);

	sqlite3_close (db);

	return ret;
}

void
test_collquery_compare_field (gpointer key, gpointer value, gpointer user_data) {
	gchar *got_value;
	GHashTable *got = (GHashTable *)user_data;

	got_value = g_hash_table_lookup (got, key);
	IS(got_value, (gchar *)value, "got expected value");
}

void
test_collquery_compare_result (GHashTable *got, GHashTable *expected) {
	g_hash_table_foreach (expected, test_collquery_compare_field, got);
}

void
test_collquery_compare_results (GList *got, GList *expected) {
	GList *i, *j;

	if (!expected) {
		OK(got == expected, "got no results");
		return;
	}

	if (!got) {
		OK(0, "expected results but got none");
		return;
	}

	OK(g_list_length (got) == g_list_length (expected), "got as much results as expected");

	for (i = got, j = expected; i && j; i = i->next, j = j->next) {
		test_collquery_compare_result (i->data, j->data);
	}
}

void
test_collquery (xmmsc_coll_t *coll, guint limit_start, guint limit_len, gchar **order, gchar **fetch, gchar **group, ...) {
	va_list ap;
	GHashTable *fields;
	GList *results, *expected_results = NULL, *i;
	gchar *key, *val;

	fields = g_hash_table_new (g_str_hash, g_str_equal);

	va_start (ap, group);

	while (1) {
		key = va_arg (ap, gchar *);
		if (!key || key[0] == '\0') {
			expected_results = g_list_append (expected_results, fields);
			fields = g_hash_table_new (g_str_hash, g_str_equal);

			if (!key) {
				break;
			}
		}
		else {
			val = va_arg (ap, gchar *);
			g_hash_table_insert (fields, key, val);
		}
	}

	va_end (ap);

	results = test_collquery_get_results (coll, limit_start, limit_len, order, fetch, group);

	test_collquery_compare_results (results, expected_results);

	for (i = expected_results; i; i = i->next) {
		g_hash_table_destroy (i->data);
	}

	g_list_free (expected_results);
}

#define COLL_EQUALS(operand, field, value) \
	COLL_FIELD_VALUE (EQUALS, operand, field, value);

#define COLL_MATCH(operand, field, value) \
	COLL_FIELD_VALUE (MATCH, operand, field, value);

#define COLL_FIELD_VALUE(type, operand, field, value) \
	xmmsc_coll_new (XMMS_COLLECTION_TYPE_##type); \
	xmmsc_coll_add_operand (coll, operand); \
	xmmsc_coll_attribute_set (coll, "field", field); \
	xmmsc_coll_attribute_set (coll, "value", value); \

#define TEST_COLLQUERY(type, operand, field, value, ...) \
	do { \
		xmmsc_coll_t *coll; \
		COLL_##type(coll, operand, field, value); \
		test_collquery (coll, ## __VA_ARGS__); \
	} while (0)

int
main (int argc, char **argv) {
	xmmsc_coll_t *coll;
	gchar *fetch[] = { "id", "artist", "title", NULL };
	gchar *order[] = { "id", NULL },

	PLAN_TESTS(19);

	g_thread_init (NULL);
	xmms_ipc_init ();
	xmms_config_init (TEST_INSTALL_DIR "/home/test/.config/xmms2/xmms2.conf");
	xmms_medialib_init (NULL);

	coll = COLL_MATCH (xmmsc_coll_universe (), "artist", "foo%");

	test_collquery (coll, 0, 0, order, fetch, NULL,
	                "id", "1", "artist", "foo", "title", "bar", "\0",
	                "id", "2", "artist", "foo", "title", "baz", "\0",
	                "id", "3", "artist", "foobar", "title", "baz", NULL);

	xmmsc_coll_unref (coll);
	coll = COLL_EQUALS (xmmsc_coll_universe (), "artist", "foo");

	test_collquery (coll, 0, 0, order, fetch, NULL,
	                "id", "1", "artist", "foo", "title", "bar", "\0",
	                "id", "2", "artist", "foo", "title", "baz", NULL);

	xmmsc_coll_unref (coll);

	return EXIT_STATUS;
}
