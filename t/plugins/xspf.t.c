#include "tap.h"
#include "test_helper_configuration.h"
#include "xmmspriv/xmms_xform.h"
#include <glib.h>

const gchar *url = "korv://sausage";

typedef struct expected_result_St {
	gchar *input_data;
	gint input_length;
	gboolean parse_success;
	GList *expected_entries;
} expected_result_t;

gboolean
test_xspf_check_entry (gpointer key, gpointer value, gpointer user_data) {
	GHashTable *entry = user_data;

	if (strcmp (key, "location") == 0) {
		xmms_object_cmd_value_t *path = g_hash_table_lookup (entry, "realpath");
		OK(path != NULL, "path defined");

		SKIP_START(!path, 1, "path not defined");
		IS(path->value.string, value, "parsed location");
		SKIP_END;

		g_hash_table_remove (entry, "realpath");
	} else {
		xmms_object_cmd_value_t *prop = g_hash_table_lookup (entry, key);
		OK(prop != NULL, "prop defined");

		SKIP_START(!prop, 1, "prop not defined");
		switch (prop->type) {
			case XMMS_OBJECT_CMD_ARG_STRING:
				IS(prop->value.string, value, "correct prop");
				break;
			case XMMS_OBJECT_CMD_ARG_INT32:
				OK(prop->value.int32 == GPOINTER_TO_INT (value), "correct prop");
				break;
			default:
				OK(0, "unexpected prop type");
		}
		SKIP_END;

		g_hash_table_remove (entry, key);
	}

	return TRUE;
}

void
test_xspf (expected_result_t *result) {
	xmms_xform_t *chain;
	xmms_xform_plugin_t *xspf, *korv;
	GString *data;
	GList *browse_results, *i, *j;
	xmms_error_t error;

	chain = xmms_xform_new (NULL, NULL, 0, NULL);
	OK(chain != NULL, "new xform chain");

	xmms_xform_outdata_type_add (chain,
	                             XMMS_STREAM_TYPE_MIMETYPE,
	                             "application/x-url",
	                             XMMS_STREAM_TYPE_URL,
	                             url,
	                             XMMS_STREAM_TYPE_END);

	korv = (xmms_xform_plugin_t *)xmms_plugin_find (XMMS_PLUGIN_TYPE_XFORM, "korv");
	OK(korv != NULL, "got korv plugin");

	chain = xmms_xform_new (korv, chain, 1, NULL);
	OK(chain != NULL, "korv in chain");

	data = xmms_xform_private_data_get (chain);
	g_string_free (data, TRUE);

	data = g_string_new_len (result->input_data, result->input_length);
	xmms_xform_private_data_set (chain, data);

	xspf = (xmms_xform_plugin_t *)xmms_plugin_find (XMMS_PLUGIN_TYPE_XFORM, "xspf");
	OK(xspf != NULL, "got xspf plugin");

	chain = xmms_xform_new (xspf, chain, 1, NULL);
	OK(chain != NULL, "xspf in chain");

	xmms_error_reset (&error);
	browse_results = xmms_xform_browse_method (chain, url, &error);

	if (result->parse_success) {
		gboolean is_error = xmms_error_iserror (&error);

		OK(!is_error, "parsing successful");

		if (is_error) {
			printf ("# parse error: %s\n", xmms_error_message_get (&error));
		}
	} else {
		OK(xmms_error_iserror (&error), "parsing failed");
	}

	OK(g_list_length (browse_results) == g_list_length (result->expected_entries), "right number of results");

	for (i = browse_results, j = result->expected_entries; i && j; i = i->next, j = j->next) {
		xmms_object_cmd_value_t *val = i->data;
		GHashTable *expected = j->data;

		g_hash_table_foreach (expected, test_xspf_check_entry, val->value.dict);
		/* isdir + path */
		OK(g_hash_table_size (val->value.dict) <= 2, "plugin set no additional properties");
	}
}

GHashTable *
test_xspf_new_entry_hash (gpointer key1, gpointer value1, ...) {
	va_list ap;
	GHashTable *entry;
	gpointer key, value;

	entry = g_hash_table_new (g_str_hash, g_str_equal);
	g_hash_table_insert (entry, key1, value1);

	va_start (ap, value1);

	while (1) {
		if (!(key = va_arg (ap, gpointer))) {
			break;
		}

		value = va_arg (ap, gpointer);

		g_hash_table_insert (entry, key, value);
	}

	va_end (ap);

	return entry;
}

int
main (int argc, char **argv) {
	expected_result_t *result;
	GList *results = NULL, *i;

	PLAN_TESTS(86);

	g_thread_init (NULL);
	xmms_log_init (0);
	xmms_ipc_init ();
	xmms_config_init ("goofy/home/xmms2d/.config/xmms2/xmms2.conf");
	xmms_medialib_init (NULL);
	xmms_plugin_init (TEST_INSTALL_DIR PLUGIN_DIR);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"3\" />";
	result->input_length = 54;
	result->parse_success = 0;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" />";
	result->input_length = 54;
	result->parse_success = 0;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList /><trackList /></playlist>";
	result->input_length = 90;
	result->parse_success = 0;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList /></playlist>";
	result->input_length = 77;
	result->parse_success = 1;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList>"
	                     "<track /></trackList></playlist>";
	result->input_length = 96;
	result->parse_success = 1;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList>"
	                     "<track><location>foo</location><title>bar</title></track>"
	                     "</trackList></playlist>";
	result->input_length = 144;
	result->parse_success = 1;
	result->expected_entries = g_list_append (NULL, test_xspf_new_entry_hash ("location", "foo", "title", "bar",  NULL));

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList>"
	                     "<track><location>foo</location><titlee>bar</titlee></track>"
	                     "</trackList></playlist>";
	result->input_length = 146;
	result->parse_success = 1;
	result->expected_entries = g_list_append (NULL, test_xspf_new_entry_hash ("location", "foo", NULL));

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList>"
	                     "<track><location></location><title>bar</title></track>"
	                     "<track><location>foo</location><title>bar</title></track>"
	                     "</trackList></playlist>";
	result->input_length = 198;
	result->parse_success = 1;
	result->expected_entries = g_list_append (NULL, test_xspf_new_entry_hash ("location", "foo", "title", "bar", NULL));

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<playlist xmlns=\"http://xspf.org/ns/0/\" version=\"1\" ><trackList>"
	                     "<track><location>bar</location><title>bar</title></track>"
	                     "<track><location>foo</location><title>bar</title></track>"
	                     "</trackList></playlist>";
	result->input_length = 201;
	result->parse_success = 1;
	result->expected_entries = g_list_append (result->expected_entries, test_xspf_new_entry_hash ("location", "bar", "title", "bar", NULL));
	result->expected_entries = g_list_append (result->expected_entries, test_xspf_new_entry_hash ("location", "foo", "title", "bar", NULL));

	results = g_list_append (results, result);

	for (i = results; i; i = i->next) {
		test_xspf (i->data);
	}

	return EXIT_STATUS;
}
