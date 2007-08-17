#include "tap.h"
#include "test_helper_configuration.h"
#include "xmmspriv/xmms_xform.h"
#include <glib.h>

const gchar *url = "korv://sausage";

typedef struct expected_result_St {
	const gchar *input_data;
	gint input_length;
	gboolean parse_success;
	GList *expected_uris;
} expected_result_t;

void
test_rss (expected_result_t *result) {
	xmms_xform_t *chain;
	xmms_xform_plugin_t *rss, *korv;
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

	rss = (xmms_xform_plugin_t *)xmms_plugin_find (XMMS_PLUGIN_TYPE_XFORM, "rss");
	OK(rss != NULL, "got rss plugin");

	chain = xmms_xform_new (rss, chain, 1, NULL);
	OK(chain != NULL, "rss in chain");

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

	OK(g_list_length (browse_results) == g_list_length (result->expected_uris), "right number of results");

	for (i = browse_results, j = result->expected_uris; i && j; i = i->next, j = j->next) {
		xmms_object_cmd_value_t *realpath, *isdir;
		xmms_object_cmd_value_t *val = i->data;
		GHashTable *dict;

		realpath = (xmms_object_cmd_value_t *)g_hash_table_lookup (val->value.dict, "realpath");
		isdir = (xmms_object_cmd_value_t *)g_hash_table_lookup (val->value.dict, "isdir");

		OK(realpath != NULL, "realpath set");
		OK(isdir != NULL, "isdir set");

		SKIP_START(!realpath, 2, "realpath not set");
		OK(realpath->type == XMMS_OBJECT_CMD_ARG_STRING, "realpath is a string");
		IS(realpath->value.string, (gchar *)j->data, "realpath looks as expected");
		SKIP_END;

		SKIP_START(!isdir, 2, "isdir not set");
		OK(isdir->type == XMMS_OBJECT_CMD_ARG_INT32, "isdir is an integer");
		OK(isdir->value.int32 == 0, "isdir is false");
		SKIP_END;
	}
}

int
main (int argc, char **argv) {
	expected_result_t *result;
	GList *results = NULL, *i;

	PLAN_TESTS(60);

	g_thread_init (NULL);
	xmms_log_init (0);
	xmms_ipc_init ();
	xmms_config_init ("goofy/home/xmms2d/.config/xmms2/xmms2.conf");
	xmms_medialib_init (NULL);
	xmms_plugin_init (TEST_INSTALL_DIR PLUGIN_DIR);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<enclosure url=\"foo\" />";
	result->input_length = 24;
	result->parse_success = 1;
	result->expected_uris = g_list_append (NULL, g_strdup ("foo"));

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<enclosure urll=\"bar\">";
	result->input_length = 23;
	result->parse_success = 1;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<enclosurekorv url=\"korv\" />";
	result->input_length = 27;
	result->parse_success = 1;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<enclosure url=\"foo\" url=\"bar\">";
	result->input_length = 32;
	result->parse_success = 0;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "enclosure url=\"foo\" />";
	result->input_length = 21;
	result->parse_success = 0;

	results = g_list_append (results, result);

	result = g_new0 (expected_result_t, 1);
	result->input_data = "<xml><enclosure foo=\"bar\" url=\"moo\" bar=\"foo\" />"
	                     "<foo />"
						 "<enclosure url=\"kooh\" /></xml>";
	result->input_length = 86;
	result->parse_success = 1;
	result->expected_uris = g_list_append (result->expected_uris, g_strdup ("moo"));
	result->expected_uris = g_list_append (result->expected_uris, g_strdup ("kooh"));

	results = g_list_append (results, result);

	for (i = results; i; i = i->next) {
		test_rss ((expected_result_t *)i->data);
	}

	/* TODO: free results */

	return EXIT_STATUS;
}
