#include "tap.h"
#include "test_helper_configuration.h"
#include "xmmspriv/xmms_xform.h"
#include <glib.h>

typedef struct expected_results_St {
	const gchar *input_data;
	gint input_len;
	const gchar *output_mimetype_suffix;
	gboolean init_should_fail;
} expected_results_t;

expected_results_t expected_results[] = {
	{ "<foo>",               6, "foo",     0 },
	{ "<foo>xx",             8, "foo",     0 },
	{ "<bar",                5, NULL,      1 },
	{ "bar<",                5, NULL,      1 },
	{ "bar>",                5, NULL,      1 },
	{ "korv",                5, NULL,      1 },
	{ "<foo bar>",          10, "foo bar", 0 },
	{ "<!foo><bar>",        12, "bar",     0 },
	{ "<?foo><bar>",        12, "bar",     0 },
	{ "<!foo><?bar><korv>", 18, "korv",    0 },
};

const gchar *url = "korv://sausage";

void
test_xml (expected_results_t results) {
	xmms_xform_t *chain;
	xmms_xform_plugin_t *xml, *korv;
	GString *data;
	const xmms_stream_type_t *out_type;
	const gchar *mime_type;
	gchar *expected_mime_type;

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

	SKIP_START(!korv, results.init_should_fail ? 3 : 4, "couldn't get korv plugin");

	chain = xmms_xform_new (korv, chain, 1, NULL);
	OK(chain != NULL, "korv in chain");

	data = xmms_xform_private_data_get (chain);
	g_string_free (data, TRUE);

	data = g_string_new_len (results.input_data, results.input_len);
	xmms_xform_private_data_set (chain, data);

	xml = (xmms_xform_plugin_t *)xmms_plugin_find (XMMS_PLUGIN_TYPE_XFORM, "xml");
	OK(xml != NULL, "got xml plugin");

	SKIP_START(!xml, results.init_should_fail ? 1 : 2, "couldn't get xml plugin");

	chain = xmms_xform_new (xml, chain, 1, NULL);
	if (results.init_should_fail) {
		OK(chain == NULL, "init failed");
		return;
	} else {
		OK(chain != NULL, "xml in chain");
	}

	SKIP_START(!chain, 1, "init failed although we expected it to succeed");

	out_type = xmms_xform_outtype_get (chain);
	mime_type = xmms_stream_type_get_str (out_type, XMMS_STREAM_TYPE_MIMETYPE);

	expected_mime_type = g_strconcat ("application/x-xmms2-xml+",
	                                  results.output_mimetype_suffix,
	                                  NULL);
	IS(mime_type, expected_mime_type, "got correct mimetype");
	g_free (expected_mime_type);

	SKIP_END; /* xml not in chain */
	SKIP_END; /* missing xml plugin */
	SKIP_END; /* missing korv plugin */
}

int
main (int argc, char **argv) {
	int i;

	PLAN_TESTS(56);

	g_thread_init (NULL);
	xmms_log_init (0);
	xmms_ipc_init ();
	xmms_config_init ("goofy/home/xmms2d/.config/xmms2/xmms2.conf");
	xmms_medialib_init (NULL);
	xmms_plugin_init (TEST_INSTALL_DIR PLUGIN_DIR);

	for (i = 0; i < G_N_ELEMENTS (expected_results); i++) {
		test_xml (expected_results[i]);
	}

	return EXIT_STATUS;
}
