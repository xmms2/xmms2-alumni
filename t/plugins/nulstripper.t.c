#include "tap.h"
#include "test_helper_configuration.h"
#include "xmmspriv/xmms_xform.h"
#include <glib.h>

struct expected_results_t {
	const char *input_data;
	int input_len;
	const char *output_data;
	int output_len;
	int init_should_fail;
};

struct expected_results_t expected_results[] = {
	{ "\0\0\0korv", 8, "korv",     5, 0 },
	{ "\0korv\0\0", 7, "korv\0\0", 6, 0 },
	{ "korv",       5, NULL,       0, 1 },
};

const gchar *url = "korv://sausage";

void
test_nulstripper (struct expected_results_t results) {
	char buf[4096];
	xmms_error_t *err;
	xmms_xform_t *xform;
	xmms_xform_plugin_t *nulstripper, *korv;
	GString *data;
	int ret;

	xform = xmms_xform_new (NULL, NULL, 0, NULL);
	OK(xform != NULL, "new xform");

	xmms_xform_outdata_type_add (xform,
	                             XMMS_STREAM_TYPE_MIMETYPE,
	                             "application/x-url",
	                             XMMS_STREAM_TYPE_URL,
	                             url,
	                             XMMS_STREAM_TYPE_END);

	korv = (xmms_xform_plugin_t *)xmms_plugin_find (XMMS_PLUGIN_TYPE_XFORM, "korv");
	OK(korv != NULL, "got korv plugin");

	SKIP_START(!korv, results.init_should_fail ? 3 : 5, "couldn't find korv plugin");

	xform = xmms_xform_new (korv, xform, 1, NULL);
	OK(xform != NULL, "korv in chain");

	data = xmms_xform_private_data_get (xform);
	g_string_free (data, TRUE);

	data = g_string_new_len (results.input_data, results.input_len);
	xmms_xform_private_data_set (xform, data);

	nulstripper = (xmms_xform_plugin_t *)xmms_plugin_find (XMMS_PLUGIN_TYPE_XFORM, "nulstripper");
	OK(nulstripper != NULL, "got nulstripper plugin");

	SKIP_START(!nulstripper, results.init_should_fail ? 1 : 3, "couldn't find nulstripper plugin");

	xform = xmms_xform_new (nulstripper, xform, 1, NULL);
	if (results.init_should_fail) {
		OK(!xform, "nulstripper init failed");
		return;
	}
	else {
		OK(xform != NULL, "nulstripper in chain");
	}

	chain_finalize (xform, 1, url);

	ret = xmms_xform_this_read (xform, buf, sizeof (buf), err);

	OK(ret == results.output_len, "output data has expected length");

	OK(!strncmp (buf, results.output_data, ret), "output data has expected content");

	SKIP_END;
	SKIP_END;
}

int main (int argc, char **argv) {
	int i;
	int num_tests = sizeof (expected_results) / sizeof (expected_results[0]);

	PLAN_TESTS(19);

	g_thread_init (NULL);
	xmms_log_init (0);
	xmms_ipc_init ();
	xmms_config_init ("goofy/home/xmms2d/.config/xmms2/xmms2.conf");
	xmms_medialib_init (NULL);
	xmms_plugin_init (TEST_INSTALL_DIR PLUGIN_DIR);

	for (i = 0; i < num_tests; i++) {
		test_nulstripper (expected_results[i]);
	}

	return EXIT_STATUS;
}
