#include "tap.h"
#include "test_xmms.h"
#include "xmmsclient/xmmsclient.h"

int
main (int argc, char **argv) {
	xmmsc_connection_t *c;
	xmmsc_result_t *res;

	PLAN_TESTS(3);

	c = test_xmms_launch ();

	res = xmmsc_configval_list (c);
	OK(res != NULL, "got result");

	xmmsc_result_wait (res);
	OK(!xmmsc_result_iserror (res), "successful request");

	OK(xmmsc_result_get_type (res) == XMMS_OBJECT_CMD_ARG_DICT, "got dict");

	test_xmms_shutdown (c);

	return EXIT_STATUS;
}
