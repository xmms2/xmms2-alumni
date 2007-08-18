#include "tap.h"
#include "test_xmms.h"
#include "xmmsclient/xmmsclient.h"
#include <glib.h>

typedef enum {
	RESULT_CONNECT_FAILED,
	RESULT_STATS_FAILED,
	RESULT_OK
} client_test_result;

static guint client_nums[] = { 2, 5, 10, 25, 50, 100, 150 }; //, 200, 400 };

static gboolean start_connect = FALSE;

GCond *connect_cond = NULL;
GMutex *connect_mutex = NULL;

gpointer do_connect (gpointer data) {
	xmmsc_result_t *res;
	xmmsc_connection_t *conn;

	conn = xmmsc_init ("clients_test");

	while (!start_connect) {
		g_cond_wait (connect_cond, connect_mutex);
	}

	if (!xmmsc_connect (conn, NULL)) {
		return GINT_TO_POINTER (RESULT_CONNECT_FAILED);
	}

	res = xmmsc_main_stats (conn);
	xmmsc_result_wait (res);

	if (xmmsc_result_iserror (res)) {
		return GINT_TO_POINTER (RESULT_STATS_FAILED);
	}

	return GINT_TO_POINTER (RESULT_OK);
}

static void
test_concurrent_connections (guint client_num) {
	guint i;
	GThread *thread;
	GList *it, *threads = NULL;
	GError *error = NULL;

	for (i = 0; i < client_num; i++) {
		thread = g_thread_create (do_connect, NULL, TRUE, &error);

		if (!thread) {
			g_error ("# failed to create thread: %s\n", error->message);
			break;
		}

		threads = g_list_prepend (threads, thread);
	}

	g_mutex_lock (connect_mutex);
	start_connect = TRUE;
	g_cond_signal (connect_cond);
	g_mutex_unlock (connect_mutex);

	for (it = threads; it; it = it->next) {
		client_test_result ret;

		ret = GPOINTER_TO_INT (g_thread_join (it->data));

		switch (ret) {
			case RESULT_CONNECT_FAILED:
				OK(0, "connection succeeded");
				printf ("# connection failed\n");
				break;
			case RESULT_STATS_FAILED:
				OK(0, "connection succeeded");
				printf ("# connection geting stats failed\n");
				break;
			case RESULT_OK:
				OK(1, "connection succeeded");
				break;
		}
	}
}

gint
main (gint argc, gchar **argv) {
	xmmsc_connection_t *c;
	guint i, num_tests = 0;

	for (i = 0; i < G_N_ELEMENTS (client_nums); i++) {
		num_tests += client_nums[i];
	}

	PLAN_TESTS(num_tests);

	c = test_xmms_launch ();

	g_thread_init (NULL);

	connect_cond = g_cond_new ();
	connect_mutex = g_mutex_new ();

	for (i = 0; i < G_N_ELEMENTS (client_nums); i++) {
		test_concurrent_connections (client_nums[i]);
	}

	test_xmms_shutdown (c);

	return EXIT_STATUS;
}
