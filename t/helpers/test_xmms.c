#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/wait.h>
#include "test_xmms.h"
#include "test_helper_configuration.h"

#define __TAP_LIB__
#include "tap.h"
#undef __TAP_LIB__

static void
bail_out (void *null) {
	BAIL_OUT ("got disconnected");
}

xmmsc_connection_t *
test_xmms_launch (void) {
	xmmsc_connection_t *c;
	char *exec_path;
	char *plugin_path;
	pid_t pid;

	exec_path = TEST_INSTALL_DIR PREFIX BIN_DIR "/xmms2d";
	plugin_path = TEST_INSTALL_DIR PLUGIN_DIR;

	pid = fork ();

	if (!pid) {
		char *env[3];
		char *args[6];
		char *xdg_config_home;
		char *xdg_cache_home;
		int fd;

		args[0] = "xmms2d";
		args[1] = "-p";
		args[2] = plugin_path;
		args[3] = "-o";
		args[4] = "null";
		args[5] = NULL;

		xdg_config_home = "XDG_CONFIG_HOME=" TEST_INSTALL_DIR "/home/test/.config";
		xdg_cache_home = "XDG_CACHE_HOME=" TEST_INSTALL_DIR "/home/test/.cache";

		fd = open ("/dev/null", O_RDONLY);
		dup2 (fd, STDIN_FILENO);

		fd = open ("/dev/null", O_WRONLY);
		dup2 (fd, STDOUT_FILENO);
		dup2 (fd, STDERR_FILENO);

		env[0] = xdg_config_home;
		env[1] = xdg_cache_home;
		env[2] = NULL;

		execve (exec_path, args, env);

		printf ("failed to launch %s: %s\n", exec_path, strerror (errno));
	}

	sleep(1); /* wait for the server socket to get ready */

	c = xmmsc_init ("Test_XMMS");
	if (!xmmsc_connect (c, NULL)) {
		BAIL_OUT ("failed to connect to xmms2d");
	}

	xmmsc_disconnect_callback_set (c, bail_out, NULL);

	return c;
}

void
test_xmms_shutdown (xmmsc_connection_t *c) {
	xmmsc_result_t *res;
	int status;

	res = xmmsc_quit (c);
	xmmsc_result_wait (res);

	waitpid (-1, &status, 0);
}
