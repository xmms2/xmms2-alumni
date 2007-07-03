#include <string.h>
#include <stdlib.h>
#include <sys/wait.h>
#include "test_xmms.h"

static char *top_dir = "/home/rafl/projects/c/xmms2/xmms2-rafl/";

xmmsc_connection_t *
test_xmms_launch (void) {
	xmmsc_connection_t *c;
	char exec_path[PATH_MAX];
	char plugin_path[PATH_MAX];
	pid_t pid;

	strcat (exec_path, top_dir);
	strcat (exec_path, "_build_/default/src/xmms/xmms2d");

	strcat (plugin_path, top_dir);
	strcat (plugin_path, "t/goofy/plugins");

	pid = fork ();

	if (!pid) {
		char *env[3];
		char *args[6];
		char xdg_config_home[PATH_MAX + 35];
		char xdg_cache_home[PATH_MAX + 33];
		int fd;

		args[0] = "xmms2d";
		args[1] = "-p";
		args[2] = plugin_path;
		args[3] = "-o";
		args[4] = "null";
		args[5] = NULL;

		strcat (xdg_config_home, "XDG_CONFIG_HOME=");
		strcat (xdg_config_home, top_dir);
		strcat (xdg_config_home, "t/goofy/home/xmms2d/.config");

		strcat (xdg_cache_home, "XDG_CACHE_HOME=");
		strcat (xdg_cache_home, top_dir);
		strcat (xdg_cache_home, "t/goofy/home/xmms2d/.cache");

		fd = open ("/dev/null", O_RDONLY);
		dup2 (fd, STDIN_FILENO);

		fd = open ("/dev/null", O_WRONLY);
		dup2 (fd, STDOUT_FILENO);
		dup2 (fd, STDERR_FILENO);

		env[0] = xdg_config_home;
		env[1] = xdg_cache_home;
		env[2] = NULL;

		execve (exec_path, args, env);
	}

	sleep(1); /* wait for the server socket to get ready */

	c = xmmsc_init ("Test_XMMS");
	if (!xmmsc_connect (c, NULL)) {
		printf("failed to connect to xmms2d\n");
		abort();
	}

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
