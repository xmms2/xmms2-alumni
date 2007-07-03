#include <stdlib.h>
#include <sys/param.h>
#include <string.h>
#include "tap.h"
#include "xmmsc/xmmsc_util.h"

void
test_xmms_default_ipcpath_get () {
	const char *default_path_prefix = "unix:///tmp/xmms-ipc-";
	char buf[PATH_MAX];

	xmms_default_ipcpath_get (buf, PATH_MAX);

	OK(!strncmp (buf, default_path_prefix, strlen (default_path_prefix)), "default ipc path looks sane");

	xmms_default_ipcpath_get (buf, 8);
	IS(buf, "unix://", "xmms_default_ipcpath_get respects max length");
}

void
test_xmms_userconfdir_get () {
	char buf[PATH_MAX];
	char *env_userconfdir = "something that's unlikely for XDG_CONFIG_HOME to be set to";

	unsetenv ("XDG_CONFIG_HOME");

	OK(xmms_userconfdir_get (buf, PATH_MAX) != NULL, "xmms_userconfdir_get");
	OK(strcmp (buf, env_userconfdir), "... looks sane without XDG_CONFIG_HOME set");

	setenv ("XDG_CONFIG_HOME", env_userconfdir, 1);

	OK(xmms_userconfdir_get (buf, PATH_MAX) != NULL, "xmms_userconfdir_get");
	OK(!strncmp (buf, env_userconfdir, strlen (env_userconfdir)), "... looks sane with XDG_CONFIG_HOME set");
}

void
test_xmms_usercachedir_get () {
	char buf[PATH_MAX];
	char *env_usercachedir = "something that's unlikely for XDG_CACHE_HOME to be set to";

	unsetenv ("XDG_CACHE_HOME");

	OK(xmms_usercachedir_get (buf, PATH_MAX) != NULL, "xmms_usercachedir_get");
	OK(strcmp (buf, env_usercachedir), "... looks sane without XDG_CACHE_HOME set");

	setenv ("XDG_CACHE_HOME", env_usercachedir, 1);

	OK(xmms_usercachedir_get (buf, PATH_MAX) != NULL, "xmms_usercachedir_get");
	OK(!strncmp (buf, env_usercachedir, strlen (env_usercachedir)), "... looks sane with XDG_CACHE_HOME set");
}

int
main (int argc, char **argv) {
	PLAN_TESTS(10);

	test_xmms_default_ipcpath_get ();
	test_xmms_userconfdir_get ();
	test_xmms_usercachedir_get ();

	return EXIT_STATUS;
}
