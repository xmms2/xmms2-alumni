#include "tap.h"
#include "url.h"

struct expected_result {
	char *in_url;

	char *protocol;
	char *username;
	char *password;

	int ipv6_host;
	char *host;
	char *port;

	char *path;
};

struct expected_result expected_results[] = {
	{ "unix:///foo",                  "unix", "",    "",    0, "",        "",        "foo"  },
	{ "unix:////foo",                 "unix", "",    "",    0, "",        "",        "/foo" },
	{ "unix://foo",                   "unix", "",    "",    0, "foo",     "",        ""     }, /* TODO: err.. unix host? */
	{ "tcp://foo.bar",                "tcp",  "",    "",    0, "foo.bar", "",        ""     },
	{ "tcp://foo.bar:9968",           "tcp",  "",    "",    0, "foo.bar", "9968",    ""     },
	{ "tcp://foo.bar:invalid",        "tcp",  "",    "",    0, "foo.bar", "invalid", ""     }, /* TODO: FAIL! */
	{ "tcp://[foo.bar]",              "tcp",  "",    "",    1, "foo.bar", "",        ""     },
	{ "tcp://[::1]",                  "tcp",  "",    "",    1, "::1",     "",        ""     },
	{ "tcp://foo:bar@foo.bar:9968",   "tcp",  "foo", "bar", 0, "foo.bar", "9968",    ""     },
	{ "tcp://foo:bar@[foo.bar]:9968", "tcp",  "foo", "bar", 1, "foo.bar", "9968",    ""     },
	{ " ",                            "",     "",    "",    0, " ",       "",        ""     }, /* TODO: FAIL! */
};

void
check_url (xmms_url_t *url, struct expected_result expected) {
	IS(url->protocol, expected.protocol, "correct protocol");
	IS(url->username, expected.username, "correct username");
	IS(url->password, expected.password, "correct password");
	OK(url->ipv6_host == expected.ipv6_host, "correct ipv6_host");
	IS(url->host, expected.host, "correct host");
	IS(url->port, expected.port, "correct port");
	IS(url->path, expected.path, "correct path");
}

int
main (int argc, char **argv) {
	xmms_url_t *url;
	int num_tests = sizeof (expected_results) / sizeof (expected_results[0]);
	int cur_test;

	PLAN_TESTS(num_tests * 7);

	for (cur_test = 0; cur_test < num_tests; cur_test++) {
		struct expected_result expected = expected_results[cur_test];

		url = parse_url(expected.in_url);

		check_url(url, expected);

		if (url) {
			free_url (url);
		}
	}

	return EXIT_STATUS;
}
