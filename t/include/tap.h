#ifndef __TAP_H__
#define __TAP_H__

#include <stdio.h>

#ifndef __TAP_LIB__
int tap_tests_planned = -1;
int tap_tests_ran = 0;
int tap_skip_num;
char *tap_skip_reason;
char *tap_todo_reason = NULL;
#endif

#define PLAN_TESTS(x) \
	tap_tests_planned = x; \
	printf ("1..%d\n", x);

#define OK1(test) \
	OK(test, NULL);

#define OK(test, desc) { \
		tap_tests_ran++; \
		int success = test; \
		if (!success) { \
			printf ("not "); \
		} \
		printf ("ok %d", tap_tests_ran); \
		if (desc) \
			printf (" - %s", desc); \
		if (tap_todo_reason) \
			printf (" # TODO %s", tap_todo_reason); \
		printf ("\n"); \
		if (!success) { \
			printf ("#    Failed test "); \
			if (desc) \
				printf ("'%s' ", desc); \
			printf ("in %s at line %d\n", __FILE__, __LINE__); \
		} \
	}

#define IS(a, b, desc) \
	do { \
		if (!a && !b) { \
			OK(1, desc); \
		} else if (!a || !b || strcmp (a, b)) { \
			OK(0, desc); \
			printf("# expected: '%s'\n# got: '%s'\n", b, a); \
		} else { \
			OK(1, desc); \
		} \
	} while (0);

#define EXIT_STATUS \
	!(tap_tests_ran == tap_tests_planned)

#define SKIP(num, desc) { \
		int i = 0; \
		while (i++ < num) { \
			tap_tests_ran++; \
			printf ("ok %d # skip %s\n", tap_tests_ran, desc); \
		} \
	}

#define TODO_START(desc) \
	tap_todo_reason = desc;

#define TODO_END \
	tap_todo_reason = NULL;

#define SKIP_START(cond, num, desc) \
	tap_skip_num = num; \
	tap_skip_reason = desc; \
	if (!cond) { \

#define SKIP_END \
	} else { \
		SKIP(tap_skip_num, tap_skip_reason); \
	}

#define BAIL_OUT(msg) \
	do { \
		printf ("screw this!  %s\n", msg); \
		exit (255); \
	} while (0);

#endif /* __TAP_H__ */
