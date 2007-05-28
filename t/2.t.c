#include "tap.h"

int main (int argc, char **argv) {
	PLAN_TESTS(5);
	OK(1, "1 is true");

	SKIP_START(1, 2, "only root can do that");
	OK1(1);
	OK1(1);
	SKIP_END;

	TODO_START("bug");
	OK1(0);
	OK1(1);
	TODO_END;

	return EXIT_STATUS;
}
