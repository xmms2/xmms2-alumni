CFLAGS = -I../../include -I../../../src/include

TESTS = unix.t

all: $(TESTS)

$(TESTS): ../../../_build_/default/src/lib/xmmsutils/libxmmsutils.a

clean:
	rm -f $(TESTS)
