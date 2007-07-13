CFLAGS = -g -O0 -I../../include -I../../../src/lib/xmmsipc -I../../../src/include

TESTS = parse_url.t client_server.t

all: $(TESTS)

parse_url.t: ../../../_build_/default/src/lib/xmmsipc/url.o

client_server.t: ../../../_build_/default/src/lib/xmmsipc/libxmmsipc.a ../../../_build_/default/src/lib/xmmssocket/libxmmssocket.a

clean:
	rm -f $(TESTS)

.PHONY: all
