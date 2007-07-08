CFLAGS = -g -O0 -I../../include -I../../../src/lib/xmmsipc -I../../../src/include

all: parse_url.t client_server.t

parse_url.t: ../../../_build_/default/src/lib/xmmsipc/url.o

client_server.t: ../../../_build_/default/src/lib/xmmsipc/libxmmsipc.a ../../../_build_/default/src/lib/xmmssocket/libxmmssocket.a

run: parse_url.t
	runtests $(VERBOSE) -e'' ./parse_url.t

.PHONY: all run
