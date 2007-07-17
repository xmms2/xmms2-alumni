CFLAGS = -O0 -g -I../include -I../../src/include -I../../src/includepriv -I../helpers/ $(shell pkg-config --cflags --libs glib-2.0 gmodule-2.0 gthread-2.0 sqlite3)

TESTS = config.t collquery.t

all: $(TESTS)

config.t: ../helpers/test_xmms.o ../../_build_/default/src/clients/lib/xmmsclient/libxmmsclient.so

collquery.t: ../../_build_/default/src/xmms/libxmms2core.a ../../_build_/default/src/lib/xmmstypes/libxmmstypes.a ../../_build_/default/src/lib/xmmsipc/libxmmsipc.a ../../_build_/default/src/lib/xmmsutils/libxmmsutils.a ../../_build_/default/src/lib/xmmssocket/libxmmssocket.a

clean:
	rm -f $(TESTS)

.PHONY: all
