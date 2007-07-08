CFLAGS = -g -O0 -I../include -I../../src/includepriv -I../../src/include $(shell pkg-config --libs --cflags glib-2.0 gthread-2.0 gmodule-2.0 sqlite3)

all: nulstripper

nulstripper: ../../_build_/default/src/xmms/libxmms2core.a ../../_build_/default/src/lib/xmmstypes/libxmmstypes.a ../../_build_/default/src/lib/xmmsipc/libxmmsipc.a ../../_build_/default/src/lib/xmmsutils/libxmmsutils.a ../../_build_/default/src/lib/xmmssocket/libxmmssocket.a

.PHONY: all
