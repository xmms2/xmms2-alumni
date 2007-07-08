CFLAGS = -I../../../src/include $(shell pkg-config --cflags glib-2.0 sqlite3) -DXMMS_VERSION="\"0.2 DrJekyll+WIP (git commit: 0347fd2deee476b26226b9839ec34f6de6459d84 + local changes)\""
LDFLAGS = $(shell pkg-config --libs glib-2.0 sqlite3 gmodule-2.0)

libxmms_korv.so: korv.o
	$(CC) $(LDFLAGS) -shared -o $@ $< \
		../../../_build_/default/src/xmms/libxmms2core.a \
		../../../_build_/default/src/lib/xmmstypes/libxmmstypes.a \
		../../../_build_/default/src/lib/xmmsutils/libxmmsutils.a \
		../../../_build_/default/src/lib/xmmsipc/libxmmsipc.a \
		../../../_build_/default/src/lib/xmmssocket/libxmmssocket.a

all: libxmms_korv.so

.PHONY: all
