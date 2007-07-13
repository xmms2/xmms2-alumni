CFLAGS = -I../include -I../../src/include -I../../src/includepriv -I../helpers/

TESTS = config.t

all: $(TESTS)

config.t: ../helpers/test_xmms.o ../../_build_/default/src/clients/lib/xmmsclient/libxmmsclient.so

clean:
	rm -f $(TESTS)

.PHONY: all
