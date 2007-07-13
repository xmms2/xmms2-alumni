SUBDIRS = xmmsipc xmmsutils

RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	for dir in $(SUBDIRS); \
		do $(MAKE) -C $$dir $@; \
	done

.PHONY: $(RECURSIVE_TARGETS)
