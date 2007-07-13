SUBDIRS = plugins lib xmms

RECURSIVE_TARGETS = all clean

all::
	perl -Ihelpers/lib -mTest::XMMS -e'Test::XMMS::create_std_goof_plugin_dir()'
	$(MAKE) -C helpers/plugins symlink

clean::
	rm -rf goofy

$(RECURSIVE_TARGETS)::
	for dir in $(SUBDIRS); \
		do $(MAKE) -C $$dir $@; \
	done

test: all
	-./harness

.PHONY: $(RECURSIVE_TARGETS) test
