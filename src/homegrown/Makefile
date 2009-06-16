src = be.c intpair.c strstore.c pat.c bpt.c
obj = $(addsuffix .o, $(basename $(src)))

all: $(backfile)

$(backfile): $(obj)
	ld -r $(obj) -o $(backfile)

$(obj): %.o : %.c
	gcc -c $(CFLAGS) $< -o $@

clean:
	rm *.o
