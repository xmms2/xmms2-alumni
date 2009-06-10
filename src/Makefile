src = s4.c set.c entry.c main.c
obj = $(addsuffix .o, $(basename $(src)))
backfile = $(CURDIR)/s4be.o
backend = bdb

CFLAGS += -g -I $(CURDIR)

export CFLAGS
export backfile

all: $(obj) backend
	gcc -o s4_test $(obj) $(backfile) -ldb

$(obj): %.o : %.c
	gcc -c $(CFLAGS) $< -o $@

backend:
	make -C $(backend)

clean:
	make -C $(backend) clean
	rm *.o
