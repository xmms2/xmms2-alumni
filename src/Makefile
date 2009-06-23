src = s4.c set.c entry.c main.c query.c
obj = $(addsuffix .o, $(basename $(src)))
backfile = $(CURDIR)/s4be.o
backend = homegrown

CFLAGS += -g -I $(CURDIR) -Wall -I /usr/include/xmms2

export CFLAGS
export backfile

all: $(obj) backend
	gcc -o s4_test $(obj) $(backfile) -lxmmsclient -lpthread

$(obj): %.o : %.c
	gcc -c $(CFLAGS) $< -o $@

backend:
	make -C $(backend)

clean:
	make -C $(backend) clean
	rm *.o
