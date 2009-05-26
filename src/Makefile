test_src = s4.c main.c pat.c strstore.c
bdb_src = bdb.c

all:
	gcc -g -Wall -o test $(test_src)
	gcc -g -Wall -O3 -ldb -o bdb $(bdb_src)
