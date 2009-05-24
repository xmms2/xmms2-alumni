#include "s4.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>



typedef struct s4_header_St {
	int32_t string_store, int_store;
	int32_t alloc_off;
} s4_header_t;



static inline long pagesize ()
{
	return sysconf (_SC_PAGESIZE);
}

static inline int align (int a, int b)
{
	return a + ((b - (a % b)) % b);
}

/* Grow the database by the number of bytes given.
 * It will align to a pagesize boundry.
 */
static void grow_db (s4_t *s4, int n)
{
	n = align (n, pagesize());

	munmap(s4->map, s4->size);

	s4->size += n;
	ftruncate(s4->fd, s4->size);

	s4->map = mmap (NULL, s4->size, PROT_READ | PROT_WRITE, MAP_SHARED, s4->fd, 0);
}


/* Initilize a new database */
static void init_db (s4_t *s4)
{
	s4_header_t *header;

	grow_db (s4, 1);

	header = s4->map;
	header->string_store = -1;
	header->alloc_off = sizeof(s4_header_t);
}


/**
 * Open an s4 database
 *
 * @param filename The file to open
 * @return A pointer to an s4 structure, or NULL on error
 */
s4_t *s4_open (const char* filename)
{
	struct stat stat_buf;
	s4_t* s4 = malloc (sizeof(s4_t));

	s4->fd = open (filename, O_RDWR | O_CREAT, 0644);
	if (s4->fd == -1) {
		free (s4);
		return NULL;
	}

	fstat(s4->fd, &stat_buf);
	s4->size = stat_buf.st_size;
	if (s4->size == 0) {
		init_db(s4);
	}
	else {
		s4->map = mmap (NULL, s4->size, PROT_READ | PROT_WRITE,	MAP_SHARED, s4->fd, 0);
		if (s4->map == MAP_FAILED) {
			return NULL;
		}
	}

	return s4;
}


/**
 * Close an open s4 database
 *
 * @param s4 The database to close
 * @return 0 on success, anything else on error
 */
int s4_close (s4_t* s4)
{
	munmap (s4->map, s4->size);
	close (s4->fd);

	return 0;
}


/**
 * Allocate atleast n bytes in the database and return the offset
 *
 * @param n The number of bytes needed
 * @return The offset into the database
 */
int32_t s4_alloc (s4_t* s4, int n)
{
	s4_header_t* header = s4->map;
	int32_t ret = header->alloc_off;
	header->alloc_off += n;

	if ((n + ret) > s4->size)
		grow_db (s4, n + ret - s4->size);

	return ret;
}


/**
 * Free the allocation at offset off
 *
 * @param off The allocation to free
 */
void s4_free(s4_t* s4, int32_t off)
{
	/* DUMMY, write some real code here */
}


int32_t s4_get_string_store (s4_t *s4)
{
	return ((s4_header_t*)s4->map)->string_store;
}

void s4_set_string_store (s4_t *s4, int32_t ss)
{
	((s4_header_t*)s4->map)->string_store = ss;
}
