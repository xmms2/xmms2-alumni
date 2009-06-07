#include "s4_be.h"
#include "be.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>



typedef struct header_St {
	int32_t string_store, int_store, int_rev;
	int32_t alloc_off;
} header_t;



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
static void grow_db (s4be_t *s4, int n)
{
	n = align (n, pagesize());

	munmap(s4->map, s4->size);

	s4->size += n;
	ftruncate(s4->fd, s4->size);

	s4->map = mmap (NULL, s4->size, PROT_READ | PROT_WRITE, MAP_SHARED, s4->fd, 0);
}


/* Initilize a new database */
static void init_db (s4be_t *s4)
{
	header_t *header;

	grow_db (s4, 1);

	header = s4->map;
	header->string_store = -1;
	header->int_store = -1;
	header->int_rev = -1;
	header->alloc_off = sizeof(header_t);
}


/**
 * Open an s4 database
 *
 * @param filename The file to open
 * @return A pointer to an s4 structure, or NULL on error
 */
s4be_t *s4be_open (const char* filename)
{
	struct stat stat_buf;
	s4be_t* s4 = malloc (sizeof(s4be_t));

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
int s4be_close (s4be_t* s4)
{
	munmap (s4->map, s4->size);
	close (s4->fd);
	free (s4);

	return 0;
}


/**
 * Allocate atleast n bytes in the database and return the offset
 *
 * @param n The number of bytes needed
 * @return The offset into the database
 */
int32_t be_alloc (s4be_t* s4, int n)
{
	header_t* header = s4->map;
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
void be_free(s4be_t* s4, int32_t off)
{
	/* DUMMY, write some real code here */
}
