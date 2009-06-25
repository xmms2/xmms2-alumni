#include "s4_be.h"
#include "be.h"
#include "pat.h"
#include "bpt.h"
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>


#define CLEAN 0
#define DIRTY 1

/* Define the biggest and smallest chunk size (exponents) */
#define BIGGEST_CHUNK 12 /* 2^12 = 4096 */
#define SMALLEST_CHUNK 4 /* 2^4  = 16 */

typedef struct header_St {
	pat_trie_t string_store;
	bpt_t int_store, int_rev;
	int32_t sync_state;

	int32_t free_lists[BIGGEST_CHUNK - SMALLEST_CHUNK + 1];
	int free;
} header_t;

typedef struct chunk_St {
	int32_t next;
} chunk_t;


pthread_t s_thread;

static inline long pagesize ()
{
	return sysconf (_SC_PAGESIZE);
}


static int log2 (unsigned int x)
{
	int ret = 31;

	if (!x) return 0;

	while (!(x & (1 << 31))) {
		x <<= 1;
		ret--;
	}

	if (x > (1 << 31)) ret++;

	return ret;
}


/* Sync the database if it is dirty */
void sync_db (s4be_t *s4)
{
	header_t *header;

	/* We only need a read lock, we're not modifying any data, just
	 * flushing it to disk, so we can have other threads reading.
	 */
	be_rlock (s4);
	header = s4->map;

	if (header->sync_state != CLEAN) {
		msync (s4->map, s4->size, MS_SYNC);
		header->sync_state = CLEAN;
		msync (s4->map, pagesize(), MS_SYNC);
	}
	be_unlock (s4);
}


/* Grow the database by the number of bytes given.
 * It will align to a pagesize boundry.
 */
static void grow_db (s4be_t *s4, int n)
{
	n = S4_ALIGN (n, pagesize());

	munmap(s4->map, s4->size);

	s4->size += n;
	ftruncate(s4->fd, s4->size);

	s4->map = mmap (NULL, s4->size, PROT_READ | PROT_WRITE, MAP_SHARED, s4->fd, 0);
}


/* Initilize a new database */
static void init_db (s4be_t *s4)
{
	header_t *header;

	s4->size = pagesize();
	ftruncate (s4->fd, s4->size);
	s4->map = mmap (NULL, s4->size, PROT_READ | PROT_WRITE, MAP_SHARED, s4->fd, 0);

	header = s4->map;

	memset (header, -1, sizeof (header_t));
	header->sync_state = DIRTY;
	header->free = 0;
	sync_db(s4);
}


static int32_t make_chunks (s4be_t *be, int exp)
{
	int size = 2 << (exp + SMALLEST_CHUNK - 1);
	int32_t ret = be->size;
	int i;
	chunk_t *chunk;

	grow_db (be, 1);

	for (i = 0; i < pagesize(); i += size) {
		chunk = S4_PNT (be, ret + i, chunk_t);
		chunk->next = ret + i + size;
	}

	chunk->next = -1;

	return ret;
}


/* Marks the database as dirty, it should be
 * marked as dirty BEFORE anything is written
 */
void mark_dirty (s4be_t *s4)
{
	header_t *header = s4->map;

	/* This should be fairly cheap, a 4 byte write */
	if (header->sync_state != DIRTY) {
		header->sync_state = DIRTY;
		msync (s4->map, pagesize(), MS_SYNC);
	}
}


/* The loop for the sync thread */
void *sync_thread (void *be)
{
	s4be_t *s4 = be;
	while (1) {
		sleep (60);
		sync_db (s4);
	}
}


s4be_t *be_open (const char *filename, int recover)
{
	struct stat stat_buf;
	s4be_t* s4 = malloc (sizeof(s4be_t));
	int flags = O_RDWR | O_CREAT;

	if (recover) {
		flags |= O_EXCL;
	}

	s4->fd = open (filename, flags, 0644);
	if (s4->fd == -1) {
		free (s4);
		fprintf (stderr, "Could not open %s: %s\n", filename, strerror (errno));
		return NULL;
	}

	pthread_rwlock_init (&s4->rwlock, NULL);

	fstat(s4->fd, &stat_buf);
	s4->size = stat_buf.st_size;
	if (s4->size == 0) {
		init_db(s4);
	}
	else {
		s4->map = mmap (NULL, s4->size,
				PROT_READ | PROT_WRITE,	MAP_SHARED, s4->fd, 0);
		if (s4->map == MAP_FAILED) {
			fprintf (stderr, "Could not mmap %s: %s\n",
					filename, strerror (errno));
			return NULL;
		}
	}

	return s4;
}


/**
 * Open an s4 database
 *
 * @param filename The file to open
 * @return A pointer to an s4 structure, or NULL on error
 */
s4be_t *s4be_open (const char* filename)
{
	s4be_t *ret, *rec;
	header_t *header;

	ret = be_open (filename, 0);

	if (ret == NULL)
		return NULL;

	header = ret->map;

	if (header->sync_state != CLEAN) {
		char buf[4096];
		strcpy (buf, filename);
		strcat (buf, ".rec");

		rec = be_open (buf, 1);

		if (rec == NULL)
			return NULL;

		_st_recover (ret, rec);
		_ip_recover (ret, rec);
		
		s4be_close (ret);
		s4be_close (rec);

		unlink (filename);
		rename (buf, filename);

		ret = be_open (filename, 0);
	}

	pthread_create (&s_thread, NULL, sync_thread, ret);
	return ret;
}


/**
 * Close an open s4 database
 *
 * @param s4 The database to close
 * @return 0 on success, anything else on error
 */
int s4be_close (s4be_t* s4)
{
	header_t *header = s4->map;

	printf ("free %i\n", header->free);

	sync_db (s4);
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
	chunk_t *chunk;
	int32_t ret;
	int l = log2 (n) - SMALLEST_CHUNK;

	if (l < 0)
		l = 0;

	if (l >= BIGGEST_CHUNK) {
		printf ("trying to allocate a bigger chunk than the biggest we allow!\n");
		return -1;
	}

	if (header->free_lists[l] == -1) {
		ret = make_chunks (s4, l);
		header = s4->map;
		header->free_lists[l] = ret;
		header->free += pagesize();
	}

	ret = header->free_lists[l];
	chunk = S4_PNT (s4, ret, chunk_t);
	header->free_lists[l] = chunk->next;

	header->free -= 2 << (l + SMALLEST_CHUNK - 1);

	return ret;
}


/**
 * Free the allocation at offset off
 *
 * @param off The allocation to free
 */
void be_free(s4be_t* s4, int32_t off, int size)
{
	int32_t l = log2 (size) - SMALLEST_CHUNK;
	header_t *header = s4->map;
	chunk_t *chunk = S4_PNT (s4, off, chunk_t);

	chunk->next = header->free_lists[l];
	header->free_lists[l] = off;
	header->free += 2 << (l + SMALLEST_CHUNK - 1);
}

/* Locking routines
 * The database is protected by a multiple readers,
 * single writer lock. Use these functions to lock/unlock
 * the database.
 */
void be_rlock (s4be_t *s4)
{
	pthread_rwlock_rdlock (&s4->rwlock);
}

void be_unlock (s4be_t *s4)
{
	pthread_rwlock_unlock (&s4->rwlock);
}

void be_wlock (s4be_t *s4)
{
	pthread_rwlock_wrlock (&s4->rwlock);
	mark_dirty (s4);
}
