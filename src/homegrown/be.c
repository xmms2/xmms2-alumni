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

typedef struct header_St {
	pat_trie_t string_store;
	bpt_t int_store, int_rev;
	int32_t alloc_off;
	int32_t sync_state;
} header_t;


pthread_t s_thread;

static inline long pagesize ()
{
	return sysconf (_SC_PAGESIZE);
}

static inline int align (int a, int b)
{
	return a + ((b - (a % b)) % b);
}

/* Sync the database if it is dirty */
void sync_db (s4be_t *s4)
{
	header_t *header;

	/* We only need a read lock, we're not modifying any data
	 * just flushing it to disk, so we can other threads reading
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

	s4->size = pagesize();
	ftruncate (s4->fd, s4->size);
	s4->map = mmap (NULL, s4->size, PROT_READ | PROT_WRITE, MAP_SHARED, s4->fd, 0);

	header = s4->map;

	memset (header, -1, sizeof (header_t));
	header->alloc_off = sizeof(header_t);
	header->sync_state = DIRTY;
	sync_db(s4);
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
