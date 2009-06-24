#include "s4.h"
#include "s4_be.h"
#include <stdlib.h>
#include <string.h>


/**
 * Create a new s4 entry and set the strings to key and val
 *
 * @param s4 The database handle
 * @param key The key
 * @param val The value
 * @return A new entry
 */
s4_entry_t *s4_entry_get_s (s4_t *s4, const char *key, const char *val)
{
	s4_entry_t *ret = malloc (sizeof (s4_entry_t));

	ret->key_s = strdup (key);
	ret->val_s = strdup (val);
	ret->key_i = ret->val_i = 0;
	ret->type = ENTRY_STR;

	return ret;
}


/**
 * Create a new int entry.
 *
 * @param s4 The database handle
 * @param key The key
 * @param val The value
 * @return A new entry
 */
s4_entry_t *s4_entry_get_i (s4_t *s4, const char *key, int val)
{
	s4_entry_t *ret = malloc (sizeof (s4_entry_t));

	ret->key_s = strdup (key);
	ret->val_s = NULL;
	ret->key_i = 0;
	ret->val_i = val;
	ret->type = ENTRY_INT;

	return ret;
}


/**
 * Free the strings the entry has
 *
 * @param entry The entry
 */
void s4_entry_free_strings (s4_entry_t *entry)
{
	if (entry->key_s != NULL)
		free (entry->key_s);
	if (entry->val_s != NULL)
		free (entry->val_s);
}


/**
 * Free an entry
 *
 * @param entry The entry to free
 */
void s4_entry_free (s4_entry_t *entry)
{
	s4_entry_free_strings (entry);

	free (entry);
}


static void _entry_unref (s4_t *s4, s4_entry_t *entry)
{
	if (s4be_st_unref (s4->be, entry->key_s) == 0)
		entry->key_i = 0;
	if (entry->type == ENTRY_STR && s4be_st_unref (s4->be, entry->val_s) == 0)
		entry->val_i = 0;
}


/* Lookup the strings in entry and fill in the int fields.
 * If ref != 0 and the strings doesn't exist yet they're made
 * The return value has bit 1 set if they key was refed and
 * bit 2 set if the val was refed.
 * All other bits are 0.
 */
int _entry_lookup (s4_t *s4, s4_entry_t *entry, int ref)
{
	int ret = 0;
	if (entry->key_i == 0) {
		entry->key_i = s4be_st_lookup (s4->be, entry->key_s);
		if (entry->key_i == 0 && ref) {
			ret = 1;
			entry->key_i = s4be_st_ref (s4->be, entry->key_s);
		} else if (!ref)
			return -1;

		if (entry->type == ENTRY_INT)
			entry->key_i = -entry->key_i;
	}
	if (entry->type == ENTRY_STR && entry->val_i == 0) {
		entry->val_i = s4be_st_lookup (s4->be, entry->val_s);
		if (entry->val_i == 0 && ref) {
			ret += 2;
			entry->val_i = s4be_st_ref (s4->be, entry->val_s);
		} else if (!ref)
			return -1;
	}

	return ret;
}


/**
 * Fill in empty fields. If the strings are missing it looks
 * them up, if the ints are missing it looks those up.
 *
 * @param s4 The database handle
 * @param entry The entry
 */
void s4_entry_fillin (s4_t *s4, s4_entry_t *entry)
{
	if (entry->type == ENTRY_STR) {
		if (entry->key_i == 0)
			entry->key_i = s4be_st_lookup (s4->be, entry->key_s);
		else if (entry->key_s == NULL)
			entry->key_s = s4be_st_reverse (s4->be, entry->key_i);
		if (entry->val_i == 0)
			entry->val_i = s4be_st_lookup (s4->be, entry->val_s);
		else if (entry->val_s == NULL)
			entry->val_s = s4be_st_reverse (s4->be, entry->val_i);

	} else if (entry->key_i == 0) {
		entry->key_i = -s4be_st_lookup (s4->be, entry->key_s);
	} else if (entry->key_s == NULL) {
		entry->key_s = s4be_st_reverse (s4->be, -entry->key_i);
	}
}


/**
 * Add a property to the entry
 *
 * @param s4 The database handle
 * @param entry The entry
 * @param prop The property
 * @return 0 on success, -1 otherwise
 */
int s4_entry_add (s4_t *s4, s4_entry_t *entry, s4_entry_t *prop)
{
	int ref;
	int ret;
	ref = _entry_lookup (s4, entry, 1);
	ref += _entry_lookup (s4, prop, 1) * 4;

	/* If the insertion went well we need to ref the strings */
	if (!(ret = s4be_ip_add (s4->be, entry, prop))) {
		if (!(ref & 1))
			s4be_st_ref (s4->be, entry->key_s);
		if (!(ref & 2) && entry->type == ENTRY_STR)
			s4be_st_ref (s4->be, entry->val_s);
		if (!(ref & 4))
			s4be_st_ref (s4->be, prop->key_s);
		if (!(ref & 8) && prop->type == ENTRY_STR)
			s4be_st_ref (s4->be, prop->val_s);
	}

	return ret;
}


/**
 * Delete a property from an entry
 *
 * @param s4 The database handle
 * @param entry The entry to remove from
 * @param prop The property to remove
 * @return 0 on success, -1 otherwise
 */
int s4_entry_del (s4_t *s4, s4_entry_t *entry, s4_entry_t *prop)
{
	int ret;
	if (_entry_lookup (s4, entry, 0) ||	_entry_lookup (s4, entry, 0))
		return -1;

	/* Unref the strings if we found them */
	if (!(ret = s4be_ip_del (s4->be, entry, prop))) {
		_entry_unref (s4, entry);
		_entry_unref (s4, prop);
	}

	return ret;
}


/**
 * Get all entries that contains entry
 *
 * @param s4 The database handle
 * @param entry The entry
 * @return A set with all the entries that contains entry
 */
s4_set_t *s4_entry_contains (s4_t *s4, s4_entry_t *entry)
{
	s4_entry_fillin (s4, entry);
	return s4be_ip_this_has (s4->be, entry);
}


/**
 * Get all entries that is contained in entry
 *
 * @param s4 The database handle
 * @param entry The entry
 * @return A set with all the entries contained in entry
 */
s4_set_t *s4_entry_contained(s4_t *s4, s4_entry_t *entry)
{
	s4_entry_fillin (s4, entry);
	return s4be_ip_has_this (s4->be, entry);
}


/**
 * Get all entries smaller than this one
 *
 * @param s4 The database handle
 * @param entry The entry
 * @return A set will all the entries that is contained in an entry
 * that has a smaller value than this one (but same key).
 */
s4_set_t *s4_entry_smaller (s4_t *s4, s4_entry_t *entry)
{
	s4_entry_fillin (s4, entry);
	return s4be_ip_smaller (s4->be, entry);
}


/**
 * Get all entries greater than this one
 *
 * @param s4 The database handle
 * @param entry The entry
 * @return A set will all the entries that is contained in an entry
 * that has a greater value than this one (but same key).
 */
s4_set_t *s4_entry_greater (s4_t *s4, s4_entry_t *entry)
{
	s4_entry_fillin (s4, entry);
	return s4be_ip_greater (s4->be, entry);
}
