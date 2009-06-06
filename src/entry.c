#include "s4.h"
#include "s4_be.h"
#include <stdlib.h>
#include <string.h>


s4_entry_t *s4_entry_get_s (s4_t *s4, const char *key, const char *val)
{
	s4_entry_t *ret = malloc (sizeof (s4_entry_t));

	ret->key_s = strdup (key);
	ret->val_s = strdup (val);
	ret->key_i = ret->val_i = 0;
	ret->type = ENTRY_STR;

	return ret;
}


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

void s4_entry_free_strings (s4_entry_t *entry)
{
	if (entry->key_s != NULL)
		free (entry->key_s);
	if (entry->val_s != NULL)
		free (entry->val_s);
}

void s4_entry_free (s4_entry_t *entry)
{
	s4_entry_free_strings (entry);

	free (entry);
}

static void _entry_ref (s4_t *s4, s4_entry_t *entry)
{
	entry->key_i = -s4be_st_ref (s4->be, entry->key_s);

	if (entry->type == ENTRY_STR) {
		entry->key_i = -entry->key_i;
		entry->val_i = s4be_st_ref (s4->be, entry->val_s);
	}
}

static void _entry_unref (s4_t *s4, s4_entry_t *entry)
{
	if (s4be_st_unref (s4->be, entry->key_s) == 0)
		entry->key_i = 0;
	if (entry->type == ENTRY_STR && s4be_st_unref (s4->be, entry->val_s) == 0)
		entry->val_i = 0;
}

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

int s4_entry_add (s4_t *s4, s4_entry_t *entry, s4_entry_t *prop)
{
	_entry_ref (s4, entry);
	_entry_ref (s4, prop);

	return s4be_ip_add (s4->be, entry, prop);
}

int s4_entry_del (s4_t *s4, s4_entry_t *entry, s4_entry_t *prop)
{
	_entry_unref (s4, entry);
	_entry_unref (s4, prop);

	return s4be_ip_del (s4->be, entry, prop);
}


s4_set_t *s4_entry_contains (s4_t *s4, s4_entry_t *entry)
{
	s4_entry_fillin (s4, entry);
	return s4be_ip_this_has (s4->be, entry);
}


s4_set_t *s4_entry_contained(s4_t *s4, s4_entry_t *entry)
{
	s4_entry_fillin (s4, entry);
	return s4be_ip_has_this (s4->be, entry);
}
