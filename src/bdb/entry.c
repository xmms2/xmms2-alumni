#include "s4.h"
#include "intpair.h"
#include "strtable.h"



s4_entry_t s4_entry_get_s (s4_t *s4, const char *key, const char *val)
{
	s4_entry_t ret;
	ret.pair.key = strtab_lookup (s4, key);
	if (ret.pair.key == -1)
		ret.pair.key = strtab_ref (s4, key);
	ret.pair.val = strtab_lookup (s4, val);
	if (ret.pair.val == -1)
		ret.pair.val = strtab_ref (s4, val);

	return ret;
}


s4_entry_t s4_entry_get_i (s4_t *s4, const char *key, int val)
{
	s4_entry_t ret;
	ret.pair.key = strtab_lookup (s4, key);
	if (ret.pair.key == -1)
		ret.pair.key = -strtab_ref (s4, key);
	else
		ret.pair.key = -ret.pair.key;
	ret.pair.val = val;

	return ret;
}


int s4_entry_add_i (s4_t *s4, s4_entry_t entry, const char *key, int val)
{
	intpair_t pair;
	pair.key = strtab_lookup (s4, key);
	if (pair.key == -1)
		pair.key = -strtab_ref (s4, key);
	else
		pair.key = -pair.key;
	pair.val = val;

	return intpair_add_property (s4, entry.pair, pair);
}


int s4_entry_add_s (s4_t *s4, s4_entry_t entry, const char *key, const char *val)
{
	intpair_t pair;
	pair.key = strtab_lookup (s4, key);
	if (pair.key == -1)
		pair.key = strtab_ref (s4, key);
	pair.val = strtab_lookup (s4, val);
	if (pair.val == -1)
		pair.val = strtab_ref (s4, val);

	return intpair_add_property (s4, entry.pair, pair);
}


s4_set_t *s4_entry_contains (s4_t *s4, s4_entry_t entry)
{
	return intpair_this_has (s4, entry.pair);
}


s4_set_t *s4_entry_contained(s4_t *s4, s4_entry_t entry)
{
	return intpair_has_this (s4, entry.pair);
}
