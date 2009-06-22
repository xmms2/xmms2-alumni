#include "s4_be.h"
#include "be.h"
#include "bpt.h"
#include <stdlib.h>


/**
 * Add the binding entry->prop to the database
 *
 * @param be The database handle
 * @param entry The entry to get a new property
 * @param prop The property to be set
 * @return 0 on success, -1 otherwise
 */
int s4be_ip_add (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop)
{
	int ret;
	bpt_record_t a, b;

	b.key_b = a.key_a = entry->key_i;
	b.val_b = a.val_a = entry->val_i;
	a.key_b = b.key_a = prop->key_i;
	a.val_b = b.val_a = prop->val_i;

	be_wlock (be);

	ret = bpt_insert (be, S4_INT_STORE, a);
	if (!ret)
		ret = bpt_insert (be, S4_REV_STORE, b);

	be_unlock (be);

	return ret;
}


/**
 * Remove the binding entry->prop
 *
 * @param be The database handle
 * @param entry The entry to remove
 * @param prop The property to remove
 * @return 0 on success, -1 otherwise
 */
int s4be_ip_del (s4be_t *be, s4_entry_t *entry, s4_entry_t *prop)
{
	int ret;
	bpt_record_t a, b;

	b.key_b = a.key_a = entry->key_i;
	b.val_b = a.val_a = entry->val_i;
	a.key_b = b.key_a = prop->key_i;
	a.val_b = b.val_a = prop->val_i;

	be_wlock (be);

	ret = bpt_remove (be, S4_INT_STORE, a);
	if (!ret)
		ret = bpt_remove (be, S4_REV_STORE, b);

	be_unlock (be);

	return ret;
}


/**
 * Get all the entries that has the property entry
 *
 * @param be The database handle
 * @param entry The property
 * @return A set with all the entries
 */
s4_set_t *s4be_ip_has_this (s4be_t *be, s4_entry_t *entry)
{
	s4_set_t *ret;
	be_rlock (be);
	ret = bpt_get_set (be, S4_REV_STORE, entry->key_i, entry->val_i);
	be_unlock (be);

	return ret;
}


/**
 * Get all the properties that this entry has
 *
 * @param be The database handle
 * @param entry The entry
 * @return A set with all the properties
 */
s4_set_t *s4be_ip_this_has (s4be_t *be, s4_entry_t *entry)
{
	s4_set_t *ret;
	be_rlock (be);
	ret = bpt_get_set (be, S4_INT_STORE, entry->key_i, entry->val_i);
	be_unlock (be);

	return ret;
}


/* Try to recover the database */
int _ip_recover (s4be_t *old, s4be_t *rec)
{

	return 0;
}
