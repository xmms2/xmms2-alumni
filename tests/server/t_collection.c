#include <locale.h>

#include "xcu.h"

#include "xmmspriv/xmms_log.h"
#include "xmmspriv/xmms_ipc.h"
#include "xmmspriv/xmms_config.h"
#include "xmmspriv/xmms_medialib.h"
#include "xmmspriv/xmms_collection.h"

#include "utils/jsonism.h"
#include "utils/value_utils.h"
#include "utils/ipc_call.h"
#include "utils/mlib_utils.h"

static xmms_medialib_t *medialib;
static xmms_coll_dag_t *dag;

SETUP (coll) {
	g_thread_init (0);

	setlocale (LC_COLLATE, "");

	xmms_ipc_init ();

	xmms_log_init (0);

	xmms_config_init ("memory://");
	xmms_config_property_register ("medialib.path", "memory://", NULL, NULL);

	medialib = xmms_medialib_init ();
	dag = xmms_collection_init (medialib);

	return 0;
}

CLEANUP () {
	xmms_object_unref (medialib);
	xmms_object_unref (dag);

	xmms_config_shutdown ();

	xmms_ipc_shutdown ();

	return 0;
}


CASE (test_client_save) {
	xmms_future_t *future;
	xmmsv_coll_t *universe;
	xmmsv_t *result, *signals, *expected;

	future = XMMS_IPC_CHECK_SIGNAL (dag, XMMS_IPC_SIGNAL_COLLECTION_CHANGED);

	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);

	/* emits XMMS_COLLECTION_CHANGED_ADD */
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	xmmsv_coll_unref (universe);

	/* each save requires a new collection, normally handled by IPC deserialization */
	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);

	/* replace the collection, emits XMMS_COLLECTION_CHANGED_UPDATE */
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	xmmsv_coll_unref (universe);

	signals = xmms_future_await (future, 2);

	/* XMMS_COLLECTION_CHANGED_ADD = 0, XMMS_COLLECTION_CHANGED_UPDATE = 1 */
	expected = xmmsv_from_xson ("[{ 'type': 'dict', 'inner': { 'type': 0, 'namespace': 'Collections', 'name': 'Test' } },"
	                            " { 'type': 'dict', 'inner': { 'type': 1, 'namespace': 'Collections', 'name': 'Test' } }]");
	CU_ASSERT (xmmsv_compare (expected, signals));
	xmmsv_unref (signals);
	xmmsv_unref (expected);
}

CASE (test_client_get) {
	xmmsv_coll_t *universe;
	xmmsv_t *result;

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("whatever"),
	                        xmmsv_new_string ("invalid namespace"));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("missing collection"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));
	xmmsv_unref (result);

	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_COLL));
	xmmsv_unref (result);
}

CASE (test_client_remove)
{
	xmms_future_t *future;
	xmmsv_coll_t *universe;
	xmmsv_t *result, *signals, *expected;

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_REMOVE,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));
	xmmsv_unref (result);

	future = XMMS_IPC_CHECK_SIGNAL (dag, XMMS_IPC_SIGNAL_COLLECTION_CHANGED);

	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_coll_unref (universe);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_REMOVE,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	signals = xmms_future_await (future, 2);

	/* XMMS_COLLECTION_CHANGED_ADD = 0, XMMS_COLLECTION_CHANGED_REMOVE = 3 */
	expected = xmmsv_from_xson ("[{ 'type': 'dict', 'inner': { 'type': 0, 'namespace': 'Collections', 'name': 'Test' } },"
	                            " { 'type': 'dict', 'inner': { 'type': 3, 'namespace': 'Collections', 'name': 'Test' } }]");
	CU_ASSERT (xmmsv_compare (expected, signals));
	xmmsv_unref (signals);
	xmmsv_unref (expected);
	xmms_future_free (future);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("Test"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));
	xmmsv_unref (result);
}

CASE (test_client_rename)
{
	xmmsv_coll_t *universe;
	xmmsv_t *result;

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_RENAME,
	                        xmmsv_new_string ("Test A"),
	                        xmmsv_new_string ("Test B"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));
	xmmsv_unref (result);

	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test A"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_RENAME,
	                        xmmsv_new_string ("Test A"),
	                        xmmsv_new_string ("Test B"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("Test A"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("Test B"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_COLL));
	xmmsv_unref (result);
}

CASE (test_client_rename_referenced)
{
	xmmsv_t *result, *universe, *reference;
	const char *name;

	universe = xmmsv_new_coll (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Before Rename"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_unref (universe);

	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "Before Rename");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test Reference"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_unref (reference);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_RENAME,
	                        xmmsv_new_string ("Before Rename"),
	                        xmmsv_new_string ("After Rename"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_GET,
	                        xmmsv_new_string ("Test Reference"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_COLL));
	CU_ASSERT (xmmsv_coll_attribute_get_string (result, "reference", &name));
	CU_ASSERT_STRING_EQUAL ("After Rename", name);

	xmmsv_unref (result);
}

CASE (test_client_find)
{
	xmms_medialib_entry_t entry;
	xmmsv_coll_t *universe, *equals;
	xmmsv_t *result;
	const gchar *string;

	entry = xmms_mock_entry (medialib, 1, "Red Fang", "Red Fang", "Prehistoric Dog");

	/* should be found */
	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test A"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_coll_unref (universe);

	/* should be found */
	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test B"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_coll_unref (universe);

	/* should not be found */
	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	equals = xmmsv_coll_new (XMMS_COLLECTION_TYPE_EQUALS);
	xmmsv_coll_attribute_set_string (equals, "type", "id");
	xmmsv_coll_attribute_set_string (equals, "value", "1337");
	xmmsv_coll_add_operand (equals, universe);
	xmmsv_coll_unref (universe);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test C"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (equals));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_coll_unref (equals);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_FIND,
	                        xmmsv_new_int (entry),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_LIST));
	CU_ASSERT_EQUAL (2, xmmsv_list_get_size (result));
	CU_ASSERT (xmmsv_list_get_string (result, 0, &string));
	CU_ASSERT (strcmp ("Test A", string) == 0 || strcmp ("Test B", string) == 0);
	CU_ASSERT (xmmsv_list_get_string (result, 1, &string));
	CU_ASSERT (strcmp ("Test A", string) == 0 || strcmp ("Test B", string) == 0);
	xmmsv_unref (result);
}

CASE (test_client_list)
{
	xmmsv_coll_t *universe, *idlist;
	xmmsv_t *result;

	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test A"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (universe));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_coll_unref (universe);

	idlist = xmmsv_coll_new (XMMS_COLLECTION_TYPE_IDLIST);
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test B"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_PLAYLISTS),
	                        xmmsv_ref (idlist));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);
	xmmsv_coll_unref (idlist);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_LIST,
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_LIST));
	CU_ASSERT_EQUAL (1, xmmsv_list_get_size (result));
	xmmsv_unref (result);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_LIST,
	                        xmmsv_new_string (XMMS_COLLECTION_NS_PLAYLISTS));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_LIST));
	CU_ASSERT_EQUAL (1, xmmsv_list_get_size (result));
	xmmsv_unref (result);
}

CASE (test_client_query_infos)
{
	xmms_medialib_entry_t first, second;
	xmms_medialib_session_t *session;
	xmmsv_coll_t *universe, *ordered;
	xmmsv_t *expected, *result, *order, *fetch, *group;

	first = xmms_mock_entry (medialib, 1, "Red Fang", "Red Fang", "Prehistoric Dog");
	second = xmms_mock_entry (medialib, 2, "Red Fang", "Red Fang", "Reverse Thunder");

	session = xmms_medialib_session_begin (medialib);
	xmms_medialib_entry_property_set_str_source (session, second,
	                                             XMMS_MEDIALIB_ENTRY_PROPERTY_YEAR,
	                                             "2009", "client/unittest");
	xmms_medialib_session_commit (session);


	universe = xmmsv_coll_new (XMMS_COLLECTION_TYPE_UNIVERSE);

	order = xmmsv_build_list (XMMSV_LIST_ENTRY_STR ("artist"),
	                          XMMSV_LIST_ENTRY_STR ("album"),
	                          XMMSV_LIST_ENTRY_STR ("tracknr"),
	                          XMMSV_LIST_END);

	ordered = xmmsv_coll_add_order_operators (universe, order);
	xmmsv_coll_unref (universe);
	xmmsv_unref (order);

	fetch = xmmsv_build_list (XMMSV_LIST_ENTRY_STR ("artist"),
	                          XMMSV_LIST_ENTRY_STR ("album"),
	                          XMMSV_LIST_ENTRY_STR ("title"),
	                          XMMSV_LIST_ENTRY_STR ("tracknr"),
	                          XMMSV_LIST_ENTRY_STR ("date"),
	                          XMMSV_LIST_END);

	group = xmmsv_build_list (XMMSV_LIST_ENTRY_STR ("position"),
	                          XMMSV_LIST_END);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_QUERY_INFOS,
	                        xmmsv_ref (ordered),
	                        fetch, group);
	xmmsv_coll_unref (ordered);

	expected = xmmsv_from_xson ("[                              "
	                            " { 'type': 'dict', 'inner': {  "
	                            "  'artist': 'Red Fang',        "
	                            "   'album': 'Red Fang',        "
	                            " 'tracknr':  1,                "
	                            "   'title': 'Prehistoric Dog'  "
	                            " } },                          "
	                            " { 'type': 'dict', 'inner': {  "
	                            "   'album': 'Red Fang',        "
	                            " 'tracknr':  2,                "
	                            "   'title': 'Reverse Thunder', "
	                            "  'artist': 'Red Fang',        "
	                            "    'date': '2009'             "
	                            " } }                           "
	                            "]                              ");

	CU_ASSERT (xmmsv_compare (expected, result));

	xmmsv_unref (expected);
	xmmsv_unref (result);
}

CASE (test_reject_direct_cyclic_collections)
{
	xmmsv_t *universe, *reference, *match;
	xmmsv_t *result;

	/* To create a cycle the collection must already exist, check that it fails */
	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "Cycle");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Cycle"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));

	xmmsv_unref (result);
	xmmsv_unref (reference);

	/* Create the non cyclic version and then later update it to be cyclic */
	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "All Media");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Cycle"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));

	xmmsv_unref (result);
	xmmsv_unref (reference);

	/* Overwrite the existing collection with a circular reference */
	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "Cycle");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Cycle"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));

	xmmsv_unref (result);
	xmmsv_unref (reference);
}

CASE (test_reject_indirect_cyclic_collections)
{
	xmmsv_t *universe, *reference, *match, *intersection;
	xmmsv_t *result;

	/* Create a correct collection pointing to all media */
	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "All Media");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("First"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));

	xmmsv_unref (result);
	xmmsv_unref (reference);

	/* Create a correct collection intersecting the first collection with all media */
	intersection = xmmsv_new_coll (XMMS_COLLECTION_TYPE_INTERSECTION);

	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "First");
	xmmsv_coll_add_operand (intersection, reference);
	xmmsv_unref (reference);

	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "All Media");
	xmmsv_coll_add_operand (intersection, reference);
	xmmsv_unref (reference);

	/* Overwrite the existing collection with a circular reference */
	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Second"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (intersection));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));

	xmmsv_unref (result);
	xmmsv_unref (intersection);

	/* Update the first collection to point to the second collection thus creating
	 * an indirect cyclic graph.. which should be rejected */
	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "Second");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("First"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_ERROR));

	xmmsv_unref (result);
	xmmsv_unref (reference);
}

CASE (test_references)
{
	xmmsv_t *universe, *reference, *match;
	xmmsv_t *result;

	reference = xmmsv_new_coll (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "All Media");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Cycle"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));

	xmmsv_unref (result);

	universe = xmmsv_new_coll (XMMS_COLLECTION_TYPE_UNIVERSE);

	match = xmmsv_new_coll (XMMS_COLLECTION_TYPE_MATCH);
	xmmsv_coll_attribute_set_string (match, "field", "artist");
	xmmsv_coll_attribute_set_string (match, "value", "The Doors");
	xmmsv_coll_add_operand (match, universe);

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("The Doors"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (match));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));

	xmmsv_unref (result);

	reference = xmmsv_coll_new (XMMS_COLLECTION_TYPE_REFERENCE);
	xmmsv_coll_attribute_set_string (reference, "namespace", XMMS_COLLECTION_NS_COLLECTIONS);
	xmmsv_coll_attribute_set_string (reference, "reference", "The Doors");

	result = XMMS_IPC_CALL (dag, XMMS_IPC_CMD_COLLECTION_SAVE,
	                        xmmsv_new_string ("Test Reference"),
	                        xmmsv_new_string (XMMS_COLLECTION_NS_COLLECTIONS),
	                        xmmsv_ref (reference));
	CU_ASSERT (xmmsv_is_type (result, XMMSV_TYPE_NONE));
	xmmsv_unref (result);

	xmmsv_unref (reference);
	xmmsv_unref (universe);
	xmmsv_unref (match);
}
