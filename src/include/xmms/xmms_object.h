/*  XMMS2 - X Music Multiplexer System
 *  Copyright (C) 2003-2008 XMMS2 Team
 *
 *  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 */




#ifndef __XMMS_OBJECT_H__
#define __XMMS_OBJECT_H__

#include <glib.h>
#include "xmms/xmms_error.h"
#include "xmms/xmms_service.h"
#include "xmms/xmms_ipc_pending.h"
#include "xmmsc/xmmsc_idnumbers.h"
#include "xmmsc/xmmsc_value.h"
#include "xmmsc/xmmsc_coll.h"

#define XMMS_OBJECT_MID 0x00455574

G_BEGIN_DECLS

struct xmms_object_St;
typedef struct xmms_object_St xmms_object_t;
typedef struct xmms_object_cmd_desc_St xmms_object_cmd_desc_t;

typedef void (*xmms_object_destroy_func_t) (xmms_object_t *object);

/* weird place, to circumvent recursive including hell */
#include "xmms/xmms_ipc.h"

/** @addtogroup Object
  * @{
  */
struct xmms_object_St {
	guint32 id;
	GMutex *mutex;

	GList *signals[XMMS_IPC_SIGNAL_END];
	xmms_object_cmd_desc_t *cmds[XMMS_IPC_CMD_END];

	gint ref;
	xmms_object_destroy_func_t destroy_func;
};


/* Convenience wrapper to create #xmmsv_t from GLib types. */
xmmsv_t *xmms_create_xmmsv_list (GList *list);
xmmsv_t *xmms_create_xmmsv_dict (GTree *dict);
xmmsv_t *xmms_create_xmmsv_bin (GString *gs);

int xmms_bin_to_gstring (xmmsv_t *value, GString **gs);
int dummy_identity (xmmsv_t *value, xmmsv_t **arg);
gboolean check_string_list (xmmsv_t *list);


/** @} */

typedef void (*xmms_object_handler_t) (xmms_object_t *object, gconstpointer data, gpointer userdata);

/* FIXME(xv): made values pointers, init+free them! */
/* FIXME(xv): error now in retval? */
#define XMMS_OBJECT_CMD_MAX_ARGS 6
typedef struct {
	xmmsv_t *values[XMMS_OBJECT_CMD_MAX_ARGS];
	xmmsv_t *retval;
	xmms_ipc_client_t *client;
	xmms_ipc_pending_id_t pid;
	xmms_error_t error;
} xmms_object_cmd_arg_t;

typedef void (*xmms_object_cmd_func_t) (xmms_object_t *object, xmms_object_cmd_arg_t *arg);

struct xmms_object_cmd_desc_St {
	xmms_object_cmd_func_t func;
	xmmsv_type_t retval;
	xmmsv_type_t args[XMMS_OBJECT_CMD_MAX_ARGS];
};

#define XMMS_OBJECT(p) ((xmms_object_t *)p)
#define XMMS_IS_OBJECT(p) (XMMS_OBJECT (p)->id == XMMS_OBJECT_MID)

void xmms_object_cleanup (xmms_object_t *object);

void xmms_object_parent_set (xmms_object_t *object, xmms_object_t *parent);

void xmms_object_connect (xmms_object_t *object, guint32 signalid,
			  xmms_object_handler_t handler, gpointer userdata);

void xmms_object_disconnect (xmms_object_t *object, guint32 signalid,
			     xmms_object_handler_t handler, gpointer userdata);

void xmms_object_emit (xmms_object_t *object, guint32 signalid,
		       gconstpointer data);

void xmms_object_emit_f (xmms_object_t *object, guint32 signalid,
			 xmmsv_type_t type, ...);

void xmms_object_cmd_arg_init (xmms_object_cmd_arg_t *arg);

void xmms_object_cmd_add (xmms_object_t *object, guint cmdid, xmms_object_cmd_desc_t *desc);

void xmms_object_cmd_call (xmms_object_t *object, guint cmdid, xmms_object_cmd_arg_t *arg);

/* Some minor macro-magic. XMMS_CMD_DEFINE and XMMS_CMD_FUNC
 * are the only ones to be used directly */

#define __XMMS_CMD_INIT_ARG_FULL(argn, argtypecode, extract_func) \
	argtypecode argval##argn; \
	g_return_if_fail (extract_func (arg->values[argn], &argval##argn));

#define __XMMS_CMD_INIT_ARG(argn, argtype, argtypecode) \
	__XMMS_CMD_INIT_ARG_FULL(argn, argtypecode, xmmsv_get_##argtype)

#define __XMMS_CMD_INIT_ARG_NONE(a)
#define __XMMS_CMD_INIT_ARG_STRING(a) __XMMS_CMD_INIT_ARG(a, string, const gchar *)
#define __XMMS_CMD_INIT_ARG_UINT32(a) __XMMS_CMD_INIT_ARG(a, uint, guint)
#define __XMMS_CMD_INIT_ARG_INT32(a)  __XMMS_CMD_INIT_ARG(a, int,  gint)
#define __XMMS_CMD_INIT_ARG_COLL(a)   __XMMS_CMD_INIT_ARG(a, collection, xmmsv_coll_t *)
#define __XMMS_CMD_INIT_ARG_BIN(a)    __XMMS_CMD_INIT_ARG_FULL(a, GString *, xmms_bin_to_gstring)
#define __XMMS_CMD_INIT_ARG_LIST(a)   __XMMS_CMD_INIT_ARG_FULL(a, xmmsv_t *, dummy_identity)
#define __XMMS_CMD_INIT_ARG_DICT(a)   __XMMS_CMD_INIT_ARG_FULL(a, xmmsv_t *, dummy_identity)
#define __XMMS_CMD_INIT_ARG_CLIENT(a) xmms_ipc_client_t *argval##a = arg->client;

#define __XMMS_CMD_PRINT_ARG_NONE(a)
#define __XMMS_CMD_PRINT_ARG_STRING(a) , argval##a
#define __XMMS_CMD_PRINT_ARG_UINT32(a) , argval##a
#define __XMMS_CMD_PRINT_ARG_INT32(a)  , argval##a
#define __XMMS_CMD_PRINT_ARG_COLL(a)   , argval##a
#define __XMMS_CMD_PRINT_ARG_BIN(a)    , argval##a
#define __XMMS_CMD_PRINT_ARG_LIST(a)   , argval##a
#define __XMMS_CMD_PRINT_ARG_DICT(a)   , argval##a
#define __XMMS_CMD_PRINT_ARG_CLIENT(a) , argval##a

#define __XMMS_CMD_DO_RETVAL_NONE() arg->retval = xmmsv_new_none();
#define __XMMS_CMD_DO_RETVAL_DICT() arg->retval = xmms_create_xmmsv_dict
#define __XMMS_CMD_DO_RETVAL_UINT32() arg->retval = xmmsv_new_uint
#define __XMMS_CMD_DO_RETVAL_INT32() arg->retval = xmmsv_new_int
#define __XMMS_CMD_DO_RETVAL_LIST() arg->retval = xmms_create_xmmsv_list
#define __XMMS_CMD_DO_RETVAL_STRING() arg->retval = xmmsv_new_string
#define __XMMS_CMD_DO_RETVAL_COLL() arg->retval = xmmsv_new_coll
#define __XMMS_CMD_DO_RETVAL_BIN() arg->retval = xmms_create_xmmsv_bin
/* PENDING: return a ipc_pending_id and prevent ipc from sending a message back */
#define __XMMS_CMD_DO_RETVAL_PENDING() arg->retval = xmmsv_new_none(); arg->pid =
/* Hack to return a dict xmmsv_t directly */
#define __XMMS_CMD_DO_RETVAL_DICTVALUE() arg->retval =

/* Slight hack: define a mapping to handle non-xmmsv_t types too */
#define __XMMS_CMD_TYPE_NONE    XMMSV_TYPE_NONE
#define __XMMS_CMD_TYPE_ERROR   XMMSV_TYPE_ERROR
#define __XMMS_CMD_TYPE_UINT32  XMMSV_TYPE_UINT32
#define __XMMS_CMD_TYPE_INT32   XMMSV_TYPE_INT32
#define __XMMS_CMD_TYPE_STRING  XMMSV_TYPE_STRING
#define __XMMS_CMD_TYPE_COLL    XMMSV_TYPE_COLL
#define __XMMS_CMD_TYPE_BIN     XMMSV_TYPE_BIN
#define __XMMS_CMD_TYPE_LIST    XMMSV_TYPE_LIST
#define __XMMS_CMD_TYPE_DICT    XMMSV_TYPE_DICT
#define __XMMS_CMD_TYPE_PENDING XMMSV_TYPE_UINT32
#define __XMMS_CMD_TYPE_DICTVALUE XMMSV_TYPE_DICT
#define __XMMS_CMD_TYPE_CLIENT  XMMSV_TYPE_NONE


#define XMMS_CMD_DEFINE6(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, argtype4, argtype5, argtype6) static void \
__int_xmms_cmd_##cmdid (xmms_object_t *object, xmms_object_cmd_arg_t *arg) \
{ \
g_return_if_fail (XMMS_IS_OBJECT (object)); \
__XMMS_CMD_INIT_ARG_##argtype1 (0) \
__XMMS_CMD_INIT_ARG_##argtype2 (1) \
__XMMS_CMD_INIT_ARG_##argtype3 (2) \
__XMMS_CMD_INIT_ARG_##argtype4 (3) \
__XMMS_CMD_INIT_ARG_##argtype5 (4) \
__XMMS_CMD_INIT_ARG_##argtype6 (5) \
__XMMS_CMD_DO_RETVAL_##_rettype() (realfunc ((argtype0)object __XMMS_CMD_PRINT_ARG_##argtype1(0) __XMMS_CMD_PRINT_ARG_##argtype2(1) __XMMS_CMD_PRINT_ARG_##argtype3(2) __XMMS_CMD_PRINT_ARG_##argtype4(3) __XMMS_CMD_PRINT_ARG_##argtype5(4) __XMMS_CMD_PRINT_ARG_##argtype6(5), &arg->error)); \
} \
xmms_object_cmd_desc_t __int_xmms_cmd_desc_##cmdid = { __int_xmms_cmd_##cmdid, __XMMS_CMD_TYPE_##_rettype, {__XMMS_CMD_TYPE_##argtype1, __XMMS_CMD_TYPE_##argtype2, __XMMS_CMD_TYPE_##argtype3, __XMMS_CMD_TYPE_##argtype4, __XMMS_CMD_TYPE_##argtype5, __XMMS_CMD_TYPE_##argtype6} }


#define XMMS_CMD_DEFINE(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2) XMMS_CMD_DEFINE6(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, NONE, NONE, NONE, NONE)
#define XMMS_CMD_DEFINE3(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3) XMMS_CMD_DEFINE6(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, NONE, NONE, NONE)
#define XMMS_CMD_DEFINE4(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, argtype4) XMMS_CMD_DEFINE6(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, argtype4, NONE, NONE)
#define XMMS_CMD_DEFINE5(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, argtype4, argtype5) XMMS_CMD_DEFINE6(cmdid, realfunc, argtype0, _rettype, argtype1, argtype2, argtype3, argtype4, argtype5, NONE)

#define XMMS_CMD_FUNC(cmdid) &__int_xmms_cmd_desc_##cmdid


void __int_xmms_object_unref (xmms_object_t *object);
xmms_object_t *__int_xmms_object_new (gint size, xmms_object_destroy_func_t destfunc);

#define xmms_object_ref(obj) do { \
	if (obj && XMMS_IS_OBJECT (obj)) { \
		XMMS_OBJECT (obj)->ref++; \
	} \
} while (0)

#define xmms_object_unref(obj) do { \
	if (obj && XMMS_IS_OBJECT (obj)) { \
		__int_xmms_object_unref (XMMS_OBJECT (obj)); \
	} \
} while (0)

#define xmms_object_new(objtype,destroyfunc) (objtype *) __int_xmms_object_new (sizeof (objtype), destroyfunc)

G_END_DECLS

#endif /* __XMMS_OBJECT_H__ */
