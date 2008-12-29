#ifndef __XMMS_PRIV_IPC_H__
#define __XMMS_PRIV_IPC_H__

#include "xmms/xmms_ipc.h"

typedef struct xmms_ipc_St xmms_ipc_t;

xmms_ipc_t *xmms_ipc_init (void);
void xmms_ipc_shutdown (void);
void on_config_ipcsocket_change (xmms_object_t *object, gconstpointer data, gpointer udata);
gboolean xmms_ipc_setup_server (const gchar *path);
gboolean xmms_ipc_setup_with_gmain (xmms_ipc_t *ipc);

gboolean xmms_ipc_has_pending (guint signalid);


/*
  Here be same macros for declaring methods.

  The quite heavily use indirection and pasting.

 */

#define XI_GLIBTYPE_UINT32 guint32

#define XI_TYP_XI_ARG(typ, nam) XI_GLIBTYPE_##typ
#define XI_NAM_XI_ARG(typ, nam) nam
#define XI_TYP_XI_NOARG()
#define XI_NAM_XI_NOARG()

#define XI_ARG_VARDECL(x) XI_TYP_##x XI_NAM_##x

#define XI_COMMA_XI_ARG(typ,nam) ,
#define XI_COMMA_XI_NOARG()
#define XI_ARG_ARG(x) XI_COMMA_##x XI_NAM_##x


#define XI_ARG_DESC_XI_ARG(typ, nam) xmmsv_list_append (args, xmmsv_build_dict (XMMSV_DICT_ENTRY_STR ("name", G_STRINGIFY(nam)), XMMSV_DICT_ENTRY_STR ("type", G_STRINGIFY(typ)), XMMSV_DICT_END))
#define XI_ARG_DESC_XI_NOARG()
#define XI_ARG_DESC(x) XI_ARG_DESC_##x



/* Argument dict deserialization stuff */
#define XI_DICT_ENTRY_GETTER_UINT32 xmmsv_get_dict_entry_uint
#define XI_GETTER_XI_ARG(typ, nam) XI_DICT_ENTRY_GETTER_##typ (args, G_STRINGIFY(nam), &nam)
#define XI_GETTER_XI_NOARG() TRUE
#define XI_ARG_GETTER(x)						\
	do {								\
		if (!XI_GETTER_##x) {					\
			XMMS_DBG ("deserialize failed");		\
			return;						\
		}							\
	} while (0)


#define XI_RETVAL_NONE(r, x) do { x; *r = xmmsv_new_none (); } while (0)
#define XI_RETVAL_UINT32(r, x) do { *r = xmmsv_new_uint (x); } while (0)
#define XI_RETVAL_DICT(r, x) do { *r = x; } while (0)
/*

  This is the actual workhorse.

  It creates a function called __xi_NAMEOFMETHOD which:
   - declares variables for the args using XI_ARG_VARDECL macro
   - grabs the args from the dict using XI_ARG_GETTER macro
   - calls the actual handler using XI_ARG_ARG to build arguments

  It also creates a function called __xi_NAMEOFMETHOD_desc which:
   - returns a xmmsv_dict describing the function for introspection

 */
#define XMMS_XI_DECLARE(methodname, function, udtype, rettype, arg1, arg2) \
	static void							\
	__xi_##methodname (xmmsv_t *args, xmmsv_t **res, gpointer ud)	\
	{								\
		udtype userdata = (udtype) ud;				\
		xmms_error_t err;					\
		XI_ARG_VARDECL(arg1);					\
		XI_ARG_VARDECL(arg2);					\
									\
		xmms_error_reset (&err);				\
									\
		XI_ARG_GETTER(arg1);					\
		XI_ARG_GETTER(arg2);					\
		XI_RETVAL_##rettype (res, function (userdata XI_ARG_ARG(arg1) XI_ARG_ARG(arg2), &err)); \
	}								\
									\
	static xmmsv_t *						\
	__xi_##methodname##_desc (void)					\
	{								\
		xmmsv_t *res;						\
		xmmsv_t *args;						\
		res = xmmsv_build_dict (XMMSV_DICT_ENTRY_STR ("name", G_STRINGIFY(methodname)),XMMSV_DICT_END);	\
		args = xmmsv_new_list ();				\
		XI_ARG_DESC(arg1);					\
		XI_ARG_DESC(arg2);					\
		xmmsv_dict_insert (res, "args", args);			\
		return res;						\
	}


void xmms_ipc_obj_new (const char *name);
void xmms_ipc_obj_meth_add (const char *objn, const char *methn, gpointer ud, gpointer func, xmmsv_t *desc);

#define XMMS_XI_OBJ_METH_ADD(obj, meth, ud) xmms_ipc_obj_meth_add (obj, G_STRINGIFY (meth), ud, __xi_##meth, __xi_##meth##_desc ());

#endif
