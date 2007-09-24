#include "perl_xmmsclient.h"

void
perl_xmmsclient_xmmsc_service_method_notify_cb (xmmsc_connection_t *conn,
                                                xmmsc_result_t *res,
                                                xmmsc_service_method_t *method,
                                                void *user_data)
{
	PerlXMMSClientCallback *cb = (PerlXMMSClientCallback *)user_data;

	perl_xmmsclient_callback_invoke (cb, conn, res, method);
}

MODULE = Audio::XMMSClient::Service::Method	PACKAGE = Audio::XMMSClient::Service::Method	PREFIX = xmmsc_service_method_

xmmsc_service_method_t *
xmmsc_service_method_new (class, name, description, func, user_data=NULL)
		const char *name
		const char *description
		SV *func
		SV *user_data
	PREINIT:
		PerlXMMSClientCallback *cb = NULL;
		PerlXMMSClientCallbackParamType param_types[3];
	INIT:
		param_types[0] = PERL_XMMSCLIENT_CALLBACK_PARAM_TYPE_CONNECTION;
		param_types[1] = PERL_XMMSCLIENT_CALLBACK_PARAM_TYPE_RESULT;
		param_types[2] = PERL_XMMSCLIENT_CALLBACK_PARAM_TYPE_METHOD;

		cb = perl_xmmsclient_callback_new (func, user_data, NULL, 3, param_types);
	CODE:
		RETVAL = xmmsc_service_method_new_full (name,
		                                        description,
		                                        perl_xmmsclient_xmmsc_service_method_notify_cb,
		                                        cb,
		                                        (xmmsc_user_data_free_func_t)perl_xmmsclient_callback_destroy);

NO_OUTPUT int
xmmsc_service_method_attribute_get (xmmsc_service_method_t *method, OUTLIST char *name, OUTLIST char *description)
	POSTCALL:
		if (RETVAL == 0) {
			XSRETURN_UNDEF;
		}

int
xmmsc_service_method_arg_type_add (method, name, type, optional=0)
		xmmsc_service_method_t *method
		const char *name
		xmmsc_service_arg_type_t type
		int32_t optional

int
xmmsc_service_method_ret_type_add (method, name, type, optional=0)
		xmmsc_service_method_t *method
		const char *name
		xmmsc_service_arg_type_t type
		int32_t optional

int
xmmsc_service_method_arg_add_uint32 (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		uint32_t value

int
xmmsc_service_method_arg_add_int32 (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		int32_t value

int
xmmsc_service_method_arg_add_string (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		char *value

int
xmmsc_service_method_arg_add_stringlist (method, name, ...)
		xmmsc_service_method_t *method
		const char *name
	PREINIT:
		int i, nargs;
		char **list;
	INIT:
		nargs = items - 2;
		list = (char **)malloc (sizeof (char *) * nargs);

		for (i = 2; i < items; i++) {
			list[i - 2] = SvPV_nolen (ST (i));
		}
	C_ARGS:
		method, name, list
	CLEANUP:
		free (list);

int
xmmsc_service_method_arg_add_coll (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		xmmsc_coll_t *value

int
xmmsc_service_method_arg_add_bin (method, name, value)
		xmmsc_service_method_t *method
		const char *name
	PREINIT:
		STRLEN len = 0;
	INPUT:
		unsigned char *data = SvPVbyte ($arg, len);
	C_ARGS:
		method, name, data, len

int
xmmsc_service_method_ret_add_uint32 (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		uint32_t value

int
xmmsc_service_method_ret_add_int32 (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		int32_t value

int
xmmsc_service_method_ret_add_string (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		char *value

int
xmmsc_service_method_ret_add_stringlist (method, name, ...)
		xmmsc_service_method_t *method
		const char *name
	PREINIT:
		int i, nargs;
		char **list;
	INIT:
		nargs = items - 2;
		list = (char **)malloc (sizeof (char *) * nargs);

		for (i = 2; i < items; i++) {
			list[i - 2] = SvPV_nolen (ST (i));
		}
	C_ARGS:
		method, name, list
	CLEANUP:
		free (list);

int
xmmsc_service_method_ret_add_coll (method, name, value)
		xmmsc_service_method_t *method
		const char *name
		xmmsc_coll_t *value

int
xmmsc_service_method_ret_add_bin (method, name, value)
		xmmsc_service_method_t *method
		const char *name
	PREINIT:
		STRLEN len = 0;
	INPUT:
		unsigned char *data = SvPVbyte ($arg, len);
	C_ARGS:
		method, name, data, len

NO_OUTPUT int
xmmsc_service_method_arg_size (xmmsc_service_method_t *method, OUTLIST uint32_t size)
	POSTCALL:
		if (RETVAL == 0) {
			XSRETURN_UNDEF;
		}

NO_OUTPUT int
xmmsc_service_method_ret_size (xmmsc_service_method_t *method, OUTLIST uint32_t size)
	POSTCALL:
		if (RETVAL == 0) {
			XSRETURN_UNDEF;
		}

NO_OUTPUT int
xmmsc_service_method_arg_attribute_get (xmmsc_service_method_t *method, const char *name, OUTLIST xmmsc_service_arg_type_t type, OUTLIST uint32_t optional, OUTLIST uint32_t none)
	POSTCALL:
		if (RETVAL == 0) {
			XSRETURN_UNDEF;
		}

NO_OUTPUT int
xmmsc_service_method_ret_attribute_get (xmmsc_service_method_t *method, const char *name, OUTLIST xmmsc_service_arg_type_t type, OUTLIST uint32_t optional, OUTLIST uint32_t none)
	POSTCALL:
		if (RETVAL == 0) {
			XSRETURN_UNDEF;
		}

int
xmmsc_service_method_arg_remove (method, name)
		xmmsc_service_method_t *method
		const char *name

int
xmmsc_service_method_ret_remove (method, name)
		xmmsc_service_method_t *method
		const char *name

const char *
xmmsc_service_method_error_get (method)
		xmmsc_service_method_t *method

int
xmmsc_service_method_error_set (method, err)
		xmmsc_service_method_t *method
		const char *err

void
xmmsc_service_method_error_reset (method)
		xmmsc_service_method_t *method

int
xmmsc_service_method_error_isset (method)
		xmmsc_service_method_t *method

void
DESTROY (method)
		xmmsc_service_method_t *method
	CODE:
		xmmsc_service_method_unref (method);
