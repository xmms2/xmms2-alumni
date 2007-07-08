#include "xmms/xmms_xformplugin.h"

static gchar *korv = "korv";

static gboolean xmms_korv_init (xmms_xform_t *xform);
static void xmms_korv_destroy (xmms_xform_t *xform);
static gint xmms_korv_read (xmms_xform_t *xform, void *buffer, gint len, xmms_error_t *error);
static gboolean xmms_korv_plugin_setup (xmms_xform_plugin_t *xform_plugin);

XMMS_XFORM_PLUGIN ("korv", "korv transport", XMMS_VERSION, "korving stuff", xmms_korv_plugin_setup);

static gboolean
xmms_korv_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
	xmms_xform_methods_t methods;

	XMMS_XFORM_METHODS_INIT (methods);
	methods.init = xmms_korv_init;
	methods.destroy = xmms_korv_destroy;
	methods.read = xmms_korv_read;

	xmms_xform_plugin_methods_set (xform_plugin, &methods);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "application/x-url",
	                              XMMS_STREAM_TYPE_URL,
	                              "korv://*",
	                              XMMS_STREAM_TYPE_END);

	return TRUE;
}

static gboolean
xmms_korv_init (xmms_xform_t *xform)
{
	GString *data;

	data = g_string_new (korv);
	xmms_xform_private_data_set (xform, data);

	xmms_xform_outdata_type_add (xform, XMMS_STREAM_TYPE_MIMETYPE, "application/x-nul-padded", XMMS_STREAM_TYPE_END);

	return TRUE;
}

static void
xmms_korv_destroy (xmms_xform_t *xform)
{
	GString *data;

	g_return_if_fail (xform);

	data = xmms_xform_private_data_get (xform);
	g_return_if_fail (data);

	g_string_free (data, TRUE);
}

static gint
xmms_korv_read (xmms_xform_t *xform, void *buffer, gint len, xmms_error_t *error)
{
	gint ret;
	GString *data;

	g_return_val_if_fail (xform, -1);
	g_return_val_if_fail (buffer, -1);
	g_return_val_if_fail (error, -1);

	data = xmms_xform_private_data_get (xform);
	g_return_val_if_fail (data, -1);

	ret = MIN (data->len, len);

	memcpy (buffer, data->str, ret);
	g_string_erase (data, 0, ret);

	return ret;
}
