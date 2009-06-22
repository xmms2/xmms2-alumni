
#include "xmms/xmms_log.h"
#include "xmms/xmms_medialib.h"
#include "xmmspriv/xmms_xform.h"
#include "xmms/xmms_streamtype.h"
#include "xmmspriv/xmms_outputplugin.h"

typedef struct xmms_middleman_priv_St {
	xmms_xform_t *inputChain;
	xmms_output_t *output;
	xmms_xform_t *outputChainBegin;
	xmms_xform_t *outputChainEnd;
} xmms_middleman_priv_t;

static xmms_xform_plugin_t *middleman_plugin;

static gboolean
xmms_middleman_plugin_init (xmms_xform_t *xform)
{
	xmms_middleman_priv_t *priv;

	priv = g_new0 (xmms_middleman_priv_t, 1);

	xmms_xform_private_data_set (xform, priv);

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              48000,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              44100,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              22050,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              11025,
	                              XMMS_STREAM_TYPE_END);

	return TRUE;
}

static void
xmms_middleman_plugin_destroy (xmms_xform_t *xform)
{
	xmms_middleman_priv_t *priv;
	priv = xmms_xform_private_data_get (xform);
}

void
xmms_middleman_xform_set_next_song_args (xmms_xform_t *xform, xmms_xform_t *inputChain, xmms_output_t *output, xmms_xform_t *outputChainBegin, xmms_xform_t *outputChainEnd)
{
	xmms_middleman_priv_t *priv;
	priv = xmms_xform_private_data_get (xform);

	priv->inputChain = inputChain;
	priv->output = output;
	priv->outputChainBegin;
	priv->outputChainEnd;
}

static gint
xmms_middleman_plugin_read (xmms_xform_t *xform, void *buffer, gint len, xmms_error_t *error)
{
	xmms_middleman_priv_t *priv;
	priv = xmms_xform_private_data_get (xform);
	gint read;

	/* TODO_xforms: If this is the end of the stream, start another */

	read = xmms_xform_read (xform, buffer, len, error);

	if (read < len) { /* eos */
		/* TODO_xforms: request the next stream */
		/* TODO_xforms: send bitrate changes as auxdata */
/*		xmms_start_next_song (priv->inputChain, priv->output, priv->outputChainBegin, priv->outputChainEnd);*/
	}
	return read;
}


static gboolean
xmms_middleman_plugin_setup (xmms_xform_plugin_t *xform_plugin)
{
	xmms_xform_methods_t methods;

	XMMS_XFORM_METHODS_INIT (methods);
	methods.init = xmms_middleman_plugin_init;
	methods.destroy = xmms_middleman_plugin_destroy;
	methods.read = xmms_middleman_plugin_read;
	/* TODO_xforms
	  methods.seek
	*/

	xmms_xform_plugin_methods_set (xform_plugin, &methods);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              48000,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              44100,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              22050,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_plugin_indata_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              11025,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              48000,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              44100,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              22050,
	                              XMMS_STREAM_TYPE_END);

	xmms_xform_outdata_type_add (xform_plugin,
	                              XMMS_STREAM_TYPE_MIMETYPE,
	                              "audio/pcm",
	                              XMMS_STREAM_TYPE_FMT_FORMAT,
	                              XMMS_SAMPLE_FORMAT_S16,
	                              XMMS_STREAM_TYPE_FMT_SAMPLERATE,
	                              11025,
	                              XMMS_STREAM_TYPE_END);

	middleman_plugin = xform_plugin;

	return TRUE;
}

void xmms_middleman_xform_set_prev (xmms_xform_t *xform, xmms_xform_t *prev)
{
	xmms_xform_set_prev (xform, prev);

	/* TODO: Configure input/output types correctly */
	xmms_xform_outdata_type_copy (xform);
	xmms_xform_auxdata_set_bin (xform, "input format changed", xmms_xform_outdata_get (xform), sizeof(xmms_stream_type_t));
}

xmms_xform_t *
xmms_middleman_xform_new (xmms_xform_t *prev, xmms_medialib_entry_t entry, GList *gt)
{
	xmms_xform_t *xform;

	/* TODO_xforms */
	xform = xmms_xform_new (middleman_plugin, prev, entry, gt);

	return xform;
}

XMMS_XFORM_BUILTIN (middleman,
                    "Middleman",
                    XMMS_VERSION,
                    "Allows Persistant XForms",
                    xmms_middleman_plugin_setup);
