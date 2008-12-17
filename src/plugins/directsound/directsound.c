/*
 * Windows DirectSound interface
 *
 * Copyright (c) 2004 Gabor Szecsi <deje@miki.hu>
 * Copyright (C) 2003-2008 XMMS2 Team
 *
 * MPlayer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * MPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with MPlayer; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <glib.h>
#include <windows.h>
#define DIRECTSOUND_VERSION 0x0600
#include <dsound.h>

#include "xmms/xmms_outputplugin.h"
#include "xmms/xmms_log.h"

#include "directsound.h"

static gboolean
xmms_directsound_plugin_setup (xmms_output_plugin_t *plugin)
{
	xmms_output_methods_t methods;

	XMMS_OUTPUT_METHODS_INIT (methods);

	methods.new = xmms_directsound_new;
	methods.destroy = xmms_directsound_destroy;

	methods.open = xmms_directsound_open;
	methods.close = xmms_directsound_close;

	methods.flush = xmms_directsound_flush;
	methods.format_set = xmms_directsound_format_set;

	methods.volume_get = xmms_directsound_volume_get;
	methods.volume_set = xmms_directsound_volume_set;

	methods.write = xmms_directsound_write;

	methods.latency_get = xmms_directsound_buffer_bytes_get;

	xmms_output_plugin_methods_set (plugin, &methods);

	/* register default device 0 (Primary Device) */
	xmms_output_plugin_config_property_register (plugin,
	                                             "device",
	                                             "0",
	                                             NULL,
	                                             NULL);

	/* register buffer length of 2000ms */
	xmms_output_plugin_config_property_register (plugin,
	                                             "bufferlength",
	                                             "2000",
	                                             NULL,
	                                             NULL);

	return TRUE;
}

/*
 * Member functions
 */
static gboolean
xmms_directsound_volume_set (xmms_output_t *output, const gchar *channel,
                             guint volume)
{
	xmms_directsound_data_t *data;
	DWORD tmp = 0;

	g_return_val_if_fail (output, FALSE);
	g_return_val_if_fail (channel, FALSE);

	data = xmms_output_private_data_get (output);
	g_return_val_if_fail (data, FALSE);

	/* update the channel volumes */
	if (!strcmp (channel, "right")) {
		tmp = (DWORD)(log10 (volume) * 5000.0) - 10000;
		IDirectSoundBuffer_SetVolume (data->hdsbuf, tmp);
	} else if (!strcmp (channel, "left")) {
		tmp = (DWORD)(log10 (volume) * 5000.0) - 10000;
		IDirectSoundBuffer_SetVolume (data->hdsbuf, tmp);
	} else {
		return FALSE;
	}

	return TRUE;
}

static gboolean
xmms_directsound_volume_get (xmms_output_t *output, gchar const **names,
                             guint *values, guint *num_channels)
{
	xmms_directsound_data_t *data;
	DWORD tmp = 0;
	gint i;
	struct {
		const gchar *name;
		gint value;
	} channel_map[] = {
		{"right", 0},
		{"left", 0}
	};

	g_return_val_if_fail (output, FALSE);

	data = xmms_output_private_data_get (output);
	g_return_val_if_fail (data, FALSE);

	if (!*num_channels) {
		*num_channels = 2;
		return TRUE;
	}

	if (data->hdsbuf) {
		IDirectSoundBuffer_GetVolume (data->hdsbuf, &tmp);
		float volume = pow (10.0, (float)(tmp+10000) / 5000.0);
		channel_map[0].value = floor (volume + 0.5);
		channel_map[1].value = floor (volume + 0.5);

	} else {
		return FALSE;
	}

	for (i = 0; i < 2; i++) {
		names[i] = channel_map[i].name;
		values[i] = channel_map[i].value;
	}

	return TRUE;
}

static gboolean
xmms_directsound_new (xmms_output_t *output)
{
	xmms_directsound_data_t *data;
	const xmms_config_property_t *val;

	g_return_val_if_fail (output, FALSE);

	data = g_new0 (xmms_directsound_data_t, 1);
	g_return_val_if_fail (data, FALSE);

	val = xmms_output_config_lookup (output, "device");
	data->device_num = xmms_config_property_get_int (val);
	data->device_index = 0;

	/* Initialize DirectSound */
	if (!InitDirectSound (data)) {
		return FALSE;
	}

	data->mutex = g_mutex_new ();
	data->cond = g_cond_new ();

	xmms_output_private_data_set (output, data);

	xmms_output_format_add (output, XMMS_SAMPLE_FORMAT_S16, 2, 44100);

	return TRUE;
}

static void
xmms_directsound_destroy (xmms_output_t *output)
{
	xmms_directsound_data_t *data;

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	UninitDirectSound (data);

	if (data->mutex) {
		g_mutex_free (data->mutex);
	}

	if (data->cond) {
		g_cond_free (data->cond);
	}

	g_free (data);
}

static gboolean
xmms_directsound_open (xmms_output_t *output)
{
	xmms_directsound_data_t *data;
	const xmms_config_property_t *val;
	int res;
	WAVEFORMATEXTENSIBLE wformat;
	DSBUFFERDESC dsbpridesc;
	DSBUFFERDESC dsbdesc;

	int channels   = 2;
	int rate       = 44100;
	int format     = AF_FORMAT_S16_LE;
	int bps        = channels * rate * (af_fmt2bits (format)>>3);
	int buffersize;

	g_return_val_if_fail (output, FALSE);
	data = xmms_output_private_data_get (output);
	g_return_val_if_fail (data, FALSE);

	val = xmms_output_config_lookup (output, "bufferlength");
	buffersize = bps * xmms_config_property_get_int (val) / 1000;

	if (buffersize<=0) {
		buffersize = ((bps >> 12) + 1) << 12; /* space for ca. 1 sec */
	}

	XMMS_DBG ("Samplerate: %iHz Channels: %i Format: %s", rate, channels,
	          af_fmt2str_short (format));
	XMMS_DBG ("Buffersize: %d bytes (%d msec)", buffersize,
	          buffersize / bps * 1000);

	/* fill waveformatex */
	ZeroMemory (&wformat, sizeof (WAVEFORMATEXTENSIBLE));
	wformat.Format.cbSize          = (channels > 2) ?
	                                 sizeof (WAVEFORMATEXTENSIBLE) -
	                                 sizeof (WAVEFORMATEX) : 0;
	wformat.Format.nChannels       = channels;
	wformat.Format.nSamplesPerSec  = rate;

	if (format == AF_FORMAT_AC3) {
		wformat.Format.wFormatTag      = WAVE_FORMAT_DOLBY_AC3_SPDIF;
		wformat.Format.wBitsPerSample  = 16;
		wformat.Format.nBlockAlign     = 4;
	} else {
		wformat.Format.wFormatTag      = (channels > 2) ?
		                                 WAVE_FORMAT_EXTENSIBLE :
		                                 WAVE_FORMAT_PCM;
		wformat.Format.wBitsPerSample  = af_fmt2bits (format);
		wformat.Format.nBlockAlign     = wformat.Format.nChannels *
		                                 (wformat.Format.wBitsPerSample >> 3);
	}

	/* fill in primary sound buffer descriptor */
	memset (&dsbpridesc, 0, sizeof (DSBUFFERDESC));
	dsbpridesc.dwSize        = sizeof (DSBUFFERDESC);
	dsbpridesc.dwFlags       = DSBCAPS_PRIMARYBUFFER;
	dsbpridesc.dwBufferBytes = 0;
	dsbpridesc.lpwfxFormat   = NULL;


	/* fill in the secondary sound buffer (=stream buffer) descriptor */
	memset (&dsbdesc, 0, sizeof (DSBUFFERDESC));
	dsbdesc.dwSize = sizeof (DSBUFFERDESC);
	dsbdesc.dwFlags = DSBCAPS_GETCURRENTPOSITION2 /* Better position accuracy */
	                | DSBCAPS_GLOBALFOCUS         /* Allows background playing*/
	                | DSBCAPS_CTRLVOLUME;         /* volume control enabled */

	if (channels > 2) {
		wformat.dwChannelMask = channel_mask[channels - 3];
		wformat.SubFormat = KSDATAFORMAT_SUBTYPE_PCM;
		wformat.Samples.wValidBitsPerSample = wformat.Format.wBitsPerSample;
		/* Needed for 5.1 on emu101k - shit soundblaster */
		dsbdesc.dwFlags |= DSBCAPS_LOCHARDWARE;
	}
	wformat.Format.nAvgBytesPerSec = wformat.Format.nSamplesPerSec *
	                                 wformat.Format.nBlockAlign;

	dsbdesc.dwBufferBytes = buffersize;
	dsbdesc.lpwfxFormat = (WAVEFORMATEX *)&wformat;
	data->buffer_size = dsbdesc.dwBufferBytes;
	data->write_offset = 0;
	data->min_free_space = wformat.Format.nBlockAlign;
	data->outburst = wformat.Format.nBlockAlign * 512;

	/* create primary buffer and set its format */
	res = IDirectSound_CreateSoundBuffer (data->hds, &dsbpridesc,
	                                      &data->hdspribuf, NULL );
	if ( res != DS_OK ) {
		UninitDirectSound (data);
		xmms_log_error ("Cannot create primary buffer (%s)\n", dserr2str (res));
		return FALSE;
	}
	res = IDirectSoundBuffer_SetFormat (data->hdspribuf,
	                                    (WAVEFORMATEX *) &wformat);
	if (res != DS_OK) {
		xmms_log_info ("Cannot set primary buffer format (%s), using standard "
		               "setting (bad quality)", dserr2str (res));
	}

	XMMS_DBG ("Primary (stream)buffer created");

	/* now create the stream buffer */
	res = IDirectSound_CreateSoundBuffer (data->hds, &dsbdesc,
	                                      &data->hdsbuf, NULL);
	if (res != DS_OK) {
		if (dsbdesc.dwFlags & DSBCAPS_LOCHARDWARE) {
			/* Try without DSBCAPS_LOCHARDWARE */
			dsbdesc.dwFlags &= ~DSBCAPS_LOCHARDWARE;
			res = IDirectSound_CreateSoundBuffer (data->hds, &dsbdesc,
			                                      &data->hdsbuf, NULL);
		}
		if (res != DS_OK) {
			UninitDirectSound (data);
			xmms_log_error ("Cannot create secondary (stream)buffer (%s)\n",
			                dserr2str (res));
			return FALSE;
		}
	}

	XMMS_DBG ("Secondary (stream)buffer created");

	return TRUE;
}

static void
xmms_directsound_close (xmms_output_t *output)
{
	xmms_directsound_data_t *data;

	XMMS_DBG ("Trying to close DirectSound");

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	if (!data->hds) {
		return;
	}

	if (TRUE) {
		IDirectSoundBuffer_Stop (data->hdsbuf);
		/* reset directsound buffer */
		IDirectSoundBuffer_SetCurrentPosition (data->hdsbuf, 0);
		data->write_offset=0;
	} else {
		/* Play buffer if we want to stop?! */
		DWORD status;
		IDirectSoundBuffer_Play (data->hdsbuf, 0, 0, 0);
		while (!IDirectSoundBuffer_GetStatus (data->hdsbuf, &status) &&
		      (status & DSBSTATUS_PLAYING)) {
			g_usleep (20000);
		}
	}

	/* Destroy the buffers */
	if (data->hdsbuf) {
		IDirectSoundBuffer_Release (data->hdsbuf);
		data->hdsbuf = NULL;
	}
	if (data->hdspribuf) {
		IDirectSoundBuffer_Release (data->hdspribuf);
		data->hdspribuf = NULL;
	}
}

static void
xmms_directsound_flush (xmms_output_t *output)
{
	xmms_directsound_data_t *data;

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);

	if (data->hdsbuf) {
		IDirectSoundBuffer_Stop (data->hdsbuf);
		/* reset directsound buffer */
		IDirectSoundBuffer_SetCurrentPosition (data->hdsbuf, 0);
		data->write_offset=0;
	}
}

static gboolean
xmms_directsound_format_set (xmms_output_t *output,
                             const xmms_stream_type_t *format)
{
	/*
	 * Format is locked to 44.1kHz, 16bit stereo.
	 * WaveMapper handles the conversion for now.
	 */

	return TRUE;
}

static void
xmms_directsound_write (xmms_output_t *output, gpointer buffer, gint len,
                        xmms_error_t *err)
{
	xmms_directsound_data_t *data;
	int cnt = 0;
	int bytes_written;

	g_return_if_fail (buffer);
	g_return_if_fail (len > 0);

	g_return_if_fail (output);
	data = xmms_output_private_data_get (output);
	g_return_if_fail (data);
	g_return_if_fail (data->hds);

	g_mutex_lock (data->mutex);

	bytes_written = write_buffer (data, buffer, len);
	while (len != bytes_written) {
		if (cnt == 10) {
			XMMS_DBG ("Tried %i times, better luck next time", cnt);
			break;
		}
		g_usleep (10000); /* Wait a tick before retry */

		/* Write remaining bytes */
		cnt++;
		if (bytes_written > 0) {
			len -= bytes_written;
			memmove (buffer, &buffer[bytes_written], len);
		}
		bytes_written = write_buffer (data, buffer, len);
	}

	g_mutex_unlock (data->mutex);
}

static guint
xmms_directsound_buffer_bytes_get (xmms_output_t *output)
{
	xmms_directsound_data_t *data;
	guint ret;

	g_return_val_if_fail (output, 0);

	data = xmms_output_private_data_get (output);
	g_return_val_if_fail (data, 0);

	g_mutex_lock (data->mutex);
	DWORD play_offset;
	IDirectSoundBuffer_GetCurrentPosition (data->hdsbuf, &play_offset, NULL);
	ret=data->buffer_size-(data->write_offset-play_offset);
	g_mutex_unlock (data->mutex);

	return ret;
}

/* Stolen methods */
static gboolean InitDirectSound (xmms_directsound_data_t *data)
{
	DSCAPS dscaps;

	/* initialize directsound */
	HRESULT (WINAPI *OurDirectSoundCreate)(LPGUID, LPDIRECTSOUND *, LPUNKNOWN);
	HRESULT (WINAPI *OurDirectSoundEnumerate)(LPDSENUMCALLBACKA, LPVOID);

	data->hdsound_dll = LoadLibrary ("DSOUND.DLL");
	if (data->hdsound_dll == NULL) {
		xmms_log_error ("Could not load DSOUND.DLL");
		return FALSE;
	}
	OurDirectSoundCreate = (void*)GetProcAddress (data->hdsound_dll,
	                                              "DirectSoundCreate");
	OurDirectSoundEnumerate = (void*)GetProcAddress (data->hdsound_dll,
	                                                 "DirectSoundEnumerateA");

	if (OurDirectSoundCreate == NULL || OurDirectSoundEnumerate == NULL) {
		xmms_log_error ("GetProcAddress FAILED");
		FreeLibrary (data->hdsound_dll);
		return FALSE;
	}

	/* Enumerate all directsound devices */
	XMMS_DBG ("Output Devices:");
	OurDirectSoundEnumerate (DSEnumCallback, data);

	/* Create the direct sound object */
	if FAILED (OurDirectSoundCreate ((data->device_num) ? &data->device : NULL,
	                                 &data->hds, NULL )) {
		xmms_log_error ("Could not create a DirectSound device");
		FreeLibrary (data->hdsound_dll);
		return FALSE;
	}

	/* Set DirectSound Cooperative level, ie what control we want over Windows
	 * sound device. In our case, DSSCL_EXCLUSIVE means that we can modify the
	 * settings of the primary buffer, but also that only the sound of our
	 * application will be hearable when it will have the focus.
	 * !!! (this is not really working as intended yet because to set the
	 * cooperative level you need the window handle of your application, and
	 * I don't know of any easy way to get it. Especially since we might play
	 * sound without any video, and so what window handle should we use ???
	 * The hack for now is to use the Desktop window handle - it seems to be
	 * working */
	if (IDirectSound_SetCooperativeLevel (data->hds, GetDesktopWindow (),
		                                  DSSCL_EXCLUSIVE)) {
		xmms_log_error ("Could not direct sound cooperative level");
		IDirectSound_Release (data->hds);
		FreeLibrary (data->hdsound_dll);
		return FALSE;
	}
	XMMS_DBG ("DirectSound initialized");

	memset (&dscaps, 0, sizeof (DSCAPS));
	dscaps.dwSize = sizeof (DSCAPS);
	if (DS_OK == IDirectSound_GetCaps (data->hds, &dscaps)) {
		if (dscaps.dwFlags & DSCAPS_EMULDRIVER) {
			xmms_log_info ("DirectSound is emulated, waveOut may give better "
			               "performance");
		}
	} else {
		xmms_log_error ("Could not get device capabilities");
	}

	return TRUE;
}

static void UninitDirectSound (xmms_directsound_data_t *data)
{
	/* finally release the DirectSound object */
	if (data->hds) {
		IDirectSound_Release (data->hds);
		data->hds = NULL;
	}
	/* free DSOUND.DLL */
	if (data->hdsound_dll) {
		FreeLibrary (data->hdsound_dll);
		data->hdsound_dll = NULL;
	}
	XMMS_DBG ("DirectSound uninitialized");
}

static BOOL CALLBACK
DSEnumCallback (LPGUID lpGuid, LPCSTR lpcstrDescription, LPCSTR lpcstrModule,
                LPVOID lpContext)
{
	xmms_directsound_data_t* data = lpContext;

	if (data->device_num == data->device_index) {
		XMMS_DBG ("%i %s <--",data->device_index,lpcstrDescription);
		if (lpGuid){
			memcpy (&data->device, lpGuid, sizeof (GUID));
		}
	} else {
		XMMS_DBG ("%i %s",data->device_index,lpcstrDescription);
	}
	(data->device_index)++;

	return TRUE;
}

static int af_fmt2bits (int format)
{
	return (format & AF_FORMAT_BITS_MASK) + 8;
}

static struct {
	const char *name;
	const int format;
} af_fmtstr_table[] = {
	{ "mulaw", AF_FORMAT_MU_LAW },
	{ "alaw", AF_FORMAT_A_LAW },
	{ "mpeg2", AF_FORMAT_MPEG2 },
	{ "ac3", AF_FORMAT_AC3 },
	{ "imaadpcm", AF_FORMAT_IMA_ADPCM },

	{ "u8", AF_FORMAT_U8 },
	{ "s8", AF_FORMAT_S8 },
	{ "u16le", AF_FORMAT_U16_LE },
	{ "u16be", AF_FORMAT_U16_BE },
	{ "u16ne", AF_FORMAT_U16_NE },
	{ "s16le", AF_FORMAT_S16_LE },
	{ "s16be", AF_FORMAT_S16_BE },
	{ "s16ne", AF_FORMAT_S16_NE },
	{ "u24le", AF_FORMAT_U24_LE },
	{ "u24be", AF_FORMAT_U24_BE },
	{ "u24ne", AF_FORMAT_U24_NE },
	{ "s24le", AF_FORMAT_S24_LE },
	{ "s24be", AF_FORMAT_S24_BE },
	{ "s24ne", AF_FORMAT_S24_NE },
	{ "u32le", AF_FORMAT_U32_LE },
	{ "u32be", AF_FORMAT_U32_BE },
	{ "u32ne", AF_FORMAT_U32_NE },
	{ "s32le", AF_FORMAT_S32_LE },
	{ "s32be", AF_FORMAT_S32_BE },
	{ "s32ne", AF_FORMAT_S32_NE },
	{ "floatle", AF_FORMAT_FLOAT_LE },
	{ "floatbe", AF_FORMAT_FLOAT_BE },
	{ "floatne", AF_FORMAT_FLOAT_NE },

	{ NULL, 0 }
};

const char *
af_fmt2str_short (int format)
{
	int i;

	for (i = 0; af_fmtstr_table[i].name; i++) {
		if (af_fmtstr_table[i].format == format) {
			return af_fmtstr_table[i].name;
		}
	}

	return "??";
}

static char *
dserr2str (int err)
{
	switch (err) {
		case DS_OK: return "DS_OK";
		case DS_NO_VIRTUALIZATION: return "DS_NO_VIRTUALIZATION";
		case DSERR_ALLOCATED: return "DS_NO_VIRTUALIZATION";
		case DSERR_CONTROLUNAVAIL: return "DSERR_CONTROLUNAVAIL";
		case DSERR_INVALIDPARAM: return "DSERR_INVALIDPARAM";
		case DSERR_INVALIDCALL: return "DSERR_INVALIDCALL";
		case DSERR_GENERIC: return "DSERR_GENERIC";
		case DSERR_PRIOLEVELNEEDED: return "DSERR_PRIOLEVELNEEDED";
		case DSERR_OUTOFMEMORY: return "DSERR_OUTOFMEMORY";
		case DSERR_BADFORMAT: return "DSERR_BADFORMAT";
		case DSERR_UNSUPPORTED: return "DSERR_UNSUPPORTED";
		case DSERR_NODRIVER: return "DSERR_NODRIVER";
		case DSERR_ALREADYINITIALIZED: return "DSERR_ALREADYINITIALIZED";
		case DSERR_NOAGGREGATION: return "DSERR_NOAGGREGATION";
		case DSERR_BUFFERLOST: return "DSERR_BUFFERLOST";
		case DSERR_OTHERAPPHASPRIO: return "DSERR_OTHERAPPHASPRIO";
		case DSERR_UNINITIALIZED: return "DSERR_UNINITIALIZED";
		case DSERR_NOINTERFACE: return "DSERR_NOINTERFACE";
		case DSERR_ACCESSDENIED: return "DSERR_ACCESSDENIED";
		default: return "unknown";
	}
}

static int
write_buffer (xmms_directsound_data_t *data, unsigned char *buffer, int len)
{
	while (get_space (data) < data->outburst) { /* buffer is full */
		g_usleep (10000); /* Wait a tick before retry */
	}

	DWORD play_offset;
	int space;

	/* make sure we have enough space to write data */
	IDirectSoundBuffer_GetCurrentPosition (data->hdsbuf, &play_offset, NULL);
	space=data->buffer_size-(data->write_offset-play_offset);
	if (space > data->buffer_size) {
		space -= data->buffer_size; /* write_offset < play_offset */
	}
	if (space < len) {
		len = space;
	}

	/* if (!(flags & AOPLAY_FINAL_CHUNK)) */
	/* len = (len / ao_data.outburst) * ao_data.outburst; */

	HRESULT res;
	LPVOID lpvPtr1;
	DWORD dwBytes1;
	LPVOID lpvPtr2;
	DWORD dwBytes2;

	/* Lock the buffer */
	res = IDirectSoundBuffer_Lock (data->hdsbuf,data->write_offset, len,
	                               &lpvPtr1, &dwBytes1, &lpvPtr2, &dwBytes2, 0);
	/* If the buffer was lost, restore and retry lock. */
	if (DSERR_BUFFERLOST == res)
	{
		IDirectSoundBuffer_Restore (data->hdsbuf);
		res = IDirectSoundBuffer_Lock (data->hdsbuf, data->write_offset, len,
		                               &lpvPtr1, &dwBytes1, &lpvPtr2, &dwBytes2,
		                               0);
	}

	int channels = 2;
	int format   = AF_FORMAT_S16_LE;

	if (SUCCEEDED (res)) {
		if ((channels == 6) && (format!=AF_FORMAT_AC3)) {
			/* reorder channels while writing to pointers. */
			/* it's this easy because buffer size and len are always */
			/* aligned to multiples of channels*bytespersample */
			/* there's probably some room for speed improvements here */
			const int chantable[6] = {0, 1, 4, 5, 2, 3}; /* reorder "matrix" */
			int i, j;
			int numsamp,sampsize;

			sampsize = af_fmt2bits (format) >> 3; /* bytes per sample */
			/* number of samples for each channel in this buffer */
			numsamp = dwBytes1 / (channels * sampsize);

			for (i = 0; i < numsamp; i++) {
				for (j = 0; j < channels; j++) {
					memcpy (lpvPtr1 + (i * channels * sampsize) +
					        (chantable[j] * sampsize),
					        buffer + (i * channels * sampsize) + (j * sampsize),
					        sampsize);
				}
			}

			if (NULL != lpvPtr2) {
				numsamp = dwBytes2 / (channels * sampsize);
				for (i = 0; i < numsamp; i++) {
					for (j = 0; j < channels; j++) {
					memcpy (lpvPtr2 + (i * channels * sampsize) +
					        (chantable[j] * sampsize),
					        buffer + dwBytes1 + (i * channels * sampsize) +
					        (j * sampsize),
					        sampsize);
					}
				}
			}

			data->write_offset += dwBytes1+dwBytes2;
			if (data->write_offset >= data->buffer_size) {
				data->write_offset = dwBytes2;
			}
		} else {
			/* Write to pointers without reordering. */
			memcpy (lpvPtr1, buffer, dwBytes1);
			if (NULL != lpvPtr2) {
				memcpy (lpvPtr2, buffer + dwBytes1, dwBytes2);
			}
			data->write_offset += dwBytes1 + dwBytes2;
			if (data->write_offset >= data->buffer_size) {
				data->write_offset=dwBytes2;
			}
		}

		/* Release the data back to DirectSound. */
		res = IDirectSoundBuffer_Unlock (data->hdsbuf, lpvPtr1, dwBytes1,
		                                 lpvPtr2, dwBytes2);
		if (SUCCEEDED (res)) {
			/* Success. */
			DWORD status;
			IDirectSoundBuffer_GetStatus (data->hdsbuf, &status);
			if (!(status & DSBSTATUS_PLAYING)){
				res = IDirectSoundBuffer_Play (data->hdsbuf, 0, 0,
				                               DSBPLAY_LOOPING);
			}
			return dwBytes1 + dwBytes2;
		}
	}

	/* Lock, Unlock, or Restore failed. */
	return 0;
}

static int
get_space (xmms_directsound_data_t *data)
{
	int space;
	DWORD play_offset;

	IDirectSoundBuffer_GetCurrentPosition (data->hdsbuf, &play_offset, NULL);
	space = data->buffer_size - (data->write_offset - play_offset);

	if (space > data->buffer_size) {
		space -= data->buffer_size; /* write_offset < play_offset */
	}

	if (space < data->min_free_space) {
		return 0;
	}

	return space-data->min_free_space;
}
