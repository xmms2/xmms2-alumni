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

#ifndef _DIRECTSOUND_H_
#define _DIRECTSOUND_H_

/*
 * Defines
 */

#define BLOCK_SIZE 16384
#define NUM_BLOCKS 6

#define WAVE_FORMAT_IEEE_FLOAT 0x0003
#define WAVE_FORMAT_DOLBY_AC3_SPDIF 0x0092
#define WAVE_FORMAT_EXTENSIBLE 0xFFFE

static const GUID KSDATAFORMAT_SUBTYPE_PCM = {0x1,0x0000,0x0010, {0x80,0x00,0x00,0xaa,0x00,0x38,0x9b,0x71}};

#define SPEAKER_FRONT_LEFT             0x1
#define SPEAKER_FRONT_RIGHT            0x2
#define SPEAKER_FRONT_CENTER           0x4
#define SPEAKER_LOW_FREQUENCY          0x8
#define SPEAKER_BACK_LEFT              0x10
#define SPEAKER_BACK_RIGHT             0x20
#define SPEAKER_FRONT_LEFT_OF_CENTER   0x40
#define SPEAKER_FRONT_RIGHT_OF_CENTER  0x80
#define SPEAKER_BACK_CENTER            0x100
#define SPEAKER_SIDE_LEFT              0x200
#define SPEAKER_SIDE_RIGHT             0x400
#define SPEAKER_TOP_CENTER             0x800
#define SPEAKER_TOP_FRONT_LEFT         0x1000
#define SPEAKER_TOP_FRONT_CENTER       0x2000
#define SPEAKER_TOP_FRONT_RIGHT        0x4000
#define SPEAKER_TOP_BACK_LEFT          0x8000
#define SPEAKER_TOP_BACK_CENTER        0x10000
#define SPEAKER_TOP_BACK_RIGHT         0x20000
#define SPEAKER_RESERVED               0x80000000

#define DSSPEAKER_HEADPHONE         0x00000001
#define DSSPEAKER_MONO              0x00000002
#define DSSPEAKER_QUAD              0x00000003
#define DSSPEAKER_STEREO            0x00000004
#define DSSPEAKER_SURROUND          0x00000005
#define DSSPEAKER_5POINT1           0x00000006

/* Endianess */
#define AF_FORMAT_BE       (0<<0) /* Big Endian */
#define AF_FORMAT_LE       (1<<0) /* Little Endian */
#define AF_FORMAT_END_MASK (1<<0)

#if WORDS_BIGENDIAN        /* Native endian of cpu */
#define	AF_FORMAT_NE       AF_FORMAT_BE
#else
#define	AF_FORMAT_NE       AF_FORMAT_LE
#endif

/* Signed/unsigned */
#define AF_FORMAT_SI        (0<<1) /* Signed */
#define AF_FORMAT_US        (1<<1) /* Unsigned */
#define AF_FORMAT_SIGN_MASK (1<<1)

/* Fixed or floating point */
#define AF_FORMAT_I          (0<<2) /* Int */
#define AF_FORMAT_F          (1<<2) /* Foating point */
#define AF_FORMAT_POINT_MASK (1<<2)

/* Bits used */
#define AF_FORMAT_8BIT      (0<<3)
#define AF_FORMAT_16BIT     (1<<3)
#define AF_FORMAT_24BIT     (2<<3)
#define AF_FORMAT_32BIT     (3<<3)
#define AF_FORMAT_40BIT     (4<<3)
#define AF_FORMAT_48BIT     (5<<3)
#define AF_FORMAT_BITS_MASK (7<<3)

/* Special flags refering to non pcm data */
#define AF_FORMAT_MU_LAW       (1<<6)
#define AF_FORMAT_A_LAW        (2<<6)
#define AF_FORMAT_MPEG2        (3<<6) /* MPEG(2) audio */
#define AF_FORMAT_AC3          (4<<6) /* Dolby Digital AC3 */
#define AF_FORMAT_IMA_ADPCM    (5<<6)
#define AF_FORMAT_SPECIAL_MASK (7<<6)

/* PREDEFINED formats */
#define AF_FORMAT_U8     (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_8BIT|AF_FORMAT_NE)
#define AF_FORMAT_S8     (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_8BIT|AF_FORMAT_NE)
#define AF_FORMAT_U16_LE (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_16BIT|AF_FORMAT_LE)
#define AF_FORMAT_U16_BE (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_16BIT|AF_FORMAT_BE)
#define AF_FORMAT_S16_LE (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_16BIT|AF_FORMAT_LE)
#define AF_FORMAT_S16_BE (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_16BIT|AF_FORMAT_BE)
#define AF_FORMAT_U24_LE (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_24BIT|AF_FORMAT_LE)
#define AF_FORMAT_U24_BE (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_24BIT|AF_FORMAT_BE)
#define AF_FORMAT_S24_LE (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_24BIT|AF_FORMAT_LE)
#define AF_FORMAT_S24_BE (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_24BIT|AF_FORMAT_BE)
#define AF_FORMAT_U32_LE (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_32BIT|AF_FORMAT_LE)
#define AF_FORMAT_U32_BE (AF_FORMAT_I|AF_FORMAT_US|AF_FORMAT_32BIT|AF_FORMAT_BE)
#define AF_FORMAT_S32_LE (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_32BIT|AF_FORMAT_LE)
#define AF_FORMAT_S32_BE (AF_FORMAT_I|AF_FORMAT_SI|AF_FORMAT_32BIT|AF_FORMAT_BE)

#define AF_FORMAT_FLOAT_LE (AF_FORMAT_F|AF_FORMAT_32BIT|AF_FORMAT_LE)
#define AF_FORMAT_FLOAT_BE (AF_FORMAT_F|AF_FORMAT_32BIT|AF_FORMAT_BE)

#ifdef WORDS_BIGENDIAN
#define AF_FORMAT_U16_NE AF_FORMAT_U16_BE
#define AF_FORMAT_S16_NE AF_FORMAT_S16_BE
#define AF_FORMAT_U24_NE AF_FORMAT_U24_BE
#define AF_FORMAT_S24_NE AF_FORMAT_S24_BE
#define AF_FORMAT_U32_NE AF_FORMAT_U32_BE
#define AF_FORMAT_S32_NE AF_FORMAT_S32_BE
#define AF_FORMAT_FLOAT_NE AF_FORMAT_FLOAT_BE
#else
#define AF_FORMAT_U16_NE AF_FORMAT_U16_LE
#define AF_FORMAT_S16_NE AF_FORMAT_S16_LE
#define AF_FORMAT_U24_NE AF_FORMAT_U24_LE
#define AF_FORMAT_S24_NE AF_FORMAT_S24_LE
#define AF_FORMAT_U32_NE AF_FORMAT_U32_LE
#define AF_FORMAT_S32_NE AF_FORMAT_S32_LE
#define AF_FORMAT_FLOAT_NE AF_FORMAT_FLOAT_LE
#endif

/*
 * Type definitions
 */
static const int channel_mask[] = {
	SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT  | SPEAKER_LOW_FREQUENCY,
	SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT  | SPEAKER_BACK_LEFT    |
	                     SPEAKER_BACK_RIGHT,
	SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT  | SPEAKER_BACK_LEFT    |
	                     SPEAKER_BACK_RIGHT | SPEAKER_LOW_FREQUENCY,
	SPEAKER_FRONT_LEFT | SPEAKER_FRONT_CENTER | SPEAKER_FRONT_RIGHT  |
	                     SPEAKER_BACK_LEFT  | SPEAKER_BACK_RIGHT |
	                     SPEAKER_LOW_FREQUENCY
};

#ifndef _WAVEFORMATEXTENSIBLE_
typedef struct {
	WAVEFORMATEX    Format;
	union {
		WORD wValidBitsPerSample;  /* bits of precision  */
		WORD wSamplesPerBlock;     /* valid if wBitsPerSample==0 */
		WORD wReserved;            /* If neither applies, set to zero. */
	} Samples;
	DWORD           dwChannelMask; /* which channels are present in stream */
	GUID            SubFormat;
} WAVEFORMATEXTENSIBLE, *PWAVEFORMATEXTENSIBLE;
#endif

typedef struct xmms_directsound_data_St {
	HINSTANCE hdsound_dll;         /* handle to the dll */
	LPDIRECTSOUND hds;             /* direct sound object */
	LPDIRECTSOUNDBUFFER hdspribuf; /* primary direct sound buffer */
	LPDIRECTSOUNDBUFFER hdsbuf;    /* secondary direct sound buffer (stream buffer) */
	int buffer_size;               /* size in bytes of the direct sound buffer */
	int write_offset;              /* offset of the write cursor in the direct sound buffer */
	/* if the free space is below this value get_space() will return 0 */
	/* there will always be at least this amout of free space to prevent */
	/* get_space() from returning wrong values when buffer is 100% full. */
	/* will be replaced with nBlockAlign in init() */
	int min_free_space;
	int device_num;                /* wanted device number */
	int device_index;              /* device index */
	GUID device;                   /* guid of the device */
	int outburst;

	/* HWAVEOUT waveout; */
	/* WAVEHDR *blocks; */
	/* gint current_block; */
	/* gint free_blocks; */
	GMutex *mutex;
	GCond *cond;
} xmms_directsound_data_t;

/*
 * Function prototypes
 */

static gboolean xmms_directsound_plugin_setup (xmms_output_plugin_t *plugin);

static gboolean xmms_directsound_new (xmms_output_t *output);
static void xmms_directsound_destroy (xmms_output_t *output);

static gboolean xmms_directsound_open (xmms_output_t *output);
static void xmms_directsound_close (xmms_output_t *output);

static void xmms_directsound_flush (xmms_output_t *output);
static gboolean xmms_directsound_format_set (xmms_output_t *output,
                                         const xmms_stream_type_t *format);

static void xmms_directsound_write (xmms_output_t *output, gpointer buffer,
                                gint len, xmms_error_t *err);

static guint xmms_directsound_buffer_bytes_get (xmms_output_t *output);

static gboolean xmms_directsound_volume_set (xmms_output_t *output, const gchar *channel, guint volume);
static gboolean xmms_directsound_volume_get (xmms_output_t *output,
                                     gchar const **names, guint *values,
                                     guint *num_channels);

static gboolean InitDirectSound (xmms_directsound_data_t *data);
static void UninitDirectSound (xmms_directsound_data_t *data);

static BOOL CALLBACK DSEnumCallback (LPGUID lpGuid, LPCSTR lpcstrDescription,
                                     LPCSTR lpcstrModule, LPVOID lpContext);

static int write_buffer (xmms_directsound_data_t *data, unsigned char *buffer,
                         int len);
static int get_space (xmms_directsound_data_t *data);

static int af_fmt2bits (int format);
/* static int af_bits2fmt(int bits); */
const char *af_fmt2str_short (int format);
int af_str2fmt_short (const char* str);
static char * dserr2str (int err);

/*
 * Plugin header
 */

XMMS_OUTPUT_PLUGIN ("directsound", "DirectSound Output", XMMS_VERSION,
                    "DirectSound Output plugin",
                    xmms_directsound_plugin_setup);

#endif /* _DIRECTSOUND_H_ */
