
/* This file is part of the GMOD package */

#include <sys/types.h>
#include <machine/soundcard.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/fcntl.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#define HEADER_SIZE	48
#define SAMPLE_SIZE	64
#define TRACK_SIZE	192
#define PATTERN_SIZE	64
#define NOTES_PER_TRACK	64
#define NR_CHANNELS	32

#define REPEAT_NOTE	0xfc	/* note signifying a RLE pattern */

#define SAMP_16BIT	4
#define SAMP_LOOP	8
#define SAMP_REVERSE	16

typedef struct
{
  char title[33];
  int version;
  int text_len;
  char *text;
  int nr_samples;
  int nr_channels;
  int nr_patterns;
  int order[256];
  int pan_positions[32];
  int sample_offset;
  int pattern_offset;
  int eoh_offset;
}

ult_header;

typedef struct
{
  char name[33];
  char filename[13];
  u_long loop_beg;
  u_long loop_end;
  u_long length;
  int volume;
  int loop;
  int finetune;
}

ult_sample;

typedef struct
{
  int a;
}

ult_pattern;

void convert_ult_effect (u_long *, u_long *, int);

void
dump_ult_header (ult_header * h, int songlength)
{
  int i;

  printf ("File created by UltraTracker; format version %d\n",
	  h->version);

  printf ("Samples: %2d   Channels: %2d  Len: %02d  Patterns: %03d Name: %s\n",
       h->nr_samples, h->nr_channels, songlength, h->nr_patterns, h->title);

  if (h->version >= 3)
    {
      printf ("Pan positions:\n");
      for (i = 0; i < h->nr_channels; i++)
	printf ("%02d%c", h->pan_positions[i], (i + 1) % 28 ? ' ' : '\n');
      if (i % 28)
	putchar ('\n');
    }

  if (h->version >= 2 && h->text_len)
    printf ("Song text: %s\n\n", h->text);
  else
    putchar ('\n');
}

static ult_sample
get_sample (char *buf)
{
  ult_sample s;
  u_long start, end;

  memcpy (s.name, buf, 32);
  s.name[32] = 0;
  buf += 32;

  memcpy (s.filename, buf, 12);
  s.filename[12] = 0;
  buf += 12;

  s.loop_beg = INTEL_LONG (buf);
  buf += 4;
  s.loop_end = INTEL_LONG (buf);
  buf += 4;
  start = INTEL_LONG (buf);
  buf += 4;
  end = INTEL_LONG (buf);
  buf += 4;
  s.volume = BYTE (buf);
  buf += 1;
  s.loop = BYTE (buf);
  buf += 1;
  s.finetune = (signed short) (INTEL_SHORT (buf));

  /* calculate length in "samples" and turn it into bytes */
  s.length = end - start;
  if (s.loop & SAMP_16BIT)
    s.length *= 2;

  return (s);
}

void
dump_ult_pattern (ult_pattern p)
{
}


/* static ult_pattern
get_pattern(char *buf)
{
  ult_pattern p;
  int i;

  return(p);
} */


ult_header
get_ult_header (FILE * mod_fd)
{
  int i;
  u_char *raw_data;
  u_char tmpchar;
  ult_header header;

  /* read in first part of header */
  fseek (mod_fd, 0, SEEK_SET);
  raw_data = (char *) malloc (HEADER_SIZE);
  fread (raw_data, 1, HEADER_SIZE, mod_fd);

  memcpy (header.title, raw_data + 15, 32);	/* copy song title */
  header.title[32] = 0;
  raw_data[15] = 0;		/* terminate "V00x" in magic number.
				 * WARNING: may overwrite title, so copy
				 * title first */

  /* get format version:
   *  2 has header+47 defined as a text_len byte,
   *  3 has pan-position table after NOP byte
   */
  header.version = atoi (raw_data + 12);

  if (header.version >= 2)
    header.text_len = raw_data[47] * 32;
  else
    header.text_len = 0;
  free (raw_data);

  if (header.text_len)
    {
      header.text = malloc (header.text_len + 1);
      fread (header.text, 1, header.text_len, mod_fd);
      header.text[header.text_len] = 0;
    }
  else
    header.text = NULL;

  /* get samples */
  fread (&tmpchar, 1, 1, mod_fd);
  header.nr_samples = tmpchar;

  header.sample_offset = HEADER_SIZE + header.text_len + 1;

  fseek (mod_fd, header.sample_offset + header.nr_samples * SAMPLE_SIZE,
	 SEEK_SET);
  raw_data = malloc (258);
  fread (raw_data, 1, 258, mod_fd);

  for (i = 0; i < 256; i++)
    header.order[i] = (unsigned) raw_data[i];

  /* these are stored as "last channel" and "last pattern",
   * so we add one to make it ordinal
   */
  header.nr_channels = raw_data[i++] + 1;
  header.nr_patterns = raw_data[i] + 1;

  header.pattern_offset = header.sample_offset +
    header.nr_samples * SAMPLE_SIZE + 258;

  if (header.version >= 3)
    {
      fread (raw_data, 1, header.nr_channels, mod_fd);
      for (i = 0; i < header.nr_channels; i++)
	header.pan_positions[i] = raw_data[i];

      header.pattern_offset += header.nr_channels;
    }
  else
    {
      for (i = 0; i < header.nr_channels; i++)
	header.pan_positions[i] = 7;
    }

  free (raw_data);

  header.eoh_offset = ftell (mod_fd);

  printf ("header offset: %d\n", header.eoh_offset);
  return (header);
}


int
load_ult_module (FILE * mod_fd, char *name, struct song_info *song_char,
		 struct options_info options)
{
  ult_header header;
  int total_sample_size, i;
  char *raw_data;
  ult_sample *samples;
  int total_mem = 0;
  int chani, notei, pati, absi;
  int gus_onboard_mem = gus_mem_free (gus_dev);

#ifdef DEBUG
  int tn = 0;
#endif

  song_char->lowest_note = 36;
  song_char->highest_note = 95;

  header = get_ult_header (mod_fd);

  /* ======== get sample data =============================== */
  total_sample_size = SAMPLE_SIZE * header.nr_samples;
  samples = (ult_sample *) malloc (header.nr_samples * sizeof (ult_sample));
  raw_data = (char *) malloc (total_sample_size);

  fseek (mod_fd, header.sample_offset, SEEK_SET);
  fread (raw_data, 1, total_sample_size, mod_fd);

  for (i = 0; i < header.nr_samples; i++)
    samples[i] = get_sample (raw_data + i * SAMPLE_SIZE);

  free (raw_data);


  /* ========== copy pattern order into tune[] ============== */
#ifdef SHOW_ORDER
  for (i = 0; i < 256 && header.order[i] != 255; i++)
    printf ("%03d%c", header.order[i], (i + 1) % 20 ? ' ' : '\n');
  putchar ('\n');
#endif

  /* copy pattern orders into the tune area and find song length
   * NB: not in the docs, but apparently empty patterns are set
   * to 255; I'm assuming the first 255 ends the song.
   */
  song_char->songlength = 0;
  for (i = 0; i < 256 && header.order[i] != 255; i++)
    {
      tune[i] = header.order[i];
      song_char->songlength++;
    }

  song_char->nr_patterns = header.nr_patterns;
  song_char->nr_channels = header.nr_channels;
  song_char->play_speed = 6;

  /* set panning */

  for (i = 0; i < header.nr_channels; i++)
    song_char->panning[i] = (header.pan_positions[i] & 0x0f) * 17 - 128;

  dump_ult_header (&header, song_char->songlength);

  /* allocate memory for patterns */
  for (pati = 0; pati < header.nr_patterns; pati++)
    pattern_table[pati] = (pattern *)
      malloc (sizeof (struct note_info) * NOTES_PER_TRACK * song_char->nr_channels);

  /* get patterns from file */
  fseek (mod_fd, header.eoh_offset, SEEK_SET);
  for (chani = 0; chani < header.nr_channels; chani++)
    {
      for (absi = 0; absi < (NOTES_PER_TRACK * song_char->nr_patterns);)
	{
	  unsigned char *p;
	  u_char note_bytes[7];
	  int repeat;

	  u_long param[2], note, effect[2], sample;

	  fread (note_bytes, 1, 5, mod_fd);
	  if (note_bytes[0] == REPEAT_NOTE)
	    {
	      fread (note_bytes + 5, 1, 2, mod_fd);
	      repeat = note_bytes[1];

	      if (repeat == 0)
		{
		  fprintf (stderr, "repeat of 0!\n");
		  repeat = 1;
		}
	      p = note_bytes + 2;
	    }
	  else
	    {
	      repeat = 1;
	      p = note_bytes;
	    }

	  note = p[0];
	  sample = p[1];
	  effect[0] = p[2] >> 4;/* are these switched? */
	  effect[1] = p[2] & 0xf;
	  param[0] = INTEL_SHORT (p + 3) >> 8;
	  param[1] = INTEL_SHORT (p + 3) & 0xff;

	  convert_ult_effect (&effect[0], &param[0], header.version);
	  convert_ult_effect (&effect[1], &param[1], header.version);

	  if (note)		/* note->period */
	    {
	      note = note + 3 * 12 - 1;	/* shift up 3 octaves */
	    }

	  for (i = 0; i < repeat && absi < (NOTES_PER_TRACK * song_char->nr_patterns); i++)
	    {
	      pati = absi / NOTES_PER_TRACK;
	      notei = absi % NOTES_PER_TRACK;

	      (*pattern_table[pati])[chani][notei].note = note;
	      (*pattern_table[pati])[chani][notei].sample = sample;
	      (*pattern_table[pati])[chani][notei].command[0] = effect[0];
	      (*pattern_table[pati])[chani][notei].parm1[0] = param[0];
	      (*pattern_table[pati])[chani][notei].parm2[0] = 0;
	      (*pattern_table[pati])[chani][notei].command[1] = effect[1];
	      (*pattern_table[pati])[chani][notei].parm1[1] = param[1];
	      (*pattern_table[pati])[chani][notei].parm2[1] = 0;
	      absi++;
#ifdef DEBUG
	      tn++;
#endif
	    }
	  if (i != repeat)
	    fprintf (stderr, "bail: i:%d repeat:%d\n", i, repeat);
	}
    }

#ifdef DEBUG
  {
    int tot_smp, j, fp, fs;
    for (tot_smp = 0, j = 0; j < header.nr_samples; j++)
      tot_smp += samples[j].length;
    fp = ftell (mod_fd);
    fs = fseek (mod_fd, 0, SEEK_END);
    fseek (mod_fd, fp, SEEK_SET);
    fprintf (stderr, "File pos: %d, file size: %d, tss: %d, rem: %d, tn %d, en: %d\n",
	     fp, fs, tot_smp, fs - fp + 1, tn, nr_channels * NOTES_PER_TRACK * nr_patterns);
  }
#endif

  for (i = 0; i < header.nr_samples; i++)
    {
      int len = samples[i].length;
      int loop_start = samples[i].loop_beg;
      int loop_end = samples[i].loop_end;
      int mem_avail;
      unsigned short loop_flags = 0;
      int tmp;

      struct patch_info *patch;

      sample_ok[i] = 0;

      if (len > 0 || options.show_empty_samples)
	fprintf (stderr, "Sample %02d: L%06d, S%06d, E%06d F%02d V%03d B%02d %12s %s\n",
		 i,
		 len,
		 loop_start,
		 loop_end,
		 samples[i].finetune,
		 samples[i].volume,
		 samples[i].loop & SAMP_16BIT ? 16 : 8,
		 samples[i].filename,
		 samples[i].name);

      if (len > 0)
	{
	  total_mem += len;

	  patch = (struct patch_info *) malloc (sizeof (*patch) + len);

	  patch->key = GUS_PATCH;
	  patch->device_no = gus_dev;
	  patch->instr_no = i;

	  loop_flags = 0;	/* signed sample data */

	  if (samples[i].loop & SAMP_16BIT)
	    loop_flags |= WAVE_16_BITS;
	  if (samples[i].loop & SAMP_LOOP)
	    loop_flags |= WAVE_LOOPING;
	  if (samples[i].loop & SAMP_REVERSE)
	    loop_flags |= WAVE_BIDIR_LOOP;

	  patch->mode = loop_flags;
	  patch->len = len;
	  patch->loop_start = loop_start;
	  patch->loop_end = loop_end;
	  patch->base_note = 261630;	/* was 261630 - Middle C */
	  if (samples[i].finetune > base_freq_table[0])
	    samples[i].finetune = base_freq_table[0];
	  patch->base_freq = base_freq_table[0] + (samples[i].finetune * 2);
	  patch->low_note = 0;
	  patch->high_note = 20000000;
	  patch->volume = 0;	/* samples[i].volume; */
	  /* if (header.pan_positions[i] > 15)
	    header.pan_positions[i] = 15; */
	  patch->panning = 16;	/* (header.pan_positions[i] - 7) * 16; */

	  if ((tmp = fread (patch->data, 1, len, mod_fd)) != len)
	    {
	      fprintf (stderr, "Short file (sample) %d, wanted %d got %d\n", i, len, tmp);
	      free (patch);
	      return 0;
	    }

#ifdef SAMPLE_DUMP
	  {
	    char sname[50];
	    int samp_fd;
	    sprintf (sname, "sample%d.raw", i);
	    samp_fd = open (sname, O_WRONLY | O_CREAT | O_TRUNC, 0666);
	    write (samp_fd, patch->data, len);
	    close (samp_fd);
	  }
#endif

	  /* try to remove loop clicking */

	  if ((loop_flags & WAVE_LOOPING) && (loop_end >= 2))
	    {
	      patch->data[loop_end] = patch->data[loop_start];
	      patch->data[loop_end - 1] =
		((signed char) (patch->data[loop_end - 2]) +
		 (signed char) (patch->data[loop_end])) / 2;
	    }

	  mem_avail = gus_mem_free (gus_dev);

	  if (mem_avail < len)
	    {
	      printf ("*** Skipping patch - %d needed, %d available ***\n",
		      len, mem_avail);
	      sample_ok[i] = 0;
	      sample_vol[i] = 0;
	    }
	  else
	    {
	      SEQ_WRPATCH (patch, sizeof (*patch) + len);

	      sample_ok[i] = 1;
	      /* if (sample->volume == 0)
		 sample->volume = 64; */
	      sample_vol[i] = samples[i].volume;
	    }

	  free (patch);
	}
    }				/* end of sample upload */

  printf ("=== %d bytes of %d total onboard used ===\n",
	  gus_onboard_mem - gus_mem_free (gus_dev), gus_onboard_mem);

  /* all done */
  free (samples);

  if (header.text_len)
    free (header.text);		/* song text of size header.text_len */

  if (header.version == 1)
    song_char->vol_type = VOL_LOG;
  else
    song_char->vol_type = VOL_LINEAR;

  return (1);
}


void
convert_ult_effect (u_long * effect, u_long * param, int version)
{
  switch (*effect)
    {
    case 1:
      break;
    case 2:
      break;
    case 3:
      break;
    case 4:
      break;
    case 0xa:			/* volslide */
      break;
    case 0xc:			/* volume */
      break;
    case 0xd:			/* pattern break */
      break;
    case 0xe:
      *effect = (0xe0) + ((*param >> 4) & 0x0f);
      *param &= 0x0f;
      break;
    case 0xf:			/* set speed */
      *effect = CVT_MOD_SPEED (*param);
      break;
    case 5:			/* special sample commands */
      *effect = 0;
      *param = 0;
      break;
    case 0xb:			/* set balance */
      *effect = CMD_SET_PAN;
      break;
    default:
      *effect = 0;
      *param = 0;
    }
}
