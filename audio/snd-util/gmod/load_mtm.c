
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

#define HEADER_SIZE	66
#define SAMPLE_SIZE	37
#define TRACK_SIZE	192
#define PATTERN_SIZE	64
#define NOTES_PER_TRACK	64
#define NR_CHANNELS	32


#define MTM_ATTRIBUTE_16BIT	1

typedef struct
{
  char magic[3];		/* must be "MTM" */
  u_char version;		/* upper 4 bits - major, lower minor */
  char name[20];		/* null-term song name string */
  u_short nr_saved_tracks;	/* number of tracks saved */
  u_char last_saved_pattern;	/* last pattern number saved */
  u_char last_played_pattern;	/* last pattern to play (songlength-1) */
  u_short comment_len;		/* length of extra comment field */
  u_char nr_samples;		/* number of samples saved (NOS) */
  u_char attribute;		/* currently defined as 0 */
  u_char bpt;			/* beats per track */
  u_char nr_played_tracks;	/* number of tracks toplay back */
  u_char pan_positions[32];	/* pan positions for each voice (0-15) */
}

mtm_header;

typedef struct
{
  char name[22];		/* null-term? */
  u_long length;		/* sample length in bytes */
  u_long loop_beg;		/* offset of beg. sample loop in bytes */
  u_long loop_end;		/* offset of end of sample loop in bytes */
  char finetune;
  u_char volume;		/* standard volume */
  u_char attribute;		/* bit 0: 0=8bit, 1=16bit,  1-7 undefined */
}

mtm_sample;

typedef struct
{
  u_char pitch;
  u_char instrument;
  u_char effect;
  u_char argument;
} mtm_note;

typedef struct
{
  mtm_note notes[NOTES_PER_TRACK];
} mtm_track;

typedef struct
{
  u_short tracks[NR_CHANNELS];
} mtm_pattern;

void
dump_mtm_header (mtm_header * h)
{
  char song_name[21];
  memcpy (song_name, h->name, 20);
  song_name[20] = 0;		/* make sure it ends */

  printf ("File created by MultiTracker version %d.%d\n",
	  h->version >> 4, h->version & 0xF);

  printf ("Name: %-20s   Samples: %2d   BPT: %4d\n\n",
	  song_name, h->nr_samples, h->bpt);
  printf ("                   Save     Played\n"
	  "                  ------    ------\n"
	  "   # Tracks        %5d     %5d\n"
	  "   Last Pattern    %5d     %5d\n\n",
	  h->nr_saved_tracks, h->nr_played_tracks,
	  h->last_saved_pattern, h->last_played_pattern);
}

static mtm_sample
get_sample (char *buf)
{
  mtm_sample s;
  memcpy (s.name, buf, 22);

  buf += 22;
  s.length = INTEL_LONG (buf);
  buf += 4;
  s.loop_beg = INTEL_LONG (buf);
  buf += 4;
  s.loop_end = INTEL_LONG (buf);
  buf += 4;
  s.finetune = *(buf++);
  s.volume = *(buf++);
  s.attribute = *(buf++);

  return (s);
}

void
dump_mtm_sample (mtm_sample s, int pan)
{
  char name[23];
  strncpy (name, s.name, 22);
  name[22] = 0;

  printf ("%-22s L%05ld B%05ld E%05ld F%02d V%02d B%02d P%02d\n",
	  name, s.length, s.loop_beg, s.loop_end,
	  s.finetune, s.volume, s.attribute & MTM_ATTRIBUTE_16BIT ? 16 : 8,
	  pan);
}

#define PITCH(p)	((*(u_char *)(p))>>2)
#define INSTRUMENT(p)	(((*(u_char *)(p)<<4) | (*((u_char *)(p)+1)>>4)) & 077)
#define EFFECT(p)	(*((u_char *)(p)+1) & 0xf)
#define ARGUMENT(p)	(*((u_char *)(p)+2))

void
dump_mtm_track (mtm_track t)
{
  int i;
  return;
  for (i = 0; i < NOTES_PER_TRACK; i++)
    printf ("P%02x I%02x E%x A%02x\n",
	    t.notes[i].pitch,
	    t.notes[i].instrument,
	    t.notes[i].effect,
	    t.notes[i].argument);
  fflush (stdout);
}

static mtm_track
get_track (char *buf)
{
  mtm_track t;
  int i;

  for (i = 0; i < NOTES_PER_TRACK; i++, buf += 3)
    {
      t.notes[i].pitch = PITCH (buf);
      t.notes[i].instrument = INSTRUMENT (buf);
      t.notes[i].effect = EFFECT (buf);
      t.notes[i].argument = ARGUMENT (buf);
    }

  return (t);
}


void
dump_mtm_pattern (mtm_pattern p)
{
  int i;
  return;
  for (i = 0; i < NR_CHANNELS; i++)
    {
      printf ("%04d%c", p.tracks[i],
	      (i + 1) % 8 ? ' ' : '\n');
    }
  printf ("\n");
}


static mtm_pattern
get_pattern (char *buf)
{
  mtm_pattern p;
  int i;

  for (i = 0; i < NR_CHANNELS; i++, buf += 2)
    p.tracks[i] = INTEL_SHORT (buf);

  return (p);
}


int
load_mtm_module (FILE * mod_fd, char *name, struct song_info *song_char,
		 struct options_info options)
{
  mtm_header header;
  int total_sample_size, i;
  char *raw_data;
  mtm_sample *samples;
  mtm_track *tracks;
  mtm_pattern *patterns;
  u_char order[128];
  int total_mem = 0;
  int tracki, notei, pati;

  int gus_onboard_mem = gus_mem_free (gus_dev);

  song_char->lowest_note = 36;
  song_char->highest_note = 98;
  song_char->vol_type = VOL_LINEAR;

  /* read in header */
  fseek (mod_fd, 0, SEEK_SET);
  raw_data = (char *) malloc (HEADER_SIZE);
  fread (raw_data, 1, HEADER_SIZE, mod_fd);

  memcpy (&header.magic, raw_data, 3);
  header.version = BYTE (raw_data + 3);
  memcpy (header.name, raw_data + 4, 20);
  header.nr_saved_tracks = INTEL_SHORT (raw_data + 24);
  header.last_saved_pattern = BYTE (raw_data + 26);
  header.last_played_pattern = BYTE (raw_data + 27);
  header.comment_len = INTEL_SHORT (raw_data + 28);
  header.nr_samples = BYTE (raw_data + 30);
  header.attribute = BYTE (raw_data + 31);
  header.bpt = BYTE (raw_data + 32);
  header.nr_played_tracks = BYTE (raw_data + 33);
  memcpy (header.pan_positions, raw_data + 34, 32);

  free (raw_data);
  dump_mtm_header (&header);

  /* ======== get sample data =============================== */
  total_sample_size = SAMPLE_SIZE * header.nr_samples;
  samples = (mtm_sample *) malloc (header.nr_samples * sizeof (mtm_sample));
  raw_data = (char *) malloc (total_sample_size);

  fseek (mod_fd, 66, SEEK_SET);
  fread (raw_data, 1, total_sample_size, mod_fd);

  for (i = 0; i < header.nr_samples; i++)
    samples[i] = get_sample (raw_data + i * SAMPLE_SIZE);

  free (raw_data);

  /* =========== read tracks ============== */
  raw_data = (char *) malloc (TRACK_SIZE * header.nr_saved_tracks);
  tracks = (mtm_track *) malloc (sizeof (mtm_track) * header.nr_saved_tracks);

  fseek (mod_fd, 194 + total_sample_size, SEEK_SET);
  fread (raw_data, 1, TRACK_SIZE * header.nr_saved_tracks, mod_fd);
  for (i = 0; i < header.nr_saved_tracks; i++)
    dump_mtm_track (tracks[i] = get_track (raw_data + i * TRACK_SIZE));

  free (raw_data);

  /* =========== read patterns ============== */
  raw_data = (char *) malloc (PATTERN_SIZE * (header.last_saved_pattern + 1));
  patterns = (mtm_pattern *) malloc (sizeof (mtm_pattern) *
				     (header.last_saved_pattern + 1));

  fseek (mod_fd, 194 + total_sample_size +
	 header.nr_saved_tracks * TRACK_SIZE, SEEK_SET);
  fread (raw_data, 1, PATTERN_SIZE * (header.last_saved_pattern + 1), mod_fd);
  for (i = 0; i <= header.last_saved_pattern; i++)
    dump_mtm_pattern (patterns[i] = get_pattern (raw_data + i * PATTERN_SIZE));

  free (raw_data);

  fseek (mod_fd, 66 + total_sample_size, SEEK_SET);
  fread (order, 1, 128, mod_fd);

  fseek (mod_fd, 194 + total_sample_size + (header.nr_saved_tracks * TRACK_SIZE) +
	 (header.last_saved_pattern + 1) * PATTERN_SIZE + header.comment_len, SEEK_SET);

  for (i = 0; i < header.nr_samples; i++)
    {
      int len = samples[i].length;
      int loop_start = samples[i].loop_beg;
      int loop_end = samples[i].loop_end;
      int mem_avail;
      unsigned short loop_flags = WAVE_UNSIGNED;
      char pname[23];		/* changed from [22] by AJR */

      struct patch_info *patch;

      sample_ok[i] = 0;

      /* found a mod with lots of samples with loop_ends of 2.  played fine
       * without them.  should I check that here?
       */
      if (loop_end > 2 && loop_end > loop_start)
	loop_flags |= WAVE_LOOPING;
      else
	loop_end = loop_start = 0;

      memcpy (pname, samples[i].name, 22);
      pname[22] = '\0';

      if (len > 0 || options.show_empty_samples)
	fprintf (stderr, "Sample %02d: L%06d, S%06d, E%06d F%02d V%02d B%02d %s\n",
		 i,
		 len,
		 loop_start,
		 loop_end,
		 samples[i].finetune,
		 samples[i].volume,
		 samples[i].attribute & MTM_ATTRIBUTE_16BIT ? 16 : 8,
		 pname);

      if (len > 0)
	{
	  total_mem += len;

	  patch = (struct patch_info *) malloc (sizeof (*patch) + len);

	  patch->key = GUS_PATCH;
	  patch->device_no = gus_dev;
	  patch->instr_no = i;
	  patch->mode = loop_flags;
	  if (samples[i].attribute & MTM_ATTRIBUTE_16BIT)
	    patch->mode |= WAVE_16_BITS;
	  patch->len = len;
	  patch->loop_start = loop_start;
	  patch->loop_end = loop_end;
	  patch->base_note = 261630;	/* was 261630 - Middle C */
	  patch->base_freq = base_freq_table[samples[i].finetune & 0xf];
	  patch->low_note = 0;
	  patch->high_note = 20000000;
	  patch->volume = 0;	/* samples[i].volume */
	  /* if (header.pan_positions[i] > 15)
	    header.pan_positions[i] = 15; */
	  patch->panning = 16;	/* (header.pan_positions[i] - 7) * 16; */


	  if (fread (patch->data, 1, len, mod_fd) != len)
	    {
	      fprintf (stderr, "Short file (sample) %d\n", i);
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
	      if (samples[i].volume > 0)
		sample_vol[i] = (samples[i].volume * 4) - 1;
	      else
		sample_vol[i] = 0;
	    }

	  free (patch);
	}
    }				/* end of sample upload */

  printf ("=== %d bytes of %d total onboard used ===\n",
	  gus_onboard_mem - gus_mem_free (gus_dev), gus_onboard_mem);

  song_char->nr_patterns = header.last_saved_pattern + 1;
  song_char->songlength = header.last_played_pattern + 1;
  song_char->nr_channels = header.nr_played_tracks;
  song_char->play_speed = 6;

  /* copy pattern orders into the tune area */
  for (i = 0; i <= header.last_played_pattern; i++)
    {
      tune[i] = order[i];
    }

  printf ("\n");

  /* determine maximum channel actually used */

  song_char->nr_channels = 0;

  for (tracki = 0; tracki < header.nr_played_tracks; tracki++)
    for (pati = 0; pati < song_char->nr_patterns; pati++)
      if ((tracki > song_char->nr_channels) && (patterns[pati].tracks[tracki]))
	song_char->nr_channels = tracki;

  song_char->nr_channels++;
  printf ("%d channels actually used.\n", song_char->nr_channels);

  /* set proper panning */

  for (i = 0; i < song_char->nr_channels; i++)
    song_char->panning[i] = (header.pan_positions[i] & 0x0f) * 17 - 128;

  for (pati = 0; pati < song_char->nr_patterns; pati++)
    {
      pattern_table[pati] = (pattern *)
	malloc (sizeof (struct note_info) * NOTES_PER_TRACK * song_char->nr_channels);
      for (tracki = 0; tracki < song_char->nr_channels; tracki++)
	{
	  for (notei = 0; notei < NOTES_PER_TRACK; notei++)
	    {
	      u_long params, note, effect, sample;
	      int track_num = patterns[pati].tracks[tracki];

	      if (track_num)
		{
		  track_num--;	/* convert to index */

		  params = tracks[track_num].notes[notei].argument;
		  effect = tracks[track_num].notes[notei].effect;
		  sample = tracks[track_num].notes[notei].instrument;
		  note = tracks[track_num].notes[notei].pitch;

		  if (effect == CMD_EXTENDED)
		    {
		      effect = ((CMD_EXTENDED << 4) & 0xf0) +
			((params >> 4) & 0x0f);
		      params &= 0x0f;
		    }

		  if (note)	/* note->period */
		    {
		      note = note + 3 * 12 - 1;	/* shift up 3 octaves */
		    }

		}
	      else
		{
		  params = note = effect = sample = 0;
		}

	      if (effect == CMD_SPEED)
		effect = CVT_MOD_SPEED (params);
	      else if ((effect == CMD_VOLUME) && (params > 0))
		params = (params * 4) - 1;

	      (*pattern_table[pati])[tracki][notei].note = note;
	      (*pattern_table[pati])[tracki][notei].sample = sample;
	      (*pattern_table[pati])[tracki][notei].command[0] = effect;
	      (*pattern_table[pati])[tracki][notei].parm1[0] = params;
	      (*pattern_table[pati])[tracki][notei].parm2[0] = 0;
	      (*pattern_table[pati])[tracki][notei].command[1] = 0;
	      (*pattern_table[pati])[tracki][notei].parm1[1] = 0;
	      (*pattern_table[pati])[tracki][notei].parm2[1] = 0;
	    }
	}
    }

  /* all done */
  free (samples);
  free (tracks);
  free (patterns);

  return (1);
}
