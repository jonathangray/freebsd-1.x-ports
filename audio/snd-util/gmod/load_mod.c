
/*
*	gmod.c	- Module player for GUS and Linux.
*		(C) Hannu Savolainen, 1993
*
*	NOTE!	This program doesn't try to be a complete module player.
*		It's just a too I used while developing the driver. In
*		addition it can be used as an example on programming
*		the VoxWare Sound Driver with GUS.
*/

/*
* Many modifications have been done by Andrew J. Robinson.
* Refer to the ChangeLog for details.
*/


#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <machine/soundcard.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"


int
load_mod_module (FILE * mod_fd, char *name, struct song_info *song_char,
		 struct options_info options)
{

  struct sample_header
    {
      char name[22];
      unsigned short length;	/* In words */

      unsigned char finetune;
      unsigned char volume;

      unsigned short repeat_point;	/* In words */
      unsigned short repeat_length;	/* In words */
    };

  int i, total_mem;
  int gus_onboard_mem = gus_mem_free (gus_dev);
  int sample_ptr, pattern_loc;

  int position;

  unsigned char *tune_ptr;	/* array 0-127 */

  char header[1084];

  int nr_samples;		/* 16 or 32 samples */
  int slen, npat;
  int pat_size;
  char mname[23];
  int bend;

  fprintf (stderr, "Loading .MOD module: %s\n", name);

  if (fread (header, 1, sizeof (header), mod_fd) != sizeof (header))
    {
      fprintf (stderr, "Short header\n");
      return 0;
    }

  strncpy (mname, header, 22);
  fprintf (stderr, "\nModule: %s - ", mname);

  if (!strncmp (&header[1080], "M.K.", 4))
    {
      nr_samples = 31;
      song_char->nr_channels = 4;
    }
  else if (!strncmp (&header[1080], "FLT", 3))
    {
      nr_samples = 31;
      song_char->nr_channels = header[1083] - 48;
    }
  else if (!strncmp (&header[1081], "CHN", 3))
    {
      nr_samples = 31;
      song_char->nr_channels = header[1080] - 48;
    }
  else if (!strncmp (&header[1080], "OCTA", 4))
    {
      nr_samples = 31;
      song_char->nr_channels = 8;
    }
  else
    {
      nr_samples = 15;
      song_char->nr_channels = 4;
    }

  printf ("%d samples/%d channels\n", nr_samples, song_char->nr_channels);

  song_char->lowest_note = 48;
  song_char->highest_note = 84;
  song_char->play_speed = 6;
  song_char->vol_type = VOL_LINEAR;

  for (i = 0; i < song_char->nr_channels; i++)
    song_char->panning[i] = panning (i);

  pat_size = 64 * song_char->nr_channels * 4;

  if (nr_samples == 31)
    {
      sample_ptr = pattern_loc = 1084;
      slen = header[950];
      tune_ptr = (unsigned char *) &header[952];
    }
  else
    {
      sample_ptr = pattern_loc = 600;
      slen = header[470];
      tune_ptr = (unsigned char *) &header[472];
    }

  npat = 0;
  for (i = 0; i < 128; i++)
    {
      tune[i] = tune_ptr[i];

      if (tune_ptr[i] > npat)
	npat = tune_ptr[i];
    }
  npat++;

  fprintf (stderr, "Song length %d, %d patterns.\n", slen, npat);

  sample_ptr += (npat * pat_size);	/* Location where the first sample is stored */
  total_mem = 0;

  for (i = 0; i < 32; i++)
    sample_ok[i] = 0;

  for (i = 0; i < nr_samples; i++)
    {
      int len, loop_start, loop_end;
      int mem_avail;
      unsigned short loop_flags = 0;
      char pname[23];		/* changed from [22] by AJR */

      struct sample_header *sample;

      struct patch_info *patch;

      sample = (struct sample_header *) &header[20 + (i * 30)];

      len = intelize (sample->length) * 2;
      loop_start = intelize (sample->repeat_point) * 2;
      loop_end = loop_start + (intelize (sample->repeat_length) * 2 - 1);

      if (loop_start > len)
	loop_start = 0;
      if (loop_end > len)
	loop_end = len;

      if (loop_end <= loop_start)
	loop_end = loop_start + 1;

      if (loop_end > 1 && loop_end > loop_start)
	loop_flags = WAVE_LOOPING;

      strncpy (pname, sample->name, 22);
      pname[22] = '\0';

      if (len > 0 || options.show_empty_samples)
	fprintf (stderr, "Sample %02d: L%06d, S%06d, E%06d V%02d %s\n",
		 i,
		 len,
		 loop_start,
		 loop_end,
		 sample->volume,
		 pname);

      if (len > 0)
	{
	  total_mem += len;

	  patch = (struct patch_info *) malloc (sizeof (*patch) + len);

	  patch->key = GUS_PATCH;
	  patch->device_no = gus_dev;
	  patch->instr_no = i;
	  patch->mode = loop_flags;
	  patch->len = len;
	  patch->loop_start = loop_start;
	  patch->loop_end = loop_end;
	  patch->base_note = 261630;	/* Middle C */
	  patch->base_freq = base_freq_table[sample->finetune & 0xf];
	  patch->low_note = 0;
	  patch->high_note = 20000000;
	  patch->volume = 0 /* changed from 120 by AJR */ ;
	  patch->panning = 16;

	  if (fseek (mod_fd, sample_ptr, SEEK_SET) == -1)
	    {
	      perror (name);
	      free (patch);
	      return 0;
	    }

	  sample_ptr += len;

	  if (fread (patch->data, 1, len, mod_fd) != len)
	    {
	      fprintf (stderr, "Short file (sample) %d\n", sample_ptr);
	      free (patch);
	      return 0;
	    }

	  /* try to remove loop clicking */

	  if ((loop_flags & WAVE_LOOPING) && (loop_end >= 2))
	    {
	      patch->data[loop_end] = patch->data[loop_start];
	      patch->data[loop_end - 1] =
		((signed char) (patch->data[loop_end - 2]) +
		 (signed char) (patch->data[loop_end])) / 2;
	    }

	  mem_avail = gus_mem_free (gus_dev);	/* XXX */

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
	      if (sample->volume > 0)
		sample_vol[i] = (sample->volume * 4) - 1;
	      else
		sample_vol[i] = 0;
	    }

	  free (patch);
	}
    }

  printf ("=== %d bytes of %d total onboard used ===\n",
	  gus_onboard_mem - gus_mem_free (gus_dev), gus_onboard_mem);

  song_char->nr_patterns = npat;
  song_char->songlength = slen;

  for (position = 0; position < npat; position++)
    {
      unsigned char patterns[64][song_char->nr_channels][4];
      int pat, channel;

      int pp = pattern_loc + (position * pat_size);

      if (fseek (mod_fd, pp, SEEK_SET) == -1)
	{
	  perror (name);
	  return 0;
	}

      if (fread (patterns, 1, pat_size, mod_fd) != pat_size)
	{
	  fprintf (stderr, "Short file (pattern %d) %d\n", tune[position], pp);
	  return 0;
	}

      if ((pattern_table[position] = (pattern *) malloc (sizeof (struct note_info) * 64 * song_char->nr_channels)) == NULL)
	{
	  fprintf (stderr, "Can't allocate memory for a pattern\n");
	  return 0;
	}

      for (pat = 0; pat < 64; pat++)
	{
	  for (channel = 0; channel < song_char->nr_channels; channel++)
	    {
	      unsigned short tmp;
	      unsigned char *p;

	      unsigned period, sample, effect, params, note;

	      p = &patterns[pat][channel][0];

	      tmp = (p[0] << 8) | p[1];
	      sample = (tmp >> 8) & 0x10;
	      period =
		MIN (tmp & 0xFFF, 1023);
	      tmp = (p[2] << 8) | p[3];
	      sample |= tmp >> 12;
	      effect = (tmp >> 8) & 0xF;
	      params = tmp & 0xFF;

	      if (effect == CMD_EXTENDED)
		{
		  effect = ((CMD_EXTENDED << 4) & 0xf0) +
		    ((params >> 4) & 0x0f);
		  params &= 0x0f;
		}

	      note = 0;

	      if (period)
		{
		  /* Convert period to a Midi note number */

		  period_to_note (period, &note, &bend);

		  if (note < song_char->lowest_note)
		    note = song_char->lowest_note;
		  else if (note > song_char->highest_note)
		    note = song_char->highest_note;
		}

	      if (effect == CMD_SPEED)
		effect = CVT_MOD_SPEED (params);
	      else if ((effect == CMD_VOLUME) && (params > 0))
		params = (params * 4) - 1;

	      (*pattern_table[position])[channel][pat].note = note;
	      (*pattern_table[position])[channel][pat].sample = sample;
	      (*pattern_table[position])[channel][pat].command[0] = effect;
	      (*pattern_table[position])[channel][pat].parm1[0] = params;
	      (*pattern_table[position])[channel][pat].parm2[0] = 0;
	      (*pattern_table[position])[channel][pat].command[1] = 0;
	      (*pattern_table[position])[channel][pat].parm1[1] = 0;
	      (*pattern_table[position])[channel][pat].parm2[1] = 0;
	    }
	}
    }
  return 1;
}

int
load_module (char *name, struct song_info *song_char,
	     struct options_info options)
{

  int i;

  char header[0x30];

  int ret_val;

  FILE *mod_fd;

  ioctl (seqfd, SNDCTL_SEQ_SYNC, 0);
  ioctl (seqfd, SNDCTL_SEQ_RESETSAMPLES, &gus_dev);

  clock_rate = 50.0;

  for (i = 0; i < MAX_POSITION; i++)
    pattern_len[i] = 64;

  for (i = 0; i < MAX_POSITION; i++)
    pattern_tempo[i] = 0;

  for (i = 0; i < MAX_SAMPLES; i++)
    sample_ok[i] = 0;

  if ((mod_fd = fopen (name, "rb")) == NULL)
    {
      perror (name);
      return 0;
    }

  if (fread (header, 1, sizeof (header), mod_fd) != sizeof (header))
    {
      fprintf (stderr, "%s: Short file (header)\n", name);
      fclose (mod_fd);
      return 0;
    }

  if (fseek (mod_fd, 0, SEEK_SET) == -1)
    {
      perror (name);
      fclose (mod_fd);
      return 0;
    }

  if (*(unsigned short *) &header[0] == 0x6669)
    ret_val = load_669_module (mod_fd, name, song_char, options);

  else if (!strncmp (header, "MTM", 3))
    ret_val = load_mtm_module (mod_fd, name, song_char, options);

  else if (!strncmp (header, "MAS_UTrack_V", 12))
    ret_val = load_ult_module (mod_fd, name, song_char, options);

  else if (!strncmp (&header[0x2c], "SCRM", 4) &&
	   (INTEL_SHORT (&header[0x28]) & 0xfff) >= 0x300)
    ret_val = load_s3m_module (mod_fd, name, song_char, options);

  else
    ret_val = load_mod_module (mod_fd, name, song_char, options);

  fclose (mod_fd);
  return (ret_val);
}
