
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
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/soundcard.h>
#include <sys/ultrasound.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

int
load_669_module (FILE * mod_fd, char *name, struct song_info *song_char,
		 struct options_info options)
{
  struct sample_header
    {
      char name[13];
      unsigned long length;	/* In bytes */
      unsigned long loop_start;
      unsigned long loop_end;
    };

  int i, total_mem;
  int gus_onboard_mem;
  int sample_ptr;

  int position;

  unsigned char *tune_ptr, *len_ptr, *tempo_ptr;	/* array 0-127 */

  char header[2097];		/* changed from 1084 */
  char msg[110];

  int nr_samples;		/* 16 or 32 samples */
  int slen, npat;

  gus_onboard_mem = gus_mem_free (gus_dev);

  clock_rate = /* 25.0 */ 30.0;

  fprintf (stderr, "Loading .669 module: %s\n", name);

  if (fread (header, 1, sizeof (header), mod_fd) != sizeof (header))
    {
      fprintf (stderr, "%s: Short file (header)\n", name);
      return 0;
    }

  if (*(unsigned short *) &header[0] != 0x6669)
    {
      fprintf (stderr, "Not a 669 file\n");
      return 0;
    }

  strncpy (msg, &header[2], 108);

  for (i = 0; i < strlen (msg); i++)
    if ((msg[i] >= ' ' && msg[i] <= 'z') || msg[i] == '\n')
      printf ("%c", msg[i]);
  printf ("\n");

  npat = header[0x6f];

  tune_ptr = &header[0x71];

  for (slen = 0; slen < 128 && tune_ptr[slen] != 0xff; slen++);
  /* slen--; */

  for (i = 0; i < slen; i++)
    tune[i] = tune_ptr[i];

  len_ptr = &header[0x171];
  for (i = 0; i < 0x80; i++)
    pattern_len[i] = len_ptr[i] + 1;	/* AJR: changed from -1 */

  tempo_ptr = &header[0xf1];
  for (i = 0; i < slen; i++)
    pattern_tempo[i] = tempo_ptr[i];

  nr_samples = header[0x6e];

  fprintf (stderr, "Song length %d, %d patterns, %d samples.\n", slen, npat, nr_samples);

  sample_ptr = 0x1f1 + (nr_samples * 0x19) + (npat * 0x600);	/* Location where the
							 * first sample is
							 * stored */
  total_mem = 0;

  for (i = 0; i < MAX_SAMPLES; i++)
    sample_ok[i] = 0;

  for (i = 0; i < nr_samples; i++)
    {
      unsigned long len, loop_start, loop_end;
      int mem_avail;
      unsigned short loop_flags = 0;

      struct sample_header *sample;
      char sname[14];

      struct patch_info *patch;

      sample = (struct sample_header *) &header[0x1f1 + (i * 0x19)];

      len = *(unsigned long *) &sample->name[13];
      loop_start = *(unsigned long *) &sample->name[17];
      loop_end = *(unsigned long *) &sample->name[21];
      if (loop_end > len)
	loop_end = 1;
      else if (loop_end == len)
	loop_end--;

      if (loop_end < loop_start)
	{
	  loop_start = 0;
	  loop_end = 0;
	}

      strncpy (sname, sample->name, 13);


      if ((len > 0) || options.show_empty_samples)
	fprintf (stderr, "Sample %02d: %05lu, %05lu, %05lu   %s\n",
		 i,
		 len,
		 loop_start,
		 loop_end,
		 sname);

      if (len > 0)
	{
	  total_mem += len;

	  patch = (struct patch_info *) malloc (sizeof (*patch) + len);

	  if (loop_end == 0)
	    loop_end = 1;
	  if (loop_end >= len)
	    loop_end = 1;

	  if (loop_end > 1)
	    loop_flags = WAVE_LOOPING;

	  patch->key = GUS_PATCH;
	  patch->device_no = gus_dev;
	  patch->instr_no = i;
	  patch->mode = WAVE_UNSIGNED | loop_flags;
	  patch->len = len;
	  patch->loop_start = loop_start;
	  patch->loop_end = loop_end;
	  patch->base_freq = 8448;
	  patch->base_note = 261630;
	  patch->low_note = 1000;
	  patch->high_note = 0x7fffffff;
	  patch->volume = 0;	/* 120; */
	  patch->panning = 16;
	  sample_vol[i] = 0;

	  if (fseek (mod_fd, sample_ptr, SEEK_SET) == -1)
	    {
	      fprintf (stderr, "Seek failed\n");
	      perror (name);
	      free (patch);
	      return 0;
	    }

	  sample_ptr += len;

	  if (fread (patch->data, 1, len, mod_fd) != len)
	    {
	      fprintf (stderr, "Short file (sample at %d)\n", sample_ptr);
	      free (patch);
	      return 0;
	    }

	  /* try to remove loop clicking */

	  if ((loop_flags & WAVE_LOOPING) && (loop_end >= 2))
	    {
	      patch->data[loop_end] = patch->data[loop_start];
	      patch->data[loop_end - 1] =
		((unsigned char) (patch->data[loop_end - 2]) +
		 (unsigned char) (patch->data[loop_end])) / 2;
	    }

	  mem_avail = gus_mem_free (gus_dev);

	  if (mem_avail < len)
	    {
	      printf ("*** Skipping patch - %lu needed, %d available ***\n",
		      len, mem_avail);
	      sample_ok[i] = 0;
	    }
	  else
	    {
	      SEQ_WRPATCH (patch, sizeof (*patch) + len);
	      sample_ok[i] = 1;
	      sample_vol[i] = 0;
	    }

	  free (patch);
	}
    }


  printf ("=== %d bytes of %d total onboard used ===\n",
	  gus_onboard_mem - gus_mem_free (gus_dev), gus_onboard_mem);

  song_char->nr_patterns = slen;
  song_char->songlength = slen;
  song_char->nr_channels = 8;
  song_char->lowest_note = 36;
  song_char->highest_note = 99;
  song_char->play_speed = 4;
  song_char->vol_type = VOL_LINEAR;

  /* set panning */

  for (i = 0; i < song_char->nr_channels; i++)
    song_char->panning[i] = panning (i);

  for (position = 0; position < npat; position++)
    {
      unsigned char patterns[0x600];
      int pat, channel, x;

      int pp = 0x1f1 + (nr_samples * 0x19) + (position * 0x600);

      if ((pattern_table[position] = (pattern *) malloc (sizeof (struct note_info) * 64 * song_char->nr_channels)) == NULL)
	{
	  fprintf (stderr, "Can't allocate memory for a pattern\n");
	  return 0;
	}

      if (fseek (mod_fd, pp, SEEK_SET) == -1)
	{
	  perror (name);
	  return 0;
	}

      if ((x = fread (patterns, 1, 0x600, mod_fd)) != 0x600)
	{
	  fprintf (stderr, "Short file (pattern at %d) %d!=1024\n", pp, x);
	  return 0;
	}

      for (pat = 0; pat < 64; pat++)
	{

	  for (channel = 0; channel < 8; channel++)
	    {
	      unsigned char *p;

	      unsigned vol, period, sample, effect, params;
	      unsigned effect1 = 0, effect2 = 0, parm1 = 0, parm2 = 0;

	      p = &patterns[pat * 24 + channel * 3];

	      if (p[0] >= 0xfe)
		{
		  sample = 0;
		  period = 0;
		}
	      else
		{
		  period = (p[0] >> 2) + 36;	/* AJR:  changed from 48 */
		  sample = (((p[0] << 4) & 0x30) | ((p[1] >> 4) & 0x0f)) + 1;
		}

	      effect = (p[2] >> 4);
	      params = p[2] & 0x0f;
	      vol = (p[1] & 0x0f) * 17;

	      if (period)
		{
		  effect1 = CMD_VOLUME;
		  parm1 = vol;
		}

	      if (p[0] == 0xfe)
		{
		  effect1 = CMD_VOLUME;
		  parm1 = vol;
		}

	      if (p[2] == 0xff)
		{
		  effect2 = CMD_NOP;
		}
	      else
		switch (effect)
		  {
		  case 0:	/* a - Portamento up */
		    effect2 = /* CMD_SLIDEUP*/ CMD_NOP;
		    break;

		  case 1:	/* b - Portamento down */
		    effect2 = /* CMD_SLIDEDOWN */ CMD_NOP;
		    break;

		  case 2:	/* c - Port to note */
		    effect2 = /* CMD_SLIDETO */ CMD_NOP;
		    break;

		  case 3:	/* d - Frequency adjust */
		    effect2 = CMD_NOP;	/* To be implemented */
		    break;

		  case 4:	/* e - Frequency vibrato */
		    effect2 = CMD_NOP;	/* To be implemented */
		    break;

		  case 5:	/* f - Set tempo */
		    effect2 = CMD_SET_TICKS;
		    parm2 = params;
		    break;

		  default:
		    effect2 = CMD_NOP;
		  }

	      if (period)
		if (period < song_char->lowest_note)
		  period = song_char->lowest_note;
		else if (period > song_char->highest_note)
		  period = song_char->highest_note;

	      (*pattern_table[position])[channel][pat].note = period;
	      (*pattern_table[position])[channel][pat].sample = sample;
	      (*pattern_table[position])[channel][pat].command[0] = effect1;
	      (*pattern_table[position])[channel][pat].parm1[0] = parm1;
	      (*pattern_table[position])[channel][pat].parm2[0] = 0;
	      (*pattern_table[position])[channel][pat].command[1] = effect2;
	      (*pattern_table[position])[channel][pat].parm1[1] = parm2;
	      (*pattern_table[position])[channel][pat].parm2[1] = 0;
	    }

	}

    }
  return 1;
}
