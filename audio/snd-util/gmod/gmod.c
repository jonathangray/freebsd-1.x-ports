


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
#include <machine/ultrasound.h>
#include <stdio.h>

#include "defines.h"
#include "structs.h"
#include "tables.h"
#include "protos.h"

SEQ_DEFINEBUF (2048);

int pattern_len[MAX_POSITION];
int pattern_tempo[MAX_POSITION];
pattern *pattern_table[MAX_PATTERN];

struct voice_info voices[MAX_TRACK];

int tune[MAX_POSITION];
double tick_duration;

int seqfd;
int sample_ok[MAX_SAMPLES], sample_vol[MAX_SAMPLES];
int tmp, gus_dev;
double this_time, next_time;
int ticks_per_division;
double clock_rate;		/* HZ */

int
main (int argc, char *argv[])
{
  int i, n, j, name_start;
  struct synth_info info;
  struct song_info song_char;
  struct options_info options =
  {255, 0, 0};

  printf ("GMOD version 1.1\n");
  printf ("Original source (C) Hannu Savolainen, 1993\n");
  printf ("Modifications by Andrew J. Robinson and Robert Sanders\n\n");

  name_start = parse_args (argc, argv, &options);

  if (name_start == argc)
    {
      printf ("\nUsage: %s [options] modfile . . .\n", argv[0]);
      printf ("Use %s -h for help.\n\n", argv[0]);
      exit (50);
    }

  if ((seqfd = open ("/dev/sequencer", O_WRONLY, 0)) == -1)
    {
      perror ("/dev/sequencer");
      exit (-1);
    }

  if (ioctl (seqfd, SNDCTL_SEQ_NRSYNTHS, &n) == -1)
    {
      perror ("/dev/sequencer");
      exit (-1);
    }

  for (i = 0; i < n; i++)
    {
      info.device = i;

      if (ioctl (seqfd, SNDCTL_SYNTH_INFO, &info) == -1)
	{
	  perror ("/dev/sequencer");
	  exit (-1);
	}

      if (info.synth_type == SYNTH_TYPE_SAMPLE
	  && info.synth_subtype == SAMPLE_TYPE_GUS)
	gus_dev = i;
    }

  if (gus_dev == -1)
    {
      fprintf (stderr, "Gravis Ultrasound not detected\n");
      exit (-1);
    }

  GUS_NUMVOICES (gus_dev, 32);

  for (j = 0; j < 32; j++)
    {
      GUS_VOICEOFF (gus_dev, j);
      GUS_RAMPOFF (gus_dev, j);
    }
  SEQ_DUMPBUF ();

  for (i = name_start; i < argc; i++)
    {
      for (j = 0; j < MAX_PATTERN; j++)
	pattern_table[j] = NULL;

      GUS_NUMVOICES (gus_dev, 32);

      for (j = 0; j < 32; j++)
	{
	  GUS_VOICEOFF (gus_dev, j);
	  GUS_RAMPOFF (gus_dev, j);
	}
      SEQ_DUMPBUF ();


      if (load_module (argv[i], &song_char, options))
	{
	  tick_duration = 100.0 / clock_rate;

	  ioctl (seqfd, SNDCTL_SEQ_SYNC, 0);

	  if (song_char.nr_channels < 14)
	    {
	      GUS_NUMVOICES (gus_dev, 14);
	    }
	  else
	    {
	      GUS_NUMVOICES (gus_dev, song_char.nr_channels);
	    }
	  SEQ_DUMPBUF ();
	  ioctl (seqfd, SNDCTL_SEQ_SYNC, 0);

	  play_module (argv[i], &song_char, options);
	}

    }

  SEQ_DUMPBUF ();
  close (seqfd);

  exit (0);
}
