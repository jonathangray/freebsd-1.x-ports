/*
 * CAUTION!	This program is just an incompletely implemented version
 *		of the patch manager daemon for GUS. Using this program
 *		with the driver version 1.99.9 will hang your system
 *		completely (sooner or later).
 *
 *		This program is for information only. The final
 *		implementation of the patch manager will not be
 *		compatible with this one.
 */

/*
 * Usage: pmgr [ -f ] [ -v || -vv ]
 *	"pmgr &"	load gus patches quietly
 *	"pmgr -f &"	load fm patches quietly
 *	"pmgr -v &"	load gus patches verbosely
 *	"pmgr -vv &"	load gus patches very verbosely
 *	  ...
 *
 * The patch manager uses cfg files and patch libraries or files in the same
 * way as ad/mp/xmp, with the exception that it does not look for cfg and
 * patch files in the same directory as the file being played.
 *					-- gl
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#ifndef __386BSD__
#include <sys/soundcard.h>
#include <sys/ultrasound.h>
#else
#include <machine/soundcard.h>
#include <machine/ultrasound.h>
#endif
#include <strings.h>
#include <sys/errno.h>

#include "adagio.h"
#include "allphase.h"
#include "cmdline.h"


/****************************************************************************
* Data for command line parsing
****************************************************************************/
#define nswitches 3
static char *switches[nswitches] =
{"-v", "-vv", "-f"};

/*
 * GM program is 0-127 for melodic voices, 128-255 for percussion (= 128
 *	plus pitch 0-127 of drum note)
 */

/*
 * load patch: in gusvoice.c; pgm is the bank (bits 22-16) and GM program;
 *	if pgm == -1, initializes
 */
int loadvoice(int dev, int pgm, int reverb, int main_volume, int voicepan);

/* initialize: in cardinit.c */
void card_init(char *devname);

/*
 * return index of GM program 'pgm' at tone bank 'bank': in gusvoice.c;
 *	if bank == 0, the index returned is equal to pgm
 */
int find_tone_bank(int dev, int pgm, int bank);

void fm_check_mode()
{
  card_info[sb_dev].device = sb_dev;
  if (ioctl(seq_fd, SNDCTL_SYNTH_INFO, &card_info[sb_dev]) == -1) {
    fprintf(stderr, "cannot get info on soundcard\n");
    perror("pmgr -f");
    exit(-1);
  }
  if (card_info[sb_dev].synth_subtype != FM_TYPE_OPL3)
	setting_4op_mode = false;
  else setting_4op_mode = (card_info[sb_dev].nr_voices == 6);
}

/*
 * interface to loadvoice() routine:
 *	get requested GM program and GS tone bank out of patmgr_info structure;
 *	call loadvoice() to load the patch;
 *	return the patch slot number and tone bank in the patmgr_info structure
 *	  (tone bank of patch may be different from that requested)
 */
int
do_load_patch (int dev, struct voice_type *voice, struct patmgr_info *rec)
{
  int pgm, tpgm, ret, bank;
  char *pname;

  pgm = rec->data.data8[0];
  bank = (rec->parm1) & 0x7f;

  if (dev == sb_dev) fm_check_mode();

  ret = loadvoice(dev, (bank << 16) | pgm, 0, 127, 64);

  tpgm = find_tone_bank(dev, pgm, bank);

  rec->data.data8[1] = voice[tpgm].prog;
  rec->parm1 = voice[tpgm].bank;

  if (verbose) {
    if (voice[tpgm].vname == NULL) pname = gm_voice[pgm].vname;
    else pname = voice[tpgm].vname;
    if (pname == NULL) pname = "unnamed voice";
    if (ret) printf ("PM: Loaded: %dop %s to slot %d\n", setting_4op_mode? 4:2,
	pname, rec->data.data8[1]);
    else printf("PM: pgm %s[%d] not loaded\n", pname, pgm);
  }

  return( !ret );
}

static int manage_fm = 0;
static int manage_gus = 1;
static int manage_dev;

int main(argc, argv)
int argc;
char **argv;
{
  struct patmgr_info inf;
  int             err;
  struct voice_type *voice;

  cl_init(switches, nswitches, NULL, 0, argv, argc);

  if (cl_switch("-v")) verbose = 1;
  if (cl_switch("-vv")) {
	really_verbose = 1;
	verbose = 1;
  }
  manage_fm = cl_switch("-f");

  /* signal no midi file known, so cfg file interpreter knows not
   * to look for a cfg file in the same directory as midi file
   */
  strcpy(midi_file_path, "*");

  if (manage_fm) {
	manage_gus = 0;
	voice = fm_voice;
  	card_init("/dev/patmgr1");
	if (sb_dev == -1) {
	    fprintf (stderr, "Error: FM card not detected\n");
	    exit (-1);
	}
	manage_dev = sb_dev;
  }
  else {
	card_init("/dev/patmgr0");
	voice = gus_voice;
	if (gus_dev == -1) {
	    fprintf (stderr, "Error: Gravis Ultrasound not detected\n");
	    exit (-1);
	}
	manage_dev = gus_dev;
  }

  while (1)
    {
      if (read (seq_fd, (char *) &inf, sizeof (inf)) != sizeof (inf))
	{
	  perror ("Read");
	  exit (-1);
	}

      if (inf.key == PM_K_EVENT)
	switch (inf.command)
	  {
	  case PM_E_OPENED:
	    if (verbose) printf ("Opened\n");
	    break;

	  case PM_E_CLOSED:
	    if (verbose) printf ("Closed\n");
/** Don't do this; application may open /dev/sequencer again to use
    patches that were previously loaded.
	    if (manage_gus) {
	      if (ioctl (seq_fd, SNDCTL_SEQ_RESETSAMPLES, &gus_dev) == -1)
	        perror ("Sample reset");
	    }
	    (void)loadvoice(-1, -1, -1, 0, 0);
**/
	    break;

	  case PM_E_PATCH_RESET:
	    if (verbose) printf ("Patch reset called\n");
	    (void)loadvoice(-1, -1, -1, 0, 0);
	    break;

	  case PM_E_PATCH_LOADED:
	    if (verbose) printf ("Patch loaded by client\n");
	    break;

	  default:
	    if (verbose) printf ("Unknown event %d\n", inf.command);
	    inf.key = PM_ERROR;
	    inf.parm1 = EINVAL;
	  }
      else if (inf.key == PM_K_COMMAND)
	switch (inf.command)
	  {
	  case _PM_LOAD_PATCH:
	    if ((err = do_load_patch (manage_dev, voice, &inf)))
	      if (err == ENOSPC)
		{
		  if (manage_gus)
		    if (ioctl (seq_fd, SNDCTL_SEQ_RESETSAMPLES, &gus_dev) == -1)
		      {
		        perror ("Sample reset");
		        return errno;
		      }

	          (void)loadvoice(-1, -1, -1, 0, 0);
		  err = do_load_patch (manage_dev, voice, &inf);
		}

	    if (err)
	      {
		inf.key = PM_ERROR;
		inf.parm1 = err;
		if (verbose) printf("Error = %d\n", err);
	      }
	    else
	     {
	        inf.key = PM_K_COMMAND;
	        inf.parm1 = 0;
	     }
	    break;

	  default:
	    if (verbose) printf ("Unknown command %d\n", inf.command);
	    inf.key = PM_ERROR;
	    inf.parm1 = EINVAL;
	  }
      else
	{
	  if (verbose) printf ("Unknown event %d/%d\n", inf.key, inf.command);
	  inf.key = PM_ERROR;
	  inf.parm1 = EINVAL;
	}

      if (write (seq_fd, (char *) &inf, sizeof (inf)) != sizeof (inf))
	{
	  perror ("write");
	  exit (-1);
	}
    }

  exit (0);
}
