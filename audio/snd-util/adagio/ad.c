/*   Adagio compiler -- scan command line, call phase1, phase2 */

/*****************************************************************************
*	    Change Log
*  Date	    | Change
*-----------+-----------------------------------------------------------------
* 12-Mar-86 | Created
* 28-May-86 | Added calls to cmdline.c for command line parsing
*****************************************************************************/

#include <stdio.h>
#include "adagio.h"
#include "userio.h"
#include "cmdline.h"
#include "allphase.h"

event_type phase1();
void phase2();
void card_init(char *devname);

extern int verbose;
extern int piano_only;
extern int recording_track;
char score_na[255];  /* this will be available to phase1 and 2 */

/****************************************************************************
* Variables set by command line switches
****************************************************************************/

/****************************************************************************
* Data for command line parsing
****************************************************************************/
#define nswitches 15
static char *switches[nswitches] = 
    { "-help", "-v", "-vv",
      "-init", "-i",
      "-trace", "-t",
      "-external", "-e",
      "-midi", "-m",
      "-p", "-s",
      "-f", "-g" };

#define noptions 4
static char *options[noptions] = { "-tune", "-r", "-R", "-C" };
#define n_e_sw 2
static char *e_switches[n_e_sw] = { "-e", "-external" };

/****************************************************************************
*	Routines local to this module
****************************************************************************/
static void cmdline_help();

/****************************************************************************
*				 cmdline_help
* Effect: 
*	Prints out command line help
****************************************************************************/

static void cmdline_help()
{
    fprintf(stderr,"ad [options] filename [options]\n");
    fprintf(stderr,"	   -help	     this message\n");
    fprintf(stderr,"	   -init (-i)        initialize channels\n");
    fprintf(stderr,"	   -v    	     verbose\n");
    fprintf(stderr,"	   -vv   	     very verbose\n");
    fprintf(stderr,"	   -tune file	     use tuning from file\n");
    fprintf(stderr,"	   -trace (-t)       trace music\n");
    fprintf(stderr,"	   -midi (-m)        midi file on stdout\n");
    fprintf(stderr,"	   -p                piano only\n");
    fprintf(stderr, "	   -s                no solo instruments\n");
    fprintf(stderr, "	   -r  chan[,prog]   record new track\n");
    fprintf(stderr, "	   -e                no external synth\n");
    fprintf(stderr, "	   -g                no gus synth\n");
    fprintf(stderr, "	   -f                no fm synth\n");
}

/****************************************************************************
*				     main
* Inputs:
*	int argc: Count of arguments
*	char * argv[]: Vector of argument pointers
* Effect: 
*	Runs adagio
****************************************************************************/

void main(argc,argv)
   int argc;
   char * argv[];
{
    FILE *fp = NULL;
    char *s;	/* temp pointer to input file name */

    if (!cl_init(switches, nswitches, options, noptions, argv, argc))
	exit(1);

    score_na[0] = 0;

    if (cl_switch("-help")) {
	cmdline_help(); 
	return;
    }
    verbose = cl_switch("-v");
    really_verbose = cl_switch("-vv");
    if (really_verbose) verbose = really_verbose;
    piano_only = cl_switch("-p");
    exclude_fm = cl_switch("-f");
    exclude_gus = cl_switch("-g");
    no_solo = cl_switch("-s");
    extflag = (cl_nswitch(e_switches, n_e_sw) == NULL);
    if (cl_option("-r") != NULL) recording_track = true;

    /* get input file: */
    if ((s = cl_arg(1)) != NULL) strcpy(score_na, s);
    fp = fileopen(score_na, "gio", "r", "Name of Adagio score file");

    if (!cl_switch("-midi") && !cl_switch("-m"))
	card_init("/dev/sequencer");
    else {
	sb_dev = 0;
	card_info[0].nr_voices = 18;
    }

    phase2(phase1(fp));
}
