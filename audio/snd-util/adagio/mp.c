/*
 * mp (Midi Play)
 *
 * This program is largely derived from a program
 * 'mftext' by Tim Thompson, and programs 'transcribe'
 * and 'adagio' by Roger Dannenberg.  Some of the code
 * for playing the SoundBlaster card came from 'fmplay'
 * by Hannu Savolainen (hsavolai@cs.helsinki.fi).
 *
 * First, a list of midi events is built up by call-backs
 * by midifile to "txt_" functions.  Second, this list of
 * midi events is used to contruct a list of notes and
 * controls, which is, third, passed on to phase2 for
 * playing.
 *
 * For the midifile functions, see midifile.c and midifile.3.
 * For the playing functions, see phase2.c.
 *
 * (Building two intermediate representations makes the
 * program slow, but it let me use Dannenberg's code
 * with least modification.)
 *
 * (The neat function titles and comments bordered by lots
 * of asterisks are from Dannenberg.)
 *		Greg Lee, lee@uhunix.uhcc.hawaii.edu
 *		1/30/93
 */


#include <stdio.h>
#include <ctype.h>
#define NO_LC_DEFINES
#include "midifile.h"


#include "adagio.h"
#include "userio.h"
#include "cmdline.h"
#include "allphase.h"

void phase2();

/****************************************************************************
* Variables set by command line switches
****************************************************************************/

int ad_print = false;		/* adagio output */
int vverbose = false;		/* tracing output */

/****************************************************************************
* Data for command line parsing
****************************************************************************/
#define nswitches 18
static char *switches[nswitches] =
{"-help", "-v", "-vv",
 "-init", "-i",
 "-trace", "-t",
 "-external", "-e",
 "-midi", "-m",
 "-adagio", "-a",
 "-p", "-s",
 "-f", "-g", "-d"};

#define noptions 4
static char *options[noptions] =
{"-tune", "-r", "-R", "-C"};
#define n_t_sw 2
static char *t_switches[n_t_sw] =
{"-t", "-trace"};
#define n_a_sw 2
static char *a_switches[n_a_sw] =
{"-a", "-adagio"};
#define n_e_sw 2
static char *e_switches[n_e_sw] =
{"-e", "-external"};


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
    fprintf(stderr, "mp [options] filename [options]\n");
    fprintf(stderr, "	   -help	     this message\n");
    fprintf(stderr, "	   -init (-i)        initialize channels\n");
    fprintf(stderr, "	   -v    	     verbose\n");
    fprintf(stderr, "	   -vv   	     very verbose\n");
    fprintf(stderr, "	   -tune file	     use tuning from file\n");
    fprintf(stderr, "	   -trace (-t)       trace music\n");
    fprintf(stderr, "	   -adagio (-a)      adagio file on stdout\n");
    fprintf(stderr, "	   -p                piano only\n");
    fprintf(stderr, "	   -s                no solo instruments\n");
    fprintf(stderr, "	   -r  chan[,prog]   record new track\n");
    fprintf(stderr, "	   -e                no external synth\n");
    fprintf(stderr, "	   -g                no gus synth\n");
    fprintf(stderr, "	   -f                no fm synth\n");
    fprintf(stderr, "	   -d                no drum rolls\n");
    fprintf(stderr, "	   -R  reverb        midi reverberation\n");
    fprintf(stderr, "	   -C  chdepth       midi chorus depth\n");
}


/****************************************************************************
*	Routines in phasem.c
****************************************************************************/
void var_init();
void initfuncs();
void rec_init();
void card_init(char *devname);
event_type rec_final();
/*************************/

/* part of interface to midifile functions */
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

static unsigned char *midi_file_contents;
static int midi_file_size;

static int filegetc()
{
    if (midi_file_size-- > 0) return (*midi_file_contents++);
    return(EOF);
}

void txt_error(char *s)
{
    fprintf(stderr, "midifile error: %s\n", s);
    exit(1);
}

long *shm_running_time;
static long keep_running_time;

int main(argc, argv)
int argc;
char **argv;
{
    char *midi_part_name;
    char midi_name[100];
    struct stat statbuf;
    int fd;

    if (!cl_init(switches, nswitches, options, noptions, argv, argc))
	exit(1);
    if (cl_switch("-help")) {
	cmdline_help();
	return (0);
    }

    if ((midi_part_name = cl_arg(1)) == NULL) {
	fprintf(stderr, "please give a file name\n");
	exit(1);
    }
    strcpy(midi_name, midi_part_name);
    if (stat(midi_name, &statbuf)) {
	strcat(midi_name, ".mid");
	if (stat(midi_name, &statbuf)) {
	    perror(midi_name);
	    exit(1);
	}
    }
    strcpy(midi_file_path, midi_name);
    midi_file_size = statbuf.st_size;
    if ( (fd = open(midi_name, O_RDONLY, 0)) == -1 ) {
	perror(midi_name);
	exit(1);
    }
    if ( (midi_file_contents = (unsigned char *)malloc(midi_file_size)) == NULL) {
	perror("malloc");
	exit(1);
    }
    if (read(fd, midi_file_contents, midi_file_size) != midi_file_size) {
	perror("read");
	exit(1);
    }
    close(fd);
#ifdef READ_MODS
    Mf_file_contents = midi_file_contents;
    Mf_file_size = midi_file_size;
#endif

    shm_running_time = &keep_running_time;

    var_init();


    if (cl_switch("-v")) verbose = 1;
    if (cl_switch("-vv")) {
	really_verbose = 1;
	verbose = 1;
    }
    if (cl_switch("-d")) setting_drum_rolls = 0;
    piano_only = cl_switch("-p");
    exclude_fm = cl_switch("-f");
    exclude_gus = cl_switch("-g");
    no_solo = cl_switch("-s");
    extflag = (cl_nswitch(e_switches, n_e_sw) == NULL);
    ad_print = (cl_nswitch(a_switches, n_a_sw) != NULL);
    vverbose = (cl_nswitch(t_switches, n_t_sw) != NULL);
    if (cl_option("-r") != NULL)
	recording_track = true;


    card_init("/dev/sequencer");/* open /dev/sequencer and get info on devices */
    rec_init();
    initfuncs();		/* set calls to us from midifile functions */
    Mf_getc = filegetc;		/* tell midifile how to get bytes to process */
    mfread();			/* call midifile */
    free(midi_file_contents);
    if (ad_print)		/* Just translating to Adagio score? */
	(void) rec_final(true);	/* write out recorded data, */
    /* suppress time of first event*/
    else
	phase2((rec_final(true))); /* No, we're actually going to play it. */
    exit(0);
}
