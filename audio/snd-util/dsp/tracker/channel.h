/* channel.h 
	vi:se ts=3 sw=3:
 */

/* $Id: channel.h,v 1.1 1994/02/19 16:03:07 ache Exp $
 * $Log: channel.h,v $
 * Revision 1.1  1994/02/19 16:03:07  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:43:33  espie
 * Added prototypes.
 *
 * Revision 1.3  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1994/01/05  01:59:14  Espie
 * Added prototypes.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.9  1993/11/17  15:31:16  espie
 * audio_channel private.
 *
 * Revision 3.8  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.7  1993/07/18  10:39:44  espie
 * Cleaned up.
 *
 * Revision 3.6  1993/05/19  11:26:39  espie
 * *** empty log message ***
 *
 * Revision 3.5  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.4  1992/11/23  10:12:23  espie
 * *** empty log message ***
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Simplified delay_pattern.
 *
 * Revision 3.2  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.7  1992/11/13  13:24:24  espie
 * Added parameters for extended Retriger command.
 * Added transpose feature.
 * Structured part of the code, especially replay ``automaton''
 * and setting up of effects.
 *
 * Revision 1.5  1991/11/16  16:54:19  espie
 * Bug correction: when doing arpeggio, there might not
 * be a new note, so we have to save the old note value
 * and do the arppeggio on that note.
 * Added fields for arpeggio.
 */

     
#ifndef NUMBER_PATTERNS
#define NUMBER_PATTERNS 128
#endif

#define MAX_ARP 3
     
/* there is no note in each channel initially.
 * This is defensive programming, because some
 * commands rely on the previous note. Checking
 * that there was no previous note is a way to
 * detect faulty modules.
 */
#define NO_NOTE 255

struct channel
   {
   struct sample_info *samp;
   struct audio_channel *audio;
   int finetune;
   int volume;             /* current volume of the sample (0-64) */
   int pitch;              /* current pitch of the sample */
   int note;               /* we have to save the note cause */
                           /* we can do an arpeggio without a new note */
    
   int arp[MAX_ARP];       /* the three pitch values for an arpeggio */
   int arpindex;           /* an index to know which note the arpeggio is doing */

   int viboffset;          /* current offset for vibrato (if any) */
   int vibdepth;           /* depth of vibrato (if any) */

   int slide;              /* step size of pitch slide */

   int pitchgoal;          /* pitch to slide to */
   int pitchrate;          /* step rate for portamento */

   int volumerate;         /* step rate for volume slide */

   int vibrate;            /* step rate for vibrato */

   int retrig;             /* delay for extended retrig command */
   int current;
                           /* current command to adjust parameters */
   void (*adjust) P((struct channel *ch));
   };

#define DO_NOTHING 0 
#define SET_SPEED 1
#define SET_SKIP 2
#define SET_FASTSKIP 4
#define SET_FINESPEED 32

#define JUMP_PATTERN 8
#define DELAY_PATTERN 16

#define NORMAL_SPEED 6
#define NORMAL_FINESPEED 125

struct automaton
   {
   int pattern_num;           /* the pattern in the song */
   int note_num;              /* the note in the pattern */
   struct block *pattern;     /* the physical pattern */
   struct song_info *info;    /* we need the song_info */

   char gonethrough[NUMBER_PATTERNS + 1];  /* to check for repeats */

   int counter;               /* the fine position inside the effect */
   int speed;                 /* the `speed', number of effect repeats */
   int finespeed;             /* the finespeed, base is 100 */

   int do_stuff;              /* keeping some stuff to do */
                              /* ... and parameters for it: */
   int new_speed, new_note, new_pattern, new_finespeed;

   int pitch, note, para;     /* some extra parameters effects need */

   int loop_note_num, loop_counter;
                              /* for command E6 */
   };
