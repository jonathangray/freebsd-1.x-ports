/* main.c 
	vi:se ts=3 sw=3:
 */

/* plays sound/noisetracker files on Sparc, silicon graphics.
 * Authors  : Liam Corner - zenith@dcs.warwick.ac.uk
 *            Marc Espie - espie@ens.fr
 *            Steve Haehnichen - shaehnic@ucsd.edu
 *            Andrew Leahy - alf@st.nepean.uws.edu.au
 *
 * Usage    : tracker <filename> 
 *  this version plays compressed files as well.
 */

/* $Id: main.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: main.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.3  1994/02/04  14:53:03  espie
 * Nice OPT_CUT/OPT_ADD.
 *
 * Revision 4.2  1994/01/13  09:16:00  espie
 * *** empty log message ***
 *
 * Revision 1.10  1994/01/12  16:12:34  espie
 * Last minute changes.
 *
 * Revision 4.1  1994/01/12  16:11:07  espie
 * Last minute changes.
 *
 * Revision 4.0  1994/01/11  17:49:27  espie
 * Base main.c for unix and others.
 *
 * Revision 1.11  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.10  1994/01/09  04:50:56  Espie
 * ?
 *
 * Revision 1.9  1994/01/08  03:55:43  Espie
 * Slight input/output changes.
 * No more call to create_notes_table().
 *
 * Revision 1.8  1994/01/08  02:04:21  Espie
 * Some notice to status.
 *
 * Revision 1.7  1994/01/06  22:32:42  Espie
 * Use new pref scheme.
 *
 * Revision 1.6  1994/01/05  16:10:49  Espie
 * *** empty log message ***
 *
 * Revision 1.5  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.4  1994/01/05  13:50:43  Espie
 * Cosmetic change.
 *
 * Revision 1.3  1993/12/28  13:54:44  Espie
 * Use info facility instead of printf for usage message.
 *
 * Revision 1.2  1993/12/26  18:54:21  Espie
 * Modified in a more consistent way.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.20  1993/12/04  17:15:18  espie
 * New version.
 *
 * Revision 3.19  1993/12/04  16:12:50  espie
 * Options changes.
 *
 * Revision 3.18  1993/12/02  15:45:33  espie
 * Changed extended file semantics.
 *
 * Revision 3.17  1993/11/17  15:31:16  espie
 * New version.
 *
 * Revision 3.16  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.15  1993/08/04  11:55:21  espie
 * Fixed upo previous song bug.
 *
 * Revision 3.13  1993/07/18  10:39:44  espie
 * Added forking under unix. Experimental...
 *
 * Revision 3.11  1993/05/09  14:06:03  espie
 * Fixed up bug with mix option no longer working.
 *
 * Revision 3.10  1993/04/25  15:13:36  espie
 * Force new version.
 *
 * Revision 3.9  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.7  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * Added loads of new options.
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Augmented usage.
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
 * Revision 2.20  1992/11/17  17:06:25  espie
 * Added PREVIOUS_SONG handling ???
 * Use streamio for new interface (obsolescent signal handlers), and
 * related changes.
 * Cleaned up path reader, and better signal handling.
 * Support for open_file.
 * Added imask.
 * Use transparent decompression/path lookup through open_file/close_file.
 * Added setup_audio().
 * Added some frequency/oversample/stereo change on the fly.
 * Necessitates rightful closing/reopening of audio.
 * Added compression methods. Changed getopt.
 * Separated mix/stereo stuff.
 * Added transpose feature.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Added recovery and reread for automatic recognition
 * of old/new tracker files.
 * Added two level of fault tolerancy.
 * Added more rational options.
 * Moved almost everything to audio and automaton.
 * Structured part of the code, especially replay ``automaton''
 * and setting up of effects.
 *
 * Revision 1.26  1991/11/17  17:09:53  espie
 * Added missing prototypes.
 * Some more info while loading files.
 * Added FAULT env variable, FAULT resistant playing,
 * for playing modules which are not quite correct.
 * Serious bug: dochangespeed was not reset all the time.
 * Check all these parameters, they MUST be reset for
 * each new song.
 * Fixed a stupid bug: when env variable LOOPING was
 * undefined, we got a segv on strcmp.
 * Now we just test for its existence, since this is
 * about all we want...
 * Bug correction: when doing arpeggio, there might not
 * be a new note, so we have to save the old note value
 * and do the arppeggio on that note.
 * Completely added control with OVERSAMPLE and FREQUENCY.
 * Added control flow.
 * Added pipe decompression, so that now you can do
 * str32 file.Z directly.
 * stdin may go away.
 * Added arpeggio.
 * Added vibslide and portaslide.
 * Added speed command.
 * Added signal control.
 * Error checking: there shouldn't be that many
 * segv signals any more.
 * Moved every command to commands.c.
 * Added some debug code for showing the full
 * sequence for a file.
 * Corrected the bug in volume slide: there is
 * no default value, i.e., if it is 0, it is 0,
 * as stupid as it may seem.
 * Added vibrato.
 * Added fastskip/corrected skip.
 * Modified control flow of the player till
 * it looks like something reasonable (i.e.,
 * the structure is more natural and reflects
 * the way stuff is played actually...)
 * Do not restart the sound when we change instruments
 * on the fly. A bit strange, but it works that way.
 * Modified main to use new data structures.
 * The sound player is MUCH cleaner, it uses now
 * a 3-state automaton for each voice.
 * Corrected ruckus with data type of sample.
 */
     

#include "defs.h"

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#endif
#include <ctype.h>
#ifdef VOLUME_CONTROL
#ifdef __hpux
#define true /* kludge to avoid typedef of boolean (name clash with macro) */
#include <audio/Alib.h>
#undef true
AGainDB	volume = -20;
char use_speaker = 0;
#endif
#endif

     
#include "song.h"
#include "extern.h"
#include "options.h"

#include "getopt.h"
#include "tags.h"
#include "prefs.h"
     
ID("$Id: main.c,v 1.1 1994/02/19 16:03:08 ache Exp $")


LOCAL void print_usage()
   {
   GENERIC handle;
   
   handle = begin_info("Usage");
   info(handle, "Usage: tracker [options] filename [...]");
   info(handle, "-help               Display usage information");
   info(handle, "-quiet              Print no output other than errors");
   info(handle, "-picky              Do not tolerate any faults (default is to ignore most)");
   info(handle, "-tolerant           Ignore all faults");
   info(handle, "-mono               Select single audio channel output");
   info(handle, "-stereo             Select dual audio channel output");
   info(handle, "-verbose            Show text representation of song");
   info(handle, "-repeats <count>    Number of repeats (0 is forever) (default 1)");
   info(handle, "-speed <speed>      Song speed.  Some songs want 60 (default 50)");
   info(handle, "-mix <percent>      Percent of channel mixing. (0 = spatial, 100 = mono)");
   info(handle, "-new -old -both     Select default reading type (default is -both)");
   info(handle, "-frequency <freq>   Set playback frequency in Hz");
   info(handle, "-oversample <times> Set oversampling factor");
   info(handle, "-transpose <n>      Transpose all notes up");
   info(handle, "-scroll             Show what's going on");
   info(handle, "-sync               Try to synch audio output with display");
#ifdef VOLUME_CONTROL
	info(handle, "-speaker				 Output audio to internal speaker");
	info(handle, "-volume <n>         Set volume in dB");
#endif
   info(handle, "");
   info(handle, "RunTime:");
   info(handle, "e,x     exit program");
   info(handle, "n       next song");
   info(handle, "p       restart/previous song");
   info(handle, ">       fast forward");
   info(handle, "<       rewind");
   info(handle, "S       NTSC tempo\t s\tPAL tempo");
   end_info(handle);
   }

/* Command-line options. */
LOCAL struct long_option long_options[] =
{
   {"help",                0, 'H', OPT_HELP},
   {"quiet",               0, 'Q', OPT_QUIET}, 
   {"picky",               0, 'P', OPT_PICKY},
   {"tolerant",            0, 'L', OPT_TOLERANT},
   {"new",                 0, 'N', OPT_NEWONLY},
   {"old",                 0, 'O', OPT_OLDONLY},
   {"both",                0, 'B', OPT_BOTH},
   {"mono",                0, 'M', OPT_MONO},
   {"stereo",              0, 'S', OPT_STEREO},
   {"verbose",             0, 'V', OPT_VERBOSE},
   {"frequency",           1, 'f', OPT_FREQUENCY},
   {"oversample",          1, 'o', OPT_OVERSAMPLE},
   {"transpose",           1, 't', OPT_TRANSPOSE},
   {"repeats",             1, 'r', OPT_REPEATS},
   {"speed",               1, 's', OPT_SPEED},
   {"mix",                 1, 'm', OPT_MIX},
   {"start",               1, 'X', OPT_START},
   {"cut",                 1, '-', OPT_CUT},
   {"add",                 1, '+', OPT_ADD},
   {"scroll",              0, 'v', OPT_SHOW},
   {"sync",                0, '=', OPT_SYNC},
#ifdef VOLUME_CONTROL
	{"speaker",					0, '#', OPT_SPEAKER},
	{"volume",					1, 'u', OPT_VOLUME},
#endif
   {0,                     0,  0 , 0}
};


/* global variable to catch various types of errors
 * and achieve the desired flow of control
 */
int error;

LOCAL int optvalue(def)
int def;
   {
   int d;

   if (sscanf(optarg, "%d", &d) == 1)
      return d;
   else
      {
      optind--;
      return def;
      }
   }

LOCAL struct song *do_read_song(name, type)
char *name;
int type;
   {
   struct song *song;
   struct exfile *file;

   file = open_file(name, "r", getenv("MODPATH"));
   if (!file)
      return NULL;
   song = read_song(file, type); 
   close_file(file);
   return song;
   }

LOCAL int ask_freq;
LOCAL int oversample;
LOCAL int stereo;
LOCAL int start;
LOCAL int transpose;


LOCAL void parse_options(argc, argv)
int argc;
char *argv[];
   {
   int c;
   
   while ((c = getlongopt(argc, argv, long_options))
      != BAD_OPTION)
      switch(c)
         {
		case OPT_CUT:
		case OPT_ADD:
			{
			int i;
			unsigned long imask = 0;
      
			for (i = 0; optarg[i]; i++)
				{
				char c = tolower(optarg[i]);

				if (c >= '1' && c <= '9')
					imask |= 1<< (c-'1');
				else if (c >= 'a' && c <= 'z')
					imask |= 1 << (c-'a'+9);
				}
			if (c == OPT_ADD)
				set_pref_scalar(PREF_IMASK, imask);
			else
				set_pref_scalar(PREF_IMASK, ~imask);
			}        
         break;
      case OPT_OLDONLY:   /* old tracker type */
         set_pref_scalar(PREF_TYPE, OLD);
         break;
      case OPT_NEWONLY:   /* new tracker type */
         set_pref_scalar(PREF_TYPE, NEW);
         break;
      case OPT_SHOW:
         set_pref_scalar(PREF_SHOW, TRUE);
         break;
      case OPT_SYNC:
         set_pref_scalar(PREF_SYNC, TRUE);
         break;
      case OPT_BOTH:   /* both tracker types */
         set_pref_scalar(PREF_TYPE, BOTH);
         break;
      case OPT_REPEATS:   /* number of repeats */
         set_pref_scalar(PREF_REPEATS, optvalue(0));
         break;
      case OPT_SPEED:  
         set_pref_scalar(PREF_SPEED, optvalue(50));
         break;
      case OPT_MONO:  
         stereo = FALSE;
         break;
      case OPT_STEREO:  
         stereo = TRUE;
         break;
      case OPT_OVERSAMPLE:  
         oversample = optvalue(1);
         break;
      case OPT_FREQUENCY:
         ask_freq = optvalue(0);
         break;
      case OPT_TRANSPOSE:
         transpose = optvalue(0);
         break;
      case OPT_PICKY:
         set_pref_scalar(PREF_TOLERATE, 0);
         break;
      case OPT_TOLERANT:
         set_pref_scalar(PREF_TOLERATE, 0);
         break;
      case OPT_MIX:     /* % of channel mix. 
                         * 0->full stereo, 100->mono */
         set_mix(optvalue(30));
         break;
      case OPT_START:
         start = optvalue(0);
         break;
      case OPT_HELP:
         print_usage();
         end_all(0);
         /* NOTREACHED */
      case OPT_VERBOSE:
         set_pref_scalar(PREF_DUMP, TRUE);
			break;
#ifdef VOLUME_CONTROL
		case OPT_VOLUME:
			volume = optvalue(-20);
			break;
		case OPT_SPEAKER:
			use_speaker = 1;
			break;
#endif
         }
   }

LOCAL struct song *load_song(name)
char *name;
   {
   struct song *song;
   char *buffer;
   int i, j;
   
   i = strlen(name);
   
   for (j = i; j > 0; j--)
      if (name[j] == '/' || name[j] == '\\')
         {
         j++;
         break;
         }
   
   buffer = malloc( i - j + 5);
   if (buffer)
      {
      sprintf(buffer, "%s...", name + j);
      status(buffer);
      }

   
   switch(get_pref_scalar(PREF_TYPE))
      {
   case BOTH:
      song = do_read_song(name, NEW);
      if (song)
			break;
      /* FALLTHRU */
   case OLD:
      song = do_read_song(name, OLD);
      break;
      /* this is explicitly flagged as a new module,
       * so we don't need to look for a signature.
       */
   case NEW:
      song = do_read_song(name, NEW_NO_CHECK);
      break;
      }
   status(0);
	return song;
   }

int main(argc, argv)
int argc;
char **argv;
   {
   struct song *song;
   boolean *is_song;
   int i;

   struct tag *result;

   is_song = (boolean *)malloc(sizeof(boolean) * argc);
   if (!is_song)
      end_all("No memory left");

   for (i = 0; i < argc; i++)
      is_song[i] = FALSE;        /* For termination */

   start = 0;
   set_pref_scalar(PREF_IMASK, 0);
   set_pref_scalar(PREF_BCDVOL, 0);
   set_pref_scalar(PREF_DUMP, FALSE);
   set_pref_scalar(PREF_SHOW, FALSE);
   set_pref_scalar(PREF_SYNC, FALSE);
   set_pref_scalar(PREF_TYPE, BOTH);
   set_pref_scalar(PREF_REPEATS, 1);
   set_pref_scalar(PREF_SPEED, 50);
   set_pref_scalar(PREF_TOLERATE, 1);

   if (argc == 1)
      {
      print_usage();
      end_all(0);
      }

   ask_freq = read_env("FREQUENCY", 0);
   oversample = read_env("OVERSAMPLE", 1);
   transpose = read_env("TRANSPOSE", 0);
   stereo = !getenv("MONO");
   set_mix(30);



      /* check the command name for default reading type */


   optind = 1;
   for (optind = 1; optind < argc; optind++)
      {
      parse_options(argc, argv);
      if (optind >= argc)
         end_all(0);
         
   
      song = load_song(argv[optind]);   
      if (song)
         is_song[optind] = TRUE;
      else
         {
         puts("not a song");
         is_song[optind] = FALSE;
         continue;
         }
play_on:
      if (get_pref_scalar(PREF_DUMP))
         dump_song(song); 
      transpose_song(song, transpose);
      setup_audio(ask_freq, stereo, oversample);
      result = play_song(song, start);
      release_song(song);
      status(0);
      while (result = get_tag(result))
         {
         switch (result->type)
            {
         case PLAY_PREVIOUS_SONG:
            optind--;
            while ((optind > 0) && !is_song[optind])
               optind--;
            if (optind == 0)
               end_all(0);
            else
               {
               song = load_song(argv[optind]);
               goto play_on;
               }
            /* NOTREACHED */
         case PLAY_LOAD_SONG:
            song = load_song(result->data.pointer);
            free(result->data.pointer);
            if (song)
               goto play_on;
         default:
            break;
            }
         result++;
         }
            
      }
   end_all(0);
   /* NOTREACHED */
   }


