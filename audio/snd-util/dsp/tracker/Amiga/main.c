/* amiga/main.c 
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
 *    Modified version of the standard main.c
 */

/* $Id: main.c,v 1.1 1994/02/19 16:03:11 ache Exp $
 * $Log: main.c,v $
 * Revision 1.1  1994/02/19 16:03:11  ache
 * Initial revision
 *
 * Revision 1.9  1994/01/09  23:25:16  Espie
 * Last bug fix.
 *
 * Revision 1.8  1994/01/09  17:38:28  Espie
 * Generalized open.c.
 *
 * Revision 1.7  1994/01/09  04:49:18  Espie
 * File requester !
 *
 * Revision 1.6  1994/01/08  19:45:29  Espie
 * Uncentralized event handling using event management functions.
 *
 * Revision 1.5  1994/01/08  04:07:20  Espie
 * Yet another mindless bug
 *
 * Revision 1.4  1994/01/08  04:00:52  Espie
 * Suppressed calls to run_in_fg().
 * Added check for / in song names to display something sensible
 * in the title bar...
 *
 * Revision 1.3  1994/01/07  15:57:20  Espie
 * Corrected minor previous song bug.
 *
 * Revision 1.2  1994/01/07  15:08:54  Espie
 * Suppressed spurious code.
 * Added CUT/ADD keywords.
 *
 * Revision 1.1  1994/01/06  22:37:26  Espie
 * Initial revision
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
     

#include <exec/types.h>
#include <exec/memory.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <dos/dosasl.h>
#include <dos/dosextens.h>
#include <exec/libraries.h>
     
#include "defs.h"

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#endif

#include "song.h"
#include "extern.h"

#include "tags.h"
#include "prefs.h"
#include "amiga/amiga.h"
     
ID("$Id: main.c,v 1.1 1994/02/19 16:03:11 ache Exp $")

LOCAL struct MinList temp;

LOCAL void print_usage()
   {
   GENERIC handle;
   
   handle = begin_info("Usage");
   info(handle, "Usage: tracker files OPTIONS");
   info(handle, "HELP               Display usage information");
   info(handle, "QUIET              Print no output other than errors");
   info(handle, "PICKY              Do not tolerate any faults (default is to ignore most)");
   info(handle, "TOLERANT           Ignore all faults");
   info(handle, "VERBOSE            Show text representation of song");
   info(handle, "REPEATS count      Number of repeats (0 is forever) (default 1)");
   info(handle, "SPEED speed        Song speed.  Some songs want 60 (default 50)");
   info(handle, "NEW/OLD/BOTH       Select default reading type (default is -both)");
   info(handle, "TRANSPOSE n        Transpose notes up by n halftones");
   info(handle, "SCROLL             Show notes scrolling by");
   info(handle, "CUT ab1            Play all instruments except number 1 a and b");
   info(handle, "ADD ab1            Play only instruments except number 1 a and b");
   info(handle, "GUI                open GUI directly");
   end_info(handle);
   }

/* arg parser for command-line options. */

LOCAL char *template = 
"FILES/M,HELP/S,QUIET/S,TOLERANT/S,PICKY/S,NEW/S,OLD/S,BOTH/S,V=VERBOSE/S,\
TRANS=TRANSPOSE/K/N,R=REPEATS/K,SPEED/K/N,START/K,\
CUT/K,ADD/K,SCROLL/S,GUI/S";

#define OPT_HELP 1
#define OPT_QUIET 2
#define OPT_TOLERANT 3
#define OPT_PICKY 4
#define OPT_NEW 5
#define OPT_OLD 6
#define OPT_BOTH 7
#define OPT_VERBOSE 8
#define OPT_TRANSPOSE 9
#define OPT_REPEATS 10
#define OPT_SPEED 11
#define OPT_START 12
#define OPT_CUT 13
#define OPT_ADD 14
#define OPT_SHOW 15
#define OPT_GUI 16

LOCAL LONG args[18];

LOCAL struct RDArgs *rdargs = 0;

LOCAL void free_args()
   {
   if (rdargs)
      FreeArgs(rdargs);
   }

/* global variable to catch various types of errors
 * and achieve the desired flow of control
 */
int error;

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

LOCAL int start;
LOCAL int transpose;


LOCAL void use_options()
   {
   char *s;
   
   if ((s = args[OPT_CUT]) || (s = args[OPT_ADD]))
      {
      char c;
      ULONG imask = 0;
      
      while (c = *s++)
         {
         if (c >= '1' && c <= '9')
            imask |= 1<< (c-'1');
         else if (c >= 'A' && c <= 'Z')
            imask |= 1 << (c-'A'+9);
         else if (c >= 'a' && c <= 'z')
            imask |= 1 << (c-'a'+9);
         }
      if (args[OPT_CUT])
         set_pref_scalar(PREF_IMASK, imask);
      else
         set_pref_scalar(PREF_IMASK, ~imask);
      }        
   if (args[OPT_OLD])
      set_pref_scalar(PREF_TYPE, OLD);
   if (args[OPT_NEW])
      set_pref_scalar(PREF_TYPE, NEW);
   if (args[OPT_SHOW])
      set_pref_scalar(PREF_SHOW, TRUE);
   if (args[OPT_BOTH])
      set_pref_scalar(PREF_TYPE, BOTH);
   if (args[OPT_REPEATS])
      set_pref_scalar(PREF_REPEATS, *((ULONG *)args[OPT_REPEATS]));
   if (args[OPT_SPEED])
      set_pref_scalar(PREF_SPEED, *((ULONG *)args[OPT_SPEED]));
   if (args[OPT_TRANSPOSE])
      transpose = *((LONG *)args[OPT_TRANSPOSE]);
   if (args[OPT_PICKY])
      set_pref_scalar(PREF_TOLERATE, 0);
   else if (args[OPT_TOLERANT])
      set_pref_scalar(PREF_TOLERATE, 0);
   if (args[OPT_START])
      start = *((ULONG *)args[OPT_START]);
   if (args[OPT_HELP])
      {
      print_usage();
      end_all(0);
      }
   if (args[OPT_VERBOSE])
      set_pref_scalar(PREF_DUMP, TRUE);
   }

LOCAL struct song *load_song(name)
char *name;
   {
   struct song *song;
   char *buffer;
   int i, j;
   
   i = strlen(name);
   
   for (j = i; j > 0; j--)
      if (name[j] == '/' || name[j] == ':')
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
   if (buffer)
      {
      status(0);
      free(buffer);
      }
   return song;
   }


#define PATHSIZE 250

struct MinList *expand_names(char *pat[])
   {
   int i;
   struct AnchorPath *ap;
   int error;
   int total;
   struct amiganame *new;
   
   NewList(&temp);
   ap = AllocVec(sizeof(struct AnchorPath) + PATHSIZE, MEMF_CLEAR);
   if (!ap)
      end_all(0);
   ap->ap_Strlen = PATHSIZE;
   for (i = 0; pat[i]; i++)
      {
      for (error = MatchFirst(pat[i], ap); !error; error = MatchNext(ap))
         {
         total = strlen(ap->ap_Buf) +1 ;
         if (strcmp(ap->ap_Buf + total -6, ".info") == 0)
            continue;
         new = malloc(sizeof(struct amiganame) + total);
         if (!new)
            continue;
         strcpy(new->s, ap->ap_Buf);
         new->i = TRUE;
            AddTail(&temp, new);
         }
      MatchEnd(ap);
      }
   FreeVec(ap);
   return &temp;
   }
      
/* add test for >=37 */

XT struct DosLibrary *DOSBase;      
int main(argc, argv)
int argc;
char **argv;
   {
   struct song *song;
   struct MinList *list;
   struct amiganame *element, *i;

   struct tag *result;

   if (DOSBase->dl_lib.lib_Version < 37)
      end_all("Need OS >= 2.04");

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

   transpose = read_env("TRANSPOSE", 0);


   if (argc > 0)     /* CLI */
      {
      /* check the command name for default reading type */

      rdargs = ReadArgs(template, args, 0);
      if (rdargs)
         at_end(free_args);
      else
         end_all(0);
   
      use_options();
   
      list = expand_names(args[0]);
      }

again:      
   if (argc == 0 || args[OPT_GUI])
      {
      launch_requester();
      list = 0;
      forever
         {
         result = get_ui();
         while (result = get_tag(result))
            {
            if (result->type == UI_LOAD_SONG)
               {
               i = (struct amiganame *)result->data.pointer;
               if (!i)
                  end_all(0);
               temp.mlh_Tail = 0;
               temp.mlh_Head = i;
               temp.mlh_TailPred = i->n.mln_Pred;
               i->n.mln_Pred->mln_Succ = &(temp.mlh_Tail);
               i->n.mln_Pred = &temp;
               list = &temp;
               break;
               }
            }
         if (list)
            break;
         await_events();
         }
      }
      
   for (element = list->mlh_Head; element->n.mln_Succ; element = element->n.mln_Succ)
      {
      if (!element->i)
         continue;
      song = load_song(element->s);   
      if (song)
         element->i = TRUE;
      else
         {
         char buffer[150];

         sprintf(buffer, "%s is not a song", element->s);
         notice(buffer);
         element->i = FALSE;
         continue;
         }
play_on:
      if (get_pref_scalar(PREF_DUMP))
         dump_song(song); 
      transpose_song(song, transpose);
      setup_audio(0, 1, 1);   /* doesn't really matter on the amiga */
      result = play_song(song, start);
      release_song(song);
      while (result = get_tag(result))
         {
         switch (result->type)
            {
         case PLAY_PREVIOUS_SONG:
            for (element = element->n.mln_Pred; element->n.mln_Pred; 
               element = element->n.mln_Pred)
               if (element->i)
                  {
                  song = load_song(element->s);
                  goto play_on;
                  }
            break;
         case PLAY_LOAD_SONG:
               /* splice play load song result into list */
            i = (struct amiganame *)result->data.pointer;
            element->n.mln_Succ->mln_Pred = i->n.mln_Pred;
            i->n.mln_Pred->mln_Succ = element->n.mln_Succ;
            element->n.mln_Succ = i;
            i->n.mln_Pred = element;
         default:
            break;
            }
         result++;
         }
            
      }
   if (argc == 0 || args[OPT_GUI])
      goto again;
   end_all(0);
   /* NOTREACHED */
   }

