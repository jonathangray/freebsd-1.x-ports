/* dump_song.c 
	vi:se ts=3 sw=3:
 */

/* $Id: dump_song.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: dump_song.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.1  1994/01/12  16:11:07  espie
 * Last minute changes.
 *
 * Revision 4.0  1994/01/11  17:46:01  espie
 * Use virtual windows.
 *
 * Revision 1.8  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.7  1994/01/08  03:55:43  Espie
 * No more call to run_in_fg(), use begin_info result instead.
 *
 * Revision 1.6  1994/01/06  22:32:42  Espie
 * Added instrument name as shown per display.c.
 *
 * Revision 1.5  1994/01/05  16:10:49  Espie
 * *** empty log message ***
 *
 * Revision 1.4  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/05  13:50:43  Espie
 * Cosmetic change.
 *
 * Revision 1.2  1993/12/28  13:54:44  Espie
 * Use info facility.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.7  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.6  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.5  1993/04/28  20:13:13  espie
 * Very small bug with volume (Lawrence).
 *
 * Revision 3.4  1993/04/25  14:08:15  espie
 * Added finetune display.
 *
 * Revision 3.3  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 */

#include "defs.h"

#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "song.h"
#include "extern.h"
#include "channel.h"

ID("$Id: dump_song.c,v 1.1 1994/02/19 16:03:08 ache Exp $")

LOCAL void *handle = 0;
LOCAL char buffer[80];
extern char instname[];

/***
 *
 *  dump_block/dump_song:
 *  shows most of the readable info
 *  concerning a module on the screen.
 *
 ***/

LOCAL void dump_block(b)
struct block *b;
   {
   int i, j;

   for (i = 0; i < BLOCK_LENGTH; i++)
      {
      for (j = 0; j < NUMBER_TRACKS; j++)
         {
         sprintf(buffer,"%8d%5d%2d%4d", b->e[j][i].sample_number,
            b->e[j][i].pitch, b->e[j][i].effect,
            b->e[j][i].parameters);
         infos(handle, buffer);
         }
      info(handle, "");
      }
   }

/* make_readable(s):
 * transform s into a really readable string */

LOCAL void make_readable(s)
char *s;
   {
   char *t, *orig;

   if (!s)
      return;

   orig = s;
   t = s;

      /* get rid of the st-xx: junk */
   if (strncmp(s, "st-", 3) == 0 || strncmp(s, "ST-", 3) == 0)
      {
      if (isdigit(s[3]) && isdigit(s[4]) && s[5] == ':')
         s += 6;
      }
   while (*s)
      {
      if (isprint(*s))
         *t++ = *s;
      s++;
      }
   *t = '\0';
   while (t != orig && isspace(t[-1]))
      *--t = '\0';
   }

void dump_song(song)
struct song *song;
   {
   int i, j;
   int maxlen;
   static char dummy[1];

   
   handle = begin_info(song->title);
   if (!handle)
      return;

   dummy[0] = '\0';
   maxlen = 0;
   for (i = 1; i < NUMBER_SAMPLES; i++)
      {
      if (!song->samples[i].name)
         song->samples[i].name = dummy;
      make_readable(song->samples[i].name);
      if (maxlen < strlen(song->samples[i].name))
         maxlen = strlen(song->samples[i].name);
      }
   for (i = 1; i < NUMBER_SAMPLES; i++)
      {
      if (song->samples[i].start || strlen(song->samples[i].name) > 2)
         {
         char s[3];
         
         s[0] = instname[i];
         s[1] = ' ';
         s[2] = 0;
         infos(handle, s);
         infos(handle, song->samples[i].name);
         for (j = strlen(song->samples[i].name); j < maxlen + 2; j++)
            infos(handle, " ");
         if (song->samples[i].start)
            {
            sprintf(buffer, "%5d", song->samples[i].length);
            infos(handle, buffer);
            if (song->samples[i].rp_length > 2)
               {
               sprintf(buffer, "(%5d %5d)", 
                  song->samples[i].rp_offset, 
                  song->samples[i].rp_length);
               infos(handle, buffer);
               }
            else
               infos(handle, "             ");
            if (song->samples[i].volume != MAX_VOLUME)
               {
               sprintf(buffer, "%3d", song->samples[i].volume);
               infos(handle, buffer);
               }
            else 
               infos(handle, "   ");
            if (song->samples[i].finetune)
               {
               sprintf(buffer, "%3d", song->samples[i].finetune);
               infos(handle, buffer);
               }
            }
         info(handle, "");
         }
      }
   end_info(handle);
   handle = 0;
   }
