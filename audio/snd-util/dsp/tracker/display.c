/* display.c 
	vi:se ts=3 sw=3:
 */

/* $Id: display.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: display.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:45:22  espie
 * Major change: does not use sprintf heavily.
 *
 * Revision 1.9  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.8  1994/01/08  03:55:43  Espie
 * Use name_of_note(), no need for run_in_fg().
 *
 * Revision 1.7  1994/01/08  02:04:21  Espie
 * Small bug: strcpy -> stringcopy.
 *
 * Revision 1.6  1994/01/07  15:06:26  Espie
 * Cond code to make show/not show robust.
 *
 * Revision 1.5  1994/01/06  22:32:42  Espie
 * Added instrument name as shown per display.c.
 *
 * Revision 1.4  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/05  01:59:14  Espie
 * Added prototypes.
 *
 * Revision 1.2  1993/12/28  13:54:44  Espie
 * Major change: use scroller interface.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 1.8  1993/12/04  16:12:50  espie
 * Lots of LOCAL added + minor changes.
 *
 * Revision 1.7  1993/12/02  15:45:33  espie
 * Try to get rid of %d format in printf.
 *
 * Revision 1.6  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 1.4  1993/07/18  10:39:44  espie
 * Added last displays.
 *
 * Revision 1.3  1993/07/17  22:23:41  espie
 * Fixed bug with bad loops.
 *
 * Revision 1.2  1993/07/17  12:00:30  espie
 * Added other commands (numerous).
 *
 */
     
#include <stdio.h>
#include <string.h>
     
#include "defs.h"
#include "song.h"
#include "channel.h"
#include "extern.h"
#include "tags.h"
#include "prefs.h"

ID("$Id: display.c,v 1.1 1994/02/19 16:03:08 ache Exp $")
LOCAL void init_display P((void));
LOCAL void (*INIT)P((void)) = init_display;
     
#define ENTRY_SIZE 14
LOCAL char buffer[80];
LOCAL char *base;

LOCAL char *num[] = {
" 0", " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9",
"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
"30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
"40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
"50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
"60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
"70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
"80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
"90", "91", "92", "93", "94", "95", "96", "97", "98", "99",
"00", "01", "02", "03", "04", "05", "06", "07", "08", "09"};

char instname[] = { ' ', '1', '2', '3', '4', '5', '6', '7', '9',
'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};

LOCAL void reset_buffer()
   {
   base = new_scroll();
   }

LOCAL void next_entry()
   {
   base += ENTRY_SIZE;
   }

LOCAL void copy1(to, from)
char *to, *from;
   {
   *to = *from;
   }
   
LOCAL void copy2(to, from)
char *to, *from;
   {
   *to++ = *from++;
   *to = *from;
   }

LOCAL void copy3(to, from)
char *to, *from;
   {
   *to++ = *from++;
   *to++ = *from++;
   *to = *from;
   }

LOCAL void stringcopy(to, from)
char *to, *from;
   {
   while (*from)
      *to++ = *from++;
   }

LOCAL void num2(to, n)
char *to;
int n;
   {
   char *v = num[n];
   *to++ = *v++;
   *to = *v;
   }

LOCAL void num3(to, n)
char *to;
int n;
   {
   char *v;

   if (n >= 100)
      *to = "0123456789"[n/100];
   while (n > 109)
      n -= 100;
   v = num[n];
   to++;
   *to++ = *v++;
   *to = *v;
   }

LOCAL char *id = "$Id: display.c,v 1.1 1994/02/19 16:03:08 ache Exp $";
     
LOCAL void (*table[NUMBER_EFFECTS]) P((int samp, int para, int note, struct channel *ch));

LOCAL void disp_default(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   }

LOCAL void disp_speed(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   if (para < 32)
      {
      stringcopy(base+6, "SPD");
      num2(base+10, para);
      }
   else
      {
      stringcopy(base+6, "spd%");
      num3(base+10, para * 100/NORMAL_FINESPEED);
      }
   }

LOCAL void disp_nothing(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   }

LOCAL void disp_portamento(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   stringcopy(base+2, "-->");
   copy3(base+5, name_of_note(note));
   if (para)
      {
      base[8] = '(';
      num3(base+9, para);
      base[12] = ')';
      }
   }

LOCAL void disp_portaslide(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   stringcopy(base+2, "-->");
   copy3(base+5, name_of_note(note));
   if (LOW(para))
      {
      base[9] = '-';
      num2(base+10, LOW(para));
      }
   else
      {
      base[9] = '+';
      num2(base+10, HI(para));
      }
   }

LOCAL void disp_upslide(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   base[8] = '-';
   if (para)
      num3(base+9, para);
   }

LOCAL void disp_downslide(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   base[8] = '+';
   if (para)
      num3(base+9, para);
   }

LOCAL void disp_vibrato(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   copy2(base+6, "vb");
   if (para)
      {
      num2(base+8, LOW(para));
      base[10] = '/';
      num2(base+11, HI(para));
      }
   }

LOCAL void disp_vibratoslide(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "vibs");
   if (LOW(para))
      {
      base[10] = '-';
      num2(base+11, LOW(para));
      }
   else
      {
      base[10] = '+';
      num2(base+11, HI(para));
      }
   }

LOCAL void disp_slidevol(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "vol");
   if (LOW(para))
      {
      base[10] = '-';
      num2(base+11, LOW(para));
      }
   else
      if (HI(para))
         {
         base[10] = '+';
         num2(base+11, HI(para));
         }
   }

LOCAL void disp_volume(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   if (para)
      {
      stringcopy(base+6, "vol");
      num3(base+10, para);
      }
   else
      stringcopy(base+6, "silent");
   }

LOCAL void disp_arpeggio(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   if (note != NO_NOTE)
      {
      copy3(base+2, name_of_note(note));
      copy3(base+6, name_of_note(note + LOW(para)));
      copy3(base+10, name_of_note(note + HI(para)));
      }
   else
      if (ch->note == NO_NOTE)
         stringcopy(base, "Arp error");
      else
         {
         copy3(base+6, name_of_note(ch->note + LOW(para)));
         copy3(base+10, name_of_note(ch->note + HI(para)));
         }  
   }

LOCAL void disp_retrig(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base + 6, "rtg");
   num3(base+9, para);
   }

LOCAL void disp_note_cut(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "cut");
   num3(base+9, para);
   }

LOCAL void disp_late_start(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "lte");
   num3(base+9, para);
   }

LOCAL void disp_offset(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "off   %");
   num3(base+9, para * 25600/ch->samp->length);
   }

LOCAL void disp_smooth_up(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "sth-");
   num3(base+10, para);
   }

LOCAL void disp_smooth_down(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "sth+");
   num3(base+10, para);
   }

LOCAL void disp_smooth_upvolume(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+8, "++");
   num3(base+10, para);
   }

LOCAL void disp_smooth_downvolume(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+8, "--");
   num3(base+10, para);
   }

LOCAL void disp_change_finetune(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "fine");
   num2(base+11, para);
   }

LOCAL void disp_skip(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   if (para)
      {
      stringcopy(base+6, "skp");
      num3(base+10, para);
      }
   else
      stringcopy(base+6, "next");
   }

LOCAL void disp_fastskip(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "ff");
   num3(base+10, para);
   }

LOCAL void disp_loop(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   if (para == 0)
      stringcopy(base+6, "SETLOOP");
   else
      {
      stringcopy(base+6, "LOOP");
      num3(base+10, para);
      }
   }

LOCAL void disp_delay_pattern(samp, para, note, ch)
int samp, para, note;
struct channel *ch;
   {
   copy3(base+2, name_of_note(note));
   stringcopy(base+6, "DLAY");
   num3(base+10, para);
   }

#define disp_nothing disp_default

LOCAL void init_display()
   {
   int i;

   for (i = 0; i < NUMBER_EFFECTS; i++)
      table[i] = disp_nothing;
   table[EFF_ARPEGGIO] = disp_arpeggio;
   table[EFF_SPEED] = disp_speed;
   table[EFF_SKIP] = disp_skip;
   table[EFF_FF] = disp_fastskip;
   table[EFF_VOLUME] = disp_volume;
   table[EFF_VOLSLIDE] = disp_slidevol;
   table[EFF_OFFSET] = disp_offset;
   table[EFF_PORTA] = disp_portamento;
   table[EFF_PORTASLIDE] = disp_portaslide;
   table[EFF_UP] = disp_upslide;
   table[EFF_DOWN] = disp_downslide;
   table[EFF_VIBRATO] = disp_vibrato;
   table[EFF_VIBSLIDE] = disp_vibratoslide;
   table[EFF_SMOOTH_UP] = disp_smooth_up;
   table[EFF_SMOOTH_DOWN] = disp_smooth_down;
   table[EFF_CHG_FTUNE] = disp_change_finetune;
   table[EFF_LOOP] = disp_loop;
   table[EFF_RETRIG] = disp_retrig;
   table[EFF_S_UPVOL] = disp_smooth_upvolume;
   table[EFF_S_DOWNVOL] = disp_smooth_downvolume;
   table[EFF_NOTECUT] = disp_note_cut;
   table[EFF_LATESTART] = disp_late_start;
   table[EFF_DELAY] = disp_delay_pattern;
   reset_buffer();
   }

void dump_event(ch, e)
struct channel *ch;
struct event *e;
   {
   INIT_ONCE;
   
   if (get_pref_scalar(PREF_SHOW))
      {
      if (ch && base)
         {
         *base = instname[e->sample_number];
         (*table[e->effect])(e->sample_number, e->parameters, e->note, ch);
         next_entry();
         }
      else
         {
         scroll();
         reset_buffer();
         }
      }
   }

