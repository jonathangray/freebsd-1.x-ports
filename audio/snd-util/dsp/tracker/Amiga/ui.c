/* amiga/ui.c 
	vi:se ts=3 sw=3:
 */

/* $Id: ui.c,v 1.1 1994/02/19 16:03:11 ache Exp $
 * $Log: ui.c,v $
 * Revision 1.1  1994/02/19 16:03:11  ache
 * Initial revision
 *
 * Revision 1.19  1994/01/09  23:25:16  Espie
 * Last bug fix.
 *
 * Revision 1.18  1994/01/09  17:38:28  Espie
 * Generalized open.c.
 *
 * Revision 1.17  1994/01/09  04:49:18  Espie
 * File requester !
 *
 * Revision 1.16  1994/01/08  20:26:07  Espie
 * Added pause gadget.
 *
 * Revision 1.15  1994/01/08  19:45:29  Espie
 * Uncentralized event handling using event management functions.
 *
 * Revision 1.13  1994/01/07  15:08:54  Espie
 * Changed name to ui_win, added Show gadget.
 *
 * Revision 1.12  1994/01/06  22:37:26  Espie
 * better coding for gadgets.
 * Nasty bug with info: did not close the file properly.
 *
 * Revision 1.11  1994/01/05  19:24:08  Espie
 * Fully working asynchronous interface.
 *
 * Revision 1.10  1994/01/05  16:48:58  Espie
 * User feedback.
 *
 * Revision 1.9  1994/01/05  16:12:06  Espie
 * Problem with output ? Still to fix !
 *
 * Revision 1.8  1994/01/05  14:55:38  Espie
 * *** empty log message ***
 *
 * Revision 1.7  1994/01/05  02:01:00  Espie
 * Added missing autoinit
 *
 * Revision 1.6  1994/01/04  15:44:03  Espie
 * Removed some typecasts.
 *
 * Revision 1.5  1993/12/28  14:03:53  Espie
 * info facility.
 * scroll post-synchronized output.
 * notice.
 *
 * Revision 1.4  1993/12/27  04:11:01  Espie
 * Cursor handling.
 *
 * Revision 1.3  1993/12/27  02:35:02  Espie
 * Used of discard_buffer for premature ending.
 *
 * Revision 1.2  1993/12/26  22:48:18  Espie
 * Mostly working.
 * Just dies with a guru.
 * Plus timing problems at start.
 *
 * Revision 1.1  1993/12/26  18:54:21  Espie
 * Initial revision
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <proto/intuition.h>
#include <proto/gadtools.h>
#include <proto/exec.h>
#include <dos/dos.h>

#include "defs.h"
#include "extern.h"
#include "amiga/amiga.h"
#include "tags.h"
#include "prefs.h"

ID("$Id: ui.c,v 1.1 1994/02/19 16:03:11 ache Exp $")
LOCAL void init_ui(void);
LOCAL void do_set_current(VALUE current);
LOCAL void handle_ui_window(GENERIC nothing);

LOCAL void (*INIT)(void) = init_ui;


/* These two variables memorize where we actually are in
 * the current song
 */
LOCAL int current_pattern;

#define SMALL_DELAY 3         /* in seconds */

/* And these when stuff last changed */
LOCAL ULONG pattern_change_seconds, pattern_change_micros,
song_change_seconds, song_change_micros;


/* The basic user interface (a simple window) */

struct IntuitionBase *IntuitionBase = 0;
struct GfxBase *GfxBase = 0;
LOCAL struct Library *GadtoolsBase = 0;
struct Window *ui_win;

/* for asynchronous easy requests */
LOCAL struct Window *notice_win = 0;

   /* for computing length */
LOCAL struct IntuiText it;

#define SHIFT 4
#define SPACEX 10
#define SPACEY 4

#define WINDOW_TITLE "Experiment IV "        /* the space in case the font is italic */

LOCAL struct NewGadget template =
   {
   0, 0,
   0, 0,
   NULL,
   0,
   0,       /* gadget ID */
   0,
   NULL,
   NULL
   };

LOCAL struct NewMenu menu_template[] =
   {
   {NM_TITLE,  "Project",        0, 0, 0, 0},
   {NM_ITEM,   "Load song...",   0, 0, 0, (void *)4},
   {NM_ITEM,   "About...",       0, 0, 0, (void *)1},
   {NM_ITEM,   "Quit",           0, 0, 0, (void *)2},
   {NM_TITLE,  "Settings",       0, 0, 0, 0},
   {NM_ITEM,   "PAL",            0, CHECKIT | CHECKED, 6, (void *)50},
   {NM_ITEM,   "NTSC",           0, CHECKIT, 5, (void *)60},
   {NM_ITEM,   "Custom",         0, CHECKIT, 3, (void *)3},
   {NM_END,    0,                0, 0, 0, 0}
   };

LOCAL struct Menu *menu;

LOCAL APTR vi;
LOCAL struct Screen *pub = 0;
LOCAL struct Gadget *glist, *title_gad, *pattern_gad, *total_gad;

/* we precisely have seven gadgets */
#define MAX_GADGET 6

/* all labelled with strings */
LOCAL char *label[MAX_GADGET + 1] =
   {
   "|<",
   "<<",
   "//",
   ">>", 
   ">|",
   "?",
   "000",
   };

#define G_NEXT 4
#define G_RESTART_PREVIOUS 0
#define G_REWIND 1
#define G_FF 3
#define G_SHOW 5
#define G_PAUSE 2

/* the special structure to display lines 
 *	
 */
LOCAL struct MinList scrolls;
struct scroll_line
   {
   struct MinNode node;
   char buffer[80];
   };

LOCAL struct ext_message *restart_msg = NULL;

LOCAL void cleanup_ui()
   {
   if (restart_msg)
      {
      send(restart_msg, TYPE_UNPAUSE);
      restart_msg = 0;
      }
   if (ui_win)
      {
      remove_signal_handler(ui_win->UserPort->mp_SigBit);
      SetWindowTitles(ui_win, 0, 0);
      ClearMenuStrip(ui_win);
      CloseWindow(ui_win);
      ui_win = 0;
      }
   if (menu)
      FreeMenus(menu);
   if (glist)
      FreeGadgets(glist);
   if (vi)
      FreeVisualInfo(vi);
   if (pub)
      UnlockPubScreen(NULL, pub);
   while (notice_win)
      await_events();
   if (GfxBase)
      CloseLibrary(GfxBase);
   if (IntuitionBase)
      CloseLibrary(IntuitionBase);
   if (GadtoolsBase)
      CloseLibrary(GadtoolsBase);
   }
   

LOCAL void init_ui(void)
   {
   struct Gadget *gad;
   int i;
   int max_width;
   int width, height;
   UWORD zoom[4];

   NewList(&scrolls);
   at_end(cleanup_ui);
   IntuitionBase = OpenLibrary("intuition.library", 37);
   if (!IntuitionBase)
      end_all("No Intuition");
   GadtoolsBase = OpenLibrary("gadtools.library", 37);
   if (!GadtoolsBase)
      end_all("No gadtools");
   GfxBase = OpenLibrary("graphics.library", 37);
   if (!GfxBase)
      end_all("No graphics");
   pub = LockPubScreen(NULL);
   if (!pub)
      end_all("No pubscreen");
   vi = GetVisualInfo(pub, TAG_END);
   if (!vi)
      end_all("No VI");
   menu = CreateMenus(menu_template, TAG_END);
   if (!menu)
      end_all("No menus");
   if (!LayoutMenus(menu, vi, TAG_END))
      end_all("Menus badly formed");
      
      /* now to create the gadgets */
      /* Compute max width/height according to the font */
   it.ITextFont = pub->Font;
   template.ng_TextAttr = pub->Font;
   
   max_width = 0;
   for (i = 0; i < MAX_GADGET + 1; i++)
      {
      it.IText = label[i];  
      width = IntuiTextLength(&it);
      if (width > max_width)
         max_width = width;
      }
    
   max_width += SPACEX;
   template.ng_Width = max_width;     
   template.ng_Height = pub->Font->ta_YSize + SPACEY;

      /* set up Top/Left Edge of initial gadget according to Wbar */
   template.ng_TopEdge = pub->WBorTop + 1 + 2 * template.ng_Height + SHIFT;
   template.ng_LeftEdge = pub->WBorLeft + SHIFT; 

   gad = CreateContext(&glist);
   if (!gad)
      end_all("No context");
   template.ng_VisualInfo = vi;
      /* lay out gadgets */
   for (i = 0; i < MAX_GADGET; i++)
      {
      template.ng_GadgetText = label[i];
      gad = CreateGadget(BUTTON_KIND, gad, &template, TAG_END);
      if (!gad)
         end_all("Bad gadget");
      template.ng_LeftEdge += template.ng_Width + SHIFT;
      template.ng_GadgetID++;
      }
   width = template.ng_LeftEdge + pub->WBorRight;
   height = template.ng_TopEdge + template.ng_Height + pub->WBorBottom + SHIFT;
 
      /* zoom box */
   zoom[0] = ~0;
   zoom[1] = ~0;
   zoom[2] = width;
   zoom[3] = pub->WBorTop + 1 + pub->Font->ta_YSize;

      /* title gadget */     
   template.ng_GadgetText = "";
   template.ng_Width = width - SHIFT - pub->WBorLeft - pub->WBorRight;
   template.ng_Width -= 2 * max_width;
   template.ng_TopEdge = pub->WBorTop + 1 + template.ng_Height + SHIFT;
   template.ng_LeftEdge = SHIFT + pub->WBorLeft;
   title_gad = gad = CreateGadget(TEXT_KIND, gad, &template, TAG_END);
   if (!gad)
      end_all("Bad gadget");
      
      /* pattern gadget */
   template.ng_LeftEdge += template.ng_Width + SHIFT;
   template.ng_Width = max_width;
   pattern_gad = gad = CreateGadget(NUMBER_KIND, gad, &template, TAG_END);
   if (!gad)
      end_all("Bad gadget");

      /* total pattern */
   template.ng_GadgetText = "/";
   template.ng_LeftEdge += template.ng_Width + SHIFT;
   total_gad = gad = CreateGadget(NUMBER_KIND, gad, &template, TAG_END);
   if (!gad)
      end_all("Bad gadget");

   ui_win = OpenWindowTags(NULL, 
      WA_Title, WINDOW_TITLE,
      WA_Width, width,
      WA_Height, height,
      WA_AutoAdjust, TRUE,
      WA_MouseQueue, 35,   /* we can't always answer messages */
      WA_DepthGadget, TRUE,
      WA_CloseGadget, TRUE,
      WA_DragBar, TRUE,
      WA_Zoom, zoom,
      WA_Gadgets, glist,
      WA_IDCMP, IDCMP_CLOSEWINDOW | BUTTONIDCMP | TEXTIDCMP | 
                IDCMP_REFRESHWINDOW | IDCMP_MENUPICK,
      TAG_DONE, 0);
   if (!ui_win)
      end_all("No window");
   GT_RefreshWindow(ui_win, NULL);  
   SetMenuStrip(ui_win, menu);

   install_signal_handler(ui_win->UserPort->mp_SigBit, handle_ui_window, 0);
   
   /* build up scroll buffer stuff */
   }


/* Max number of input messages we can remember */
#define MAX_INPUT 10
LOCAL struct tag result[MAX_INPUT +1];
LOCAL int i;


LOCAL void handle_ui_window(GENERIC nothing)
   {
   struct IntuiMessage *msg;
   UWORD number;
   struct MenuItem *item;
   int id;
   VALUE temp;
   
   while((msg = GT_GetIMsg(ui_win->UserPort)) && i < MAX_INPUT)
      switch(msg->Class)
         {
      case IDCMP_CLOSEWINDOW:
         GT_ReplyIMsg(msg);
         set_break();
         break;
      case IDCMP_MENUPICK:
         number = msg->Code;
         while (number != MENUNULL)
            {
            item = ItemAddress(menu, msg->Code);
            switch((int)GTMENUITEM_USERDATA(item))
               {
            case 1:
               notice(
"Tracker 4.0\n\
      by Marc Espie (Marc.Espie@ens.fr)\n\n\
This is a giftware program\n\
If you want, you can send me some money\n\
My address is:\n\
      Espie Marc\n\
      60 rue du 4 septembre\n\
      87100 Limoges\n\
      France\n\n\
For the most recent version:\n\
      ftp Aminet or nic.funet.fi");
               break;
            case 2:
               GT_ReplyIMsg(msg);
               set_break();
               break;
            case 4:
               launch_requester();
               break;
            case 50:
            case 60:
               result[i].type = UI_SET_BPM;
               result[i++].data.scalar = (int)GTMENUITEM_USERDATA(item);
               break;
            default:
               break;
               }
            number = item->NextSelect;
            }
         GT_ReplyIMsg(msg);
         break;
      case IDCMP_REFRESHWINDOW:
         GT_ReplyIMsg(msg);
         GT_BeginRefresh(ui_win);
         GT_EndRefresh(ui_win, TRUE);
         break;
      case IDCMP_GADGETUP:
         id = ((struct Gadget *)msg->IAddress)->GadgetID;
         switch(id)
            {
         case G_NEXT:
            result[i++].type = UI_NEXT_SONG;
            break;
         case G_RESTART_PREVIOUS:
            if (msg->Seconds < song_change_seconds + SMALL_DELAY ||
            	(msg->Seconds == song_change_seconds + SMALL_DELAY && 
                msg->Micros <= song_change_micros) )
                {
                result[i++].type = UI_PREVIOUS_SONG;
                break;
                }
            else
               {
               result[i++].type = UI_RESTART;
               song_change_seconds = msg->Seconds;
               song_change_micros = msg->Micros;
               }
            break;
         case G_REWIND:
            result[i].type = UI_JUMP_TO_PATTERN;
            result[i].data.scalar = current_pattern;
            if (msg->Seconds < pattern_change_seconds + SMALL_DELAY ||
            	(msg->Seconds == pattern_change_seconds + SMALL_DELAY && 
                msg->Micros <= pattern_change_micros) )
            	result[i].data.scalar--;
            	   /* give some immediate feedback to the user */
            temp.scalar = result[i].data.scalar;
            do_set_current(temp);
				i++;
            break;
         case G_FF:
            result[i].type = UI_JUMP_TO_PATTERN;
            result[i].data.scalar = current_pattern + 1;
                  /* give some immediate feedback to the user */
            temp.scalar = result[i].data.scalar;
            do_set_current(temp);
            i++;
            break;
         case G_SHOW:
            set_pref_scalar(PREF_SHOW, TRUE);
            break;
         case G_PAUSE:
            if (restart_msg)
               {
               send(restart_msg, TYPE_UNPAUSE);
               restart_msg = 0;
               }
            else
               {
               struct ext_message *msg;
               
               msg = obtain_message();
               restart_msg = obtain_message();
               send(msg, TYPE_PAUSE);
               }
            }
         GT_ReplyIMsg(msg);
         break;
      default:
         GT_ReplyIMsg(msg);
         }
   }


void requested_file(struct amiganame *name)
   {
   result[i].data.pointer = name;
   result[i++].type = UI_LOAD_SONG;
   }

   
struct tag *get_ui()
   {

   INIT_ONCE

   if (checkbrk())
      result[i++].type = UI_QUIT;
   
   result[i].type = TAG_END;

   i = 0;

   return result;
   }


void song_title(char *s)
   {
   static char title[25];

   INIT_ONCE;

   strncpy(title, s, 25);
   if (ui_win)
      GT_SetGadgetAttrs(title_gad, ui_win, 0, GTTX_Text, title, TAG_END);
   /* stamp the time we changed the song */
   CurrentTime(&song_change_seconds, &song_change_micros);
   }

void status(char *s)   
   {
   INIT_ONCE;
   
   SetWindowTitles(ui_win, s ? s : WINDOW_TITLE, -1);
   }

/***
 ***
 ***	Scrolling line handling: 
 ***		note this is totally asynchronous and uses the TYPE_DO_SYNC
 ***		message for synchronizing with songs that are really played
 ***/


/* look at client.c/audio.c for the use of inhibit_output with respect
 * to discard_buffer
 */
unsigned int inhibit_output = 0;

LOCAL struct scroll_line *scroll_buffer = 0;

char *new_scroll(void)
   {
   char *s;
   	/* need some temporary storage in case everything is full */
	LOCAL char buffer[80];
   INIT_ONCE;
                           /* check for a scroll line available */
   scroll_buffer = RemHead(&scrolls);
   if (!scroll_buffer)     /* none available ? allocate one on the fly */
      scroll_buffer = malloc(sizeof(struct scroll_line));
   if (scroll_buffer)
      s = scroll_buffer->buffer;
   else                    /* still none ? use static buffer */
      s = buffer;
   strcpy(s, "                                                       ");
   return s;
   }

/* The actual hook that does all the printing */
LOCAL void do_scroll(VALUE p)  
   {
   struct scroll_line *s = p.pointer;
	
	if (inhibit_output == 0 && get_pref_scalar(PREF_SHOW))
		{
		add_scroller(s->buffer);
      }
	AddTail(&scrolls, s);
   }

void scroll()
   {
   struct ext_message *msg;
   
   if (scroll_buffer)		/* did we obtain a scroll line ? */
      {                    /* then set up to scroll it */
      msg = obtain_message();
      msg->data.hook.func = do_scroll;
      msg->data.hook.p.pointer = scroll_buffer;
      send(msg, TYPE_SYNC_DO);
      }
   scroll_buffer = 0;
   }

/* hook to change current pattern */
LOCAL void do_set_current(VALUE p)
   {
   if (!inhibit_output)
      {
      INIT_ONCE;
      if (ui_win)
         GT_SetGadgetAttrs(pattern_gad, ui_win, 0, GTNM_Number, p.scalar, TAG_END);
      }
   current_pattern = p.scalar;
   /* stamp the time we changed the pattern */
   CurrentTime(&pattern_change_seconds, &pattern_change_micros);
   }

/* hook to change current pattern total */
LOCAL void do_set_total(VALUE p)
	{
	INIT_ONCE;
   if (ui_win)
      GT_SetGadgetAttrs(total_gad, ui_win, 0, GTNM_Number, p.scalar, TAG_END);
	}

void display_pattern(int current, int total)
   {
   struct ext_message *msg;

   INIT_ONCE 

   msg = obtain_message();
	msg->data.hook.func = do_set_total;
   msg->data.hook.p.scalar = total;
   send(msg, TYPE_SYNC_DO);

   msg = obtain_message();
	msg->data.hook.func = do_set_current;
   msg->data.hook.p.scalar = current;
   send(msg, TYPE_SYNC_DO);
   }



/***
 ***
 ***		Info window handling
 ***
 ***/

/* We chose the easy way: outputting everything in a console window
 * on the fly. An interesting improvement would be to buffer everything
 * and open the window with the right size afterwards
 */
struct handle
   {
   FILE *file;
   int linecount;
   int maxlength;
   int currentlength;
   };

#define H(h, field)  ( ((struct handle *)h)->field )

void *begin_info(char *title)
   {
   struct handle *new;
   
   char buffer[50];
   
   new = malloc(sizeof(struct handle));
   if (!new)
      return 0;
   sprintf(buffer, "CON:////%s/auto/close/wait", title);
   new->file=fopen(buffer, "w");
   if (!new->file)
      {
      free(new);
      return 0;
      }
   new->linecount = 0;
   new->maxlength = 0;
   new->currentlength = 0;
   return new;
   }

void infos(void *handle, char *s)
   {
   if (handle)
      {
      fprintf( H(handle,file), s);
      H(handle, currentlength) += strlen(s);
      }
   }

void info(void *handle, char *line)
   {
   infos(handle, line);
   if (handle)
      {
      fputc('\n', H(handle, file));
      if ( H(handle, currentlength) > H(handle, maxlength) )
         H(handle, maxlength) = H(handle, currentlength);
      H(handle, linecount)++;
      }
   }

void end_info(void *handle)
   {
   if (handle)
      {
      
      fclose(H(handle, file));
      free(handle);
      }
   }



/***
 ***
 ***	notice() pseudo-system call.
 ***	mostly used to report errors
 ***
 ***	The only difficulty comes from the fact
 ***	that we may be called under any kind of environment
 ***
 ***/

#ifdef USE_ARQ
#include "arq.h"
/* arq 1.78 doesn't notice BuildEasyRequest().
   I need to contact Martin Laubach about it
       FidoNet: 2:310/3.14 
       Usenet:  mjl@alison.at (home) 
                               mjl@auto.tuwien.ac.at (work) 
                {cbmvax!cbmehq,mcsun!tuvie}!cbmvie!alison!mjl 

       Peter, the graphics and animation wizard, can be reached
     2:310/42 in FidoNet.  
 */

LOCAL struct ExtEasyStruct es =
   {
   0,
   0,
   ARQ_ID_INFO,
   0,
   ARQ_MAGIC,
   0, 0, 0,
   sizeof(struct EasyStruct),
   0,
   "Notice\xa0",
   NULL,
   "Proceed"
   };
LOCAL struct EasyStruct *esp = & (es.Easy);
#else
LOCAL struct EasyStruct es = 
   {
   sizeof(struct EasyStruct),
   0,
   "Notice",
   NULL,
   "Proceed"
   };
LOCAL struct EasyStruct *esp = &es;
#endif


void handle_notice(struct Window *w)
   {
   if (SysReqHandler(w, 0, FALSE) != -2)
      {
      remove_signal_handler(w->UserPort->mp_SigBit);
      FreeSysRequest(w);
      notice_win = 0;
      }
   }

void notice(char *s)
   {
   INIT_ONCE;
   
   if (!IntuitionBase)
      {
      fprintf(stderr, s);
      fputc('\n', stderr);
      }
   else
      {
         /* wait for previous notice to go away */
      while (notice_win)
         await_events();
      
      esp->es_TextFormat = s;
      notice_win = BuildEasyRequest(0, esp, NULL, NULL);
      install_signal_handler(notice_win->UserPort->mp_SigBit, handle_notice, notice_win);
      }
   }

