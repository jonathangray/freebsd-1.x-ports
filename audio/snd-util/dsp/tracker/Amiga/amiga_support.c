/* support functions for yours truly amiga 
	vi:se ts=3 sw=3:
 */

/* $Id: amiga_support.c,v 1.1 1994/02/19 16:03:09 ache Exp $ */
/* $Log: amiga_support.c,v $
/* Revision 1.1  1994/02/19 16:03:09  ache
/* Initial revision
/*
 * Revision 1.4  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 * Got rid of channel in resample.
 *
 * Revision 1.3  1993/12/02  15:45:33  espie
 * Stupid fix + type casts.
 *
 * Revision 1.2  1993/11/17  15:29:53  espie
 * Added higher-level support. Separated plainly from audio.c
 * */

#include <stdio.h>
#include "defs.h"
#include "extern.h"

#include "song.h"
#include "channel.h"

#include <hardware/cia.h>
#include <hardware/intbits.h>
#include <hardware/dmabits.h>
#include <exec/nodes.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <devices/audio.h>
#include <devices/timer.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/timer.h>
#undef X
#include <proto/intuition.h>
#include <proto/gadtools.h>

LOCAL struct audio_channel
	{
	int amiga_number;
	struct sample_info *samp;
	int volume;
	int pitch;
	} chan[4];

LOCAL struct sample_info dummy =
	{
	NULL,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	NULL,
	NULL
	};

LOCAL int allocated = 0;

struct audio_channel *new_channel()
	{
	struct audio_channel *new;

	new = &chan[allocated];
	new->amiga_number = allocated++;
	new->samp = &dummy;
	new->volume = 0;
	new->pitch = 0;
	new->samp = 0;
	return new;
	}

void no_audio_channels()
	{
	allocated = 0;
	}

/* list scanning.
 * next is needed: with it, we can actually unlink the node while scanning
 *	the current list.  Type is provided to avoid type-casting errors in
 * SCANLIST expansion.
 */
#define SCANLIST(node, next, list, type) \
	for((node) = (type)((struct MinList *)(list))->mlh_Head; \
		(next) = (type)((struct MinNode *)(node))->mln_Succ; \
		(node) = (next))

LOCAL char *version = "\0$VER: tracker 3.11";

/* remember allocated samples for cleaning up in panic case */
LOCAL struct MinList tracked_list;
/* timer variables:
 *		the message port 
 */
LOCAL struct MsgPort *tport = 0;
/*		the basic wait request */
LOCAL struct timerequest *tr = 0;
/* 		the reference system time at which the song started */
LOCAL struct EClockVal system_time;

LOCAL struct Library *TimerBase;

/* Clean up: Do we need to close timer ? */
LOCAL boolean timer_opened = FALSE;
/* Not really useful */
LOCAL boolean first_time = FALSE;
extern volatile struct CIA __far ciaa;

LOCAL void init_timer()
	{
	int fail;
	tport = CreateMsgPort();
	if (!tport)
		{
		exit(10);
		}
	tr = CreateIORequest(tport, sizeof(struct timerequest));
	if (!tr)
		{
		exit(10);
		}
	fail = OpenDevice(TIMERNAME, UNIT_WAITECLOCK, (struct IORequest *)tr, 0);
	if (fail)
		exit(10);
	else
		timer_opened = TRUE;
	TimerBase = tr->tr_node.io_Device;
	}

/* The basic user interface (a simple window) */
LOCAL struct IntuitionBase *IntuitionBase = 0;
LOCAL struct Library *GadtoolsBase = 0;
LOCAL struct Window *win;
/* We use topaz.font so as not to get into sizing problems */
LOCAL struct TextAttr topaz =
	{
	"topaz.font",
	8,
	0,
	0
	};

LOCAL struct NewGadget template =
	{
	10, 10, 	/* start position (offset from left/topedge) */
	72, 20,		/* width/height */
	NULL,
	&topaz,
	0,			/* gadget ID */
	0,
	NULL,
	NULL
	};

LOCAL APTR vi;
LOCAL struct Screen *pub = 0;
LOCAL struct Gadget *glist;

/* we precisely have seven gadgets */
#define MAX_GADGET 7

/* all labelled with strings */
LOCAL char *label[MAX_GADGET] =
	{
	"Previous",
	"Next",
	"Restart",
	"<<",
	">>", 
	"NTSC",
	"PAL",
	};

/* and mapping to these commands of the normal tracker interface */
LOCAL char mapto[MAX_GADGET] = { 'p', 'n', 'r', '<', '>', 'S', 's' };
	
LOCAL void init_ui(void)
	{
	struct Gadget *gad;
	int i;

	IntuitionBase = OpenLibrary("intuition.library", 37);
	if (!IntuitionBase)
		exit(10);
	GadtoolsBase = OpenLibrary("gadtools.library", 37);
	if (!GadtoolsBase)
		exit(10);
	pub = LockPubScreen(NULL);
	if (!pub)
		exit(10);
	vi = GetVisualInfo(pub, TAG_END);
	if (!vi)
		exit(10);
	gad = CreateContext(&glist);
	if (!gad)
		exit(10);
	template.ng_VisualInfo = vi;
		/* set up Top/Left Edge of initial gadget according to Wbar */
	template.ng_TopEdge += pub->WBorTop + pub->Font->ta_YSize + 1;
	template.ng_LeftEdge += pub->WBorLeft;	/* which I forgot... */
		/* lay out gadgets */
	for (i = 0; i < MAX_GADGET; i++)
		{
		template.ng_GadgetText = label[i];
		gad = CreateGadget(BUTTON_KIND, gad, &template, TAG_END);
		if (!gad)
			exit(10);
		template.ng_LeftEdge += template.ng_Width + 5;
		template.ng_GadgetID++;
		}
		
	win = OpenWindowTags(NULL, 
		WA_Title, "Experiment IV",
		WA_Width, template.ng_LeftEdge + 10,	
		WA_Height, template.ng_Height + template.ng_TopEdge + 10,
		WA_MouseQueue, 35,	/* we can't always answer messages */
		WA_DepthGadget, TRUE,
		WA_CloseGadget, TRUE,
		WA_DragBar, TRUE,
		WA_SmartRefresh, TRUE,	/* don't want to be bothered with refresh */
		WA_Gadgets, glist,
		WA_IDCMP, IDCMP_CLOSEWINDOW | BUTTONIDCMP | IDCMP_REFRESHWINDOW,
		TAG_DONE, 0);
	if (!win)
		exit(10);
	GT_RefreshWindow(win, NULL);	
	}


/* the audio interface */

/* we allow for lots more audio requests than we need */
#define QUEUE_LENGTH 50

LOCAL struct ext_audio 
	{
	struct IOAudio request;
	struct ext_audio *next;
	} 
/* the audio queue */	
	*first = 0, 
/* the specific request that get used for closing/opening */
	*req = 0;

LOCAL struct MsgPort *port = 0;
/* in order to clean up afterwards: */
LOCAL boolean audio_opened = FALSE;

LOCAL void command_audio(struct ext_audio *io);
LOCAL void send_immediate(ULONG command, int mask);

LOCAL UBYTE whichchannel[] = {15};

/* allocate_channels(): sends a request to the audio.device for
 * the channels.
 */
LOCAL void allocate_channels(void)
   { 
   struct ext_audio *sweep;

   req->request.ioa_Request.io_Command = ADCMD_ALLOCATE;
   req->request.ioa_AllocKey = 0;
   req->request.ioa_Data = whichchannel;
   req->request.ioa_Length = sizeof(whichchannel);
   command_audio(req);

   for (sweep = first; sweep; sweep = sweep->next)
      sweep->request.ioa_AllocKey = req->request.ioa_AllocKey;
   }
    
/* free_channels(): give back the channels.
 */ 
LOCAL void free_channels(void)
   {
   req->request.ioa_Request.io_Command = ADCMD_FREE;
   command_audio(req);
   }


LOCAL struct ext_audio *create_request(void)
   {
   struct ext_audio *new;

   new = AllocMem(sizeof(struct ext_audio), MEMF_CLEAR|MEMF_PUBLIC);
   if (req)
      *new = *req;
   return new;
   }

/* creates a whole queue of audio requests */   
LOCAL void create_queue(void)
   {
   struct ext_audio *last, *new;
   int i;
   
   last = NULL;
   for (i = 0; i < QUEUE_LENGTH; i++)
      {
      new = create_request();
      new->next = last;
      last = new;
      }
   first = new;
   }

LOCAL void get_requests(void)
   {
   struct ext_audio *back;

   while(back = GetMsg(port))
   	if (back != req)		/* only those belonging to the queue */
	      {
   	   back->next = first;
      	first = back;
	      }
   }

LOCAL void send_immediate(ULONG command, int mask)
   {
   first->request.ioa_Request.io_Command = command;
   first->request.ioa_Request.io_Flags = IOF_QUICK;
   first->request.ioa_Request.io_Unit = mask;
   BeginIO((struct IORequest *)first);
   }

LOCAL void command_audio(struct ext_audio *io)
   {
   struct ext_audio *request;

   BeginIO((struct IORequest *)io);
   WaitPort(port);
	get_requests();
   }

LOCAL void reset_audio(void)
   {
   send_immediate(CMD_RESET, 15);
   get_requests();
   }

/* obtain_audio(): allocate all the structures we will need to
 * play with the audio device
 */ 
LOCAL void obtain_audio(void)
   {
   BYTE fail;
   
   port = CreateMsgPort();
   if (!port)
   	{
   	fprintf(stderr, "Couldn't allocate port");
		exit(10);
		}
   req = CreateIORequest(port, sizeof(struct ext_audio));
   if (!req)
   	{
   	fprintf(stderr, "Couldn't allocate io request");
   	exit(10);
		}
   req->request.ioa_Request.io_Message.mn_Node.ln_Pri = 0;
      /* Note that OpenDevice returns 0 on success
       */
   fail = OpenDevice("audio.device", 0L, (struct IORequest *)req, 0L);
   if (fail)
		{
		fprintf(stderr, "Couldn't open audio device");
		exit(10);
		}
	else
		audio_opened = TRUE;
   }

/* remember old task priority at exit (CLI process runs on the CLI context,
 * and reverts to being a shell with the same priority on exit
 */
LOCAL int oldpri;

LOCAL void amiga_init()
	{
	oldpri = SetTaskPri(FindTask(0), 15);
		/* make cursor invisible, speed up output some */
	printf("\233""0 p\n");
	obtain_audio();
	create_queue();
	allocate_channels();
	init_timer();
		/* audio filter OFF */
	ciaa.ciapra |= CIAF_LED;
	init_ui();
	}

LOCAL void amiga_cleanup()
	{
	struct MinNode *current, *next;
	
	SetTaskPri(FindTask(0), oldpri);
	printf("\233"" p\n");
	if (win)
		CloseWindow(win);
	if (glist)
		FreeGadgets(glist);
	if (vi)
		FreeVisualInfo(vi);
	if (pub)
		UnlockPubScreen(NULL, pub);
	if (IntuitionBase)
		CloseLibrary((struct Library *)IntuitionBase);
	if (GadtoolsBase)
		CloseLibrary(GadtoolsBase);
	SCANLIST(current, next, &tracked_list, struct MinNode *)
		FreeVec(current);
   if (req->request.ioa_AllocKey)
		free_channels();
	if (audio_opened)
		CloseDevice((struct IORequest *)req);
	if (req)
		DeleteIORequest(req);
	if (port)
		DeletePort(port);
	while(first)
		{
		struct ext_audio *temp;
		
		temp = first;
		first = first->next;
		FreeMem(temp, sizeof(struct ext_audio));
		}

	if (timer_opened)
		{
		if (!CheckIO((struct IORequest *)tr))
			{
			AbortIO((struct IORequest *)tr);
			WaitIO((struct IORequest *)tr);
			}
		CloseDevice((struct IORequest *)tr);
		}
	if (tr)
		DeleteIORequest(tr);
	if (tport)
		DeleteMsgPort(tport);
	ciaa.ciapra &= ~CIAF_LED;
	}

/* indispensible for not losing memory ! */

void __regargs __chkabort()
	{
	}
	
void *alloc_sample(int len)
	{
	char *s;
	
	s = AllocVec(len + sizeof(struct MinNode), MEMF_CHIP | MEMF_CLEAR);
	if (!s)
		return 0;
	AddTail((struct List *)&tracked_list, (struct Node *)s);
	return s + sizeof(struct MinNode);
	}

void free_sample(char *s)
	{
	s -= sizeof(struct MinNode);
	Remove((struct Node *)s);
	FreeVec(s);
	}

/* termio */
boolean run_in_fg()
	{
	return 1;
	}

void nonblocking_io()
	{
	static boolean yet = 0;
	if (!yet)
		{
		NewList((struct List *)&tracked_list);
		atexit(amiga_cleanup);
		amiga_init();
		yet = 1;
		}
	}

void sane_tty()
	{
	}
	
int may_getchar()
	{
	struct IntuiMessage *msg;
	int id;
	if (SetSignal(0,0) & SIGBREAKF_CTRL_C)
		exit(10);
	while(msg = GT_GetIMsg(win->UserPort))
		switch(msg->Class)
			{
		case IDCMP_CLOSEWINDOW:
			GT_ReplyIMsg(msg);
			return 'q';
		case IDCMP_REFRESHWINDOW:
			GT_ReplyIMsg(msg);
			GT_BeginRefresh(win);
			GT_EndRefresh(win, TRUE);
			break;
		case IDCMP_GADGETUP:
			id = ((struct Gadget *)msg->IAddress)->GadgetID;
			GT_ReplyIMsg(msg);
			return mapto[id];
		default:
			GT_ReplyIMsg(msg);
			}
	return -1;
	}

/* audio */

/* empty operation on the amiga */
void init_tables(oversample, frequency, chan)
int oversample, frequency;
struct channel *chan;
	{
	}

void resample(oversample, number)
int oversample;
int number;
	{
	int i;
/*
	if (first_time)
		{
		first_time = FALSE;
		system_time.tv_micro += number;
		}
*/
		tr->tr_node.io_Command = TR_ADDREQUEST;
		tr->tr_time.tv_secs = system_time.ev_hi;
		tr->tr_time.tv_micro = system_time.ev_lo;
		SendIO((struct IORequest *)tr);
		WaitPort(tport);
		while(GetMsg(tport))
			;
	system_time.ev_lo += (unsigned long)number;
	if (system_time.ev_lo < (unsigned long)number)
		system_time.ev_hi++;
	}

void play_note(au, samp, pitch)
struct audio_channel *au;
struct sample_info *samp;
int pitch;
	{
	au->pitch = pitch;
	send_immediate(CMD_FLUSH, 1 << au->amiga_number);
	get_requests();
	if (samp)
		{
		au->samp = samp;
		if (au->samp->start)
			{
			first->request.ioa_Request.io_Command = CMD_WRITE;
			first->request.ioa_Request.io_Flags = ADIOF_PERVOL;
			first->request.ioa_Request.io_Unit = 1<<au->amiga_number;
			first->request.ioa_Data = au->samp->start;
			first->request.ioa_Length = au->samp->length;
			first->request.ioa_Period = au->pitch;
			first->request.ioa_Volume = au->volume;
			first->request.ioa_Cycles = 1;
			BeginIO((struct IORequest *)first);
			first = first->next;
			if (au->samp->rp_start)
				{
				first->request.ioa_Request.io_Command = CMD_WRITE;
				first->request.ioa_Request.io_Flags = 0;
				first->request.ioa_Request.io_Unit = 1<<au->amiga_number;
				first->request.ioa_Data = au->samp->rp_start;
				first->request.ioa_Length = au->samp->rp_length;
				first->request.ioa_Cycles = 0;
				BeginIO((struct IORequest *)first);
				first = first->next;
				}  
			}
		}
	}

void set_play_pitch(au, pitch)
struct audio_channel *au;
int pitch;
	{
	if (pitch != au->pitch)
		{
		au->pitch = pitch;
		first->request.ioa_Period = pitch;
		first->request.ioa_Volume = au->volume;
		send_immediate(ADCMD_PERVOL, 1 << au->amiga_number);
		}
	}

void set_play_volume(au, volume)
struct audio_channel *au;
int volume;
	{
	if (volume != au->volume)
		{
		au->volume = volume;
		first->request.ioa_Period = au->pitch;
		first->request.ioa_Volume = volume;
		send_immediate(ADCMD_PERVOL, 1 << au->amiga_number);
		}
	}

void set_play_position(au, pos)
struct audio_channel *au;
int pos;
	{
	send_immediate(CMD_FLUSH, 1 << au->amiga_number);
	get_requests();
	if (au->samp->start)
		{
		if (pos < au->samp->length)
			{
			first->request.ioa_Request.io_Command = CMD_WRITE;
			first->request.ioa_Request.io_Flags = ADIOF_PERVOL;
			first->request.ioa_Request.io_Unit = 1<<au->amiga_number;
			first->request.ioa_Data = au->samp->start + pos;
			first->request.ioa_Length = au->samp->length - pos;
			first->request.ioa_Period = au->pitch;
			first->request.ioa_Volume = au->volume;
			first->request.ioa_Cycles = 1;
			BeginIO((struct IORequest *)first);
			first = first->next;
			if (au->samp->rp_start)
				{
				first->request.ioa_Request.io_Command = CMD_WRITE;
				first->request.ioa_Request.io_Flags = 0;
				first->request.ioa_Request.io_Unit = 1<<au->amiga_number;
				first->request.ioa_Data = au->samp->rp_start;
				first->request.ioa_Length = au->samp->rp_length;
				first->request.ioa_Cycles = 0;
				BeginIO((struct IORequest *)first);
				first = first->next;
				}  
			}
		}
	}


void set_mix(percent)
int percent;
	{
	}

int open_audio(f, s)
int f, s;
	{
	first_time = TRUE;
	return ReadEClock(&system_time);		/* samples/sec used as a timing unit: 1sec =1 000 000 µs */
	}
	
void set_synchro(s)
boolean s;
	{
	ReadEClock(&system_time);
	first_time = TRUE;
	}

int update_frequency()
	{
	return 0;
	}

void output_samples(int left, int right)
	{
	}
	
void flush_buffer(void)
	{
	}

void discard_buffer(void)
	{
	reset_audio();
	}
	
void close_audio(void)
	{
	}
	

	
/* amiga: popen() for OS2_04 */


#include <proto/dos.h>
#include <dos/dostags.h>
#include <stdio.h>
#include <string.h>

/*
###	CSupport/popen
###
###	NAME
###		popen/pclose -- Unix-like pipes
###
###	STATUS
###		Experimental
###
 */
FILE *popen(char *command, char *mode)
	{
	
	if (strcmp(mode, "r") == 0)
		/* open pipe for reading */
		{
		FILE *reader;
		BPTR writer, null;

		writer = Open("PIPE:", MODE_NEWFILE);
		reader = fopen("PIPE:", "r");
		null = Open("NIL:", MODE_NEWFILE);
		if (SystemTags(command, SYS_Input, null, 
			SYS_Output, writer, SYS_Asynch, TRUE, TAG_END) == -1)
			{
			Close(null);
			Close(writer);
			fclose(reader);
			return NULL;
			}
		else
			return reader;
		}
	else if (strcmp(mode, "w") == 0)
		/* open pipe for writing */
		{
		FILE *writer;
		BPTR reader, null;
		
		writer = fopen("PIPE:", "w");
		reader = Open("PIPE:", MODE_OLDFILE);
		null = Open("NIL:", MODE_NEWFILE);
		if (SystemTags(command, SYS_Input, reader, 
			SYS_Output, null, SYS_Asynch, TRUE, TAG_END) == -1)
			{
			Close(null);
			Close(reader);
			fclose(writer);
			return NULL;
			}
		else
			return writer;
		}
	else
		return NULL;
	}

/* for us, pclose is just fclose.
 * We hope everything will close out alright
 */
void pclose(FILE *f)
	{
	fclose(f);
	}

