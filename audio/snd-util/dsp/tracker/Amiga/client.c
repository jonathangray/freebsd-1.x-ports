/* amiga/client.c 
	vi:se ts=3 sw=3:
 */

/* $Id: client.c,v 1.1 1994/02/19 16:03:10 ache Exp $
 * $Log: client.c,v $
 * Revision 1.1  1994/02/19 16:03:10  ache
 * Initial revision
 *
 * Revision 1.8  1994/01/09  17:38:28  Espie
 * Generalized open.c.
 *
 * Revision 1.7  1994/01/08  19:45:29  Espie
 * Uncentralized event handling using event management functions.
 *
 * Revision 1.6  1994/01/08  04:00:52  Espie
 * Priority lowered.
 *
 * Revision 1.5  1994/01/07  15:57:20  Espie
 * Added check_type call for non-blocking processing.
 *
 * Revision 1.4  1994/01/07  15:08:54  Espie
 * Id.
 *
 * Revision 1.3  1994/01/06  22:37:26  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1994/01/05  19:24:08  Espie
 * Fully working asynchronous interface.
 *
 * Revision 1.1  1994/01/05  16:48:58  Espie
 * Initial revision
 *
 */

/* client to the audio server */


#include <exec/types.h>
#include <exec/tasks.h>
#include <exec/memory.h>
#include <exec/ports.h>

#include <proto/exec.h>

#include <stdio.h>

#include "defs.h"
#include "extern.h"
#include "song.h"
#include "amiga/amiga.h"

ID("$Id: client.c,v 1.1 1994/02/19 16:03:10 ache Exp $")
XT unsigned int inhibit_output;

LOCAL void init_client(void);
LOCAL void (*INIT)(void) = init_client;

#define STACK_SIZE 4000
#define HIPRI 50     /* should be higher than intuition's */
#define SUBNAME "Tracker sound server"

LOCAL struct MsgPort *subport = 0;

LOCAL struct MsgPort *myport = 0;

LOCAL struct ext_message *chunk = 0;

LOCAL struct MinList buffer;
LOCAL boolean live_task = FALSE;
LOCAL int watched_type = TYPE_INVALID; 
LOCAL struct ext_message *watched_message;

LOCAL void handle_subtask_events(GENERIC nothing)
   {
   struct ext_message *msg;
   
   while (msg = GetMsg(myport))
      {
      if (msg->type == TYPE_SYNC_DO)
         (msg->data.hook.func)(msg->data.hook.p);
      AddTail(&buffer, msg);
      if (msg->type == watched_type)
         {
         watched_message = msg;
         watched_type = TYPE_INVALID;
         }
      }
   }

struct ext_message *obtain_message()
   {
   struct ext_message *msg;
   
   INIT_ONCE;

   forever
      {
         /* get messages from port first: best synchronization */
      check_events();
            /* message available ? */
      if (msg = RemHead(&buffer))
         return msg;
      else  /* no-> wait for one */
         await_events();
      }
   }


void send(struct ext_message *msg, int type)
   {
      /* valid only for messages obtained through obtain_message ! */
   msg->type = type;
   msg->msg.mn_ReplyPort = myport;
   PutMsg(subport, msg);
   }


struct ext_message *check_type(int type)
   {
   struct ext_message *msg;

   watched_type = type;
   check_events();
   if (watched_message)
      {
      msg = watched_message;
      watched_message = 0;
      return msg;
      }
   else
      return 0;
   }
   
/* Note that await_type returns a message for checking purposes.
 * This message is NOT available
 */
struct ext_message *await_type(int type)
   {
   struct ext_message *msg;

   forever
      {
      watched_type = type;
      check_events();
      if (watched_message)
         {
         msg = watched_message;
         watched_message = 0;
         return msg;
         }
      await_events();
      }
   }

LOCAL void kill_subtask()
   {
   struct ext_message *msg;
      /* tell the subtask to die */
   msg = obtain_message();
   send(msg, TYPE_DIE);
      /* and wait for it to be in a dying state (Wait(0)) */
   msg = await_type(TYPE_DIE);
#ifndef EXTERNAL
      /* then kill it */
   RemTask(msg->data.comm.task);
#endif
   live_task = FALSE;
   }



LOCAL struct Task *newtask = 0;
LOCAL void *stack = 0;

/* We build up the task structure by ourselves. That way, we can
 * easily pass a message around by pushing it on the stack
 */
LOCAL void create_subtask()
   {
   ULONG *p;
   struct ext_message *msg;

#ifdef EXTERNAL
      /* we just have to find the task */
   struct MsgPort *pubport;

   pubport = FindPort(PUBLIC_PORT_NAME);
   if (!pubport)
      end_all("Could not rendez-vous");
      /* it's there: get it in working order */
   msg = obtain_message();
   msg->type = TYPE_COMM;
   msg->msg.mn_ReplyPort = myport;
   PutMsg(pubport, msg);
   msg = await_type(TYPE_COMM);
      /* check it's running correctly */
   if (msg->data.comm.port)
      {
      subport = msg->data.comm.port;
      live_task = TRUE;
      }
   else
      {
      end_all("subtask creation failed");
      }
#else
      /* build the new task from scratch */
   newtask = AllocVec(sizeof(struct Task), MEMF_CLEAR | MEMF_PUBLIC);
   if (!newtask)
      end_all("No task struct");
   stack = AllocVec(STACK_SIZE, MEMF_CLEAR);
   if (!stack)
      end_all("No stack");
   newtask->tc_SPLower = stack;
   newtask->tc_SPUpper = (APTR)((ULONG)(newtask->tc_SPLower) + STACK_SIZE);
   newtask->tc_Node.ln_Type = NT_TASK;
   newtask->tc_Node.ln_Pri = HIPRI;
   newtask->tc_Node.ln_Name = SUBNAME;

      /* ready to run: set it up for answering */      
   msg = obtain_message();
   msg->type = TYPE_COMM;
   msg->msg.mn_ReplyPort = myport;
      /* push message on the stack */
   p = newtask->tc_SPUpper;
   *(--p) = (ULONG)msg;
   newtask->tc_SPReg = p;
   
   if (!AddTask(newtask, subtask, 0))
      end_all("No subtask");
      /* Check it started up okay */
   msg = await_type(TYPE_COMM);
   if (msg->data.comm.port)
      {
      subport = msg->data.comm.port;
      live_task = TRUE;
      }
   else
      {
      RemTask(msg->data.comm.task);
      end_all("subtask creation failed");
      }
#endif
   }


/* right now, messages are statically allocated.
 * It might be a good idea to start with a SMALL
 * fixed number of messages (say 50) and increase
 * the queue on timing faults. A bit tricky, though.
 */
LOCAL void alloc_messages()
   {
   int i;
   
   myport = CreateMsgPort();
   if (!myport)
      end_all("Couldn't open message port");
   install_signal_handler(myport->mp_SigBit, handle_subtask_events, 0);
   chunk = AllocVec(sizeof(struct ext_message) * BUFFER_SIZE, MEMF_PUBLIC | MEMF_CLEAR);
   if (!chunk)
      end_all("Message allocation failed");
   for (i = 0; i < BUFFER_SIZE; i++)
      {
         /* don't forget this ! */
      chunk[i].msg.mn_Node.ln_Type = NT_MESSAGE;
      chunk[i].msg.mn_Length = sizeof(struct ext_message);
      AddTail(&buffer, chunk+i);
      }
   }

LOCAL void end_client()
   {
   if (live_task)
      kill_subtask();
      /* note that the subtask is already dead when end_client is called */
   if (chunk)
      FreeVec(chunk);
   if (newtask)
      FreeVec(newtask);
   if (stack)
      FreeVec(stack);
   if (myport)
      {
      remove_signal_handler(myport->mp_SigBit);
      DeleteMsgPort(myport);
      }
   }
      
LOCAL void init_client()
   {
   NewList(&buffer);
   at_end(end_client);
   alloc_messages();    /* note we must call alloc_messages BEFORE create_subtask
                         * since create_subtask depends on obtain_message
                         */
   create_subtask();    /* this hooks kill_subtask, AFTER end_client,
                         * so it will be called BEFORE.
                         */
   }

void close_audio(void)
   {
   if (live_task)
      {
      struct ext_message *msg;

      msg = obtain_message();
      msg->data.info.channel_mask = 15;
      send(msg, TYPE_FLUSH_CHANNEL);
      while (msg != await_type(TYPE_FLUSH_CHANNEL))
         ;
      }
   }

