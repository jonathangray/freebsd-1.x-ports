/* amiga/server.h */

/* $Id: server.h,v 1.1 1994/02/19 16:03:12 ache Exp $
 * $Log: server.h,v $
 * Revision 1.1  1994/02/19 16:03:12  ache
 * Initial revision
 *
 * Revision 1.2  1994/01/08  20:26:45  Espie
 * Added pause gadget.
 *
 * Revision 1.1  1994/01/04  15:45:37  Espie
 * Initial revision
 *
 */

/* definitions only relevant internally to the server operation */

struct MsgPort *start_audio(void);
struct MsgPort *open_timer(void);

void end_audio(void);
void close_timer(void);

void handle_audio(struct List *events, boolean signaled);
void handle_timer(struct List *events, boolean signaled);
void do_events(struct List *events);

/* fast interface to audio (used for pause) */
void send_immediate(ULONG command, int mask);