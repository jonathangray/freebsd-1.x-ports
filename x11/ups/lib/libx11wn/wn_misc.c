/* wn_misc.c - miscellaneous functions - updating on/off, error reporting etc */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_misc_c_sccsid[] = "@(#)wn_misc.c	1.26 25/4/92 (UKC)";

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_misc.h"

#ifdef SUNVIEW
#include <syscall.h>
#include <sys/ioctl.h>
#include <sundev/kbio.h>
#undef ALT /* because <machine/kbd.h> and <mon/keyboard.h disagree.  Sigh */
#include <mon/keyboard.h>
#include <signal.h>

/*  <suntool/seln.h> includes <suntool/selection_svc.h> which includes
 *  <rpc/rpc.h> which includes <rpc/types.h> which includes <malloc.h>
 *  which wrongly declares free() as returning int.  Thats what you get
 *  when you mix SunOS (the chain of #includes) and System Bletch (the
 *  bogus declaration of free().  Sigh.  Gross hack workaround follows ...
 */
#define free dont_declare_free
#include <suntool/seln.h>
#undef free
#endif /* SUNVIEW */

#ifdef X11
#include <X11/Xatom.h>
#endif /* X11 */

/*  BUG: this should be in ansi/stddef.h, but select() is so undefined
 *  that I don't want it there.
 */
int select PROTO((int nfds, int *ifds, int *ofds, int *efds,
							struct timeval *timeout));

static char *store_set_selection PROTO((const char *data, int nbytes));
static char *store_got_selection PROTO((const char *data, int nbytes));
#ifdef SUNVIEW
static void func_key_proc PROTO((char *client_data, Seln_function_buffer *args));
static Seln_result send_selection PROTO((Seln_attribute item, Seln_replier_data *context, int length));

static int Sunview_wakeup_pipe[2];
#endif /* SUNVIEW */

/*  Error message for a bad window number.
 */
char wn__Badmesg[] = "bad window number (out of range or closed)";

extern int wn__Batching_enabled;

#ifdef X11
void
wn__do_xflush()
{
	XFlush(wn__Dpy);
}
#endif /* X11 */

/*  Show the latest updates to the screen.
 */
void
wn_show_updates(wn)
int wn;
{
#ifndef X11
	register swin_t *w = WN_TO_W(wn);
	int old_level;
#endif /* !X11 */

	W_CHECK(wn);
#ifdef X11
	wn__do_xflush();
#else
	if (w->w_bw->bw_upd_level > 0) {
		old_level = w->w_bw->bw_upd_level;
		w->w_bw->bw_upd_level = 1;
		wn_updating_on(wn);
		wn_updating_off(wn);
		w->w_bw->bw_upd_level = old_level;
	}
#endif /* !X11 */
}

/*   Turn off screen updating - this and w_unfreeze nest
 */
void
wn_updating_off(wn)
int wn;
{
	register swin_t *w = WN_TO_W(wn);
#ifdef X11
	W_CHECK(wn);
	if (wn__Batching_enabled)
		w->w_bw->bw_upd_level++;
#endif /* X11 */
#ifdef SUNVIEW
	struct rect r;
	
	W_CHECK(wn);
	if (wn__Batching_enabled && w->w_bw->bw_upd_level++ == 0)
		pw_batch_on(w->w_base->w_pw);
#endif /* SUNVIEW */
}

/*   Turn on screen updating
 */
void
wn_updating_on(wn)
int wn;
{
	register swin_t *w = WN_TO_W(wn);

#ifdef X11
	W_CHECK(wn);
	if (wn__Batching_enabled && --w->w_bw->bw_upd_level == 0)
		wn__do_xflush();
#endif /* X11 */

#ifdef SUNVIEW
	W_CHECK(wn);
	if (wn__Batching_enabled && --w->w_bw->bw_upd_level == 0)
		pw_batch_off(w->w_base->w_pw);
#endif /* SUNVIEW */
}

/*  Interface to malloc() that never returns NULL - it aborts if malloc
 *  returns NULL.
 */
char *
wn__e_malloc(nbytes)
size_t nbytes;
{
	char *ptr;

	if ((ptr = malloc(nbytes)) == NULL)
		wn__panic("malloc returned NULL");
	return ptr;
}

/*  Interface to malloc() that never returns NULL - it aborts if malloc
 *  returns NULL.
 */
char *
wn__e_realloc(ptr, nbytes)
char *ptr;
size_t nbytes;
{
	char *new;

	if ((new = realloc(ptr, nbytes)) == NULL)
		wn__panic("realloc returned NULL");
	return new;
}

/*  Print message and dump core - called for fatal errors.
 */
void
wn__panic(mesg)
const char *mesg;
{
	fprintf(stderr, "Fatal internal error (wn): %s (aborting)\n", mesg);
	fflush(stderr);
	abort();
}

/* ARGSUSED */
void
wn_bell(wn)
int wn;
{
#ifdef X11
	XBell(wn__Dpy, 0);
	XFlush(wn__Dpy);
#endif /* X11 */

#ifdef SUNVIEW
	static struct timeval wait_tv = { 0, 100000 };
	static int kfd = -2;
	int cmd;

	if (kfd == -2)
		kfd = open("/dev/kbd", 0);
	if (kfd != -1) {
		cmd = KBD_CMD_BELL;
		(void) ioctl(kfd, KIOCCMD, &cmd);
		(void) syscall(SYS_select, 0, (int *)NULL, (int *)NULL, (int *)NULL, &wait_tv);
		cmd = KBD_CMD_NOBELL;
		(void) ioctl(kfd, KIOCCMD, &cmd);
	}
#endif /* SUNVIEW */
}

static char	*current_selection = NULL;
static int	current_selection_len = 0;

#ifdef SUNVIEW
static char	*get_sel_data;
int		wn__Lost_selection = FALSE;
#endif /* SUNVIEW */

void
wn_set_selection(buf, nbytes)
const char *buf;
int nbytes;
{
#ifdef X11
	current_selection = store_set_selection(buf, nbytes);
	current_selection_len = nbytes;
	XSetSelectionOwner(wn__Dpy, XA_PRIMARY, WN_TO_W(WN_STDWIN)->w_win, CurrentTime);
	XStoreBytes(wn__Dpy, current_selection, nbytes);
	XFlush(wn__Dpy);
#endif /* X11 */

#ifdef SUNVIEW
	static Seln_client	s_client;
	static int	first_time = 1;
	Seln_rank	ret;
	char		header_buf[100];
	int		fd;

	if (first_time) {
		first_time = 0;

		s_client = seln_create(func_key_proc, send_selection,
			NULL);
		if (s_client == NULL) {
			wn__panic("seln_create failed in set_selection");
		}
		(void) notify_do_dispatch();
	}

	current_selection = store_set_selection(buf, nbytes);
	if ((ret = seln_acquire(s_client, SELN_PRIMARY)) != SELN_PRIMARY) {
		wn__panic("seln_acquire failed in set_selection");
	}

	if ((fd = creat("/tmp/winselection", 0666)) != -1) {
		(void) sprintf(header_buf, "TYPE=1, ITEMS=%d, ITEMBYTES=1, PUBFLAGS=1, PRIVDATA=0\n",
			nbytes);
		write(fd, header_buf, strlen(header_buf));
		write(fd, buf, nbytes);
		close(fd);
	}
#endif /* SUNVIEW */
}

void
wn_get_selection(p_buf, p_nbytes)
const char **p_buf;
int *p_nbytes;
{
#ifdef X11
	XEvent	event;
	static struct	timeval	timeout = {3, 0};
	int	server_fd_mask;
	extern int	errno;
	Atom	actual_type;
	int	actual_format;
	unsigned long	nitems, bytes_after;
	char	*data;
	int	old_inmode;

	old_inmode = wn_inmode(WN_STDWIN, _WN_MININPUT);
	XConvertSelection(wn__Dpy, XA_PRIMARY, XA_STRING, XA_STRING,
		WN_TO_W(WN_STDWIN)->w_win, CurrentTime);
	server_fd_mask = (1 << ConnectionNumber(wn__Dpy));

	do {
		int	result;

		do {
			if (XPending(wn__Dpy) > 0) {
				result = 1;
			} else {
				result = select(32, &server_fd_mask, (int *)NULL,
					(int *)NULL, &timeout);
				if (result == -1 && errno != EINTR) {
					wn__panic("select failed in wn_get_selection");
				}
			}
		} while (result == -1);

		if (result == 0) {
			*p_buf = "";
			*p_nbytes = 0;
			(void) wn_inmode(WN_STDWIN, old_inmode);
			return;
		}

		XNextEvent(wn__Dpy, &event);
		if (event.type == SelectionRequest) wn__send_selection(&event);
	} while (event.type != SelectionNotify);

	if (event.xselection.property == None) {
		*p_buf = XFetchBytes(wn__Dpy, p_nbytes);
		(void) wn_inmode(WN_STDWIN, old_inmode);
		return;
	}

	XGetWindowProperty(wn__Dpy,
		event.xselection.requestor,
		event.xselection.property,
		0L, 1024L,
		False,
		AnyPropertyType,
		&actual_type,
		&actual_format,
		&nitems,
		&bytes_after,
		(unsigned char **)&data);
	
	*p_buf = store_got_selection(data, (int)nitems);
	*p_nbytes = nitems;
	(void) wn_inmode(WN_STDWIN, old_inmode);
	return;
#endif /* X11 */

#ifdef SUNVIEW
	Seln_holder	holder;
	int		context = 0;
	int		len;
	Seln_result	read_proc();

	holder = seln_inquire(SELN_PRIMARY);

	if (holder.state == SELN_NONE) {
		get_from_winselection(p_buf, p_nbytes);
	} else {

		get_sel_data = NULL;

		(void) seln_query(&holder, read_proc, &context,
			SELN_REQ_CONTENTS_ASCII, 0, 0);
		
		if (get_sel_data == NULL || get_sel_data[0] == '\0') {
			get_from_winselection(p_buf, p_nbytes);
		} else {
			len = strlen(get_sel_data);
			*p_buf = store_got_selection(get_sel_data, len);
			*p_nbytes = len;
		}
	}
#endif /* SUNVIEW */
}

#ifdef SUNVIEW
static
get_from_winselection(p_buf, p_nbytes)
char	**p_buf;
int	*p_nbytes;
{
	FILE	*fp;
	char	buffer[2048];
	int	len;

	if ((fp = fopen("/tmp/winselection", "r")) == NULL) {
		*p_buf = "";
		*p_nbytes = 0;
	} else {
		(void) fgets(buffer, sizeof(buffer), fp);
		if (fgets(buffer, sizeof(buffer), fp) == NULL) {
			*p_buf = "";
			*p_nbytes = 0;
		} else {
			len = strlen(buffer);
			*p_buf = store_got_selection(buffer, len);
			*p_nbytes = len;
		}

		(void) fclose(fp);
	}
}
#endif /* SUNVIEW */

#ifdef X11
void
wn__send_selection(ptr_event)
XEvent	*ptr_event;
{
	XSelectionEvent	notify_event;
	XSelectionRequestEvent	*req_event;
	unsigned char *data;

	data = (unsigned char *)((current_selection == NULL) ? ""
							     : current_selection);
	XChangeProperty(ptr_event->xselectionrequest.display,
			ptr_event->xselectionrequest.requestor,
			ptr_event->xselectionrequest.property,
			ptr_event->xselectionrequest.target,
			8,
			PropModeReplace,
			data,
			(current_selection == NULL) ? 0 : current_selection_len);

	req_event = &(ptr_event->xselectionrequest);

	notify_event.type      = SelectionNotify;
	notify_event.display   = req_event->display;
	notify_event.requestor = req_event->requestor;
	notify_event.selection = req_event->selection;
	notify_event.target    = req_event->target;
	notify_event.time      = req_event->time;

	if (req_event->property == None) {
		notify_event.property = req_event->target;
	} else {
		notify_event.property = req_event->property;
	}

	(void) XSendEvent(req_event->display,
			req_event->requestor,
			False,
			0,
			(XEvent *)&notify_event);
	XFlush(wn__Dpy);
}
#endif /* X11 */

#ifdef SUNVIEW
static Seln_result
read_proc(buffer)
Seln_request	*buffer;
{
	if (*buffer->requester.context == 0) {
		if (buffer == (Seln_request *)NULL ||
			*((Seln_attribute *) buffer->data) !=
			SELN_REQ_CONTENTS_ASCII) {
			get_sel_data = "";
			return SELN_FAILED;
		}
		get_sel_data = buffer->data + sizeof(Seln_attribute);
		*buffer->requester.context = 1;
	} else {
		get_sel_data = buffer->data;
	}
	return SELN_SUCCESS;
}

static void
do_nothing()
{}

static Seln_result
send_selection(item, context, length)
Seln_attribute	item;
Seln_replier_data	*context;
int	length;
{
	int	size,
		needed;
	char	*destp;
	void (*old_sig)PROTO((int sig));

	switch(item) {

	case SELN_REQ_CONTENTS_ASCII:
		if (context->context == NULL) {
			if (current_selection == NULL) return SELN_DIDNT_HAVE;
			context->context = current_selection;
		}

		size = strlen(context->context);
		destp = (char *)context->response_pointer;

		needed = size + 4;
		if (size % 4 != 0) {
			needed += 4 - size % 4;
		}

		if (needed <= length) {
			/* It fits */
			strcpy(destp, context->context);
			destp += size;

			while((int)destp % 4 != 0) {
				*destp++ = '\0';
			}

			context->response_pointer = (char **)destp;
			*context->response_pointer++ = 0;

			return SELN_SUCCESS;
		} else {
			strncpy(destp, context->context, length);
			destp += length;
			context->response_pointer = (char **)destp;
			context->context += length;

			return SELN_CONTINUED;
		}
		/* NOTREACHED */

	case SELN_REQ_YIELD:
		*context->response_pointer++ = (char *)SELN_SUCCESS;
		wn__Lost_selection = TRUE;
		write(Sunview_wakeup_pipe[1], "X", 1);
		break;
	
	case SELN_REQ_BYTESIZE:
		if (current_selection == NULL) return SELN_DIDNT_HAVE;
		*context->response_pointer++ = (char *)strlen(current_selection);

		return SELN_SUCCESS;
	
	case SELN_REQ_END_REQUEST:
		return SELN_SUCCESS;

	default:
		return SELN_UNRECOGNIZED;
	}
	/* NOTREACHED */
}

int
wn__setup_sunview_wakeup_pipe()
{
	if (pipe(Sunview_wakeup_pipe) == -1)
		return -1;
	return Sunview_wakeup_pipe[0];
}

static void
func_key_proc(client_data, args)
char	*client_data;
Seln_function_buffer	*args;
{
}
#endif /* SUNVIEW */

static char *
store_set_selection(data, nbytes)
const char *data;
int nbytes;
{
	static char *copy = NULL;
	static size_t malloc_size = 1024;

	if (copy == NULL)
		copy = wn__e_malloc(malloc_size);

	while (nbytes >= malloc_size) {
		malloc_size = malloc_size * 2;
		copy = wn__e_realloc(copy, malloc_size);
	}

	memcpy(copy, data, nbytes);
	copy[nbytes] = '\0';
	return copy;
}

static char *
store_got_selection(data, nbytes)
const char *data;
int nbytes;
{
	static char *copy = NULL;
	static size_t malloc_size = 1024;

	if (copy == NULL)
		copy = wn__e_malloc(malloc_size);

	while (nbytes >= malloc_size) {
		malloc_size = malloc_size * 2;
		copy = wn__e_realloc(copy, malloc_size);
	}

	memcpy(copy, data, nbytes);
	return copy;
}

#ifdef NO_MEMXXX
char *
memcpy(dst, src, nbytes)
register char *dst;
register const char *src;
int nbytes;
{
	char *res;

	res = dst;
	while (--nbytes >= 0)
		*dst++ = *src++;
	return res;
}

int
memcmp(m1, m2, nbytes)
register char *m1, *m2;
int nbytes;
{
	for (; --nbytes >= 0; ++m1, ++m2)
		if (*m1 != *m2)
			return m1 - m2;
	return 0;
}

char *
memset(m, c, nbytes)
register char *m;
int c, nbytes;
{
	char *res;

	res = m;
	while (--nbytes >= 0)
		*m++ = c;
	return res;
}

char *
memchr(m, c, nbytes)
register char *m;
int c, nbytes;
{
	register char *lim;

	for (lim = m + nbytes; m < lim; ++m)
		if (*m == c)
			return m;
	return NULL;
}
#endif
