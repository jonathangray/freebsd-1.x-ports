/* arg_redir.c - handle Bourne shell and csh type i/o redirection */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_redir_sccsid[] = "@(#)arg_redir.c	1.10 26/4/92 (UKC)";

#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#include <errno.h>
#include <stdlib.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "arg.h"

typedef enum actionen {
	RDA_CLOSE,		/* close(fd1)		*/
	RDA_DUP,		/* dup2(fd1, fd2)	*/
	RDA_DUP_AND_CLOSE	/* dup2(fd1, fd2); close(fd1) */
} action_t;

typedef struct rdlistst {
	action_t rdl_action;
	int rdl_fd1;
	int rdl_fd2;
	struct rdlistst *rdl_next;
} rdlist_t;

#ifdef DEBUG
#define rda_close(fd)		errf("\tclose(%d)", fd)
#define rda_dup2(fd1, fd2)	errf("\tdup2(%d, %d)", fd1, fd2)
#else
#define rda_close(fd)		close(fd)
#define rda_dup2(fd1, fd2)	dup2(fd1, fd2)
#endif /* !DEBUG */

static int dtablesize PROTO((void));
static int getfd PROTO((const char **p_s, int *p_fd));
static rdlist_t *make_rdl PROTO((action_t action, int fd1, int fd2, rdlist_t **p_first, rdlist_t *last));

static int
dtablesize()
{
	static int dts = 0;

	if (dts == 0)
		dts = getdtablesize();
	return dts;
}

static int
getfd(p_s, p_fd)
const char **p_s;
int *p_fd;
{
	int fd;
	const char *s;

	s = *p_s;
	fd = 0;
	while (*s != '\0' && isdigit(*s))
		fd = fd * 10 + *s++ - '0';
	if (fd < 0 || fd > dtablesize()) {
		errf("file descriptor %d is out of range", fd);
		return -1;
	}
	*p_fd = fd;
	*p_s = s;
	return 0;
}

static rdlist_t *
make_rdl(action, fd1, fd2, p_first, last)
action_t action;
int fd1, fd2;
rdlist_t **p_first, *last;
{
	rdlist_t *rdl;

	rdl = (rdlist_t *) e_malloc(sizeof(rdlist_t));
	rdl->rdl_action = action;
	rdl->rdl_fd1 = fd1;
	rdl->rdl_fd2 = fd2;
	if (last != NULL)
		last->rdl_next = rdl;
	else
		*p_first = rdl;
	rdl->rdl_next = NULL;
	return rdl;
}

/*  Parse a redirection (but not the filename if any being redirected to).
 *  We recognise the sh forms ">", "<", ">>", "dig>" and "dig>&dig"
 *  We also recognise the csh form ">&".
 *  We complain about "<<" as we aren't in the business of parsing scripts.
 */
redirtype_t
arg_get_redir(p_s, p_fdaddr, p_lrdl)
const char **p_s;
int **p_fdaddr;
long *p_lrdl;
{
	redirtype_t redirtype;
	int rfd, outfd, redirch;
	const char *s, *save_s;
	rdlist_t *rdlhead, *rdl, *last;

	last = NULL;
	rdlhead = *(rdlist_t **)p_lrdl;
	for (rdl = rdlhead; rdl != NULL; rdl = rdl->rdl_next)
		last = rdl;
	s = save_s = *p_s;
	for (; *s != '\0' && isdigit(*s); s++)
		;
	redirch = *s++;
	if (redirch != '>' && redirch != '<')
		return RD_NOT_REDIR;

	if (isdigit(*save_s)) {
		if (getfd(&save_s, &rfd) != 0)
			return RD_ERROR;
	}
	else
		rfd = (redirch == '<') ? 0 : 1;

	rdl = last = make_rdl(RDA_DUP_AND_CLOSE, -1, rfd, &rdlhead, last);
	if (redirch == '>') {
		redirtype = RD_CREATE;
		if (*s == '>') {
			redirtype = RD_APPEND;
			s++;
		}
		if (*s == '&' && s[1] != '-' && !isdigit(s[1])) {
			last = make_rdl(RDA_DUP, rfd, 2, &rdlhead, last);
			s++;
		}
	}
	else  {
		if (*s == '<') {
			errf("can't handle << redirection");
			return RD_ERROR;
		}
		redirtype = RD_READ;
	}

	if (*s != '&') {
		/*  Need a following filename
		 */
		*p_fdaddr = &rdl->rdl_fd1;
	}
	else {
		/*  dig>&dig or dig>&-
		 */
		if (redirtype == RD_APPEND) {
			errf("illegal redirection >>&");
			return RD_ERROR;
		}
		s++;
		if (*s == '-') {
			s++;
			rdl->rdl_fd1 = rfd;
			rdl->rdl_action = RDA_CLOSE;
		}
		else {
			if (!isdigit(*s)) {
				errf("missing digit after %c&", redirch);
				return RD_ERROR;
			}
			if (getfd(&s, &outfd) == -1)
				return RD_ERROR;
			rdl->rdl_action = RDA_DUP;
			rdl->rdl_fd1 = outfd;
		}
	}
	*p_lrdl = (long) rdlhead;
	*p_s = s;
	return redirtype;
}

void
arg_tidy_redirs_in_parent(lrdl)
long lrdl;
{
	rdlist_t *rdl, *next;

	for (rdl = (rdlist_t *)lrdl; rdl != NULL; rdl = next) {
		next = rdl->rdl_next;
		if (rdl->rdl_action == RDA_DUP_AND_CLOSE)
			rda_close(rdl->rdl_fd1);
		free((char *)rdl);
	}
}

void
arg_do_redirs_in_child(lrdl)
long lrdl;
{
	int i;
	long dont_close;
	rdlist_t *rdl;

	dont_close = 0x7;	/* fds 0, 1, and 2 */
	for (rdl = (rdlist_t *)lrdl; rdl != NULL; rdl = rdl->rdl_next) {
		switch(rdl->rdl_action) {
		case RDA_CLOSE:
			rda_close(rdl->rdl_fd1);
			break;
		case RDA_DUP:
			if (rdl->rdl_fd1 != rdl->rdl_fd2)
				rda_dup2(rdl->rdl_fd1, rdl->rdl_fd2);
			dont_close |= 1 << rdl->rdl_fd2;
			break;
		case RDA_DUP_AND_CLOSE:
			if (rdl->rdl_fd1 != rdl->rdl_fd2) {
				rda_dup2(rdl->rdl_fd1, rdl->rdl_fd2);
				rda_close(rdl->rdl_fd1);
			}
			dont_close |= 1 << rdl->rdl_fd2;
			break;
		default:
			panic("bad action %d in dric");
		}
		dont_close |= 1 << rdl->rdl_fd1;
	}
	for (i = dtablesize() - 1; i >= 0; --i)
		if ((dont_close & (1<<i)) == 0)
			rda_close(i);
}

int
arg_open_redir_file(name, redirtype)
const char *name;
redirtype_t redirtype;
{
	int fd;
	const char *mesg;
	extern int errno;

	mesg = NULL;
	switch(redirtype) {
	case RD_READ:
		if ((fd = open(name, O_RDONLY)) == -1)
			mesg = "Can't open %s for reading (%m)";
		break;
	case RD_APPEND:
		if ((fd = open(name, O_WRONLY)) == -1) {
			if (errno != ENOENT)
				mesg = "can't open %s for writing (%m)";
			else if ((fd = creat(name, 0666)) == -1)
				mesg = "Can't create %s (%m)";
		}
		if (fd != -1 && lseek(fd, 0L, L_XTND) == -1) {
			int save_errno;

			mesg = "Can't lseek to end of %s (%m)";
			save_errno = errno;
			(void) close(fd);
			errno = save_errno;
		}
		break;
	case RD_CREATE:
		if ((fd = creat(name, 0666)) == -1)
			mesg = "Can't create %s (%m)";
		break;
	default:
		panic("unknown redirection type in ord");
		fd = -1; /* to satisfy gcc */
	}
	if (mesg != NULL)
		errf(mesg, name);
	return fd;
}
