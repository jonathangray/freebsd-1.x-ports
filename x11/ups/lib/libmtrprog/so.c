/* so.c - source file i/o handling */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char shared_so_c_sccsid[] = "@(#)so.c	1.22 15/9/92 (UKC)";

#include "ifdefs.h"

#include <sys/types.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "alloc.h"
#include "so.h"
#include "strcache.h"
#include "utils.h"

#define LINES_PER_BLOCK		128

#ifndef O_RDONLY
#define O_RDONLY	0
#endif
#ifndef L_SET
#define L_SET		0
#endif

typedef struct blockst {
	off_t bl_offset;
	off_t bl_lines[LINES_PER_BLOCK];
	struct blockst *bl_next;
} block_t;

typedef struct {
	int mf_fd;
	char *mf_name;
	int mf_last_used;
} mfd_t;

typedef struct sost {
	char *so_name;
	so_line_callback_t so_line_callback;
	so_info_func_t so_get_info;
	char *so_arg;
	strcache_id_t so_strcache_id;
	time_t so_mtime;
	off_t so_size;
	long so_nlines;
	long so_max_linelen;
	block_t *so_block;
	char *so_lastline;
	long so_lastline_size;
	off_t so_lastline_offset;
	char *so_peekbuf;
	int so_peekbuf_size;
	int so_tabwidth;
} so_t;

static block_t *new_block PROTO((void));
static void free_block_list PROTO((block_t *block));
static void readline PROTO((so_t *so, off_t offset));
static void get_line_offsets PROTO((so_t *so));
static int last_line_changed PROTO((so_t *so));

static void get_fd_slot PROTO((mfd_t *mf));
static void free_fd_slot PROTO((mfd_t *mf));

static void close_mfd PROTO((char *arg));
int get_mfd_info PROTO((char *arg, so_info_t *si));
static int get_input_from_mfd PROTO((char *arg,
				     off_t offset, char *buf, int nbytes));

/*  Maximum number of simultaneously open file descriptors.
 */
#define MAXFDS	10

/*  Array for keeping track of least recently used file descriptors.
 */
static mfd_t *Mfdtab[MAXFDS];

/*  Use_time keeps track of the last used time of a file descriptor.
 *  Each time we use a file descriptor, we set mf_last_used to
 *  this variable and increment it.
 */
static long Use_time = 0;

static int Default_tabwidth = 8;

ALLOC_NEW_FREELIST(static,block_t,block,bl_next)

#define MAXPATHS	20

typedef struct pathlist_s {
	const char *pl_path;
	struct pathlist_s *pl_next;
} pathlist_t;

static pathlist_t *Pathlist = NULL, *Pathlist_tail;

static int Max_pathlen = 0;

void
so_add_to_source_path(path)
const char *path;
{
	pathlist_t *pl;

	pl = (pathlist_t *)e_malloc(sizeof(pathlist_t));
	pl->pl_path = strsave(path);
	pl->pl_next = NULL;

	if (Pathlist == NULL)
		Pathlist = pl;
	else
		Pathlist_tail->pl_next = pl;
	Pathlist_tail = pl;
		
	if (strlen(path) > Max_pathlen)
		Max_pathlen = strlen(path);
}

static void
get_fd_slot(mf)
mfd_t *mf;
{
	long oldest_time;
	int i, oldest;

	oldest = 0;		 /* to satisfy gcc */
	oldest_time = Use_time;

	for (i = 0; i < MAXFDS; i++) {
		if (Mfdtab[i] == NULL) {
			Mfdtab[i] = mf;
			return;
		}
		if (Mfdtab[i]->mf_last_used < oldest_time) {
			oldest = i;
			oldest_time = Mfdtab[i]->mf_last_used;
		}
	}
	
	/*  Fd table full, so must close least recently used fd.
	 */
	close(Mfdtab[oldest]->mf_fd);
	Mfdtab[oldest]->mf_fd = -1;
	Mfdtab[oldest] = mf;
}

static void
free_fd_slot(mf)
mfd_t *mf;
{
	int i;

	for (i = 0; i < MAXFDS; i++) {
		if (Mfdtab[i] == mf) {
			Mfdtab[i] = NULL;
			return;
		}
	}
	panic("so not in Mfdtab in free_fd_slot");
}

static void
close_mfd(arg)
char *arg;
{
	mfd_t *mf;

	mf = (mfd_t *)arg;
	if (mf->mf_fd != -1) {
		close(mf->mf_fd);
		free_fd_slot(mf);
	}
	free((char *)mf);
}

static int
get_input_from_mfd(arg, offset, buf, nbytes)
char *arg;
off_t offset;
char *buf;
int nbytes;
{
	mfd_t *mf;
	int fd;

	mf = (mfd_t *)arg;

	if (mf->mf_fd == -1) {
		get_fd_slot(mf);
		if ((mf->mf_fd = open(mf->mf_name, O_RDONLY)) == -1)
			panic("can't reopen %s");
	}
	fd = (int)mf->mf_fd;
	mf->mf_last_used = Use_time++;

	if (lseek(fd, offset, L_SET) == -1)
		return -1;
	return read(fd, buf, nbytes);
}

so_id_t
so_open_file(name, line_callback)
const char *name;
so_line_callback_t line_callback;
{
	char *nbuf;
	int fd;
	mfd_t *mf;

	if (*name == '/' || Pathlist == NULL) {
		if ((fd = open(name, O_RDONLY)) == -1)
			return 0;
		nbuf = strsave(name);
	}
	else {
		pathlist_t *pl;

		fd = -1; /* to satisy gcc */
		nbuf = e_malloc(Max_pathlen + 1 + strlen(name) + 1);

		for (pl = Pathlist; pl != NULL; pl = pl->pl_next) {
			(void) sprintf(nbuf, "%s/%s", pl->pl_path, name);
			if ((fd = open(nbuf, O_RDONLY)) != -1)
				break;
		}

		if (pl == NULL) {
			free(nbuf);
			return 0;
		}
	}

	mf = (mfd_t *)e_malloc(sizeof(mfd_t));
	get_fd_slot(mf);
	mf->mf_name = nbuf;
	mf->mf_fd = fd;
	mf->mf_last_used = Use_time++;
	return so_open_via_func(nbuf, get_input_from_mfd, close_mfd,
				get_mfd_info, (char *)mf, line_callback);
}

static void
readline(so, offset)
so_t *so;
off_t offset;
{
	const char *iptr, *ilim;
	char *obuf, *optr, *olim;
	int ch, oldindex, tabspaces;
	long ilen;

	iptr = sc_get_string(so->so_strcache_id, offset, '\n', &ilen);
	if (iptr == NULL)
		iptr = "";
	ilim = iptr + ilen;

	optr = obuf = so->so_peekbuf;
	olim = obuf + so->so_peekbuf_size - 1;
	tabspaces = 0;
	for (;;) {
		if (tabspaces > 0) {
			ch = ' ';
			--tabspaces;
		}
		else {
			if (iptr == ilim)
				break;
			ch = *iptr++;
			if (ch == '\t') {
				tabspaces = (so->so_tabwidth - 1) -
					       (optr - obuf) % so->so_tabwidth;
				ch = ' ';
			}
		}
		if (optr == olim) {
			so->so_peekbuf_size *= 2;
			oldindex = optr - obuf;
			obuf = so->so_peekbuf = e_realloc(obuf, so->so_peekbuf_size);
			optr = obuf + oldindex;
			olim = obuf + so->so_peekbuf_size - 1;
		}
		if (ch == '\b' && optr > obuf)
			--optr;
		else if (ch == '\0')
			*optr++ = 128;
		else
			*optr++ = ch;
	}
	*optr++ = '\0';
}

so_id_t
so_open_via_func(name, input_func, close_func, get_info_func, arg, line_callback)
const char *name;
so_input_func_t input_func;
so_close_func_t close_func;
so_info_func_t get_info_func;
char *arg;
so_line_callback_t line_callback;
{
	so_t *so;

	so = (so_t *) e_malloc(sizeof(so_t));

	so->so_name = strsave(name);
	so->so_line_callback = line_callback;
	so->so_nlines = 0;
	so->so_max_linelen = 0;
	so->so_block = NULL;
	so->so_strcache_id = sc_make_strcache(input_func, close_func, arg);
	so->so_get_info = get_info_func;
	so->so_arg = arg;
	so->so_peekbuf_size = 128;
	so->so_peekbuf = e_malloc(so->so_peekbuf_size);
	so->so_lastline_size = 128;
	so->so_lastline = e_malloc(so->so_lastline_size + 1);
	so->so_tabwidth = Default_tabwidth;

	get_line_offsets(so);

	return (so_id_t)so;
}

int
so_set_tabwidth(so_id, tabwidth)
so_id_t so_id;
int tabwidth;
{
	so_t *so;
	int oldval;

	if (tabwidth < 1)
		panic("ts < 1 on sst");
	so = (so_t *)so_id;

	oldval = so->so_tabwidth;
	so->so_tabwidth = tabwidth;
	return oldval;
}

int
so_set_default_tabwidth(tabwidth)
int tabwidth;
{
	int oldval;

	if (tabwidth < 1)
		panic("ts < 1 on sst");

	oldval = Default_tabwidth;
	Default_tabwidth = tabwidth;
	return oldval;
}

void
so_close(so_id)
so_id_t so_id;
{
	so_t *so;

	so = (so_t *)so_id;

	free_block_list(so->so_block);
	sc_close_strcache(so->so_strcache_id);
	free(so->so_name);
	free(so->so_peekbuf);
	free(so->so_lastline);
	free((char *)so);
}

static void
get_line_offsets(so)
so_t *so;
{
	off_t *lptr, *llim;
	long len;
	block_t *bl, *last_block;
	off_t pos;
	so_info_t sibuf;
	const char *line;

	so->so_nlines = 0;
	last_block = so->so_block;
	if (last_block == NULL) {
		lptr = llim = NULL;
		pos = 0;
	}
	else {
		for (;;) {
			bl = last_block->bl_next;
			if (bl == NULL)
				break;
			so->so_nlines += LINES_PER_BLOCK;
			last_block = bl;
		}
		lptr = last_block->bl_lines;
		llim = last_block->bl_lines + LINES_PER_BLOCK;
		pos = last_block->bl_offset;

		sc_file_has_grown(so->so_strcache_id);
	}

	while ((line = sc_get_string(so->so_strcache_id, pos, '\n', &len)) != NULL) {
		int specials_offset;
		const char *cptr, *lim;

		specials_offset = 0;
		lim = line + len;
		for (cptr = line; cptr < lim; ++cptr) {
			if (*cptr == '\b') {
				if (cptr - line + specials_offset > 1)
					specials_offset -= 2;
			}
			else if (*cptr == '\t') {
				int cpos;

				cpos = cptr - line + specials_offset;
				specials_offset += (so->so_tabwidth - 1) -
							cpos % so->so_tabwidth;
			}
		}
		if (len + specials_offset > so->so_max_linelen)
			so->so_max_linelen = len + specials_offset;

		if (lptr == llim) {
			bl = new_block();
			bl->bl_offset = pos;
			bl->bl_next = NULL;

			if (so->so_block == NULL)
				so->so_block = bl;
			else
				last_block->bl_next = bl;
			last_block = bl;

			lptr = bl->bl_lines;
			llim = bl->bl_lines + LINES_PER_BLOCK;
		}

		*lptr++ = pos;
		pos += len + ((*cptr != '\0') ? 1 : 0);

		if (so->so_line_callback != NULL &&
		    (*so->so_line_callback)((so_id_t)so, so->so_nlines, pos))
			break;
		++so->so_nlines;
	}

	so->so_size = pos;
	if (so->so_get_info != NULL && (*so->so_get_info)(so->so_arg, &sibuf) == 0)
		so->so_mtime = sibuf.si_mtime;
	else
		so->so_mtime = 0;

	if (so->so_nlines != 0) {
		const char *cptr;
		long llen;

		so->so_lastline_offset = lptr[-1];
		cptr = sc_get_string(so->so_strcache_id, so->so_lastline_offset,
								'\n', &llen);
		
		if (llen >= so->so_lastline_size) {
			free(so->so_lastline);

			do {
				so->so_lastline_size *= 2;
			} while (llen >= so->so_lastline_size);

			so->so_lastline = e_malloc(so->so_lastline_size + 1);
		}

		if (cptr == NULL)
			so->so_lastline[0] = '\0';
		else {
			memcpy(so->so_lastline, cptr, llen);
			so->so_lastline[llen] = '\0';
		}
	}
}

long
so_get_size(so_id)
so_id_t so_id;
{
	return ((so_t *)so_id)->so_size;
}

const char *
so_get_name(so_id)
so_id_t so_id;
{
	return ((so_t *)so_id)->so_name;
}

int
so_get_max_linelen(so_id)
so_id_t so_id;
{
	return ((so_t *)so_id)->so_max_linelen;
}

int
so_get_nlines(so_id)
so_id_t so_id;
{
	return ((so_t *)so_id)->so_nlines;
}

static int
last_line_changed(so)
so_t *so;
{
	const char *line;
	long junk_len;

	if (so->so_nlines == 0)
		return FALSE;
	if (so->so_lastline == NULL)
		return TRUE;
	line = sc_get_string(so->so_strcache_id, so->so_lastline_offset,
								'\n', &junk_len);
	return strncmp(so->so_lastline, line, strlen(so->so_lastline)) != 0;
}

void
so_read_more(so_id, reread)
so_id_t so_id;
bool reread;
{
	so_t *so;

	so = (so_t *)so_id;
	if (reread) {
		free_block_list(so->so_block);
		so->so_block = NULL;
		so->so_nlines = 0;
		sc_forget_buffers(so->so_strcache_id);
	}
	get_line_offsets(so);
}

int
get_mfd_info(arg, si)
char *arg;
so_info_t *si;
{
	struct stat stbuf;

	if (fstat(((mfd_t *)arg)->mf_fd, &stbuf) != 0)
		return FALSE;
	si->si_mtime = stbuf.st_mtime;
	si->si_size = stbuf.st_size;
	return 0;
}

int
so_has_changed(so_id, p_reread)
so_id_t so_id;
int *p_reread;
{
	so_t *so;
	so_info_t sibuf;

	so = (so_t *)so_id;

	if (so->so_get_info == NULL || (*so->so_get_info)(so->so_arg, &sibuf) != 0)
		return FALSE;

	if (sibuf.si_mtime == so->so_mtime && sibuf.si_size == so->so_size)
		return FALSE;

	*p_reread = sibuf.si_size <= so->so_size || last_line_changed(so);
	so->so_size = sibuf.si_size;
	return TRUE;
}

long
so_mod_time(so_id)
so_id_t so_id;
{
	so_info_t sibuf;
	so_t *so;

	so = (so_t *)so_id;
	if (so->so_get_info == NULL || (*so->so_get_info)(so->so_arg, &sibuf) != 0)
		return 0;
	return sibuf.si_mtime;
}

char *
so_getline(so_id, lnum)
so_id_t so_id;
int lnum;
{
	block_t *bl;
	so_t *so;
	int nblocks;

	so = (so_t *)so_id;

	if (lnum < 0 || lnum >= so->so_nlines)
		panic("lnum out of range in so_getline");

	bl = so->so_block;
	for (nblocks = lnum / LINES_PER_BLOCK; nblocks > 0; --nblocks)
		bl = bl->bl_next;

	readline(so, bl->bl_lines[lnum % LINES_PER_BLOCK]);
	return so->so_peekbuf;
}
