/* output.c - code for the ups $printf output window */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_output_c_sccsid[] = "@(#)output.c	1.12 26/7/92 (UKC)";

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>

#include <local/ukcprog.h>
#include <mtrprog/so.h>

#include "textwin.h"
#include "output.h"
#include "tdr.h"

typedef struct blocktabst {
	int bt_blocktab_size;
	int bt_nblocks;
	int bt_blocksize;
	int bt_last_block_len;
	char **bt_blocks;
	int bt_nwrites;
	int bt_linepos;
	int bt_linelen;
} blocktab_t;

typedef struct outwinst {
	blocktab_t *ow_blocktab;
	textwin_id_t ow_textwin_id;
	so_id_t ow_so;
	font_t *ow_font;
	int ow_fg;
	int ow_bg;

	int ow_hl_lnum;
	int ow_hl_cnum;
	int ow_hl_nchars;
} outwin_t;

#define NO_HL_LNUM	(-1)

static void get_output_line PROTO((char *arg, int lnum,
			           const char **p_line, int *p_fg, int *p_bg,
			           font_t **p_font, tw_hlinfo_t *hl));

static blocktab_t *make_blocktab PROTO((void));
static int read_from_blocktab PROTO((char *arg, off_t offset,
							char *buf, int nbytes));
static int get_blocktab_info PROTO((char *arg, so_info_t *si));
static void truncate_blocktab_data PROTO((blocktab_t *bt));

static blocktab_t *
make_blocktab()
{
	blocktab_t *bt;

	bt = (blocktab_t *)e_malloc(sizeof(blocktab_t));
	bt->bt_blocktab_size = 1;
	bt->bt_nblocks = 0;
	bt->bt_blocksize = 1024;
	bt->bt_last_block_len = 0;
	bt->bt_blocks = (char **)e_malloc(bt->bt_blocktab_size * sizeof(char *));
	bt->bt_blocks[0] = e_malloc(bt->bt_blocksize);
	bt->bt_nwrites = 0;
	bt->bt_linepos = 0;
	bt->bt_linelen = -1;	/* no line wrapping by default */
	return bt;
}

static int
get_blocktab_info(arg, si)
char *arg;
so_info_t *si;
{
	blocktab_t *bt;

	bt = (blocktab_t *)arg;
	si->si_mtime = bt->bt_nwrites;
	si->si_size = bt->bt_nblocks * bt->bt_blocksize + bt->bt_last_block_len;
	return 0;
}

static int
read_from_blocktab(arg, offset, obuf, nbytes)
char *arg;
off_t offset;
char *obuf;
int nbytes;
{
	const char *iptr, *ilim;
	char *optr, *olim;
	blocktab_t *bt;
	int bno, offset_in_block;

	bt = (blocktab_t *)arg;
	
	iptr = ilim = NULL;
	optr = obuf;
	olim = obuf + nbytes;

	bno = offset / bt->bt_blocksize;
	offset_in_block = offset % bt->bt_blocksize;

	while (optr < olim) {
		if (iptr == ilim) {
			const char *ibuf;

			if (bno > bt->bt_nblocks)
				break;
			ibuf = bt->bt_blocks[bno];
			iptr = ibuf + offset_in_block;
			if (bno == bt->bt_nblocks)
				ilim = ibuf + bt->bt_last_block_len;
			else
				ilim = ibuf + bt->bt_blocksize;
			if (iptr >= ilim)
				break;
			++bno;
			offset_in_block = 0;
		}
		*optr++ = *iptr++;
	}
	return optr - obuf;
}

void
ow_putc(outwin_id, c)
outwin_id_t outwin_id;
int c;
{
	char ch;

	ch = c;
	ow_write(outwin_id, &ch, 1);
}

void
ow_write(outwin_id, buf, nbytes)
outwin_id_t outwin_id;
const char *buf;
int nbytes;
{
	blocktab_t *bt;
	const char *iptr, *ilim, *save_iptr, *save_ilim;
	char *optr, *olim;
	int bno, linelen, linepos;

	bt = ((outwin_t *)outwin_id)->ow_blocktab;

	iptr = buf;
	ilim = buf + nbytes;

	bno = bt->bt_nblocks;
	optr = bt->bt_blocks[bno] + bt->bt_last_block_len;
	olim = bt->bt_blocks[bno] + bt->bt_blocksize;

	linelen = bt->bt_linelen;
	linepos = bt->bt_linepos;

	save_ilim = NULL;
	save_iptr = NULL;	/* to satisfy gcc */

	for (;;) {
		if (iptr >= ilim) {
			if (iptr > ilim)
				panic("iptr botch");

			if (save_ilim == NULL)
				break;

			iptr = save_iptr;
			ilim = save_ilim;
			save_ilim = NULL;
		}

		if (optr == olim) {
			++bno;
			if (bno == bt->bt_blocktab_size) {
				bt->bt_blocktab_size *= 2;
				bt->bt_blocks = (char **)e_realloc((char *)bt->bt_blocks,
							bt->bt_blocktab_size *
								sizeof(char *));
			}
			bt->bt_blocks[bno] = e_malloc(bt->bt_blocksize);
			optr = bt->bt_blocks[bno];
			olim = optr + bt->bt_blocksize;
		}

		/*  Force a newline of we have reached the end of a line.
		 */
		if (linepos == linelen && *iptr != '\n') {
			save_iptr = iptr;
			save_ilim = ilim;
			iptr = "\n";
			ilim = iptr + 1;
		}

		if (*iptr == '\n')
			linepos = 0;
		else
			++linepos;

		*optr++ = *iptr++;
	}

	bt->bt_nblocks = bno;
	bt->bt_last_block_len = optr - bt->bt_blocks[bno];
	bt->bt_linepos = linepos;
	++bt->bt_nwrites;
}

static void
truncate_blocktab_data(bt)
blocktab_t *bt;
{
	bt->bt_nblocks = 0;
	bt->bt_last_block_len = 0;
	++bt->bt_nwrites;
	bt->bt_linepos = 0;
}

outwin_id_t
ow_make_outwin(wn, fg, bg, font)
int wn;
int fg, bg;
font_t *font;
{
	outwin_t *ow;

	ow = (outwin_t *)e_malloc(sizeof(outwin_t));

	ow->ow_blocktab = make_blocktab();
	ow->ow_so = so_open_via_func("<output window>",
				     read_from_blocktab,
				     (so_close_func_t)NULL,
				     get_blocktab_info,
				     (char *)ow->ow_blocktab,
				     (so_line_callback_t)NULL);

	if (wn == -1) {
		ow->ow_textwin_id = NULL;
		ow->ow_blocktab->bt_linelen = 70;
	}
	else {
		ow->ow_textwin_id = tw_make_textwin(wn, bg, font->ft_height);
		ow->ow_blocktab->bt_linelen = wn_get_width(wn) / font->ft_width;
	}

	ow->ow_fg = fg;
	ow->ow_bg = bg;
	ow->ow_font = font;
	ow->ow_hl_lnum = NO_HL_LNUM;

	return (outwin_id_t)ow;
}

void
ow_redraw(outwin_id)
outwin_id_t outwin_id;
{
	outwin_t *ow;

	ow = (outwin_t *)outwin_id;
	tw_redraw_text(ow->ow_textwin_id, TRUE);
	ow->ow_blocktab->bt_linelen =
		tw_get_window_width(ow->ow_textwin_id) / ow->ow_font->ft_width;
}

void
ow_refresh(outwin_id)
outwin_id_t outwin_id;
{
	outwin_t *ow;
	int old_nlines, new_nlines;
	bool reread;

	ow = (outwin_t *)outwin_id;

	old_nlines = so_get_nlines(ow->ow_so);
	if (so_has_changed(ow->ow_so, &reread))
		so_read_more(ow->ow_so, reread);
	new_nlines = so_get_nlines(ow->ow_so);

	td_handle_output(ow->ow_so, old_nlines, new_nlines);

	if (ow->ow_textwin_id != NULL) {
		if (old_nlines == 0) {
			tw_set_text(ow->ow_textwin_id, ow->ow_bg,
				    get_output_line, (char *)ow,
				    new_nlines, new_nlines - 1, TRUE);
		}
		else {
			tw_handle_nlines_delta(ow->ow_textwin_id,
					       old_nlines - 1, 0,
					       new_nlines - old_nlines,
					       new_nlines - 1, TRUE);
		}
	}
}

static void
get_output_line(arg, lnum, p_line, p_fg, p_bg, p_font, hl)
char *arg;
int lnum;
const char **p_line;
int *p_fg, *p_bg;
font_t **p_font;
tw_hlinfo_t *hl;
{
	outwin_t *ow;

	ow = (outwin_t *)arg;
	*p_line = so_getline(ow->ow_so, lnum);
	*p_fg = ow->ow_fg;
	*p_bg = ow->ow_bg;
	*p_font = ow->ow_font;

	if (hl != NULL && lnum == ow->ow_hl_lnum) {
		hl->hl_type = TW_HL_INVERT;
		hl->hl_cnum = ow->ow_hl_cnum;
		hl->hl_nchars = ow->ow_hl_nchars;
	}

}

void
ow_set_textwin_callback_and_data(outwin_id, textwin_callback, data)
outwin_id_t outwin_id;
tw_callback_t textwin_callback;
char *data;
{
	tw_set_callback_and_data(((outwin_t *)outwin_id)->ow_textwin_id,
				 textwin_callback, data);
}

void
ow_goto_ypos(outwin_id, ypos)
outwin_id_t outwin_id;
int ypos;
{
	outwin_t *ow;
	int lnum;

	ow = (outwin_t *)outwin_id;
	lnum = ypos / tw_get_line_height(ow->ow_textwin_id);
	tw_goto(ow->ow_textwin_id, lnum, FALSE);
}

int
ow_scroll(outwin_id, npixels)
outwin_id_t outwin_id;
int npixels;
{
	return tw_scroll(((outwin_t *)outwin_id)->ow_textwin_id, npixels);
}

void
ow_clear(outwin_id)
outwin_id_t outwin_id;
{
	outwin_t *ow;
	int old_nlines;

	ow = (outwin_t *)outwin_id;

	ow->ow_hl_lnum = NO_HL_LNUM;
	old_nlines = so_get_nlines(ow->ow_so);
	truncate_blocktab_data(ow->ow_blocktab);
	so_read_more(ow->ow_so, TRUE);
	tw_handle_nlines_delta(ow->ow_textwin_id, 0, old_nlines, 0, 0, FALSE);
	tw_goto(ow->ow_textwin_id, 0, FALSE);
}

void
ow_page(outwin_id, direction)
outwin_id_t outwin_id;
ow_page_direction_t direction;
{
	outwin_t *ow;
	bool dist;

	ow = (outwin_t *)outwin_id;
	dist = tw_get_window_height(ow->ow_textwin_id) / 2;
	tw_scroll(ow->ow_textwin_id, (direction == OW_PAGE_DOWN) ? dist : -dist);
}

/*  BUG: this is an edited copy of the src_search function.  With some
 *       effort these functions could be combined.
 */
void
ow_search(outwin_id, pattern, forwards)
outwin_id_t outwin_id;
const char *pattern;
bool forwards;
{
	outwin_t *ow;
	tw_search_res_t search_res;
	int start_lnum, start_cnum, lnum, cnum, nchars;

	ow = (outwin_t *)outwin_id;

	if (*pattern == '\0') {
		errf("Can't search for zero length pattern");
		return;
	}

	if (ow->ow_hl_lnum != NO_HL_LNUM &&
				tw_is_visible(ow->ow_textwin_id, ow->ow_hl_lnum)) {
		start_lnum = ow->ow_hl_lnum;
		start_cnum = ow->ow_hl_cnum + (forwards ? ow->ow_hl_nchars : 0);
	}
	else {
		start_lnum = tw_ypos_to_lnum(ow->ow_textwin_id, 0);
		start_cnum = 0;
	}

	search_res = tw_search(ow->ow_textwin_id,
			       pattern, start_lnum, start_cnum, forwards,
			       &lnum, &cnum, &nchars);
	
	switch (search_res) {
	case TW_SR_FOUND:
		ow->ow_hl_lnum = lnum;
		ow->ow_hl_cnum = cnum;
		ow->ow_hl_nchars = nchars;
		break;
	case TW_SR_NOT_FOUND:
		errf("Reached %s of saved output without finding \"%s\"",
					forwards ? "end" : "start", pattern);
		break;
	case TW_SR_SEARCH_ERROR:
		errf("Internal error in searching");
		break;
	case TW_SR_INTERRUPTED:
		errf("Search interrupted");
		break;
	default:
		panic("bad res from ts");
	}

	if (search_res == TW_SR_FOUND || search_res == TW_SR_INTERRUPTED) {
		if (tw_is_visible(ow->ow_textwin_id, lnum))
			tw_redraw_text(ow->ow_textwin_id, FALSE);
		else
			tw_goto(ow->ow_textwin_id, lnum, TRUE);
	}
}
