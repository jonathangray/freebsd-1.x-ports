/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* zchar2.c */
/* Level 2 character operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gschar.h"
#include "gsmatrix.h"		/* for gxfont.h */
#include "gxfixed.h"		/* for gxfont.h */
#include "gxfont.h"
#include "alloc.h"
#include "estack.h"
#include "font.h"
#include "iname.h"
#include "state.h"
#include "store.h"
#include "stream.h"
#include "bnum.h"

/* Imported from zchar.c */
extern int op_show_setup(P2(os_ptr, int /*bool*/));
extern int op_show_continue(P1(os_ptr));
extern int op_show_continue_dispatch(P2(os_ptr, int));
extern gs_show_enum *op_show_find(P0());
extern gs_show_enum *op_show_senum(P0());
extern ref *op_show_psslot(P0());
extern void op_show_free(P0());

/* Table of continuation procedures. */
private int xshow_continue(P1(os_ptr));
private int yshow_continue(P1(os_ptr));
private int xyshow_continue(P1(os_ptr));
static op_proc_p xyshow_continues[4] = {
	0, xshow_continue, yshow_continue, xyshow_continue
};

/* Forward references */
private int cshow_continue(P1(os_ptr));
private int moveshow(P2(os_ptr, int));
private int moveshow_continue(P2(os_ptr, int));

/* <proc> <string> cshow - */
private int
zcshow(os_ptr op)
{	int code;
	check_proc(op[-1]);
	if ( (code = op_show_setup(op, 1)) < 0 ) return code;
	if ( (code = gs_cshow_n_init(op_show_senum(), igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	*op_show_psslot() = op[-1];		/* save kerning proc */
	pop(2);  op -= 2;
	return cshow_continue(op);
}
private int
cshow_continue(os_ptr op)
{	gs_show_enum *penum = op_show_senum();
	int code = gs_show_next(penum);
	if ( code != gs_show_move )
	{	code = op_show_continue_dispatch(op, code);
		if ( code == o_push_estack )	/* must be gs_show_render */
		{	make_op_estack(esp - 1, cshow_continue);
		}
		else if ( code < 0 )
			goto errx;
		return code;
	}
	/* Push the character code and width, and call the procedure. */
	{	gs_show_enum *penum = op_show_senum();
		ref *pslot = op_show_psslot();
		gs_point wpt;
		gs_show_current_width(penum, &wpt);
		push(3);
		make_int(op - 2, gs_show_current_char(penum));
		make_real(op - 1, wpt.x);
		make_real(op, wpt.y);
		push_op_estack(cshow_continue);
		*++esp = *pslot;	/* user procedure */
	}
	return o_push_estack;
errx:	op_show_free();
	return code;
}

/* <charname> glyphshow - */
private int
zglyphshow(os_ptr op)
{	int code;
	check_type(*op, t_name);
	if ( (code = op_show_setup(op, 0)) < 0 )
		return code;
	if ( (code = gs_glyphshow_init(op_show_senum(), igs,
		(gs_glyph)name_index(op))) < 0
	   )
	{	op_show_free();
		return code;
	}
	pop(1);  op--;
	return op_show_continue(op);
}

/* - rootfont <font> */
private int
zrootfont(os_ptr op)
{	gs_font *pfont = gs_rootfont(op_show_find(), igs);
	push(1);
	*op = ((font_data *)(pfont->client_data))->dict;
	return 0;
}

/* <w0x> <w0y> <llx> <lly> <urx> <ury> <w1x> <w1y> <vx> <vy> setcachedevice2 - */
private int
zsetcachedevice2(os_ptr op)
{	float wbox[10];
	gs_show_enum *penum = op_show_find();
	int code = num_params(op, 10, wbox);
	if ( penum == 0 )
		return_error(e_undefined);
	if ( code < 0 ) return code;
	if ( (code = gs_setcachedevice2(penum, igs, wbox[0], wbox[1], wbox[2], wbox[3], wbox[4], wbox[5], wbox[6], wbox[7], wbox[8], wbox[9])) < 0 )
		return code;
	pop(10);
	return 0;
}

/* <string> <numarray|numstring> xshow - */
private int
zxshow(os_ptr op)
{	return moveshow(op, 1);
}

/* <string> <numarray|numstring> yshow - */
private int
zyshow(os_ptr op)
{	return moveshow(op, 2);
}

/* <string> <numarray|numstring> xyshow - */
private int
zxyshow(os_ptr op)
{	return moveshow(op, 3);
}

/* Common code for {x,y,xy}show */
private int
moveshow(os_ptr op, int xymask)
{	int code = op_show_setup(op - 1, 1);
	gs_show_enum *penum = op_show_senum();
	ref *psslot = op_show_psslot();
	stream *s;
	if ( code < 0 ) return code;
	if ( (code = gs_xyshow_n_init(penum, igs, (char *)op[-1].value.bytes, r_size(op - 1)) < 0) )
	{	op_show_free();
		return code;
	}
	s = (stream *)alloc(1, sizeof(stream), "moveshow(stream)");
	if ( s == 0 )
		return_error(e_VMerror);
	make_tasv(psslot, t_string, 0, sizeof(stream), bytes, (byte *)s);
	code = sread_num_array(s, op);
	if ( code < 0 )
	{	op_show_free();
		return code;
	}
	pop(2);  op -= 2;
	return moveshow_continue(op, xymask);
}

/* Continuation procedures */

private int
xshow_continue(os_ptr op)
{	return moveshow_continue(op, 1);
}

private int
yshow_continue(os_ptr op)
{	return moveshow_continue(op, 2);
}

private int
xyshow_continue(os_ptr op)
{	return moveshow_continue(op, 3);
}

/* Get one value from the encoded number string or array. */
/* Sets pvalue->value.realval. */
private int
sget_real(stream *s, ref *pvalue, int read)
{	if ( read )
	{	int code;
		switch ( code = sget_encoded_number(s, pvalue) )
		{
		case t_integer: pvalue->value.realval = pvalue->value.intval;
		case t_real: break;
		case t_null: code = e_rangecheck;
		default: return code;
		}
	}
	else
		pvalue->value.realval = 0;
	return 0;
}

private int
moveshow_continue(os_ptr op, int xymask)
{	int code;
	stream *s = (stream *)(op_show_psslot()->value.bytes);
	gs_show_enum *penum = op_show_senum();
next:	code = gs_show_next(penum);
	if ( code != gs_show_move )
	{	code = op_show_continue_dispatch(op, code);
		if ( code == o_push_estack )	/* must be gs_show_render */
		{	make_op_estack(esp - 1, xyshow_continues[xymask]);
		}
		else if ( code < 0 )
			goto errx;
		return code;
	}
	{	/* Move according to the next value(s) from the stream. */
		ref rwx, rwy;
		code = sget_real(s, &rwx, xymask & 1);
		if ( code < 0 ) goto errx;
		code = sget_real(s, &rwy, xymask & 2);
		if ( code < 0 ) goto errx;
		code = gs_rmoveto(igs, rwx.value.realval, rwy.value.realval);
		if ( code < 0 ) goto errx;
	}
	goto next;
errx:	op_show_free();
	return code;
}

/* ------ Initialization procedure ------ */

op_def zchar2_op_defs[] = {
	{"2cshow", zcshow},
	{"1glyphshow", zglyphshow},
	{"0rootfont", zrootfont},
	{":setcachedevice2", zsetcachedevice2},
	{"2xshow", zxshow},
	{"2xyshow", zxyshow},
	{"2yshow", zyshow},
		/* Internal operators */
	{"0%cshow_continue", cshow_continue},
	{"0%xshow_continue", xshow_continue},
	{"0%yshow_continue", yshow_continue},
	{"0%xyshow_continue", xyshow_continue},
	op_def_end(0)
};
