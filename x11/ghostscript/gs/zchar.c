/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* zchar.c */
/* Character operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gxarith.h"
#include "gxfixed.h"			/* for gstype1.h */
#include "gxmatrix.h"			/* for font.h */
#include "gschar.h"
#include "gxdevice.h"			/* for gxfont.h */
#include "gxfont.h"
#include "gxtype1.h"			/* should be gstype1.h, but */
					/* we need sizeof(gs_type1_state) */
					/* so we can use stack allocation */
#include "gzpath.h"			/* for type1addpath: see below */
#include "gzstate.h"
#include "alloc.h"
#include "dict.h"
#include "font.h"
#include "estack.h"
#include "ilevel.h"
#include "iname.h"
#include "state.h"
#include "store.h"

/* Procedures exported for zchar2.c; */
/* these are also internal forward references. */
int op_show_setup(P2(os_ptr, int /*bool*/));
int op_show_continue(P1(os_ptr));
int op_show_continue_dispatch(P2(os_ptr, int));
gs_show_enum *op_show_find(P0());
/* gs_show_enum *op_show_senum(P0()); */
/* ref *op_show_psslot(P0()); */
int op_show_cleanup(P1(os_ptr));
void op_show_free(P0());

/* All the character rendering operators use the execution stack */
/* for loop control -- see estack.h for details. */
/* The information pushed by these operators is as follows: */
/*	the enumerator (t_string, but points to a gs_show_enum); */
/*	a slot for the procedure for kshow or the stream for [x][y]show */
/*		(t_string, but points to a stream), unused otherwise; */
/*	the procedure to be called at the end of the enumeration */
/*		(t_operator, but called directly, not by the interpreter); */
/*	the usual e-stack mark (t_null). */
#define snumpush 4
#define senum (gs_show_enum *)(esp->value.bytes)
#define sslot esp[-1]
#define seproc esp[-2]

/* Forward references */
private int show_enum_setup(P2(os_ptr, op_proc_p));
private int finish_show(P1(os_ptr));
private int finish_stringwidth(P1(os_ptr));

/* <string> show - */
int
zshow(register os_ptr op)
{	int code = op_show_setup(op, 1);
	if ( code < 0 ) return code;
	if ( (code = gs_show_n_init(senum, igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	pop(1);  op--;
	return op_show_continue(op);
}

/* <ax> <ay> <string> ashow - */
int
zashow(register os_ptr op)
{	int code;
	float axy[2];
	if (	(code = num_params(op - 1, 2, axy)) < 0 ||
		(code = op_show_setup(op, 1)) < 0
	   )
		return code;
	if ( (code = gs_ashow_n_init(senum, igs, axy[0], axy[1], (char *)op->value.bytes, r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	pop(3);  op -= 3;
	return op_show_continue(op);
}

/* <cx> <cy> <char> <string> widthshow - */
int
zwidthshow(register os_ptr op)
{	int code;
	float cxy[2];
	check_type(op[-1], t_integer);
	if ( (gs_char)(op[-1].value.intval) != op[-1].value.intval )
		return_error(e_rangecheck);
	if (	(code = num_params(op - 2, 2, cxy)) < 0 ||
		(code = op_show_setup(op, 1)) < 0
	   )
		return code;
	if ( (code = gs_widthshow_n_init(senum, igs, cxy[0], cxy[1],
					 (gs_char)op[-1].value.intval,
					 (char *)op->value.bytes,
					 r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	pop(4);  op -= 4;
	return op_show_continue(op);
}

/* <cx> <cy> <char> <ax> <ay> <string> awidthshow - */
int
zawidthshow(register os_ptr op)
{	int code;
	float cxy[2], axy[2];
	check_type(op[-3], t_integer);
	if ( (gs_char)(op[-3].value.intval) != op[-3].value.intval )
		return_error(e_rangecheck);
	if (	(code = num_params(op - 4, 2, cxy)) < 0 ||
		(code = num_params(op - 1, 2, axy)) < 0 ||
		(code = op_show_setup(op, 1)) < 0
	   )
		return code;
	if ( (code = gs_awidthshow_n_init(senum, igs, cxy[0], cxy[1],
					  (gs_char)op[-3].value.intval,
					  axy[0], axy[1],
					  (char *)op->value.bytes,
					  r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	pop(6);  op -= 6;
	return op_show_continue(op);
}

/* <proc> <string> kshow - */
int
zkshow(register os_ptr op)
{	int code;
	check_proc(op[-1]);
	if ( (code = op_show_setup(op, 1)) < 0 ) return code;
	if ( (code = gs_kshow_n_init(senum, igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	sslot = op[-1];		/* save kerning proc */
	pop(2);  op -= 2;
	return op_show_continue(op);
}

/* Common finish procedure for all show operations. */
/* Doesn't have to do anything. */
private int
finish_show(os_ptr op)
{	return 0;
}

/* <string> stringwidth <wx> <wy> */
int
zstringwidth(register os_ptr op)
{	int code;
	check_read_type(*op, t_string);
	code = show_enum_setup(op, finish_stringwidth);
	if ( code < 0 ) return code;
	if ( (code = gs_stringwidth_n_init(senum, igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	op_show_free();
		return code;
	   }
	pop(1);  op--;
	return op_show_continue(op);
}
/* Finishing procedure for stringwidth. */
/* Pushes the accumulated width. */
private int
finish_stringwidth(register os_ptr op)
{	gs_point width;
	gs_show_width(senum, &width);
	push(2);
	make_real(op - 1, width.x);
	make_real(op, width.y);
	return 0;
}

/* <string> <outline_bool> charpath - */
int
zcharpath(register os_ptr op)
{	int code;
	check_type(*op, t_boolean);
	code = op_show_setup(op - 1, 1);
	if ( code < 0 ) return code;
	if ( (code = gs_charpath_n_init(senum, igs, (char *)op[-1].value.bytes, r_size(op - 1), op->value.index)) < 0 )
	   {	op_show_free();
		return code;
	   }
	pop(2);  op -= 2;
	return op_show_continue(op);
}

/* <wx> <wy> <llx> <lly> <urx> <ury> setcachedevice - */
int
zsetcachedevice(register os_ptr op)
{	float wbox[6];
	gs_show_enum *penum = op_show_find();
	int code = num_params(op, 6, wbox);
	if ( penum == 0 )
		return_error(e_undefined);
	if ( code < 0 )
		return code;
	if ( (code = gs_setcachedevice(penum, igs, wbox[0], wbox[1], wbox[2], wbox[3], wbox[4], wbox[5])) < 0 )
		return code;
	pop(6);
	return 0;
}

/* <wx> <wy> setcharwidth - */
int
zsetcharwidth(register os_ptr op)
{	float width[2];
	gs_show_enum *penum = op_show_find();
	int code = num_params(op, 2, width);
	if ( penum == 0 )
		return_error(e_undefined);
	if (	code < 0 || 
		(code = gs_setcharwidth(penum, igs, width[0], width[1])) < 0
	   )
		return code;
	pop(2);
	return 0;
}

/* <string> .type1addpath - */
/* <string> <lsbx> <lsby> .type1addpath - */
typedef struct {
	gs_font *pfont;
	fixed *osptr;			/* fake interpreter operand stack */
	fixed ostack[2];
} z1_data;
int
ztype1addpath(register os_ptr op)
{	int code = 0;
	int value;
	gs_show_enum *penum = op_show_find();
	gs_font *pfont = gs_currentfont(igs);
	font_data *pfdata = (font_data *)pfont->client_data;
	gs_type1_state is;		/* stack allocate to avoid sandbars */
	gs_type1_state *pis = &is;
	float sbxy[2];
	gs_point sbpt;
	gs_point *psbpt = 0;
	os_ptr opc = op;
	gs_type1_data tdata;
	z1_data zdata;
	const byte *charstring;
	if ( penum == 0 )
		return_error(e_undefined);
	if ( num_params(op, 2, sbxy) >= 0 )
	{	sbpt.x = sbxy[0];
		sbpt.y = sbxy[1];
		psbpt = &sbpt;
		opc -= 2;
	}
	check_type(*opc, t_string);
	tdata = pfont->data.base.type1_data;
	zdata.pfont = pfont;
	zdata.osptr = zdata.ostack;
	tdata.proc_data = (char *)&zdata;
	if ( r_size(opc) <= tdata.lenIV )
	   {	/* String is empty, or too short.  Just ignore it. */
		goto ret;
	   }
	code = gs_type1_init(pis, penum,
			     gs_show_in_charpath(penum), tdata.PaintType,
			     &tdata);
	if ( code < 0 ) return code;
	charstring = opc->value.const_bytes;
more:	code = gs_type1_interpret(pis, charstring, psbpt, &value);
	charstring = 0;
	switch ( code )
	   {
	case type1_result_seac:
	   {	ref *pcstr;
		ref enc_entry;
		code = array_get(&StandardEncoding, (long)value, &enc_entry);
		if ( code < 0 ) return code;
		if ( dict_find(&pfdata->CharStrings,
			       &enc_entry, &pcstr) <= 0 )
			return_error(e_undefined);
		if ( !r_has_type(pcstr, t_string) )
			return_error(e_invalidfont);
		charstring = pcstr->value.const_bytes;
	   }	goto more;
	case type1_result_callothersubr:
		/* The Type 1 interpreter handles all known othersubrs, */
		/* so this must be an unknown one. */
		code = e_rangecheck;
		break;
	   }
	if ( code < 0 ) return code;
ret:	pop((psbpt == 0 ? 1 : 3));
	return code;
}

/* ------ Auxiliary procedures for type 1 fonts ------ */

int
z1_subr_proc(gs_type1_data *pdata, int index, const byte **pstr)
{	gs_font *pfont = ((z1_data *)(pdata->proc_data))->pfont;
	font_data *pfdata = (font_data *)(pfont->client_data);
	ref *psubr;
	if ( index < 0 || index >= r_size(&pfdata->Subrs) )
		return_error(e_rangecheck);
	psubr = pfdata->Subrs.value.refs + index;
	check_type(*psubr, t_string);
	*pstr = psubr->value.bytes;
	return 0;
}

int
z1_pop_proc(gs_type1_data *pdata, fixed *pf)
{	*pf = *--(((z1_data *)(pdata->proc_data))->osptr);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zchar_op_defs[] = {
	{"3ashow", zashow},
	{"6awidthshow", zawidthshow},
	{"2charpath", zcharpath},
	{"2kshow", zkshow},
	{"6setcachedevice", zsetcachedevice},
	{"2setcharwidth", zsetcharwidth},
	{"1show", zshow},
	{"1stringwidth", zstringwidth},
	{"1.type1addpath", ztype1addpath},
	{"4widthshow", zwidthshow},
		/* Internal operators */
	{"0%finish_show", finish_show},
	{"0%finish_stringwidth", finish_stringwidth},
	{"0%op_show_continue", op_show_continue},
	op_def_end(0)
};

/* ------ Subroutines ------ */

/* Most of these are exported for zchar2.c. */ 

/* Set up for a show operator.  If check_string is true, */
/* the top stack element must be the string to be scanned. */
/* The caller has already done all other argument checking. */
int
op_show_setup(os_ptr op, int check_string)
{	if ( check_string )
	{	check_read_type(*op, t_string);
	}
	return show_enum_setup(op, finish_show);
}
private int
show_enum_setup(os_ptr op, op_proc_p endproc /* end procedure */)
{	gs_show_enum *penum;
	check_estack(snumpush + 2);
	if ( (penum = (gs_show_enum *)alloc(1, gs_show_enum_sizeof, "setup_show")) == 0 )
		return_error(e_VMerror);
	mark_estack(es_show, op_show_cleanup);
	push_op_estack(endproc);
	++esp;
	make_null(esp);		/* reserve sslot */
	++esp;
	make_tasv(esp, t_string, 0, gs_show_enum_sizeof, bytes, (byte *)penum);
	return o_push_estack;
}

/* Continuation operator for character rendering. */
int
op_show_continue(os_ptr op)
{	return op_show_continue_dispatch(op, gs_show_next(senum));
}
int
op_show_continue_dispatch(register os_ptr op, int code)
{	gs_show_enum *penum = senum;
	switch ( code )
	   {
	case 0:				/* all done */
		code = (*real_opproc(&seproc))(op);
		op_show_free();
		return (code >= 0 ? o_pop_estack : code);
	case gs_show_kern:
	   {	ref *pslot = &sslot;
		push(2);
		make_int(op - 1, gs_kshow_previous_char(penum));
		make_int(op, gs_kshow_next_char(penum));
		push_op_estack(op_show_continue);		/* continue after kerning */
		*++esp = *pslot;	/* kerning procedure */
	   }
		return o_push_estack;
	case gs_show_render:
	   {	font_data *pfdata = (font_data *)gs_currentfont(igs)->client_data;
		push(2);
		op[-1] = pfdata->dict;	/* push the font */
		if ( level2_enabled && !r_has_type(&pfdata->BuildGlyph, t_null) )
		{	/* Use BuildGlyph. */
			gs_glyph glyph = gs_show_current_glyph(penum);
			if ( glyph == gs_no_glyph )
				goto err;	/* not possible (!) */
			name_index_ref(glyph, op);
			esp[2] = pfdata->BuildGlyph;
		}
		else
		{	/* Use BuildChar. */
			gs_char chr = gs_show_current_char(penum);
			if ( chr == gs_no_char )
				goto err;	/* only possible for glyphshow */
			make_int(op, chr);
			esp[2] = pfdata->BuildChar;
		}
		push_op_estack(op_show_continue);
		++esp;		/* skip BuildChar or BuildGlyph proc */
	   }
		return o_push_estack;
	default:			/* error */
err:		op_show_free();
		return (code < 0 ? code : e_invalidfont);
	   }
}

/* Find the current show enumerator on the e-stack. */
gs_show_enum *
op_show_find(void)
{	es_ptr ep = esp;
	while ( !(r_is_estack_mark(ep) && estack_mark_index(ep) == es_show) )
	   {	if ( --ep < esbot ) return 0;	/* no mark */
	   }
	return (gs_show_enum *)ep[snumpush - 1].value.bytes;
}

/* Return the current enumerator for the continuation procedure. */
gs_show_enum *
op_show_senum(void)
{	return senum;
}

/* Return the address of the current procedure slot ditto. */
ref *
op_show_psslot(void)
{	return &sslot;
}

/* Discard the show record (after an error, or at the end). */
int
op_show_cleanup(os_ptr op)
{	register es_ptr ep = esp + snumpush;
#define esp ep		/* for senum, sslot */
	if ( r_has_type(&sslot, t_string) )
		alloc_free((char *)sslot.value.bytes, 1, r_size(&sslot),
			   "free_show(stream)");
	alloc_free((char *)senum, 1, gs_show_enum_sizeof, "free_show(enum)");
#undef esp
	return 0;
}
void
op_show_free(void)
{	esp -= snumpush;
	op_show_cleanup(osp);
}
