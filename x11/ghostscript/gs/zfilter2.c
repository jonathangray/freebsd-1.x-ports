/* Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* zfilter2.c */
/* Additional filter creation for Ghostscript */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "dict.h"
#include "dparam.h"
#include "stream.h"
#include "sdct.h"

/* Imported from zfilter.c */
int filter_read(P4(os_ptr, const stream_procs _ds *, stream **, uint));
int filter_write(P4(os_ptr, const stream_procs _ds *, stream **, uint));

/* Names of keys in CCITTFax filter dictionary: */
static ref name_Uncompressed;
static ref name_K;
static ref name_EndOfLine;
static ref name_EncodedByteAlign;
static ref name_Columns;
static ref name_Rows;
static ref name_EndOfBlock;
static ref name_BlackIs1;
static ref name_DamagedRowsBeforeError;
static ref name_FirstBitLowOrder;
/* Names of (additional) keys in DCT filter dictionary: */
static ref name_Colors;
static ref name_HSamples;
static ref name_VSamples;
static ref name_QuantTables;
static ref name_QFactor;
static ref name_HuffTables;
static ref name_ColorTransform;

/* Initialization */
private void
zfilter2_init(void)
{	static const names_def f2n[] = {

	/* Create the names of the known entries in */
	/* CCITTFax filter dictionaries. */
	   { "Uncompressed", &name_Uncompressed },
	   { "K", &name_K },
	   { "EndOfLine", &name_EndOfLine },
	   { "EncodedByteAlign", &name_EncodedByteAlign },
	   { "Columns", &name_Columns },
	   { "Rows", &name_Rows },
	   { "EndOfBlock", &name_EndOfBlock },
	   { "BlackIs1", &name_BlackIs1 },
	   { "DamagedRowsBeforeError", &name_DamagedRowsBeforeError },
	   { "FirstBitLowOrder", &name_FirstBitLowOrder },

	/* Create the names of the (additional) known entries in */
	/* CCITTFax filter dictionaries. */
	   { "Colors", &name_Colors },
	   { "HSamples", &name_HSamples },
	   { "VSamples", &name_VSamples },
	   { "QuantTables", &name_QuantTables },
	   { "QFactor", &name_QFactor },
	   { "HuffTables", &name_HuffTables },
	   { "ColorTransform", &name_ColorTransform },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	init_names(f2n);
}

/* ------ ASCII85 filters ------ */

/* <target> .filter_ASCII85Encode <file> */
extern const stream_procs s_A85E_procs;
int
zA85E(os_ptr op)
{	return filter_write(op, &s_A85E_procs, NULL, 0);
}

/* <source> .filter_ASCII85Decode <file> */
extern const stream_procs s_A85D_procs;
int
zA85D(os_ptr op)
{	return filter_read(op, &s_A85D_procs, NULL, 0);
}

/* ------ CCITTFax filters ------ */

/* Common setup for encoding and decoding filters. */
private int
cf_setup(os_ptr op, CCITTFax_state *pcfs)
{	int code;
	check_type(*op, t_dictionary);
	check_dict_read(*op);
	if ( (code = dict_bool_param(op, &name_Uncompressed, 0,
				     &pcfs->Uncompressed)) < 0 ||
	     (code = dict_int_param(op, &name_K, -9999, 9999, 0,
				    &pcfs->K)) < 0 ||
	     (code = dict_bool_param(op, &name_EndOfLine, 0,
				     &pcfs->EndOfLine)) < 0 ||
	     (code = dict_bool_param(op, &name_EncodedByteAlign, 0,
				     &pcfs->EncodedByteAlign)) < 0 ||
	     (code = dict_int_param(op, &name_Columns, 0, 9999, 1728,
				    &pcfs->Columns)) < 0 ||
	     (code = dict_int_param(op, &name_Rows, 0, 9999, 0,
				    &pcfs->Rows)) < 0 ||
	     (code = dict_bool_param(op, &name_EndOfBlock, 1,
				     &pcfs->EndOfBlock)) < 0 ||
	     (code = dict_bool_param(op, &name_BlackIs1, 0,
				     &pcfs->BlackIs1)) < 0 ||
	     (code = dict_int_param(op, &name_DamagedRowsBeforeError, 0, 9999,
				    0, &pcfs->DamagedRowsBeforeError)) < 0 ||
	     (code = dict_bool_param(op, &name_FirstBitLowOrder, 0,
				     &pcfs->FirstBitLowOrder)) < 0
	   )
		return code;
	pcfs->raster = (pcfs->Columns + 7) >> 3;
	return 0;
}

/* <target> <dict> .filter_CCITTFaxEncode <file> */
extern const stream_procs s_CFE_procs;
extern void s_CFE_init(P2(stream *, CCITTFax_state *));
int
zCFE(os_ptr op)
{	CCITTFax_state cfs;
	stream *s;
	int code = cf_setup(op, &cfs);
	if ( code < 0 ) return code;
	/* We need room for 2 full scan lines + 1 byte to handle 2-D. */
	code = filter_write(op - 1, &s_CFE_procs, &s, cfs.raster * 2 + 1);
	if ( code < 0 ) return code;
	s_CFE_init(s, &cfs);
	pop(1);
	return 0;
}

/* <source> <dict> .filter_CCITTFaxDecode <file> */
extern const stream_procs s_CFD_procs;
extern void s_CFD_init(P2(stream *, CCITTFax_state *));
int
zCFD(os_ptr op)
{	CCITTFax_state cfs;
	stream *s;
	int code = cf_setup(op, &cfs);
	if ( code < 0 ) return code;
	/* We need room for 3 full scan lines to handle 2-D. */
	code = filter_read(op - 1, &s_CFD_procs, &s, cfs.raster * 3 + 1);
	if ( code < 0 ) return code;
	s_CFD_init(s, &cfs);
	pop(1);
	return 0;
}

/* ------ DCT filters ------ */

/* Common setup for encoding and decoding filters. */
private int
dct_setup_samples(os_ptr op, const ref *pname, int num_colors,
  dct_color_params *params, int hvi)
{	int code;
	int i;
	int samples[4];
	samples[0] = samples[1] = samples[2] = samples[3] = 1;
	if ( (code = dict_int_array_param(op, pname, num_colors, samples)) < 0 )
		return code;
	else if ( code != 0 && code != num_colors )
		return_error(e_rangecheck);
	for ( i = 0; i < num_colors; i++ )
	{	if ( samples[i] < 1 || samples[i] > 4 )
			return_error(e_rangecheck);
		params[i].HVSamples[hvi] = samples[i];
	}
	return 0;
}
private int
dct_setup(os_ptr op, DCT_state *pdct)
{	int code;
	int num_colors;
	dct_color_params dcp[4];
	check_type(*op, t_dictionary);
	check_dict_read(*op);
	if ( (code = dict_int_param(op, &name_Columns, 0, 0x7fff, -1,
				    &pdct->Columns)) < 0 ||
	     (code = dict_int_param(op, &name_Rows, 0, 0x7fff, -1,
				    &pdct->Rows)) < 0 ||
	     (code = dict_int_param(op, &name_Colors, 1, 4, -1,
				    &num_colors)) < 0 ||
	     (code = dct_setup_samples(op, &name_HSamples, num_colors,
				       dcp, 0)) < 0 ||
	     (code = dct_setup_samples(op, &name_VSamples, num_colors,
				       dcp, 1)) < 0 ||
	     (code = dict_float_param(op, &name_QFactor, 1.0,
				      &pdct->QFactor)) < 0 ||
	     (code = dict_int_param(op, &name_ColorTransform, 0, 1, 0,
				    &pdct->ColorTransform)) < 0
	   )
		return code;
	pdct->Colors = num_colors;
	/****** QuantTables are NYI ******/
	/****** HuffTables are NYI ******/
	pdct->params = (dct_color_params *)alloc(num_colors,
				sizeof(dct_color_params),
				"dct_setup(params)");
	if ( pdct->params == 0 )
		return_error(e_VMerror);
	memcpy(pdct->params, dcp, num_colors * sizeof(dct_color_params));
	return 0;
}

/* <target> <dict> .filter_DCTEncode <file> */
extern const stream_procs s_DCTE_procs;
extern void s_DCTE_init(P2(stream *, DCT_state *));
int
zDCTE(os_ptr op)
{	DCT_state dcts;
	stream *s;
	int code = dct_setup(op, &dcts);
	if ( code < 0 ) return code;
	code = filter_write(op - 1, &s_DCTE_procs, &s, 0);
	if ( code < 0 )
	{	/****** RELEASE STUFF ******/
		return code;
	}
	s_DCTE_init(s, &dcts);
	pop(1);
	return 0;
}

/* <source> <dict> .filter_DCTDecode <file> */
extern const stream_procs s_DCTD_procs;
extern void s_DCTD_init(P2(stream *, DCT_state *));
int
zDCTD(os_ptr op)
{	DCT_state dcts;
	stream *s;
	int code = dct_setup(op, &dcts);
	if ( code < 0 ) return code;
	code = filter_read(op - 1, &s_DCTD_procs, &s, 0);
	{	/****** RELEASE STUFF ******/
		return code;
	}
	s_DCTD_init(s, &dcts);
	pop(1);
	return 0;
}

/* ------ LZW filters ------ */

/* <target> .filter_LZWEncode <file> */
extern const stream_procs s_LZWE_procs;
extern const uint s_LZWE_table_sizeof;
typedef struct lzw_encode_table_s lzw_encode_table;
extern void s_LZWE_init(P3(stream *, lzw_encode_table *, int));
int
zLZWE_open(os_ptr op, int enhanced)
{	stream *s;
	int code = filter_write(op, &s_LZWE_procs, &s, 0);
	lzw_encode_table *table;
	if ( code < 0 ) return code;
	table = (lzw_encode_table *)alloc(1, s_LZWE_table_sizeof,
					  "filterLZWEncode(table)");
	if ( table == 0 )
		return_error(e_VMerror);
	s_LZWE_init(s, table, enhanced);
	return code;
}
int
zLZWE(os_ptr op)
{	return zLZWE_open(op, 0);
}
int
zALZWE(os_ptr op)
{	return zLZWE_open(op, 1);
}

/* <source> .filter_LZWDecode <file> */
extern const stream_procs s_LZWD_procs;
extern const uint s_LZWD_table_sizeof;
typedef struct lzw_decode_table_s lzw_decode_table;
extern void s_LZWD_init(P3(stream *, lzw_decode_table *, int));
int
zLZWD_open(os_ptr op, int enhanced)
{	stream *s;
	int code = filter_read(op, &s_LZWD_procs, &s, 0);
	lzw_decode_table *table;
	if ( code < 0 ) return code;
	table = (lzw_decode_table *)alloc(1, s_LZWD_table_sizeof,
					  "filterLZWDecode(table)");
	if ( table == 0 )
		return_error(e_VMerror);
	s_LZWD_init(s, table, enhanced);
	return 0;
}
int
zLZWD(os_ptr op)
{	return zLZWD_open(op, 0);
}
int
zALZWD(os_ptr op)
{	return zLZWD_open(op, 1);
}

/* ------ RunLength filters ------ */

/* <target> <record_size> .filter_RunLengthEncode <file> */
extern const stream_procs s_RLE_procs;
extern void s_RLE_init(P2(stream *, uint));
int
zRLE(register os_ptr op)
{	stream *s;
	int code;
	check_type(*op, t_integer);
	if ( (ulong)(op->value.intval) > max_uint )
		return_error(e_rangecheck);
	code = filter_write(op - 1, &s_RLE_procs, &s, 0);
	if ( code < 0 )
		return code;
	s_RLE_init(s, (uint)(op->value.intval));
	pop(1);
	return 0;
}

/* <source> .filter_RunLengthDecode <file> */
extern const stream_procs s_RLD_procs;
extern void s_RLD_init(P1(stream *));
int
zRLD(os_ptr op)
{	stream *s;
	int code = filter_read(op, &s_RLD_procs, &s, 0);
	if ( code < 0 ) return code;
	s_RLD_init(s);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zfilter2_op_defs[] = {
	{"1.filter_ASCII85Encode", zA85E},
	{"1.filter_ASCII85Decode", zA85D},
	{"2.filter_CCITTFaxEncode", zCFE},
	{"2.filter_CCITTFaxDecode", zCFD},
#if 0			/* NYI */
	{"2.filter_DCTEncode", zDCTE},
	{"2.filter_DCTDecode", zDCTD},
#endif
	{"1.filter_LZWDecode", zLZWD},
	{"1.filter_LZWEncode", zLZWE},
	{"2.filter_RunLengthEncode", zRLE},
	{"1.filter_RunLengthDecode", zRLD},
	op_def_end(zfilter2_init)
};
