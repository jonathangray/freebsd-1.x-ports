/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.

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

/* zht2.c */
/* Level 2 halftone operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "dict.h"
#include "dparam.h"
#include "iname.h"		/* for name_eq */
#include "state.h"
#include "store.h"

/* Structures (should be shared with gsht.c) */
typedef struct screen_params_s {
	float frequency;
	float angle;
	float (*spot_function)(P2(floatp, floatp));
} screen_params;
typedef struct threshold_params_s {
	int width;
	int height;
	const byte *thresholds;
} threshold_params;

/* Structures for component names in halftone dictionaries */
typedef struct screen_names_s {
	ref Frequency;
	ref Angle;
	ref SpotFunction;
} screen_names;
typedef struct threshold_names_s {
	ref Width;
	ref Height;
	ref Thresholds;
} threshold_names;

/* Forward references */
private int dict_screen_params(P4(const ref *, const screen_names *,
  screen_params *, ref *));
private int dict_threshold_params(P3(const ref *, const threshold_names *,
  threshold_params *));

/* Keys in halftone dictionaries: */
static ref name_HalftoneType;
static ref name_TransferFunction;
/* Type 1: */
static screen_names names_type1;
static ref name_AccurateScreens;
static ref name_ActualFrequency;
static ref name_ActualAngle;
/* Type 2: */
static screen_names names_type2[4];
/* Type 3: */
static threshold_names names_type3;
/* Type 4: */
static threshold_names names_type4[4];
/* Type 5: */
static ref color_names[8];
/* Red, Green, Blue, Gray must be 0-3. */
#define name_Red color_names[0]
#define name_Green color_names[1]
#define name_Blue color_names[2]
#define name_Gray color_names[3]
#define name_Cyan color_names[4]
#define name_Magenta color_names[5]
#define name_Yellow color_names[6]
#define name_Black color_names[7]
static ref name_Default;

/* Initialization */
private void
zht2_init(void)
{	static const names_def htn[] = {

	/* Create the names of the halftone dictionary keys. */
	   { "HalftoneType", &name_HalftoneType },
	   { "TransferFunction", &name_TransferFunction },
		/* Type 1: */
	   { "Frequency", &names_type1.Frequency },
	   { "Angle", &names_type1.Angle },
	   { "SpotFunction", &names_type1.SpotFunction },
	   { "AccurateScreens", &name_AccurateScreens },
	   { "ActualFrequency", &name_ActualFrequency },
	   { "ActualAngle", &name_ActualAngle },
		/* Type 2: */
	   { "RedFrequency", &names_type2[0].Frequency },
	   { "RedAngle", &names_type2[0].Angle },
	   { "RedSpotFunction", &names_type2[0].SpotFunction },
	   { "GreenFrequency", &names_type2[1].Frequency },
	   { "GreenAngle", &names_type2[1].Angle },
	   { "GreenSpotFunction", &names_type2[1].SpotFunction },
	   { "BlueFrequency", &names_type2[2].Frequency },
	   { "BlueAngle", &names_type2[2].Angle },
	   { "BlueSpotFunction", &names_type2[2].SpotFunction },
	   { "GrayFrequency", &names_type2[3].Frequency },
	   { "GrayAngle", &names_type2[3].Angle },
	   { "GraySpotFunction", &names_type2[3].SpotFunction },
		/* Type 3: */
	   { "Width", &names_type3.Width },
	   { "Height", &names_type3.Height },
	   { "Thresholds", &names_type3.Thresholds },
		/* Type 4: */
	   { "RedWidth", &names_type4[0].Width },
	   { "RedHeight", &names_type4[0].Height },
	   { "RedThresholds", &names_type4[0].Thresholds },
	   { "GreenWidth", &names_type4[1].Width },
	   { "GreenHeight", &names_type4[1].Height },
	   { "GreenThresholds", &names_type4[1].Thresholds },
	   { "BlueWidth", &names_type4[2].Width },
	   { "BlueHeight", &names_type4[2].Height },
	   { "BlueThresholds", &names_type4[2].Thresholds },
	   { "GrayWidth", &names_type4[3].Width },
	   { "GrayHeight", &names_type4[3].Height },
	   { "GrayThresholds", &names_type4[3].Thresholds },
		/* Type 5 only: */
	   { "Red", &name_Red },
	   { "Green", &name_Green },
	   { "Blue", &name_Blue },
	   { "Gray", &name_Gray },
	   { "Cyan", &name_Cyan },
	   { "Magenta", &name_Magenta },
	   { "Yellow", &name_Yellow },
	   { "Black", &name_Black },
	   { "Default", &name_Default },

	/* Mark the end of the initialized name list. */
	   names_def_end
	};

	init_names(htn);
}


/* - .currenthalftone <dict> 0 */
/* - .currenthalftone <frequency> <angle> <proc> 1 */
/* - .currenthalftone <red_freq> ... <gray_proc> 2 */
private int
zcurrenthalftone(register os_ptr op)
{	if ( !r_has_type(&istate.halftone, t_null) )
	{	/* Screen was set by sethalftone. */
		push(2);
		op[-1] = istate.halftone;
		make_int(op, 0);
	}
	else if ( 1 )
	{	/* Screen was set by setscreen. */
		float freq, angle;
		float (*proc)(P2(floatp, floatp));
		gs_currentscreen(igs, &freq, &angle, &proc);
		push(4);
		make_real(op - 3, freq);
		make_real(op - 2, angle);
		op[-1] = istate.screen_procs.gray;
		make_int(op, 1);
	}
	else
	{	/* Screen was set by setcolorscreen. */
		push(13);
		make_real(op, 2);
	}
	return 0;
}

/* <dict> .sethalftone1 - */
private int
zsethalftone1(register os_ptr op)
{	screen_params par;
	ref spot;
	int code;
	check_dict_read(*op);
	code = dict_screen_params(op, &names_type1, &par, &spot);
	if ( code < 0 ) return code;
	/* NYI */
	pop(1);
	return 0;
}

/* <dict> .sethalftone2 - */
private int
zsethalftone2(register os_ptr op)
{	screen_params par[4];
	ref spot[4];
	int i;
	check_dict_read(*op);
	for ( i = 0; i < 4; i++ )
	{	int code = dict_screen_params(op, &names_type2[i],
					      &par[i], &spot[i]);
		if ( code < 0 ) return code;
	}
	/* NYI */
	pop(1);
	return 0;
}

/* <dict> .sethalftone3 - */
private int
zsethalftone3(register os_ptr op)
{	threshold_params par;
	int code;
	check_dict_read(*op);
	code = dict_threshold_params(op, &names_type3, &par);
	if ( code < 0 ) return code;
	/* NYI */
	pop(1);
	return 0;
}

/* <dict> .sethalftone4 - */
private int
zsethalftone4(register os_ptr op)
{	threshold_params par[4];
	int i;
	check_dict_read(*op);
	for ( i = 0; i < 4; i++ )
	{	int code = dict_threshold_params(op, &names_type4[i],
						 &par[i]);
		if ( code < 0 ) return code;
	}
	/* NYI */
	pop(1);
	return 0;
}

/* <dict> .sethalftone5 - */
private int
zsethalftone5(register os_ptr op)
{	uint length;
	typedef union { struct { screen_params p; ref r; } s; threshold_params t; } ht_params;
	ht_params *phts;
	int index;
	ht_params *p;
	ref elt[2];			/* key, value */
	check_dict_read(*op);
	length = dict_length(op);
	phts = (ht_params *)alloc(length, sizeof(ht_params), ".sethalftone5");
	if ( phts == 0 )
		return_error(e_VMerror);
	index = dict_first(op);
	p = phts;
	/* BOGUS: doesn't save the name, .... */
	while ( (index = dict_next(op, index, &elt[0])) >= 0 )
	{	int code;
		check_dict_read(elt[1]);
		if ( (code = dict_screen_params(&elt[0], &names_type1,
				&p->s.p, &p->s.r)) == e_undefined )
		{	code = dict_threshold_params(&elt[0], &names_type3,
						     &p->t);
		}
		if ( code < 0 ) return code;
	}
	/* NYI */
	pop(1);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zht2_op_defs[] = {
	{"0.currenthalftone", zcurrenthalftone},
	{"1.sethalftone1", zsethalftone1},
	{"1.sethalftone2", zsethalftone2},
	{"1.sethalftone3", zsethalftone3},
	{"1.sethalftone4", zsethalftone4},
	{"1.sethalftone5", zsethalftone5},
	op_def_end(zht2_init)
};

/* ------ Internal routines ------ */

/* Extract frequency, angle, and spot function from a dictionary. */
private int
dict_screen_params(const ref *pdict, const screen_names *pnames,
  screen_params *psp, ref *pproc)
{	int code;
	ref *sproc;
	if ( (code = dict_float_param(pdict, &pnames->Frequency, 0.0,
				      &psp->frequency)) < 0 ||
	     (code = dict_float_param(pdict, &pnames->Angle, 0.0,
     				      &psp->angle)) < 0 ||
	     (code = dict_find(pdict, &pnames->SpotFunction, &sproc)) < 0
	   )
		return code;
	check_proc(*sproc);
	*pproc = *sproc;
	return 0;
}

/* Extract width, height, and thresholds from a dictionary. */
private int
dict_threshold_params(const ref *pdict, const threshold_names *pnames,
  threshold_params *ptp)
{	int code;
	ref *tstring;
	if ( (code = dict_int_param(pdict, &pnames->Width, 1, 0x7fff, -1,
				    &ptp->width)) < 0 ||
	     (code = dict_int_param(pdict, &pnames->Height, 1, 0x7fff, -1,
				    &ptp->height)) < 0 ||
	     (code = dict_find(pdict, &pnames->Thresholds, &tstring)) < 0
	   )
		return code;
	check_read_type(*tstring, t_string);
	if ( r_size(tstring) != (long)ptp->width * ptp->height )
		return_error(e_rangecheck);
	ptp->thresholds = tstring->value.const_bytes;
	return 0;
}
