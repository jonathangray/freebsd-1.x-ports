/*
 *  Copyright (C) 1991 By DeepCore Technologies
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *      DeepCore Technologies
 *	Att: Søren Schmidt 	Email:	sos@kmd-ac.dk
 *	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
 *	DK9210 Aalborg SO	Phone:  +45 9814 8076
 */

#ifndef NULL
#define NULL		0
#endif

#define	SV_PHSYNC	0x0001
#define	SV_NHSYNC	0x0002
#define	SV_PVSYNC	0x0004
#define	SV_NVSYNC	0x0008
#define	SV_INTERLACE	0x0010
#define	SV_FASTCLOCK	0x0020
#define	SV_HALFCLOCK	0x0040
#define	SV_256COLOR	0x0080
#define	SV_HICOLOR	0x0100	

typedef struct {
  int	Flags;		/* option flags */
  int	Clock;		/* oscilator number */
  int	HDisplay;	/* horizontal displayed */
  int	HTotal;		/* horizontal total */
  int	HSyncStart;	/* horizontal sync pulse start */
  int	HSyncEnd;	/* horizontal sync pulse end */
  int	VDisplay;	/* vertical displayed */
  int	VTotal;		/* vertical total */
  int	VSyncStart;	/* vertical sync pulse start */
  int	VSyncEnd;	/* vertical sync pulse end */
} SvgaSetup;

typedef struct {
  unsigned char MISC;		/* Misc Output Register */
  unsigned char CRTC[0x1C];	/* CRTC Controller */
  unsigned char TS[0x08];	/* Timing Sequencer */
  unsigned char GDC[0x09];	/* Graphics Date Controller */
  unsigned char ATC[0x17];	/* Atribute Controller*/
  unsigned char LUT[0x300];	/* Internal Colorlookuptable */
} SvgaRegs;

extern SvgaRegs *SvgaGraphicsMode;
/*
 * function definitions.
 */
int SvgaMode ( int mode , int init );
void SvgaExit ( void );
void SvgaSave ( void );
void SvgaRestore ( void );
SvgaRegs *SvgaSetupRegs ( SvgaSetup *setup , SvgaRegs *mode );
int SvgaSetmode ( SvgaRegs *svgatab );
SvgaRegs *SvgaGetmode ( SvgaRegs *svgatab );
int SvgaDebug ( SvgaRegs *regtab );
