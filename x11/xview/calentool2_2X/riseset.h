/*
 * $Id: riseset.h,v 1.1 1993/08/17 09:42:31 alm Exp $
 */
/*
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Copyright 1988, 1989, 1991 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 

/*
 * index into riseset_buf[]
 */
#define	B_GMT	0	/* Greenwich Mean Time (GMT) (==UTC) */
#define	B_TDT	1	/* Terrestrial Dynamical Time */
#define	B_LCT	2	/* Local Civil Time */
#define	B_LMT	3	/* Local Mean Time */
#define	B_POM	4	/* Phase Of Moon */
#define	B_JLD	5	/* Julian Day */
#define	B_GST	6	/* Greenwich Mean Sideral Time */
#define	B_LST	7	/* Local Sidereal Time */
#define	B_LHA	8	/* L.H.A. of Sun */
#define	B_SDE	9	/* Declination of Sun */
#define	B_SAZ	10	/* Azimuth of Sun */
#define	B_SEL	11	/* Elevation of Sun */
#define	B_SRD	12	/* Sun Rise Today */
#define	B_SSD	13	/* Sun Set Today */
#define	B_SRT	14	/* Sun Rise Tomorrow */
#define	B_SST	15	/* Sun Set Tomorrow */
#define	B_MRD	16	/* Moon Rise Today */
#define	B_MSD	17	/* Moon Set Today */
#define	B_MRT	19	/* Moon Rise Tomorrow */
#define	B_MST	20	/* Moon Set Tomorrow */
#define B_DMY	21	/* Day-Month-Year */
#define B_LOD	22	/* Length of Day (Sunlight) */

#define B_SIZE	(B_LOD+1)
