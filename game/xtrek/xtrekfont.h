/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Definitions of character offsets within the XTrek font.
 */
#define SHIP_BASE	64
#define SHIP_MULT	16

#define EXP_GLYPHS_LEFT	0
#define EXP_GLYPHS_RIGHT 5
#define CLOUD_GLYPH	10
#define ETorp_GLYPH	11
#define MTorp_GLYPH	12
#define Shield_GLYPH	13
#define YShield_GLYPH	14
#define RShield_GLYPH	15
#define PLANET_GLYPHS	32
#define MPLANET_GLYPHS	48

/* Definitions of various character sizes */

/**** These are the ships ****/
#define VIEWS		16
#define ship_width	16
#define ship_height	16

/**** These are the explosions ****/
#define EX_FRAMES	5
#define ex_width	64
#define ex_height	64


#define cloud_width 7
#define cloud_height 7

#define etorp_width 3
#define etorp_height 3

#define mtorp_width 3
#define mtorp_height 3

#define crossw 15
#define crossh 15

#define crossmask_width 15
#define crossmask_height 15

#define planet_width 30
#define planet_height 30

#define mplanet_width 16
#define mplanet_height 16

#define shield_width 20
#define shield_height 20

#define ship_hheight   (ship_height>>1)
#define ship_hwidth   (ship_width>>1)
#define ex_hheight   (ex_height>>1)
#define ex_hwidth   (ex_width>>1)
#define cloud_hheight (cloud_height>>1)
#define cloud_hwidth (cloud_width>>1)
#define etorp_hheight (etorp_height>>1)
#define etorp_hwidth (etorp_width>>1)
#define mtorp_hheight (mtorp_height>>1)
#define mtorp_hwidth (mtorp_width>>1)
#define crossmask_hheight (crossmask_height>>1)
#define crossmask_hwidth (crossmask_width>>1)
#define planet_hheight (planet_height>>1)
#define planet_hwidth (planet_width>>1)
#define mplanet_hheight (mplanet_height>>1)
#define mplanet_hwidth (mplanet_width>>1)
#define shield_hheight (shield_height>>1)
#define shield_hwidth (shield_width>>1)
