/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
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

/* This is where the xtrek font lives, in case it isn't somewhere
 * standard. Xtrek will dynamically add this directory to the
 * font searchpath if it isn't already there (with sanity checking).
 *
 * Make sure you modify the Imakefile to relect this path if necesary
 */

#define FONTDIR "/usr/X386/lib/X11/fonts/xtrek"
#define FONTNAME "xtrek"

/* Where config files, bitmaps, and such are stored
 * Note that the default doesn't neccesarily need to
 * be in the standard places.
 *
 * Make sure you modify the Imakefile to reflect these paths
 */

#define LIBDIR "/usr/X386/lib/X11/games/xtrek"
#define DEFAULT_CONFIG "/usr/X386/lib/X11/games/xtrek/default.config"

/* name of the program (used for Xdefaults grabbing
 * Note that argv[0] is irrelavant. (!)
 */

#define PROGRAM_NAME "xtrek"

/* The MOTD shown on entering
 */
#define MOTD "/usr/X386/lib/X11/games/xtrek/motd"

/* TURBO_OPTION and TELEPORT_OPTION are documented in xtrekguide.tex
 * They can contribute to significant game unbalancing, so they are
 * not turned on by default. They are implemented mostly because we would
 * be receiving *many* implementations of the same features if this wasn't
 * in the code already.
 */

/* #define TURBO_OPTION */
/* #define TELEPORT_OPTION */

/* If you find the normal robots too easy, try out this option... 
 */

/* #define KILLERROBOTS */
