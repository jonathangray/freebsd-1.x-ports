/*
 * patchlevel.h --
 *
 * This file does nothing except define a "patch level" for Tk.
 * The patch level has the form "X.YpZ" where X.Y is the base
 * release, and Z is a serial number that is used to sequence
 * patches for a given release.  Thus 3.6p1 is the first patch
 * to release 3.6, 3.6p2 is the patch that follows 3.6p1, and
 * so on.  The patch level ensures that patches are applied in
 * the correct order and only to appropriate sources.
 */

#define TK_PATCH_LEVEL "3.6p1"
