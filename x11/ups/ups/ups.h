/* ups.h - typedefs etc that appear in most source files */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ups.h	1.6 26/4/92 (UKC) */

/*  Opaque handle on a target process or core file.
 */
typedef struct { int dummy; } *proc_t;

/*  Type used for target addresses.
 */
typedef unsigned long taddr_t;

/*  Bit pattern of a NULL pointer as read from the target address space.
 *
 *  Note that ups has never been tested (and probably would not work)
 *  with this value anything other than zero.
 */
#define TADDR_NULL	((taddr_t)0)
