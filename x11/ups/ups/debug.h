/* debug.h - bit values for the -debug flag */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)debug.h	1.5 4/7/91 (UKC) */

/*  These flags are set by the -dbflags command line switch.
 *  They mostly enable debugging output (useful in the first
 *  stages of porting ups to a machine with no existing useful
 *  debugger).
 */

/*  This flag has no global purpose - it is used (usually temporarily)
 *  to alter the behaviour of bits of code for testing.
 */
#define DBFLAG_MISC			0x0001
 
/*  If stack.c is compiled with -DSTACK_DEBUG and this flag is set,
 *  stack frame information is printed to stderr by build_stack_trace().
 */
#define DBFLAG_STACK			0x0002

/*  This flag makes some of the as_*.c disassemblers try to produce
 *  output that looks more like the output of dbx (even if this means
 *  making the output worse).
 */
#define DBFLAG_DBXASM			0x0004

/*  This flag makes the $debug commands asm and asmsrc label instrunctions
 *  with addresses of the form "main+0x44" rather than absolute hex.
 */
#define DBFLAG_ASM_OFFSET_ADDRS		0x0008

/*  This flag makes the output of asm and asmsrc line buffered.
 */
#define DBFLAG_ASM_LINEBUF		0x0010

/*  This flag makes ups print out C interpreter pseudo-assembler
 *  when breakpoint code is edited.
 */
#define DBFLAG_SHOW_CI_CODE		0x0020

/*  If proc.c is compiled with -DPTRACE_DEBUG and this flag is set,
 *  the arguments and results of ptrace calls are logged to stderr.
 */
#define DBFLAG_PTRACE			0x0040

/*  This flag enables the boundary moving code in ups, which is now
 *  switched off by default.
 */
#define DBFLAG_MVLINES			0x0080

/*  This flag turns off the stop button in ups (i.e. you can't select
 *  stop to stop the target).  I think this is causing spurious signals.
 */
#define DBFLAGS_NO_STOP			0x0100

extern unsigned long Debug_flags;
