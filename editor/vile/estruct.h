/*	ESTRUCT:	Structure and preprocessor defines for
			vile.  Reshaped from the original, which
			was for MicroEMACS 3.9

			vile is by Paul Fox
			MicroEmacs was written by Dave G. Conroy
			modified by Steve Wilhite, George Jones
			substantially modified by Daniel Lawrence
*/

/*
 * $Log: estruct.h,v $
 * Revision 1.1  1994/02/01 03:29:17  jkh
 * Initial revision
 *
 * Revision 1.141  1993/09/16  11:07:32  pgf
 * added LPAREN/RPAREN
 *
 * Revision 1.140  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.139  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.138  1993/08/18  16:45:00  pgf
 * added VIEWOK flag for functions that execute macros.  it says it's okay
 * to execute them in view mode, even though they have the UNDO bit set
 *
 * Revision 1.137  1993/08/18  15:10:36  pgf
 * OPT_XTERM is now turned on for X11, as an ease-of-use feature for the
 * user.  it doesn't actually do anything
 *
 * Revision 1.136  1993/08/18  11:52:12  pgf
 * turn off OPT_WORKING for USG builds
 *
 * Revision 1.135  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.134  1993/08/05  14:39:55  pgf
 * removed conflicting #define USG 0 from djgpp settings
 *
 * Revision 1.133  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.132  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.131  1993/07/20  18:07:24  pgf
 * change which ScratchName macro is used for STDC
 *
 * Revision 1.130  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.129  1993/07/09  14:00:38  pgf
 * fix for predefined GO32 in DJ GCC
 *
 * Revision 1.128  1993/07/06  16:55:12  pgf
 * corrected calloc macro definition
 *
 * Revision 1.127  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.126  1993/07/06  12:32:50  pgf
 * added b_modtime_at_warn to BUFFER
 *
 * Revision 1.125  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.124  1993/06/30  10:05:54  pgf
 * change ABS to ABSM, since osf/1 defines ABS in a system header
 *
 * Revision 1.123  1993/06/29  17:58:56  pgf
 * allow undo to preserve DOT's offset, by overloading two more fields in
 * the LINE struct to hold the forward and backward offsets
 *
 * Revision 1.122  1993/06/28  15:27:40  pgf
 * deleted vestigial #define TIMING
 *
 * Revision 1.121  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.120  1993/06/23  21:32:25  pgf
 * added "undolimit" mode, and made undo able to restore unmodified state
 * to buffer, based on a new type of stack separator
 *
 * Revision 1.119  1993/06/22  10:27:31  pgf
 * new macros for undo stack separators
 *
 * Revision 1.118  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.117  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.116  1993/05/24  15:25:41  pgf
 * tom's 3.47 changes, part b
 *
 * Revision 1.115  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.114  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.113  1993/05/05  11:41:05  pgf
 * backspc is now handled separately from chartypes[backspc]
 *
 * Revision 1.112  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.111  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.110  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.109  1993/04/21  15:41:27  pgf
 * changed NAMEC from SPACE to TAB
 *
 * Revision 1.108  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.107  1993/04/02  10:59:13  pgf
 * vms needs setjmp.h
 *
 * Revision 1.106  1993/04/01  14:43:39  pgf
 * fix for NeXT
 *
 * Revision 1.105  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.104  1993/04/01  12:01:07  pgf
 * add setjmp.h
 *
 * Revision 1.103  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.102  1993/03/17  10:00:29  pgf
 * initial changes to make VMS work again
 *
 * Revision 1.101  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.100  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.99  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.98  1993/02/24  09:31:02  pgf
 * added macro for catching ">>filename", for appending
 *
 * Revision 1.97  1993/02/12  10:42:32  pgf
 * don't redefine "const" on linux
 *
 * Revision 1.96  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.95  1993/01/23  13:38:23  foxharp
 * macros for process exit codes for VMS,
 *
 * Revision 1.94  1993/01/16  10:28:35  foxharp
 * new chartypes (scrtch, shpipe), new mode (autobuffer), new macros
 * (isShellOrPipe, isScratchName, isInternalName, for_each_buffer,
 * for_each_window)
 *
 * Revision 1.93  1993/01/12  08:48:43  foxharp
 * tom dickey's changes to support "set number", i.e. line numbering
 *
 * Revision 1.92  1992/12/20  14:38:46  foxharp
 * added lflipmark macro, for 'v' command
 *
 * Revision 1.91  1992/12/14  08:29:18  foxharp
 * changes for lint support, from Tom Dickey.  Still not nearly 100% lint free,
 * and may not get much closer -- the code gets too ugly for me....
 *
 * Revision 1.90  1992/12/05  13:53:56  foxharp
 * fix APOLLO signal types
 *
 * Revision 1.89  1992/12/04  09:28:35  foxharp
 * added APOLLO
 *
 * Revision 1.88  1992/12/02  09:13:16  foxharp
 * changes for "c-shiftwidth"
 *
 * Revision 1.87  1992/11/19  08:59:00  foxharp
 * added "qident" bit definition, for "qualified" (a la c++) identifiers
 *
 * Revision 1.86  1992/08/19  22:55:55  foxharp
 * made NFILEN bigger -- much safer
 *
 * Revision 1.85  1992/08/07  17:32:47  pgf
 * don't use bcopy
 *
 * Revision 1.84  1992/08/06  23:51:47  foxharp
 * nextsw() now finds next "shiftwidth stop", like nextab() finds next tabstop
 *
 * Revision 1.83  1992/08/04  20:09:59  foxharp
 * bsd386 doesn't have bfill, but it does have mem{set,cpy}
 *
 * Revision 1.82  1992/07/24  18:22:51  foxharp
 * deleted local atoi() routine -- now we use the system's copy
 *
 * Revision 1.81  1992/07/20  22:45:53  foxharp
 * if using TERMCAP, define TTputc directly as putchar(),
 * for some performance gain
 *
 * Revision 1.80  1992/07/17  19:16:45  foxharp
 * change "sun" to "SUNOS" to make way for solaris
 *
 * Revision 1.79  1992/07/13  20:08:17  foxharp
 * "terse" is now a boolean mode rather than a variable, and
 * added "tagsrelative" mode
 *
 * Revision 1.78  1992/07/13  09:24:41  foxharp
 * added b_dispfname, which will allow printing shorter paths
 * for files by trimming leading 'pwd' matches
 *
 * Revision 1.77  1992/07/08  08:20:22  foxharp
 * made the _rest_ of the command flags long.  sigh.
 *
 * Revision 1.76  1992/07/07  08:36:07  foxharp
 * redefined the command flags as long constants, so the upper 16 bits don't
 * get lost on 16 bit machines.  (DOS)
 *
 * Revision 1.75  1992/07/01  17:00:59  foxharp
 * added ZIBMPC define, and comment re: COLOR support
 *
 * Revision 1.74  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.73  1992/06/22  08:33:28  foxharp
 * another ifdef for UNIXPC
 *
 * Revision 1.72  1992/06/12  22:23:42  foxharp
 * changes for separate 'comments' r.e. for formatregion
 *
 * Revision 1.71  1992/06/04  19:42:37  foxharp
 * use #ifdef __STDC__ in favor of #if
 *
 * Revision 1.70  1992/06/01  20:35:59  foxharp
 * added "tabinsert" support
 *
 * Revision 1.69  1992/05/25  21:27:14  foxharp
 * some func declarations moved in here, from edef.h and other .c files
 *
 * Revision 1.68  1992/05/25  21:25:35  foxharp
 * bad control char in log comment
 *
 * Revision 1.67  1992/05/20  18:57:16  foxharp
 * added a/ux, fixed my stdarg ifdef
 *
 * Revision 1.66  1992/05/19  18:28:04  foxharp
 * more proto-isms
 *
 * Revision 1.65  1992/05/19  09:15:45  foxharp
 * more prototype fixups
 *
 * Revision 1.64  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.63  1992/04/30  17:53:09  pgf
 * fixed ifdef on HAVE_MKDIR
 *
 * Revision 1.62  1992/04/14  08:55:38  pgf
 * added support for OSF1 (affects termio only)
 *
 * Revision 1.61  1992/04/10  19:52:40  pgf
 * make sure svr3 implies usg
 *
 * Revision 1.60  1992/04/10  18:51:31  pgf
 * rearrange some config ifdefs, so makefile can do config with targets
 *
 * Revision 1.59  1992/03/26  09:15:23  pgf
 * system choice ifdef cleanup
 *
 * Revision 1.58  1992/03/25  19:13:17  pgf
 * BSD portability changes
 *
 * Revision 1.57  1992/03/24  22:45:55  pgf
 * corrected typos
 *
 * Revision 1.56  1992/03/19  23:33:35  pgf
 * added b_linecount to BUFFER, so finderr can count from bottom of buffer
 *
 * Revision 1.55  1992/03/19  23:17:52  pgf
 * SIGT for signals, and linux portability
 *
 * Revision 1.54  1992/03/07  10:21:29  pgf
 * added AIX support (also need to link against -lcurses)
 *
 * Revision 1.53  1992/03/03  09:35:52  pgf
 * added support for getting "words" out of the buffer via variables --
 * needed _nonspace character type
 *
 * Revision 1.52  1992/03/01  18:38:40  pgf
 * compilation error #if COLOR
 *
 * Revision 1.51  1992/02/17  09:18:55  pgf
 * turn off DEBUG and DEBUGM for release
 *
 * Revision 1.50  1992/02/17  09:00:28  pgf
 * added "showmode" support, and
 * kill registers now hold unsigned chars
 *
 * Revision 1.49  1992/01/22  20:30:08  pgf
 * added HPUX "support", and big warning for non-CRYPT support
 *
 * Revision 1.48  1992/01/10  07:10:46  pgf
 * added VAL_SWIDTH
 *
 * Revision 1.47  1992/01/03  23:29:37  pgf
 * added UNIXPC support (on hints from Eric Krohn), and
 * changed the b_fname element of a BUFFER to a pointer
 *
 * Revision 1.46  1992/01/01  16:16:52  pgf
 * fixed typo
 *
 * Revision 1.45  1991/11/18  08:35:32  pgf
 * ifdef botch
 *
 * Revision 1.44  1991/11/16  18:29:15  pgf
 * ifdef cleanup
 *
 * Revision 1.43  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.42  1991/11/08  13:18:23  pgf
 * added FIOABRT error
 *
 * Revision 1.41  1991/11/06  23:28:08  pgf
 * added fence character type macros
 * getfence() will scan for a fence if not on one to begin with.  it'll
 * scan in either direction, depending on arg to matchfence or matchfenceback
 *
 * Revision 1.40  1991/11/01  14:24:11  pgf
 * added lsprintf decl
 *
 * Revision 1.39  1991/11/01  14:10:35  pgf
 * make matchlen part of the regexp struct:  mlen, and
 * changed regmust from pointer to offset into program, to make
 * regexps relocatable
 *
 * Revision 1.38  1991/10/28  14:24:04  pgf
 * eliminated some useless macros, made gacount part of the BUFFER struct
 *
 * Revision 1.37  1991/10/27  01:58:53  pgf
 * added declarations/definitions for the regexp stuff (courtesy Henry Spencer),
 * and added the new regex value definitions
 *
 * Revision 1.36  1991/10/24  13:05:52  pgf
 * conversion to new regex package -- much faster
 *
 * Revision 1.35  1991/10/23  12:05:37  pgf
 * support for the NeXT machine
 *
 * Revision 1.34  1991/10/18  10:56:54  pgf
 * modified VALUE structures and lists to make them more easily settable
 *
 * Revision 1.33  1991/10/15  03:10:00  pgf
 * added backspacelimit and taglength
 *
 * Revision 1.32  1991/10/10  12:33:33  pgf
 * changes to support "block malloc" of line text -- now for most files
 * there is are two mallocs and a single read, no copies.  previously there
 * were two mallocs per line, and two copies (stdio's and ours).  This change
 * implies that lines and line text should not move between buffers, without
 * checking that the text and line struct do not "belong" to the buffer.
 *
 * Revision 1.31  1991/09/26  13:08:55  pgf
 * created window values, moved list mode there
 *
 * Revision 1.30  1991/09/19  13:35:29  pgf
 * MDEXACT is now MDIGNCASE, and names are now more vi-compliant
 *
 * Revision 1.29  1991/09/10  12:29:57  pgf
 * added b_wline macro into b_wtraits
 *
 * Revision 1.28  1991/09/10  01:08:28  pgf
 * added BEL define
 *
 * Revision 1.27  1991/09/10  00:44:29  pgf
 * added ESC defines
 *
 * Revision 1.26  1991/08/16  10:58:54  pgf
 * added the third flavor of insertmode
 *
 * Revision 1.25  1991/08/13  02:48:59  pgf
 * added select and poll selectors, and alphabetized the VAL_XXX's
 *
 * Revision 1.24  1991/08/12  15:06:21  pgf
 * added ANSI_SPEC capability -- can now use the arrow keys from
 * command or insert mode
 *
 * Revision 1.23  1991/08/12  09:25:10  pgf
 * now store w_line in w_traits while buffer is offscreen, so reframe
 * isn't always necessary.  don't force reframe on redisplay.
 *
 * Revision 1.22  1991/08/07  11:51:32  pgf
 * added RCS log entries
 *
 * revision 1.21
 * date: 1991/08/06 15:07:43;
 * global/local values
 * ----------------------------
 * revision 1.20
 * date: 1991/06/28 10:52:53;
 * added config for ISC, and changed some "#if" to "#ifdef"
 * ----------------------------
 * revision 1.19
 * date: 1991/06/25 19:51:43;
 * massive data structure restructure
 * ----------------------------
 * revision 1.18
 * date: 1991/06/16 17:30:21;
 * fixed tabs to be modulo intead of mask, added ctabstop capability, added
 * LOCAL_VALUES #define to control local buffer values
 * ----------------------------
 * revision 1.17
 * date: 1991/06/06 13:57:52;
 * added auto-indent mode
 * ----------------------------
 * revision 1.16
 * date: 1991/06/04 09:20:53;
 * kcod2key is now a macro
 * ----------------------------
 * revision 1.15
 * date: 1991/06/03 17:34:35;
 * switch from "meta" etc. to "ctla" etc.
 * ----------------------------
 * revision 1.14
 * date: 1991/06/03 13:58:40;
 * made bind description list better
 * ----------------------------
 * revision 1.13
 * date: 1991/06/03 10:16:34;
 * cleanup, for release of 2.3
 * ----------------------------
 * revision 1.12
 * date: 1991/05/31 10:46:27;
 * added lspec character class for ex line specifiers
 * added end pointer and offset to the region struct
 * added #defines for the ex range allowances
 * ----------------------------
 * revision 1.11
 * date: 1991/04/22 09:00:46;
 * added ODT, POSIX defines.
 * also added iswild() support
 * ----------------------------
 * revision 1.10
 * date: 1991/04/05 13:04:55;
 * fixed "shorten" directory name
 * ----------------------------
 * revision 1.9
 * date: 1991/04/04 09:28:32;
 * line text is now separate from LINE struct
 * ----------------------------
 * revision 1.8
 * date: 1991/03/26 17:01:11;
 * new undo dot offset field
 * ----------------------------
 * revision 1.7
 * date: 1991/02/21 09:13:00;
 * added sideways offsets for horiz scrolling
 * ----------------------------
 * revision 1.6
 * date: 1990/12/16 22:23:19;
 * changed the default configuration
 * ----------------------------
 * revision 1.5
 * date: 1990/10/04 13:07:38;
 * added #define for ODT
 * ----------------------------
 * revision 1.4
 * date: 1990/10/03 16:00:49;
 * make backspace work for everyone
 * ----------------------------
 * revision 1.3
 * date: 1990/10/01 12:16:36;
 * make provisions for shortnames, and added HAVE_MKDIR define
 * ----------------------------
 * revision 1.2
 * date: 1990/09/28 14:36:22;
 * cleanup of ifdefs, response to porting problems
 * ----------------------------
 * revision 1.1
 * date: 1990/09/21 10:25:09;
 * initial vile RCS revision
 */

#ifndef os_chosen

/* Note that as of vile 3.15, most of these choices can be made
	from the makefile, using separate make targets, so you may
	not need to edit this... */

/*	Machine/OS definitions			*/
/* unix flavors */
/* If you're UNIX, choose one of these... */
#define BERK	0	/* Berkeley derived */
#define USG	0	/* AT&T derived */
#define SVR3	1	/* AT&T derived, more recently */
#define V7	0	/* the good old days */

/* ...and turn this on if you can... */
#define POSIX	0	/* the good new days */

/* unix sub-flavors */
/* ...and turn on one of these if appropriate... */
#define ODT	0			/* UNIX OPEN DESK TOP		*/
#define ULTRIX	0			/* UNIX ULTRIX			*/
#define ISC	0			/* Interactive Systems */
#define SUNOS	0			/* SunOS 3 or 4 */
#define NeXT	0
#define UNIXPC	0
#define HPUX	0
#define AIX	0
#define OSF1	0
#define LINUX	0
#define BSD386	0
#define APOLLO	0
#define AUX2	0			/* Apple A/UX 2.0 */

/* the following overrides are mostly for convenience only */
#if ULTRIX || SUNOS
# undef BERK
# undef USG
# undef POSIX
# define BERK	1
# define USG	0
# define POSIX	1
#endif

#if defined(mips)
# undef BERK
# undef USG
# define BERK	0
# define USG	1
#endif

#if ODT || ISC 
# undef BERK
# undef USG
# undef POSIX
# define BERK	0
# define USG	1
# define POSIX	1
# define SVR3_PTEM	1	/* for ptem.h in display.c */
#endif

#if HPUX	/* reported to work with 7.05.  Could someone confirm these? */
# define HAVE_SELECT
# undef POSIX
# undef BERK
# undef USG
# define POSIX	0
# define BERK	0
# define USG	1
#endif

#if NeXT
# define __STRICT_BSD__
#endif

/* if anyone can verify that these are right, and work, let me know... -pgf */
#if UNIXPC
# undef BERK
# undef USG
# define BERK	0
# define USG	1
# define winit xxwinit
# undef HAVE_MKDIR
# undef HAVE_SELECT
# define HAVE_MKDIR 0
# define HAVE_SELECT 1
#endif

#if AIX
# undef POSIX
# undef BERK
# undef USG
# define POSIX	1
# define BERK	0
# define USG	1
# define HAVE_SELECT 1
# undef __STR__
#endif

#if LINUX
# define HAVE_SELECT 1
# define HAVE_POLL 0
#endif

#if ! LINUX	/* there are probably others that don't want const defined */
# define const
#endif

#if APOLLO
# undef POSIX
# undef BERK
# undef USG
# define POSIX 0
# define BERK 1
# define USG 0
#endif

#ifdef VMS		/* predefined by VAX/VMS compiler */
# undef BERK
# undef SVR3
# undef USG
# define BERK   0
# define SVR3   0
# define USG    0
# define scrn_chosen
# define VMSVT  1
#else
# define VMS    0
#endif

/* non-unix flavors */
#undef	LATTICE		/* don't use their definitions...use ours	*/
#undef	MSDOS
#undef	CPM
#undef	AMIGA
#undef	EGA

#define AMIGA	0			/* AmigaDOS			*/
#define ST520	0			/* ST520, TOS		       */
#define MSDOS	0			/* MS-DOS		       */
#define CPM	0			/* CP/M-86		       */

/*	Compiler definitions			*/
#define MWC86	0	/* marc williams compiler */
#define	LATTICE	0	/* Lattice 2.14 through 3.0 compilers */
#define	AZTEC	0	/* Aztec C 3.20e */
#define	MSC	0	/* MicroSoft C compile version 3 & 4 & 5 & 6 */
#define	ZTC	0	/* Zortech C compiler */
#define	TURBO	0	/* Turbo C/MSDOS */
#define	WATCOM	0	/* WATCOM C/386 version 9.0 or above */
#undef  GO32            /* Sorry, DJ has defined it already */
#define	GO32	0	/* DJ's GCC version 1.09 */

#ifdef __TURBOC__ 	/* Borland C/C++ 3.0 */
#undef TURBO
#undef SVR3
#undef MSDOS
#define SVR3   0
#define TURBO  1
#define MSDOS  1
#endif

#ifdef __WATCOMC__
#undef SVR3
#undef MSDOS
#undef WATCOM
#define SVR3   0
#define MSDOS  1
#define WATCOM 1
#endif

#ifdef __GO32__  	/* DJ's GCC version 1.09 */
#undef GO32
#undef SVR3
#undef USG
#undef MSDOS
#define SVR3   0
#define MSDOS  1
#define GO32   1
#endif

/* As of version 3.51 of vile, NEWDOSCC should be correct for Turbo, Watcom,
 *  and the DJ gcc (GO32) compilers.  I'm betting that it's also probably
 *  correct for MSC (Microsoft C) and ZTC (Zortech), but I'm not sure of those.
 */
#if TURBO || WATCOM || MSC || GO32 || ZTC
# define NEWDOSCC 1
#else
# define NEWDOSCC 0
#endif

#endif /* os_chosen */

#if SVR3
# undef BERK
# undef USG
# define BERK	0
# define USG	1
#endif

#define UNIX	(V7 | BERK | USG)	/* any unix		*/

#include <stdio.h>
#include <sys/types.h>

/* definitions for testing apollo version with gcc warnings */
#if APOLLO
# ifdef __GNUC__		/* only tested for SR10.3 with gcc 1.36 */
#  define _APOLLO_SOURCE	/* appease gcc by forcing extern-defs */
#  define __attribute(s)
#  define APOLLO_STDLIB 1
# endif
# if defined(L_tmpnam)		/* SR10.3, CC 6.8 defines in <stdio.h> */
#  define APOLLO_STDLIB 1
# endif
#endif

#ifndef APOLLO_STDLIB
# define APOLLO_STDLIB 0
#endif

/* choose between void and int signal handler return type.
  "typedefs?  we don't need no steenking typedefs..." */
#if POSIX || (BERK && BSD386) || SVR3 || APOLLO_STDLIB || NEWDOSCC
# define SIGT void
# define SIGRET
#else
# define SIGT int
# define SIGRET return 0
#endif

#if UNIX || MSDOS || VMS
#include	<signal.h>
# if APOLLO
#  if APOLLO_STDLIB && !defined(lint)	/* SR10.3, CC 6.8 */
#   define ACTUAL_SIG_ARGS int signo, ...
#   define ACTUAL_SIG_DECL /* empty */
#   define DEFINE_SIG_ARGS ACTUAL_SIG_ARGS
#  endif
# endif
#endif

#ifdef SIGALRM
# define HAS_ALARM 1
#else
# define HAS_ALARM 0
#endif

#ifndef ACTUAL_SIG_ARGS
# if __STDC__
#  define ACTUAL_SIG_ARGS int signo
#  define ACTUAL_SIG_DECL /* empty */
#  define DEFINE_SIG_ARGS int
# else
#  define ACTUAL_SIG_ARGS signo
#  define ACTUAL_SIG_DECL int signo;
#  define DEFINE_SIG_ARGS
# endif
#endif

#if defined(__GNUC__)
# undef  SIG_DFL
# undef  SIG_IGN
# define SIG_DFL	(SIGT (*)(DEFINE_SIG_ARGS))0
# define SIG_IGN	(SIGT (*)(DEFINE_SIG_ARGS))1
#endif

#if UNIX || MSDOS || VMS
#include	<setjmp.h>
#endif

/* argument for 'exit()' or '_exit()' */
#if	VMS
#include	<stsdef.h>
#define GOOD	(STS$M_INHIB_MSG | STS$K_SUCCESS)
#define BAD(c)	(STS$M_INHIB_MSG | STS$K_ERROR)
#endif

#ifndef GOOD
#define GOOD	0
#define BAD(c)	(c)
#endif

/*	Porting constraints			*/
#ifndef HAVE_MKDIR
# define HAVE_MKDIR	1	/* if your system has the mkdir() system call */
#endif


/* has the select() or poll() call, only used for short sleeps in fmatch() */
#ifndef HAVE_SELECT
# if BERK
#  define HAVE_SELECT 1
# else
#  define HAVE_SELECT 0
# endif
#endif

#ifndef HAVE_POLL
# if !HAVE_SELECT && (POSIX || SVR3 || ( USG && defined(pyr) ))
#  define HAVE_POLL 1
# else
#  define HAVE_POLL 0
# endif
#endif

#ifndef scrn_chosen
/*	Terminal Output definitions		*/
/* choose ONLY one of the following */
#define TERMCAP UNIX			/* Use TERMCAP			*/
#define ANSI	0			/* ANSI escape sequences	*/
#define AT386	0			/* AT style 386 unix console	*/
#define	HP150	0			/* HP150 screen driver		*/
#define	HP110	0			/* HP110 screen driver		*/
#define	VMSVT	0			/* various VMS terminal entries	*/
#define VT52	0			/* VT52 terminal (Zenith).	*/
#define	IBMPC	MSDOS			/* IBM-PC CGA/MONO/EGA driver	*/
#define	ZIBMPC	0			/* Zortech lib IBM-PC CGA/MONO/EGA driver	*/
#define	DG10	0			/* Data General system/10	*/
#define	TIPC	0			/* TI Professional PC driver	*/
#define	Z309	0			/* Zenith 100 PC family	driver	*/
#define	MAC	0			/* Macintosh			*/
#define	ATARI	0			/* Atari 520/1040ST screen	*/
#define	X11	0			/* X Window System */

/*   Special keyboard definitions	     */
#define WANGPC	0		/* WangPC - mostly escape sequences	*/
/* the WANGPC stuff isn't in the cmdtbl keyboard definitions: sorry -- pgf */
#endif

/*	Configuration options... pick and choose as you wish */

/* Appearance */
#define	TYPEAH	1	/* type ahead causes screen refresh to be delayed */
#define	REVSTA	1	/* Status line appears in reverse video		*/

/* NOTE -- COLOR doesn't currently do anything if you're using X or TERMCAP */
/* (But I think X11 may honor colors from the command line or .Xdefaults) */
/* (and DOS definitely does do things with COLOR, but it may not work) */
#define	COLOR	(ANSI|MSDOS|X11)	/* color commands and windows			*/

/* Feature turnon/turnoff */
#define ANSI_SPEC	1 /* ANSI function/arrow keys */
#define	DOSFILES 1	/* turn on code for DOS mode (lines that end in crlf) */
			/* use DOSFILES, for instance, if you edit DOS- */
			/*	created files under UNIX		*/
#define	CFENCE	1	/* do fence matching in CMODE			*/
#define	REBIND	1	/* permit rebinding of keys at run-time		*/
#define	APROP	1	/* Add code for Apropos command	(needs REBIND)	*/
#define	FILOCK	0	/* file locking under unix BSD 4.2 (uses scanf) */
#define	ISRCH	1	/* Incremental searches like ITS EMACS		*/
#define	FLABEL	0	/* function key label code [HP150]		*/
#define	CRYPT	0	/* file encryption (not crypt(1) compatible!)	*/
#define	TAGS	1	/* tags support  				*/
#define	WORDPRO	1	/* "Advanced" word processing features		*/
#define	WORDCOUNT 0	/* "count-words" command"			*/
#define	AEDIT	1	/* advanced editing options: e.g. en/detabbing	*/
#define	PROC	1	/* named procedures				*/
#define	FINDERR	1	/* finderr support. uses scanf()		*/
#define	GLOBALS	1	/* "global" command support.			*/
#define	KSH_HISTORY 0	/* ksh-style history commands			*/
#define	PATHLOOK 1	/* look along $PATH for startup and help files	*/
#define	SCROLLCODE 1	/* code in display.c for scrolling the screen.
			   Only useful if your display can scroll
			   regions, or at least insert/delete lines. 
			   ANSI, TERMCAP, IBMPC, VMSVT and AT386 can do this */
#define CVMVAS	1	/* arguments to forward/back page and half page */
			/* are in pages	instead of rows */
#define PRETTIER_SCROLL 1 /* can improve the appearance of a scrolling screen */
#define STUTTER_SEC_CMD 0 /* must the next/prev section commands (i.e.
				']]' and '[[' be stuttered?  they must be
				stuttered in real vi, I prefer them not
				to be */

/*	Code size options	*/
#define	FEWNAMES 0	/* strip some names - will no longer be bindable */
#define	SMALLER	0	/* strip some fluff -- not a lot smaller, but some */
#define OPT_MAP_MEMORY 0	/* tiny systems can page out data */

/* show "working..." message */
/* we suppress this on USG machines in case system calls are not restartable
	after signals */
#define OPT_WORKING (!USG && HAS_ALARM && !SMALLER)

#define OPT_B_LIMITS    !SMALLER		/* left-margin */
#define OPT_EVAL        !SMALLER		/* expression-evaluation */
#define OPT_FLASH       !SMALLER || IBMPC	/* visible-bell */
#define OPT_HISTORY     !SMALLER		/* command-history */
#define OPT_MS_MOUSE    !SMALLER && IBMPC	/* MsDos-mouse */
#define OPT_UPBUFF      !SMALLER		/* animated buffer-update */

#if	(TERMCAP || X11) && !SMALLER
/* we turn on OPT_XTERM under X11 _only_ because it's then easier for users to
 * be able to put "set xterm-mouse" in their .vilerc which is shared between
 * vile and xvile.  otherwise, xterm-mouse has _no_ effect under X11
 */
#define	OPT_XTERM	2	/* mouse-clicking support */
#else
#define	OPT_XTERM	0	/* vile doesn't recognize xterm mouse */
#endif

	/* any mouse capability */
#define OPT_MOUSE       (X11 || OPT_XTERM || OPT_MS_MOUSE)

/*	Debugging options	*/
#define	RAMSIZE	0	/* dynamic RAM memory usage tracking */
#define	VMALLOC	0	/* verify malloc operation (slow!) */
#define	DEBUG	0	/* allows core dump from keyboard under UNIX */
#define DEBUGM	0	/* $debug triggers macro debugging		*/
#define	VISMAC	0	/* update display during keyboard macros	*/


/* That's the end of the user selections -- the rest is static definition */
/* (i.e. you shouldn't need to touch anything below here */
/* ====================================================================== */

#include <errno.h>
#if VMS
#include <perror.h>	/* defines 'sys_errlist[]' */
#endif
#if UNIX
extern	int	errno;	/* some systems don't define this in <errno.h> */
extern	int	sys_nerr;
extern	char *	sys_errlist[];
#endif
#define	set_errno(code)	errno = code

	/* bit-mask definitions */
#define	lBIT(n)	(1L<<(n))
#define	iBIT(n) (1 <<(n))

/* use 'size_t' if we have it */
#if POSIX || APOLLO || VMS || NEWDOSCC
# define SIZE_T  size_t
# if defined(lint) && (SUNOS||APOLLO)
#  define ALLOC_T unsigned
# else
#  define ALLOC_T size_t
# endif
#else
# define SIZE_T  int
# define ALLOC_T unsigned
#endif

#if APOLLO
# ifndef __STDCPP__
#  define ANSI_VARARGS 0
#  define ANSI_PROTOS 0 /* SR10.2 does not like protos w/o variable names */
# endif
#endif

#ifndef ANSI_PROTOS
#  if defined(__STDC__) || VMS || NEWDOSCC
#    define ANSI_PROTOS 1
#  else
#    define ANSI_PROTOS 0
#  endif
#endif

#ifndef ANSI_VARARGS
# if defined(__STDC__) || VMS || NEWDOSCC
#  define ANSI_VARARGS 1	/* look in <stdarg.h> */
# else
#  define ANSI_VARARGS 0	/* look in <varargs.h> */
# endif
#endif

#if ANSI_PROTOS
# define P(a) a
#else
# define P(a) ()
#endif

#if BERK && ! POSIX && !APOLLO
#define USE_INDEX 1
#endif
#if never && BERK && ! POSIX && ! BSD386
#define USE_BCOPY 1
#endif

#ifdef USE_INDEX
#define strchr index
#define strrchr rindex
extern char *index();
extern char *rindex();
#endif

#ifdef USE_BCOPY
#define memcmp		bcmp
#define memcpy(a,b,c)	bcopy(b,a,c)
#define memset(a,c,b)	bfill(a,b,c)
#else
# if POSIX
#  include <memory.h>
# endif
#endif

#if APOLLO
# ifndef L_tmpnam		/* CC 6.8 <stdio.h> defines, CC 6.7 doesn't */
#  include <memory.h>
   extern time_t time P((time_t *));
   extern uid_t  getuid P((void));
# endif
#endif

/*	System dependent library redefinitions, structures and includes	*/

#if	NEWDOSCC
#include <dos.h>
#endif

#if	NEWDOSCC && ! GO32
#undef peek
#undef poke
#define	peek(a,b,c,d)	movedata(a,b,FP_SEG(c),FP_OFF(c),d)
#define	poke(a,b,c,d)	movedata(FP_SEG(c),FP_OFF(c),a,b,d)
#define	movmem(a, b, c)		memcpy(b, a, c)
#endif

#if 	WATCOM
#include      <string.h>
#endif

#if 	WATCOM || GO32
#define	movmem(a, b, c)		memcpy(b, a, c)
#endif

#if	MSC || ZTC
#include <memory.h>
#endif

#if	LATTICE
#define	unsigned
#endif

#if	AZTEC
#undef	fputc
#undef	fgetc
#if	MSDOS
#define	fgetc	a1getc
#else
#define	fgetc	agetc
#endif
#define	fputc	aputc
#define	int86	sysint
#define	intdos(a, b)	sysint(33, a, b)
#define	inp	inportb
#define	outp	outportb

struct XREG {
	int ax,bx,cx,dx,si,di;
};

struct HREG {
	char al,ah,bl,bh,cl,ch,dl,dh;
};

union REGS {
	struct XREG x;
	struct HREG h;
};
#endif

/* on MS-DOS we have to open files in binary mode to see the ^Z characters. */

#if MSDOS
#define FOPEN_READ	"rb"
#define FOPEN_WRITE	"wb"
#define FOPEN_APPEND	"ab"
#define FOPEN_UPDATE	"w+b"
#else
#define FOPEN_READ	"r"
#define FOPEN_WRITE	"w"
#define FOPEN_APPEND	"a"
#define FOPEN_UPDATE	"w+"
#endif


#if MSDOS
# define slashc(c) (c == '\\' || c == '/')
#endif

#if ST520
# define slashc(c) (c == '\\')
#endif

#ifndef slashc
# define slashc(c) (c == '/')
#endif

#if	MSDOS && MWC86
#include	<dos.h>
#define	int86(a, b, c)	intcall(b, c, a)
#define	inp	in

struct XREG {
	int ax,bx,cx,dx,si,di,ds,es,flags;
};

struct HREG {
	char al,ah,bl,bh,cl,ch,dl,dh;
	int ds,es,flags;
};

union REGS {
	struct XREG x;
	struct HREG h;
};
#endif

#if	MSDOS && LATTICE
#undef	CPM
#undef	LATTICE
#include	<dos.h>
#undef	CPM
#endif

#if	VMS
#define	unlink(a)	delete(a)
#endif

/*	define some ability flags */

#if OPT_WORKING
# define if_OPT_WORKING(statement) statement;
#else
# define if_OPT_WORKING(statement)
#endif

	/* intermediate config-controls for filec.c (needed in nemode.h) */
#if !SMALLER && !OPT_MAP_MEMORY
#define COMPLETE_FILES  (UNIX || MSDOS || VMS)
#define	COMPLETE_DIRS   (UNIX || MSDOS)
#else
#define COMPLETE_FILES  0
#define COMPLETE_DIRS   0
#endif

	/* semaphore may be needed to prevent interrupt of display-code */
#if defined(SIGWINCH) || OPT_WORKING
# define beginDisplay displaying++
# define endofDisplay displaying--
#else
# define beginDisplay
# define endofDisplay
#endif

	/* how to signal this process group */
#if BERK
# define signal_pg(sig) killpg(getpgrp(0), sig)
#else
/* pass the 0 if we can, since it's safer --- the machines where we can't are
 * probably POSIX machines with ANSI C.
 */
# if AIX || (defined(__STDC__) && POSIX)
#  define signal_pg(sig) kill(-getpgrp(), sig)
# else
#  define signal_pg(sig) kill(-getpgrp(0), sig)
# endif
#endif

#if	IBMPC || Z309
#define	MEMMAP	1
#else
#define	MEMMAP	0
#endif

#if	((MSDOS) && (LATTICE || AZTEC || NEWDOSCC)) || UNIX || VMS
#define	ENVFUNC	1
#else
#define	ENVFUNC	0
#endif

#define UCHAR	unsigned char
#define UINT	unsigned int
#define USHORT	unsigned short
#define ULONG	unsigned long

/*	internal constants	*/

#if MSDOS
#define	BITS_PER_INT	16
#endif

#ifndef	BITS_PER_INT
#define	BITS_PER_INT	32
#endif

#define	NBINDS	100			/* max # of bound prefixed keys	*/
#define NFILEN	256			/* # of bytes, file name	*/
#define NBUFN	20			/* # of bytes, buffer name	*/
#define NLINE	256			/* # of bytes, input line	*/
#define	NSTRING	128			/* # of bytes, string buffers	*/
#define NPAT	128			/* # of bytes, pattern		*/
#define HUGE	(1<<(BITS_PER_INT-2))	/* Huge number			*/
#define	NLOCKS	100			/* max # of file locks active	*/
#define	NCOLORS	8			/* number of supported colors	*/
#define	KBLOCK	256			/* sizeof kill buffer chunks	*/
#define	NKREGS	36			/* number of kill buffers	*/
#define	NBLOCK	16			/* line block chunk size	*/

#define C_BLACK 0
#define C_WHITE (NCOLORS-1)

/* SPEC is just 8th bit set, for convenience in some systems */
#define N_chars 128			/* must be a power-of-2		*/
#define SPEC	0x0080			/* special key (function keys)	*/
#define CTLA	0x0100			/* ^A flag, or'ed in		*/
#define CTLX	0x0200			/* ^X flag, or'ed in		*/

#define kcod2key(c)	(c & (N_chars-1)) /* strip off the above prefixes */
#define	isspecial(c)	(c & ~(N_chars-1))

#define	char2int(c)	((int)(c & 0xff)) /* mask off sign-extension, etc. */

#define	PLURAL(n)	((n!=1)?"s":"")

#define	EOS     '\0'

#ifdef	FALSE
#undef	FALSE
#endif
#ifdef	TRUE
#undef	TRUE
#endif

#define FALSE	0			/* False, no, bad, etc. 	*/
#define TRUE	1			/* True, yes, good, etc.	*/
#define ABORT	2			/* Death, ESC, abort, etc.	*/
#define	FAILED	3			/* not-quite fatal false return	*/
#define	SORTOFTRUE	4		/* really!	*/

#define	STOP	0			/* keyboard macro not in use	*/
#define	PLAY	1			/*	"     "	  playing	*/
#define	RECORD	2			/*	"     "   recording	*/

/* flook options */
#define FL_HERE 1
#define FL_HERE_HOME 2
#define FL_ANYWHERE 3

/* definitions for name-completion */
#define	NAMEC		name_cmpl /* char for forcing name-completion */
#define	TESTC		test_cmpl /* char for testing name-completion */

/* kbd_string options */
#define KBD_EXPAND	0x1	/* do we want to expand %, #, : */
#define KBD_QUOTES	0x2	/* do we add and delete '\' chars for the caller */
#define KBD_LOWERC	0x4	/* do we force input to lowercase */
#define KBD_UPPERC	0x8	/* do we force input to uppercase */
#define KBD_NOEVAL	0x10	/* disable 'tokval()' (e.g., from buffer) */
#define KBD_MAYBEC	0x20	/* may be completed -- or not */
#define KBD_NULLOK	0x40	/* may be empty -- or not */
#define KBD_EXPCMD	0x80	/* expand %, #, : only in shell-command */

/* default option for 'mlreply' (used in modes.c also) */
#if !MSDOS
#define	KBD_NORMAL	KBD_EXPAND|KBD_QUOTES
#else
#define	KBD_NORMAL	KBD_EXPAND
#endif

/* reserve space for ram-usage option */
#if RAMSIZE
#define	LastMsgCol	(term.t_ncol - 10)
#else
#define	LastMsgCol	(term.t_ncol - 1)
#endif

/*	Directive definitions	*/

#if ! SMALLER

#define	DIF		0
#define DELSE		1
#define DENDIF		2
#define DGOTO		3
#define DRETURN		4
#define DENDM		5
#define DWHILE		6
#define	DENDWHILE	7
#define	DBREAK		8
#define DFORCE		9

#define NUMDIRS		10

#else

#define DENDM		0
#define NUMDIRS		1

#endif

/*
 * PTBEG, PTEND, FORWARD, and REVERSE are all toggle-able values for
 * the scan routines.
 */
#define	PTBEG	0	/* Leave the point at the beginning on search	*/
#define	PTEND	1	/* Leave the point at the end on search		*/
#define	FORWARD	0			/* forward direction		*/
#define REVERSE	1			/* backwards direction		*/

#define FIOSUC  0			/* File I/O, success.		*/
#define FIOFNF  1			/* File I/O, file not found.	*/
#define FIOEOF  2			/* File I/O, end of file.	*/
#define FIOERR  3			/* File I/O, error.		*/
#define FIOMEM  4			/* File I/O, out of memory	*/
#define FIOABRT 5			/* File I/O, aborted		*/
	/* nonfatal codes */
#define FIOFUN  -1			/* File I/O, eod of file/bad line*/

/* three flavors of insert mode	*/
/* it's FALSE, or one of:	*/
#define INSERT 1
#define OVERWRITE 2
#define REPLACECHAR 3

/* kill register control */
#define KNEEDCLEAN   0x01		/* Kill register needs cleaning */
#define KYANK        0x02		/* Kill register resulted from yank */
#define KLINES       0x04		/* Kill register contains full lines */
#define KAPPEND      0x08		/* Kill register should be appended */

/* operator types.  Needed mainly because word movement changes depending on
	whether operator is "delete" or not.  Aargh.  */
#define OPDEL 1
#define OPOTHER 2

/* define these so C-fence matching doesn't get confused when we're editing
	the cfence code itself */
#define LBRACE '{'
#define RBRACE '}'
#define LPAREN '('
#define RPAREN ')'


/* separator used when scanning PATH environment variable */
#if VMS
#define	PATHCHR	','
#endif

#if MSDOS
#define	PATHCHR	';'
#endif

#ifndef PATHCHR				/* e.g., UNIX */
#define	PATHCHR	':'
#endif

/* how big is the ascii rep. of an int? */
#define	INTWIDTH	sizeof(int) * 3

/*	Macro argument token types					*/

#define	TKNUL	0			/* end-of-string		*/
#define	TKARG	1			/* interactive argument		*/
#define	TKBUF	2			/* buffer argument		*/
#define	TKVAR	3			/* user variables		*/
#define	TKENV	4			/* environment variables	*/
#define	TKFUN	5			/* function....			*/
#define	TKDIR	6			/* directive			*/
#define	TKLBL	7			/* line label			*/
#define	TKLIT	8			/* numeric literal		*/
#define	TKSTR	9			/* quoted string literal	*/
#define	TKCMD	10			/* command name			*/

/*	Internal defined functions					*/

#define	nextab(a)	(((a / curtabval) + 1) * curtabval)
#define	nextsw(a)	(((a / curswval) + 1) * curswval)

/* these are the bits that go into the _chartypes_ array */
/* the macros below test for them */
#define _upper	0x1		/* upper case */
#define _lower	0x2		/* lower case */
#define _digit	0x4		/* digits */
#define _space	0x8		/* whitespace */
#define _bspace 0x10		/* backspace character (^H, DEL, and user's) */
#define _cntrl	0x20		/* control characters, including DEL */
#define _print	0x40		/* printable */
#define _punct	0x80		/* punctuation */
#define _ident	0x100		/* is typically legal in "normal" identifier */
#define _pathn	0x200		/* is typically legal in a file's pathname */
#define _wild	0x400		/* is typically a shell wildcard char */
#define _linespec 0x800		/* ex-style line range: 1,$ or 13,15 or % etc.*/
#define _fence	0x1000		/* a fence, i.e. (, ), [, ], {, } */
#define _nonspace	0x2000	/* non-whitespace */
#define _qident	0x4000		/* is typically legal in "qualified" identifier */

#if !SMALLER
#define	_scrtch 0x8000		/* legal in scratch-buffer names */
#define	_shpipe 0x10000L	/* legal in shell/pipe-buffer names */

#define	screen_to_bname(buf)\
	screen_string(buf,sizeof(buf),(CMASK)(_pathn|_scrtch|_shpipe))
typedef	long CMASK;
#else
#define	screen_to_bname(buf)\
	screen_string(buf,sizeof(buf),(CMASK)(_pathn))
typedef short CMASK;
#endif

/* these intentionally match the ctypes.h definitions, except that
	they force the char to 7-bit ascii first */
#define istype(sometype,c)	((_chartypes_[(c)&(N_chars-1)] & (sometype))!=0)
#define islower(c)	istype(_lower, c)
#define isupper(c)	istype(_upper, c)
#define isdigit(c)	istype(_digit, c)
#define isspace(c)	istype(_space, c)
#define iscntrl(c)	istype(_cntrl, c)
#define isprint(c)	istype(_print, c)
#define ispunct(c)	istype(_punct, c)
#define iswild(c)	istype(_wild, c)
#define isalpha(c)	istype(_lower|_upper, c)
#define isalnum(c)	istype(_lower|_upper|_digit, c)
#define isident(c)	istype(_ident, c)
#define ispath(c)	istype(_pathn, c)
#define isbackspace(c)	(istype(_bspace, c) || (c) == backspc)
#define islinespecchar(c)	istype(_linespec, c)
#define isfence(c)	istype(_fence, c)

/* macro for cases where return & newline are equivalent */
#define	isreturn(c)	((c == '\r') || (c == '\n'))

/* macro for whitespace (non-return) */
#define	isblank(c)      ((c == '\t') || (c == ' '))

/* DIFCASE represents the difference between upper
   and lower case letters, DIFCNTRL the difference between upper case and
   control characters.	They are xor-able values.  */
#define	DIFCASE		0x20
#define	DIFCNTRL	0x40
#define toupper(c)	((c)^DIFCASE)
#define tolower(c)	((c)^DIFCASE)
#define tocntrl(c)	((c)^DIFCNTRL)
#define toalpha(c)	((c)^DIFCNTRL)

#define nocase_eq(bc,pc)	((bc) == (pc) || \
			(isalpha(bc) && (((bc) ^ DIFCASE) == (pc))))

#define ESC	tocntrl('[')
#define BEL	tocntrl('G')	/* ascii bell character		*/

/*
 * Definitions etc. for regexp(3) routines.
 *
 *	the regexp code is:
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 */
#define NSUBEXP  10
typedef struct regexp {
	char *startp[NSUBEXP];
	char *endp[NSUBEXP];
	short mlen;		/* convenience:  endp[0] - startp[0] */
	char regstart;		/* Internal use only. */
	char reganch;		/* Internal use only. */
	int regmust;		/* Internal use only. */
	int regmlen;		/* Internal use only. */
	unsigned size;		/* vile addition -- how big is this */
	char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;

/*
 * The first byte of the regexp internal "program" is actually this magic
 * number; the start node begins in the second byte.
 */
#define	REGEXP_MAGIC	0234

#ifndef CHARBITS
#define	UCHAR_AT(p)	((int)*(UCHAR *)(p))
#else
#define	UCHAR_AT(p)	((int)*(p)&CHARBITS)
#endif

/* end of regexp stuff */

/*
 * Definitions for 'tbuff.c' (temporary/dynamic char-buffers)
 */
typedef	struct	_tbuff	{
	char *	tb_data;	/* the buffer-data */
	ALLOC_T	tb_size;	/* allocated size */
	ALLOC_T	tb_used;	/* total used in */
	ALLOC_T	tb_last;	/* last put/get index */
	int	tb_endc;
	} TBUFF;

/*
 * Primitive types
 */
typedef	int		L_NUM;		/* line-number */
typedef	int		C_NUM;		/* column-number */
typedef	long		L_FLAG;		/* LINE-flags */

typedef	ULONG		CMDFLAGS;	/* CMDFUNC flags */
typedef	long		B_COUNT;	/* byte-count */

/*
 * All text is kept in circularly linked lists of "LINE" structures. These
 * begin at the header line. This line is pointed to by the "BUFFER".
 * Each line contains:
 *  number of bytes in the line (the "used" size), 
 *  the size of the text array,
 *  the text.
 * The end of line is not stored as a byte; it's implied. Future
 * additions may include update hints, and a list of marks into the line.
 *
 * Lines are additionally sometimes stacked in undo lists.
 */
#if OPT_MAP_MEMORY
typedef	long	BLK_T;
typedef	int	OFF_T;
typedef	struct	{ BLK_T blk; OFF_T off; } LINEPTR;
#else
typedef	struct	LINE*	LINEPTR;
#endif

typedef struct	LINE {
	LINEPTR l_fp;			/* Link to the next line	*/
	LINEPTR l_bp;			/* Link to the previous line	*/
	union {
		SIZE_T	l_sze;		/* Allocated size 		*/
		C_NUM	l_fo;		/* forward undo dot offs (undo only) */
	} l_s_fo;
	union {
		L_NUM	l_nmbr;		/* line-# iff b_numlines > 0	*/
		C_NUM	l_bo;		/* backward undo dot offs (undo only) */
	} l_n_bo;
	int	l_used;			/* Used size (may be negative)	*/
	union {
	    char *l_txt;		/* The data for this line	*/
	    LINEPTR l_nxt;		/* if an undo stack separator,	*/
	} lt;				/*  a pointer to the next one	*/
#if OPT_MAP_MEMORY
	struct
#else
	union
#endif
	{
	    LINEPTR	l_stklnk;	/* Link for undo stack		*/
	    L_FLAG	l_flag;		/* flags for undo ops		*/
	} l;
}	LINE;

#define l_size		l_s_fo.l_sze
#define l_forw_offs	l_s_fo.l_fo
#define l_number	l_n_bo.l_nmbr
#define l_back_offs	l_n_bo.l_bo
#define l_text		lt.l_txt
#define l_nextsep	lt.l_nxt

/* flag values */
#define LCOPIED 1	/* original line is already on an undo stack */
#define LGMARK 2	/* line matched a global scan */

/* macros to ease the use of lines */
#define	for_each_line(lp,bp) for (lp = lForw(bp->b_line.l); \
					lp != l_ref(bp->b_line.l); \
					lp = lforw(lp))

#define l_nxtundo		l.l_stklnk

	/*
	 * Special values used in LINE.l_used
	 */
#define LINENOTREAL	((int)(-1)) /* for undo, marks an inserted line */
#define LINEUNDOPATCH	((int)(-2)) /* provides stack patching value for undo */
/* #define MARKPATCH	((int)(-3)) *//*	unused */
#define STACKSEP	((int)(-4)) /* delimit set of changes on undo stack */
#define PURESTACKSEP	((int)(-5)) /* as above, but buffer unmodified before */
					/* this change */

	/*
	 * If we are configured with mapped-data, references to LINE pointers
	 * are translated by functions to/from LINEPTR structs (see file tmp.c).
	 *
	 * LINEPTR is a structure holding the block- and offset-value in the
	 * mapped-data file.  Each LINEPTR corresponds to a LINE struct.
	 *
	 * Basically, l_ref and l_ptr map into functions that translate between
	 * 'LINE *' and LINEPTR.  If you forget to use one of these functions,
	 * then the C-compiler will nag you about it if you turn on
	 * OPT_MAP_MEMORY, since you cannot really assign or compare between
	 * these two types.  (To be honest, my compiler doesn't catch all of
	 * the comparison cases; I also linted the code). 
	 */
#if OPT_MAP_MEMORY
#define fast_ptr	/* can't do it */
#define	same_ptr(a,b)	(((a).blk == (b).blk) && ((a).off == (b).off))
#define	null_ptr	nullmark.l
#define	rls_region()	l_region((REGION *)0)
#else
#define fast_ptr	register
#define	same_ptr(a,b)	((a) == (b))
#define	null_ptr	(LINE *)0
#define	l_ref(lp)	lp
#define	l_ptr(lp)	lp
#define set_lforw(a,b)	lforw(a) = b
#define set_lback(a,b)	lback(a) = b
#define lforw(lp)	(lp)->l_fp
#define lback(lp)	(lp)->l_bp
#define	rls_region()	/* unused */
#endif

	/*
	 * Macros for referencing fields in the LINE struct.
	 */
#define lgetc(lp, n)		char2int((lp)->l_text[(n)])
#define lputc(lp, n, c) 	((lp)->l_text[(n)]=(c))
#define llength(lp)		((lp)->l_used)

#define liscopied(lp)		((lp)->l.l_flag & LCOPIED)
#define lismarked(lp)		((lp)->l.l_flag & LGMARK)
#define lsetcopied(lp)		((lp)->l.l_flag |= LCOPIED)
#define lsetnotcopied(lp)	((lp)->l.l_flag &= ~LCOPIED)
#define lsetmarked(lp)		((lp)->l.l_flag |= LGMARK)
#define lsetnotmarked(lp)	((lp)->l.l_flag &= ~LGMARK)
#define lflipmark(lp)		((lp)->l.l_flag ^= LGMARK)
#if !OPT_MAP_MEMORY
#define lsetclear(lp)		((lp)->l.l_flag = 0)
#endif
#define lisreal(lp)		((lp)->l_used >= 0)
#define lisnotreal(lp)		((lp)->l_used == LINENOTREAL)
#define lislinepatch(lp)	((lp)->l_used == LINEUNDOPATCH)
/* #define lismarkpatch(lp)	((lp)->l_used == MARKPATCH) */
#define lispatch(lp)		(lislinepatch(lp) /* || lismarkpatch(lp) */ )
#define lisstacksep(lp)		((lp)->l_used == STACKSEP || \
					(lp)->l_used == PURESTACKSEP)
#define lispurestacksep(lp)	((lp)->l_used == PURESTACKSEP)

	/*
	 * Corresponding (mixed-case : mixed-type) names for LINEPTR references
	 *
	 * I introduced the mixed-case macros so that the l_ref/l_ptr
	 * translations wouldn't be _too_ intrusive (and also to limit the
	 * amount of text that I changed).  They all serve the same function.
	 * (dickey@software.org)
	 */
#define lGetc(lp, n)		lgetc(l_ref(lp), n)
#define lPutc(lp, n, c)		lputc(l_ref(lp), n, c)
#define lLength(lp)		llength(l_ref(lp))

#if OPT_MAP_MEMORY

#define	lForw(lp)		lforw_p2r(lp)
#define	lBack(lp)		lback_p2r(lp)

#define	lFORW(lp)		lforw_p2p(lp)
#define	lBACK(lp)		lback_p2p(lp)

#define	set_lForw(dst,src)	set_lforw_p2r(dst, src)
#define	set_lBack(dst,src)	set_lback_p2r(dst, src)

#define	set_lFORW(dst,src)	set_lforw_p2p(dst, src)
#define	set_lBACK(dst,src)	set_lback_p2p(dst, src)

#else	/* 'LINEPTR' == 'LINE*' */

#define	lForw(lp)		lforw(l_ref(lp))
#define	lBack(lp)		lback(l_ref(lp))

#define	lFORW(lp)		l_ptr(lForw(lp))
#define	lBACK(lp)		l_ptr(lBack(lp))

#define	set_lForw(dst,src)	set_lforw(dst, src)
#define	set_lBack(dst,src)	set_lback(dst, src)

#define	set_lFORW(dst,src)	set_lforw(dst, src)
#define	set_lBACK(dst,src)	set_lback(dst, src)

#endif


/* marks are a line and an offset into that line */
typedef struct MARK {
	LINEPTR l;
	C_NUM o;
} MARK;

/* some macros that take marks as arguments */
#define is_at_end_of_line(m)	(m.o == lLength(m.l))
#define is_empty_line(m)	(lLength(m.l) == 0)
#define sameline(m1,m2)		(same_ptr(m1.l, m2.l))
#define samepoint(m1,m2)	(sameline(m1,m2) && (m1.o == m2.o))
#define char_at(m)		(lGetc(m.l,m.o))
#define put_char_at(m,c)	(lPutc(m.l,m.o,c))
#define is_header_line(m,bp)	(same_ptr(m.l, bp->b_line.l))
#define is_last_line(m,bp)	(lForw(m.l) == l_ref(bp->b_line.l))
#define is_first_line(m,bp)	(lBack(m.l) == l_ref(bp->b_line.l))

/* settable values have their names stored here, along with a synonym, and
	what type they are */
struct VALNAMES {
		char *name;
		char *shortname;
		short type;
		short winflags;
};
/* the values of VALNAMES->type */
#define VALTYPE_INT 0
#define VALTYPE_STRING 1
#define VALTYPE_BOOL 2
#define VALTYPE_REGEX 3
#define VALTYPE_COLOR 4

typedef	struct {
	char *pat;
	regexp *reg;
} REGEXVAL;

/* this is to ensure values can be of any type we wish.
   more can be added if needed.  */
union V {
	int i;
	char *p;
	REGEXVAL *r;
};

struct VAL {
	union V v;
	union V *vp;
};

typedef	struct	{
	struct VALNAMES *names;
	struct VAL      *local;
	struct VAL      *global;
} VALARGS;

	/*
	 * Values are either local or global. We distinguish the two cases
	 * by whether the value-pointer points into the VAL-struct or not.
	 */
#define is_local_val(lv,which)          (lv[which].vp == &(lv[which].v))
#define make_local_val(lv,which)        (lv[which].vp = &(lv[which].v))
#define make_global_val(lv,gv,which)    (lv[which].vp = &(gv[which].v))

/* these are masks for the WINDOW.w_flag hint */
#define WFFORCE 0x01			/* Window needs forced reframe	*/
#define WFMOVE	0x02			/* Movement from line to line	*/
#define WFEDIT	0x04			/* Editing within a line	*/
#define WFHARD	0x08			/* Better do a full display	*/
#define WFMODE	0x10			/* Update mode line.		*/
#define WFCOLR	0x20			/* Needs a color change		*/
#define WFKILLS	0x40			/* something was deleted	*/
#define WFINS	0x80			/* something was inserted	*/
#define WFSTAT	0x100			/* Update mode line (info only).*/

/* define indices for GLOBAL, BUFFER, WINDOW modes */
#include "nemode.h"

/* macros for setting GLOBAL modes */

#define global_g_val(which) global_g_values.gv[which].v.i
#define set_global_g_val(which,val) global_g_val(which) = val
#define global_g_val_ptr(which) global_g_values.gv[which].v.p
#define set_global_g_val_ptr(which,val) global_g_val_ptr(which) = val
#define global_g_val_rexp(which) global_g_values.gv[which].v.r
#define set_global_g_val_rexp(which,val) global_g_val_rexp(which) = val

/* these are window properties affecting window appearance _only_ */
typedef struct	{
	MARK 	w_dt;			/* Line containing "."	       */
		/* i don't think "mark" needs to be here -- I think it 
			could safely live only in the buffer -pgf */
#ifdef WINMARK
	MARK 	w_mk;	        	/* Line containing "mark"      */
#endif
	MARK 	w_ld;	        	/* Line containing "lastdotmark"*/
	MARK 	w_ln;		/* Top line in the window (offset unused) */
	W_VALUES w_vals;
} W_TRAITS;

#define global_w_val(which) global_w_values.wv[which].v.i
#define set_global_w_val(which,val) global_w_val(which) = val
#define global_w_val_ptr(which) global_w_values.wv[which].v.p
#define set_global_w_val_ptr(which,val) global_w_val_ptr(which) = val

#define w_val(wp,val) (wp->w_values.wv[val].vp->i)
#define set_w_val(wp,which,val) w_val(wp,which) = val
#define w_val_ptr(wp,val) (wp->w_values.wv[val].vp->p)
#define set_w_val_ptr(wp,which,val) w_val_ptr(wp,which) = val

#define make_local_w_val(wp,which)  \
	make_local_val(wp->w_values.wv, which)
#define make_global_w_val(wp,which)  \
	make_global_val(wp->w_values.wv, global_wvalues.wv, which)

#define is_local_w_val(wp,which)  \
	is_local_val(wp->w_values.wv,which)

#if COLOR
#define gfcolor global_w_val(WVAL_FCOLOR)
#define gbcolor global_w_val(WVAL_BCOLOR)
#else
#define gfcolor C_WHITE
#define gbcolor C_BLACK
#endif

/*
 * Text is kept in buffers. A buffer header, described below, exists for every
 * buffer in the system. The buffers are kept in a big list, so that commands
 * that search for a buffer by name can find the buffer header. There is a
 * safe store for the dot and mark in the header, but this is only valid if
 * the buffer is not being displayed (that is, if "b_nwnd" is 0). The text for
 * the buffer is kept in a circularly linked list of lines, with a pointer to
 * the header line in "b_line"	Buffers may be "Inactive" which means the files associated with them
 * have not been read in yet. These get read in at "use buffer" time.
 */

typedef struct	BUFFER {
	MARK 	b_line;		/* Link to the header LINE (offset unused) */
	struct	BUFFER *b_bufp; 	/* Link to next BUFFER		*/
	MARK 	*b_nmmarks;		/* named marks a-z		*/
	B_VALUES b_values;		/* buffer traits we inherit from */
					/*  global values		*/
	W_TRAITS b_wtraits;		/* saved window traits, while we're */
					/*  not displayed		*/
	B_COUNT	b_bytecount;		/* # of chars			*/
	L_NUM	b_linecount;		/* no. lines as of last read/write */
	LINEPTR b_udstks[2];		/* undo stack pointers		*/
	MARK 	b_uddot[2];		/* Link to "." before undoable op*/
	short	b_udstkindx;		/* which of above to use	*/
	LINEPTR b_udtail;		/* tail of undo backstack	*/
	LINEPTR b_udlastsep;		/* last stack separator pushed	*/
	int	b_udcount;		/* how many undo's we can do	*/
#if !OPT_MAP_MEMORY
	LINEPTR	b_LINEs;		/* block-malloced LINE structs */
	LINEPTR	b_LINEs_end;		/* end of 	"	"	" */
	LINEPTR	b_freeLINEs;		/* list of free " 	"	" */
	UCHAR	*b_ltext;		/* block-malloced text */
	UCHAR	*b_ltext_end;		/* end of block-malloced text */
#endif
	LINEPTR	b_ulinep;		/* pointer at 'Undo' line	*/
	int	b_active;		/* window activated flag	*/
	int	b_nwnd;		        /* Count of windows on buffer   */
	int	b_flag;		        /* Flags 		        */
	short	b_acount;		/* auto-save count	        */
	char	*b_fname;		/* File name			*/
	int	b_fnlen;		/* length of filename		*/
	char	b_bname[NBUFN]; 	/* Buffer name			*/
#if	CRYPT
	char	b_key[NPAT];		/* current encrypted key	*/
#endif
#ifdef	MDCHK_MODTIME
	long	b_modtime;		/* file's last-modification time */
	long	b_modtime_at_warn;	/* file's modtime when user warned */
#endif
#if	OPT_UPBUFF
	int	(*b_upbuff) P((struct BUFFER *)); /* call to recompute  */
#endif
#if	OPT_B_LIMITS
	int	b_lim_left;		/* extra left-margin (cf:show-reg) */
#endif
	struct	BUFFER *b_relink; 	/* Link to next BUFFER (sorting) */
	int	b_created;
	int	b_last_used;
}	BUFFER;

/*
 * Special symbols for scratch-buffer names.
 */
#define	SCRTCH_LEFT  "["
#define	SCRTCH_RIGHT "]"
#define	SHPIPE_LEFT  "!"

/* warning:  code in file.c and fileio.c knows how long the shell, pipe, and
	append prefixes are (e.g. fn += 2 when appending) */
#define	isShellOrPipe(s)  ((s)[0] == SHPIPE_LEFT[0])
#define	isInternalName(s) (isShellOrPipe(s) || is_internalname(s))
#define	isAppendToName(s) ((s)[0] == '>' && (s)[1] == '>')

/* shift-commands can be repeated when typed on :-command */
#define isRepeatable(c)   ((c) == '<' || (c) == '>')

#if	defined(apollo)
#if	defined(__STDCPP__)	/* cc 6.8 */
#define	ScratchName(s) SCRTCH_LEFT ## #s ## SCRTCH_RIGHT
#else				/* cc 6.7 */
#define	ScratchName(s)	"[s]"	/* K&R-style macro */
#endif
#endif	/* apollo */

#ifndef	ScratchName
#if defined(__STDC__) || NEWDOSCC || NeXT
#define	ScratchName(s) SCRTCH_LEFT    #s    SCRTCH_RIGHT
#endif
#endif

#ifndef	ScratchName
#define	ScratchName(s)	"[s]"	/* K&R-style macro */
#endif	/* ScratchName */

/*
 * Macros for manipulating buffer-struct members.
 */
#define	for_each_buffer(bp) for (bp = bheadp; bp; bp = bp->b_bufp)

#define global_b_val(which) global_b_values.bv[which].v.i
#define set_global_b_val(which,val) global_b_val(which) = val
#define global_b_val_ptr(which) global_b_values.bv[which].v.p
#define set_global_b_val_ptr(which,val) global_b_val_ptr(which) = val
#define global_b_val_rexp(which) global_b_values.bv[which].v.r
#define set_global_b_val_rexp(which,val) global_b_val_rexp(which) = val

#define b_val(bp,val) (bp->b_values.bv[val].vp->i)
#define set_b_val(bp,which,val) b_val(bp,which) = val
#define b_val_ptr(bp,val) (bp->b_values.bv[val].vp->p)
#define set_b_val_ptr(bp,which,val) b_val_ptr(bp,which) = val
#define b_val_rexp(bp,val) (bp->b_values.bv[val].vp->r)
#define set_b_val_rexp(bp,which,val) b_val_rexp(bp,which) = val

#define make_local_b_val(bp,which)  \
		make_local_val(bp->b_values.bv, which)
#define make_global_b_val(bp,which)  \
		make_global_val(bp->b_values.bv, global_b_values.bv, which)

#define is_local_b_val(bp,which)  \
	is_local_val(bp->b_values.bv,which)

#define is_empty_buf(bp) (lForw(bp->b_line.l) == l_ref(bp->b_line.l))

#define b_dot     b_wtraits.w_dt
#ifdef WINMARK
#define b_mark    b_wtraits.w_mk
#endif
#define b_lastdot b_wtraits.w_ld
#define b_wline   b_wtraits.w_ln

/* buffer-name may not have trailing null */
#define set_bname(bp,name) (void)strncpy(bp->b_bname, name, NBUFN)
#define eql_bname(bp,name) !strncmp(bp->b_bname, name, NBUFN)

/* values for b_flag */
#define BFINVS     0x01			/* Internal invisible buffer	*/
#define BFCHG      0x02			/* Changed since last write	*/
#define BFSCRTCH   0x04			/* scratch -- gone on last close */
#define BFARGS     0x08			/* set for ":args" buffers */
#define BFIMPLY    0x010		/* set for implied-# buffers */
#define BFSIZES    0x020		/* set if byte/line counts current */
#define BFUPBUFF   0x040		/* set if buffer should be updated */

/* macros for manipulating b_flag */
#define b_is_implied(bp)        ((bp)->b_flag & (BFIMPLY))
#define b_is_argument(bp)       ((bp)->b_flag & (BFARGS))
#define b_is_changed(bp)        ((bp)->b_flag & (BFCHG))
#define b_is_invisible(bp)      ((bp)->b_flag & (BFINVS))
#define b_is_scratch(bp)        ((bp)->b_flag & (BFSCRTCH))
#define b_is_temporary(bp)      ((bp)->b_flag & (BFINVS|BFSCRTCH))
#define b_is_counted(bp)        ((bp)->b_flag & (BFSIZES))
#define b_is_obsolete(bp)       ((bp)->b_flag & (BFUPBUFF))

#define b_set_flags(bp,flags)   (bp)->b_flag |= (flags)
#define b_set_changed(bp)       b_set_flags(bp, BFCHG)
#define b_set_counted(bp)       b_set_flags(bp, BFSIZES)
#define b_set_invisible(bp)     b_set_flags(bp, BFINVS)
#define b_set_obsolete(bp)      b_set_flags(bp, BFUPBUFF)
#define b_set_scratch(bp)       b_set_flags(bp, BFSCRTCH)

#define b_clr_flags(bp,flags)   (bp)->b_flag &= ~(flags)
#define b_clr_changed(bp)       b_clr_flags(bp, BFCHG)
#define b_clr_counted(bp)       b_clr_flags(bp, BFSIZES)
#define b_clr_obsolete(bp)      b_clr_flags(bp, BFUPBUFF)

#if OPT_B_LIMITS
#define b_left_margin(bp)       bp->b_lim_left
#define b_set_left_margin(bp,n) b_left_margin(bp) = n
#else
#define b_left_margin(bp)       0
#define b_set_left_margin(bp,n)
#endif

/*
 * There is a window structure allocated for every active display window. The
 * windows are kept in a big list, in top to bottom screen order, with the
 * listhead at "wheadp". Each window contains its own values of dot and mark.
 * The flag field contains some bits that are set by commands to guide
 * redisplay. Although this is a bit of a compromise in terms of decoupling,
 * the full blown redisplay is just too expensive to run for every input
 * character.
 */

typedef struct	WINDOW {
	W_TRAITS w_traits;		/* features of the window we should */
					/*  remember between displays */
	struct	WINDOW *w_wndp; 	/* Next window			*/
	BUFFER  *w_bufp; 		/* Buffer displayed in window	*/
	int	w_toprow;	        /* Origin 0 top row of window   */
	int	w_ntrows;	        /* # of rows of text in window  */
	int	w_force; 	        /* If non-zero, forcing row.    */
	int	w_flag;		        /* Flags.		        */
#ifdef WMDRULER
	int	w_ruler_line;
	int	w_ruler_col;
#endif
}	WINDOW;

#define	for_each_window(wp) for (wp = wheadp; wp; wp = wp->w_wndp)

#define w_dot     w_traits.w_dt
#ifdef WINMARK
#define w_mark    w_traits.w_mk
#endif
#define w_lastdot w_traits.w_ld
#define w_line    w_traits.w_ln
#define w_values  w_traits.w_vals

#define mode_row(wp)	((wp)->w_toprow + (wp)->w_ntrows)
#define	buf_head(bp)	(bp)->b_line.l
#define	win_head(wp)	buf_head((wp)->w_bufp)

#define DOT curwp->w_dot
#ifdef WINMARK
#define MK curwp->w_mark
#else
#define MK Mark
#endif

	/* we use left-margin for protecting the prefix-area of [Registers]
	 * from cut/paste selection.
	 */
#define w_left_margin(wp) b_left_margin(wp->w_bufp)

/*
 * The starting position of a region, and the size of the region in
 * characters, is kept in a region structure.  Used by the region commands.
 */
typedef struct	{
	MARK 	r_orig;			/* Origin LINE address. 	*/
	MARK	r_end;			/* Ending LINE address. 	*/
	B_COUNT	r_size; 		/* Length in characters.	*/
}	REGION;

/*
 * The editor communicates with the display using a high level interface. A
 * "TERM" structure holds useful variables, and indirect pointers to routines
 * that do useful operations. The low level get and put routines are here too.
 * This lets a terminal, in addition to having non standard commands, have
 * funny get and put character code too. The calls might get changed to
 * "termp->t_field" style in the future, to make it possible to run more than
 * one terminal type.
 */
typedef struct	{
	int	t_mrow;			/* max number of rows allowable */
	int	t_nrow; 		/* current number of rows used	*/
	int	t_mcol; 		/* max Number of columns.	*/
	int	t_ncol; 		/* current Number of columns.	*/
	int	t_margin;		/* min margin for extended lines*/
	int	t_scrsiz;		/* size of scroll region "	*/
	int	t_pause;		/* # times thru update to pause */
	void	(*t_open) P((void));	/* Open terminal at the start.	*/
	void	(*t_close) P((void));	/* Close terminal at end.	*/
	void	(*t_kopen) P((void));	/* Open keyboard		*/
	void	(*t_kclose) P((void));	/* close keyboard		*/
	int	(*t_getchar) P((void)); /* Get character from keyboard. */
	void	(*t_putchar) P((int)); 	/* Put character to display.	*/
	void	(*t_flush) P((void));	/* Flush output buffers.	*/
	void	(*t_move) P((int,int));	/* Move the cursor, origin 0.	*/
	void	(*t_eeol) P((void));	/* Erase to end of line.	*/
	void	(*t_eeop) P((void));	/* Erase to end of page.	*/
	void	(*t_beep) P((void));	/* Beep.			*/
	void	(*t_rev) P((int));	/* set reverse video state	*/
	int	(*t_rez) P((char *));	/* change screen resolution	*/
#if	COLOR
	void	(*t_setfor) P((int));	/* set foreground color		*/
	void	(*t_setback) P((int));	/* set background color		*/
#endif
#if	SCROLLCODE
	void	(*t_scroll) P((int,int,int)); /* scroll a region of the screen */
#endif
}	TERM;

/*	TEMPORARY macros for terminal I/O  (to be placed in a machine
					    dependent place later)	*/

#define	TTopen		(*term.t_open)
#define	TTclose		(*term.t_close)
#define	TTkopen		(*term.t_kopen)
#define	TTkclose	(*term.t_kclose)
#define	TTgetc		(*term.t_getchar)
#if ! TERMCAP
#define	TTputc		(*term.t_putchar)
#else
#define	TTputc		(void)putchar
#endif
#define	TTflush		(*term.t_flush)
#define	TTmove		(*term.t_move)
#define	TTeeol		(*term.t_eeol)
#define	TTeeop		(*term.t_eeop)
#define	TTbeep		(*term.t_beep)
#define	TTrev		(*term.t_rev)
#define	TTrez		(*term.t_rez)
#if	COLOR
#define	TTforg		(*term.t_setfor)
#define	TTbacg		(*term.t_setback)
#endif
#if	SCROLLCODE
#define	TTscroll	(*term.t_scroll)
#endif

typedef struct  VIDEO {
        int	v_flag;                 /* Flags */
#if	COLOR
	int	v_fcolor;		/* current forground color */
	int	v_bcolor;		/* current background color */
	int	v_rfcolor;		/* requested forground color */
	int	v_rbcolor;		/* requested background color */
#endif
	/* allocate 4 bytes here, and malloc 4 bytes less than we need,
		to keep malloc from rounding up. */
        char    v_text[4];              /* Screen data. */
}       VIDEO;

#define VFCHG   0x0001                  /* Changed flag			*/
#define	VFEXT	0x0002			/* extended (beyond column 80)	*/
#define	VFREV	0x0004			/* reverse video status		*/
#define	VFREQ	0x0008			/* reverse video request	*/
#define	VFCOL	0x0010			/* color change requested	*/

#if IBMPC
/*
 * these need to go into edef.h eventually!
 */
#define	CDCGA	0			/* color graphics card		*/
#define	CDMONO	1			/* monochrome text card		*/
#define	CDEGA	2			/* EGA color adapter		*/
#define	CDVGA	3			/* VGA color adapter		*/
#define	CDSENSE	-1			/* detect the card type		*/

#if COLOR
#define	CD_25LINE	CDCGA
#else
#define	CD_25LINE	CDMONO
#endif

#endif


/* Commands are represented as CMDFUNC structures, which contain a
 *	pointer to the actual function, and flags which help to classify it.
 *	(things like is it a MOTION, can it be UNDOne)
 *
 *	These structures are generated automatically from the cmdtbl file,
 *	and can be found in the file nefunc.h
 */
#if ANSI_PROTOS
# define CMD_ARGS int f, int n
# define CMD_DECL
#else
# define CMD_ARGS f, n
# define CMD_DECL int f,n;
#endif

typedef	int	(*CmdFunc) P(( int, int ));

typedef  struct {
	CmdFunc  c_func;	/* function name is bound to */
	CMDFLAGS c_flags;	/* what sort of command is it? */
}	CMDFUNC;

/* when referencing a command by name (e.g ":e file") it is looked up in
 *	the nametbl, which is an array of NTAB structures, containing the
 *	name, and a pointer to the CMDFUNC structure.  There can be several
 *	entries pointing at a single CMDFUNC, since a command might have
 *	several synonymous names.
 *
 *	The nametbl array is generated automatically from the cmdtbl file,
 *	and can be found in the file nename.h
 */
typedef struct {
	char *n_name;
	CMDFUNC	*n_cmd;
}	NTAB;

/* when a command is referenced by bound key (like h,j,k,l, or "dd"), it
 *	is looked up one of two ways:  single character 7-bit ascii commands
 *	(by far the majority) are simply indexed into a 128 element array of
 *	CMDFUNC pointers.  Other commands (those with ^A, ^X, or SPEC
 *	prefixes) are searched for in a binding table, made up of KBIND
 *	structures.  This structure contains the command code, and again, a
 *	pointer to the CMDFUNC structure for the command 
 *
 *	The asciitbl array, and the kbindtbl array are generated automatically
 *	from the cmdtbl file, and can be found in the file nebind.h
 */
typedef struct {
	short	k_code; 		/* Key code			*/
	CMDFUNC	*k_cmd;
}	KBIND;


/* These are the flags which can appear in the CMDFUNC structure, describing a
 * command.
 */
#define NONE    0L
#define cmdBIT(n) lBIT(n)	/* ...to simplify typing */
#define UNDO    cmdBIT(0)	/* command is undo-able, so clean up undo lists */
#define REDO    cmdBIT(1)	/* command is redo-able, record it for dotcmd */
#define MOTION  cmdBIT(2)	/* command causes motion, okay after operator cmds */
#define FL      cmdBIT(3)	/* if command causes motion, opers act on full lines */
#define ABSM    cmdBIT(4)	/* command causes absolute (i.e. non-relative) motion */
#define GOAL    cmdBIT(5)	/* column goal should be retained */
#define GLOBOK  cmdBIT(6)	/* permitted after global command */
#define OPER    cmdBIT(7)	/* function is an operator, affects a region */
#define LISTED  cmdBIT(8)	/* internal use only -- used in describing
				 * bindings to only describe each once */
#define NOMOVE  cmdBIT(9)	/* dot doesn't move (although address may be used) */
#define VIEWOK  cmdBIT(10)	/* command is okay in view mode, even though it
				 * _may_ be undoable (macros and maps) */

/* These flags are 'ex' argument descriptors, adapted from elvis.  Not all are
 * used or honored or implemented.
 */
#define argBIT(n) cmdBIT(n+10)	/* ...to simplify adding bits */
#define FROM    argBIT(1)	/* allow a linespec */
#define TO      argBIT(2)	/* allow a second linespec */
#define BANG    argBIT(3)	/* allow a ! after the command name */
#define EXTRA   argBIT(4)	/* allow extra args after command name */
#define XFILE   argBIT(5)	/* expand wildcards in extra part */
#define NOSPC   argBIT(6)	/* no spaces allowed in the extra part */
#define DFLALL  argBIT(7)	/* default file range is 1,$ */
#define DFLNONE argBIT(9)	/* no default file range */
#define NODFL   argBIT(10)	/* do not default to the current file name */
#define EXRCOK  argBIT(11)	/* can be in a .exrc file */
#define NL      argBIT(12)	/* if !exmode, then write a newline first */
#define PLUS    argBIT(13)	/* allow a line number, as in ":e +32 foo" */
#define ZERO    argBIT(14)	/* allow 0 to be given as a line number */
#define OPTREG  argBIT(15)	/* allow optional register-name */
#define FILES   (XFILE | EXTRA)	/* multiple extra files allowed */
#define WORD1   (EXTRA | NOSPC)	/* one extra word allowed */
#define FILE1   (FILES | NOSPC)	/* 1 file allowed, defaults to current file */
#define NAMEDF  (FILE1 | NODFL)	/* 1 file allowed, defaults to "" */
#define NAMEDFS (FILES | NODFL)	/* multiple files allowed, default is "" */
#define RANGE   (FROM  | TO)	/* range of linespecs allowed */

/* definitions for 'mlreply_file()' and other filename-completion */
#define	FILEC_REREAD   4
#define	FILEC_READ     3
#define	FILEC_UNKNOWN  2
#define	FILEC_WRITE    1

#define	FILEC_PROMPT   8	/* always prompt (never from screen) */
#define	FILEC_EXPAND   16	/* allow glob-expansion to multiple files */

#ifndef P_tmpdir		/* not all systems define this */
#if MSDOS
#define P_tmpdir ""
#endif
#if UNIX
#define P_tmpdir "/usr/tmp"
#endif
#if VMS
#define P_tmpdir "sys$scratch:"
#endif
#endif	/* P_tmpdir */

#undef TMPDIR

#if OPT_EVAL
#define TMPDIR gtenv("directory")
#else
#define TMPDIR P_tmpdir		/* defined in <stdio.h> */
#endif	/* !SMALLER */

/*	The editor holds deleted text chunks in the KILL registers. The
	kill registers are logically a stream of ascii characters, however
	due to unpredictable size, are implemented as a linked
	list of chunks. (The d_ prefix is for "deleted" text, as k_
	was taken up by the keycode structure)
*/

typedef	struct KILL {
	struct KILL *d_next;	/* link to next chunk, NULL if last */
	UCHAR d_chunk[KBLOCK];	/* deleted text */
} KILL;

typedef struct KILLREG {
	struct KILL *kbufp;	/* current kill register chunk pointer */
	struct KILL *kbufh;	/* kill register header pointer	*/
	unsigned kused;		/* # of bytes used in kill last chunk	*/
	short kbflag;		/* flags describing kill register	*/
} KILLREG;

/*	The !WHILE directive in the execution language needs to
	stack references to pending whiles. These are stored linked
	to each currently open procedure via a linked list of
	the following structure
*/

typedef struct WHBLOCK {
	LINEPTR	w_begin;	/* ptr to !while statement */
	LINEPTR	w_end;		/* ptr to the !endwhile statement*/
	int w_type;		/* block type */
	struct WHBLOCK *w_next;	/* next while */
} WHBLOCK;

#define	BTWHILE		1
#define	BTBREAK		2

/*
 * Incremental search defines.
 */
#if	ISRCH

#define	CMDBUFLEN	256	/* Length of our command buffer */

#define IS_REVERSE	tocntrl('R')	/* Search backward */
#define	IS_FORWARD	tocntrl('F')	/* Search forward */

#endif

#ifndef NULL
# define NULL 0
#endif

/*
 * General purpose includes
 */

#if ANSI_VARARGS
# include <stdarg.h>
#else
# include <varargs.h>
# ifndef lint
#  ifndef va_dcl	 /* then try these out */
    typedef char *va_list;
#   define va_dcl int va_alist;
#   define va_start(list) list = (char *) &va_alist
#   define va_end(list)
#   define va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]
#  endif
# endif
#endif /* ANSI_VARARGS */

#ifdef lint
# undef  va_dcl
# define va_dcl char * va_alist;
# undef  va_start
# define va_start(list) list = (char *) &va_alist
# undef  va_arg
# define va_arg(ptr,cast) (cast)(ptr-(char *)0)
#endif

#if X11 && APOLLO
#define SYSV_STRINGS
#endif
#include <string.h>

#if POSIX
#include "unistd.h"
#include "stdlib.h"
#else
# if (APOLLO_STDLIB && !defined(lint)) || VMS || NEWDOSCC
#include <stdlib.h>
# else
#  if ! VMALLOC
 extern char *malloc();
 extern char *realloc();
#  endif
extern long strtol P((char *, char **, int));
extern char *getenv P((char *));
extern void exit P((int));
extern void _exit P((int));
# endif	/* APOLLO (special handling of lint vs __STDC__) */
#endif	/* POSIX */

#if ! USG
extern char *getwd P(( char * ));
extern char *getcwd P(( char *, int ));
#endif

#ifndef	free
#if	APOLLO && (!APOLLO_STDLIB || defined(lint))
 extern void free (/*char *s*/);
#endif
#endif	/* free */

/* array/table size */
#define	SIZEOF(v)	(sizeof(v)/sizeof(v[0]))

#ifndef	offsetof	/* <stddef.h> */
#define	offsetof(type, member)	((size_t)&(((type*)0)->member))
#endif

/* structure-allocate, for linting */
#ifdef	lint
#define	castalloc(cast,nbytes)		((cast *)0)
#define	castrealloc(cast,ptr,nbytes)	((ptr)+(nbytes))
#define	typealloc(cast)			((cast *)0)
#define	typeallocn(cast,ntypes)		(((cast *)0)+(ntypes))
#define	typereallocn(cast,ptr,ntypes)	((ptr)+(ntypes))
#define	typeallocplus(cast,extra)	(((cast *)0)+(extra))
#else
#define	castalloc(cast,nbytes)		(cast *)malloc(nbytes)
#define	castrealloc(cast,ptr,nbytes)	(cast *)realloc((char *)(ptr),(nbytes))
#define	typealloc(cast)			(cast *)malloc(sizeof(cast))
#define	typeallocn(cast,ntypes)		(cast *)malloc((ntypes)*sizeof(cast))
#define	typereallocn(cast,ptr,ntypes)	(cast *)realloc((char *)(ptr),\
							(ntypes)*sizeof(cast))
#define	typeallocplus(cast,extra)	(cast *)malloc((extra)+sizeof(cast))
#endif

#define	FreeAndNull(p)	if ((p) != 0) { free((char *)p); p = 0; }
#define	FreeIfNeeded(p)	if ((p) != 0) free((char *)(p))

/* extra level for cleanup of temp-file */
#if OPT_MAP_MEMORY
#define	ExitProgram(code)	exit_program(code)
#else
#define	ExitProgram(code)	exit(code)
#endif

#if HAVE_SELECT
#if UNIXPC
#include <select.h>
#else
#include <sys/time.h>
#endif
#endif


/*
 * Local prototypes
 */

#include "proto.h" 

/*
 * Debugging/memory-leak testing
 */

#ifndef	DOALLOC		/* record info for 'show_alloc()' */
#define	DOALLOC		0
#endif
#ifndef	DBMALLOC	/* test malloc/free/strcpy/memcpy, etc. */
#define	DBMALLOC	0
#endif
#ifndef	NO_LEAKS	/* free permanent memory, analyze leaks */
#define	NO_LEAKS	0
#endif

#if	DBMALLOC
#undef strchr
#undef strrchr
#undef malloc
#undef realloc
#undef free
#include "malloc.h"
#else
#if	NO_LEAKS
extern	void	show_alloc P((void));
#endif
#if	DOALLOC
extern	char *	doalloc P((char *,unsigned));
extern	void	dofree P((char *));
#undef	malloc
#define	malloc(n)	doalloc((char *)0,n)
#undef	realloc
#define	realloc(p,n)	doalloc(p,n)
#undef	free
#define	free(p)		dofree(p)
#endif	/* DOALLOC */
#endif	/* DBMALLOC */

/*	Dynamic RAM tracking and reporting redefinitions	*/
#if	RAMSIZE
#undef	realloc
#define	realloc	reallocate
#undef	calloc
#define	calloc(n,m)	allocate((n)*(m))
#undef	malloc
#define	malloc	allocate
#undef	free
#define	free	release
#endif

#if VMALLOC
extern	char *vmalloc P(( SIZE_T, char *, int ));
extern	void vfree P(( char *, char *, int ));
extern	void rvverify P(( char *, char *, int ));
extern	char *vrealloc P(( char *, SIZE_T, char *, int ));
extern	char *vcalloc P(( int, SIZE_T, char *, int ));
extern	void vdump P(( char * ));
# define malloc(x) vmalloc(x,__FILE__,__LINE__)
# define free(x) vfree(x,__FILE__,__LINE__)
# define realloc(x,y) vrealloc(x,y,__FILE__,__LINE__)
# define calloc(x,y) vcalloc(x,y,__FILE__,__LINE__)
# define vverify(s) rvverify(s,__FILE__,__LINE__)
#else
# define vverify(s) ;
#endif
