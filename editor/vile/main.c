/*
 *	This used to be MicroEMACS 3.9
 *			written by Dave G. Conroy.
 *			substantially modified by Daniel M. Lawrence
 *
 *	Turned into "VI Like Emacs", a.k.a. vile, by Paul Fox
 *
 *	(C)opyright 1987 by Daniel M. Lawrence
 *	MicroEMACS 3.9 can be copied and distributed freely for any
 *	non-commercial purposes. MicroEMACS 3.9 can only be incorporated
 *	into commercial software with the permission of the current author.
 *
 *	The same goes for vile.  -pgf
 *
 *
 * $Log: main.c,v $
 * Revision 1.1  1994/02/01 03:29:30  jkh
 * Initial revision
 *
 * Revision 1.141  1993/09/16  11:07:58  pgf
 * added .scm to c-suffixes list.  for scheme.  this is getting a little
 * unwieldly.
 *
 * Revision 1.140  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.139  1993/09/06  16:29:26  pgf
 * added multibeep mode
 *
 * Revision 1.138  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.137  1993/08/18  11:51:56  pgf
 * turn off xterm-mouse mode by default
 *
 * Revision 1.136  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.135  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.134  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.133  1993/07/22  14:02:38  pgf
 * change the para/section/sentence/comment REs to reflect new status of
 * \s and \S as strict atoms
 *
 * Revision 1.132  1993/07/21  11:34:12  pgf
 * make sure the BFSCRTCH bit is on while loading up vileinit, to suppress
 * undo actions
 *
 * Revision 1.131  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.130  1993/07/09  19:12:25  pgf
 * make DOS mode be on by default for MSDOS versions of vile
 *
 * Revision 1.129  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.128  1993/07/01  16:18:26  pgf
 * oops -- mismerge
 *
 * Revision 1.127  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.126  1993/06/29  11:09:47  pgf
 * changed 'naptime' to 'timeout-value'
 *
 * Revision 1.125  1993/06/28  16:59:53  pgf
 * if startup() fails, go straight to update() and loop()
 *
 * Revision 1.124  1993/06/28  14:31:40  pgf
 * implemented new mode, "naptime"
 *
 * Revision 1.123  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.122  1993/06/23  21:31:16  pgf
 * added "undolimit" mode
 *
 * Revision 1.121  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.120  1993/06/10  14:58:10  pgf
 * initial :map support, from Otto Lind
 *
 * Revision 1.119  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.118  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.117  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.116  1993/05/11  15:46:42  pgf
 * commentary
 *
 * Revision 1.115  1993/05/06  11:56:59  pgf
 * mark the "vileinit" buffer active before switching to it, so swbuffer
 * doesn't try a readin()
 *
 * Revision 1.114  1993/05/05  11:27:48  pgf
 * reordered initial message and initial update, to make sure the update
 * happens for X11
 *
 * Revision 1.113  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.112  1993/04/28  17:15:56  pgf
 * got rid of LOOKTAGS mode and ifdefs
 *
 * Revision 1.111  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.110  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.109  1993/04/22  15:14:39  pgf
 * small fixups to do_num/rept_proc, and now they're static
 *
 * Revision 1.108  1993/04/21  14:38:16  pgf
 * support for glob mode
 *
 * Revision 1.107  1993/04/21  13:51:19  pgf
 * multiple repeat counts (as in 3d2l) now multiply correctly, and
 * repeat counts on '.' now take precedence over original counts
 *
 * Revision 1.106  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.105  1993/04/09  13:36:47  pgf
 * made the dummy functions more real, so that prototypes work
 *
 * Revision 1.104  1993/04/08  11:09:27  pgf
 * implemented horizscroll mode
 *
 * Revision 1.103  1993/04/01  14:43:16  pgf
 * typo -- missing semicolon
 *
 * Revision 1.102  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.101  1993/04/01  12:05:46  pgf
 * add setjmp/longjmp to tgetc() and catchintr(), so that ^C gets
 * through on BSD-style signals
 *
 * Revision 1.100  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.99  1993/03/17  11:35:04  pgf
 * cleaned up confusion in alt-tabstop mode
 *
 * Revision 1.98  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.97  1993/03/05  18:46:39  pgf
 * fix for tab cursor positioning in insert mode, and mode to control
 * positioning style
 *
 * Revision 1.96  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.95  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.94  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.93  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.92  1993/01/23  13:38:23  foxharp
 * writeall is now in file.c,
 * use new exit code macros
 *
 * Revision 1.91  1993/01/16  10:37:36  foxharp
 * macro-ization of special filenames, and re-ordering of bval initializations
 *
 * Revision 1.90  1992/12/23  09:20:50  foxharp
 * added .C, .i to cmode suffixes, for C++ and cpp output files
 *
 * Revision 1.89  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.88  1992/12/04  09:24:12  foxharp
 * deleted unused assigns
 *
 * Revision 1.87  1992/12/02  09:13:16  foxharp
 * changes for "c-shiftwidth"
 *
 * Revision 1.86  1992/11/19  09:11:45  foxharp
 * eric krohn's changes for xvile "foreground", "background", and "name"
 * arguments, and
 * the "_qident" class of characters, useful for C++ "qualified" identifiers
 *
 * Revision 1.85  1992/11/19  08:48:48  foxharp
 * comment fixup
 *
 * Revision 1.84  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.83  1992/08/19  22:59:05  foxharp
 * catch SIGINT and critical errors under DOS, glob command line args,
 * and clean up debug log ifdefs
 *
 * Revision 1.82  1992/08/18  22:39:59  foxharp
 * prevent double zotting of VILEINIT buffer by delaying setting the
 * BFSCRTCH flag (which probably isn't necessary anyway)
 *
 * Revision 1.81  1992/08/11  23:29:53  foxharp
 * fixed core from vileinit buf getting killed too early -- no longer
 * a scratch buffer
 *
 * Revision 1.80  1992/08/07  21:41:09  foxharp
 * cosmetic ifdef cleanup
 *
 * Revision 1.79  1992/07/15  08:56:28  foxharp
 * find our basename, so we can come up in view mode if named "view".
 *
 * Revision 1.78  1992/07/13  22:14:37  foxharp
 * initialize TERSE and TAGSRELATIVE modes
 *
 * Revision 1.77  1992/07/13  09:26:56  foxharp
 * use canonpath() on args before creating their buffers
 *
 * Revision 1.76  1992/07/04  14:37:49  foxharp
 * allow the cursor to rest on the 'newline', in the case where we're in
 * the middle of insert mode, and are only out here due to using arrow
 * keys.  otherwise, there's no way to append to end of line with arrow
 * keys -- you're blocked at the last character.
 *
 * Revision 1.75  1992/07/01  17:01:42  foxharp
 * make sure startstat is always set properly, and
 * commented the usage of the FF logfile
 *
 * Revision 1.74  1992/06/26  22:22:23  foxharp
 * moved reset of curbp (after makecurrent()) up higher -- vtinit may
 * call update().
 * some small fixes to the dos arg. expander.
 * took out all the freshmem stuff -- it wasn't doing anything.
 *
 * Revision 1.73  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.72  1992/06/22  08:36:14  foxharp
 * bug in section r.e.
 *
 * Revision 1.71  1992/06/12  22:23:42  foxharp
 * changes for separate 'comments' r.e. for formatregion
 *
 * Revision 1.70  1992/06/08  08:56:05  foxharp
 * added version no. to usage message
 *
 * Revision 1.69  1992/06/04  19:44:29  foxharp
 * added -V for version info
 *
 * Revision 1.68  1992/06/01  20:38:12  foxharp
 * writeall() no longer calls pressreturn() if successful, and
 * added initialization for "tabinsert" mode
 *
 * Revision 1.67  1992/05/31  22:11:11  foxharp
 * paragraph regexp augmented to support reformatting of comments
 *
 * Revision 1.66  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.65  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.64  1992/05/13  09:12:20  pgf
 * force initial update in X11 -- not sure why -- seems to wait for event
 * otherwise
 *
 * Revision 1.63  1992/04/30  17:53:37  pgf
 * use new \s atom in paragraph and section regexps
 *
 * Revision 1.62  1992/04/14  08:42:42  pgf
 * don't handle SIGWINCH in X11 case
 *
 * Revision 1.61  1992/04/10  18:49:23  pgf
 * mark VILEINIT buf as scratch, so it goes away quickly
 *
 * Revision 1.60  1992/04/09  08:32:35  pgf
 * new vilerc processing was happening too late, after the first buffer
 * was already in.  this kept some modes from "sticking"
 *
 * Revision 1.59  1992/04/02  23:00:28  pgf
 * fixed empty buffer bug, just introduced
 *
 * Revision 1.58  1992/03/24  07:44:05  pgf
 * added support for VILEINIT variable for initialization
 *
 * Revision 1.57  1992/03/19  23:22:46  pgf
 * SIGT for signals, linux port
 *
 * Revision 1.56  1992/03/19  23:09:22  pgf
 * usage cleanup
 *
 * Revision 1.55  1992/03/07  10:36:29  pgf
 * fix missing goto-line argument problem.  "vile + file.c" now goes to end of
 * file, as it should
 *
 * Revision 1.54  1992/03/05  09:19:55  pgf
 * changed some mlwrite() to mlforce(), due to new terse support
 *
 * Revision 1.53  1992/03/03  21:59:02  pgf
 * added '`' to the _wild character set
 *
 * Revision 1.52  1992/03/03  09:35:52  pgf
 * added support for getting "words" out of the buffer via variables --
 * needed _nonspace character type
 *
 * Revision 1.51  1992/02/17  08:58:12  pgf
 * added "showmode" support, and kill registers now hold unsigned chars
 *
 * Revision 1.50  1992/01/14  20:24:54  pgf
 * don't ask for pressreturn() confirmation in quickexit() (ZZ command)
 *
 * Revision 1.49  1992/01/10  08:08:28  pgf
 * added initialization of shiftwidth
 *
 * Revision 1.48  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.47  1992/01/03  23:31:49  pgf
 * use new ch_fname() to manipulate filenames, since b_fname is now
 * a malloc'ed string, to avoid length limits
 *
 * Revision 1.46  1991/11/27  10:09:09  pgf
 * slight rearrangement of init code, to prevent null filenames in makecurrent
 *
 * Revision 1.45  1991/11/16  18:38:58  pgf
 * changes for better magic mode -- the regexps for sections, paras etc. had
 * to change
 *
 * Revision 1.44  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.43  1991/11/07  02:00:32  pgf
 * lint cleanup
 *
 * Revision 1.42  1991/11/06  23:26:27  pgf
 * recomp the search pattern if set from command line, and
 * added _fence to chartypes
 *
 * Revision 1.41  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.40  1991/10/29  14:35:29  pgf
 * implemented the & commands: substagain
 *
 * Revision 1.39  1991/10/29  03:02:55  pgf
 * rename ctlx? routines to be ...kbd_macro... names, and allowed
 * for replaying of named registers
 *
 * Revision 1.38  1991/10/28  14:25:44  pgf
 * VAL_ASAVE --> VAL_ASAVECNT
 *
 * Revision 1.37  1991/10/27  01:45:10  pgf
 * set new regex values in global_val_init
 *
 * Revision 1.36  1991/10/24  13:05:52  pgf
 * conversion to new regex package -- much faster
 *
 * Revision 1.35  1991/10/23  14:20:53  pgf
 * changes to fix interactions between dotcmdmode and kbdmode and tungetc
 *
 * Revision 1.34  1991/10/22  03:08:45  pgf
 * call cmdlinetag() for command-line tags
 *
 * Revision 1.33  1991/10/20  23:07:56  pgf
 * pass taglength value to tags()
 *
 * Revision 1.32  1991/10/18  10:56:54  pgf
 * modified VALUE structures and lists to make them more easily settable
 *
 * Revision 1.31  1991/10/15  03:10:49  pgf
 * added backspacelimit and taglength
 *
 * Revision 1.30  1991/09/26  13:13:54  pgf
 * initialize window values, and
 * make sure file writing errors in writeall() are made visible
 *
 * Revision 1.29  1991/09/19  13:39:17  pgf
 * MDEXACT is now MDIGNCASE, and initialize new VAL_TAGS value to "tags"
 *
 * Revision 1.28  1991/09/17  13:02:57  pgf
 * added write-all and brethren
 *
 * Revision 1.27  1991/09/10  00:46:07	pgf
 * cleanup of the dotcmd stuff
 *
 * Revision 1.26  1991/08/13  02:50:13	pgf
 * initialize showmatch b_val
 *
 * Revision 1.25  1991/08/12  11:22:52	pgf
 * added 'vi +/searchstring file.c' invocation
 *
 * Revision 1.24  1991/08/12  10:23:43	pgf
 * esc() no longer kills keyboard recording
 *
 * Revision 1.23  1991/08/08  23:28:49	pgf
 * moved init of VAL_FILL to after vtinit, since it depends on term.t_ncol
 *
 * Revision 1.22  1991/08/08  13:19:58	pgf
 * removed ifdef BEFORE
 *
 * Revision 1.21  1991/08/07  12:35:07	pgf
 * added RCS log messages
 *
 * revision 1.20
 * date: 1991/08/06 15:55:24;
 * fixed dos mode on empty buffer
 *
 * revision 1.19
 * date: 1991/08/06 15:23:42;
 *  global/local values
 *
 * revision 1.18
 * date: 1991/07/23 11:12:19;
 * don't reset lastdot if absolute motion is on behalf of an operator
 *
 * revision 1.17
 * date: 1991/07/19 17:16:03;
 * ignore SIG_QUIT unless DEBUG
 *
 * revision 1.16
 * date: 1991/06/28 10:53:42;
 * undo last change -- was breaking some ops
 *
 * revision 1.15
 * date: 1991/06/27 19:45:16;
 * moved back from eol and eob to execute()
 *
 * revision 1.14
 * date: 1991/06/26 09:38:15;
 * removed an ifdef BEFORE
 *
 * revision 1.13
 * date: 1991/06/25 19:52:59;
 * massive data structure restructure
 *
 * revision 1.12
 * date: 1991/06/16 17:36:17;
 * support for local vs. global fillcol value
 *
 * revision 1.11
 * date: 1991/06/07 22:00:18;
 * changed header
 *
 * revision 1.10
 * date: 1991/06/07 13:22:23;
 * don't move "last dot" mark if ABS command doesn't change dot
 *
 * revision 1.9
 * date: 1991/06/03 17:34:55;
 * switch from "meta" etc. to "ctla" etc.
 *
 * revision 1.8
 * date: 1991/06/03 10:25:16;
 * command loop is now a separate routine, and
 * if (doingopcmd) stuff is now in operators()
 *
 * revision 1.7
 * date: 1991/05/31 12:54:25;
 * put _wild chartypes back in -- dropped by mistake
 *
 * revision 1.6
 * date: 1991/05/31 11:12:19;
 * changed args to execute(), and
 * added linespec character class, and
 * added unimplemented ex functions
 *
 * revision 1.5
 * date: 1991/04/22 09:00:12;
 * added iswild type to chartypes
 *
 * revision 1.4
 * date: 1991/04/04 09:37:48;
 * new arg. to unqname
 *
 * revision 1.3
 * date: 1991/02/19 17:27:03;
 * set up the reverse pattern if -s is given
 *
 * revision 1.2
 * date: 1990/10/03 16:00:57;
 * make backspace work for everyone
 *
 * revision 1.1
 * date: 1990/09/21 10:25:35;
 * initial vile RCS revision
 */

/* Make global definitions not external */
#define realdef
#include	"estruct.h"	/* global structures and defines */
#include	"edef.h"	/* global definitions */
#include	"glob.h"
#include	"nevars.h"

extern char *pathname[];	/* startup file path/name array */

/* for MSDOS, increase the default stack space */

#if	MSDOS & LATTICE
unsigned _stack = 32767;
#endif

#if	ATARI & LATTICE & 0
int _mneed = 256000;		/* reset memory pool size */
#endif

#if	MSDOS & AZTEC
int _STKSIZ = 32767/16; 	/* stack size in paragraphs */
int _STKRED = 1024;		/* stack checking limit */
int _HEAPSIZ = 4096/16; 	/* (in paragraphs) */
int _STKLOW = 0;		/* default is stack above heap (small only) */
#endif

#if	MSDOS & TURBO
unsigned _stklen = 32768U;
#endif

static	void	do_num_proc P(( int *, int *, int * ));
static	void	do_rept_arg_proc P(( int *, int *, int * ));

/*--------------------------------------------------------------------------*/
#define	GetArgVal(param)	if (!*(++param))\
					param = argv[++carg];\
				if (param == 0)\
					goto usage

int
main(argc, argv)
int	argc;
char	*argv[];
{
	int    c;			/* command character */
	register BUFFER *bp;		/* temp buffer pointer */
	register int	carg;		/* current arg to scan */
	register int	ranstartup = FALSE;/* startup executed flag */
	int startstat = TRUE;		/* result of running startup */
	BUFFER *firstbp = NULL; 	/* ptr to first buffer in cmd line */
	int gotoflag = FALSE;		/* do we need to goto line at start? */
	int gline = FALSE;		/* if so, what line? */
	int helpflag = FALSE;		/* do we need help at start? */
	int searchflag = FALSE; 	/* Do we need to search at start? */
	char bname[NBUFN];		/* buffer name of file to read */
	char *msg;
#if TAGS
	int didtag = FALSE;		/* look up a tag to start? */
	char *tname = NULL;
#endif
#if	CRYPT
	char ekey[NPAT];		/* startup encryption key */
	*ekey = EOS;
#endif

#if OPT_MAP_MEMORY
	null_ptr = l_ptr((LINE *)0);
	pre_op_dot.l = null_ptr;
	nullmark.l = null_ptr;
#if !WINMARK
	Mark.l = null_ptr;	/* ...so we don't confuse with blk 0 */
#endif
#endif
	charinit();		/* character types -- we need these pretty
					early  */

	global_val_init();	/* global buffer values */

#if MSDOS
	slash = '\\';  /* getswitchar() == '/' ? '\\' : '/'; */
#else
#if ST520
	slash = '\\';
#else	/* UNIX */
	slash = '/';
#endif
#endif
#if !UNIX
	expand_wild_args(&argc, &argv);
#endif
	prog_arg = argv[0];	/* this contains our only clue to exec-path */

#if UNIX || VMS
	makeversion();
#endif

	start_debug_log(argc,argv);

	if (strcmp(pathleaf(prog_arg), "view") == 0)
		set_global_b_val(MDVIEW,TRUE);

#if IBMPC
	ibmtype = CDSENSE;
#endif

#if X11
	x_preparse_args(&argc, &argv);
#endif
	/* Parse the command line */
	for (carg = 1; carg < argc; ++carg) {
		register char *param = argv[carg];
#if X11
		if (*param == '=') {
			x_set_geometry(param);
			continue;
		}
#endif


		/* Process Switches */
		if (*param == '-') {
			switch (*(++param)) {
#if X11
			case 'd':
				if ((param = argv[++carg]) != 0)
					x_set_dpy(param);
				else
					goto usage;
				break;
			case 'r':
			case 'R':
				x_set_rv();
				break;
			case 'f':
				if (argv[++carg] != 0) {
					if (strcmp(param, "foreground") == 0
					 || strcmp(param, "fg") == 0)
						x_setforeground(argv[carg]);
					else
						x_setfont(argv[carg]);
				} else
					goto usage;
				break;
			case 'b':
				if (argv[++carg] != 0) {
					if (strcmp(param, "background") == 0
					 || strcmp(param, "bg") == 0)
						x_setbackground(argv[carg]);
				} else
					goto usage;
				break;
			case 'n':
				if (strcmp(param, "name") == 0
				 && argv[++carg] != 0)
					x_setname(argv[carg]);
				else
					goto usage;

				break;
#endif
			case 'e':	/* -e for Edit file */
			case 'E':
				set_global_b_val(MDVIEW,FALSE);
				break;
			case 'g':	/* -g for initial goto */
			case 'G':
				gotoflag = TRUE;
				GetArgVal(param);
				gline = atoi(param);
				break;
			case 'h':	/* -h for initial help */
			case 'H':
				helpflag = TRUE;
				break;
#if	CRYPT
			case 'k':	/* -k<key> for code key */
			case 'K':
				GetArgVal(param);
				(void)strcpy(ekey, param);
				(void)memset(param, '.', strlen(param));
				ue_crypt((char *)NULL, 0);
				ue_crypt(ekey, (int)strlen(ekey));
				break;
#endif
			case 's':  /* -s for initial search string */
			case 'S':
		dosearch:
				searchflag = TRUE;
				GetArgVal(param);
				(void)strncpy(pat, param, NPAT);
				gregexp = regcomp(pat, global_b_val(MDMAGIC));
				break;
#if TAGS
			case 't':  /* -t for initial tag lookup */
			case 'T':
				GetArgVal(param);
				tname = param;
				break;
#endif
			case 'V':
				(void)printf("vile %s\n", version);
				ExitProgram(GOOD);

			case 'v':	/* -v for View File */
				set_global_b_val(MDVIEW,TRUE);
				break;

			/*
			 * Note that ibmtype is now only used to detect
			 * whether a command line option was given, i.e., if
			 * it is not equal to CDSENSE then a command line
			 * option was given
			 */
#if IBMPC

#if __ZTC__
#define	OptScreen(type)	ibmtype = type; set43 = (type != CD_25LINE)
#else
#define	OptScreen(type)	ibmtype = type
#endif	/* __ZTC__ */
			case '2':	/* 25 line mode */
				OptScreen(CD_25LINE);
				break;
			case '4':	/* 43 line mode */
				OptScreen(CDEGA);
				break;
			case '5':	/* 50 line mode */
				OptScreen(CDVGA);
				break;
#endif	/* IBMPC */

			case '?':
			default:	/* unknown switch */
			usage:
				print_usage();
			}

		} else if (*param == '+') { /* alternate form of -g */
			if (*(++param) == '/')
				goto dosearch;
			gotoflag = TRUE;
			gline = atoi(param);
		} else if (*param == '@') {
			/* Process Startup macros */
			if ((startstat = startup(++param)) == TRUE)
				ranstartup = TRUE; /* don't execute .vilerc */
		} else if (*param != EOS) {

			/* Process an input file */
#if CRYPT
			cryptkey = (*ekey != EOS) ? ekey : 0;
#endif
			/* set up a buffer for this file */
			makename(bname, param);
			unqname(bname,FALSE);

			bp = bfind(bname, BFARGS);
			ch_fname(bp, param);
			make_current(bp); /* pull it to the front */
			if (firstbp == 0)
				firstbp = bp;
#if CRYPT
			cryptkey = 0;
#endif
		}
	}


	/* if stdin isn't a terminal, assume the user is trying to pipe a
	 * file into a buffer.
	 */
#if UNIX
	if (!isatty(fileno(stdin))) {
		FILE	*in;

		bp = bfind(ScratchName(Standard Input), BFARGS);
		make_current(bp); /* pull it to the front */
		if (firstbp == 0)
			firstbp = bp;
		ffp = fdopen(dup(fileno(stdin)), "r");
		if ((in = fopen("/dev/tty", "r")) != 0) {
			(void)close(0);		/* not all systems have dup2() */
			(void)dup(fileno(in));	/* so 'ttopen()' will work */
			*stdin = *in;
		}
		(void)slowreadf(bp, &(bp->b_linecount));
		set_rdonly(bp, bp->b_fname);
		(void)ffclose();
#if FINDERR
		set_febuff(get_bname(bp));
#endif
	}
#endif

	/* we made some calls to make_current() above, to shuffle the
		list order.  this set curbp, which isn't actually kosher */
	curbp = NULL;

	/* initialize the editor */
#if UNIX
	(void)signal(SIGINT,catchintr);
	(void)signal(SIGHUP,imdying);
#ifdef SIGBUS
	(void)signal(SIGBUS,imdying);
#endif
#ifdef SIGSYS
	(void)signal(SIGSYS,imdying);
#endif
	(void)signal(SIGSEGV,imdying);
	(void)signal(SIGTERM,imdying);
#if DEBUG
	(void)signal(SIGQUIT,imdying);
#else
	(void)signal(SIGQUIT,SIG_IGN);
#endif
	(void)signal(SIGPIPE,SIG_IGN);
#if defined(SIGWINCH) && ! X11
	(void)signal(SIGWINCH,sizesignal);
#endif
#else
# if MSDOS
	(void)signal(SIGINT,catchintr);
#  if ! GO32
#   if WATCOM
	{
	/* clean up Warning from Watcom C */
	void *ptrfunc = dos_crit_handler;
	_harderr(ptrfunc);
	}
#   else	/* TURBO */
	_harderr(dos_crit_handler);
#   endif
#  endif
# endif
#endif

	vtinit();		/* Display */
	winit();		/* windows */

	/* this comes out to 70 on an 80 (or greater) column display */
	{	register int fill;
		fill = (7 * term.t_ncol) / 8;  /* must be done after vtinit() */
		if (fill > 70) fill = 70;
		set_global_b_val(VAL_FILL, fill);
	}

	/* pull in an unnamed buffer, if we were given none to work with */
	if (firstbp == 0) {
		bp = bfind(ScratchName(unnamed), 0);
		bp->b_active = TRUE;
#if DOSFILES
		make_local_b_val(bp,MDDOS);
		set_b_val(bp, MDDOS, FALSE );
#endif
		swbuffer(bp);
	}

	/* if invoked with no other startup files,
	   run the system startup file here */
	if (!ranstartup) {
		char *vileinit = getenv("VILEINIT");
		if (vileinit != NULL) {
			int odiscmd;
			BUFFER *vbp, *obp;
			int oflags = 0;

			/* mark as modified, to prevent undispbuff() from
				 clobbering */
			obp = curbp;
			if (obp) {
				oflags = obp->b_flag;
				b_set_changed(obp);
			}

			if ((vbp=bfind(ScratchName(vileinit), 0))==NULL)
				ExitProgram(BAD(1));

			/* don't want swbuffer to try to read it */
			vbp->b_active = TRUE;
			swbuffer(vbp);
			b_set_scratch(vbp);
			bprintf("%s", vileinit);
			/* if we leave it scratch, swbuffer(obp) may zot it,
				and we may zot it again */
			b_clr_flags(vbp,BFSCRTCH);
			set_rdonly(vbp, vbp->b_fname);

			/* go execute it! */
			odiscmd = discmd;
			discmd = FALSE;
			startstat = dobuf(vbp);
			discmd = odiscmd;
			if (startstat != TRUE)
				goto begin;
			if (obp) {
				swbuffer(obp);
				obp->b_flag = oflags;
			}
			/* remove the now unneeded buffer */
			b_set_scratch(vbp);  /* make sure it will go */
			(void)zotbuf(vbp);
		} else {
			char *fname;
			/* if .vilerc is one of the input files....
					don't clobber it */
#if MSDOS
			/* search PATH for vilerc under dos */
	 		fname = flook(pathname[0], FL_ANYWHERE);
#else
			fname = pathname[0];
#endif
			if (firstbp != 0
			 && eql_bname(firstbp, pathname[0])) {
				c = firstbp->b_bname[0];
				firstbp->b_bname[0] = SCRTCH_LEFT[0];
				startstat = startup(fname);
				firstbp->b_bname[0] = c;
			} else {
				if (fname)
					startstat = startup(fname);
				else
					startstat = TRUE;
			}
			if (startstat != TRUE)
				goto begin;
		}
	}


	/* if there are any files to read, read the first one! */
	if (firstbp != 0) {
		nextbuffer(FALSE,0);
	}
#if TAGS
	else if (tname) {
		cmdlinetag(tname);
		didtag = TRUE;
	}
#endif
	msg = "";
	if (helpflag) {
		if (help(TRUE,1) != TRUE) {
			msg =
	"[Problem with help information. Type \":quit\" to exit if you wish]";
		}
	} else {
		msg = "[Use ^A-h, ^X-h, or :help to get help]";
	}

	/* Deal with startup gotos and searches */
	if (gotoflag + searchflag
#if TAGS
		 + (tname?1:0)
#endif
		> 1) {
#if TAGS
		msg = "[Search, goto and tag are used one at a time]";
#else
		msg = "[Cannot search and goto at the same time]";
#endif
	} else if (gotoflag) {
		if (gotoline(gline != 0, gline) == FALSE)
			msg = "[Invalid goto argument]";
	} else if (searchflag) {
		forwhunt(FALSE, 0);
#if TAGS
	} else if (tname && !didtag) {
		cmdlinetag(tname);
#endif
	}

	if (startstat == TRUE)  /* else there's probably an error message */
		mlforce(msg);

 begin:
	(void)update(FALSE);

	/* process commands */
	loop();

	/* NOTREACHED */
	return BAD(1);
}

/* this is nothing but the main command loop */
void
loop()
{
	CMDFUNC	*cfp, *last_cfp = NULL;
	int s,c,f,n;

	while(1) {

		/* vi doesn't let the cursor rest on the newline itself.  This
			takes care of that. */
		/* if we're inserting, or will be inserting again, then
			suppress.  this happens if we're using arrow keys
			during insert */
		if (is_at_end_of_line(DOT) && (DOT.o > w_left_margin(curwp)) &&
				!insertmode && !insert_mode_was)
			backchar(TRUE,1);

		/* same goes for end-of-file -- I'm actually not sure if
			this can ever happen, but I _am_ sure that it's
			a lot safer not to let it... */
		if (is_header_line(DOT,curbp) && !is_empty_buf(curbp))
			backline(TRUE,1);

		/* start recording for '.' command */
		dotcmdbegin();

		/* Fix up the screen	*/
		s = update(FALSE);

		/* get the next command from the keyboard */
		c = kbd_seq();

		/* if there is something on the command line, clear it */
		if (mpresf != 0) {
			mlerase();
			if (s != SORTOFTRUE) /* did nothing due to typeahead */
				(void)update(FALSE);
		}

		f = FALSE;
		n = 1;

		do_repeats(&c,&f,&n);
		map_check(c);

		kregflag = 0;

		/* flag the first time through for some commands -- e.g. subst
			must know to not prompt for strings again, and pregion
			must only restart the p-lines buffer once for each
			command. */
		calledbefore = FALSE;

		/* and execute the command */
		cfp = kcod2fnc(c);
		s = execute(cfp, f, n);

		/* stop recording for '.' command */
		dotcmdfinish();

		/* If this was a motion that failed, sound the alarm (like vi),
		 * but limit it to once, in case the user is holding down the
		 * autorepeat-key.
		 */
		if ( (cfp != NULL)
		 && ((cfp->c_flags & MOTION) != 0)
		 && (s != TRUE) ) {
			if (cfp != last_cfp || global_g_val(GMDMULTIBEEP)) {
				last_cfp = cfp;
				kbd_alarm();
			}
		} else
			last_cfp = NULL; /* avoid noise! */
	}
}

char *
strmalloc(s)
char *s;
{
	register char *ns = castalloc(char,strlen(s)+1);
	if (ns)
		return strcpy(ns,s);
	else
		return NULL;

}

int
no_memory(s)
char	*s;
{
	mlforce("[%s] %s", out_of_mem, s);
	return FALSE;
}

void
global_val_init()
{
	register int i;
	/* set up so the global value pointers point at the global
		values.  we never actually use the global pointers
		directly, but when buffers get a copy of the
		global_b_values structure, the pointers will still point
		back at the global values, which is what we want */
	for (i = 0; i <= NUM_G_VALUES; i++)
		make_local_val(global_g_values.gv, i);

	for (i = 0; i <= NUM_B_VALUES; i++)
		make_local_val(global_b_values.bv, i);

	for (i = 0; i <= NUM_W_VALUES; i++)
		make_local_val(global_w_values.wv, i);


	/*
	 * Universal-mode defaults
	 */
	set_global_g_val(GMDABUFF,	TRUE); 	/* auto-buffer */
	set_global_g_val(GMDALTTABPOS,	FALSE); /* emacs-style tab
							positioning */
#ifdef GMDDIRC
	set_global_g_val(GMDDIRC,	FALSE); /* directory-completion */
#endif
#if OPT_FLASH
	set_global_g_val(GMDFLASH,  	TRUE);	/* use it if we've got it */
#endif
#ifdef GMDHISTORY
	set_global_g_val(GMDHISTORY,	TRUE);
#endif
	set_global_g_val(GMDMULTIBEEP,	TRUE); /* multiple beeps for multiple
						motion failures */
	set_global_g_val(GVAL_TIMEOUTVAL, 500);	/* catnap time -- how long
							to wait for ESC seq */
#if VMS || MSDOS			/* ':' gets in the way of drives */
	set_global_g_val_ptr(GVAL_EXPAND_CHARS,strmalloc("%#"));
#else	/* UNIX */
	set_global_g_val_ptr(GVAL_EXPAND_CHARS,strmalloc("%#:"));
#endif
	set_global_g_val(GMDEXPAND_PATH,FALSE);
#ifdef GMDGLOB
	set_global_g_val(GMDGLOB, TRUE);
#endif
#ifdef GVAL_GLOB
	set_global_g_val_ptr(GVAL_GLOB, strmalloc("!echo %s"));
#endif

	set_global_g_val(GMDIMPLYBUFF,	FALSE); /* imply-buffer */
#ifdef GMDPOPUP_FILEC
	set_global_g_val(GMDPOPUP_FILEC,FALSE); /* popup-choices */
#endif
#ifdef GMDPOPUP_MSGS
	set_global_g_val(GMDPOPUP_MSGS,	FALSE); /* popup-msgs */
#endif
#ifdef GMDRAMSIZE
	set_global_g_val(GMDRAMSIZE,	TRUE);	/* show ram-usage */
#endif
	set_global_g_val(GVAL_REPORT,	5);	/* report changes */
#if	OPT_XTERM
	set_global_g_val(GMDXTERM_MOUSE,FALSE);	/* mouse-clicking */
#endif

	/*
	 * Buffer-mode defaults
	 */
	set_global_b_val(MDAIND,	FALSE); /* auto-indent */
	set_global_b_val(MDASAVE,	FALSE);	/* auto-save */
	set_global_b_val(MDBACKLIMIT,	TRUE); 	/* limit backspacing to
							insert point */
#ifdef	MDCHK_MODTIME
	set_global_b_val(MDCHK_MODTIME,	FALSE); /* modtime-check */
#endif
	set_global_b_val(MDCMOD,	FALSE); /* C mode */
#ifdef MDCRYPT
	set_global_b_val(MDCRYPT,	FALSE);	/* crypt */
#endif
	set_global_b_val(MDIGNCASE,	FALSE); /* exact matches */
#if MSDOS
	set_global_b_val(MDDOS,		TRUE);	/* dos mode */
#else
	set_global_b_val(MDDOS,		FALSE);	/* dos mode */
#endif
	set_global_b_val(MDMAGIC,	TRUE); 	/* magic searches */
	set_global_b_val(MDNEWLINE,	TRUE); 	/* trailing-newline */
	set_global_b_val(MDSHOWMAT,	FALSE);	/* show-match */
	set_global_b_val(MDSHOWMODE,	TRUE);	/* show-mode */
	set_global_b_val(MDSWRAP,	TRUE); 	/* scan wrap */
	set_global_b_val(MDTABINSERT,	TRUE);	/* allow tab insertion */
	set_global_b_val(MDTAGSRELTIV,	FALSE);	/* path relative tag lookups */
	set_global_b_val(MDTERSE,	FALSE);	/* terse messaging */
#if	OPT_UPBUFF
	set_global_b_val(MDUPBUFF,	TRUE);	/* animated */
#endif
	set_global_b_val(MDVIEW,	FALSE); /* view-only */
	set_global_b_val(MDWRAP,	FALSE); /* wrap */

	set_global_b_val(VAL_ASAVECNT,	256);	/* autosave count */
	set_global_b_val(VAL_C_SWIDTH,	8); 	/* C file shiftwidth */
	set_global_b_val(VAL_C_TAB,	8); 	/* C file tab stop */
	set_global_b_val(VAL_SWIDTH,	8); 	/* shiftwidth */
	set_global_b_val(VAL_TAB,	8);	/* tab stop */
	set_global_b_val(VAL_TAGLEN,	0);	/* significant tag length */
	set_global_b_val(VAL_UNDOLIM,	10);	/* undo limit */

	set_global_b_val_ptr(VAL_TAGS, strmalloc("tags")); /* tags filename */

#if VMS
#define	DEFAULT_CSUFFIX	"\\.\\(\\([CHIS]\\)\\|CC\\|CXX\\|HXX\\)\\(;[0-9]*\\)\\?$"
#endif
#if MSDOS
#define	DEFAULT_CSUFFIX	"\\.\\(\\([chis]\\)\\|cpp\\|cxx\\|hxx\\)$"
#endif
#ifndef DEFAULT_CSUFFIX	/* UNIX (mixed-case names) */
#define	DEFAULT_CSUFFIX	"\\.\\(\\([Cchis]\\)\\|CC\\|cpp\\|cxx\\|hxx\\|scm\\)$"
#endif

	/* suffixes for C mode */
	set_global_g_val_rexp(GVAL_CSUFFIXES,
		new_regexval(
			DEFAULT_CSUFFIX,
			TRUE));

	/* where do paragraphs start? */
	set_global_b_val_rexp(VAL_PARAGRAPHS,
		new_regexval(
			"^\\.[ILPQ]P\\>\\|^\\.P\\>\\|^\\.LI\\>\\|\
^\\.[plinb]p\\>\\|^\\.\\?\\s*$",
			TRUE));

	/* where do comments start and end, for formatting them */
	set_global_b_val_rexp(VAL_COMMENTS,
		new_regexval(
			"^\\s*/\\?[#*>]\\+/\\?\\s*$",
			TRUE));

	/* where do sections start? */
	set_global_b_val_rexp(VAL_SECTIONS,
		new_regexval(
			"^[{\014]\\|^\\.[NS]H\\>\\|^\\.HU\\?\\>\\|\
^\\.[us]h\\>\\|^+c\\>",
			TRUE));

	/* where do sentences start? */
	set_global_b_val_rexp(VAL_SENTENCES,
		new_regexval(
	"[.!?][])\"']* \\?$\\|[.!?][])\"']*  \\|^\\.[ILPQ]P\\>\\|\
^\\.P\\>\\|^\\.LI\\>\\|^\\.[plinb]p\\>\\|^\\.\\?\\s*$",
			TRUE));

	/*
	 * Window-mode defaults
	 */
#ifdef WMDLINEWRAP
	set_global_w_val(WMDLINEWRAP,	FALSE); /* line-wrap */
#endif
	set_global_w_val(WMDLIST,	FALSE); /* list-mode */
	set_global_w_val(WMDNUMBER,	FALSE);	/* number */
	set_global_w_val(WMDHORSCROLL,	TRUE);	/* horizontal scrolling */

	set_global_w_val(WVAL_SIDEWAYS,	0);	/* list-mode */
#if defined(WVAL_FCOLOR) || defined(WVAL_BCOLOR)
	set_global_w_val(WVAL_FCOLOR,	C_WHITE); /* foreground color */
	set_global_w_val(WVAL_BCOLOR,	C_BLACK); /* background color */
#endif


}

#if UNIX || MSDOS || VMS
/* ARGSUSED */
SIGT
catchintr (ACTUAL_SIG_ARGS)
{
	interrupted = TRUE;
#if USG || MSDOS
	(void)signal(SIGINT,catchintr);
#endif
	if (doing_kbd_read)
		longjmp(read_jmp_buf, signo);
	SIGRET;
}
#endif

#if MSDOS
# if WATCOM
    int  dos_crit_handler(unsigned deverror, unsigned errcode, unsigned *devhdr)
# else
    void dos_crit_handler()
# endif
{
# if WATCOM
	_hardresume((int)_HARDERR_FAIL);
	return (int)_HARDERR_FAIL;
# else
#  if ! GO32
	_hardresume(_HARDERR_FAIL);
#  endif
# endif
}
#endif

/* do number processing if needed */
static void
do_num_proc(cp,fp,np)
int *cp, *fp, *np;
{
	register int c, f, n;
	register int	mflag;
	register int oldn;

	c = *cp;

	if (iscntrl(c) || (c & (CTLA|CTLX|SPEC)))
		return;

	f = *fp;
	n = *np;
	if (f)
		oldn = n;
	else
		oldn = 1;
	n = 1;

	if ( isdigit(c) && c != '0' ) {
		n = 0;		/* start with a zero default */
		f = TRUE;	/* there is a # arg */
		mflag = 1;		/* current minus flag */
		while (isdigit(c) || (c == '-')) {
			if (c == '-') {
				/* already hit a minus or digit? */
				if ((mflag == -1) || (n != 0))
					break;
				mflag = -1;
			} else {
				n = n * 10 + (c - '0');
			}
			if ((n == 0) && (mflag == -1))	/* lonely - */
				mlwrite("arg:");
			else
				mlwrite("arg: %d",n * mflag);

			c = kbd_seq();	/* get the next key */
		}
		n = n * mflag;	/* figure in the sign */
	}
	*cp = c;
	*fp = f;
	*np = n * oldn;
}

/* do ^U-style repeat argument processing -- vile binds this to 'K' */
static void
do_rept_arg_proc(cp,fp,np)
int *cp, *fp, *np;
{
	register int c, f, n;
	register int	mflag;
	register int	oldn;
	c = *cp;

	if (c != reptc)
		return;

	f = *fp;
	n = *np;

	if (f)
		oldn = n;
	else
		oldn = 1;

	n = 4;		/* start with a 4 */
	f = TRUE;	/* there is a # arg */
	mflag = 0;			/* that can be discarded. */
	mlwrite("arg: %d",n);
	while (isdigit(c=kbd_seq()) || c==reptc || c=='-'){
		if (c == reptc)
			/* wow.  what does this do?  -pgf */
			/* (i've been told it controls overflow...) */
			if ((n > 0) == ((n*4) > 0))
				n = n*4;
			else
				n = 1;
		/*
		 * If dash, and start of argument string, set arg.
		 * to -1.  Otherwise, insert it.
		 */
		else if (c == '-') {
			if (mflag)
				break;
			n = 0;
			mflag = -1;
		}
		/*
		 * If first digit entered, replace previous argument
		 * with digit and set sign.  Otherwise, append to arg.
		 */
		else {
			if (!mflag) {
				n = 0;
				mflag = 1;
			}
			n = 10*n + c - '0';
		}
		mlwrite("arg: %d", (mflag >=0) ? n : (n ? -n : -1));
	}
	/*
	 * Make arguments preceded by a minus sign negative and change
	 * the special argument "^U -" to an effective "^U -1".
	 */
	if (mflag == -1) {
		if (n == 0)
			n++;
		n = -n;
	}

	*cp = c;
	*fp = f;
	*np = n * oldn;
}

/* handle all repeat counts */
void
do_repeats(cp,fp,np)
int *cp,*fp,*np;
{
	do_num_proc(cp,fp,np);
	do_rept_arg_proc(cp,fp,np);
	if (dotcmdmode == PLAY) {
		if (dotcmdarg)	/* then repeats are done by dotcmdcnt */
			*np = 1;
	} else {
		/* then we want to cancel any dotcmdcnt repeats */
		if (*fp) dotcmdarg = FALSE;
	}
}

/* the vi ZZ command -- write all, then quit */
int
zzquit(f,n)
int f,n;
{
	int thiscmd;
	int cnt;

	thiscmd = lastcmd;
	cnt = anycb();
	if (cnt) {
		mlprompt("Will write %d buffer%c  %s ",
			cnt, cnt > 1 ? 's':'.',
			clexec ? "" : "Repeat command to continue.");
		if (!clexec && !isnamedcmd) {
			if (thiscmd != kbd_seq())
				return FALSE;
		}

		if (writeall(TRUE,1) != TRUE)
			return FALSE;

	} else if (!clexec && !isnamedcmd) {
		/* consume the next char. anyway */
		if (thiscmd != kbd_seq())
			return FALSE;
	}
	return quithard(f, n);
}

/*
 * Fancy quit command, as implemented by Norm. If the any buffer has
 * changed do a write on that buffer and exit, otherwise simply exit.
 */
int
quickexit(f, n)
int f,n;
{
	register int status;
	if ((status = writeall(TRUE,1)) == TRUE)
		status = quithard(f, n);  /* conditionally quit	*/
	return status;
}

/* Force quit by giving argument */
/* ARGSUSED */
int
quithard(f,n)
int f,n;
{
	return quit(TRUE,1);
}

/*
 * Quit command. If an argument, always quit. Otherwise confirm if a buffer
 * has been changed and not written out.
 */
/* ARGSUSED */
int
quit(f, n)
int f,n;
{
	int cnt;

	if (f == FALSE && (cnt = anycb()) != 0) {
		if (cnt == 1)
			mlforce(
			"There is an unwritten modified buffer.  Write it, or use :q!");
		else
			mlforce(
			"There are %d unwritten modified buffers.  Write them, or use :q!",
				cnt);
		return FALSE;
	}
	vttidy(TRUE);
#if	FILOCK
	if (lockrel() != TRUE) {
		ExitProgram(BAD(1));
		/* NOTREACHED */
	}
#endif
#if UNIX
	(void)signal(SIGHUP,SIG_DFL);	/* I don't care anymore */
#endif
#if NO_LEAKS
	{
		beginDisplay;		/* ...this may take a while... */

		/* free all of the global data structures */
		onel_leaks();
		path_leaks();
		kbs_leaks();
		tb_leaks();
		wp_leaks();
		bp_leaks();
		vt_leaks();
		ev_leaks();
		tmp_leaks();
#if X11
		x11_leaks();
#endif

		free_local_vals(g_valuenames, global_g_values.gv);
		free_local_vals(b_valuenames, global_b_values.bv);
		free_local_vals(w_valuenames, global_w_values.wv);

		FreeAndNull(gregexp);
		FreeAndNull(patmatch);

#if UNIX
		if (strcmp(pathname[2], ".")) free(pathname[2]);
#endif
		/* whatever is left over must be a leak */
		show_alloc();
	}
#endif
	ExitProgram(GOOD);
	/* NOTREACHED */
	return FALSE;
}

/* ARGSUSED */
int
writequit(f,n)
int f,n;
{
	int s;
	s = filesave(FALSE,n);
	if (s != TRUE)
		return s;
	return quit(FALSE,n);
}

/*
 * Abort.
 * Beep the beeper. Kill off any keyboard macro, etc., that is in progress.
 * Sometimes called as a routine, to do general aborting of stuff.
 */
/* ARGSUSED */
int
esc(f, n)
int f,n;
{
	TTbeep();
	dotcmdmode = STOP;
	fulllineregions = FALSE;
	doingopcmd = FALSE;
	opcmd = 0;
	mlforce("[Aborted]");
	return ABORT;
}

/* tell the user that this command is illegal while we are in
   VIEW (read-only) mode				*/

int
rdonly()
{
	TTbeep();
	mlforce("[No changes are allowed while in \"view\" mode]");
	return FALSE;
}

/* ARGSUSED */
int
unimpl(f,n)
int f,n;
{
	TTbeep();
	mlforce("[Sorry, that vi command is unimplemented in vile ]");
	return FALSE;
}

int opercopy(f,n) int f,n; { return unimpl(f,n); }
int opermove(f,n) int f,n; { return unimpl(f,n); }
int opertransf(f,n) int f,n; { return unimpl(f,n); }

int operglobals(f,n) int f,n; { return unimpl(f,n); }
int opervglobals(f,n) int f,n; { return unimpl(f,n); }

int source(f,n) int f,n; { return unimpl(f,n); }

int visual(f,n) int f,n; { return unimpl(f,n); }
int ex(f,n) int f,n; { return unimpl(f,n); }

/* ARGSUSED */
int
nullproc(f,n)	/* user function that does (almost) NOTHING */
int f,n;
{
	return TRUE;
}

/* ARGSUSED */
int
cntl_af(f,n)	/* dummy function for binding to control-a prefix */
int f,n;
{
	return TRUE;
}

/* ARGSUSED */
int
cntl_xf(f,n)	/* dummy function for binding to control-x prefix */
int f,n;
{
	return TRUE;
}

/* ARGSUSED */
int
unarg(f,n) /* dummy function for binding to universal-argument */
int f,n;
{
	return TRUE;
}

/* initialize our version of the "chartypes" stuff normally in ctypes.h */
void
charinit()
{
	register int c;

	/* legal in pathnames */
	_chartypes_['.'] =
		_chartypes_['_'] =
		_chartypes_['-'] =
		_chartypes_['*'] =
		_chartypes_['/'] = _pathn;

	/* legal in "identifiers" */
	_chartypes_['_'] |= _ident|_qident;
	_chartypes_[':'] |= _qident;

	/* whitespace */
	_chartypes_[' '] =
		_chartypes_['\t'] =
		_chartypes_['\r'] =
		_chartypes_['\n'] =
		_chartypes_['\f'] = _space;

	/* control characters */
	for (c = 0; c < ' '; c++)
		_chartypes_[c] |= _cntrl;
	_chartypes_[127] |= _cntrl;

	/* lowercase */
	for (c = 'a'; c <= 'z'; c++)
		_chartypes_[c] |= _lower|_pathn|_ident|_qident;

	/* uppercase */
	for (c = 'A'; c <= 'Z'; c++)
		_chartypes_[c] |= _upper|_pathn|_ident|_qident;

	/* digits */
	for (c = '0'; c <= '9'; c++)
		_chartypes_[c] |= _digit|_pathn|_ident|_qident|_linespec;

	/* punctuation */
	for (c = '!'; c <= '/'; c++)
		_chartypes_[c] |= _punct;
	for (c = ':'; c <= '@'; c++)
		_chartypes_[c] |= _punct;
	for (c = '['; c <= '`'; c++)
		_chartypes_[c] |= _punct;
	for (c = '{'; c <= '~'; c++)
		_chartypes_[c] |= _punct;

	/* printable */
	for (c = ' '; c <= '~'; c++)
		_chartypes_[c] |= _print;

	/* backspacers: ^H, rubout, and the user's backspace char */
	/* we'll add the user's char later */
	_chartypes_['\b'] |= _bspace;
	_chartypes_[127] |= _bspace;

	/* wildcard chars for most shells */
	_chartypes_['*'] |= _wild;
	_chartypes_['?'] |= _wild;
#if !VMS
	_chartypes_['~'] |= _wild;
	_chartypes_['['] |= _wild;
	_chartypes_[']'] |= _wild;
	_chartypes_['$'] |= _wild;
	_chartypes_['{'] |= _wild;
	_chartypes_['}'] |= _wild;
	_chartypes_['`'] |= _wild;
#endif

	/* ex mode line specifiers */
	_chartypes_[','] |= _linespec;
	_chartypes_['%'] |= _linespec;
	_chartypes_['-'] |= _linespec;
	_chartypes_['+'] |= _linespec;
	_chartypes_['.'] |= _linespec;
	_chartypes_['$'] |= _linespec;
	_chartypes_['\''] |= _linespec;

	/* fences */
	_chartypes_['('] |= _fence;
	_chartypes_[')'] |= _fence;
	_chartypes_['['] |= _fence;
	_chartypes_[']'] |= _fence;
	_chartypes_['{'] |= _fence;
	_chartypes_['}'] |= _fence;

#if VMS
	_chartypes_['['] |= _pathn;	/* actually, "<", ">" too */
	_chartypes_[']'] |= _pathn;
	_chartypes_['$'] |= _pathn;
	_chartypes_[':'] |= _pathn;
	_chartypes_[';'] |= _pathn;
#endif

#if !SMALLER
	/* scratch-buffer-names (usually superset of _pathn) */
	_chartypes_[(unsigned)SCRTCH_LEFT[0]]  |= _scrtch;
	_chartypes_[(unsigned)SCRTCH_RIGHT[0]] |= _scrtch;
	_chartypes_[' '] |= _scrtch;	/* ...to handle "[Buffer List]" */
#endif

	for (c = 0; c < N_chars; c++) {
#if !SMALLER
		if (isspace(c) || isprint(c))
			_chartypes_[c] |= _shpipe;
		if (ispath(c))
			_chartypes_[c] |= _scrtch;
#endif
		if ((_chartypes_[c] & _space) == 0)
			_chartypes_[c] |= _nonspace;
	}
}


/*****		Compiler specific Library functions	****/

#if	MWC86 & MSDOS
movmem(source, dest, size)
char *source;	/* mem location to move memory from */
char *dest;	/* memory location to move text to */
int size;	/* number of bytes to move */
{
	register int i;

	for (i=0; i < size; i++)
		*dest++ = *source++;
}
#endif

#if	RAMSIZE
/*	These routines will allow me to track memory usage by placing
	a layer on top of the standard system malloc() and free() calls.
	with this code defined, the environment variable, $RAM, will
	report on the number of bytes allocated via malloc.

	with SHOWRAM defined, the number is also posted on the
	end of the bottom mode line and is updated whenever it is changed.
*/

#undef	realloc
#undef	malloc
#undef	free

	/* display the amount of RAM currently malloc'ed */
static void
display_ram_usage P((void))
{
	beginDisplay;
	if (global_g_val(GMDRAMSIZE)) {
		char mbuf[20];
		int	saverow = ttrow;
		int	savecol = ttcol;

		if (saverow >= 0 && saverow <= term.t_nrow
		 && savecol >= 0 && savecol <= term.t_ncol) {
			movecursor(term.t_nrow, LastMsgCol);
#if	COLOR
			TTforg(gfcolor);
			TTbacg(gbcolor);
#endif
			(void)lsprintf(mbuf, "[%ld]", envram);
			kbd_puts(mbuf);
			movecursor(saverow, savecol);
			TTflush();
		}
	}
	endofDisplay;
}

	/* reallocate mp with nbytes and track */
char *reallocate(mp, nbytes)
char *mp;
unsigned nbytes;
{
	if (mp != 0) {
		mp -= sizeof(SIZE_T);
		envram -= *((SIZE_T *)mp);
		nbytes += sizeof(SIZE_T);
		mp = realloc(mp, nbytes);
		if (mp != 0) {
			*((SIZE_T *)mp) = nbytes;
			envram += nbytes;
		}
		display_ram_usage();
	} else
		mp = allocate(nbytes);
	return mp;
}

	/* allocate nbytes and track */
char *allocate(nbytes)
unsigned nbytes;	/* # of bytes to allocate */
{
	char *mp;	/* ptr returned from malloc */

	nbytes += sizeof(SIZE_T);
	if ((mp = malloc(nbytes)) != 0) {
		(void)memset(mp, 0, nbytes);	/* so we can use for calloc */
		*((SIZE_T *)mp) = nbytes;
		envram += nbytes;
		mp += sizeof(SIZE_T);
		display_ram_usage();
	}

	return mp;
}

	/* release malloced memory and track */
void
release(mp)
char *mp;	/* chunk of RAM to release */
{
	if (mp) {
		mp -= sizeof(SIZE_T);
		envram -= *((SIZE_T *)mp);
		free(mp);
		display_ram_usage();
	}
}
#endif	/* RAMSIZE */

#if MALLOCDEBUG
mallocdbg(f,n)
{
	int lvl;
	lvl = malloc_debug(n);
	mlwrite("malloc debug level was %d",lvl);
	if (!f) {
		malloc_debug(lvl);
	} else if (n > 2) {
		malloc_verify();
	}
	return TRUE;
}
#endif


/*
 *	the log file is left open, unbuffered.  thus any code can do
 *
 * 	extern FILE *FF;
 *	fprintf(FF, "...", ...);
 *
 *	to log events without disturbing the screen
 */

#ifdef DEBUGLOG
/* suppress the declaration so that the link will fail if someone uses it */
FILE *FF;
#endif

void
start_debug_log(ac,av)	/* ARGSUSED */
int ac;
char **av;
{
#ifdef DEBUGLOG
	int i;
	FF = fopen("vilelog", "w");
	setbuf(FF,NULL);
	for (i = 0; i < ac; i++)
		(void)fprintf(FF,"arg %d: %s\n",i,av[i]);
#endif
}

#if TURBO
int
showmemory(f,n)
int	f,n;
{
	extern	long	coreleft(void);
	mlforce("Memory left: %D bytes", coreleft());
	return TRUE;
}
#endif

#if WATCOM
int
showmemory(f,n)
int	f,n;
{
	extern	long	_memavl(void);
	mlforce("Memory left: %D bytes", _memavl());
	return TRUE;
}
#endif

/*
 * Try to invoke 'exit()' from only one point so we can cleanup temporary
 * files.
 */
#if OPT_MAP_MEMORY
void
exit_program(code)
int	code;
{
	tmp_cleanup();
	exit(code);
}
#endif
