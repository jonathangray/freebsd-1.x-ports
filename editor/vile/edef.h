/*	EDEF:		Global variable definitions for vile
			

			written for MicroEMACS 3.9 by Dave G. Conroy
			modified by Steve Wilhite, George Jones
			greatly modified by Daniel Lawrence
			modified even more than that by Paul Fox.  honest.
*/

/*
 * $Log: edef.h,v $
 * Revision 1.1  1994/02/01 03:29:17  jkh
 * Initial revision
 *
 * Revision 1.115  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.114  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.113  1993/08/18  16:48:29  pgf
 * v. 3.59
 *
 * Revision 1.112  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.111  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.110  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.109  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.108  1993/07/09  19:17:35  pgf
 * v 3.54
 *
 * Revision 1.107  1993/07/07  16:15:02  pgf
 * v.3.53
 *
 * Revision 1.106  1993/07/06  16:54:55  pgf
 * v. 3.52
 *
 * Revision 1.105  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.104  1993/06/23  21:28:57  pgf
 * 3.50
 *
 * Revision 1.103  1993/06/22  10:36:31  pgf
 * 3.49
 *
 * Revision 1.102  1993/06/18  15:55:13  pgf
 * 3.48+
 *
 * Revision 1.102  1993/06/18  15:55:13  pgf
 * 3.48+
 *
 * Revision 1.101  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.100  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.99  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.98  1993/05/11  15:48:03  pgf
 * 3.46
 *
 * Revision 1.97  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.96  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.95  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.94  1993/04/22  12:12:58  pgf
 * dotcmdkreg should be 0 to start
 *
 * Revision 1.93  1993/04/21  14:09:01  pgf
 * added dotcmdarg and dotcmdkreg (the latter is unused as yet)
 *
 * Revision 1.92  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.88  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.87  1993/04/01  12:04:43  pgf
 * added keyboard read flag, and jmp_buf, for handling ^C with BSD-style
 * signals
 *
 * Revision 1.86  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.85  1993/03/18  17:41:15  pgf
 * v. 3.38
 *
 * Revision 1.84  1993/03/17  10:14:32  pgf
 * bumped to vers 3.37.  that was fast.
 *
 * Revision 1.83  1993/03/16  16:01:26  pgf
 * vers. 3.36
 *
 * Revision 1.82  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.81  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.80  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.79  1993/02/12  10:47:20  pgf
 * v. 3.33
 *
 * Revision 1.78  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.77  1993/01/23  15:08:35  foxharp
 * v. 3.31
 *
 * Revision 1.76  1993/01/23  13:38:23  foxharp
 * dfoutfn is no longer a global
 *
 * Revision 1.75  1993/01/16  10:47:39  foxharp
 * v 3.30
 *
 * Revision 1.74  1993/01/16  10:28:04  foxharp
 * chartypes is now array of longs, and autobuffer mode is new.
 *
 * Revision 1.73  1993/01/12  08:48:43  foxharp
 * tom dickey's changes to support "set number", i.e. line numbering
 *
 * Revision 1.72  1992/12/30  20:30:35  foxharp
 * v. 3.29
 *
 * Revision 1.71  1992/12/14  09:04:55  foxharp
 * v. 3.28
 *
 * Revision 1.70  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.69  1992/12/04  09:49:50  foxharp
 * for you, eric, i'll make the version string numeric...
 *
 * Revision 1.68  1992/12/02  09:13:16  foxharp
 * changes for "c-shiftwidth"
 *
 * Revision 1.67  1992/11/19  08:57:34  foxharp
 * not version four yet -- 3.26
 *
 * Revision 1.66  1992/08/28  09:08:01  foxharp
 * changed tagsrelative to tagrelative, because of name conflict w/ tags
 *
 * Revision 1.65  1992/08/27  08:31:39  foxharp
 * version four, finally
 *
 * Revision 1.64  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.63  1992/08/19  22:55:20  foxharp
 * v. 3.25
 *
 * Revision 1.62  1992/07/24  18:20:08  foxharp
 * v. 3.24
 *
 * Revision 1.61  1992/07/15  08:53:12  foxharp
 * added "slash", for UNIX vs. DOS path separators
 *
 * Revision 1.60  1992/07/13  20:08:17  foxharp
 * "terse" is now a boolean mode rather than a variable, and
 * added "tagsrelative" mode
 *
 * Revision 1.59  1992/07/13  09:25:32  foxharp
 * added "usefullpaths", which tells us all filenames are absolute
 *
 * Revision 1.58  1992/07/08  08:48:46  foxharp
 * v. 3.23
 *
 * Revision 1.57  1992/07/07  08:41:33  foxharp
 * v. 3.22
 *
 * Revision 1.56  1992/07/07  08:35:40  foxharp
 * v. 3.21
 *
 * Revision 1.55  1992/07/04  14:31:06  foxharp
 * insert_mode_was is now a global
 *
 * Revision 1.54  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.53  1992/06/14  12:40:30  foxharp
 * working on v. 3.20
 *
 * Revision 1.52  1992/06/12  22:23:42  foxharp
 * changes for separate 'comments' r.e. for formatregion
 *
 * Revision 1.51  1992/06/03  22:19:49  foxharp
 * v. 3.19
 *
 * Revision 1.50  1992/06/01  20:35:59  foxharp
 * added "tabinsert" support
 *
 * Revision 1.49  1992/05/27  08:32:57  foxharp
 * v 3.18
 *
 * Revision 1.48  1992/05/25  22:07:45  foxharp
 * v 3.17
 *
 * Revision 1.47  1992/05/25  21:09:01  foxharp
 * func. decls moved to proto.h
 *
 * Revision 1.46  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.45  1992/04/29  07:31:56  pgf
 * v 3.16
 *
 * Revision 1.44  1992/04/10  19:54:00  pgf
 * v 3.15
 *
 * Revision 1.43  1992/04/03  07:23:08  pgf
 * v. 3.14
 *
 * Revision 1.42  1992/03/24  07:35:39  pgf
 * since we include string.h, we also put the extern decls for index and rindex
 * here if we need them
 *
 * Revision 1.41  1992/03/23  08:40:19  pgf
 * v 3.13
 *
 * Revision 1.40  1992/03/19  23:42:34  pgf
 * version 3.12
 *
 * Revision 1.39  1992/03/19  23:16:11  pgf
 * vers. 3.11
 *
 * Revision 1.38  1992/03/13  19:49:30  pgf
 * version three point ten
 *
 * Revision 1.37  1992/03/05  09:17:21  pgf
 * added support for new "terse" variable, to control unnecessary messages
 *
 * Revision 1.36  1992/03/03  08:38:20  pgf
 * use string.h instead of our own decl's
 *
 * Revision 1.35  1992/02/17  09:18:32  pgf
 * version 3.9
 *
 * Revision 1.34  1992/02/17  08:59:33  pgf
 * added "showmode", and
 * macros, and dotcmd, kill registers all now hold unsigned chars
 *
 * Revision 1.33  1992/01/10  08:22:58  pgf
 * v. 3.85
 *
 * Revision 1.32  1992/01/10  07:11:20  pgf
 * added shiftwidth
 *
 * Revision 1.31  1992/01/03  23:30:35  pgf
 * added pre_op_dot as a global -- it's the current position at the start
 * of an operator command
 *
 * Revision 1.30  1991/12/30  23:15:04  pgf
 * version 3.8
 *
 * Revision 1.29  1991/12/04  09:23:48  pgf
 * now version three seven
 *
 * Revision 1.28  1991/11/08  13:17:40  pgf
 * lint cleanup (deleted unused's), and added klines/kchars
 *
 * Revision 1.27  1991/11/04  14:23:36  pgf
 * got rid of unused matchlen, matchpos, and mlenold
 *
 * Revision 1.26  1991/10/29  03:06:26  pgf
 * changes for replaying named registers
 *
 * Revision 1.25  1991/10/28  14:23:31  pgf
 * renamed curtabstopval to curtabval
 *
 * Revision 1.24  1991/10/28  01:11:56  pgf
 * version three point six
 *
 * Revision 1.23  1991/10/27  16:09:38  pgf
 * added gregexp, the compiled pattern
 *
 * Revision 1.22  1991/10/26  00:17:07  pgf
 * paragraph, sentence, sectino, and suffix regex values
 *
 * Revision 1.21  1991/10/24  13:05:52  pgf
 * conversion to new regex package -- much faster
 *
 * Revision 1.20  1991/10/22  03:07:34  pgf
 * bumped to version three 5ive
 *
 * Revision 1.19  1991/10/20  23:05:44  pgf
 * declared realloc()
 *
 * Revision 1.18  1991/10/18  10:56:54  pgf
 * modified VALUE structures and lists to make them more easily settable
 *
 * Revision 1.17  1991/10/15  03:08:57  pgf
 * added backspacelimit and taglength
 *
 * Revision 1.16  1991/09/26  13:07:45  pgf
 * moved LIST mode to window vals, and created window vals
 *
 * Revision 1.15  1991/09/19  13:34:30  pgf
 * added short synonyms for mode and value names, and made names more vi-compliant
 *
 * Revision 1.14  1991/08/13  02:47:23  pgf
 * alphabetized VAL_XXX's, and added "showmatch"
 *
 * Revision 1.13  1991/08/07  11:51:32  pgf
 * added RCS log entries
 *
 * revision 1.12
 * date: 1991/08/06 15:08:00;
 * global/local values
 * ----------------------------
 * revision 1.11
 * date: 1991/06/25 19:51:57;
 * massive data structure restructure
 * ----------------------------
 * revision 1.10
 * date: 1991/06/16 17:32:01;
 * added ctabstop, switched over to "values" array (global and local) to
 * hold things like tabstops and fillcol
 * ----------------------------
 * revision 1.9
 * date: 1991/06/06 13:58:06;
 * added auto-indent mode
 * ----------------------------
 * revision 1.8
 * date: 1991/06/03 18:07:34;
 * changed version number
 * to version three
 * ----------------------------
 * revision 1.7
 * date: 1991/06/03 17:34:49;
 * switch from "meta" etc. to "ctla" etc.
 * ----------------------------
 * revision 1.6
 * date: 1991/06/03 10:36:32;
 * added exmode flag
 * ----------------------------
 * revision 1.5
 * date: 1991/05/31 10:36:44;
 * bumped version no. to 2.3, and
 * changed plinesdone flag to more generic "calledbefore"
 * ----------------------------
 * revision 1.4
 * date: 1991/02/21 10:02:45;
 * don't need lbound for display anymore
 * ----------------------------
 * revision 1.3
 * date: 1990/10/03 16:04:07;
 * up'ed version number to 2.2
 * ----------------------------
 * revision 1.2
 * date: 1990/10/03 16:00:47;
 * make backspace work for everyone
 * ----------------------------
 * revision 1.1
 * date: 1990/09/21 10:25:05;
 * initial vile RCS revision
 */

/* I know this declaration stuff is really ugly, and I probably won't ever
 *	do it again.  promise.  but it _does_ make it easy to add/change
 *	globals. -pgf
 */
#ifdef realdef
# define decl_init(thing,value) thing = value
# define decl_uninit(thing) thing
#else
# define decl_init(thing,value) extern thing
# define decl_uninit(thing) extern thing
#endif

decl_uninit( char *prog_arg );		/* argv[0] from main.c */

decl_init( char prognam[], "vile");
decl_init( char version[], "version 3.61");

decl_init( int slash, '/'); 		/* so DOS can use '\' as path separator */

decl_init( int autoindented , -1);	/* how many chars (not cols) indented */
decl_uninit( int isnamedcmd );		/* are we typing a command name */
decl_uninit( int calledbefore );	/* called before during this command? */
decl_uninit( CMASK _chartypes_[N_chars] );	/* character types	*/
decl_uninit( int interrupted );		/* interrupt signal?		*/
decl_uninit( int displaying );		/* flag set during screen updates */
decl_uninit( int doing_kbd_read );	/* flag set during keyboard reading */
decl_uninit( int reading_msg_line );	/* flag set during msgline reading */
decl_uninit( jmp_buf read_jmp_buf );	/* for setjmp/longjmp on SIGINT */
decl_uninit( int insertmode );		/* are we inserting or overwriting? */
decl_uninit( int insert_mode_was );	/* were we (and will we be?)	*/
					/*	inserting or overwriting? */
decl_uninit( int lastkey );		/* last keystoke (tgetc)	*/
decl_init( int tungotc , -1);		/* last un-gotten key (tungetc) */
decl_uninit( int last1key );		/* last keystoke (kbd_key)	*/
decl_uninit( int lastcmd );		/* last command	(kbd_seq)	*/
decl_uninit( short fulllineregions );   /* regions should be full lines */
decl_uninit( short doingopcmd );        /* operator command in progress */
decl_uninit( MARK pre_op_dot );		/* current pos. before operator cmd */
decl_uninit( short opcmd );             /* what sort of operator?	*/
decl_uninit( CMDFUNC *havemotion );	/* so we can use "oper" routines
					   internally */
decl_uninit( int currow );              /* Cursor row                   */
decl_uninit( int curcol );              /* Cursor column                */
decl_uninit( WINDOW *curwp );           /* Current window               */
decl_uninit( BUFFER *curbp );           /* Current buffer               */
decl_uninit( WINDOW *wheadp );          /* Head of list of windows      */
decl_uninit( BUFFER *bheadp );          /* Head of list of buffers      */

decl_uninit( char sres[NBUFN] );	/* current screen resolution	*/

decl_uninit( char pat[NPAT] );          /* Search pattern		*/
decl_uninit( char rpat[NPAT] );		/* replacement pattern		*/

decl_uninit( regexp *gregexp );		/* compiled version of pat */

/* patmatch holds the string that satisfied the search command.  */
decl_uninit( char *patmatch );

decl_uninit( int ignorecase );

decl_init( int curgoal, -1 );           /* column goal			*/
decl_uninit( char *execstr );		/* pointer to string to execute	*/
decl_uninit( char golabel[NPAT] );	/* current line to go to	*/
decl_uninit( int execlevel );		/* execution IF level		*/
decl_init( int	eolexist, TRUE );	/* does clear to EOL exist	*/
decl_uninit( int revexist );		/* does reverse video exist?	*/
#if MSDOS || ZIBMPC || OPT_EVAL
decl_uninit( int flickcode );		/* do flicker suppression?	*/
#endif
decl_uninit( int curtabval );		/* current tab width		*/
decl_uninit( int curswval );		/* current shiftwidth		*/

#ifdef realdef
#if OPT_MAP_MEMORY
	MARK	nullmark = { {0,0}, 0 };
#else
	MARK	nullmark = { NULL, 0 };
#endif
#else
extern	MARK	nullmark;
#endif

#if ! WINMARK
decl_uninit( MARK Mark );		/* the worker mark */
#endif

/* these get their initial values in main.c, in global_val_init() */
decl_uninit( G_VALUES global_g_values );
decl_uninit( B_VALUES global_b_values );
decl_uninit( W_VALUES global_w_values );

decl_init( int sgarbf, TRUE );          /* TRUE if screen is garbage	*/
decl_uninit( int mpresf );              /* zero if message-line empty	*/
decl_uninit( int clexec	);		/* command line execution flag	*/
decl_uninit( int mstore	);		/* storing text to macro flag	*/
decl_init( int discmd, TRUE );		/* display command flag		*/
decl_init( int disinp, TRUE );		/* display input characters	*/
decl_uninit( struct BUFFER *bstore );	/* buffer to store macro text to*/
decl_uninit( int vtrow );               /* Row location of SW cursor	*/
decl_uninit( int vtcol );               /* Column location of SW cursor */
decl_init( int ttrow, HUGE );           /* Row location of HW cursor	*/
decl_init( int ttcol, HUGE );           /* Column location of HW cursor */
decl_uninit( int taboff	);		/* tab offset for display	*/

/* Special characters, used in keyboard control (some values are set on
 * initialization in termio.c).
 */
decl_init( int cntl_a, tocntrl('A') );	/* current meta character	*/
decl_init( int cntl_x, tocntrl('X') );	/* current control X prefix char */
decl_init( int reptc, 'K' );		/* current universal repeat char */
decl_init( int abortc, tocntrl('[') );	/* ESC: current abort command char */
decl_init( int quotec, tocntrl('V') );	/* quote char during mlreply()	*/
decl_init( int killc, tocntrl('U') );	/* current line kill char	*/
decl_init( int wkillc, tocntrl('W') );	/* current word kill char	*/
decl_init( int intrc, tocntrl('C') );	/* current interrupt char	*/
decl_init( int suspc, tocntrl('Z') );	/* current suspend char	*/
decl_init( int startc, tocntrl('Q') );	/* current output start char	*/
decl_init( int stopc, tocntrl('S') );	/* current output stop char	*/
decl_init( int backspc, '\b');		/* current backspace char	*/
decl_init( int name_cmpl, '\t');	/* do name-completion		*/
decl_init( int test_cmpl, '?');		/* show name-completion		*/

decl_uninit( KILLREG kbs[NKREGS] );	/* all chars, 1 thru 9, and default */
decl_uninit( short ukb );		/* index of current kbuffs */
decl_uninit( int kregflag );		/* info for pending kill into reg */
decl_uninit( int kchars );		/* how much did we kill? */
decl_uninit( int klines );
decl_uninit( int lines_deleted );	/* from 'ldelete()', for reporting */

#if !SMALLER
decl_uninit( WINDOW *swindow );		/* saved window pointer		*/
#endif

#if CRYPT
decl_init( int cryptflag, FALSE );	/* currently encrypting?	*/
decl_init( char * cryptkey, 0 );	/* top-level crypt-key, if any	*/
#endif

decl_init( int dotcmdmode, RECORD );	/* current dot command mode	*/
decl_init( int dotcmdarg, FALSE);	/* was there an arg to '.'? */
decl_uninit( int dotcmdkreg);		/* original dot command kill reg */
decl_uninit( TBUFF *dotcmd );		/* recorded-text of dot-commands */
decl_uninit( int dotcmdcnt );		/* down-counter for dot-commands */
decl_uninit( int dotcmdrep );		/* original dot-command repeat-count */

decl_init( int	kbdmode, STOP );	/* current keyboard macro mode	*/
decl_uninit( int seed );		/* random number seed		*/

#if RAMSIZE
decl_uninit( long envram );		/* # of bytes current used malloc */
#endif

#if OPT_EVAL || DEBUGM
decl_uninit( int macbug );		/* macro debugging flag		*/
#endif

#if OPT_WORKING
decl_uninit( B_COUNT max_working );	/* 100% value for slowreadf	*/
decl_uninit( B_COUNT cur_working );	/* current-value for slowreadf	*/
decl_uninit( B_COUNT old_working );	/* previous-value for slowreadf	*/
#endif

decl_init( char out_of_mem[], "OUT OF MEMORY" );
decl_init( char	errorm[], "ERROR" );	/* error literal		*/
decl_init( char	truem[], "TRUE" );	/* true literal			*/
decl_init( char	falsem[], "FALSE" );	/* false literal		*/

decl_init( int	cmdstatus, TRUE );	/* last command status		*/
decl_uninit( char palstr[49] );		/* palette string		*/
decl_uninit( char *fline );		/* dynamic return line		*/
decl_uninit( ALLOC_T flen );		/* current length of fline	*/

decl_uninit( int kbd_expand );		/* -1 kbd_putc shows tab as space */
					/* +1 kbd_putc shows cr as ^M */


decl_uninit( FILE *ffp );		/* File pointer, all functions. */
decl_uninit( int fileispipe );
decl_uninit( int eofflag );		/* end-of-file flag */

/* defined in nebind.h and nename.h */
extern NTAB nametbl[];
extern CMDFUNC *asciitbl[];
extern KBIND kbindtbl[];

/* terminal table defined only in TERM.C */

#ifndef	termdef
extern  TERM    term;                   /* Terminal information.        */
#endif

#if IBMPC
#if __ZTC__
extern int set43;
#endif	/* __ZTC__ */

extern int ibmtype;
#endif	/* IBMPC */
