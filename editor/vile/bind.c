/*	This file is for functions having to do with key bindings,
 *	descriptions, help commands and startup file.
 *
 *	written 11-feb-86 by Daniel Lawrence
 *
 * $Log: bind.c,v $
 * Revision 1.1  1994/02/01 03:29:11  jkh
 * Initial revision
 *
 * Revision 1.56  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.55  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.54  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.53  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.52  1993/07/19  15:28:23  pgf
 * moved hex digit processing from prc2kcod to token()
 *
 * Revision 1.51  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.50  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.49  1993/07/01  10:56:21  pgf
 * bugfix to prc2kcod()
 *
 * Revision 1.48  1993/06/30  14:07:54  pgf
 * made M- synonymous w/ FN-
 *
 * Revision 1.47  1993/06/28  20:03:29  pgf
 * cleaned prc2kcod, and added 0xNN and literal control character support
 *
 * Revision 1.46  1993/06/28  17:11:31  pgf
 * tightened up parsing and error checking for key-sequences
 *
 * Revision 1.45  1993/06/21  14:22:38  pgf
 * don't kbd_putc to the last column, to avoid auto-wrap problems.  should
 * really check ":am:", but this means adding to the TERM struct.
 *
 * Revision 1.44  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.43  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.42  1993/05/10  12:04:04  pgf
 * fnc2engl() now trys to return a long name for a function, rather
 * than a real short name
 *
 * Revision 1.41  1993/05/05  12:30:11  pgf
 * name changes for set-terminal stuff, and took out the keys that are
 * bindable with bind-key
 *
 * Revision 1.40  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.39  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.38  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.37  1993/04/21  15:40:40  pgf
 * change alternate eolchar for kbd_engl_stat to ' ', since NAMEC is now TAB
 *
 * Revision 1.36  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.35  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.34  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.33  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.32  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.31  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.30  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.29  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.28  1993/02/12  10:41:28  pgf
 * added new function, insertion_cmd(), which returns char that gives
 * us simple insert mode.  used in x11.c and input.c, for pasting and for
 * insert-mode arrow keys
 *
 * Revision 1.27  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.26  1993/01/16  10:21:17  foxharp
 * use new ScratchName, isShellOrPipe, and isreturn macros
 *
 * Revision 1.25  1992/12/04  09:08:45  foxharp
 * deleted unused assigns
 *
 * Revision 1.24  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.23  1992/06/04  19:45:14  foxharp
 * cast strlen() to int for new ANSI promotion semantics :-(
 *
 * Revision 1.22  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.21  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.20  1992/03/13  08:44:53  pgf
 * honor \n like \r in kbd_engl_stat
 *
 * Revision 1.19  1992/03/05  09:19:55  pgf
 * changed some mlwrite() to mlforce(), due to new terse support
 *
 * Revision 1.18  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.17  1992/01/03  23:31:49  pgf
 * use new ch_fname() to manipulate filenames, since b_fname is now
 * a malloc'ed sting, to avoid length limits
 *
 * Revision 1.16  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.15  1991/10/22  14:08:23  pgf
 * took out old ifdef BEFORE code
 *
 * Revision 1.14  1991/09/27  02:49:01  pgf
 * removed scalar init of static array
 *
 * Revision 1.13  1991/09/19  13:33:48  pgf
 * MDEXACT changed to MDIGNCASE
 *
 * Revision 1.12  1991/08/12  15:05:14  pgf
 * added fnc2key function, for getting back into insert mode
 *
 * Revision 1.11  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.10
 * date: 1991/08/06 15:10:56;
 * global/local values
 * and new printf/list stuff
 * 
 * revision 1.9
 * date: 1991/06/27 19:45:08;
 * fixed prompts
 * 
 * revision 1.8
 * date: 1991/06/03 17:34:51;
 * switch from "meta" etc. to "ctla" etc.
 * 
 * revision 1.7
 * date: 1991/06/03 13:58:22;
 * made bind description list better
 * 
 * revision 1.6
 * date: 1991/06/03 10:18:31;
 * fix apropos bug, and a bind nit
 * 
 * revision 1.5
 * date: 1991/05/31 10:31:34;
 * new kbd_engl_stat() routine, which returns more status, for use in the
 * new namedcmd() code
 * 
 * revision 1.4
 * date: 1990/12/06 19:49:07;
 * always rebuild Binding List buffer on request
 * 
 * revision 1.3
 * date: 1990/10/03 16:00:30;
 * make backspace work for everyone
 * 
 * revision 1.2
 * date: 1990/09/28 14:34:57;
 * changed prc2kcod decl to int
 * 
 * revision 1.1
 * date: 1990/09/21 10:24:44;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"
#include	"epath.h"

/* dummy prefix binding functions */
extern CMDFUNC f_cntl_af, f_cntl_xf, f_unarg, f_esc, f_speckey;

#if REBIND
#define	isSpecialCmd(k) \
		( (k == &f_cntl_af)\
		||(k == &f_cntl_xf)\
		||(k == &f_unarg)\
		||(k == &f_esc)\
		||(k == &f_speckey))
#if !SMALLER
static	int	chr_complete P(( int, char *, int * ));
static	int	chr_eol P(( char *, int, int, int ));
static	void	makechrslist P(( int, char *));
#endif
static	void	ostring P(( char * ));
static	char *	quoted P(( char *, char * ));
static	void	convert_kcode P(( int, char * ));
static	int	key_to_bind P(( CMDFUNC * ));
static	int	converted_len P(( char * ));
static	char *	to_tabstop P(( char * ));
static	int	is_shift_cmd P(( char *, int ));
#endif

static	char *	skip_partial P(( char *, int, char *, unsigned ));
static	void	show_partial P(( char *, int, char *, unsigned ));
static	int	fill_partial P(( char *, int, char *, char *, unsigned ));
static	int	cmd_complete P(( int, char *, int * ));
static	int	eol_command  P(( char *, int, int, int ));

/*----------------------------------------------------------------------------*/

/* give me some help!!!! bring up a buffer and read the help file into it */
/* ARGSUSED */
int
help(f, n)
int f,n;
{
	register BUFFER *bp;	/* buffer pointer to help */
	char *fname;		/* ptr to file returned by flook() */

	/* first check if we are already here */
	bp = bfind(ScratchName(Help), BFSCRTCH);
	if (bp == NULL)
		return FALSE;

	if (bp->b_active == FALSE) { /* never been used */
		fname = flook(pathname[1], FL_ANYWHERE);
		if (fname == NULL) {
			mlforce("[Sorry, can't find the help information]");
			(void)zotbuf(bp);
			return(FALSE);
		}
		/* and read the stuff in */
		if (readin(fname, 0, bp, TRUE) == FALSE ||
				popupbuff(bp) == FALSE) {
			(void)zotbuf(bp);
			return(FALSE);
		}
		set_bname(bp, ScratchName(Help));
		set_rdonly(bp, non_filename());

		make_local_b_val(bp,MDIGNCASE); /* easy to search, */
		set_b_val(bp,MDIGNCASE,TRUE);
		b_set_scratch(bp);
	}
	return swbuffer(bp);
}

static	int	poundc	= '#';	/* mark for special-bindings */

#if REBIND

#if !SMALLER

	/* patch: this table and the corresponding initializations should be
	 * generated by 'mktbls'.  In any case, the table must be sorted to use
	 * name-completion on it.
	 */
static struct {
		char	*name;
		int	*value;
		char	how_to;
	} TermChrs[] = {
		{"backspace",		&backspc,	's'},
		{"interrupt",		&intrc,		's'},
		{"line-kill",		&killc,		's'},
		{"name-complete",	&name_cmpl,	0},
		{"quote-next",		&quotec,	0},
		{"start-output",	&startc,	's'},
		{"stop-output",		&stopc,		's'},
		{"suspend",		&suspc,		's'},
		{"test-completions",	&test_cmpl,	0},
		{"word-kill",		&wkillc,	's'},
		{0}
	};

/*----------------------------------------------------------------------------*/

/* list the current chrs into the current buffer */
/* ARGSUSED */
static void
makechrslist(dum1,ptr)
int dum1;
char *ptr;
{
	register int i;
	char	temp[NLINE];

	bprintf("--- Terminal Settings %*P\n", term.t_ncol-1, '-');
	for (i = 0; TermChrs[i].name != 0; i++) {
		bprintf("\n%s = %s",
			TermChrs[i].name,
			kcod2prc(*(TermChrs[i].value), temp));
	}
}

/*
 * Find a special-character definition, given the name
 */
static	int	chr_lookup P(( char * ));
static int
chr_lookup(name)
char	*name;
{
	register int	j;
	for (j = 0; TermChrs[j].name != 0; j++)
		if (!strcmp(name, TermChrs[j].name))
			return j;
	return -1;
}

/*
 * The 'chr_complete()' and 'chr_eol()' functions are invoked from
 * 'kbd_reply()' to setup the mode-name completion and query displays.
 */
static int
chr_complete(c, buf, pos)
int	c;
char	*buf;
int	*pos;
{
	return kbd_complete(c, buf, pos, (char *)&TermChrs[0],
		sizeof(TermChrs[0]));
}

static int
/*ARGSUSED*/
chr_eol(buffer, cpos, c, eolchar)
char *	buffer;
int	cpos;
int	c;
int	eolchar;
{
	return isspace(c);
}

/* ARGSUSED */
int
set_termchrs(f,n)
int f,n;
{
	register int s, j;
	char	name[NLINE];
	int c;
#ifdef BEFORE
	CMDFUNC	*kcmd;
#endif

	/* get the table-entry */
	*name = EOS;
	if ((s = kbd_reply("Terminal setting: ", name, sizeof(name), chr_eol,
		' ', 0, chr_complete)) == TRUE) {

		j = chr_lookup(name);
		switch (TermChrs[j].how_to) {
#ifdef BEFORE
		case 'b':
			kcmd = engl2fnc(name);
			s = rebind_key(key_to_bind(kcmd), kcmd);
			break;
#endif
		case 's':
		default:
			c = key_to_bind((CMDFUNC *)0);
			if (c < 0)
				return(FALSE);
			*(TermChrs[j].value) = c;
			break;
		}
	}
	return s;
}

/* ARGSUSED */
int
show_termchrs(f,n)
int f,n;
{
	return liststuff(ScratchName(Terminal), makechrslist, 0, (char *)0);
}
#endif

static void
ostring(s)	/* output a string of output characters */
char *s;	/* string to output */
{
	if (discmd)
		kbd_puts(s);
}

/* ARGSUSED */
int
deskey(f, n)	/* describe the command for a certain key */
int f,n;
{
	register int c;		/* key to describe */
	register char *ptr;	/* string pointer to scan output strings */
	char outseq[NSTRING];	/* output buffer for command sequence */

	/* prompt the user to type us a key to describe */
	mlprompt("Describe the function bound to this key sequence: ");

	/* get the command sequence to describe
	   change it to something we can print as well */

	/* check to see if we are executing a command line */
	if (clexec) {
		char tok[NSTRING];
		macarg(tok);	/* get the next token */
		c = prc2kcod(tok);
		if (c < 0) {
			mlforce("[Illegal key-sequence \"%s\"]",tok);
			return(FALSE);
		}
	} else {
		c = kbd_seq();
		if (c < 0) {
			mlforce("[Not a bindable key-sequence]");
			return(FALSE);
		}
	}

	ostring(kcod2prc(c, outseq));
	ostring(" ");
	hst_append(outseq, EOS); /* cannot replay this, but can see it */

	/* find the right ->function */
	if ((ptr = fnc2engl(kcod2fnc(c))) == NULL)
		ptr = "Not Bound";

	/* output the command sequence */
	ostring(ptr);
	return TRUE;
}

/* bindkey:	add a new key to the key binding table		*/

/* ARGSUSED */
int
bindkey(f, n)
int f, n;	/* command arguments [IGNORED] */
{
	register CMDFUNC *kcmd;	/* ptr to the requested function to bind to */
	char cmd[NLINE];
	char *fnp;

	/* prompt the user to type in a key to bind */
	/* and get the function name to bind it to */
	fnp = kbd_engl("Bind function with english name: ", cmd);

	if (fnp == NULL || (kcmd = engl2fnc(fnp)) == NULL) {
		mlforce("[No such function]");
		return(FALSE);
	}

	return rebind_key(key_to_bind(kcmd), kcmd);
}

/*
 * Prompt-for and return the key-code to bind.
 */
static int
key_to_bind(kcmd)
register CMDFUNC *kcmd;
{
	char outseq[NLINE];	/* output buffer for keystroke sequence */
	register int c;

	mlprompt("...to keyboard sequence (type it exactly): ");

	/* get the command sequence to bind */
	if (clexec) {
		char tok[NSTRING];
		macarg(tok);	/* get the next token */
		c = prc2kcod(tok);
	} else {
		/* perhaps we only want a single key, not a sequence */
		/* 	(see more comments below) */
		if (isSpecialCmd(kcmd))
			c = kbd_key();
		else
			c = kbd_seq();
	}

	if (c >= 0) {
		/* change it to something we can print as well */
		ostring(kcod2prc(c, outseq));
		hst_append(outseq, FALSE);
	} else {
		mlforce("[Not a proper key-sequence]");
	}
	return c;
}

/*
 * Given a key-code and a command-function pointer, rebind the key-code to
 * the command-function.
 */
int
rebind_key (c, kcmd)
register int	c;
register CMDFUNC *kcmd;
{
	CMDFUNC ignored, *old = &ignored;
	return install_bind (c, kcmd, &old);
}

/*
 * Bind a command-function pointer to a given key-code (saving the old
 * value of the function-pointer via an pointer given by the caller).
 */
int
install_bind (c, kcmd, oldfunc)
register int	c;
register CMDFUNC *kcmd;
CMDFUNC **oldfunc;
{
	register KBIND *kbp;	/* pointer into a binding table */

	if (c < 0)
		return FALSE;	/* not a legal key-code */

	/* if the function is a prefix key, i.e. we're changing the definition
		of a prefix key, then they typed a dummy function name, which
		has been translated into a dummy function pointer */
	if (isSpecialCmd(kcmd)) {
		register int j;
		/* search for an existing binding for the prefix key */
		for (j = 0; j < N_chars; j++) {
			if (asciitbl[j] == kcmd) {
				(void)unbindchar(j);
				break;
			}
		}
		/* reset the appropriate global prefix variable */
		if (kcmd == &f_cntl_af)
			cntl_a = c;
		if (kcmd == &f_cntl_xf)
			cntl_x = c;
		if (kcmd == &f_unarg)
			reptc = c;
		if (kcmd == &f_esc)
			abortc = c;
		if (kcmd == &f_speckey)
			poundc = c;
	}
	
	if (!isspecial(c)) {
		*oldfunc = asciitbl[c];
		asciitbl[c] = kcmd;
	} else {
		kbp = kcode2kbind(c);
		if (kbp->k_cmd) { /* found it, change it in place */
			*oldfunc = kbp->k_cmd;
			kbp->k_cmd = kcmd;
		} else {
			if (kbp >= &kbindtbl[NBINDS-1]) {
				mlforce("[Prefixed binding table full]");
				return(FALSE);
			}
			kbp->k_code = c;	/* add keycode */
			kbp->k_cmd = kcmd;	/* and func pointer */
			++kbp;		/* and make sure the next is null */
			kbp->k_code = 0;
			kbp->k_cmd = NULL;
			*oldfunc   = NULL;
		}
	}
	return(TRUE);
}

/* unbindkey:	delete a key from the key binding table	*/

/* ARGSUSED */
int
unbindkey(f, n)
int f, n;	/* command arguments [IGNORED] */
{
	register int c;		/* command key to unbind */
	char outseq[NLINE];	/* output buffer for keystroke sequence */

	/* prompt the user to type in a key to unbind */
	mlprompt("Unbind this key sequence: ");

	/* get the command sequence to unbind */
	if (clexec) {
		char tok[NSTRING];
		macarg(tok);	/* get the next token */
		c = prc2kcod(tok);
		if (c < 0) {
			mlforce("[Illegal key-sequence \"%s\"]",tok);
			return FALSE;
		}
	} else {
		c = kbd_seq();
		if (c < 0) {
			mlforce("[Not a bindable key-sequence]");
			return(FALSE);
		}
	}

	/* change it to something we can print as well */
	ostring(kcod2prc(c, outseq));

	/* if it isn't bound, bitch */
	if (unbindchar(c) == FALSE) {
		mlforce("[Key not bound]");
		return(FALSE);
	}
	return(TRUE);
}

int
unbindchar(c)
int c;		/* command key to unbind */
{
	register KBIND *kbp;	/* pointer into the command table */
	register KBIND *skbp;	/* saved pointer into the command table */

	if (!isspecial(c)) {
		asciitbl[c] = NULL;
	} else {
		/* search the table to see if the key exists */
		kbp = kcode2kbind(c);

		/* if it isn't bound, bitch */
		if (kbp->k_cmd == NULL)
			return(FALSE);

		/* save the pointer and scan to the end of the table */
		skbp = kbp;
		while (kbp->k_cmd != NULL)
			++kbp;
		--kbp;		/* backup to the last legit entry */

		/* copy the last entry to the current one */
		skbp->k_code = kbp->k_code;
		skbp->k_cmd  = kbp->k_cmd;

		/* null out the last one */
		kbp->k_code = 0;
		kbp->k_cmd = NULL;
	}
	return TRUE;
}

/* describe bindings bring up a fake buffer and list the key bindings
		   into it with view mode			*/
/* ARGSUSED */
int
desbind(f, n)
int f,n;
{
	return liststuff(ScratchName(Binding List),makebindlist,1,(char *)0);
}

#if	APROP
/* ARGSUSED */
int
apro(f, n)	/* Apropos (List functions that match a substring) */
int f,n;
{
	static char mstring[NSTRING];	/* string to match cmd names to */
	register int    s;


	s = mlreply("Apropos string: ", mstring, sizeof(mstring));
	if (s != TRUE)
		return(s);

	return liststuff(ScratchName(Binding List),makebindlist,1,mstring);
}
#endif

/* returns a name in double-quotes */
static char *	
quoted(dst, src)
char	*dst;
char	*src;
{
	return strcat(strcat(strcpy(dst, "\""), src), "\"");
}

/* returns the number of columns used by the given string */
static int
converted_len(buffer)
register char	*buffer;
{
	register int len = 0, c;
	while ((c = *buffer++) != EOS) {
		if (c == '\t')
			len |= 7;
		len++;
	}
	return len;
}

/* force the buffer to a tab-stop if needed */
static char *
to_tabstop(buffer)
char	*buffer;
{
	register int	cpos = converted_len(buffer);
	if (cpos & 7)
		(void)strcat(buffer, "\t");
	return buffer + strlen(buffer);
}

/* convert a key binding, padding to the next multiple of 8 columns */
static void
convert_kcode(c, buffer)
int	c;
char	*buffer;
{
	(void)kcod2prc(c, to_tabstop(buffer));
}

/* build a binding list (limited or full) */
/* ARGSUSED */
void
makebindlist(dummy, mstring)
int dummy;
char *mstring;		/* match string if partial list, NULL to list all */
{
#if	ST520 & LATTICE
#define	register		
#endif
	register KBIND *kbp;	/* pointer into a key binding table */
	register NTAB *nptr,*nptr2;	/* pointer into the name table */
	char outseq[NLINE];	/* output buffer for keystroke sequence */
	int i,pass;
	int ok = TRUE;		/* reset if out-of-memory, etc. */

	/* let us know this is in progress */
	mlwrite("[Building binding list]");

	/* build the contents of this window, inserting it line by line */
	for (pass = 0; pass < 2; pass++) {
	    for (nptr = nametbl; nptr->n_name != NULL; ++nptr) {

		/* if we've already described this one, move on */
		if (nptr->n_cmd->c_flags & LISTED)
			continue;

		/* try to avoid alphabetizing by the real short names */
		if (pass == 0 && (int)strlen(nptr->n_name) <= 2)
			continue;

		/* add in the command name */
		(void)quoted(outseq, nptr->n_name);
		while (converted_len(outseq) < 32)
			(void)strcat(outseq, "\t");
		
#if	APROP
		/* if we are executing an apropos command
		   and current string doesn't include the search string */
		if (mstring && (strinc(outseq, mstring) == FALSE))
			continue;
#endif
		/* look in the simple ascii binding table first */
		for (i = 0; i < N_chars; i++)
			if (asciitbl[i] == nptr->n_cmd)
				convert_kcode(i, outseq);

		/* then look in the multi-key table */
		for (kbp = kbindtbl; kbp->k_cmd; kbp++)
			if (kbp->k_cmd == nptr->n_cmd)
				convert_kcode(kbp->k_code, outseq);

		/* dump the line */
		if (!addline(curbp,outseq,-1)) {
			ok = FALSE;
			break;
		}

		/* then look for synonyms */
		(void)strcpy(outseq, "\t");
		for (nptr2 = nametbl; nptr2->n_name != NULL; ++nptr2) {
			/* if it's the one we're on, skip */
			if (nptr2 == nptr)
				continue;
			/* if it's already been listed, skip */
			if (nptr2->n_cmd->c_flags & LISTED)
				continue;
			/* if it's not a synonym, skip */
			if (nptr2->n_cmd != nptr->n_cmd)
				continue;
			(void)quoted(outseq+1, nptr2->n_name);
			if (!addline(curbp,outseq,-1)) {
				ok = FALSE;
				break;
			}
		}

		nptr->n_cmd->c_flags |= LISTED; /* mark it as already listed */
	    }
	}

	for (nptr = nametbl; nptr->n_name != NULL; ++nptr)
		nptr->n_cmd->c_flags &= ~LISTED; /* mark it as unlisted */

	if (ok)
		mlerase();	/* clear the message line */
}

#if	APROP
int
strinc(sourc, sub)	/* does source include sub? */
char *sourc;	/* string to search in */
char *sub;	/* substring to look for */
{
	char *sp;	/* ptr into source */
	char *nxtsp;	/* next ptr into source */
	char *tp;	/* ptr into substring */

	/* for each character in the source string */
	sp = sourc;
	while (*sp) {
		tp = sub;
		nxtsp = sp;

		/* is the substring here? */
		while (*tp) {
			if (*nxtsp++ != *tp)
				break;
			else
				tp++;
		}

		/* yes, return a success */
		if (*tp == EOS)
			return(TRUE);

		/* no, onward */
		sp++;
	}
	return(FALSE);
}
#endif

#endif /* REBIND */


/* execute the startup file */

int
startup(sfname)
char *sfname;	/* name of startup file  */
{
	char *fname;	/* resulting file name to execute */

	/* look up the startup file */
	fname = flook(sfname, FL_HERE_HOME);

	/* if it isn't around, don't sweat it */
	if (fname == NULL) {
		mlforce("[Can't find startup file %s]",sfname);
		return(TRUE);
	}

	/* otherwise, execute the sucker */
	return(dofile(fname));
}

/*	Look up the existence of a file along the normal or PATH
	environment variable. Look first in the HOME directory if
	asked and possible
*/

char *
flook(fname, hflag)
char *fname;	/* base file name to search for */
int hflag;	/* Look in the HOME environment variable first? */
{
	register char *home;	/* path to home directory */
	register char *path;	/* environmental PATH variable */
	register char *sp;	/* pointer into path spec */
	register int i;		/* index */
	static char fspec[NSTRING];	/* full path spec to search */
#if VMS
	static TBUFF *myfiles;
#endif

	/* take care of special cases */
	if (!fname || !fname[0] || isspace(fname[0]))
		return NULL;
	else if (isShellOrPipe(fname))
		return fname;

	/* always try the current directory first */
	if (ffropen(fname) == FIOSUC) {
		ffclose();
		return(fname);
	}

	if (hflag == FL_HERE)
		return NULL;

#if	ENVFUNC

	if (hflag) {
		home = getenv("HOME");
		if (home != NULL) {
			/* try home dir file spec */
			if (ffropen(pathcat(fspec,home,fname)) == FIOSUC) {
				ffclose();
				return(fspec);
			}
		}
	}

	if (hflag == FL_HERE_HOME)
		return NULL;

#if PATHLOOK
#if VMS
	/* On VAX/VMS, the PATH environment variable is only the current-dir.
	 * Fake up an acceptable alternative.
	 */
	if (!tb_length(myfiles)) {
		char	mypath[NFILEN];

		(void)strcpy(mypath, prog_arg);
		if ((sp = vms_pathleaf(mypath)) == mypath)
			(void)strcpy(mypath, current_directory(FALSE));
		else
			*sp = EOS;

		if (!tb_init(&myfiles, EOS)
		 || !tb_sappend(&myfiles, mypath)
		 || !tb_sappend(&myfiles, ",SYS$SYSTEM:,SYS$LIBRARY:")
		 || !tb_append(&myfiles, EOS))
		return NULL;
	}
	path = tb_values(myfiles);
#else	/* UNIX or MSDOS */
	path = getenv("PATH");	/* get the PATH variable */
#endif

	if (path != NULL)
		while (*path) {

			/* build next possible file spec */
			sp = fspec;
			while (*path && (*path != PATHCHR))
				*sp++ = *path++;
			*sp = EOS;

			/* and try it out */
			if (ffropen(pathcat(fspec, fspec, fname)) == FIOSUC) {
				ffclose();
				return(fspec);
			}

			if (*path == PATHCHR)
				++path;
		}
#endif	/* PATHLOOK */
#endif	/* ENVFUNC */

	/* look it up via the old table method */
	for (i=2; i < NPNAMES; i++) {
		if (ffropen(pathcat(fspec, pathname[i], fname)) == FIOSUC) {
			ffclose();
			return(fspec);
		}
	}


	return NULL;	/* no such luck */
}

/* translate a keycode to its binding-string */
char *
kcod2str(c, seq)
int c;		/* sequence to translate */
char *seq;	/* destination string for sequence */
{
	register char *ptr = seq; /* pointer into current position in sequence */
	char	*base = ptr;

	if (c & CTLA)
		*ptr++ = cntl_a;

	if (c & CTLX)
		*ptr++ = cntl_x;

	if (c & SPEC)
		*ptr++ = poundc;
	
	*ptr++ = kcod2key(c);
	*ptr = EOS;
	return base;
}

/* translates a binding string into printable form */
char *
string2prc(dst, src)
char	*dst;
char	*src;
{
	char	*base;
	register int	c;
	register char	*tmp;

	for (base = dst; (c = (*dst = *src)) != EOS; dst++, src++) {
		tmp = NULL;

		if (c == ' ')
			tmp = "<sp>";
		else if (c == '\t')
			tmp = "<tab>";
		else if (iscntrl(c)) {
			*dst++ = '^';
			*dst = tocntrl(c);
		} else if (c == poundc && src[1] != EOS)
			tmp = "FN";
		else
			*dst = c;

		if (tmp != NULL) {
			while ((*dst++ = *tmp++) != EOS)
				;
			dst -= 2;	/* point back to last nonnull */
		}
		if (src[1] != EOS)
			*++dst = '-';
	}
	return base;
}

/* translate a 10-bit keycode to its printable name (like "M-j")  */
char *
kcod2prc(c, seq)
int c;		/* sequence to translate */
char *seq;	/* destination string for sequence */
{
	char	temp[NSTRING];
	return string2prc(seq, kcod2str(c,temp));
}

/* insertion_cmd -- what char puts us in insert mode? */
#if X11
int
insertion_cmd(direction)
int direction;
{
	extern CMDFUNC f_insert;
	extern CMDFUNC f_opendown;
	extern CMDFUNC f_openup;

	register int	c;

	switch (direction) {
	case -1:	c = fnc2key(&f_openup);		break;
	case 0:		c = fnc2key(&f_insert);		break;
	case 1:		c = fnc2key(&f_opendown);	break;
	default:	c = -1;
	}
	return c;
}
#endif /* X11 */

/* kcode2kbind: translate a 10-bit key-binding to the table-pointer
 */
KBIND *
kcode2kbind(code)
register int code;
{
	register KBIND	*kbp;	/* pointer into a binding table */

	for (kbp = kbindtbl; kbp->k_cmd && kbp->k_code != code; kbp++)
		;
	return kbp;
}

/* kcod2fnc:  translate a 10-bit keycode to a function pointer */
/*	(look a key binding up in the binding table)		*/
CMDFUNC *
kcod2fnc(c)
int c;	/* key to find what is bound to it */
{
	if (!isspecial(c)) {
		return asciitbl[c];
	} else {
		return kcode2kbind(c)->k_cmd;
	}
}

/* fnc2kcod: translate a function pointer to a keycode */
#ifdef GMDDOTMACRO
int
fnc2kcod(f)
CMDFUNC *f;
{
	register KBIND *kbp;
	register int	c;

	for (c = 0; c < N_chars; c++)
		if (f == asciitbl[c])
			return c;

	for (kbp = kbindtbl; kbp->k_cmd != 0; kbp++) {
		if (kbp->k_cmd == f)
			return kbp->k_code;
	}

	return -1;	/* none found */
}
#endif

/* fnc2engl: translate a function pointer to the english name for 
		that function
*/

char *
fnc2engl(cfp)
CMDFUNC *cfp;	/* ptr to the requested function to bind to */
{
	register NTAB *nptr;	/* pointer into the name table */
	register NTAB *shortnptr = NULL; /* pointer into the name table */

	/* skim through the table, looking for a match */
	for (nptr = nametbl; nptr->n_cmd; nptr++) {
		if (nptr->n_cmd == cfp) {
			/* if it's a long name, return it */
			if ((int)strlen(nptr->n_name) > 2)
				return nptr->n_name;
			/* remember the first short name, in case there's
				no long name */
			if (!shortnptr)
				shortnptr = nptr;
		}
	}
	if (shortnptr)
		return shortnptr->n_name;

	return NULL;
}

/* fnc2key: translate a function pointer to a simple key that is bound
		to that function
*/

#if X11
int
fnc2key(cfp)
CMDFUNC *cfp;	/* ptr to the requested function to bind to */
{
	register int i;

	for (i = 0; i < N_chars; i++) {
		if (cfp == asciitbl[i])
			return i;
	}
	return -1;
}
#endif

#if NEEDED
/* translate a function pointer to its associated flags */
fnc2flags(func)
CMDFUNC *cfp;	/* ptr to the requested function to bind to */
{
	register NTAB *nptr;	/* pointer into the name binding table */

	/* skim through the table, looking for a match */
	nptr = nametbl;
	while (nptr->n_cmd != NULL) {
		if (nptr->n_cmd == cfp) {
			return nptr->n_flags;
		}
		++nptr;
	}
	return NONE;
}
#endif


/* engl2fnc: match name to a function in the names table
	translate english name to function pointer
 		 return any match or NULL if none
 */
CMDFUNC *
engl2fnc(fname)
char *fname;	/* name to attempt to match */
{
	register NTAB *nptr;	/* pointer to entry in name binding table */
	register SIZE_T len = strlen(fname);

	if (len != 0) {	/* scan through the table, returning any match */
		nptr = nametbl;
		while (nptr->n_cmd != NULL) {
			if (strncmp(fname, nptr->n_name, len) == 0) {
				NTAB *test = (nptr+1);
				if (!strcmp(fname, nptr->n_name)
				 || test->n_name == NULL
				 || strncmp(fname, test->n_name, len) != 0)
					return nptr->n_cmd;
				else
					break;
			}
			++nptr;
		}
	}
	return NULL;
}

/* prc2kcod: translate printable code to 10 bit keycode */
int 
prc2kcod(kk)
char *kk;		/* name of key to translate to Command key form */
{
	register UINT c;	/* key sequence to return */
	register UINT pref = 0;	/* key prefixes */
	register int len = strlen(kk);
	register UCHAR *k = (UCHAR *)kk;

	if (len > 3 && *(k+2) == '-') {
		if (*k == '^') {
			if (*(k+1) == toalpha(cntl_a))
				pref = CTLA;
			if (*(k+1) == toalpha(cntl_x))
				pref = CTLX;
		} else if (!strncmp((char *)k, "FN", 2)) {
			pref = SPEC;
		}
		if (pref != 0)
			k += 3;
	} else if (len > 2 && !strncmp((char *)k, "M-", 2)) {
		pref = SPEC;
		k += 2;
	} else if (len > 1) {
		if (*k == cntl_a)
			pref = CTLA;
		else if (*k == cntl_x)
			pref = CTLX;
		else if (*k == poundc)
			pref = SPEC;
		if (pref != 0)
			k++;
	}

	/* a control char? */
	if (*k == '^' && *(k+1) != EOS) {
		c = *(k+1);
		if (islower(c)) c = toupper(c);
		c = tocntrl(c);
		k += 2;
	} else if (!strcmp((char *)k,"<sp>")) {
		c = ' ';		/* the string <sp> */
		k += 4;
	} else if (!strcmp((char *)k,"<tab>")) {
		c = '\t';		/* the string <tab> */
		k += 5;
	} else {		/* any single char, control or not */
		c = *k++;
	}

	if (*k != EOS)		/* we should have eaten the whole thing */
		return -1;
	
	return (int)(pref|c);
}


#if OPT_EVAL
/* translate printable code (like "M-r") to english command name */
char *
prc2engl(skey)	/* string key name to binding name.... */
char *skey;	/* name of key to get binding for */
{
	char *bindname;
	int c;

	c = prc2kcod(skey);
	if (c < 0)
		return "ERROR";

	bindname = fnc2engl(kcod2fnc(c));
	if (bindname == NULL)
		bindname = "ERROR";

	return bindname;
}
#endif

/*
 * Get an english command name from the user
 */
char *
kbd_engl(prompt, buffer)
char *prompt;		/* null pointer to splice calls */
char *buffer;
{
	if (kbd_engl_stat(prompt, buffer) == TRUE)
		return buffer;
	return NULL;
}

/* sound the alarm! */
void
kbd_alarm()
{
	TTbeep();
	TTflush();
}

/* put a character to the keyboard-prompt, updating 'ttcol' */
void
kbd_putc(c)
	int	c;
{
	beginDisplay;
	if ((kbd_expand <= 0) && isreturn(c)) {
		TTputc(c);
		ttcol = 0;
	} else if (isprint(c)) {
		if (ttcol < term.t_ncol-1) /* -1 to avoid auto-wrap problems */
			TTputc(c);
		ttcol++;
	} else if ((kbd_expand < 0) && (c == '\t')) {
		kbd_putc(' ');
	} else {
		kbd_putc('^');
		kbd_putc(toalpha(c));
	}
	endofDisplay;
}

/* put a string to the keyboard-prompt */
void
kbd_puts(s)
	char	*s;
{
	while (*s)
		kbd_putc(*s++);
}

/* erase a character from the display by wiping it out */
void
kbd_erase()
{
	beginDisplay;
	if (ttcol > 0) {
		if (--ttcol < term.t_ncol) {
			TTputc('\b');
			TTputc(' ');
			TTputc('\b');
		}
	} else
		ttcol = 0;
	endofDisplay;
}

/* definitions for name-completion */
#define	NEXT_DATA(p)	((p)+size_entry)
#define	PREV_DATA(p)	((p)-size_entry)

#ifdef	lint
static	/*ARGSUSED*/
char *	THIS_NAME(p)	char *p; { return 0; }
#else
#define	THIS_NAME(p)	(*(char **)(p))
#endif
#define	NEXT_NAME(p)	THIS_NAME(NEXT_DATA(p))

/*
 * Scan down until we no longer match the current input, or reach the end of
 * the symbol table.
 */
static char *
skip_partial(buf, len, table, size_entry)
char	*buf;
int	len;
char	*table;
unsigned size_entry;
{
	register char *	next = NEXT_DATA(table);
	register char *	sp;

	while ((sp = THIS_NAME(next)) != 0) {
		if (strncmp(buf, sp, len) != 0)
			break;
		next = NEXT_DATA(next);
	}
	return next;
}

/*
 * Shows a partial-match.  This is invoked in the symbol table at a partial
 * match, and the user wants to know what characters could be typed next.
 * If there is more than one possibility, they are shown in square-brackets.
 * If there is only one possibility, it is shown in curly-braces.
 */
static void
show_partial(buf, len, table, size_entry)
char	*buf;
int	len;
char	*table;
unsigned size_entry;
{
	register char	*next = skip_partial(buf, len, table, size_entry);
	register char	*last = PREV_DATA(next);
	register int	c;

	if (THIS_NAME(table)[len] == THIS_NAME(last)[len]) {
		kbd_putc('{');
		while ((c = THIS_NAME(table)[len]) != 0) {
			if (c == THIS_NAME(last)[len]) {
				kbd_putc(c);
				len++;
			} else
				break;
		}
		kbd_putc('}');
	}
	if (next != NEXT_DATA(table)) {
		c = TESTC;	/* shouldn't be in the table! */
		kbd_putc('[');
		while (table != next) {
			register char *sp = THIS_NAME(table);
			if (c != sp[len]) {
				c = sp[len];
				kbd_putc(c ? c : '$');
			}
			table = NEXT_DATA(table);
		}
		kbd_putc(']');
	}
	TTflush();
}

/*
 * Attempt to partial-complete the string, char at a time
 */
static int
fill_partial(buf, pos, first, last, size_entry)
char	*buf;
int	pos;
char	*first;
char	*last;
unsigned size_entry;
{
	register char	*p;
	register int	n = pos;
	char	*this_name = THIS_NAME(first);

	for (;;) {
		buf[n] = this_name[n];	/* add the next char in */
		buf[n+1] = EOS;

		/* scan through the candidates */
		for (p = NEXT_DATA(first); p != last; p = NEXT_DATA(p))
			if (THIS_NAME(p)[n] != buf[n]) {
				buf[n] = EOS;
				if (n == pos) TTbeep();
				TTflush();
				return n;
			}

		if (!clexec)
			kbd_putc(buf[n]); /* add the character */
		n++;
	}
}

static	int	testcol;	/* records the column when TESTC is decoded */

/*
 * Initializes the name-completion logic
 */
void
kbd_init()
{
	testcol = -1;
}

/*
 * Erases the display that was shown in response to TESTC
 */
void
kbd_unquery()
{
	beginDisplay;
	if (testcol >= 0) {
		while (ttcol > testcol)
			kbd_erase();
		TTflush();
		testcol = -1;
	}
	endofDisplay;
}

/*
 * This is invoked to find the closest name to complete from the current buffer
 * contents.
 */
int
kbd_complete(c, buf, pos, table, size_entry)
int	c;		/* TESTC, NAMEC or isreturn() */
char	*buf;
int	*pos;
char	*table;
unsigned size_entry;
{
	register int   cpos = *pos;
	register char *nbp;	/* first ptr to entry in name binding table */

	kbd_init();		/* nothing to erase */
	buf[cpos] = EOS;	/* terminate it for us */
	nbp = table;		/* scan for matches */

	while (THIS_NAME(nbp) != NULL) {
		if (strncmp(buf,  THIS_NAME(nbp), strlen(buf)) == 0) {
			/* a possible match! exact? no more than one? */
			if (c == TESTC) {
				testcol = ttcol;
				show_partial(buf, cpos, nbp, size_entry);
				return FALSE;
			}
			if (strcmp(buf,  THIS_NAME(nbp)) == 0 || /* exact? */
				NEXT_NAME(nbp) == NULL ||
				strncmp(buf, NEXT_NAME(nbp), strlen(buf)) != 0)
			{
				/* exact or only one like it.  print it */
				if (!clexec) {
					kbd_puts(THIS_NAME(nbp) + cpos);
					TTflush();
				}
				if (c != NAMEC)  /* put it back */
					tungetc(c);
				/* return complete name */
				(void)strncpy(buf, THIS_NAME(nbp), NLINE);
				*pos = cpos;
				return TRUE;
			}

			/* try for a partial match against the list */
			*pos = fill_partial(buf, cpos, nbp,
				skip_partial(buf, cpos, nbp, size_entry),
				size_entry);
			return FALSE;

		}
		nbp = NEXT_DATA(nbp);
	}

	kbd_alarm();	/* no match */
	buf[*pos = cpos] = EOS;
	return FALSE;
}

/*
 * Test a buffer to see if it looks like a shift-command, which may have
 * repeated characters (but they must all be the same).
 */
static int
is_shift_cmd(buffer, cpos)
char	*buffer;
int	cpos;
{
	register int c = *buffer;
	if (isRepeatable(c)) {
		while (--cpos > 0)
			if (*(++buffer) != c)
				return FALSE;
		return TRUE;
	}
	return FALSE;
}

/*
 * The following mess causes the command to terminate if:
 *
 *	we've got a space
 *		-or-
 *	we're in the first few chars and we're switching from punctuation
 *	to alphanumerics, or vice-versa.  oh yeah -- '!' is considered
 *	alphanumeric today.
 *	All this allows things like:
 *		: e#
 *		: !ls
 *		: q!
 *	to work properly.
 *	If we pass this "if" with c != NAMEC, then c is ungotten below,
 *	so it can be picked up by the commands argument getter later.
 */
static int
eol_command(buffer, cpos, c, eolchar)
char *	buffer;
int	cpos;
int	c;
int	eolchar;
{
	/*
	 * Handle special case of repeated-character implying repeat-count
	 */
	if (is_shift_cmd(buffer, cpos) && (c == *buffer))
		return TRUE;

	/*
	 * Shell-commands aren't complete until the line is complete.
	 */
#if OPT_HISTORY
	if ((cpos > 0) && isShellOrPipe(buffer))
		return isreturn(c);
#endif

	return	(c == eolchar)
	  ||	(
		  cpos > 0
	      &&  cpos < 3
	      &&(
		  (!ispunct(c)
		&&  ispunct(buffer[cpos-1])
		  )
		|| ((c != '!' && ispunct(c))
		  && (buffer[cpos-1] == '!' || !ispunct(buffer[cpos-1]))
		  )
		)
	      );
}

/*
 * This procedure is invoked from 'kbd_string()' to setup the command-name
 * completion and query displays.
 */
static int
cmd_complete(c, buf, pos)
int	c;
char	*buf;
int	*pos;
{
	register int status;
#if OPT_HISTORY
	/*
	 * If the user scrolled back in 'edithistory()', the text may be a
	 * repeated-shift command, which won't match the command-table (e.g.,
	 * ">>>").
	 */
	if ((*pos > 1) && is_shift_cmd(buf, *pos)) {
		int	len = 1;
		char	tmp[NLINE];
		tmp[0] = *buf;
		tmp[1] = EOS;
		status = cmd_complete(c, tmp, &len);
	} else if ((*pos > 0) && isShellOrPipe(buf)) {
		status = isreturn(c);
		if (c != NAMEC)
			tungetc(c);
	} else
#endif
	 status = kbd_complete(c, buf, pos, (char *)&nametbl[0], sizeof(nametbl[0]));
	return status;
}

int
kbd_engl_stat(prompt, buffer)
char	*prompt;
char	*buffer;
{
	int	kbd_flags = KBD_EXPCMD|KBD_NULLOK|((NAMEC != ' ') ? 0 : KBD_MAYBEC);

	*buffer = EOS;
	return kbd_reply(
		prompt,		/* no-prompt => splice */
		buffer,		/* in/out buffer */
		NLINE,		/* sizeof(buffer) */
		eol_command,
		' ',		/* eolchar */
		kbd_flags,	/* allow blank-return */
		cmd_complete);
}
