#include <ctype.h>
#include "pt.h"
/*
 * These routines are BSD regex(3)/ed(1) compatible regular-expression
 * routines written by Ozan S. Yigit, Computer Science, York University.
 * Parts of the code that are not needed by Prospero have been removed,
 * but most of the accompanying information has been left intact. 
 * This file is to be included on those operating systems that do not
 * support re_comp and re_exec.
 */

/*
 * regex - Regular expression pattern matching
 *         and replacement
 *
 * by:  Ozan S. Yigit (oz@nexus.yorku.ca)
 *      York University
 *
 * These routines are the PUBLIC DOMAIN equivalents 
 * of regex routines as found in 4.nBSD UN*X, with minor
 * extensions.
 *
 * Modification history:
 *
 * $Log: regex.c,v $
 * Revision 1.1  1994/02/15 22:12:39  jkh
 * Initial revision
 *
 * Revision 1.3  1992/03/04  17:07:18  crowley
 * Backup
 *
 * Revision 1.2  1992/02/19  19:56:49  crowley
 * Backup
 *
 * Revision 1.1  1992/02/19  16:43:42  crowley
 * Backup
 *
 * Revision 1.3  89/04/01  14:18:09  oz
 * Change all references to a dfa: this is actually an nfa.
 * 
 * Revision 1.2  88/08/28  15:36:04  oz
 * Use a complement bitmap to represent NCL.
 * This removes the need to have seperate 
 * code in the pmatch case block - it is 
 * just CCL code now.
 * 
 * Use the actual CCL code in the CLO
 * section of pmatch. No need for a recursive
 * pmatch call.
 * 
 * Use a bitmap table to set char bits in an
 * 8-bit chunk.
 * 
 * Routines:
 *      re_comp:        compile a regular expression into
 *                      a NFA.
 *
 *			char *re_comp(s)
 *			char *s;
 *
 *      re_exec:        execute the NFA to match a pattern.
 *
 *			int re_exec(s)
 *			char *s;
 *
 * Regular Expressions:
 *
 *      [1]     char    matches itself, unless it is a special
 *                      character (metachar): . \ [ ] * + ^ $
 *
 *      [2]     .       matches any character.
 *
 *      [3]     \       matches the character following it, except
 *			when followed by a left or right round bracket,
 *			a digit 1 to 9 or a left or right angle bracket. 
 *			(see [7], [8] and [9])
 *			It is used as an escape character for all 
 *			other meta-characters, and itself. When used
 *			in a set ([4]), it is treated as an ordinary
 *			character.
 *
 *      [4]     [set]   matches one of the characters in the set.
 *                      If the first character in the set is "^",
 *                      it matches a character NOT in the set, i.e. 
 *			complements the set. A shorthand S-E is 
 *			used to specify a set of characters S upto 
 *			E, inclusive. The special characters "]" and 
 *			"-" have no special meaning if they appear 
 *			as the first chars in the set.
 *                      examples:        match:
 *
 *                              [a-z]    any lowercase alpha
 *
 *                              [^]-]    any char except ] and -
 *
 *                              [^A-Z]   any char except uppercase
 *                                       alpha
 *
 *                              [a-zA-Z] any alpha
 *
 *      [5]     *       any regular expression form [1] to [4], followed by
 *                      closure char (*) matches zero or more matches of
 *                      that form.
 *
 *      [6]     +       same as [5], except it matches one or more.
 *
 *      [7]             a regular expression in the form [1] to [10], enclosed
 *                      as \(form\) matches what form matches. The enclosure
 *                      creates a set of tags, used for [8] and for
 *                      pattern substution. The tagged forms are numbered
 *			starting from 1.
 *
 *      [8]             a \ followed by a digit 1 to 9 matches whatever a
 *                      previously tagged regular expression ([7]) matched.
 *
 *	[9]	\<	a regular expression starting with a \< construct
 *		\>	and/or ending with a \> construct, restricts the
 *			pattern matching to the beginning of a word, and/or
 *			the end of a word. A word is defined to be a character
 *			string beginning and/or ending with the characters
 *			A-Z a-z 0-9 and _. It must also be preceded and/or
 *			followed by any character outside those mentioned.
 *
 *      [10]            a composite regular expression xy where x and y
 *                      are in the form [1] to [10] matches the longest
 *                      match of x followed by a match for y.
 *
 *      [11]	^	a regular expression starting with a ^ character
 *		$	and/or ending with a $ character, restricts the
 *                      pattern matching to the beginning of the line,
 *                      or the end of line. [anchors] Elsewhere in the
 *			pattern, ^ and $ are treated as ordinary characters.
 *
 *
 * Acknowledgements:
 *
 *	HCR's Hugh Redelmeier has been most helpful in various
 *	stages of development. He convinced me to include BOW
 *	and EOW constructs, originally invented by Rob Pike at
 *	the University of Toronto.
 *
 * References:
 *              Software tools			Kernighan & Plauger
 *              Software tools in Pascal        Kernighan & Plauger
 *              Grep [rsx-11 C dist]            David Conroy
 *		ed - text editor		Un*x Programmer's Manual
 *		Advanced editing on Un*x	B. W. Kernighan
 *		regexp routines			Henry Spencer
 *
 * Notes:
 *
 *	This implementation uses a bit-set representation for character
 *	classes for speed and compactness. Each character is represented 
 *	by one bit in a 128-bit block. Thus, CCL always takes a 
 *	constant 16 bytes in the internal nfa, and re_exec does a single
 *	bit comparison to locate the character in the set.
 *
 * Examples:
 *
 *	pattern:	foo*.*
 *	compile:	CHR f CHR o CLO CHR o END CLO ANY END END
 *	matches:	fo foo fooo foobar fobar foxx ...
 *
 *	pattern:	fo[ob]a[rz]	
 *	compile:	CHR f CHR o CCL bitset CHR a CCL bitset END
 *	matches:	fobar fooar fobaz fooaz
 *
 *	pattern:	foo\\+
 *	compile:	CHR f CHR o CHR o CHR \ CLO CHR \ END END
 *	matches:	foo\ foo\\ foo\\\  ...
 *
 *	pattern:	\(foo\)[1-3]\1	(same as foo[1-3]foo)
 *	compile:	BOT 1 CHR f CHR o CHR o EOT 1 CCL bitset REF 1 END
 *	matches:	foo1foo foo2foo foo3foo
 *
 *	pattern:	\(fo.*\)-\1
 *	compile:	BOT 1 CHR f CHR o CLO ANY END EOT 1 CHR - REF 1 END
 *	matches:	foo-foo fo-fo fob-fob foobar-foobar ...
 * 
 */

#define MAXNFA  1024
#define MAXTAG  10

#define OKP     1
#define NOP     0

#define CHR     1
#define ANY     2
#define CCL     3
#define BOL     4
#define EOL     5
#define BOT     6
#define EOT     7
#define BOW	8
#define EOW	9
#define REF     10
#define CLO     11
#define CHR2    12
	/* two characters (for case insensitive search) */

#define END     0

/*
 * The following defines are not meant
 * to be changeable. They are for readability
 * only.
 *
 */
#define MAXCHR	128
#define CHRBIT	8
#define BITBLK	MAXCHR/CHRBIT
#define BLKIND	0170
#define BITIND	07

#define ASCIIB	0177

typedef /*unsigned*/ char CHAR;

static int  tagstk[MAXTAG];             /* subpat tag stack..*/
static CHAR nfa[MAXNFA];		/* automaton..       */
static int  sta = NOP;               	/* status of lastpat */

static CHAR bittab[BITBLK];		/* bit table for CCL */
					/* pre-set bits...   */
static CHAR bitarr[] = {1,2,4,8,16,32,64,128};

static int internal_error;

static void
chset(c)
register CHAR c;
{
	bittab[((c) & BLKIND) >> 3] |= bitarr[(c) & BITIND];
}

#define badpat(x)	return (*nfa = END, x)
#define store(x)	*mp++ = x
 
char *     
re_comp(pat)
char *pat;
{
	extern int ignoreCase;
	register char *p;               /* pattern pointer   */
	register CHAR *mp = nfa;        /* nfa pointer       */
	register CHAR *lp;              /* saved pointer..   */
	register CHAR *sp = nfa;        /* another one..     */

	register int tagi = 0;          /* tag stack index   */
	register int tagc = 1;          /* actual tag count  */

	register int n;
	register CHAR mask;		/* xor mask -CCL/NCL */
	int c1, c2;
	char ch;
		
	if (!pat || !*pat)
		if (sta)
			return 0;
		else
			badpat("No previous regular expression");
	sta = NOP;

	for (p = pat; *p; p++) {
		lp = mp;
		switch(*p) {

		case '.':               /* match any char..  */
			store(ANY);
			break;

		case '^':               /* match beginning.. */
			store(BOL);
			break;

		case '$':               /* match endofline.. */
			store(EOL);
			break;

		case '[':               /* match char class..*/
			store(CCL);

			if (*++p == '^') {
				mask = 0377;	
				p++;
			}
			else
				mask = 0;

			if (*p == '-')		/* real dash */
				chset(*p++);
			if (*p == ']')		/* real brac */
				chset(*p++);
			while (*p && *p != ']') {
				if (*p == '-' && *(p+1) && *(p+1) != ']') {
					p++;
					c1 = *(p-2) + 1;
					c2 = *p++;
					while (c1 <= c2) {
						if(isalpha(c1) && ignoreCase) {
							chset(toupper(c1));
							chset(tolower(c1));
						} else
							chset(c1);
						++c1;
					}
				}
#ifdef EXTEND
				else if (*p == '\\' && *(p+1)) {
					p++;
					chset(*p++);
				}
#endif
				else {
					ch = *p++;
					if( isalpha(ch) && ignoreCase ) {
						chset(toupper(ch));
						chset(tolower(ch));
					} else
						chset(ch);
				}
			}
			if (!*p)
				badpat("Missing ]");

			for (n = 0; n < BITBLK; bittab[n++] = (char) 0)
				store(mask ^ bittab[n]);
	
			break;

		case '*':               /* match 0 or more.. */
		case '+':               /* match 1 or more.. */
			if (p == pat)
				badpat("Empty closure");
			lp = sp;		/* previous opcode */
			if (*lp == CLO)		/* equivalence..   */
				break;
			switch(*lp) {

			case BOL:
			case BOT:
			case EOT:
			case BOW:
			case EOW:
			case REF:
				badpat("Illegal closure");
			default:
				break;
			}

			if (*p == '+')
				for (sp = mp; lp < sp; lp++)
					store(*lp);

			store(END);
			store(END);
			sp = mp;
			while (--mp > lp)
				*mp = mp[-1];
			store(CLO);
			mp = sp;
			break;

		case '\\':              /* tags, backrefs .. */
			switch(*++p) {

			case '(':
				if (tagc < MAXTAG) {
					tagstk[++tagi] = tagc;
					store(BOT);
					store(tagc++);
				}
				else
					badpat("Too many \\(\\) pairs");
				break;
			case ')':
				if (*sp == BOT)
					badpat("Null pattern inside \\(\\)");
				if (tagi > 0) {
					store(EOT);
					store(tagstk[tagi--]);
				}
				else
					badpat("Unmatched \\)");
				break;
			case '<':
				store(BOW);
				break;
			case '>':
				if (*sp == BOW)
					badpat("Null pattern inside \\<\\>");
				store(EOW);
				break;
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				n = *p-'0';
				if (tagi > 0 && tagstk[tagi] == n)
					badpat("Cyclical reference");
				if (tagc > n) {
					store(REF);
					store(n);
				}
				else
					badpat("Undetermined reference");
				break;
			case 'b':
				store(CHR);
				store('\b');
				break;
			case 'n':
				store(CHR);
				store('\n');
				break;
			case 'f':
				store(CHR);
				store('\f');
				break;
			case 'r':
				store(CHR);
				store('\r');
				break;
			case 't':
				store(CHR);
				store('\t');
				break;
			default:
				store(CHR);
				store(*p);
			}
			break;

		default :               /* an ordinary char  */
			ch = *p;
			if( isalpha(ch) && ignoreCase ) {
				store(CHR2);
				store(toupper(ch));
				store(tolower(ch));
			} else {
				store(CHR);
				store(ch);
			}
			break;
		}
		sp = lp;
	}
	if (tagi > 0)
		badpat("Unmatched \\(");
	store(END);
	sta = OKP;
	return 0;
}

static char * bol;
int regex_bopat[MAXTAG];
int regex_eopat[MAXTAG];
int regex_pmatch();
static int pmatch();

static int line_count;

/*
 * re_exec:
 * 	execute nfa to find a match.
 *
 *	special cases: (nfa[0])	
 *		BOL
 *			Match only once, starting from the
 *			beginning.
 *		CHR
 *			First locate the character without
 *			calling pmatch, and if found, call
 *			pmatch for the remaining string.
 *		END
 *			re_comp failed, poor luser did not
 *			check for it. Fail fast.
 *
 *	If a match is found, regex_bopat[0] and regex_eopat[0] are set
 *	to the beginning and the end of the matched fragment,
 *	respectively.
 *
 */
 
static void
re_exec_init()
{
	line_count = 0;

	regex_bopat[0] = 0;
	regex_bopat[1] = 0;
	regex_bopat[2] = 0;
	regex_bopat[3] = 0;
	regex_bopat[4] = 0;
	regex_bopat[5] = 0;
	regex_bopat[6] = 0;
	regex_bopat[7] = 0;
	regex_bopat[8] = 0;
	regex_bopat[9] = 0;
	
	ClearByteCache();
}

int
re_exec( fid, cp, end_cp, lines_passed)
	int fid;
	register int cp;
	int end_cp;
	int *lines_passed;
{
	char c, c2;
	int ep = 0;
	CHAR *ap = nfa;
	char ch;

	re_exec_init();

	switch(*ap) {

	case CHR2:			/* ordinary char: locate it fast */
		c = *(ap+1);
		c2 = *(ap+2);
		ch = getCachedFileByte( fid, cp );
		if( ch == '\n' )
			++line_count;
		while( (cp<=end_cp) && (ch != c) && (ch != c2) ) {
			ch = getCachedFileByte( fid, ++cp );
			if( ch == '\n' )
				++line_count;
		}
		if( cp > end_cp )	/* if EOS, fail, else fall thru. */
			return 0;
		goto normalCase;

	case CHR:			/* ordinary char: locate it fast */
		c = *(ap+1);
		ch = getCachedFileByte( fid, cp );
		if( ch == '\n' )
			++line_count;
		while( (cp<=end_cp) && (ch != c) ) {
			ch = getCachedFileByte( fid, ++cp );
			if( ch == '\n' )
				++line_count;
		}
		if( cp > end_cp )	/* if EOS, fail, else fall thru. */
			return 0;
	default:			/* regular matching all the way. */
	normalCase:
		while( cp<=end_cp ) {
			/*SUPPRESS 560*/
			if ((ep = pmatch(fid,cp,end_cp,ap)))
				break;
			++cp;
		}
		break;
	case END:			/* munged automaton. fail always */
		return 0;
	}
	if (!ep)
		return 0;

	if (internal_error)
		return -1;

	regex_bopat[0] = cp;
	regex_eopat[0] = ep;
	*lines_passed = line_count;
	return 1;
}

int
re_exec_reversed( fid, cp, end_cp, lines_passed)
	int fid;
	register int cp;
	int end_cp;
	int *lines_passed;
{
	char c, c2;
	int ep = 0;
	CHAR *ap = nfa;
	char ch;
	int end_file = fileSize(fid) - 1;

	re_exec_init();

	switch(*ap) {

	case CHR2:			/* ordinary char: locate it fast */
		c = *(ap+1);
		c2 = *(ap+2);
		ch = getCachedFileByte( fid, end_cp );
		if( ch == '\n' )
			++line_count;
		while( (cp<=end_cp) && (ch != c) && (ch != c2) ) {
			ch = getCachedFileByte( fid, --end_cp );
			if( ch == '\n' )
				++line_count;
		}
		if( cp > end_cp )	/* if EOS, fail, else fall thru. */
			return 0;
		goto normalCase;

	case CHR:			/* ordinary char: locate it fast */
		c = *(ap+1);
		ch = getCachedFileByte( fid, end_cp );
		if( ch == '\n' )
			++line_count;
		while( (cp<=end_cp) && (ch != c) ) {
			ch = getCachedFileByte( fid, --end_cp );
			if( ch == '\n' )
				++line_count;
		}
		if( cp > end_cp )	/* if EOS, fail, else fall thru. */
			return 0;
	default:			/* regular matching all the way. */
	normalCase:
		while( cp<=end_cp ) {
			/*SUPPRESS 560*/
			if ((ep = pmatch(fid,end_cp,end_file,ap)))
				break;
			--end_cp;
		}
		break;
	case END:			/* munged automaton. fail always */
		return 0;
	}
	if (!ep)
		return 0;

	if (internal_error)
		return -1;

	regex_bopat[0] = end_cp;
	regex_eopat[0] = ep;
	*lines_passed = line_count;
	return 1;
}


/* 
 * pmatch: 
 *	internal routine for the hard part
 *
 * 	This code is mostly snarfed from an early
 * 	grep written by David Conroy. The backref and
 * 	tag stuff, and various other mods are by oZ.
 *
 *	special cases: (nfa[n], nfa[n+1])
 *		CLO ANY
 *			We KNOW ".*" will match ANYTHING
 *			upto the end of line. Thus, go to
 *			the end of line straight, without
 *			calling pmatch recursively. As in
 *			the other closure cases, the remaining
 *			pattern must be matched by moving
 *			backwards on the string recursively,
 *			to find a match for xy (x is ".*" and 
 *			y is the remaining pattern) where
 *			the match satisfies the LONGEST match
 *			for x followed by a match for y.
 *		CLO CHR
 *			We can again scan the string forward
 *			for the single char without recursion, 
 *			and at the point of failure, we execute 
 *			the remaining nfa recursively, as
 *			described above.
 *
 *	At the end of a successful match, regex_bopat[n] and regex_eopat[n]
 *	are set to the beginning and end of subpatterns matched
 *	by tagged expressions (n = 1 to 9).	
 *
 */

/*
 * character classification table for word boundary
 * operators BOW and EOW. the reason for not using 
 * ctype macros is that we can let the user add into 
 * our own table. see re_modw. This table is not in
 * the bitset form, since we may wish to extend it
 * in the future for other character classifications. 
 *
 *	TRUE for 0-9 A-Z a-z _
 */
static char chrtyp[MAXCHR] = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 
	1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
	0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
	1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
	1, 1, 1, 0, 0, 0, 0, 0
	};

#define inascii(x)	(0177&(x))
#define iswordc(x) 	chrtyp[inascii(x)]
#define isinset(x,y) 	((x)[((y)&BLKIND)>>3] & bitarr[(y)&BITIND])

/*
 * skip values for CLO XXX to skip past the closure
 *
 */

#define ANYSKIP	2 	/* [CLO] ANY END ...	     */
#define CHRSKIP	3	/* [CLO] CHR chr END ...     */
#define CHR2SKIP 4	/* [CLO] CHR chr END ...     */
#define CCLSKIP 18	/* [CLO] CCL 16bytes END ... */

static int
pmatch( fid, cp, end_cp, ap )
	int fid;
	register int cp;
	register int end_cp;
	register CHAR *ap;
{
	register int op, c, n;
	register int e;		/* extra pointer for CLO */
	register int bp;		/* beginning of subpat.. */
	register int ep;		/* ending of subpat..	 */
	int are;			/* to save the line ptr. */
	char ch, c2;

	while ((op = *ap++) != END) {
		ch = getCachedFileByte( fid, cp );
		if( ch == '\n' )
			++line_count;
		switch(op) {

		case CHR:
			++cp;
			if( ch != *ap++ )
				return 0;
			break;
		case CHR2:
			++cp;
			/* first check against the upper case letter */
			if( ch != *ap++ ) {
				/* check against the lower case letter */
				if( ch != *ap++ )
					/* if neither, then no match */
					return 0;
			} else
				++ap; /* skip the lower case character */
			break;
		case ANY:
			++cp;
			/* the ANY wildcard does not match newlines */
			if( (ch == '\n') || (cp > end_cp) )
				return 0;
			break;
		case CCL:
			++cp;
			if( !isinset(ap,ch) )
				return 0;
			ap += BITBLK;
			break;
		case BOL:
			if( cp > 0 && '\n' != getCachedFileByte(fid,cp-1) )
				return 0;
			break;
		case EOL:
			if( getCachedFileByte(fid,cp+1) != '\n' )
				return 0;
			break;
		case BOT:
			regex_bopat[(unsigned char)*ap++] = cp;
			break;
		case EOT:
			regex_eopat[(unsigned char)*ap++] = cp;
			break;
 		case BOW:
			if(
			    (cp > 0 && iswordc( getCachedFileByte(fid,cp-1) ))
			    || !iswordc(ch)
			 )
				return 0;
			break;
		case EOW:
			if( !iswordc(getCachedFileByte(fid,cp-1)) || iswordc(ch) )
				return 0;
			break;
		case REF:
			n = *ap++;
			bp = regex_bopat[n];
			ep = regex_eopat[n];
			while( bp < ep ) {
				if( getCachedFileByte(fid,bp++)
						!= getCachedFileByte(fid,cp++) )
					return 0;
			}
			break;
		case CLO:
/************************************
Handle line counts in closures correctly
***************************************/
			are = cp;
			switch( *ap ) {

			case ANY:
				/* the ANY wildcard does not match newlines */
				while( (ch != '\n') && (cp <= end_cp) )
					ch = getCachedFileByte( fid, ++cp );
				n = ANYSKIP;
				break;
			case CHR:
				c = *(ap+1);
				while( (cp<=end_cp) && c == ch )
					ch = getCachedFileByte( fid, ++cp );
				n = CHRSKIP;
				break;
			case CHR2:
				c = *(ap+1);
				c2 = *(ap+2);
				while( (cp<=end_cp) && (c == ch || c2 == ch) )
					ch = getCachedFileByte( fid, ++cp );
				n = CHR2SKIP;
				break;
			case CCL:
				while( (cp <= end_cp) && isinset(ap+1,ch) )
					ch = getCachedFileByte( fid, ++cp );
				n = CCLSKIP;
				break;
			default:
				internal_error++;
				return 0;
			}

			ap += n;

			while( cp >= are ) {
				/*SUPPRESS 560*/
				if( (e = pmatch(fid,cp,end_cp,ap)) )
					return e;
				--cp;
			}
			return 0;
		default:
			internal_error++;
			return 0;
		}
	}
	return cp;
}

static char * pmatch2();

int
re_match( lp )
	CHAR *lp;
{
	char * ep;
	
	bol = lp;

	ep = pmatch2( lp, nfa );

	if( ep == 0 )
		return 0;
	else
		return 1;
}

static char *
pmatch2( lp, ap )
	register CHAR *lp;
	register CHAR *ap;
{
	register int op, c, n;
	register char * e;		/* extra pointer for CLO */
	char * are;			/* to save the line ptr. */
	char ch2;

	while ((op = *ap++) != END) {
		switch(op) {

		case CHR2:
			ch2 = *lp++;
			if( ch2 != *ap++ ) {
				if( ch2 != *ap++ )
					return 0;
				/* else it matches to drop to the break */
			} else {
				/* skip the lower case letter */
				++ap;
			}
			break;
		case CHR:
			if( *lp++ != *ap++ )
				return 0;
			break;
		case ANY:
			if( !*lp++ )
				return 0;
			break;
		case CCL:
			c = *lp++;
			if( !isinset(ap,c) )
				return 0;
			ap += BITBLK;
			break;
		case BOL:
			if( lp != bol )
				return 0;
			break;
		case EOL:
			if( *lp )
				return 0;
			break;
		case CLO:
			are = lp;
			switch( *ap ) {

			case ANY:
				while( *lp )
					lp++;
				n = ANYSKIP;
				break;
			case CHR:
				c = *(ap+1);
				while( *lp && c == *lp )
					lp++;
				n = CHRSKIP;
				break;
			case CCL:
				while( (c = *lp) && isinset(ap+1,c) )
					lp++;
				n = CCLSKIP;
				break;
			default:
				internal_error++;
				return 0;
			}

			ap += n;

			while( lp >= are ) {
				/*SUPPRESS 560*/
				if( (e = pmatch2( lp, ap )) )
					return e;
				--lp;
			}
			return 0;
		default:
			internal_error++;
			return 0;
		}
	}
	return lp;
}

/*ARGSUSED*/
void
RegexReplaceAll( w, searchFor, replaceWith, inSelection )
	struct window * w;
	char * searchFor;
	char * replaceWith;
	int inSelection;
{
	printf("RegexReplaceAll: not implemented yet\n");
}

#define BUFFER_LENGTH	1024

/*ARGSUSED*/
int
RegexReplaceOne( w, searchFor, replaceWith )
	struct window * w;
	char * searchFor;
	char * replaceWith;
{
	extern Offset selBegin, selEnd;
	extern struct window *selWindow;

	int cp, cp_limit, diff_lengths;
	char buffer[BUFFER_LENGTH];
	char * to = buffer;
	char * to_limit = to + BUFFER_LENGTH;
	char * from = replaceWith;

	while( *from != '\0' ) {
		char ch = *from++;
		switch( ch ) {
		default:
			*to++ = ch;
			break;
		case '&':
			cp = regex_bopat[0];
			cp_limit = regex_eopat[0];
			while( cp < cp_limit ) {
				*to++ = getFileByte(w->fileId, cp++);
				if( to >= to_limit )
					--to;
			}
			break;
		case '\\':
			ch = *from++;
			switch( ch ) {
			default:
				*to++ = ch;
				break;
			case '\\':
				*to++ = '\\';
				break;
			case '1': case '2': case '3': case '4': case '5':
			case '6': case '7': case '8': case '9': 
				cp = regex_bopat[ch-'0'];
				cp_limit = regex_eopat[ch-'0'];
				while( cp < cp_limit ) {
					*to++ = getFileByte(w->fileId, cp++);
					if( to >= to_limit )
						--to;
				}
				break;
			}
		}
		if( to >= to_limit )
			--to;
	}
	*to = '\0';
	selBegin = regex_bopat[0];
	selEnd = regex_eopat[0] - 1;
	diff_lengths = strlen(buffer) - (selEnd-selBegin);
	if( selBegin <= selEnd )
		deleteChars( w->fileId, UPDATEWINDOWS, 1 );
	else
		selEnd = selBegin;
	for( from = buffer; *from != '\0'; ++from )
		insertChar( *from );
	drawWindow( selWindow );
	return diff_lengths;
}

