/* -*-C-*- dvialw.c */
/*-->dvialw*/
/**********************************************************************/
/******************************* dvialw *******************************/
/**********************************************************************/

#include "dvihead.h"

/***********************************************************************
[23-Oct-87]	Modify output so that on  each page, fonts appear first,
		followed  by   "save  ...text...   restore"   sequences.
		Current  PostScript    implementations    on  commercial
		printers  lack  garbage collection,  and strings consume
		memory  even after printing.   The Apple LaserWriter and
		LaserWriter Plus have  only  about 100Kb free for   each
		job, which  at  current   memory   prices    is  grossly
		inadequate.     Other PostScript  printer  vendors   are
		offering substantially more memory.

		This feature is implemented not by time-consuming double
		passes   through  the  DVI file, but    rather by having
		setchar() and   setstr() buffer their output,   which is
		dumped  (with bracketing save/restore   commands) either
		when  the  buffer fills  up,   or when  end-of-page   is
		reached.   A buffer of 8Kb is  adequate for this purpose
		(based  on a 100-page document (dvidriver.ltx) producing
		a  .dvi-alw file of  830Kb,   of  which  163Kb was  font
		definitions,  giving  an  average  text  requirement  of
		6.7Kb/page).  These  statistics were determined  on Unix
		by

		awk '/^\[/,/^>/' foo.dvi-alw >foo.tmp
		ls -l foo.*

		Awk extracts the font definitions into the file foo.tmp,
		and ls tells how big the files  are.   A similar test on
		pages  from the TeXBook  found some pages  needing up to
		11Kb of text.

		Several new routines (putfontname(), text*()) and macros
		(OUT_*) are introduced to support these  changes.   Some
		care is needed when  introducing new  output sections to
		call OUT_IMMEDIATE or  OUT_DEFERRED,  as appropropriate.
		If this is  not done,  output  will  be in an  incorrect
		order.

		Now that  almost   all output  flows  through  a  single
		routine, textchr(), it is possible to monitor the output
		to ensure   that lines do not   exceed   PS_WIDTH.  Text
		output  for the \special{}  command   is an exception to
		this  rule; it  is transmitted exactly  as found.   Long
		comments are continued  with a trailing "-"  followed by
		"%-" on the  next  line.   When textflush()  empties the
		text buffer, newlines preceding "(" (the commonest case)
		are  squeezed out,  as long as this does  not exceed the
		line width limit.   Font   changes are shortened  to  F#
		sequences, where # is the sequential number of  the font
		(1,2,...) in the   dvi file.    F0  is reserved    for a
		temporary font created inside a  save/restore pair for a
		large character.

		Perhaps at some later  date, we could  sort the buffered
		text by font type, and minimize font changing.

[10-Dec-86]	Fix off-by-one  error in  value  of y-offset  output  in
		character definition  in loadchar();  this affects  only
		the combination of rules with characters, such as  LaTeX
		arrowheads.  No other DVI drivers are affected by this.

		Fix off-by-one error in size  of rule; this involves  no
		code  change   here,  only   a  change   in   dvialw.ps.
		Examination of output rules  under high power  magnifier
		shows that to draw a rule of width N dots, the  bounding
		box should have a width of N-1 dots; the boundary counts
		toward the fill; this is  not clear from the  PostScript
		manual.

[13-Jun-86]	Fix  special()--a  short  plot  file  could  result   in
		infinite  loop  in  search  for  %%BoundingBox.    Fixed
		openfont() to output requested magnification as well  as
		substituted magnification.

[19-Apr-86]	Fix infinite  loop in  setstr(); BIGCHAR()  forced  exit
		from character collection loop  on first iteration,  and
		the current input  character was stuffed  back into  the
		input buffer.	A  big character  should  just  suppress
		downloading in the first loop.

[12-Mar-86]	Version 2.03  -- Major  overhaul of  PostScript  output.
		Removed most explicit PostScript commands to header file
		dvialw.ps (in current directory), or if unavailable, the
		system  file  texinputs:dvialw.ps  (defined  by  symbols
		PSDEF_PATH and PSDEF_FILE) which is copied to the output
		file by new procedure  cppsfile(), stripped of  comments
		and excess whitespace.  References  to these macros  are
		output instead.   This  makes  parameter  twiddling  and
		local customization much easier, and will allow users to
		obtain a  variety of  page formats  and orientations  by
		simple changes to  personal copies of  the header  file.
		The header  file  is  heavily commented  and  should  be
		referred to for macro descriptions and an outline of the
		general PostScript document format produced by DVIALW.

		Removed variable-length  argument lists  to fatal()  and
		warning() for  portability.   Changed  all  preprocessor
		#ifdef's and  #ifndef's to  #if's for  portability;  all
		symbols   for	 devices,   operating	 systems,    and
		implementations are now explicitly defined to be 0 or 1.

		Added procedures fontfile() and fontsub() to encapsulate
		construction of  system-dependent  font file  names  and
		provide  for  user-specifiable  font  substitutions  for
		unavailable font files.

		Fonts and character definitions are now encapsulated  in
		compact macros.  With  help of Neal  Holtz's DVI2PS  and
		TEX.PS, figured  out how  to  define fonts  with  origin
		along baseline, allowing output of several characters in
		a single command (except  where roundoff would make  TeX
		and  PostScript  positions  disagree).	 New   procedure
		setstr() handles this.  Font scaling is now 1 for normal
		output, but  can  be  changed  to  any  other  value  if
		PostScript is to rescale the bitmaps (slow).  Tests show
		a reduction  of about  20% in  total output  file  size.
		When PS_SHORTLINES  is  defined, character  bitmaps  are
		output one  scan line  per  line, making  them  somewhat
		readable, and avoiding long lines.  Leaving them as  one
		long line  could  reduce  the file  size  even  further.
		Added support for LaTeX invisible  fonts; if you send  a
		bitmap of size 0  by 0 to  the LaserWriter, all  further
		fonts are printed as blanks  until you cycle the  power.
		No character definitions are loaded for such fonts.

		Made reloading of fonts for each page an option "-v"; it
		should now rarely be necessary.  With large manuscripts,
		avoiding reloading cuts  the space by  60% or more,  and
		seems to be  more successful at  printing, averaging  30
		sec/page and  700-780 char/sec  (close to  limit of  960
		char/sec from 9600 baud serial line).

		Removed old #ifdef  FOOBAR ... #endif  code sections  in
		several  procedures  which  were  completely   obsolete.
		Added header comment line to every file; it contains the
		EMACS "-*-C-*-" mode string and the exact (case-sensive)
		filename, since many  functions have  been defined  with
		names in  mixed case  for readability  (probably  should
		have used underscore instead, but none do), and on Unix,
		the letter case matters.

		Replaced index()  and  rindex(),  which  have  different
		definitions in  different C  implementations, by  4.2BSD
		(and coming  ANSI  C standard)  functions  strchr()  and
		strrchr(), for which .h files are provided.

		To further reduce the PostScript output size, PostScript
		interactive mode experiments determined that spaces  are
		not required  around parentheses,  brackets, or  braces,
		but ARE necessary between  numbers and names.  Thus  the
		sequences

		[ <hexstring> # # # # # ] # D
		(string) # # S

		can be reduced to

		[<hexstring># # # # #]# D
		(string)# # S

		saving 4 and 1 characters  respectively for each use  of
		these commands.  The PostScript file copy utility, LW78,
		already reduces CR LF pairs  to LF, so in the  interests
		of editability,  we  retain  the  former.   Heavily-used
		macros have  also  been  redefined  with  single  letter
		names.

[18-Jan-86]	Version 2.02 -- Added macros  to define new fonts  (NF),
		set fonts (SF),  and define characters  (CH) in  main.h,
		and use  them  in  readfont.h  and  setchar().	 Changed
		setchar() to skip  character loading  when character  is
		not  on  page.	  Added  setstr()   to  output	 several
		characters at  a time,  instead of  using setchar()  for
		each  of  them.   These  reduce  the  output  file  size
		considerably.  Moved character  bitmap output code  into
		loadchar() where it should have been in the first place.
		Added test  for  output  errors  (usually  because  disk
		storage is exhausted) in main.h and prtpage.h.

[20-Dec-85]	Following recommendation of Allan Hetzel on LASER-LOVERS
		ARPA BBOARD, added "() pop" at top of every page.  This
		selected by the switch PS_XONXOFFBUG:

		--------------------------------------------------------
		From: Allan Hetzel  <SYSAL%UKCC.BITNET@WISCVM.ARPA>
		Subject: Re: Apple Laserwriter XON/XOFF problem
		To:  <LASER-LOVERS@WASHINGTON.ARPA>

		The note  posted to  Laser-Lovers on  Oct 28  by  Joseph
		Goldstone of  Symbolics  Cambridge Research  Center  was
		helpful but I didn't try his suggested fix correctly and
		so I called Adobe.  They were very helpful (much more so
		than Apple) and explained the problem and how to  bypass
		it.

		My  apologies  to   Adobe  if  this   not  an	accurate
		description of the problem.   The problem apparently  is
		due to the  PostScript interpreter  getting confused  by
		what it thinks is an excess of white space characters.

		The bypass is to place a special character (parenthesis,
		bracket, brace, slash, etc.) to the right of some token,
		but without an intervening white space character. As the
		PostScript scanner  reads the  token it  also reads  the
		special character which acts as a delimiter. The scanner
		then has  to  back  up  one character  so  that  it  can
		reprocess the special character  after it has  processed
		the preceding token.  During  this backing up process  a
		number of  internal values  are recalculated,  including
		some pointer  into the  input buffer.	This causes  the
		XON/XOFF protocol to work properly.  Doing this once per
		page seems to keep everybody happy.

		Each page  in  the PostScript  file  built by  our  word
		processing program  is surrounded  by a  "save  restore"
		sequence.  We  changed	 the  beginning  save	sequence
		"/saveobj save def" to  read "/saveobj save def()  pop".
		The "() pop" is effectively  a no-op and we are  assured
		that the necessary recalculations are done on each page.
		This seems to have corrected the problem completely.

		Allan Hetzel (sysal@ukcc.bitnet)
		--------------------------------------------------------
***********************************************************************/

/**********************************************************************/
/************************  Device Definitions  ************************/
/**********************************************************************/

/* All output-device-specific definitions go here.  This section must
be changed when modifying a dvi driver for use on a new device */

#undef POSTSCRIPT
#define  POSTSCRIPT   1			/* conditional compilation flag */

#define PS_SIZELIMIT 150		/* characters larger than this */
					/* many dots high get loaded with */
					/* surrounding save/restore to */
					/* get around PostScript bugs */

#undef PS_XONXOFFBUG
#define PS_XONXOFFBUG 1			/* Allen Hetzel's XON/XOFF bug fix */

#ifndef PS_SHORTLINES
#define PS_SHORTLINES	0		/* run with long output lines */
#endif

#define VERSION_NO	"2.10"		/* DVI driver version number */

#if    PS_XONXOFFBUG
#undef VERSION_NO
#define VERSION_NO	"2.10b"		/* DVI driver version number */
#endif


#define  DEVICE_ID	"PostScript [Apple LaserWriter laser printer]"
				/* this string is printed at runtime */

#define OUTFILE_EXT	"alw"

#define MAXPSLINE 80	/* longest line in PSDEF_FILE */

#define PSDEF_PATH subpath	    /* pathname of system header file */
#define PSDEF_FILE "dvialw.ps"	    /* name of header file */

#define  BYTE_SIZE	  7		/* output file byte size */

#undef STDRES
#define STDRES  1			/* to get standard font resolution */

#undef MAXOPEN
#define MAXOPEN 12			/* limit on number of open files */

#define  MAXLINE	4096		/* maximum input file line size */
					/* it is needed only in special() */

#define  XDPI		300		/* horizontal dots/inch */
#define  XPSIZE		14		/* horizontal paper size in inches */
					/* (allow for landscape paper) */
#define  XSIZE		(((XDPI*XPSIZE+2*HOST_WORD_SIZE-1)/\
				(2*HOST_WORD_SIZE))*(2*HOST_WORD_SIZE))
					/* number of horizontal dots; */
					/* MUST BE multiple of */
					/* 2*HOST_WORD_SIZE */

#define  YDPI		300		/* vertical dots/inch */
#define  YPSIZE		11		/* vertical paper size in inches */
#define  YSIZE		(YDPI*YPSIZE)	/* number of vertical dots */


#if    PS_SHORTLINES

#ifndef PS_MAXWIDTH
#define  PS_MAXWIDTH	72		/* output never exceeds this width */
#endif

INT16    out_width;                     /* current line width */

#endif /* PS_SHORTLINES */

/***********************************************************************
The following variables and macros implement the buffering of PostScript
output  so  that output   size  can  be  reduced by    removing unneeded
separators, and so that a maximum line width can be enforced, except for
code from special{}, which must be output verbatim.

All PostScript output filters through textchr() at the lowest level, and
on the basis of the `deferred' flag setting, it either defers  output of
its  argument character by buffering  it in  textbuf[], or it outputs it
immediately with putc().

Because  PostScript line  breaking  conventions are  context  sensitive,
textchr() maintains static variables to  keep track  of the output state
(comment,   text,  or  string).    It  also    handles all   end-of-line
translations, so \n can be used for end-of-line everywhere else.

All  output  calls outside the  text*() routines are done through macros
OUT_*,  and to  shorten coding,  OUT_NUM provides  a trailing delimiting
space.

textbuf[] is allocated dynamically on the first call  to devinit() so as
not to consume space in the executable program disk file.

textflush()  is  called by textchr()   when  textbuf[]  fills up, and by
eopact() at end-of-page.

***********************************************************************/

#define MAXTEXT		16384		/* size of textbuf[] buffer */

static char* textbuf = (char*)NULL;	/* buffer for setchar() and setstr() */
static char* ptext;			/* next position in textbuf[] */
static BOOLEAN deferred;		/* deferred output status flag */
static UNSIGN16 last_font_number; 	/* set by loadchar() */

#if    PS_SHORTLINES
static BOOLEAN is_comment = FALSE;
static BOOLEAN is_text = FALSE;
static BOOLEAN is_string = FALSE;
#endif /* PS_SHORTLINES */

#define OUT_CHR(c) textchr((char)c)

#define OUT_DEFERRED {deferred = TRUE;}

#define OUT_FLT(fmt,flt) {\
    char s[25];\
    (void)sprintf(s,fmt,flt);\
    OUT_STR(s);}

#define OUT_FONTNAME {\
    char s[11];\
    (void)sprintf(s,"F%d",fontptr->font_number);\
    OUT_STR(s);}

#define OUT_IMMEDIATE {deferred = FALSE;}

#define OUT_NL OUT_CHR('\n')

#define OUT_NUM(n) textnum((long)(n))

#define OUT_STR(s) textstr(s)

#define OUT_XCHR(c) emitchar(c)

#include "main.h"
#include "abortrun.h"
#include "actfact.h"
#include "alldone.h"

/*-->bopact*/
/**********************************************************************/
/******************************* bopact *******************************/
/**********************************************************************/

void
bopact()			/* beginning of page action */
{
    INT16 page_number;	    /* TeX's \count0 parameter			*/

    page_number = (INT16)tex_counter[0];

    if (cur_page_number <= MAXPAGE)
    {
	(void)fflush(plotfp);
	page_loc[cur_page_number] = (long)FTELL(plotfp);
	page_tex[cur_page_number] = (INT32)page_number;
    }
    OUT_IMMEDIATE;
    OUT_STR("%%Page: ");
    OUT_NUM(page_number);
    OUT_NUM(cur_page_number);
    OUT_NL;
    if (ps_vmbug)
        OUT_STR("/SaveImage save def() pop\n");
    OUT_STR("BOP\n");

    rule_height = -1;		/* reset last rule parameters */
    rule_width = -1;
    str_ycp = -1;		/* last string ycp */
}

#include "chargf.h"
#include "charpk.h"
#include "charpxl.h"
#include "clrrow.h"

/*-->cppsfile*/
/**********************************************************************/
/****************************** cppsfile ******************************/
/**********************************************************************/

void
cppsfile()		/* Copy PostScript header file to output */
{			/* discarding comments and collapsing whitespace */
    register int c;	/* input character */
    register int k;	/* index in line[] */
    FILE *psfp;		/* PostScript macro definition file */
    char fname[MAXFNAME];
    char line[MAXPSLINE+1];	/* extra space for NUL */

    /* Try private version of header file first, and if that fails, use */
    /* system version.  Tell the user when a private version is selected. */

    OUT_IMMEDIATE;
    (void)strcpy(fname,PSDEF_FILE);
    psfp = fopen(fname,"r");
    DEBUG_OPEN(psfp,fname,"r");
    if (psfp == (FILE *)NULL)
    {
	(void)strcpy(fname,PSDEF_PATH);
	(void)strcat(fname,PSDEF_FILE);
	psfp = fopen(fname,"r");
	DEBUG_OPEN(psfp,fname,"r");
	if (psfp == (FILE *)NULL)
	{
	    (void)sprintf(message,
		"cppsfile():  Cannot open PostScript definition file [%s]",
		fname);
	    (void)fatal(message);
	}
    }
    else if (!quiet)
    {
	(void)fprintf(stderr,"[Using private PostScript definition file [%s]]",
	    fname);
	NEWLINE(stderr);
    }

    k = 0;
    while ((c = getc(psfp)) != EOF)
    {
	if (c == '%')	/* flush comments to (but not including) end-of-line */
	{
	    while (((c = getc(psfp)) != EOF) && (c != '\n'))
		;	/* flush to end-of-line or end-of-file */
	    c = '\n';	/* to force line output */
	    line[k++] = '\n';
	}
	else if ((c == ' ') || (c == '\t') || (c == '\f'))
	{		/* have whitespace */
	    if (k)			/* then not at beginning of line */
		line[k++] = ' ';	/* so save only one blank */
	    while (((c == ' ') || (c == '\t') || (c == '\f')) && (c != EOF))
		c = getc(psfp);	/* discard following whitespace */
	    ungetc(c,psfp);	/* put back lookahead */
	}
	else if (c != '\r')	/* save all but CR */
	    line[k++] = (char)c;

	if ((c == '\n') || (c == EOF) || (k >= MAXPSLINE))
	{
	    if (c == '\n')		/* discard LF */
		k--;
	    while ((k > 0) && (line[k-1] == ' ')) /* and trailing blanks */
		k--;
	    line[k] = '\0';
	    if (k > 0)	/* non-empty line */
	    {
		OUT_STR(line);
		OUT_NL;
	    }
	    k = 0;
	}
    }

    while ((k > 0) && (line[k-1] == ' '))	/* discard trailing blanks */
	k--;
    line[k] = '\0';
    if (k > 0)	/* non-empty line */
    {
	OUT_STR(line);
	OUT_NL;
    }

    (void)fclose(psfp);
}

#include "dbgopen.h"

/*-->devinit*/
/**********************************************************************/
/****************************** devinit *******************************/
/**********************************************************************/

void
devinit(argc,argv)		/* initialize device */
int argc;
char *argv[];
{
    register INT16 k;		/* loop index */
    char timestring[26];	/* "wkd mon dd hh:mm:ss 19yy\n" */
    long timeval;		/* clock value from time() for ctime()	*/

    if (textbuf == (char*)NULL)
    {				/* allocate textbuf[] only once */
	textbuf = (char*)MALLOC((unsigned)(MAXTEXT+1));
	if (textbuf == (char*)NULL)
            (void)fatal("Cannot allocate memory for textbuf[]");
    }

    ptext = textbuf;

    OUT_IMMEDIATE;		/* need immediate output here */
    OUT_STR("%!\n");		/* magical file header */
    OUT_STR("%%Dimensions: 0 0 612 792\n"); /* 8.5 x 11 inch page size */

    OUT_STR("%%Title: ");
    OUT_STR(argv[0]);		/* start of our command line */
    for (k = 1; k < (INT16)argc; ++k)
    {
	OUT_CHR(' ');
	OUT_STR(argv[k]);
    }
    OUT_NL;			/* end of %%Title line */

    timeval = time((long*)NULL);
    (void)strcpy(timestring,ctime(&timeval));
    timestring[24] = '\0';	/* kill stupid \n from ctime() */

    OUT_STR("%%CreationDate: ");
    OUT_STR(timestring);
    OUT_NL;
    OUT_STR("%%Creator: ");
    OUT_STR((cuserid((char*)NULL) == (char*)NULL) ? "" : cuserid((char*)NULL));
    OUT_STR(" and [TeX82 DVI Translator Version ");
    OUT_STR(VERSION_NO);
    OUT_STR(" for ");
    OUT_STR(DEVICE_ID);
    OUT_NL;

    OUT_STR("%%Pages: (atend)\n");

    if (ps_vmbug)
    {
	OUT_STR("%%BugHistory: Incorporates save/restore and font");
	OUT_STR(" reloading for each page as PS Version 23.0");
	OUT_STR(" \"VM error\" bug workaround\n");
    }

#if    PS_XONXOFFBUG
    OUT_STR("%%BugHistory: Incorporates Allan Hetzel\'s 31-Oct-85");
    OUT_STR(" DARPA LASER-LOVERS PS Version 23.0 X-on/X-off");
    OUT_STR(" bug workaround\n");
#endif /* PS_XONXOFFBUG */

    OUT_STR("%%EndComments\n");
    OUT_STR("%%EndProlog\n");
    font_switched = TRUE;

    (void)cppsfile();	/* copy standard PostScript definitions */

    OUT_STR("TeXdict begin\n");
    OUT_STR("BOJ\n");

    /* TB and TE (Text Begin and Text End) are save/restore sequences;
       they are used when textflush() has to empty its current buffer. */

    OUT_STR("/TB {save} bdf\n");
    OUT_STR("/TE {restore} bdf\n");

#if    0
    OUT_STR("/TE {currentpoint 3 -1 roll restore moveto} bdf\n");
#endif

    /* BB and BE (Big character Begin and End) are save/restore
       sequences that create a temporary font and set a character
       inside a save/restore pair. */
    OUT_STR("/BB {save /F0 NF 1 /F0 SF} bdf\n");
    OUT_STR("/BE {restore} bdf\n");

    font_count = 0;		/* no font numbers are assigned yet */
}

/*-->devterm*/
/**********************************************************************/
/****************************** devterm *******************************/
/**********************************************************************/

void
devterm()			/* terminate device */
{
    register INT16 k;		/* loop index */

    OUT_IMMEDIATE;
    OUT_STR("EOJ\n");
    OUT_STR("%%Trailer\n");
    OUT_STR("%%Pages: ");
    OUT_NUM(page_count);
    OUT_NL;
    OUT_STR("%%PageTable: ");

    for (k = 1; k <= MIN(MAXPAGE,cur_page_number); ++k)
    {
	OUT_NUM(page_tex[k]);
	OUT_NUM(k);
	OUT_NUM(page_loc[k]);
    }

    OUT_NL;
    OUT_CHR('\004');		/* PostScript end-of-job signal */
}

#include "dvifile.h"
#include "dviinit.h"
#include "dviterm.h"

/*-->emitchar*/
/**********************************************************************/
/****************************** emitchar ******************************/
/**********************************************************************/

void
emitchar(c)				/* output string character with */
register BYTE c;			/* escapes and octal as necessary */
{

    /* we do our own octal formatting for speed */
    static char octalchars[] = "01234567";

#define OCTAL(b) octalchars[((b) & 07)]

#if    PS_SHORTLINES
    if (!deferred && ((out_width + 4) >= (PS_MAXWIDTH-1)))
    {
        OUT_CHR('\\');                     /* PostScript ignores */
        OUT_NL;                /* backslash-newline sequences */
    }
#endif /* PS_SHORTLINES */

    if (isprint(c) &&
	/* [textchr() requires the following restrictions] */
        !((c == ' ' ) || (c == '%') || (c == '(') || (c == ')') ||
	   (c == '<') || (c == '>'))) /* character is printable */
    {
	if (c == '\\')		/* double backslashes */
	    OUT_CHR('\\');
	OUT_CHR(c);
    }
    else				/* use octal form */
    {
	OUT_CHR('\\');
	OUT_CHR(OCTAL(c >> 6));
	OUT_CHR(OCTAL(c >> 3));
	OUT_CHR(OCTAL(c));
    }
}

/*-->eopact*/
/**********************************************************************/
/******************************* eopact *******************************/
/**********************************************************************/

void
eopact()			/* end of page action */
{
    register INT32 k;		/* loop index */

    textflush();		/* output accumulated text */
    OUT_IMMEDIATE;
    OUT_NUM(copies);
    OUT_STR("EOP\n");

    if (ps_vmbug)
    {
	OUT_STR("SaveImage restore() pop\n");
	font_switched = TRUE;
	fontptr = hfontptr;
	while (fontptr != (struct font_entry *)NULL)
	{
	    for (k = 0; k < NPXLCHARS; ++k)
		fontptr->ch[k].isloaded = FALSE;
	    fontptr = fontptr->next;
	}
    }

    for (k = (INT32)copies; k; --k)
        OUTC('\f');		/* FF's for simple page accounting */
}

#include "f20open.h"
#include "fatal.h"

/*-->fillrect*/
/**********************************************************************/
/****************************** fillrect ******************************/
/**********************************************************************/

void
fillrect(x,y,width,height)
COORDINATE x,y,width,height;		/* lower left corner, size */

/***********************************************************************
With the  page origin  (0,0) at  the lower-left  corner, draw  a  filled
rectangle at (x,y).

For most  TeX  uses, rules  are  uncommon, and  little  optimization  is
possible.  However, for the LaTeX Bezier option, curves are simulated by
many small rules (typically  2 x 2)  separated by positioning  commands.
By remembering  the size  of the  last rule  set, we  can test  for  the
occurrence of repeated rules of the same size, and reduce the output  by
omitting the rule  sizes.  The  last rule  parameters are  reset by  the
begin-page action in prtpage(), so they do not carry across pages.

It is not possible to use relative, instead of absolute, moves in  these
sequences, without stacking rules for the whole page, because each  rule
is separated in  the DVI file  by push, pop,  and positioning  commands,
making  for  an  uncertain  correspondence  between  internal  (xcp,ycp)
pixel page coordinates and external device coordinates.
***********************************************************************/

{
    str_ycp = -1;		/* invalidate string y coordinate */

    OUT_IMMEDIATE;		/* because Q uses width and height saved */
    OUT_NUM(x);			/* by B, we cannot use deferred output; */
    OUT_NUM(y);			/* otherwise a TB B TE TB Q TE would fail */
    if ((height != rule_height) || (width != rule_width))
    {
	OUT_CHR('M');
	OUT_CHR(' ');
	OUT_NUM(width);
	OUT_NUM(height);
	OUT_CHR('B');
	OUT_CHR(' ');
	rule_width = width;	/* save current rule parameters */
	rule_height = height;
    }
    else
    {
	OUT_CHR('Q');
	OUT_CHR(' ');
    }
}

#include "findpost.h"
#include "fixpos.h"
#include "fontfile.h"
#include "fontsub.h"
#include "getbytes.h"
#include "getfntdf.h"
#include "getpgtab.h"
#include "inch.h"
#include "initglob.h"

/*-->loadchar*/
/**********************************************************************/
/****************************** loadchar ******************************/
/**********************************************************************/

void
loadchar(c)
register BYTE c;
{
    void (*charyy)();		/* subterfuge to get around PCC-20 bug */
    register struct char_entry *tcharptr; /* temporary char_entry pointer */

    if ((c < FIRSTPXLCHAR) || (LASTPXLCHAR < c)) /* check character range */
	return;

    tcharptr = &(fontptr->ch[c]);

    if (!VISIBLE(tcharptr))
	return;				/* do nothing for invisible fonts */

    if (fontptr != pfontptr)
	openfont(fontptr->n);

    if (fontfp == (FILE *)NULL)		/* do nothing if no font file */
	return;

    tcharptr->isloaded = TRUE;

    clearerr(plotfp);		/* VMS sets the error flag unexpectedly */

    OUT_IMMEDIATE;
    OUT_STR("[<");

    /* Bug workaround: PCC-20 otherwise jumps to charxx instead of *charxx */
    charyy = fontptr->charxx;
    (void)(*charyy)(c,outrow);		/* output rasters */

    OUT_CHR('>');
    OUT_NUM(tcharptr->xoffp);
    OUT_NUM((tcharptr->yoffp)+1);
    OUT_NUM(tcharptr->wp);
    OUT_NUM(tcharptr->hp);
    OUT_FLT("%.7f]",(float)(tcharptr->tfmw)*conv)
    OUT_NUM(c);
    OUT_CHR('D');

#if    PS_SHORTLINES
    OUT_CHR(' ');
#else
    OUT_NL;
#endif /* PS_SHORTLINES */

    /* Because of the deferred output feature, we now need to remember
       the last font used, so on the next call, we know whether to
       issue another OUT_FONTNAME or not.  font_switched is no longer
       sufficient.
    */
    last_font_number = fontptr->font_number;

    if (DISKFULL(plotfp))
	(void)fatal("loadchar():  Output error -- disk storage probably full");
}


#include "movedown.h"
#include "moveover.h"
#include "moveto.h"

/*-->newfont*/
/**********************************************************************/
/****************************** newfont *******************************/
/**********************************************************************/

void
newfont()
{
    register UNSIGN16 the_char;	/* loop index */
    char s[MAXSTR];

    for (the_char = FIRSTPXLCHAR; the_char <= LASTPXLCHAR; the_char++)
	fontptr->ch[the_char].isloaded = FALSE;

    font_count++;
    fontptr->font_number = font_count;

    OUT_IMMEDIATE;

    (void)sprintf(s,"/%s NF %% %s\n",putfontname(fontptr),
		  fontptr->name);
    OUT_STR(s);	/* declare new font for PostScript */

    (void)sprintf(s,"/F%d {1 /%s SF} bdf\n",
		  fontptr->font_number,putfontname(fontptr));
    OUT_STR(s);	/* compact shorthand for font changes */
}

#include "nosignex.h"
#include "openfont.h"
#include "option.h"

/*-->outrow*/
/**********************************************************************/
/******************************* outrow *******************************/
/**********************************************************************/

void
outrow(c,yoff)	/* output img_row[] to device */
BYTE c;		/* current character value */
UNSIGN16 yoff;	/* offset from top row (0,1,...,hp-1) (UNUSED here) */
{
    UNSIGN16 bytes_per_row;	/* number of raster bytes to copy */
    register UNSIGN16 k;	/* loop index */
    register UNSIGN32 *p;	/* pointer into img_row[] */
    struct char_entry *tcharptr;/* temporary char_entry pointer */
    register BYTE the_byte;	/* unpacked raster byte */

    /* we do our own hexadecimal formatting for speed */
    static char hexchars[] = "0123456789ABCDEF";

#define NIBBLE(b) hexchars[(b) & 0x0f]

    tcharptr = &(fontptr->ch[c]); /* assume check for valid c has been done */
    bytes_per_row = (UNSIGN16)((tcharptr->wp) + 7) >> 3; /* wp div 8 */
    p = img_row;		/* we step pointer p along img_row[] */

    for (k = bytes_per_row; k > 0; ++p)
    {

	the_byte = (BYTE)((*p) >> 24);
	OUT_CHR(NIBBLE(the_byte>>4));
	OUT_CHR(NIBBLE(the_byte));
	if ((--k) <= 0)
	    break;

	the_byte = (BYTE)((*p) >> 16);
	OUT_CHR(NIBBLE(the_byte>>4));
	OUT_CHR(NIBBLE(the_byte));
	if ((--k) <= 0)
	    break;

	the_byte = (BYTE)((*p) >> 8);
	OUT_CHR(NIBBLE(the_byte>>4));
	OUT_CHR(NIBBLE(the_byte));
	if ((--k) <= 0)
	    break;

	the_byte = (BYTE)(*p);
	OUT_CHR(NIBBLE(the_byte>>4));
	OUT_CHR(NIBBLE(the_byte));
	if ((--k) <= 0)
	    break;
    }

#if    PS_SHORTLINES
    /* line breaking handled by textchr() */
#else
    k = 40 / MAX(1,bytes_per_row); /* rows per 80-character line */
    /* break after last row, or whenever an 80-character line has
       been filled up */
    if (((yoff+1) == tcharptr->hp) || (((yoff+1) % k) == 0))
        OUT_NL;
#endif /* PS_SHORTLINES */

}

#include "prtpage.h"

/*-->putfontname*/
/**********************************************************************/
/**************************** putfontname *****************************/
/**********************************************************************/

char*
putfontname(font_ptr)
register struct font_entry *font_ptr;
/* Output TeX font name and magnification in form "cmr10_1500" as a
unique PostScript font identifier */
{
    register char* nameptr;
    register char* p;
    static char namebuf[MAXSTR];

    nameptr = &(font_ptr->n[0]);
    p = namebuf;

    while (*nameptr)	/* make name with non-alphanumerics  */
    {			/* changed to underscore for PostScript */
	*p++ = isalnum(*nameptr) ? *nameptr : '_';
	nameptr++;
    }

    (void)sprintf(p,"_%d",(int)font_ptr->magnification);

    return (&namebuf[0]);
}

#include "readfont.h"
#include "readgf.h"
#include "readpk.h"
#include "readpost.h"
#include "readpxl.h"
#include "reldfont.h"
#include "rulepxl.h"

/*-->setchar*/
/**********************************************************************/
/****************************** setchar *******************************/
/**********************************************************************/

void
setchar(c, update_h)
register BYTE c;
register BOOLEAN update_h;
{
    register struct char_entry *tcharptr;  /* temporary char_entry pointer */

    /* BIGCHAR() and ONPAGE() are used here and in setstr() */

#define BIGCHAR(t) ((t->wp > (COORDINATE)size_limit) ||\
    (t->hp > (COORDINATE)size_limit))

#define ONPAGE(t) (((hh - t->xoffp + t->pxlw) <= XSIZE) \
    && (hh >= 0)\
    && (vv <= YSIZE)\
    && (vv >= 0))

    if (DBGOPT(DBG_SET_TEXT))
    {
	(void)fprintf(stderr,"setchar('");
	if (isprint(c))
	    (void)putc(c,stderr);
	else
	    (void)fprintf(stderr,"\\%03o",(int)c);
	(void)fprintf(stderr,"'<%d>) (hh,vv) = (%ld,%ld) font name <%s>",
	    (int)c, (long)hh, (long)vv, fontptr->n);
	NEWLINE(stderr);
    }

    tcharptr = &(fontptr->ch[c]);

    moveto(hh,YSIZE-vv);
    if (ONPAGE(tcharptr))
    {				/* character fits entirely on page */
	if (VISIBLE(tcharptr))
	{
	    if (BIGCHAR(tcharptr))
	    {
		/* always need absolute coordinates (save/restore causes */
		/* loss of updated position) */
		OUT_IMMEDIATE;

#if    PS_SHORTLINES
		/* line breaking handled by textchar() */
#else
		OUT_NL;
#endif /* PS_SHORTLINES */

		OUT_STR("BB");

#if    PS_SHORTLINES
		OUT_CHR(' ');
#else
		OUT_NL;
#endif /* PS_SHORTLINES */

		loadchar(c);
		OUT_CHR('(');
		OUT_XCHR(c);
		OUT_CHR(')');
		OUT_NUM(xcp);
		OUT_NUM(ycp);
		OUT_CHR('S');

#if    PS_SHORTLINES
		OUT_CHR(' ');
#else
		OUT_NL;
#endif /* PS_SHORTLINES */

		OUT_STR("BE");

#if    PS_SHORTLINES
		/* line breaking handled by textchar() */
#else
		OUT_NL;
#endif /* PS_SHORTLINES */

		tcharptr->isloaded = FALSE;	/* 'unload' character */
	    }
	    else
	    {
		if (!tcharptr->isloaded)
		{
		    if ((font_switched) ||
			(fontptr->font_number != last_font_number))
		    {
			OUT_IMMEDIATE;
			OUT_FONTNAME;
		    }
		    loadchar(c);
		}
		OUT_DEFERRED;
		if (font_switched)
		{
		    OUT_FONTNAME;
		    font_switched = FALSE;
		}
		OUT_CHR('(');
		OUT_XCHR(c);
		OUT_CHR(')');
		if (ycp != str_ycp)
		{
		    OUT_NUM(xcp);
		    OUT_NUM(ycp);
		    OUT_STR("S\n");
		    str_ycp = ycp;
		}
		else
		{
		    OUT_NUM(xcp);
		    OUT_STR("T\n");
		}
	    }

	}
    }
    else if (DBGOPT(DBG_OFF_PAGE) && !quiet)
    {				/* character is off page -- discard it */
	(void)fprintf(stderr,
	    "setchar(): Char %c [10#%3d 8#%03o 16#%02x] off page.",
	    isprint(c) ? c : '?',c,c,c);
	NEWLINE(stderr);
    }

    if (update_h)
    {
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;
	hh = fixpos(hh-lmargin,h,conv) + lmargin;
    }
}

#include "setfntnm.h"
#include "setrule.h"

/*-->setstr*/
/**********************************************************************/
/******************************* setstr *******************************/
/**********************************************************************/

void
setstr(c)
register BYTE c;
{
    register struct char_entry *tcharptr;  /* temporary char_entry pointer */
    register BOOLEAN inside;
    COORDINATE xcp_start,ycp_start;	/* starting coordinates of string */
    INT32 h0,v0;		/* (h,v) at entry */
    COORDINATE hh0,vv0;		/* (hh,vv) at entry */
    register UNSIGN16 k;	/* loop index */
    UNSIGN16 maxstr;		/* loop limit */
    UNSIGN16 nstr;		/* number of characters in str[] */
    BOOLEAN save_font_switched;	/* for saving font_switched value */
    BYTE str[MAXSTR+1];		/* string accumulator */
    BOOLEAN truncated;		/* off-page string truncation flag */

    /*******************************************************************
    Set a sequence of characters in SETC_000 .. SETC_127 with a  minimal
    number of PostScript print-string commands.  These sequences tend to
    occur in  long clumps  in  a DVI  file,  and setting  them  together
    whenever possible  substantially decreases  the PostScript  overhead
    and the size of the output file.  A sequence can be set as a  single
    string if

	* TeX and PostScript coordinates of each character agree (always
	  true since  PostScript  has  high-precision  character  widths
	  available;  for  non-PostScript  devices,  violation  of  this
	  requirement can be detected if fixpos() changes hh, or if  ycp
	  != ycp_start), AND

	* each character is in the same font (this will always be true
	  in a sequence from a DVI file), AND

	* each character fits within the page boundaries, AND

	* each character definition is already loaded, AND

	* each character is from a visible font, AND

	* each  character bitmap  extent is smaller  than the size_limit
	  (which is  used to  enable discarding  large characters  after
	  each use in order  to conserve virtual  memory storage on  the
	  output device).

    Whenever any of these conditions  does not hold, any string  already
    output is terminated, and a new one begun.  In order to avoid output
    of empty  string requests,  a flag  "inside" is  set when  a  string
    opener "(" is output, and unset when the string terminator ") x y S"
    or ") P" is output.

    Two output optimizations are implemented here.  First, up to  MAXSTR
    (in practice more  than enough) characters  are collected in  str[],
    and any  that  require downloading  are  handled.  Then  the  entire
    string is set at once, subject to the above limitations.  Second, by
    recording the vertical page coordinate, ycp, in the global  variable
    str_ycp  (reset  in  prtpage()  at  begin-page  processing),  it  is
    possible to avoid  outputting y coordinates  unnecessarily, since  a
    single line of  text will  generally result  in many  calls to  this
    function.
    *******************************************************************/

#define BEGINSTRING {inside = TRUE;\
    xcp_start = xcp;\
    ycp_start = ycp;\
    OUT_CHR('(');}

#define ENDSTRING {inside = FALSE;\
    OUT_CHR(')');\
    if (ycp == str_ycp)\
    {\
        OUT_NUM(xcp_start);\
        OUT_STR("T\n");\
    }\
    else\
    {\
        OUT_NUM(xcp_start);\
        OUT_NUM(ycp_start);\
        OUT_STR("S\n");\
	str_ycp = ycp;\
    }}

#define OFF_PAGE (-32767)    /* off-page coordinate value */

    inside = FALSE;
    truncated = FALSE;
    OUT_IMMEDIATE;

    hh0 = hh;
    vv0 = vv;
    h0 = h;
    v0 = v;
    save_font_switched = font_switched;
    nstr = 0;

#if    PS_SHORTLINES
    /* With deferred output, textflush() cannot do any output parsing,
       so to avoid long lines, we allow at most a string
       "(...)nnnnn nnnnn S" on one line. */
    maxstr = MIN(PS_MAXWIDTH-15,MAXSTR);
#else
    maxstr = MAXSTR;
#endif /* PS_SHORTLINES */

    while ((SETC_000 <= c) && (c <= SETC_127) && (nstr < maxstr))
    {   /* collect character sequence and download needed fonts */
	tcharptr = &(fontptr->ch[c]);

	moveto(hh,YSIZE-vv);

	if (ONPAGE(tcharptr) && VISIBLE(tcharptr))
	{		/* character fits entirely on page and is visible */
	    if ((!tcharptr->isloaded) && (!BIGCHAR(tcharptr)))
	    {
		if (font_switched ||
		    (fontptr->font_number != last_font_number))
		{
		    OUT_FONTNAME;
		    font_switched = FALSE;
		}
		loadchar(c);
	    }
	}
	/* update horizontal positions in TFM and pixel units */
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;
	hh = fixpos(hh-lmargin,h,conv) + lmargin;

	str[nstr++] = c;		/* save string character */

	c = (BYTE)nosignex(dvifp,(BYTE)1);
    }

    /* put back character which terminated the loop */
    (void)UNGETC((int)(c),dvifp);

    hh = hh0;			/* restore coordinates at entry */
    vv = vv0;
    h = h0;
    v = v0;

    if (DBGOPT(DBG_SET_TEXT))
    {
	(void)fprintf(stderr,"setstr(\"");
	for (k = 0; k < nstr; ++k)
	{
	    c = str[k];
	    if (isprint(c))
	        (void)putc(c,stderr);
	    else
	        (void)fprintf(stderr,"\\%03o",(int)c);
	}
	(void)fprintf(stderr,"\") (hh,vv) = (%ld,%ld) font name <%s>",
	    (long)hh, (long)vv, fontptr->n);
	NEWLINE(stderr);
    }

    OUT_DEFERRED;
    font_switched = save_font_switched;
    if (font_switched)
    {
	OUT_FONTNAME;
	OUT_NL;			/* string might not be visible */
	font_switched = FALSE;
    }

    for (k = 0; k < nstr; ++k)
    {   /* now set the collected characters */
	c = str[k];
	tcharptr = &(fontptr->ch[c]);
	moveto(hh,YSIZE-vv);

	if (ONPAGE(tcharptr) && VISIBLE(tcharptr))
	{		/* character fits entirely on page and is visible */
	    if (tcharptr->isloaded) /* character already downloaded */
	    {
		if (!inside)
		    BEGINSTRING;
		OUT_XCHR(c);
	    }
	    else /* must be big character (others are already downloaded) */
	    {
		if (inside)
		    ENDSTRING;	    /* finish any open string */
		if (BIGCHAR(tcharptr))
		{	/* Large character to be discarded. */
			/* Inside save/restore, updated current point */
			/* is lost, so we must force absolute positioning */
	    		/* by resetting str_ycp before and after setting */
			/* the character. */
		    str_ycp = OFF_PAGE;
		    OUT_IMMEDIATE;

#if    PS_SHORTLINES
		/* line breaking handled by textchar() */
#else
		    OUT_NL;
#endif /* PS_SHORTLINES */

                    OUT_STR("BB");

#if    PS_SHORTLINES
		    OUT_CHR(' ');
#else
		    OUT_NL;
#endif /* PS_SHORTLINES */

		    loadchar(c);
		    BEGINSTRING;
		    OUT_XCHR(c);
		    ENDSTRING;
		    OUT_STR("BE");

#if    PS_SHORTLINES
		    OUT_CHR(' ');
#else
		    OUT_NL;
#endif /* PS_SHORTLINES */

		    OUT_CHR(' ');
		    OUT_DEFERRED;
		    tcharptr->isloaded = FALSE;	/* 'unload' character */
		    str_ycp = OFF_PAGE;
		}
	    }
	}
	else		/* character does not fit on page -- output */
	{		/* current string and discard the character */
	    truncated = TRUE;
	    if (inside)
		ENDSTRING;
	}
	/* update horizontal positions in TFM and pixel units */
	h += (INT32)tcharptr->tfmw;
	hh += (COORDINATE)tcharptr->pxlw;
	hh = fixpos(hh-lmargin,h,conv) + lmargin;
    }
    if (truncated && DBGOPT(DBG_OFF_PAGE) && !quiet)
    {
	(void)fprintf(stderr,"setstr(): Text [");
	for (k = 0; k < nstr; ++k)
	    (void)fprintf(stderr,isprint(str[k]) ? "%c" : "\\%03o",str[k]);
	(void)fprintf(stderr,"] truncated at page boundaries.");
	NEWLINE(stderr);
    }

    if (inside)		/* finish last string */
	ENDSTRING;
}

#include "signex.h"
#include "skgfspec.h"
#include "skipfont.h"
#include "skpkspec.h"

/*-->special*/
/**********************************************************************/
/****************************** special *******************************/
/**********************************************************************/

void
special(s)			/* process TeX \special{} string in s[] */
register char *s;
{
    char line[MAXLINE+2];
    FILE *specfile;
    BOOLEAN abspos;
    INT16 k;
    int llx,lly,urx,ury;		/* must be int for sscanf() */
    struct stat statbuf;		/* so fstat() can get file size */

/***********************************************************************
The TeX \special{} command is expected to look like

    \special{overlay filename}		% absolute positioning
    or
    \special{include filename}		% relative positioning
    or
    \special{insert filename}		% relative positioning

In the first  case, the PostScript  file to be  included will be  mapped
onto the page at precisely the  coordinates it specifies.  In the  other
two cases, the upper-left corner of  the bounding box will be placed  at
the current point.  The PostScript file must then contain (usually  near
the start) a comment of the form

%%BoundingBox: llx lly urx ury

specifying the bounding  box lower-left and  upper-right coordinates  in
standard PostScript units (1/72 inch).  Alternatively, if the comment

%%BoundingBox: (atend)

is found in  the file,  the last  1000 characters  of the  file will  be
searched to find a comment of the form:

%%BoundingBox: llx lly urx ury

If the  PostScript file  cannot  be opened,  or the  \special{}  command
string cannot be recognized, or  for relative positioning, the  bounding
box cannot be determined, a warning  message is issued and the  \special
command is ignored.

Otherwise, the section of the PostScript file between the comment lines

%begin(plot)
%end(plot)

is copied to the output file surrounded by

save
300 72 div 300 72 div scale % revert to standard 1/72 inch units
(xcp(in 1/72in)-llx) (ycp(in 1/72in)-ury) translate  % if relative positioning
...PostScript file contents...
restore

***********************************************************************/

    clearerr(plotfp);		/* VMS sets the error flag unexpectedly */

    if (strncmp("overlay ",s,8) == 0)
    {
	k = 8;
	abspos = TRUE;
    }
    else if (strncmp("include ",s,8) == 0)
    {
	k = 8;
	abspos = FALSE;
    }
    else if (strncmp("insert ",s,7) == 0)
    {
	k = 7;
	abspos = FALSE;
    }
    else
    {
	NEWLINE(stderr);
	(void)fprintf(stderr,
	    "[TeX \\special{%s} command not understood]",s);
	NEWLINE(stderr);
	(void)fprintf(stderr,"Expected one of:");
	NEWLINE(stderr);
	(void)fprintf(stderr,
	    "\t\\special{overlay filename} [relative to page origin in ");
	(void)fprintf(stderr,
	    "lower-left corner]");
	NEWLINE(stderr);
	(void)fprintf(stderr,"or");
	NEWLINE(stderr);
	(void)fprintf(stderr,
	    "\t\\special{insert filename} [relative to current position]");
	NEWLINE(stderr);
	(void)fprintf(stderr,"or");
	NEWLINE(stderr);
	(void)fprintf(stderr,
	    "\t\\special{include filename} [relative to current position]");
	NEWLINE(stderr);
	(void)fprintf(stderr,"\\special{%s} request ignored",s);
	NEWLINE(stderr);
	(void)fprintf(stderr,"Current TeX page counters: [%s]",tctos());
	NEWLINE(stderr);
	return;
    }
    specfile = fopen(&s[k],"r");
    DEBUG_OPEN(specfile,&s[k],"r");
    if (specfile == (FILE *)NULL)
    {
	NEWLINE(stderr);
	(void)fprintf(stderr,
	    "Open failure on \\special file [%s]",&s[k]);
	NEWLINE(stderr);
	(void)fprintf(stderr,"\\special{%s} request ignored",s);
	NEWLINE(stderr);
	(void)fprintf(stderr,"Current TeX page counters: [%s]",tctos());
	NEWLINE(stderr);
	return;
    }
    if (abspos)
    {
	llx = lly = 0;
	urx = (72*17)/2;
	ury = 72*11;
    }
    else
    {
	llx = lly = urx = ury = -1;
	while (fgets(line,MAXLINE,specfile) != (char *)NULL)
	{
	    llx = lly = urx = ury = -1;
	    if (strncmp(line,"%%BoundingBox: (atend)",22) == 0)
	    {	/* reposition to up to 1000 chars from end of file */
		(void)fstat(fileno(specfile),&statbuf);
		k = MIN(1000,(INT16)((long)statbuf.st_size-ftell(specfile)));
		if (k > 0)
		    (void)fseek(specfile,(long)(-k),2);
	    }
	    else if (strncmp(line,"%%BoundingBox:",14) == 0)
	    {
		k = (INT16)sscanf(line,
		    "%%%%BoundingBox: %d %d %d %d",&llx,&lly,&urx,&ury);
		if (k == 4)
		    break;		/* got %%BoundingBox */
	    }
	}
    }
    if (ury == (-1))
    {
	NEWLINE(stderr);
	(void)fprintf(stderr,
	    "Could not find PostScript %%%%BoundingBox command ");
	(void)fprintf(stderr,"needed to position plot on page");
	NEWLINE(stderr);
	(void)fprintf(stderr,"\\special{%s} request ignored",s);
	NEWLINE(stderr);
	(void)fprintf(stderr,"Current TeX page counters: [%s]",tctos());
	NEWLINE(stderr);
	return;
    }
    NEWLINE(stderr);
    (void)fprintf(stderr,"\t[\\special{%s}] ",s);
    NEWLINE(stderr);

    OUT_IMMEDIATE;
    OUT_STR("save\n");		/* save current state */

    OUT_STR("300 72 div 300 72 div scale\n");	/* revert to standard units */

    if (!abspos)			/* move origin for include/insert */
    {
	moveto(hh,YSIZE-vv);		/* update current point */
	OUT_NUM((72*xcp)/300-llx);
	OUT_NUM((72*ycp)/300-ury);
	OUT_STR("translate\n");
    }
    (void)REWIND(specfile);		/* rewind file */

    while (fgets(line,MAXLINE,specfile) != (char *)NULL)
    {			/* search for %begin(plot) ... %end(plot) */
	if (strncmp(line,"%begin(plot)",12) == 0)
	{				/* copy until %end(plot) */
	    while (fgets(line,MAXLINE,specfile) != (char *)NULL)
	    {
		if (strncmp(line,"%end(plot)",10) == 0)
		    break;
		OUTS(line);
	    }
	    break;
	}
    }
    OUT_STR("restore\n");
    (void)fclose(specfile);
    if (!quiet)
    {
	(void)fprintf(stderr," [OK]");
	NEWLINE(stderr);
    }
    if (DISKFULL(plotfp))
	(void)fatal("special():  Output error -- disk storage probably full");
}

#include "strchr.h"
#include "strcm2.h"
#include "strid2.h"
#include "strrchr.h"
#include "tctos.h"

/*-->textchr*/
/**********************************************************************/
/****************************** textchr *******************************/
/**********************************************************************/

void
textchr(c)				/* output character c */
register char c;
{
    char* psave;

    if (deferred)			/* deferred output */
    {
	*ptext++ = c;
	if (ptext >= (textbuf + MAXTEXT))
	{
	    /* textbuf[] is full.  Backup to the last newline, output
	       textbuf[] to that point, move the remainder to the
	       start of textbf[], and resume character collection. */
	    *ptext = '\0';	/* mark the end */
	    psave = strrchr(textbuf,'\n'); /* find last line break */
	    ptext = psave;	/* shorten buffer */
	    *ptext = '\0';	/* mark the new end */
	    textflush();	/* output the buffer */
	    ++psave;		/* where we want to pick up remainder */
	    OUT_FONTNAME;	/* make sure current font starts buffer */
	    OUT_NL;
	    OUT_NUM(xcp);	/* make sure current point is set too */
	    OUT_NUM(ycp);
	    OUT_STR("M\n");

	    /* Since the above initial sequence requires at most 25
	       characters, we should be safe here, since psave should
	       be pointing near the end of textbuf[].  Nevertheless, we
	       should watch for this time bomb, because someday someone
	       might make MAXTEXT ridiculously small.  */
	    if (ptext > psave)
	        fatal("textchr(): internal error--textbuf[] too small");

	    (void)strcpy(ptext,psave); /* move remainder to start */
	    ptext = strchr(textbuf,'\0'); /* find the new end again */
	}
    }
    else				/* immediate output */
    {

#if    PS_SHORTLINES
        /***************************************************************
        Unfortunately, PostScript does allows only text bracketed  by ()
        to be broken across lines   by a   \newline sequence.  We   must
        therefore continue comments manually and break <...>  strings by
        a newline.  emitchar()  ensures that %  in () and non-bracketing
        () <> are encoded in octal, so as not to confuse us here.
        ***************************************************************/

	switch (c)		/* set flags for exiting environments */
	{
	case '\n':
	    is_comment = FALSE;
	    break;

	    /* '<' editor balance */
	case '>':
	    if (!is_comment)
	        is_string = FALSE;
	    break;

	    /* '(' editor balance */
	case ')':
	    if (!is_comment)
	        is_text = FALSE;
	    break;
	}

	if (out_width < (PS_MAXWIDTH-2)) /* 3 or more slots left on line */
	{
	    switch (c)
	    {
	    case '\n':
		NEWLINE(plotfp);
	        out_width = 0;
		break;

	    default:
		OUTC(c);
	        out_width++;
		break;
	    }
	}
	else if (out_width == (PS_MAXWIDTH-2)) /* 2 slots left on line */
	{
	    if (is_comment)
	    {
		OUTC(c);
		out_width++;
	    }
	    else		/* not comment */
	    {
		switch (c)
		{
		case '(':	/* special \newline inside (...text...) */
		    OUTC(c);
		    OUTC('\\');
		    NEWLINE(plotfp);
		    out_width = 0;
		    break;

		case '\n':		/* ordinary newline */
		    NEWLINE(plotfp);
		    out_width = 0;
		    break;

		default:		/* else just output the character */
		    OUTC(c);
		    out_width++;
		    break;
		}		/* end switch */
	    }			/* end if (is_comment) */
	}
	else			/* at most 1 slot left */
	{
	    if (is_comment)
	    {			/* make special continued comment */
		OUTC('-');
		NEWLINE(plotfp);
		OUTS("%-");
		OUTC(c);
		out_width = 3;
	    }
	    else		/* not comment */
	    {
		switch (c)
		{
		case '%':	/* start newline for comment or (...text...) */
		case '(':
		    NEWLINE(plotfp);
		    OUTC(c);
		    out_width = 1;
		    break;

		case ')':		/* follow (...text...) with newline */
		case '<':		/* newline inside <...text...> okay */
		case '>':		/* follow <...text...> with newline */
		    OUTC(c);
		    NEWLINE(plotfp);
		    out_width = 0;
		    break;

		case ' ':		/* change trailing space to newline */
		case '\n':		/* newline */
		    NEWLINE(plotfp);
		    out_width = 0;
		    break;

		default:
		    if (is_text) /* \newline continuation in (...text...) */
		    {
			OUTC('\\');
			NEWLINE(plotfp);
			OUTC(c);
			out_width = 1;
		    }
		    else if (is_string)	/* newline inside <...text...> okay */
		    {
			OUTC(c);
			NEWLINE(plotfp);
			out_width = 0;
		    }
		    else	/* cannot break at this point, sigh... */
		    {
			OUTC(c);
			out_width++;
		    }
		    break;
		}		/* end switch */
	    }			/* end if (is_comment) */
	}

	switch (c)		/* set flags for entering environments */
	{
	case '%':
	    is_comment = TRUE;
	    break;

	case '<':
	    if (!is_comment)
	        is_string = TRUE;
	    break;

	case '(':
	    if (!is_comment)
	        is_text = TRUE;
	    break;
	}
#else /* NOT PS_SHORTLINES */
	OUTC(c);
#endif /* PS_SHORTLINES */

    }				/* endif deferred/immediate */
}

/*-->textflush*/
/**********************************************************************/
/***************************** textflush ******************************/
/**********************************************************************/

void
textflush()				/* flush current textbuf[] to output */
{

#if    PS_SHORTLINES
    register char* pbreak;		/* point to character before */
					/* which we can insert a line break */
    char linebuf[PS_MAXWIDTH+1];	/* buffer for output line */
    register INT16 k;			/* index in linebuf[] */
    register INT16 kbreak;		/* break index in linebuf[] */
					/* corresponding to pbreak */
#endif /* PS_SHORTLINES */

    /* Wrap the current textbuf[] contents with save/restore, but
       preserve the current point and font. */
    if (ptext > textbuf)	/* make sure textbuf[] is not empty */
    {

#if    PS_SHORTLINES
	if (out_width > 0)	/* want TB ... TE on separate lines */
	    NEWLINE(plotfp);
#endif /* PS_SHORTLINES */

	OUTS("TB");
	NEWLINE(plotfp);

	*ptext = '\0';		/* terminate current textbuf[] */

#if    PS_SHORTLINES
	ptext = textbuf;
	pbreak = ptext;
	k = 0;
	kbreak = 0;
	while (*ptext)
	{
	    if (k >= PS_MAXWIDTH)
	    {
		if (kbreak > 0)
		{
		    k = kbreak;
		    ptext = pbreak;
		}
		while ((k > 0) && (linebuf[k-1] == ' '))
		    k--;		/* trim trailing space */
		linebuf[k] = '\0';
		OUTS(linebuf);
		k = 0;
		if ((kbreak > 0) || (*(ptext+1) == '\0'))
		    NEWLINE(plotfp);
	    }
	    if ((*ptext == '\n') && (*(ptext+1) == '('))
	        ptext++;		/* squeeze out useless newline */
	    if (*ptext == '\n')
	        *ptext = ' ';		/* change newline to blank */
	    if ((k == 0) && (*ptext == ' '))
		/* NO-OP */;			/* don't save leading space */
	    else
	        linebuf[k++] = *ptext;
	    if ((*ptext == ' ') || (*ptext == '('))
	    {
		kbreak = k - 1;
	        pbreak = ptext;
	    }
	    else if (*ptext == ')')
	    {
		kbreak = k;
	        pbreak = ptext + 1;
	    }
	    ptext++;
	}
	if (k > 0)
	{
	    while ((k > 0) && (linebuf[k-1] == ' '))
	        k--;		/* trim trailing space */
	    linebuf[k] = '\0';
	    OUTS(linebuf);
	    NEWLINE(plotfp);
	}
	out_width = 0;
#else /* NOT PS_SHORTLINES */
	OUTS(textbuf);
	NEWLINE(plotfp);
#endif /* PS_SHORTLINES */

	OUTS("TE");
	NEWLINE(plotfp);

	ptext = textbuf;
    }
}

/*-->textnum*/
/**********************************************************************/
/****************************** textnum *******************************/
/**********************************************************************/

void
textnum(n)			/* output number with one trailing space */
long n;				/* the number */
{
    char digits[11];

    (void)sprintf(digits,"%ld",n);
    textstr(digits);

#if    PS_SHORTLINES
    if (!deferred && !is_comment && (out_width == 0))
	/* NO-OP */;		/* omit space at beginning of line */
    else
	textchr(' ');

#else /* NOT PS_SHORTLINES */
    textchr(' ');
#endif /* PS_SHORTLINES */

}



/*-->textstr*/
/**********************************************************************/
/****************************** textstr *******************************/
/**********************************************************************/

void
textstr(s)				/* output string s[] */
register char* s;
{

#if    PS_SHORTLINES
    if (!deferred && !is_comment && ((out_width + strlen(s)) > PS_MAXWIDTH))
        OUT_NL;
#endif /* PS_SHORTLINES */

    for (; *s; ++s)
        textchr(*s);
}

#include "usage.h"
#include "warning.h"
