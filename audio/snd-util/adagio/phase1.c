/* phase1.c -- Phase 1 of adagio compilation...	 */
/*
 * this module parses adagio programs and builds a linked list structure
 * consisting of notes and control changes in time order.
 */

/*****************************************************************************
*	    Change Log
*  Date	    | Change
*-----------+-----------------------------------------------------------------
* 31-Dec-85 | Created changelog
* 31-Dec-85 | Add c:\ to include directives
* 31-Dec-85 | Added standard command scanner, metronome variable, need to add
*	    | cmdline_help procedure
* 31-Dec-85 | Call intr_init
* 31-Dec-85 | Set musictrace from command line via -trace
* 31-Dec-85 | Added -poll
*  1-Jan-86 | Put error messages out to stderr
*  1-Jan-86 | Set IsAT.	 Can be later overridden by -at and -xt switches,
*	    | currently only used for diagnostics (may be needed for
*	    | compatibles, who knows?  In which case remove the tests which
*	    | confirm the type of processor)
*  1-Jan-86 | <rgd/jmn> Removed dur-adjusted message
*  1-Jan-86 | Added miditrace
* 18-Jan-86 | Shortened durations by 1/200 s to avoid roundoff problems --
*	    | see buildnote for details.
*  3-Mar-86 | Allow octave and accidentals in either order after pitch name.
*	    | Default octave is now one that gets nearest previous pitch,
*	    |  the tritone (half an octave) interval is descending by default.
*	    | Special commands handled by table search, !Rate command added
*	    |  to scale all times by a percentage (50 = half speed).
*  9-Mar-86 | Use space to limit amount of storage allocation.	Otherwise
*	    |	exhausting storage in phase1 caused phase2 to fail.
* 12-Mar-86 | Broke off command line parser into adagio.c, only parser remains
* 24-Mar-86 | Changed representation from note_struct to event_struct
*	    | Parse M, N, O, X, and Y as control change commands
* 23-May-86 | Added , and ; syntax: "," means "N0\n", ";" means "\n"
* 16-Jul-86 | modify to only call toupper/lower with upper/lower case as
*	    |  parameter to be compatible with standard C functions
*  7-Aug-86 | fixed bug with default pitches and rests
*****************************************************************************/

#ifdef UNIX
#include <stdio.h>
#include <ctype.h>
#ifndef __386BSD__
#include <malloc.h>
#endif
#else
#include "stdio.h"
#include "ctype.h"
#include "malloc.h"
#endif
#include "adagio.h"
#include "cmdline.h"
#include "allphase.h"
#include "midicode.h"

/*extern long space;*/		/* remaining free bytes */

int loadvoice(int, int, int, int, int);

#define MAINTAIN_PRECISION
/****************************************************************************
* The following are used to simulate fixed point with the radix point
* 8 bits from the right:
****************************************************************************/
/**
#define unity 256
#define round(x) (((x)+128)>>8)
#define precise(x) ((x)<<8)
**/
#define unity 16
#define round(x) (((x)+8)>>4)
#define precise(x) ((x)<<4)

#define nullstring(s) (s[0] == 0)

/****************************************************************************
* Routines local to this module:
****************************************************************************/
static event_type ctrlalloc();
static void do_a_rest();
static void doabsdur();
static void doabspitch();
static void docomment();
static void doctrl();
static void dodur();
static void doerror();
static void doloud();
static void donextdur();
static void dopitch();
static void doprogram();
static void dorate();
static void dospecial();
static void dotempo();
static void dotime();
static void dovoice();
static void fferror();
static int ad_init();
static void ins_event();
static int ins_ctrl();
static int ins_note();
static int issymbol();
static void marker();
static event_type nalloc();
static void parseend();
static void parsefield();
static int parsenote();
static void reverse();
static int scan();
static int scan1();
static int scanint();

/****************************************************************************
* data structures for parser lookup tables
****************************************************************************/

  struct durt {			/* duration translation table */
      char symbol;
      long value;
  };
struct durt durtable[5] =
{
    {'W', 240},
    {'H', 120},
    {'Q', 60},
    {'I', 30},
    {'S', 15}
};

  struct loudt {		/* loudness translation table */
      char symbol[4];
      int value;
  };

struct loudt loudtable[] =
{
    {"PPP", 20},
    {"PP\0", 26},
    {"P\0\0", 34},
    {"MP\0", 44},
    {"MF\0", 58},
    {"F\0\0", 75},
    {"FF\0", 98},
    {"FFF", 127}
};

#ifdef UNIX
static char *ssymbols[] =
{"TEMPO", "RATE"};
#else
static char *ssymbols[] =
{
    {"TEMPO"},
    {"RATE"}};
 /* this was inside istempo, but */
 /* I moved it here because of a compiler bug */
#endif

#define sym_tempo 0
#define sym_rate 1
/* number of symbols */
#define sym_n 2

#define linesize 100
static char line[linesize];	/* the input line */
static char token[linesize];	/* a token scanned from the input line */

static int vverbose = false;	/* controls verbose printout */
static void enter_prog(int chan, int prog, int bank);

static int pitchtable[7] =
{57, 59, 48, 50, 52, 53, 55};

#include "admp.c"


/****************************************************************************
*				do_a_rest
* Effect: parses a rest (R) command
****************************************************************************/

static void do_a_rest()
{
    if (fieldx < strlen(token))
	fferror("Nothing expected after rest");
    rest_flag = true;
}

/****************************************************************************
*				doabsdur
* Effect: parses an absolute dur (U) command
****************************************************************************/

static void doabsdur()
{
    if (isdigit(token[fieldx])) {
	dur = precise((long) scanint());
	dur = (dur * 100) / rate;
	if (fieldx < strlen(token)) {
	    fieldx = 2;
	    fferror("U must be followed by digits only");
	}
	symbolic_dur_flag = false;
    } else
	fferror("No digit after U");
}

/****************************************************************************
*				doabspitch
* Effect: parses an absolute pitch (P) command
****************************************************************************/

static void doabspitch()
{
    if (isdigit(token[fieldx])) {
	pitch = scanint();
	pitch_flag = true;
	if (fieldx < strlen(token))
	    fferror("P must be followed by digits only");
	else if (pitch < -12) {
	    fieldx = 1;
	    fferror("Minimum pitch of 12 will be used");
	    pitch = -12;
	} else if (pitch > 115) {
	    fieldx = 1;
	    fferror("Maximum pitch of 115 will be used");
	    pitch = 115;
	}
    } else
	fferror("No digits after P");
}

/****************************************************************************
*				docomment
* Effect: parses a comment (*) command
****************************************************************************/

static void docomment()
{
    line[linex] = 0;		/* force end of line to skip comment line */
}

/****************************************************************************
*				doctrl
* Inputs:
*    n: control number
* Effect: parses a control (J, K, M, O, X, or Y) command
****************************************************************************/

static void doctrl(n)
int n;
{
    ctrlval[n] = scanint();
    if (fieldx < strlen(token)) {
	fferror("Only digits expected here");
    } else {
	ctrlflag[n] = true;
	ctrlflag[0] = true;	/* ctrlflag[0] set if any flag is set */
    }
}

/****************************************************************************
*				dodur
* Effect: parses a duration (S, I, Q, H, or W) command
****************************************************************************/

static void dodur()
{
    int i, dotcnt = 0;
    long dotfactor;

    for (i = 0; i <= 5; i++) {
	if (durtable[i].symbol == token[fieldx - 1]) {
	    dur = precise(durtable[i].value);
	    break;
	}
    }
    if (i == 5) {
	fieldx--;
	fferror("Duration expected: one of W, H, Q, I, or S");
	return;
    }
    while (fieldx < strlen(token)) {
	if (token[fieldx] == 'T') {	/* triplet notation */
	    dur = (dur * 2) / 3;
	    fieldx++;
	} else if (token[fieldx] == '.') {	/* dotted notation */
	    dotcnt++;
	    fieldx++;
	} else if (isdigit(token[fieldx])) {	/* numbers are multipliers */
	    dur = dur * (long) scanint();
	} else {
	    fferror("Bad duration");
	    fieldx = strlen(token) + 1;
	}
    }
    dotfactor = 1;
    for (i = 1; i <= dotcnt; i++)
	dotfactor = dotfactor * 2;
    dur = (2 * dur) - (dur / dotfactor);
    dur = (dur * 100) / tempo;	/* time in centiseconds */
    dur = (dur * 100) / rate;
    symbolic_dur_flag = true;	/* see symbolic_dur_flag declaration */
}

/****************************************************************************
*				doerror
* Effect: parse an unrecognized field by reporting an error
****************************************************************************/

static void doerror()
{
    fieldx = 0;
    fferror("Bad field");
}

/****************************************************************************
*				doloud
* Effect: parse a loudness (L) command
****************************************************************************/

static void doloud()
{
    int i, j;

    if (strlen(token) <= 1) {
	fieldx = 0;
	fferror("L must be followed by loudness indication");
	return;
    }
    if (isdigit(token[fieldx])) {
	loud = scanint();
	if (fieldx < strlen(token))
	    fferror("Digits expected after L");
	else if (loud > 127) {
	    fieldx = 1;
	    fferror("Maximum loudness of 127 will be used");
	    loud = 127;
	}
	return;
    }
    if (strlen(token) > 4) {	/* maximum is 4, e.g. "Lppp" */
	fieldx = 0;
	fferror("Loudness field too long");
	return;
    }
    if (strlen(token) != 4) {	/* pad short symbols with 0	*/
	i = strlen(token);	/* e.g. "p\0" -> "p\0\0"	*/
	token[i + 1] = '\0';
    }
    for (i = 0; i <= 7; i++) {	/* loop through possibilities	*/
	for (j = 0; j <= 2; j++) {	/* test 3 characters	*/
	    if (token[fieldx + j] != loudtable[i].symbol[j])
		break;
	}
	if (j == 3) {
	    loud = loudtable[i].value;
	    return;
	}
    }
    fieldx = 1;
    fferror("Bad loudness indication");
}

/****************************************************************************
*				donextdur
* Effect: parse a next (N) command
* Implementation:
*	The syntax is N followed by a duration, so save dur and use dodur()
*	to parse the duration field.  Then restore dur (what a hack!).
*	The form N<digits> is parsed directly with scanint().
****************************************************************************/

static void donextdur()
{
    long save;			/* save the current duration */

    ndurp = true;		/* flag that N was given */
    if (isdigit(token[fieldx])) {
	ntime = precise((long) scanint());
	if (fieldx < strlen(token))
	    fferror("Only digits were expected here");
    } else {
	fieldx++;
	save = dur;
	dodur();
	ntime = dur;		/* get the result from dur, */
	dur = save;		/* and then restore it	*/
    }
}

/****************************************************************************
*				dopitch
* Effect: parses a pitch command
****************************************************************************/

static void dopitch()
{
    int p, octave = 0;
    int octflag = false;	/* set if octave is specified */

    p = pitchtable[token[0] - 'A'];
    while (true) {
	if (token[fieldx] == 'S') {	/* sharp */
	    p++;
	    fieldx++;
	} else if (token[fieldx] == 'N') {	/* skip */
	    fieldx++;
	} else if (token[fieldx] == 'F') {	/* flat */
	    p--;
	    fieldx++;
	} else if (isdigit(token[fieldx]) && !octflag) {	/* octave */
	    octave = scanint();
	    octflag = true;
	} else
	    break;		/* none of the above */
    }
    if (octflag)
	p = (p - 48) + 12 * octave;	/* adjust p to given octave */
    else {			/* adjust p to note nearest the default pitch */
	int octdiff = (p + 126 - pitch) / 12;
	p = p + 120 - (octdiff * 12);
    }
    if (fieldx != strlen(token))/* any unparsed characters? */
	fferror("Bad pitch indication");
    if (p > 115) {		/* pitch in range? */
	fieldx = 1;
	fferror("Pitch too high");
	p = 115;
    }
    pitch = p;
    pitch_flag = true;
}

/****************************************************************************
*				doprogram
* Effect: parses a program change (Z) command
****************************************************************************/

static void doprogram()
{
    if (isdigit(token[fieldx])) {
	new_prog = scanint();
	if (fieldx < strlen(token)) {
	    fferror("Z must be followed by digits only");
	} else if (new_prog < 1) {
	    fieldx = 1;
	    fferror("Minimum program of 1 will be used");
	    new_prog = 1;
	} else if (new_prog > 128) {
	    fieldx = 1;
	    fferror("Maximum program of 128 will be used");
	    new_prog = 128;
	}
    } else
	fferror("No digit after Z");
}

/****************************************************************************
*				dorate
* Effect: parses a !rate command
****************************************************************************/

static void dorate()
{
    linex += scan(&line[linex]);
    if (strlen(token) == 0)
	fferror("rate number expected");
    else {
	long oldrate = rate;
	fieldx = 0;
	rate = scanint();
	if (fieldx < strlen(token))
	    fferror("Only digits expected here");
	if (rate == 0) {
	    fieldx = 0;
	    fferror("Rate 100 will be used here");
	    rate = 100;
	}
	start = thetime;
	/* adjust dur in case it is inherited by next note */
	dur = (dur * oldrate);
	dur = dur / rate;
    }
}

/****************************************************************************
*				dospecial
* Effect: parses special (those starting with "!") commands
****************************************************************************/

static void dospecial()
{
    switch (issymbol()) {
    case sym_tempo:
	dotempo();
	break;
    case sym_rate:
	dorate();
	break;
    default:
	fferror("Special command expected");
    }
    parseend();			/* flush the rest of the line */
}

/****************************************************************************
*				dotempo
* Effect: parses a !tempo command
****************************************************************************/

static void dotempo()
{
    linex += scan(&line[linex]);
    if (strlen(token) == 0)
	fferror("Tempo number expected");
    else {
	long oldtempo = tempo;
	fieldx = 0;
	tempo = scanint();
	if (fieldx < strlen(token))
	    fferror("Only digits expected here");
	if (tempo == 0) {
	    fieldx = 0;
	    fferror("Tempo 100 will be used here");
	    tempo = 100;
	}
	start = thetime;
	/* adjust dur in case it is inherited by next note */
	if (symbolic_dur_flag) {
	    dur = (dur * oldtempo);
	    dur = dur / tempo;
	}
    }
}

/****************************************************************************
*				dotime
* Effect: parses a time (T) command
* Implementation: see implementation of donextdur()
****************************************************************************/

static void dotime()
{
    int save;

    if (isdigit(token[fieldx])) {
	thetime = precise((long) scanint());
	if (fieldx < strlen(token))
	    fferror("Only digits were expected here");
    } else {
	fieldx++;
	save = dur;
	dodur();
	thetime = dur;
	dur = save;
    }
    thetime += start;		/* time is relative to start */
}

/****************************************************************************
*				dovoice
* Effect: parse a voice (V) command (the voice is the MIDI channel)
****************************************************************************/

static void dovoice()
{
    if (isdigit(token[fieldx])) {
	voice = scanint();
	if (fieldx < strlen(token))
	    fferror("V must be followed by digits only");
	if (voice > 16) {
	    fferror("number too high, using 16 instead");
	    voice = 16;
	} else if (voice < 1) {
	    fferror("number too low, using 1 instead");
	    voice = 1;
	}
    } else
	fferror("No digit after V");
}

/****************************************************************************
*				fferror
* Inputs:
*	char *s: an error message string
* Effect:
*	prints the line with the error
*	puts a cursor (^) at the error location
*	prints the error message (s)
* Implementation:
*	this routine prints a carat under the character that
*	was copied into token[fieldx].	E.g. if fieldx = 0, the
*	carat will point to the first character in the field.
****************************************************************************/

static void fferror(s)
char *s;
{
    fprintf(stderr, "%3d | ", lineno);
    fprintf(stderr, "%s", line);
    marker(linex - strlen(token) + fieldx + 1 + 6);
    fprintf(stderr, "Error: %s.\n", s);
}

/****************************************************************************
*				issymbol
* Outputs: returns symbol number, or -1 if no match
* Assumes: token[1] has the symbol to look up (token[0] == '!')
****************************************************************************/

static int issymbol()
{
    int i, symb_num;
    char *sym;

    for (symb_num = 0; symb_num < sym_n; symb_num++) {
	sym = ssymbols[symb_num];
	i = 1;
	while (true) {
	    if (token[i] != *sym)
		break;
	    if (*sym == 0)
		return symb_num;
	    sym++;
	    i++;
	}
    }
    return -1;
}

/****************************************************************************
*				marker
* Inputs:
*	int count: the number of characters to indent
* Effect:
*	prints a carat (^) at the position specified on file stderr
****************************************************************************/

static void marker(count)
int count;
{
    int i;
    for (i = 1; i <= count - 1; i++)
	fprintf(stderr, " ");
    fprintf(stderr, "^\n");
}

/*****************************************************************
*			parseend
* Effect:
*	parse the note terminator, either ",", ";", or "\n"
*
****************************************************************/

static void parseend()
{
    linex += scan1(&line[linex]);
    switch (token[0]) {
    case ',':
	ndurp = true;		/* switch that next time was specified */
	ntime = 0;
	break;
    case ';':
    case '\n':
	break;
    default:
	fferror("Internal error: illegal separator?");
	break;
    }
}

/****************************************************************************
*				parsefield
* Effect: looks at first character of token and calls a parsing routine
*
****************************************************************************/

static void parsefield()
{
    fieldx = 1;
    switch (token[0]) {
    case 'T':
	dotime();
	break;
    case 'W':
    case 'H':
    case 'Q':
    case 'S':
    case 'I':
	dodur();
	break;
    case 'R':
	do_a_rest();
	break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
	dopitch();
	break;
    case 'P':
	doabspitch();
	break;
    case 'U':
	doabsdur();
	break;
    case 'L':
	doloud();
	break;
    case 'N':
	donextdur();
	break;
    case 'J':
	doctrl(1);
	break;
    case 'K':
	doctrl(2);
	break;
    case 'M':
	doctrl(3);
	break;
    case 'O':
	doctrl(4);
	break;
    case 'X':
	doctrl(5);
	break;
    case 'Y':
	doctrl(6);
	break;
    case 'V':
	dovoice();
	break;
    case 'Z':
	doprogram();
	break;
    default:
	doerror();
	break;
    }
}


/****************************************************************************
*				phase1
* Inputs:
*	FILE *fp: input file
* Outputs:
*	returns event_type: the parsed score
* Effect:
*	parses score from input file and builds score data structure
****************************************************************************/

event_type phase1(fp)
FILE *fp;
{
    event_type score = NULL;	/* the translated note list */
    int out_of_memory = false;	/* set when no more memory */

    if (!ad_init()) {		/* something bad happened in init(), STOP */
	fprintf(stderr, "WOOPS; something strange happened in INIT()!  ...exiting\n");
	exit(1);
	return NULL;		/* make lint happy */
    }
    lineno = 0;

    /* this loop reads lines */
    while ((fgets(line, linesize, fp) != NULL) && !out_of_memory) {
	lineno++;
	linex = 0;
	/* this loop reads notes from a line */
	while ((line[linex] != 0) && !out_of_memory) {
	    /* loop invariant: line[linex] is first char of next note */
	    pitch_flag = false;
	    linex += scan(&line[linex]);
	    if (!nullstring(token)) {
		if (token[0] == '*')
		    docomment();
		else if (token[0] == '!')
		    dospecial();
		else
		    out_of_memory = parsenote(&score);
	    } else
		parseend();
	}
    }

    if (out_of_memory) {
	fprintf(stderr, "Out of note memory at line %d,\n", lineno - 1);
	fprintf(stderr, "    the rest of your file will be ignored.\n");
    }
    if (verbose)
	printf(
		  "\nPhase 1 completed; %d note(s), %d ctrl(s) have been translated.\n",
		  note_count, ctrl_count);

    reverse(&score);
    return score;
}

/****************************************************************************
*				scan
* Inputs:
*	char *start: the string to scan
* Outputs:
*	returns int: the index of the next char in start to scan
* Effect:
*	skips over leading blanks
*	copies characters from start into token, converting to upper case
*	scanning stops on delimiter: one of space, tab, newline, semicolon
****************************************************************************/

static int scan(start)
char *start;
{
    int i = 0;
    int j = 0;
    char c;

    while ((start[i] == ' ') || (start[i] == '\t'))
	i++;

    while ((c = start[i]) != ' ' && c != '\t' && c != '\n' &&
	   c != ',' && c != ';') {
	if (islower(start[i]))
	    token[j] = toupper(start[i]);
	else
	    token[j] = start[i];
	j++;
	i++;
    }
    token[j] = '\0';
    return i;
}

/****************************************************************************
*				scan1
* Inputs:
*	char *start: the string to scan
* Outputs:
*	returns int: the index of the next char in start to scan
* Effect:
*	copies one char from start into token, converting to upper case
****************************************************************************/

static int scan1(start)
char *start;
{
    int i = 0;

    token[0] = *start;
    if (islower(token[0]))
	token[0] = toupper(token[0]);

    if (!nullstring(token)) {
	token[1] = '\0';
	i = 1;
    }
    return i;
}

/****************************************************************************
*				scanint
* Outputs:
*	returns int: the scanned integer
* Effect:
*	scans an unsigned integer from token, starting at fieldx
*	fieldx is incremented to end of the integer
****************************************************************************/

static int scanint()
{
    int i = 0;
    char c;
    while (fieldx < strlen(token)) {
	c = token[fieldx];
	if ((c >= '0') && (c <= '9')) {
	    i = (i * 10) + (c - '0');
	    fieldx++;
	} else
	    return i;
    }
    return i;
}
