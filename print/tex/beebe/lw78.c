/* -*-C-*- lw78.c */
/*-->lw78*/
/**********************************************************************/
/******************************** lw78 ********************************/
/**********************************************************************/
/* /home/csc-sun/u/sy/beebe/tex/dvi/tmp/lw78.c, Wed Oct  5 16:47:41 1988 */
/* Edit by Nelson H.F. Beebe (beebe at plot79.utah.edu) */
/* Add support for Unix, and clean up code with several JSYS-like macros.*/
/* <BEEBE.TEX.DVI>LW78.C.27, 29-Oct-87 16:01:55, Edit by BEEBE */
/* Added DOBE JSYS (dismiss process until output buffer empty) in */
/* Put_Packet(); this seems to have completely eliminated the I/O error */
/* conditions previously experienced; they were probably due to ALW+ */
/* buffer overruns.  A straight "COPY foo.ps ttynn:" got only 3 pages */
/* out before the ALW+ said "I/O error", which is what led me to */
/* suspect that overrun might be responsible for our ALW+ miserable */
/* performance. */
/* <BEEBE.TEX.DVI.NEW>LW78.C.9, 24-Sep-86 22:49:07, Edit by BEEBE */
/* Add -n (notify user) option */
/* <BEEBE.TEX.DVI.NEW>LW78.C.5, 19-Aug-86 14:13:53, Edit by BEEBE */
/* Change check for PostScript header to "%!" and add PShdrfile (-h) */
/* option support. */
/* <BEEBE.TEX.DVI>LW78.C.15,  2-Dec-85 18:41:26, Edit by BEEBE */
/* Add check for PostScript header in Copy_File */
/* <BEEBE.TEX.DVI>LW78.C.11, 21-Nov-85 12:05:20, Edit by BEEBE */
/* Add Time_Stamp() and calls thereto for each output line */
/***********************************************************************

This utility   supports sending files to  an  Apple LaserWriter  (or any
serial  PostScript  printer) over a 9600-baud  serial line.  The current
implementation supports  TOPS-20 and Unix (both Berkeley   and  System V
variants).

On TOPS-20, input  files may be either 7-bit   or   8-bit.  The  printer
serial line is  opened in binary  (Unix raw) mode, except that  XON/XOFF
flow control requests from the printer are obeyed.  Status messages from
the printer are monitored and dealt with.

Usage:
        lw78 [-aAcctFile] [-d] [-en] [-hPShdrfile] [-n] [-r] \
		-oOutfile infile1 [infile2] ... [infilek]

Letter case on the switches is ignored.

The  optional -a  switch  specifies  a page accounting  file  to which a
single  entry will be  made recording  a  time stamp, username, and page
count for  each file processed.  This  option is currently ignored under
Unix.

The  -d switch  causes a delete  and expunge  of  each file after  it is
printed.  This happens even if errors occurred, but that is exactly what
is needed when this program is run as a background spooler.

The -en switch optionally sends ASCII character n after each file.

The -h switch defines an optional header file which  will be prefixed to
non-PostScript files to allow them to be printed.

The -n switch requests that each file owner be sent  a message  when the
file has been printed.

The -r switch causes the files to be processed in reverse order  (useful
because of the reversed stacking of the LaserWriter).

If no  files  are  specified on the   command line, then read  from  the
standard input.   On  TOPS-20, this requires  that the input  be a 7-bit
file.

[05-Oct-88]
***********************************************************************/

#include <stdio.h>
#include <string.h>

#ifndef KCC_20
#define KCC_20 0
#endif

#ifndef PCC_20
#define PCC_20 0
#endif

#ifdef unix
#define OS_UNIX 1
#ifndef ATT
#define ATT 0
#endif
#ifndef BSD
#define BSD 0
#endif
#ifndef HPUX
#define HPUX 0
#endif
#ifdef mips
#undef ATT
#define ATT 1
#endif
#ifdef sgi
#undef ATT
#define ATT 1
#endif
#ifdef hpux
#undef HPUX
#define HPUX 1
#endif
#if (ATT | BSD | HPUX)
#else
#undef BSD
#define BSD 1
#endif
#else
#define OS_UNIX 0
#endif

#if OS_UNIX
#define OS_TOPS20 0
#else				/* assume TOPS-20 */
#define OS_TOPS20 1
#if    (KCC_20 | PCC_20)
#else
#undef PCC_20
#define PCC_20 1		/* PCC-20 is default compilers */
#endif
#endif				/* OS_UNIX */

#if (OS_TOPS20 | OS_UNIX)
#else
/* Cause a compilation error */
!!! This code has only been implemented for TOPS-20 and UNIX !!!
#endif

#if    KCC_20
#include <jsys.h>

static int acs[5];		/* standard argument for jsys() */
#define ac1 acs[1]
#define ac2 acs[2]
#define ac3 acs[3]
#define ac4 acs[4]
#define POINT(s)	((int)(s - 1))	/* byte pointer for ILDB */

/* KCC-20 and PCC-20  have similar enough JSYS  interfaces that we  just
define values for KCC-20 using PCC-20 names,  and copy in definitions of
a few macros from PCC-20's tops20.h */
#define JSbin	BIN
#define JSbkjfn	BKJFN
#define JSbout	BOUT
#define JSclosf	CLOSF
#define JSdisms	DISMS
#define JSdobe	DOBE
#define JSdvchr	DVCHR
#define JSgfust	GFUST
#define JSjfns	JFNS
#define JSmtopr	MTOPR
#define JSopenf	OPENF
#define JSsfcoc	SFCOC
#define JSsfmod	SFMOD
#define JSsibe	SIBE
#define JSsout	SOUT
#define JSstpar	STPAR

#define DVtty 012		/* terminal */
#define DV_typ 0777:35-17	/* device type field */

#define GFlwr 01		/* get file last writer */

#define GJ_fou 01:35-0		/* file is for output use */
#define GJ_sht 01:35-17		/* short call format */

#define MOslw 031		/* set width */
#define MOsll 033		/* set length */
#define MOsnt 034		/* set tty non-terminal status */
#define MOsmn 01		/* no system messages(i.e. suppress) */

#define OF_bsz 077:35-5		/* byte size */
#define OF_rd 01:35-19		/* read */
#define OF_wr 01:35-20		/* write */

#define TT_mff 01:35-1		/* mechanical formfeed present */
#define TT_tab 01:35-2		/* mechanical tab present */
#define TT_lca 01:35-3		/* lower case capabilities present */
#define TT_eco 01:35-24		/* echos on */
#define TT_dam 03:35-29		/* data mode */
#define TTbin 00		/* binary */
#define TT_pgm 01:35-34		/* page mode */

#define Absmask(name) ( (1?name) << (0?name) )    /* maybe use this one */
#define Getmask(name) 	 ( 1?name )
#define Getshift(name)	 ( 0?name )

#define getfield(var, name)	( (var) >> Getshift(name) & Getmask(name) )
#define getright(b)	( (b) & 0777777 )
#define makefield(name, value)	( ((value) & Getmask(name)) << Getshift(name) )
#define makeword(l, r)	( ((l) << 18) | (r) )
#define setfield(var, name, value) ( (var) = ((var) & ~Absmask(name)) |\
	makefield(name, value) )
#endif /* KCC_20 */

#if    PCC_20
#include <file.h>
#include <monsym.h>
#endif /* PCC_20 */

#if    OS_UNIX
#include <fcntl.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
struct passwd* getpwuid();
struct passwd* pwd;
struct stat statbuf;

#define MAXJFN 256
#define VALID(jfn) ((0 <= jfn) && (jfn < MAXJFN))
int ungot_char[MAXJFN];			/* One for each file descriptor; */
					/* there is no standard name for */
					/* this limit, but 256 is plenty. */

#if    (ATT | HPUX)
#include <fcntl.h>
#include <termio.h>

static struct termio tty;
static struct termio ttysave;
static int setblock();
#endif

#if    BSD
#include <sgtty.h>
#include <sys/ioctl.h>

static struct sgttyb tty;
static struct sgttyb ttysave;
static struct tchars the_tchars;
#endif

#endif /* OS_UNIX */

typedef int BOOLEAN;
#define TRUE 1
#define FALSE 0

#define JSerr 0			/* must match values used by KCC-20 */
#define JSok 1			/* and PCC-20 */

#if    OS_TOPS20
typedef int packet_data;
#endif

#if    OS_UNIX
typedef char packet_data;
#endif

#define ACCOUNTING 1			/* define for accounting code */

/* global constants */

#define INTERVAL (5*512)                /* how often to check input buffer */
#define MAXERRMSG 512                   /* error message size */
#define MAXFILENAME 144			/* longest filename */
#define MAXIDLE (10*1000)               /* msec to wait for idle status */
#define MAXLINE 128			/* maximum id line size */
#define MAXPACKET (5*512)               /* packet size to output */
#define MAXPAUSE (500)                  /* msec to wait for new byte */
#define MAXUSERNAME 256			/* longest username */
#define MAXWAIT (5*1000)                /* msec to wait after each file */

#define PSHEADER "%!"			/* what every PostScript file has */

/* global variables */

char author[MAXUSERNAME+1];
BOOLEAN del_flag;                       /* file delete flag */
int eof_char;                           /* end-of-file character */
char errmsg[MAXERRMSG+1];               /* string from output device */
int nbytes;                             /* global input byte count */
BOOLEAN notify_owner;			/* notify file owner when printed */
int npacket;				/* packet[] index */
packet_data packet[MAXPACKET+1];	/* want space for 1 extra eof_char */
char timestr[27];
long timeval;

#if    ACCOUNTING
FILE *acctfile;				/* accounting file */
#endif

FILE *PShdrfile = (FILE *)NULL;		/* PostScript header file */

char* current_file = (char*)NULL;	/* name of file being processed */
					/* (saved for Unix and JFNS) */

/* Function macro definitions */

#define BYTEPTR(x) (0004400000000 | (int)((x)-1)) /* x must be int*, not int */
                                        /* PCC uses 36-bit bytes */
#define CTL(c) ((char)(((int)(c)) & 037))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

/***********************************************************************
Procedure  macro definitions.  For  programming convenience, these  look
like TOPS-20 system  calls, but might  expand to Unix equivalents.  Here
is what they do:

BIN		-- byte input
BKJFN		-- backup file input pointer one byte (like ungetc())
BOUT		-- byte output
CLOSF		-- close file
DOBE		-- dismiss process until output buffer empty
GFUST		-- get file owner username string
HAVE_INPUT	-- test if data exists in input buffer
JFNS		-- jfn to string - convert job file number to file name
SOUT		-- string output
WAITMS		-- wait for n milliseconds.

***********************************************************************/

#if    OS_TOPS20

#define jsBIN(jfn)	(ac1 = jfn, (void)jsys(JSbin, acs), ac2)

#define jsBKJFN(jfn,c)	(void)(ac1 = jfn, jsys(JSbkjfn, acs))

#define jsBOUT(jfn,c)	(void)(ac1 = jfn, ac2 = c, jsys(JSbout, acs))

#define jsCLOSF(jfn)	(void)(ac1 = jfn, jsys(JSclosf, acs))

#define jsDOBE(jfn)	(void)(ac1 = jfn, (void)jsys(JSdobe, acs))

#define jsGFUST(jfn,s)	(void)(ac1 = makeword(GFlwr,jfn), ac2 = POINT(s),\
    jsys(JSgfust, acs))

#define HAVE_INPUT(jfn)	(ac1 = jfn, (void)jsys(JSsibe, acs), (ac2 > 0))

#define jsJFNS(jfn,buf)	(void)(ac1 = POINT(buf), ac2 = jfn, ac3 = 0, ac4 = 0,\
    buf[0] = '\0', jsys(JSjfns, acs))

#define jsSOUT(jfn,buf,n)	(((n) > 0) ? \
    (ac1 = jfn, ac2 = BYTEPTR(buf), ac3 = -(n), ac4 = 0, jsys(JSsout, acs)) :\
    JSok)

#define WAITMS(msec)	(void)(ac1 = msec, (void)jsys(JSdisms, acs))

#endif /* OS_TOPS20 */

#if    OS_UNIX

#define jfnof(fd)	(fd)		/* jfn == file descriptor on Unix */

#define jsBIN(jfn)	Get_One_Byte((jfn))

#define jsBKJFN(jfn,c)	(void)(ungot_char[(jfn)] = (c))

#define jsBOUT(jfn,c)	(cbuf[0] = (c), write((jfn),cbuf,1))
char cbuf[1];				/* temporary buffer for BOUT */

#define jsCLOSF(jfn)	(void)close(jfn)

#if    (ATT | HPUX)
#define jsDOBE(jfn)	(void)ioctl((jfn),TCSBRK,1)
#else
#define jsDOBE(jfn)	/* BSD has no equivalent, imagine that! */
#endif /* (ATT | HPUX) */

#define jsGFUST(jfn,s)	(void)(((fstat((jfn),&statbuf) == 0) &&\
	((pwd = getpwuid((int)statbuf.st_uid)) != (struct passwd*)NULL)) ?\
	(int)strcpy(s,pwd->pw_name) : (s[0] = '\0'))

#define HAVE_INPUT(jfn) (Have_Input(jfn) > 0)

/* Unix keeps no filename, so we use the global saved copy */
#define jsJFNS(jfn,buf)	(void)strcpy(buf,current_file)

#define jsSOUT(jfn,buf,n)	(((n) > 0) ? \
	( (write((jfn),(buf),(n)) == (n) ? JSok: JSerr) ) : JSok)

#ifdef sun
/* use microsecond sleep on Sun OS */
#define WAITMS(msec) (void)usleep((unsigned)(1000*(msec)))
#else
/* use 1-second resolution sleep; sleep for 2 guarantees 1 second delay */
#define WAITMS(msec) (void)sleep(MAX(2,(msec)/1000))
#endif

#endif /* OS_UNIX */


/* Function/Procedure declarations */

void Copy_File();
char *ctime();
char *fgetname();
int Get_Byte();

#if    OS_UNIX
int Get_One_Byte();
#endif /* OS_UNIX */

void Get_Msg();

#if    OS_UNIX
int Have_Input();
#endif /* OS_UNIX */

void Init_Terminal();
BOOLEAN Input_Error();
void Put_Packet();
long time();
void Time_Stamp();
void Wait_For_Printer_Ready();

/***********************************************************************
For TOPS-20, it would be nice to have done this entirely with getc/putc,
but alas, PCC  will  not open an 8-bit  file  to a terminal,  because it
attempts to issue an  illegal SFBSZ%  JSYS  to set  the file  byte size,
instead  of checking to see if  it is a  terminal, and then just setting
the byte size in the OPENF% JSYS.   We therefore  use the JSYS interface
routines to do our own GTJFN%, OPENF%, and BOUT% calls.  Sigh....

For Unix,  these calls map fairly easily  into low-level open  / close /
read / write calls, and the `jfn' (job file number) is then just  a Unix
small integer file descriptor.
***********************************************************************/

int
main(argc,argv)
int argc;
char* argv[];
{
        BOOLEAN reverse_order;
        register int c;
        register int k;
        register int len;
        int fp;
        int outjfn;
        int nfile;
        int kfile;
        register FILE* infile;

        if (argc < 2)
        {
            (void)fprintf(stderr,
              "%%Usage: %s [-aAcctFile] [-d] [-en] [-hPShdrfile] [-n] [-r] \
-ooutfile infile1 [infile2] ... [infilek]\n",
                argv[0]);
            exit(1);
        }

        del_flag = FALSE;
        eof_char = -1;
        nfile = 0;
	notify_owner = FALSE;
        outjfn = -1;
        reverse_order = FALSE;
	PShdrfile = (FILE *)NULL;

#if    ACCOUNTING
	acctfile = (FILE *)NULL;
#endif

        for (k = 1; k < argc; ++k)
        {
            if (*argv[k] != '-')        /* not switch, must be file name */
                nfile++;

#if    ACCOUNTING
            else if ((strncmp("-a",argv[k],2) == 0) ||
                (strncmp("-A",argv[k],2) == 0))
            {
		if ((acctfile = fopen(argv[k]+2,"a")) == (FILE *)NULL)
		{
		    (void)fprintf(stderr,"?Cannot open accounting file [%s]\n",
		        argv[k]+2);
		    exit(1);
		}
	    }
#endif

            else if ((strncmp("-d",argv[k],2) == 0) ||
                (strncmp("-D",argv[k],2) == 0))
                del_flag = TRUE;
            else if ((strncmp("-e",argv[k],2) == 0) ||
                (strncmp("-E",argv[k],2) == 0))
                eof_char = atoi(argv[k]+2);
            else if ((strncmp("-h",argv[k],2) == 0) ||
                (strncmp("-H",argv[k],2) == 0))
            {
		if ((PShdrfile = fopen(argv[k]+2,"r")) == (FILE *)NULL)
		{
		    (void)fprintf(stderr,"?Cannot open PShdrfile [%s]\n",
		        argv[k]+2);
		    exit(1);
		}
	    }
            else if ((strncmp("-n",argv[k],2) == 0) ||
                (strncmp("-N",argv[k],2) == 0))
		notify_owner = TRUE;
            else if ((strncmp("-r",argv[k],2) == 0) ||
                (strncmp("-R",argv[k],2) == 0))
                reverse_order = TRUE;
            else if ((strncmp("-o",argv[k],2) == 0) ||
                (strncmp("-O",argv[k],2) == 0))
            {
#if OS_TOPS20
                if ((outjfn = _gtjfn(argv[k]+2, makefield(GJ_fou,1) |
                    makefield(GJ_sht,1))) != -1)      /* then got valid jfn */
                {
                    ac1 = outjfn = getright(outjfn);
                    ac2 = makefield(OF_bsz, 8);
                    ac2 = setfield(ac2,OF_wr,1);
                    ac2 = setfield(ac2,OF_rd,1);
                    if (jsys(JSopenf, acs) == JSerr)
                        outjfn = -1;    /* open failed */
                }
#endif /* OS_TOPS20 */
#if OS_UNIX
		if (strcmp(argv[k]+2,"-") == 0)
		    /* -O- says printer is on stdout */
		    outjfn = fileno(fdopen(fileno(stdout),"r+"));
		else
		    outjfn = open(argv[k]+2, O_RDWR | O_NDELAY);
		if (VALID(outjfn))
		    ungot_char[outjfn] = EOF;
#endif /* OS_UNIX */

            }
            else
            {
                (void)fprintf(stderr,"?Unknown switch [%s]\n",argv[k]);
                exit(1);
            }
        }
        if (outjfn == -1)
        {
            (void)fprintf(stderr,"?Cannot open output file\n");
            exit(1);
        }
        else                /* open okay */
            Init_Terminal(outjfn);

	if (nfile == 0)
	{
	    argv[1] = "-";	/* input from stdin when no files given */
	    nfile = 1;
	}

        kfile = 0;
        for (k = (reverse_order ? (argc-1) : 1);
            (0 < k) && (k < argc);
            (reverse_order ? --k : ++k))
        {
            if ((argv[k][0] == '-') && (argv[k][1] != '\0'))
                ;                       /* ignore switch field */

#if    KCC_20
            else if ((infile = (argv[k][0] == '-') ? stdin :
		fopen(argv[k],"rb7")) != (FILE*)NULL)
#endif
#if    PCC_20
            else if ((fp = (argv[k][0] == '-') ? fileno(stdin) :
		open(argv[k],FATT_RDONLY | FATT_SETSIZE | FATT_BINARY,7)) >= 0)
#endif
#if    OS_UNIX
            else if ((infile =
		(argv[k][0] == '-') ? stdin : fopen(argv[k],"r"))
		!= (FILE*)NULL)
#endif
            {
#if    PCC_20
                infile = fdopen(fp,"rb");
#endif
                Wait_For_Printer_Ready(outjfn);

		Time_Stamp(stderr);
                (void)fprintf(stderr,"[%d of %d: %s]",++kfile,nfile,argv[k]);
		current_file = argv[k];	/* saved for Unix accounting */
                Copy_File(infile,outjfn);
		if (notify_owner)
		{
		    jsGFUST(jfnof(fileno(infile)),author);
		    timeval = time((long*)NULL);
		    strcpy(timestr,ctime(&timeval));
		    len = strlen(timestr) - 1;	/* ctime has its own \n */
		    timestr[len] = '\0';        /* so kill it */
#if    OS_TOPS20
		    (void)sprintf(errmsg,
		        "send %s %s: %s printed on LaserWriter\n",
		        author,timestr,argv[k]);
#endif /* OS_TOPS20 */
#if    OS_UNIX
		    (void)sprintf(errmsg,
			"write %s <<END-OF-FILE\n%s:\
 %s printed on PostScript laser printer\nEND-OF-FILE",
		        author,timestr,argv[k]);
#endif /* OS_UNIX */
		    (void)system(errmsg);
		}
                (void)fclose(infile);

                if (del_flag)
                {
                    (void)unlink(argv[k]);      /* delete the file */
                    (void)fprintf(stderr," [OK - deleted]");
                }
                else
                    (void)fprintf(stderr," [OK]");

                if (kfile < nfile)      /* delay next output to try to */
                    WAITMS(MAXWAIT);      /* avoid timeout bug! */
            }
            else
	    {
	        Time_Stamp(stderr);
                (void)fprintf(stderr,"?Cannot open input file [%s]",argv[k]);
	    }
        }

	jsCLOSF(outjfn);
	Time_Stamp(stderr);
	(void)fprintf(stderr,"Spooler terminated\n\n\n\n");
	return(0);
}

void
Copy_File(infile,outjfn)
register FILE* infile;
register int outjfn;
{
    register int c;
    register int k;
    char filename[MAXFILENAME+1];
    char idline[MAXLINE+1];

#if    ACCOUNTING
    int page_count;
    packet_data *pbyte;
#endif

    (void)
    fgets(idline,MAXLINE,infile);	/* peek at first line */
    rewind(infile);

    if (strncmp(idline,PSHEADER,strlen(PSHEADER)) != 0)
    {
	if (PShdrfile != (FILE *)NULL)
	{
		rewind(PShdrfile);
		nbytes = 0;
		while ((npacket = read(fileno(PShdrfile),packet,MAXPACKET)) > 0)
		{
		    nbytes += npacket;
		    Put_Packet(outjfn,packet,npacket);
		    if (Input_Error(PShdrfile,outjfn))
		        return;
		}
	}
	else /* reject non-PostScript file */
	{
            k = MAX(2,strlen(idline));
	    if (idline[k-1] == '\n')
	        idline[k-1] = '\0';		/* kill NL */
	    if (idline[k-2] == '\r')
		idline[k-2] = '\0';		/* kill CR */
	    Time_Stamp(stderr);
	    (void)fprintf(stderr,
	        "?File may not contain PostScript.  First line has\n");
	    (void)fprintf(stderr,"\t[%s] instead of [%s] -- file discarded",
		idline,PSHEADER);
	    return;
	}
    }

#if    ACCOUNTING
    page_count = 0;
#endif

    nbytes = 0;
#if    (INTERVAL-MAXPACKET)		/* copy one at a time */
    npacket = 0;
    while ((c = getc(infile)) != EOF)
    {
#if    ACCOUNTING
        if (c == '\f')
	    page_count++;		/* each FF counts one page */
#endif
        if ((!((++nbytes) % INTERVAL)) && Input_Error(infile,outjfn))
            return;
        if (npacket >= MAXPACKET)
        {
            Put_Packet(outjfn,packet,npacket);
            npacket = 0;
        }
        packet[npacket++] = (packet_data)c;
    }
#else           /* copy in block mode */
    while ((npacket = read(fileno(infile),packet,MAXPACKET)) > 0)
    {
        nbytes += npacket;
        Put_Packet(outjfn,packet,npacket);

#if    ACCOUNTING
	if (acctfile != (FILE*)NULL)
	{
	    for ((pbyte = packet, k = npacket); k; (--k, ++pbyte))
	        if ((char)*pbyte == '\f')
		    page_count++;	/* each FF counts one page */
	}
#endif

        if (Input_Error(infile,outjfn))
            return;
    }
    if (npacket < 0)
    {
        Time_Stamp(stderr);
	(void)fprintf(stderr,"?I/O error on input file");
    }
    npacket = 0;
#endif

    if (eof_char >= 0)
        packet[npacket++] = (packet_data)eof_char;
    if (npacket > 0)

        Put_Packet(outjfn,packet,npacket);

#if    ACCOUNTING
    if (acctfile != (FILE*)NULL)
    {
	jsGFUST(jfnof(fileno(infile)),author);
	Time_Stamp(acctfile);
	(void)fprintf(acctfile,"%8d\t%s\t%s",
	    MAX(1,page_count),author,fgetname(infile,filename));
    }
#endif

}

int
Get_Byte(thejfn)                        /* get byte (maybe waiting) */
register int thejfn;                    /* returns EOF if no input */
{
    if (HAVE_INPUT(thejfn))
        return(jsBIN(thejfn));

    WAITMS(MAXPAUSE);

    if (HAVE_INPUT(thejfn))
        return(jsBIN(thejfn));
    else                                /* still no input */
        return(EOF);
}

void
Get_Msg(thejfn)
int thejfn;
{
    register int cm1,cm2,c,k;

    Time_Stamp(stderr);
    k = 0;
    c = 0;
    cm1 = 0;
    cm2 = 0;
    while (!((cm2 == ']') && (cm1 == '%') && (c == '%'))) /* until "]%%" */
    {   /* skip <garbage> and collect %%[ ... ]%% */
        cm2 = cm1;
        cm1 = c;
        if ((c = Get_Byte(thejfn)) == EOF)
            break;                      /* no input yet */
        if ((cm2 == '%') && (cm1 == '%') && (c == '['))
        {                               /* restart message collection */
            k = 2;
            errmsg[0] = '%';
            errmsg[1] = '%';
        }
        if (k < MAXERRMSG)
            errmsg[k++] = c;
        putc(c,stderr);
    }


    errmsg[k] = '\0';                   /* terminate string */

    while (TRUE)                        /* "infinite" loop to discard any */
                                        /* remaining input up to start of */
                                        /* next message */
    {

        if ((c = Get_Byte(thejfn)) == EOF)
            break;                      /* no input, so exit loop */

        if (c == '%')                   /* if start of new message then */
        {                               /* put character back and exit */
	    jsBKJFN(thejfn,c);
            break;
        }

	if (!((c == '\r') || (c == '\n')))/* echo the character */
	    putc(c,stderr);		/* but discard terminal CR LF */
    }					/* to avoid unwanted blank lines */
}

#if    OS_UNIX
int
Get_One_Byte(thejfn)
int thejfn;
{
    char char_buf[1];

    if (ungot_char[thejfn] == EOF)
	read(thejfn,&char_buf[0],1);
    else
    {
	char_buf[0] = ungot_char[thejfn];
	ungot_char[thejfn] = EOF;
    }
    return ((int)char_buf[0]);
}
#endif /* OS_UNIX */

#if    OS_UNIX
int
Have_Input(thejfn)
int thejfn;
{
#if    (ATT | HPUX)
    char c;

    if (setblock(thejfn,FALSE) == EOF)
	return (EOF);
    switch (read(thejfn,&c,1))
    {
    case -1:
	return (EOF);

    case 0:
	return (0);

    default:
	ungot_char[thejfn] = (int)c;	/* save the character for next read */
	return (1);
    }
#endif /* (ATT | HPUX) */
#if    BSD
    int count;
    int code;

    code = ioctl(thejfn, FIONREAD, &count);

    return ((code == -1) ? EOF : count);
#endif /* BSD */
}
#endif /* OS_UNIX */

void
Init_Terminal(thejfn)
register int thejfn;
{
#if    OS_TOPS20
    ac1 = thejfn;
    (void)jsys(JSdvchr, acs);           /* get device characteristics */

    if (getfield(ac2,DV_typ) == DVtty)
    {                                   /* have terminal, set its features */
        ac1 = thejfn;
        ac2 = MOsnt;
        ac3 = MOsmn;                    /* no system messages */
        (void)jsys(JSmtopr, acs);

        ac1 = thejfn;
        ac2 = makefield(TT_mff,1);      /* has formfeed */
        ac2 = setfield(ac2,TT_tab,1);   /* has tab */
        ac2 = setfield(ac2,TT_lca,1);   /* has lowercase */
        ac2 = setfield(ac2,TT_pgm,1);   /* has X-on/X-off */
        (void)jsys(JSstpar, acs);

        ac1 = thejfn;
        ac2 = makefield(TT_dam,TTbin);  /* binary output */
        ac2 = setfield(ac2,TT_eco,0);   /* no echo */
        (void)jsys(JSsfmod, acs);

        ac1 = thejfn;
        ac2 = 0525252525252;
        ac3 = 0525252525252;            /* no CTL-char translation */
        (void)jsys(JSsfcoc, acs);

        ac1 = thejfn;
        ac2 = MOslw;
        ac3 = 0;                        /* line width = 0 */
        (void)jsys(JSmtopr, acs);

        ac1 = thejfn;
        ac2 = MOsll;
        ac3 = 0;                        /* page length = 0 */
        (void)jsys(JSmtopr, acs);
    }
#endif /* OS_TOPS20 */
#if    OS_UNIX
#if    (ATT | HPUX)
	tty = ttysave;
	tty.c_iflag &= ~(INLCR | ICRNL | IUCLC | ISTRIP | BRKINT);
	tty.c_iflag |= (IXON | IXOFF);	/* raw mode, but use flow control */
	tty.c_lflag &= ~(ICANON | ISIG | ECHO);
	tty.c_cflag &= ~CBAUD;
	tty.c_cflag |= B9600;
	tty.c_cc[4] = 5;		/* MIN */
	tty.c_cc[5] = 2;		/* TIME */
	if (ioctl(thejfn,TCSETAF,&tty) == -1)
	    (void)fprintf(stderr,"Cannot set printer line to RAW mode\n");
#endif /* (ATT | HPUX) */
#if    BSD
	tty = ttysave;
	tty.sg_ispeed = B9600;
	tty.sg_ospeed = B9600;
	tty.sg_flags &= ~(CRMOD | ECHO | LCASE | RAW);
	tty.sg_flags |= TANDEM;		/* allow input flow control */
	if (ioctl(thejfn,TIOCSETP,&tty) == -1)
	    (void)fprintf(stderr,"Cannot set printer line to RAW mode\n");
	(void)ioctl(thejfn,TIOCGETC,&the_tchars);
	the_tchars.t_startc = CTL('Q');	/* restart terminal output */
	the_tchars.t_stopc = CTL('S');	/* suspend terminal output */
	if (ioctl(thejfn,TIOCSETC,&the_tchars) == -1)
	    (void)fprintf(stderr,
	    "Cannot set printer line to XON/XOFF flow control\n");
#endif /* BSD */
#endif /* OS_UNIX */

}

BOOLEAN
Input_Error(infile,thejfn)		/* return TRUE if error */
register FILE* infile;
register int thejfn;
{
    register int c;

    if (HAVE_INPUT(thejfn))
    {
        Get_Msg(thejfn);
        if (strncmp(errmsg,"%%[ Error: timeout;",19) == 0)
        {
            Time_Stamp(stderr);
	    (void)fprintf(stderr,"[Error occurred in input byte range %d..%d]",
                MAX(0,nbytes-32),nbytes);
            Time_Stamp(stderr);
	    (void)fprintf(stderr,
            "Attempting to recover from Apple LaserWriter BUG of \
timeout error");
            (void)fflush(stderr);
            return(FALSE);
        }
	else if (strncmp(errmsg,"%%[ PrinterError: out of paper",30) == 0)
	{
	    Time_Stamp(stderr);
	    (void)fprintf(stderr,"\t[Waiting 10 sec for paper]");
	    (void)fflush(stderr);
	    WAITMS(10000);
	    return(FALSE);
	}
	else if (strncmp(errmsg,"%%[ PrinterError: no paper tray",31) == 0)
	{
	    Time_Stamp(stderr);
	    (void)fprintf(stderr,"\t[Waiting 10 sec for paper tray]");
	    (void)fflush(stderr);
	    WAITMS(10000);
	    return(FALSE);
	}
        else if (strncmp(errmsg,"%%[ Error: VMerror;",19) == 0)
        {
            Time_Stamp(stderr);
	    (void)fprintf(stderr,
            "Attempting to recover from Apple LaserWriter BUG of VMerror");
            (void)fflush(stderr);
	    Wait_For_Printer_Ready(thejfn);
	    do				/* skip to next page or end-of-job */
	    {
		c = getc(infile);
	    }
	    while ((c != EOF) && (c != CTL('D')) && (c != CTL('L')));
	    npacket = 0;		/* discard last packet */
            return(FALSE);
        }
        else if (strcmp(errmsg,"%%[ status: idle ]%%") == 0)
            return(FALSE);
        else
        {
            Time_Stamp(stderr);
	    (void)fprintf(stderr,"[Error occurred in input byte range %d..%d]",
                MAX(0,nbytes-32),nbytes);
	    Time_Stamp(stderr);
            (void)fprintf(stderr,"[Job flushed]");
            (void)fflush(stderr);
            return(TRUE);
        }
    }
    else                                /* no input yet */
        return(FALSE);
}

void
Put_Packet(thejfn,packet,n)
int thejfn;
packet_data packet[];
int n;
{
    jsDOBE(thejfn);			/* dismiss until output buffer empty */

    if (jsSOUT(thejfn,packet,n) == JSerr)
    {
	Time_Stamp(stderr);
	perror("?SOUT error");
	exit(1);
    }
}

void
Time_Stamp(fp)
FILE *fp;
{
    int k;

    timeval = time((long*)NULL);
    strcpy(timestr,ctime(&timeval));
    k = strlen(timestr) - 1;	/* ctime has its own \n */
    timestr[k] = '\0';          /* so kill it */
    (void)fprintf(fp,"\n%s\t",timestr);
}

void
Wait_For_Printer_Ready(thejfn)
register int thejfn;
{
#if    OS_UNIX
    char c[1];
#endif /* OS_UNIX */

    while (TRUE)                        /* "infinite" loop */
    {
	jsBOUT(thejfn,CTL('T'));		/* CTL-T for status message */
	jsDOBE(thejfn);			/* wait until output sent */
        Get_Msg(thejfn);
        if (strcmp(errmsg,"%%[ status: idle ]%%") == 0)
            break;                      /* printer ready to go */

	jsBOUT(thejfn,CTL('D'));		/* CTL-D to terminate previous job */
	Time_Stamp(stderr);
        (void)fprintf(stderr,"\t[Waiting %d sec]",MAXIDLE/1000);
        WAITMS(MAXIDLE);
    }
}

char *
fgetname(fp,buffer)
FILE *fp;
char buffer[];
{
    jsJFNS(jfnof(fileno(fp)),buffer);
    return(buffer);
}
#if    OS_UNIX
#if    (ATT | HPUX)
static int
setblock(fd,on)		/* turn blocking on or off */
			/* return EOF on error, 0 on success */
int fd;			/* file descriptor */
BOOLEAN on;		/* FALSE == turn off, TRUE == turn on */
{
    static int blockf;
    static int nonblockf;
    static BOOLEAN first = TRUE;
    int flags;

    if (first)
    {
	first = FALSE;
	if ((flags = fcntl(fd,F_GETFL,0)) == -1)
	    return (EOF);
	blockf = flags & ~O_NDELAY;	/* turn off O_NDELAY */
	nonblockf = flags | O_NDELAY;	/* turn on O_NDELAY */
    }
    if (fcntl(fd,F_SETFL,on ? blockf : nonblockf) == -1)
	return (EOF);
    return (0);
}
#endif /* (ATT | HPUX) */
#endif /* OS_UNIX */
