/*
 *  xv.h  -  header file for xv, but you probably guessed as much
 * 
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 */

/* Copyright Notice
 * ================
 * Copyright 1989, 1990, 1991, 1992, 1993 by John Bradley
 * 
 * Permission to use, copy, and distribute XV in its entirety, for 
 * non-commercial purposes, is hereby granted without fee, provided that
 * this license information and copyright notice appear in all copies.
 * 
 * Note that distributing XV 'bundled' in with ANY product is considered
 * to be a 'commercial purpose'.
 *
 * Also note that any copies of XV that are distributed MUST be built
 * and/or configured to be in their 'unregistered copy' mode, so that it
 * is made obvious to the user that XV is shareware, and that they should
 * consider donating, or at least reading this License Info.
 * 
 * The software may be modified for your own purposes, but modified
 * versions may NOT be distributed without prior consent of the author.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author be held liable for any damages
 * arising from the use of this software.
 * 
 * If you would like to do something with XV that this copyright
 * prohibits (such as distributing it with a commercial product, 
 * using portions of the source in some other program, etc.), please
 * contact the author (preferably via email).  Arrangements can
 * probably be worked out.
 *
 * XV is shareware for PERSONAL USE only.  You may use XV for your own
 * amusement, and if you find it nifty, useful, generally cool, or of
 * some value to you, your non-deductable donation would be greatly
 * appreciated.  $25 is the suggested donation, though, of course,
 * larger donations are quite welcome.  Folks who donate $25 or more
 * can receive a Real Nice bound copy of the XV manual for no extra
 * charge.
 * 
 * Commercial, government, and institutional users MUST register their
 * copies of XV, for the exceedingly REASONABLE price of just $25 per
 * workstation/X terminal.  Site licenses are available for those who
 * wish to run XV on a large number of machines.  Contact the author
 * for more details.
 *
 * The author may be contacted via:
 *    US Mail:  John Bradley
 *              1053 Floyd Terrace
 *              Bryn Mawr, PA  19010
 *
 *    Phone:    (215) 898-8813
 *    EMail:    bradley@cis.upenn.edu
 */


#define REVDATE   "Version 3.00  Rev: 3/30/93"
#define VERSTR    "3.00"

/*
 * uncomment the following, and modify for your site, but only if you've
 * actually registered your copy of XV...
 */
/* #define REGSTR "Registered to the GRASP Lab, University of Pennsylvania" */


#ifndef VMS
#define THUMBDIR ".xvpics"  /* name of thumbnail file subdirectories */
#else
#define THUMBDIR "xvpics"
#endif



/*************************************************/
/* START OF MACHINE-DEPENDENT CONFIGURATION INFO */
/*************************************************/

/* Things to make xv more likely to just build, without the user tweaking
   the makefile */

#ifdef hpux        /* HPUX machines (SVR4, NO_RANDOM) */
#undef  SVR4
#define SVR4
#undef  NO_RANDOM
#define NO_RANDOM
#endif


#ifdef sgi         /* SGI machines (SVR4) */
#undef  SVR4
#define SVR4
#endif

#include <X11/Xos.h>     /* need type declarations immediately */

/*********************************************************/


/* The BSD typedefs are used throughout.
 * If your system doesn't have them in <sys/types.h>,
 * then define BSDTYPES in your Makefile.
 */
#if defined(BSDTYPES) || defined(VMS)
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
#endif


#ifdef __UMAXV__              /* for Encore Computers UMAXV */
#include <sys/fs/b4param.h>   /* Get bsd fast file system params*/
#endif


/* things that *DON'T* have dirent.  Hopefully a very short list */
#if defined(__UMAXV__)
# ifndef NODIRENT
#  define NODIRENT
# endif
#endif


/* include files */
#include <stdio.h>
#include <math.h>
#include <ctype.h>

#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#endif


/* include the appropriate string header file */ 
#if defined(SVR4) || defined(__convex__) || defined(VMS)
#include <string.h>
#define index strchr
#define rindex strrchr
#else
#include <strings.h>
#endif


#if defined(SVR4) || defined(sco) || defined(XENIX)
#define GETWD(x) getcwd(x, sizeof(x))
#else
#define GETWD(x) getwd(x)
#endif


#if defined(apollo) || defined(pyr)
/* DomainOS 10.2 BSD4.3 version of str[r]chr is broken ([r]index works) */
/* pyramid bsd doesn't have str[r]chr */
#define strchr index
#define strrchr rindex
#endif

#ifndef VMS
#include <errno.h>
extern int   errno;             /* this SHOULD be in errno.h */
extern char *sys_errlist[];     /* this too... */
#endif

/* not everyone has the strerror() function, or so I'm told */
#ifndef VMS
#define ERRSTR(x) sys_errlist[x]
#else
#define ERRSTR(x) strerror(x, vaxc$errno)
#endif




#ifdef VMS   /* VMS config, hacks & kludges */
#define MAXPATHLEN    512
#define popUp xv_popup
#define qsort xv_qsort
#define random rand
#define srandom srand
#define cols xv_cols
#include <errno.h>              /* in VMS they *are* in errno.h */
#include <perror.h>             /* and perror.h */
#endif


#ifndef VMS   /* VMS still hates multi-line '#if's */
/* lots of things don't have <malloc.h> */
/* A/UX systems include it from stdlib, from Xos.h */
# if !defined(ibm032)                    && \
     !defined(__convex__)                && \
     !(defined(vax) && !defined(ultrix)) && \
     !defined(mips)                      && \
     !defined(apollo)                    && \
     !defined(pyr)                       && \
     !defined(__UMAXV__)                 && \
     !defined(bsd43)                     && \
     !defined(macII)                     && \
     !defined(sequent)


#  if defined(hp300) || defined(hp800) || defined(NeXT)
#   include <sys/malloc.h>                /* it's in 'sys' on HPs and NeXT */
#  else
#   include <malloc.h>
#  endif
# endif
#endif /* !VMS */


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/Xmd.h>



#if defined(NEEDSTIME) || defined(NEEDSDIR)
#include <sys/types.h>    /* only include <sys/types.h> once */
#endif

#ifdef NEEDSTIME
#  if defined(SVR4) || defined(macII)
#    include <sys/time.h>
#  else
#    if !defined(sgi) && !defined(__UMAXV__) && !defined(macII)
#      include <sys/timeb.h>
#    endif
#  endif

#  undef SIGCHLD           /* defined in both Xos.h and signal.h */
#  include <signal.h>

#  ifdef sgi               /* need 'CLK_TCK' value for sginap() call */
#    include <limits.h>
#  endif

#  ifndef  sigmask
#    define  sigmask(m)      (1 << ((m)-1))
#  endif

#endif  /* NEEDSTIME */

#ifdef NEEDSDIR
#  ifdef VMS
#    include <descrip.h>
#    include <stat.h>
#    include "dirent.h"
#  else
#    ifdef NODIRENT
#      include <sys/dir.h>
#    else
#      include <dirent.h>
#    endif

#    include <sys/param.h>
#    include <sys/stat.h>

#    if defined(__convex__) && defined (__STDC__)
#      define S_IFMT  _S_IFMT
#      define S_IFDIR _S_IFDIR
#      define S_IFCHR _S_IFCHR
#      define S_IFBLK _S_IFBLK
#    endif
#  endif
#endif


#ifdef NEEDSARGS
#  if defined(__STDC__) && !defined(NOSTDHDRS)
#    include <stdarg.h>
#  else
#    include <varargs.h>
#  endif
#endif


/* signal macros */
#define HOLD_SIG         sigblock(sigmask(SIGALRM))
#define RELEASE_SIG      sigblock(0)
#define PAUSE_SIG        sigpause(0)


/* default for most folks */
#define UNCOMPRESS "/usr/ucb/uncompress"   /* for uncompressing .Z files */

#if defined(hpux) || defined(SVR4) || defined(__386BSD__)
#undef  UNCOMPRESS
#define UNCOMPRESS "/usr/bin/uncompress"   /* for uncompressing .Z files */
#endif


#ifdef VMS
   /* you WILL have to personalize for your own DECUS VMS version of
      the Martin Minow LZDCMP for this to work properly... */
#  undef UNCOMPRESS
#  ifdef HAVE_LZW
#    define UNCOMPRESS "LZDCMP /Export = Unix /Mode = Binary"
#  else
#    define UNCOMPRESS "DECOMPRESS"
#  endif /* HAVE_LZW */
#endif /* VMS */


#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

#ifdef NO_RANDOM
#define random  rand
#define srandom srand
#endif



/*****************************/
/* END OF CONFIGURATION INFO */
/*****************************/




#define PROGNAME  "xv"             /* used in resource database */

#define INFOWIDE 500               /* (fixed) size of info window */
#define INFOHIGH 250

#define CTRLWIDE 440               /* (fixed) size of control window */
#define CTRLHIGH 385

#define MAXNAMES 4096              /* max # of files in ctrlW list */

#define MAXBRWIN   4               /* max # of vis browser windows */

/* strings in the INFOBOX (used in SetISTR and GetISTR) */
#define NISTR         9    /* number of ISTRs */
#define ISTR_INFO     0
#define ISTR_WARNING  1
#define ISTR_FILENAME 2
#define ISTR_FORMAT   3
#define ISTR_RES      4
#define ISTR_CROP     5
#define ISTR_EXPAND   6
#define ISTR_COLOR    7
#define ISTR_COLOR2   8

/* potential values of 'infomode', used in info box drawing routines */
#define INF_NONE 0    /* empty box */
#define INF_STR  1    /* just ISTR_INFO */
#define INF_PART 2    /* filename, format, size and infostr */
#define INF_FULL 3    /* INF_PART + clipping, expansion, colorinfo */


/* buttons in the ctrl window */
#define NBUTTS   34
#define BVBROWSE 0
#define BNEXT    1
#define BPREV    2
#define BLOAD    3
#define BSAVE    4
#define BGAMMA   5
#define BINFO    6
#define BLICENSE 7
#define BTXVIEW  8
#define BCMTVIEW 9
#define BGRAB    10
#define BDELETE  11
#define BQUIT    12
#define BCROP    13
#define BUNCROP  14
#define BACROP   15
#define BNORM    16
#define BMAX     17
#define BMAXPECT 18
#define BUP2     19
#define BDN2     20
#define BSETSIZE 21
#define BASPECT  22
#define B4BY3    23
#define BINTSIZE 24
#define BRAW     25
#define BDITH    26
#define BSMOOTH  27
#define BDN10    28
#define BUP10    29
#define BROTL    30
#define BROTR    31
#define BFLIPH   32
#define BFLIPV   33


/* buttons in the load/save window */
#define S_LOAD_NBUTTS  4
#define S_NBUTTS   5
#define S_BOK      0
#define S_BCANC    1
#define S_BRESCAN  2
#define S_BLOADALL 3
#define S_BOLDSET  3
#define S_BOLDNAM  4


/* buttons in the 'gamma' window */
#define G_NBUTTS   24
#define G_BAPPLY   0
#define G_BNOGAM   1
#define G_BRESET   2
#define G_BCLOSE   3
#define G_BUP_BR   4
#define G_BDN_BR   5
#define G_BUP_CN   6
#define G_BDN_CN   7
#define G_B1       8
#define G_B2       9
#define G_B3       10
#define G_B4       11
#define G_BSET     12
#define G_BUNDO    13
#define G_BREDO    14
#define G_BCOLREV  15
#define G_BRNDCOL  16
#define G_BHSVRGB  17
#define G_BCOLUNDO 18
#define G_BRV      19
#define G_BMONO    20
#define G_BMAXCONT 21
#define G_BGETRES  22
#define G_BHISTEQ  23


/* constants for setting radio buttons in dirW */
#define F_COLORS    0
#define F_FORMAT    1

#define F_FULLCOLOR 0
#define F_GREYSCALE 1
#define F_BWDITHER  2
#define F_REDUCED   3

#define F_GIF       0
#define F_PM        1
#define F_PBMRAW    2
#define F_PBMASCII  3
#define F_XBM       4
#define F_SUNRAS    5
#define F_BMP       6
#define F_PS        7
#define F_IRIS      8

#ifdef HAVE_JPEG
#define F_JPEG      9
#endif

#ifdef HAVE_TIFF
#ifdef HAVE_JPEG
#define F_TIFF      10
#else
#define F_TIFF      9
#endif
#endif


/* return values from ReadFileType()
 * positive values are *definitely* readable formats (HAVE_*** is defined)
 * negative values are random files that XV can't read, but display as
 *   different icons in the visual browser 
 */
#define RFT_ERROR    -1    /* couldn't open file, or whatever... */
#define RFT_UNKNOWN   0
#define RFT_GIF       1
#define RFT_PM        2
#define RFT_PBM       3
#define RFT_XBM       4
#define RFT_SUNRAS    5
#define RFT_BMP       6
#define RFT_UTAHRLE   7
#define RFT_IRIS      8
#define RFT_PCX       9
#define RFT_JFIF     10
#define RFT_TIFF     11
#define RFT_PDSVICAR 12
#define RFT_COMPRESS 13
#define RFT_PS       14

/* definitions for page up/down, arrow up/down list control */
#define LS_PAGEUP   0
#define LS_PAGEDOWN 1
#define LS_LINEUP   2
#define LS_LINEDOWN 3
#define LS_HOME     4
#define LS_END      5


/* values 'epicMode' can take */
#define EM_RAW    0
#define EM_DITH   1
#define EM_SMOOTH 2


/* things EventLoop() can return (0 and above reserved for 'goto pic#') */
#define QUIT     -1    /* exit immediately  */
#define NEXTPIC  -2    /* goto next picture */
#define PREVPIC  -3    /* goto prev picture */
#define NEXTQUIT -4    /* goto next picture, quit if none (used by 'wait') */
#define LOADPIC  -5    /* load 'named' pic (from directory box) */
#define NEXTLOOP -6    /* load next pic, loop if we're at end */
#define DFLTPIC  -7    /* load the default image */
#define DELETE   -8    /* just deleted pic.  load 'right' thing */
#define GRABBED  -9    /* just grabbed a pic.  'load' it up */
#define POLLED   -10   /* polling, and image file has changed... */
#define RELOAD   -11   /* 'reload' interrupt came. be happier about errors */
#define THISNEXT -12   /* load 'current' selection, Next until success */
#define OP_PAGEUP -13  /* load previous page of multi-page document */
#define OP_PAGEDN -14  /* load next page of multi-page document */


/* possible values of 'rootMode' */
#define RM_NORMAL  0     /* default X tiling */
#define RM_TILE    1     /* integer tiling */
#define RM_MIRROR  2     /* mirror tiling */
#define RM_IMIRROR 3     /* integer mirror tiling */
#define RM_CENTER  4     /* modes >= RM_CENTER centered on some sort of bg */
#define RM_CENTILE 4     /* centered and tiled.  NOTE: equals RM_CENTER */
#define RM_CSOLID  5     /* centered on a solid bg */
#define RM_CWARP   6     /* centered on a 'warp-effect' bg */
#define RM_CBRICK  7     /* centered on a 'brick' bg */
#define RM_ECENTER 8     /* symmetrical tiled */
#define RM_ECMIRR  9     /* symmetrical mirror tiled */
#define RM_MAX     RM_ECMIRR


/* values of colorMapMode */
#define CM_NORMAL    0        /* normal RO or RW color allocation */
#define CM_PERFECT   1        /* install own cmap if necessary */
#define CM_OWNCMAP   2        /* install own cmap always */
#define CM_STDCMAP   3        /* use stdcmap */


/* values of haveStdCmap */
#define STD_NONE     0        /* no stdcmap currently defined */
#define STD_111      1        /* 1/1/1 stdcmap is available */
#define STD_222      2        /* 2/2/2 stdcmap is available */
#define STD_666      3        /* 6x6x6 stdcmap is available */
#define STD_332      4        /* 3/3/2 stdcmap is available */


/* values of allocMode */
#define AM_READONLY  0
#define AM_READWRITE 1


/* selections in dispMB */
#define DMB_WINDOW   0
#define DMB_ROOT     1
#define DMB_TILE     2
#define DMB_MIRROR   3
#define DMB_IMIRROR  4
#define DMB_CENTILE  5
#define DMB_CSOLID   6
#define DMB_CWARP    7
#define DMB_CBRICK   8
#define DMB_ECENTER  9
#define DMB_ECMIRR   10
#define DMB_SEP1     11     /* ---- separator */
#define DMB_COLRW    12
#define DMB_SEP2     13     /* ---- separator */
#define DMB_COLNORM  14
#define DMB_COLPERF  15
#define DMB_COLOWNC  16
#define DMB_COLSTDC  17
#define DMB_MAX      18


/* indicies into conv24MB */
#define CONV24_8BIT  0
#define CONV24_24BIT 1
#define CONV24_SEP1  2
#define CONV24_LOCK  3
#define CONV24_SEP2  4
#define CONV24_FAST  5
#define CONV24_SLOW  6
#define CONV24_BEST  7
#define CONV24_MAX   8

/* values 'picType' can take */
#define PIC8  CONV24_8BIT
#define PIC24 CONV24_24BIT

/* indicies into algMB */
#define ALG_NONE  0
#define ALG_SEP1  1  /* separator */
#define ALG_BLUR3 2
#define ALG_BLUR5 3
#define ALG_BLUR7 4
#define ALG_EDGE  5
#define ALG_TINF  6
#define ALG_OIL   7
#define ALG_MAX   8


/* definitions of first char of dirnames[i] (filetype) */
#define C_FIFO  'f'    /* FIFO special file */
#define C_CHR   'c'    /* character special file */
#define C_DIR   'd'    /* directory */
#define C_BLK   'b'    /* block special file */
#define C_LNK   'l'    /* symbolic link */
#define C_SOCK  's'    /* socket */
#define C_REG   ' '    /* regular file */
#define C_EXE   'x'    /* executable file */


/* values used in Draw3dRect() */
#define R3D_OUT 0  /* rect sticks 'out' from screen */
#define R3D_IN  1  /* rect goes 'in' screen */

#define MBSEP "\001"   /* special string for a --- separator in MBUTT */

/* random string-placing definitions */
#define SPACING 3      /* vertical space between strings */
#define ASCENT   (mfinfo->ascent)
#define DESCENT  (mfinfo->descent)
#define CHIGH    (ASCENT + DESCENT)
#define LINEHIGH (CHIGH + SPACING)


#define STDINSTR "<stdin>"


#ifndef MAIN
#define WHERE extern
#else
#define WHERE
#endif

typedef unsigned char byte;

typedef struct { Window win;            /* window ID */
		 int len;               /* length of major axis */
		 int vert;              /* true if vertical, else horizontal */
		 int active;            /* true if scroll bar can do anything*/
		 int min,max;           /* min/max values 'pos' can take */
		 int val;               /* 'value' of scrollbar */
		 int page;              /* amt val change on pageup/pagedown */
		 int tpos;              /* thumb pos. (pixels from tmin) */
		 int tmin,tmax;         /* min/max thumb offsets (from 0,0) */
		 int tsize;             /* size of thumb (in pixels) */
		 u_long fg,bg,hi,lo;    /* colors */
		 void (*drawobj)();     /* redraws obj controlled by scrl*/
		 int uplit, dnlit;      /* true if up&down arrows are lit */
	       } SCRL;

typedef struct { Window win;            /* window ID */
		 int w,h;               /* size of window */
		 int active;            /* true if can do anything*/
		 int min,max;           /* min/max values 'pos' can take */
		 int val;               /* 'value' of dial */
		 int page;              /* amt val change on pageup/pagedown */
		 char *title;           /* title for this guage */
		 char *units;           /* string appended to value */
		 u_long fg,bg,hi,lo;    /* colors */
		 int rad, cx, cy;       /* internals */
		 int bx[4], by[4];      /* more internals */
		 void (*drawobj)();     /* redraws obj controlled by dial */
	       } DIAL;

typedef struct { Window win;            /* parent window */
		 int x,y,w,h;           /* size of button rectangle */
		 int lit;               /* if true, invert colors */
		 int active;            /* if false, stipple gray */
		 int toggle;            /* if true, clicking toggles state */
		 u_long fg,bg,hi,lo;    /* colors */
		 char *str;             /* string in button */
		 Pixmap pix;            /* use pixmap instead of string */
		 int pw,ph;             /* size of pixmap */
		 int style;             /* ... */
		 int fwidth;            /* width of frame */
	       } BUTT;


typedef struct rbutt { Window        win;      /* parent window */
		       int           x,y;      /* position in parent */
		       char         *str;      /* the message string */
		       int           selected; /* selected or not */
		       int           active;   /* selectable? */
		       struct rbutt *next;     /* pointer to next in group */
		       u_long fg,bg,hi,lo;     /* colors */
		     } RBUTT;


typedef struct cbutt { Window        win;      /* parent window */
		       int           x,y;      /* position in parent */
		       char         *str;      /* the message string */
		       int           val;      /* 1=selected, 0=not */
		       int           active;   /* selectable? */
		       u_long fg,bg,hi,lo;     /* colors */
		     } CBUTT;


#define MAXMBLEN 32  /* max # of items in an mbutt */
typedef struct mbutt { Window        win;      /* parent window */
		       int           x,y,w,h;  /* position in parent */
		       char         *title;    /* title string in norm state */
		       int           active;   /* selectable? */
		       char        **list;     /* list of strings in menu */
		       int           nlist;    /* # of strings in menu */
		       byte          flags[MAXMBLEN];
		                               /* checkmarks on items */
		       byte          dim[MAXMBLEN];
		                               /* dim individual choices */
		       Pixmap pix;             /* use pixmap instd of string */
		       int pw,ph;              /* size of pixmap */
		       u_long fg,bg,hi,lo;     /* colors */
		       Window        mwin;     /* popup menu window */
		     } MBUTT;


typedef struct { Window win;            /* window */
		 int x,y,w,h;           /* size of window */
		 u_long fg,bg,hi,lo;    /* colors */
		 char **str;            /* ptr to list of strings */
		 int   nstr;            /* number of strings */
		 int   selected;        /* number of 'selected' string */
		 int   nlines;          /* number of lines shown at once */
		 SCRL  scrl;            /* scrollbar that controls list */
		 int   filetypes;       /* true if filetype icons to be drawn*/
		 int   dirsonly;        /* if true, only dirs selectable */
	       } LIST;


/* info structure filled in by the LoadXXX() image reading routines */
typedef struct { byte *pic;                  /* image data */
		 int   w, h;                 /* size */
		 int   type;                 /* PIC8 or PIC24 */

		 byte  r[256],g[256],b[256];
		                             /* colormap, if PIC8 */

		 int   frmType;              /* def. Format type to save in */
		 int   colType;              /* def. Color type to save in */
		 char  fullInfo[128];        /* Format: field in info box */
		 char  shrtInfo[128];        /* short format info */
		 char *comment;              /* comment text */
		 
		 int   numpages;             /* # of page files, if >1 */
		 char  pagebname[64];        /* basename of page files */
	       } PICINFO;

#define MAX_GHANDS 16   /* maximum # of GRAF handles */

#define N_GFB 6
#define GFB_SPLINE 0
#define GFB_LINE   1
#define GFB_ADDH   2
#define GFB_DELH   3
#define GFB_RESET  4
#define GFB_GAMMA  5

#define GVMAX 8

typedef struct {  Window win;          /* window ID */
		  Window gwin;         /* graph subwindow */
		  int    spline;       /* spline curve or lines? */
		  int    entergamma;   /* currently entering gamma value */
		  int    gammamode;    /* currently using gamma function */
		  double gamma;        /* gamma value (if gammamode) */
		  int    nhands;       /* current # of handles */
		  XPoint hands[MAX_GHANDS];   /* positions of handles */
		  byte   func[256];    /* output function of GRAF */
		  BUTT   butts[N_GFB]; /* control buttons */
		  u_long fg,bg;        /* colors */
		  char   *str;         /* title string */
		  char   gvstr[GVMAX+1];    /* gamma value input string */
		  void   (*drawobj)();
		} GRAF;

typedef struct {  int    spline;
		  int    entergamma;
		  int    gammamode;
		  double gamma;
		  int    nhands;
		  XPoint hands[MAX_GHANDS];
		  char   gvstr[GVMAX+1];
		} GRAF_STATE;


/* MACROS */
#define CENTERX(f,x,str) ((x)-XTextWidth(f,str,strlen(str))/2)
#define CENTERY(f,y) ((y)-((f->ascent+f->descent)/2)+f->ascent)

/* RANGE forces a to be in the range b..c (inclusive) */
#define RANGE(a,b,c) { if (a < b) a = b;  if (a > c) a = c; }

/* PTINRECT returns '1' if x,y is in rect (inclusive) */
#define PTINRECT(x,y,rx,ry,rw,rh) \
           ((x)>=(rx) && (y)>=(ry) && (x)<=(rx)+(rw) && (y)<=(ry)+(rh))

/* MONO returns total intensity of r,g,b components */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)  /*.33R+ .5G+ .17B*/

/* ISPIPE returns true if the passed in character is considered the
   start of a 'load-from-pipe' or 'save-to-pipe' string */
#define ISPIPE(c) ((c)=='!' || (c)=='|')




/* X stuff */
WHERE Display       *theDisp;
WHERE int           theScreen;
WHERE unsigned int  ncells, dispWIDE, dispHIGH, dispDEEP;
WHERE unsigned int  vrWIDE, vrHIGH, maxWIDE, maxHIGH;
WHERE Colormap      theCmap, LocalCmap;
WHERE Window        rootW, mainW, vrootW;
WHERE GC            theGC;
WHERE u_long        black, white, fg, bg, infofg, infobg;
WHERE u_long        hicol, locol;
WHERE u_long        blkRGB, whtRGB;
WHERE Font          mfont, monofont;
WHERE XFontStruct   *mfinfo, *monofinfo;
WHERE Visual        *theVisual;
WHERE Cursor        arrow, cross, tcross, zoom, inviso;
WHERE Pixmap        iconPix, iconmask;
WHERE int           showzoomcursor;


/* XV global vars */
WHERE byte          *pic;                   /* ptr to loaded picture */
WHERE int            pWIDE,pHIGH;           /* size of 'pic' */
WHERE byte           rMap[256],gMap[256],bMap[256];  /* colormap */
WHERE char          *cmd;                   /* program name for printf's */
WHERE int            DEBUG;                 /* print debugging info */
WHERE int            mono;                  /* true if displaying grayscale */
WHERE char           formatStr[80];         /* short-form 'file format' */
WHERE int            picType;               /* CONV24_8BIT,CONV24_24BIT,etc.*/
WHERE char          *picComments;           /* text comments on current pic */

WHERE int            numPages, curPage;     /* for multi-page files */
WHERE char           pageBaseName[64];      /* basename for multi-page files */

WHERE byte          *cpic;         /* cropped version of pic */
WHERE int           cWIDE, cHIGH,  /* size of cropped region */
                    cXOFF, cYOFF;  /* offset of region from 0,0 of pic */

WHERE byte          *epic;         /* expanded version of cpic */
                                   /* points to cpic when at 1:1 expansion */
                                   /* this is converted to 'theImage' */
WHERE int           eWIDE, eHIGH;  /* size of epic */

WHERE byte          *egampic;      /* expanded, gammified cpic 
				      (only used in 24-bit mode) */

WHERE int           p_offx, p_offy;  /* offset of reparented windows */
WHERE int           ch_offx,ch_offy; /* ChngAttr ofst for reparented windows */
WHERE int           kludge_offx,     /* WM kludges for SetWindowPos routine */ 
                    kludge_offy;

WHERE byte           rorg[256], gorg[256], borg[256];  /* ORIGINAL colormap */
WHERE byte           rcmap[256], gcmap[256], bcmap[256]; /*post-cmap-editing*/
WHERE byte           rdisp[256],gdisp[256],bdisp[256];  /* DISPLAYED colors */
WHERE byte           colAllocOrder[256];   /* order to allocate cols */
WHERE unsigned long  freecols[256]; /* list of pixel values to free */
WHERE byte           rwpc2pc[256]; /* mapping of shared pixels in -rw mode */
WHERE int            nfcols;       /* number of colors to free */
WHERE unsigned long  cols[256];    /* maps pic pixel values to X pixel vals */
WHERE int            fc2pcol[256]; /* maps freecols into pic pixel values */
WHERE int            numcols;      /* # of desired colors in picture */

/* Std Cmap stuff */
WHERE byte           stdr[256], stdg[256], stdb[256];  /* std 3/3/2 cmap */
WHERE unsigned long  stdcols[256];                     /* 3/3/2 -> X colors */
WHERE byte           stdrdisp[256], stdgdisp[256], stdbdisp[256];
WHERE unsigned long  stdfreecols[256];   /* list of cols to free on exit */
WHERE int            stdnfcols;          /* # of cols in stdfreecols[] */

/* colormap for 'browser' window */
WHERE byte           browR[256], browG[256], browB[256];  /* used in genIcon */
WHERE unsigned long  browcols[256];   /* maps 3/3/2 colors into X colors */
WHERE int            browPerfect;
WHERE Colormap       browCmap;

WHERE byte           fsgamcr[256]; /* gamma correction curve (for FS dither) */


/* vars that affect how color allocation is done */
WHERE int            allocMode;    /* AM_READONLY, AM_READWRITE */
WHERE int            rwthistime;   /* true if we DID use R/W color cells */
WHERE int            colorMapMode; /* CM_NORMAL, CM_PERFECT, CM_OWMCMAP ... */
WHERE int            haveStdCmap;  /* STD_NONE, STD_222, STD_666, STD_332 */
WHERE int            novbrowse;    /* if true, won't need colors for browser */
WHERE int            defaultCmapMode;  /* last user-selected cmap mode */

WHERE XImage        *theImage;     /* X version of epic */


WHERE int           ncols;         /* max # of (different) colors to alloc */

WHERE char          str[128];      /* dummy string used for error messages */
WHERE char          initdir[MAXPATHLEN];   /* cwd when xv was started */
WHERE char          searchdir[MAXPATHLEN]; /* '-dir' option */
WHERE char          fullfname[MAXPATHLEN]; /* full name of current file */
WHERE char         *winTitle;      /* user-specified mainW title */

WHERE int           bwidth,        /* border width of created windows */
                    fixedaspect,   /* fixed aspect ratio */
                    conv24,        /* 24to8 algorithm to use (CONV24_*) */
                    ninstall,      /* true if using icccm-complaint WM
				      (a WM that will does install CMaps */
                    useroot,       /* true if we should draw in rootW */
		    nolimits,	   /* No limits on picture size */
		    resetroot,     /* true if we should clear in window mode */
                    noqcheck,      /* true if we should NOT do QuickCheck */
                    epicMode,      /* either SMOOTH, DITH, or RAW */
                    autoclose,     /* if true, autoclose when iconifying */
                    polling,       /* if true, reload if file changes */
                    viewonly,      /* if true, ignore any user input */
                    noFreeCols,    /* don't free colors when loading new pic */
                    autoquit,      /* quit in '-root' or when click on win */
                    xerrcode,      /* errorcode of last X error */
                    grabDelay;     /* # of seconds to sleep at start of Grab */

WHERE int           state824;      /* displays warning when going 8->24 */

WHERE float         defaspect,     /* default aspect ratio to use */
                    normaspect;    /* normal aspect ratio of this picture */

WHERE int           crx1, cry1,    /* dimensions of cropping rectangle */
                    crx2, cry2;

WHERE unsigned long rootbg, rootfg;   /* fg/bg for root border */
WHERE int           waitsec;          /* secs btwn pics. -1=wait for event */
WHERE int           waitloop;         /* loop at end of slide show? */
WHERE int           automax;          /* maximize pic on open */
WHERE int           rootMode;         /* mode used for -root images */

WHERE int           nostat;           /* if true, don't stat() in LdCurDir */

WHERE int           ctrlColor;        /* whether or not to use colored butts */

WHERE char         *def_str;          /* used by rd_*() routines */
WHERE int           def_int;
WHERE char         *tmpdir;           /* equal to "/tmp" or $TMPDIR env var */
WHERE Pixmap        gray25Tile,       /* used for 3d effect on 1-bit disp's */
                    gray50Tile;


/* stuff used for 'info' box */
WHERE Window        infoW;
WHERE int           infoUp;       /* boolean:  whether infobox is visible */
WHERE int           infoMode;


/* stuff used for 'ctrl' box */
WHERE Window        ctrlW;
WHERE int           ctrlUp;       /* boolean:  whether ctrlbox is visible */
WHERE char         *namelist[MAXNAMES];  /* list of file names from argv */
WHERE char         *dispnames[MAXNAMES]; /* truncated names shown in listbox */
WHERE int           numnames, curname;
WHERE LIST          nList;
WHERE BUTT          but[NBUTTS];         /* command buttons in ctrl window */
WHERE Pixmap        grayTile;            /* bg pixmap used on 1-bit systems */
WHERE Pixmap        dimStip;             /* for drawing dim things */
WHERE MBUTT         dispMB;              /* display mode menu button */
WHERE int           dispMode;
WHERE MBUTT         conv24MB;            /* 24-to-8 conversion mode mbutt */
WHERE MBUTT         algMB;               /* Algorithms mbutt */


/* stuff used for 'directory' box */
WHERE Window        dirW, dnamW;
WHERE int           dirUp;       /* is dirW mapped or not */
WHERE LIST          dList;       /* list of filenames in current directory */
WHERE BUTT          dbut[S_NBUTTS];
WHERE CBUTT         browseCB, savenormCB, flistCB;


/* stuff used for 'gamma' box */
WHERE Window        gamW;
WHERE int           gamUp;       /* is gamW mapped or not */
WHERE BUTT          gbut[G_NBUTTS];
WHERE int           editColor;   /* currently selected color # */
WHERE int           hsvmode;     /* true if in HSVmode */
WHERE int cellgroup[256], curgroup, maxgroup;  /* grouped colorcell stuff */
WHERE int           cmapInGam;


/* stuff used for 'browse' box */
WHERE int           anyBrowUp;            /* whether *any* browser visible */

/* stuff used for textview windows */
WHERE int           anyTextUp;            /* are any text windows visible? */
WHERE int           commentUp;            /* comment window up? */

/* stuff used for 'ps' box */
WHERE Window        psW;
WHERE int           psUp;       /* is psW mapped, or what? */
WHERE CBUTT         encapsCB, pscompCB;   
WHERE char         *gsDev, *gsGeomStr;
WHERE int           gsRes;


#ifdef HAVE_JPEG
/* stuff used for 'jpeg' box */
WHERE Window        jpegW;
WHERE int           jpegUp;       /* is jpegW mapped, or what? */
#endif


#ifdef HAVE_TIFF
/* stuff used for 'tiff' box */
WHERE Window        tiffW;
WHERE int           tiffUp;       /* is tiffW mapped, or what? */
#endif


#undef WHERE





/* function declarations for externally-callable functions */

#ifdef __STDC__ 
/****************************** XV.C ****************************/
int   ReadFileType(char *);
int   ReadPicFile(char *, int, PICINFO *, int);
int   UncompressFile(char *, char *);
void  KillPageFiles(char *, int);

void NewPicGetColors(int, int);
void FixAspect(int, int *, int *);
void ActivePrevNext(void);
int  DeleteCmd(void);
void StickInCtrlList(void);
void AddFNameToCtrlList(char *, char *);
void ChangedCtrlList(void);
void HandleDispMode(void);
char *lower_str(char *);
int  rd_int(char *);
int  rd_str(char *);
int  rd_flag(char *);
int  rd_str_cl(char *, char *);

/****************************** XVEVENT.C ****************************/
int  EventLoop(void);
int  HandleEvent(XEvent *, int *);
void DrawWindow(int,int,int,int);
void WResize(int, int);
void WRotate(void);
void WCrop(int, int);
void WUnCrop(void);
void GetWindowPos(XWindowAttributes *);
void SetWindowPos(XWindowAttributes *);
void InvCropRect(void);
void SetEpicMode(void);
int  xvErrorHandler(Display *, XErrorEvent *);

/****************************** XVROOT.C ****************************/
void MakeRootPic(void);
void ClearRoot(void);
void SaveRootInfo(void);
void KillOldRootInfo(void);

/*************************** XVMISC.C ***************************/
void StoreDeleteWindowProp(Window);
Window CreateWindow(char *, char *, char *, int, int, 
		    u_long, u_long, int);
void CenterString(Window, char *, int, int);
void ULineString(Window, char *, int, int);
int  StringWidth(char *);
void FakeButtonPress(BUTT *);
void FakeKeyPress(Window, KeySym);
void GenExpose(Window, int, int, int, int);
void DimRect(Window, int, int, int, int, u_long);
void Draw3dRect(Window, int, int, int, int, int, int, u_long, u_long, u_long);
void xvDestroyImage(XImage *);
void SetCropString(int);
void Warning(void);
void FatalError(char *);
void Quit(int);
void LoadFishCursors(void);
void WaitCursor(void);
void SetCursors(int);
char *BaseName(char *);
void DrawTempGauge(Window, int, int, int, int, double, 
		   u_long, u_long, u_long, u_long);
void XVDeletedFile(char *);
void XVCreatedFile(char *);
void xvbcopy(char *, char *, int);
int  xvbcmp(char *, char *, int);
void xvbzero(char *, int);
void Timer(int);

/*************************** XVCOLOR.C ***************************/
void   SortColormap(void);
void   AllocColors(void);
Status xvAllocColor(Display *, Colormap, XColor *);
void   xvFreeColors(Display *, Colormap, u_long *, int, u_long);
void   FreeColors(void);
void   ApplyEditColor(int);
int    MakeStdCmaps(void);
void   MakeBrowCmap(void);
void   ChangeCmapMode(int, int, int);

/*************************** XVIMAGE.C ***************************/
void Resize(int, int);
void GenerateEpic(int, int);
void DoZoom(int, int, int);
void Crop(void);
void UnCrop(void);
void AutoCrop(void);
int  DoAutoCrop(void);
void DoCrop(int, int, int, int);
void Rotate(int);
void DoRotate(int);
void RotatePic(byte *, int, int *, int *, int);
void Flip(int);
void FlipPic(byte *, int, int, int);
void InstallNewPic(void);
void DrawEpic(void);
void KillOldPics(void);
byte *FSDither(byte *, int, int, int, byte *, byte *, byte *, int, int);
void CreateXImage(void);
XImage *Pic8ToXImage(byte *, int, int, u_long *, byte *,byte *,byte *);
XImage *Pic24ToXImage(byte *, int, int);

void Set824Menus(int);
void Change824Mode(int);
void FreeEpic(void);
void InvertPic24(byte *, int, int);

/*************************** XVALG.C ***************************/
void AlgInit(void);
void DoAlg(int);

/*************************** XVSMOOTH.C ***************************/
byte *SmoothResize(byte *, int, int, int, int, byte *, byte *, byte *,
		   byte *, byte *, byte *, int);
byte *Smooth24(byte *, int, int, int, int, int, byte *, byte *, byte *);
byte *DoColorDither(byte *, byte *, int, int, byte *, byte *, byte *, 
		    byte *, byte *, byte *, int);
byte *Do332ColorDither(byte *, byte *, int, int, byte *, byte *, byte *, 
		    byte *, byte *, byte *, int);

/*************************** XV24TO8.C **************************/
void Init24to8(void);
byte *Conv24to8(byte *, int, int, int, byte *, byte *, byte *);
byte *Conv8to24(byte *, int, int, byte *, byte *, byte *);

/**************************** XVCTRL.C **************************/
void CreateCtrl(char *);
void CtrlBox(int);
void RedrawCtrl(int, int, int, int);
int  ClickCtrl(int, int);
void DrawCtrlNumFiles(void);
void DrawCtrlStr(void);
void ScrollToCurrent(LIST *);

void LSCreate(LIST *, Window, int, int, int, int, int, char **, int, 
	      u_long, u_long, u_long, u_long,
	      void (*)(int, SCRL *), int, int);
void LSRedraw(LIST *, int);
int  LSClick (LIST *, XButtonEvent *);
void LSChangeData(LIST *, char **, int);
void LSNewData(LIST *, char **, int);
void LSKey(LIST *, int);


/*************************** XVINFO.C ***************************/
void  CreateInfo(char *);
void  InfoBox(int);
void  RedrawInfo(int, int, int, int);
void  SetInfoMode(int);
#if defined(__STDC__) && !defined(NOSTDHDRS)
void  SetISTR(int, ...);
#else
void  SetISTR();
#endif
char *GetISTR(int);


/**************************** XVDIR.C ***************************/
void CreateDirW(char *);
void DirBox(int);
void RedrawDirW(int,int,int,int);
int  ClickDirW(int, int);
void LoadCurrentDirectory(void);
void GetDirPath(char *);
int  DirCheckCD(void);
void RedrawDDirW(void);
void RedrawDNamW(void);
void SelectDir(int);
void TrackDDirW(int,int);
int  DirKey(int);
int  DoSave(void);
void SetDirFName(char *);
char *GetDirFName(void);
void SetDirRButt(int, int);
int  Globify(char *);
FILE *OpenOutFile(char *);
int  CloseOutFile(FILE *, char *, int);
byte *HandleBWandReduced(int, int *, byte **, byte **, byte **);
void InitPoll(void);
int  CheckPoll(int);
void DIRDeletedFile(char *);
void DIRCreatedFile(char *);


/*************************** XVBROWSE.C ************************/
void CreateBrowse(char *, char *, char *, char *, char *);
void OpenBrowse(void);
void HideBrowseWindows(void);
void UnHideBrowseWindows(void);
void SetBrowseCursor(Cursor);
void KillBrowseWindows(void);
int  BrowseCheckEvent(XEvent *, int *, int *);
int  BrowseDelWin(Window);
void SetBrowStr(char *);
void RegenBrowseIcons(void);
void BRDeletedFile(char *);
void BRCreatedFile(char *);



/*************************** XVTEXT.C ************************/
void CreateTextWins(char *, char *);
void TextView(char *);
void OpenTextView(char *, int, char *, int);

void OpenCommentText(void);
void CloseCommentText(void);
void ChangeCommentText(void);

void ShowLicense(void);

void HideTextWindows(void);
void UnHideTextWindows(void);
void RaiseTextWindows(void);
void SetTextCursor(Cursor);
void KillTextWindows(void);
int  TextCheckEvent(XEvent *, int *, int *);
int  TextDelWin(Window);



/**************************** XVGAM.C **************************/
void CreateGam(char *, double, double, double, double, int);
int  GamCheckEvent(XEvent *);
void GamBox(int);
void NewCMap(void);
void RedrawCMap(void);
void ChangeEC(int);
void ApplyECctrls(void);
void GenerateFSGamma(void);
void DoNorm(void);
void DoHistEq(void);
void GammifyColors(void);
void Gammify1(int);
void rgb2hsv(int, int, int, double *, double *, double *);
void hsv2rgb(double, double, double, int *, int *, int *);

byte *GammifyPic24(byte *, int, int);
void GamSetAutoApply(int);

/*************************** XVSCRL.C ***************************/
void SCCreate  (SCRL *, Window, int, int, int, int, int, int, int, int, 
                      u_long, u_long, u_long, u_long, void (*)(int, SCRL *));
void SCChange  (SCRL *, int, int, int, int, int, int, int, int);
void SCSetRange(SCRL *, int, int, int, int);
int  SCSetVal  (SCRL *, int);
void SCRedraw  (SCRL *);
void SCTrack   (SCRL *, int, int);


/*************************** XVDIAL.C ***************************/
void DCreate  (DIAL *, Window, int, int, int, int, int, int, int, int, 
                      u_long, u_long, u_long, u_long, char *, char *);
void DSetRange(DIAL *, int, int, int, int);
void DSetVal  (DIAL *, int);
void DSetActive(DIAL *, int);
void DRedraw  (DIAL *);
int  DTrack   (DIAL *, int, int);


/**************************** XVBUTT.C ***************************/

void BTCreate(BUTT *, Window, int, int, int, int, char *, 
	      u_long, u_long, u_long, u_long);
void BTSetActive(BUTT *, int);
void BTRedraw(BUTT *);
int  BTTrack (BUTT *);


RBUTT *RBCreate(RBUTT *, Window, int, int, char *, 
		u_long, u_long, u_long, u_long);
void   RBRedraw(RBUTT *, int);
void   RBSelect(RBUTT *, int);
int    RBWhich(RBUTT *);
int    RBCount(RBUTT *);
void   RBSetActive(RBUTT *, int, int);
int    RBClick(RBUTT *, int, int);
int    RBTrack(RBUTT *, int);

void   CBCreate(CBUTT *, Window, int, int, char *, 
		u_long, u_long, u_long, u_long);
void   CBRedraw(CBUTT *);
void   CBSetActive(CBUTT *, int);
int    CBClick(CBUTT *,int,int);
int    CBTrack(CBUTT *);

void   MBCreate(MBUTT *, Window, int, int, int, int, char *, 
		char **, int, u_long, u_long, u_long, u_long);
void   MBRedraw(MBUTT *);
void   MBSetActive(MBUTT *, int);
int    MBClick(MBUTT *, int, int);
int    MBTrack(MBUTT *);


/**************************** XVGRAF.C ***************************/
void   CreateGraf(GRAF *, Window, int, int, u_long, u_long, char *);
void   InitGraf  (GRAF *);
void   RedrawGraf(GRAF *, int);
int    ClickGraf (GRAF *, Window, int, int);
int    GrafKey   (GRAF *, char *);
void   GenerateGrafFunc(GRAF *, int);
void   Graf2Str  (GRAF_STATE *, char *);
int    Str2Graf  (GRAF_STATE *, char *);
void   GetGrafState (GRAF *, GRAF_STATE *);
int    SetGrafState (GRAF *, GRAF_STATE *);
void   InitSpline(int *, int *, int, double *);
double EvalSpline(int *, int *, double *, int, double);


/**************************** XVGIF.C ***************************/
int LoadGIF(char *, PICINFO *);

/*************************** XVGIFWR.C **************************/
int WriteGIF(FILE *, byte *, int, int, int, byte *, byte *, byte *, int, int,
	     char *);

/**************************** XVPM.C ****************************/
int LoadPM(char *, PICINFO *);
int WritePM(FILE *, byte *, int, int, int, byte *, byte *, byte *, int, int,
	    char *);

/**************************** XVPBM.C ***************************/
int LoadPBM(char *, PICINFO *);
int WritePBM(FILE *, byte *, int, int, int, byte *, byte *, byte *, 
	     int, int, int, char *);

/**************************** XVXBM.C ***************************/
int LoadXBM (char *, PICINFO *);
int WriteXBM(FILE *, byte *, int, int, byte *, byte *, byte *, char *);

/**************************** XVSUNRAS.C ***************************/
int LoadSunRas(char *, PICINFO *);
int WriteSunRas(FILE *, byte *, int, int, int, byte *, byte *, byte*, 
		int, int, int);

/**************************** XVBMP.C ***************************/
int LoadBMP(char *, PICINFO *);
int WriteBMP(FILE *, byte *, int, int, int, byte *, byte *, byte *, 
	       int, int);

/**************************** XVRLE.C ***************************/
int LoadRLE(char *, PICINFO *);

/**************************** XVIRIS.C ***************************/
int LoadIRIS(char *, PICINFO *);
int WriteIRIS(FILE *, byte *, int, int, int, byte *, byte *, byte *, 
		int, int);

/**************************** XVPCX.C ***************************/
int LoadPCX(char *, PICINFO *);

/**************************** XVJPEG.C ***************************/
int  LoadJFIF(char *, PICINFO *);
void CreateJPEGW(void);
void JPEGDialog(int);
int  JPEGCheckEvent(XEvent *);
void JPEGSaveParams(char *, int);

/**************************** XVTIFF.C ***************************/
int   LoadTIFF(char *, PICINFO *);
void  CreateTIFFW(void);
void  TIFFDialog(int);
int   TIFFCheckEvent(XEvent *);
void  TIFFSaveParams(char *, int);

/**************************** XVPDS.C ***************************/
int LoadPDS(char *, PICINFO *);

/*************************** XVPS.C ***************************/
void  CreatePSD(char *);
void  PSDialog(int);
int   PSCheckEvent(XEvent *);
void  PSSaveParams(char *, int);
void  PSResize(void);
int   LoadPS(char *, PICINFO *, int);

/*************************** XVPOPUP.C ***************************/
void  CenterMapWindow(Window, int, int, int, int);
int   PopUp(char *, char **, int);
void  ErrPopUp(char *, char *);
int   GetStrPopUp(char *, char **, int, char *, int, char *, int);
void  ClosePopUp(void);
void  OpenAlert(char *);
void  CloseAlert(void);
int   PUCheckEvent(XEvent *);
void  TextRect(Window, char *, int, int, int, int, u_long);

/*************************** XVDFLT.C ***************************/
void LoadDfltPic(PICINFO *);
void xbm2pic(char *, int, int, byte *, int, int, int, int, int);

/**************************** XVGRAB.C ***************************/
int Grab(void);
int LoadGrab(PICINFO *);




#else     /* using non-ANSI cc.  Function defs, but no params */




/****************************** XV.C ****************************/
int  ReadFileType();
int  ReadPicFile();
int  UncompressFile();
void KillPageFiles();

void NewPicGetColors();
void FixAspect(), StickInCtrlList(), HandleDispMode();
void ActivePrevNext(), AddFNameToCtrlList(), ChangedCtrlList();
int  DeleteCmd(), rd_int(), rd_str(), rd_flag(), rd_str_cl();
char *lower_str();

/****************************** XVEVENT.C ****************************/
int  EventLoop(), HandleEvent();
void DrawWindow(), WResize(), WRotate(), WCrop(), WUnCrop();
void GetWindowPos(), SetWindowPos(), InvCropRect(), SetEpicMode();
int  xvErrorHandler();

/****************************** XVROOT.C ****************************/
void MakeRootPic(), ClearRoot(), KillOldRootInfo(), SaveRootInfo();

/*************************** XVMISC.C ***************************/
void   StoreDeleteWindowProp();
Window CreateWindow();
int    StringWidth();
void   CenterString(), ULineString(), FakeButtonPress(), FakeKeyPress();
void   GenExpose();
void   DimRect(), SetCropString(), Warning(), FatalError(), Quit();
void   LoadFishCursors(), WaitCursor(), SetCursors(), Timer();
void   xvDestroyImage(), Draw3dRect();
char  *BaseName();
void   DrawTempGauge(), XVDeletedFile(), XVCreatedFile(), xvbcopy(), xvbzero();
int    xvbcmp();

/*************************** XVCOLOR.C ***************************/
Status xvAllocColor();
void   SortColormap(), AllocColors();
void   xvFreeColors(), FreeColors(), ApplyEditColor();
int    MakeStdCmaps();
void   MakeBrowCmap();
void   ChangeCmapMode();

/*************************** XVIMAGE.C ***************************/
void Resize(),GenerateEpic(),DoZoom(), Crop(), UnCrop(), AutoCrop();
void DoRotate(), DoCrop(), RotatePic();
int  DoAutoCrop();
void Rotate(), Flip(), FlipPic(), InstallNewPic(), DrawEpic();
void KillOldPics(), CreateXImage();
byte *FSDither();
XImage *Pic8ToXImage();
XImage *Pic24ToXImage();

void Set824Menus(), Change824Mode();
void FreeEpic();
void InvertPic24();

/*************************** XVALG.C ***************************/
void AlgInit(), DoAlg();

/*************************** XVSMOOTH.C ***************************/
byte *SmoothResize(), *Smooth24(), *DoColorDither(), *Do332ColorDither();

/*************************** XV24TO8.C **************************/
void Init24to8();
byte *Conv24to8();
byte *Conv8to24();

/**************************** XVCTRL.C **************************/
void CreateCtrl(), CtrlBox(), RedrawCtrl(), DrawCtrlStr(), ScrollToCurrent();
void DrawCtrlNumFiles();
int  ClickCtrl();

void LSCreate(), LSRedraw(), LSChangeData(), LSNewData(), LSKey();
int  LSClick();

/*************************** XVINFO.C ***************************/
void  CreateInfo(), InfoBox(), RedrawInfo(), SetInfoMode(), SetISTR();
char *GetISTR();

/**************************** XVDIR.C ***************************/
void CreateDirW(), DirBox(), RedrawDirW(), LoadCurrentDirectory();
void GetDirPath();
int  ClickDirW(), DoSave(), DirKey(), DirCheckCD();
void RedrawDDirW(), RedrawDNamW(), SelectDir(), TrackDDirW();
void SetDirFName(), SetDirRButt();
char *GetDirFName();
int  Globify(), CloseOutFile();
FILE *OpenOutFile();
byte *HandleBWandReduced();
void InitPoll();
int  CheckPoll();
void DIRDeletedFile(), DIRCreatedFile();


/*************************** XVBROWSE.C ************************/
void CreateBrowse(), BrowseBox(), ResizeBrowse(), SetBrowStr();
int  BrowseCheckEvent();
void RescanBrowse(), RegenBrowseIcons();

void CreateBrowse(), OpenBrowse(), HideBrowseWindows(), UnHideBrowseWindows();
void SetBrowseCursor(), KillBrowseWindows();
int  BrowseCheckEvent();
int  BrowseDelWin();
void SetBrowStr(), RegenBrowseIcons();
void BRDeletedFile();
void BRCreatedFile();


/*************************** XVTEXT.C ************************/
void CreateTextWins(), TextView(), OpenTextView(), HideTextWindows();
void OpenCommentText(), CloseCommentText(), ChangeCommentText();
void ShowLicense();
void UnHideTextWindows(), SetTextCursor(), KillTextWindows();
void RaiseTextWindows();
int  TextCheckEvent(), TextDelWin();



/**************************** XVGAM.C **************************/
void CreateGam();
int  GamCheckEvent();
void GamBox(), NewCMap(), RedrawCMap();
void ChangeEC(), ApplyECctrls();
void GenerateFSGamma(), DoNorm(), DoHistEq(), GammifyColors();
void Gammify1(), rgb2hsv(), hsv2rgb();

byte *GammifyPic24();
void GamSetAutoApply();

/*************************** XVSCRL.C ***************************/
void SCCreate(), SCChange(), SCSetRange(), SCRedraw(), SCTrack();
int SCSetVal();

/*************************** XVDIAL.C ***************************/
void DCreate(), DSetRange(), DSetVal(), DRedraw(), DSetActive();
int  DTrack();

/**************************** XVBUTT.C ***************************/
void BTCreate(), BTSetActive(), BTRedraw();
int  BTTrack();

RBUTT *RBCreate();
void   RBRedraw(), RBSelect(), RBSetActive();
int    RBWhich(), RBCount(), RBClick(), RBTrack();

void CBCreate(), CBRedraw(), CBSetActive();
int  CBClick(), CBTrack();

void MBCreate(), MBRedraw(), MBSetActive();
int  MBClick(), MBTrack();


/*************************** XVGRAF.C ***************************/
void   CreateGraf(), InitGraf(), RedrawGraf(), GenerateGrafFunc();
void   Graf2Str(), GetGrafState(), InitSpline();
int    ClickGraf(), GrafKey(), Str2Graf(), SetGrafState();
double EvalSpline();


/**************************** XVGIF.C ***************************/
int LoadGIF();

/*************************** XVGIFWR.C **************************/
int WriteGIF();

/**************************** XVPM.C ****************************/
int LoadPM();
int WritePM();

/**************************** XVPBM.C ***************************/
int LoadPBM();
int WritePBM();

/**************************** XVXBM.C ***************************/
int LoadXBM();
int WriteXBM();

/**************************** XVSUNRAS.C ***************************/
int LoadSunRas();
int WriteSunRas();

/**************************** XVBMP.C ***************************/
int LoadBMP();
int WriteBMP();

/**************************** XVRLE.C ***************************/
int LoadRLE();

/**************************** XVIRIS.C ***************************/
int LoadIRIS();
int WriteIRIS();

/**************************** XVPCX.C ***************************/
int LoadPCX();

/**************************** XVJPEG.C ***************************/
int  LoadJFIF();
int  JPEGCheckEvent();
void CreateJPEGW(), JPEGDialog(), JPEGSaveParams();

/**************************** XVTIFF.C ***************************/
int  LoadTIFF();
void CreateTIFFW(), TIFFDialog(), TIFFSaveParams();
int  TIFFCheckEvent();

/*************************** XVPS.C ***************************/
void  CreatePSD(), PSDialog(), PSSaveParams(), PSResize();
int   PSCheckEvent();
int   LoadPS();

/*************************** XVPOPUP.C ***************************/
void CenterMapWindow(), ErrPopUp(), ClosePopUp(), OpenAlert(), CloseAlert();
int  PopUp(), PUCheckEvent(), GetStrPopUp();
void TextRect();

/*************************** XVDFLT.C ***************************/
void LoadDfltPic();
void xbm2pic();

/**************************** XVGRAB.C ***************************/
int Grab();
int LoadGrab();

/**************************** XVPDS.C ***************************/
int LoadPDS();

#endif
