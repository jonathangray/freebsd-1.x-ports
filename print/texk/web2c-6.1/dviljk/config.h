#define USEPXL

#define MAKETEXPK /* always enable MakeTeXPK */

/* 
 * if your LaserJet II P or LaserJet III or LaserJet 2000
 * complains about not enough memory, you might try to reduce 
 * the value below or uncomment to use the default settings
 */  
#ifdef LJ2P
#define  MAX_FONTS_PER_PAGE 255         /* maximum number of fonts per page */
#endif


/* Timing is not very portable.... if you have troubles, use
 * -DNO_TIMING in the Makefile
 */

#ifdef u3b2
#define NO_TIMING
#endif
#ifdef _AMIGA
#define NO_TIMING
#endif


/*
 * assure that LJ2P is defined when LJ4 is defined;
 * compile with support for LJ4's resident fonts
 */
#ifdef LJ4
#define LJ2P
#define LJ_RESIDENT_FONTS
#endif

/*
 * assure that LJ2 is defined when LJ2P is defined
 */
#ifdef LJ2P
#ifndef LJ2
#define LJ2
#endif
#endif

/*
 * assure that LJ is defined when LJ2 of LJ4 is defined
 */
#if defined(LJ2)
#ifndef LJ
#define LJ
#endif
#endif

/*
 * assure that IBM3812 is not defined when LJ is defined
 */ 
#ifdef LJ
#ifdef IBM3812
#undef IBM3812
#endif
#endif



#define  UNKNOWN     -1

#define  STRSIZE         255     /* stringsize for file specifications  */

typedef  char    bool;

#ifdef hpux
typedef  char    signed_char;
#else
#ifdef vms
typedef  char    signed_char;
#else
#ifdef u3b2
typedef  short signed_char;
#else
#if __STDC__
/* this is the default !! */
typedef  signed char    signed_char; 
#else
typedef  char           signed_char;
#endif /* not __STDC__ */
#endif 
#endif
#endif

#if !defined(u3b2) && !defined(LONG_64_BITS)
#define  ARITHMETIC_RIGHT_SHIFT
#endif

#ifdef LONG_64_BITS
#define long int
#endif

bool findfile();


/* 
 * maximal number of characters in font file
 * #define  LASTFNTCHAR  127        7-bit classic version
 * #define  LASTFNTCHAR  255        8-bit fonts
 */

#ifdef SEVENBIT 
#define LASTFNTCHAR 127
#else
#define LASTFNTCHAR  255
#endif



/* this information is needed in findfile.c and dvi2xx.c, NO CUSTOMIZATION */
#ifdef LJ
#ifdef LJ4
#define RESOLUTION    600
#ifndef MFMODE
#define MFMODE "ljfour"   /* mode definition for metafont */
#endif
#else
#define RESOLUTION    300
#ifndef MFMODE
#define MFMODE "imagen"    /* mode definition for metafont */
#endif
#endif
#endif

#ifdef IBM3812
#define RESOLUTION    240
#ifndef MFMODE
#define MFMODE "IBMThreeEightOneTwo"    /* mode definition for metafont */
#endif
#endif


#ifdef unix
#define OS "Unix"
#define READ_BINARY     "r"
#define WRITE_BINARY    "w"
#define labs(x) abs(x)
#endif
#ifdef MSDOS
#define OS "MS-DOS"
#define READ_BINARY     "rb"
#define WRITE_BINARY    "wb"
#define MSC5
#endif
#ifdef OS2
#define OS "OS/2"
#define READ_BINARY     "rb"
#define WRITE_BINARY    "wb"
#define MSC5
#endif

#ifdef vms
#define OS "VMS"
#include <ssdef.h>
#include <stsdef.h>
#define ftell vms_ftell		    /* use some external routines, because */
#define fseek vms_fseek		    /* of some bugs in the VMS run time    */
#define getchar vms_getchar	    /* library */
#define getenv vms_getenv
#define ungetc vms_ungetc
#define getname vms_getname
#define READ_BINARY     "rb"
#define WRITE_BINARY    "wb","rfm=fix","bls=512","mrs=512" /* fixed records */
#define labs(x) abs(x)
#endif

#ifdef _AMIGA
#define OS "Amiga"
#define READ_BINARY     "r"
#define WRITE_BINARY    "w"
#ifdef __SASC
#define sys_errlist __sys_errlist
#include <stdlib.h>
#endif
#endif


/* Information returned by tfm_read_info. */
typedef struct {
  /* These string lengths are imposed by the TFM format. Either of these
     values may be the empty string.  */
  char coding_scheme[40];
  char family[20];
   
  /* The second fontdimen. */
  unsigned interword;
   
  /* These values are what will work to select the font in PCL. If this
     TFM file doesn't have the `KN' extensions (distinguishable by the
     family == "HPAUTOTFM"). */
#define SPACING_FIXED 0
#define SPACING_PROPORTIONAL 1
  unsigned spacing;
  int weight;
  unsigned style;
  unsigned typeface_id;

  /* TFM files can always have 256 characters, even if we're using the
     old pixel format that only supports 128. The values are fix-words
     scaled by the design size; i.e., straight from the TFM file. */
  long widths[256];
} tfm_info_type;
