#define USEPXL

/*
 *  default font path
 *  can be overridden by environment variable TEXPXL
 *  or -a option
 */
#define  FONTAREA       "/usr/local/lib/tex/fonts/pk"

/* 
 * if your LaserJet II P or LaserJet III or LaserJet 2000
 * complains about not enough memory, you might try to recompile the
 * program with the following lines commented out
 */  
#ifdef LJ2P
#define LJ_LARGE_FONT_MEMORY
#endif

/* unix user: remove the following definition if you cannot access the
 * appropriate C library functions
 */
/*#define TIMING*/


/*
 * assure that LJ is defined when LJ2P is defined
 */
#ifdef LJ2P
#ifndef LJ
#define LJ
#endif
#endif


/*
 * assure that IBM3812 is not defined when LJ2 is defined
 */ 
#ifdef LJ
#ifdef IBM3812
#undef IBM3812
#endif
#endif


#define  TRUE      (bool) 1
#define  FALSE     (bool) 0
#define  UNKNOWN     -1

#define  STRSIZE         255     /* stringsize for file specifications  */

typedef  char    bool;

#ifdef hpux
typedef  char    signed_char;
#else
typedef  signed char    signed_char;
#endif

bool findfile();


/* 
 * maximal number of characters in font file
 * #define  LASTFNTCHAR  127        7-bit classic version
 * #define  LASTFNTCHAR  255        8-bit fonts
 */

#ifdef SEVENBIT 
#define LASTFNTCHAR 127
#define VIS   33
#define VIS2  (VIS+32)
unsigned char
VisChar(c)
unsigned char   c;
{
    c &= 0xff;
    if (c < VIS)
        return ((unsigned char)(160 + c));
    if (c < 128)
        return (c);
    if (c < (255 - VIS2))
        return ((unsigned char)(VIS2 + c));
    return (255);
}
#else
#define  LASTFNTCHAR  255
#define VisChar(c) (unsigned char)(c)
#endif







