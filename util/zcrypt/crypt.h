/*
   crypt.h (full version) by Info-ZIP.       Last revised:  6 Sep 93

   This header file is not copyrighted and may be distributed without
   restriction.
 */

#ifndef __crypt_h   /* don't include more than once */
#define __crypt_h

#ifndef CRYPT
#  define CRYPT     /* full version */
#endif

/* Hacks for compatibility with unzip 5.0p1 and unzip 5.1f.
 * zip.h in unzip 5.0p1 defines SKIP_TIME_H.
 */
#if defined(SKIP_TIME_H) /* unzip 5.0p1 */
#  define UNZIP
#  define UNZIP50P1
#  undef crc32
#  define CRC32(c, b) (crc_32_tab[((int)(c) ^ (b)) & 0xff] ^ ((c) >> 8))
#endif

#ifndef PWLEN
#  define PWLEN  80   /* input buffer size for reading encryption key */
#endif
#ifndef RAND_HEAD_LEN
#  define RAND_HEAD_LEN  12    /* length of encryption random header */
#endif

/* encode byte c, using temp t.  Warning: c must not have side effects. */
#define zencode(c,t)  (t=decrypt_byte(), update_keys(c), t^(c))

/* decode byte c in place */
#define zdecode(c)   update_keys(c ^= decrypt_byte())

int  decrypt_byte OF((void));
void update_keys OF((int c));
void init_keys OF((char *passwd));
void crypthead OF((char *, ulg, FILE *));
char *getp OF((char *m, char *p, int n));
int  decrypt_member OF((void));
/* zip.h in unzip 5.1 may have: #define decrypt_member decrypt
 * to shorten the names. Can't do it yet because of conflict with
 * decrypt variable in funzip.
 */

#ifdef UTIL
   int zipcloak OF((struct zlist far *, FILE *, FILE *, char *));
   int zipbare OF((struct zlist far *, FILE *, FILE *, char *));
#else /* !UTIL */
   unsigned zfwrite OF((voidp *, extent, extent, FILE *));
   extern char *key;
   extern int newzip;   /* for unzip */
#endif /* ?UTIL */

#ifdef VMS
#  define echoff(f)  echo(0)
#  define echon()    echo(1)
   int echo OF((int));
#else
   void echoff OF((int));
   void echon OF((void));
#endif

#if defined(MSDOS) || defined(OS2) || defined(__human68k__)
#  ifndef DOS_OS2
#    define DOS_OS2
#  endif
#endif

#ifdef TOPS20
#  define decrypt_member dcrmem
#  define decrypt_byte   dcrbyt
#endif

#ifdef AMIGA
#  ifndef SIGBREAKF_CTRL_C
#    define SIGBREAKF_CTRL_C (1L << 12)
#  endif
#  ifdef UNZIP
     void Signal(void *, long), *FindTask(void *);
#  endif
   /* Note: getpid() is only used for random number seeding */
#  define getpid()  (long) FindTask(NULL)       /* more secure than pi */
#  ifdef __SASC_60
#    define echoff(f)  /* rawcon(1) */
#    define echon()    /* rawcon(0) */
#    define ECHO_NEWLINE()   putc('\n', stderr)
#  else
#    ifdef AZTEC_C
#      define getch()        getchar()
#      define echon()        set_con()
#      define echoff(f)      set_raw()
#      define ECHO_NEWLINE() putc('\n', stderr)
#  else
#    define getch()    getchar()
#      define echoff(f)      { fputs("\033[30;40m",stderr);fflush(stderr); }
#      define echon()        { fputs("\033[31;40m",stderr);fflush(stderr); }
#      define ECHO_NEWLINE()
#    endif
#  endif
#endif

#endif /* !__crypt_h */
