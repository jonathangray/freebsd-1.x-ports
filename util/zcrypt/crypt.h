/*
   crypt.h (full version) by Info-ZIP.       Last revised:  23 Oct 92

   This header file is not copyrighted and may be distributed without
   restriction.
 */

#ifndef __crypt_h   /* don't include more than once */
#define __crypt_h

#ifndef CRYPT
#  define CRYPT     /* full version */
#endif

#define PWLEN  80   /* input buffer size for reading encryption key */
#define RAND_HEAD_LEN  12    /* length of encryption random header */

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

#ifdef UTIL
   int zipcloak OF((struct zlist far *, FILE *, FILE *, char *));
   int zipbare OF((struct zlist far *, FILE *, FILE *, char *));
#else /* !UTIL */
   unsigned zfwrite OF((voidp *, extent, extent, FILE *));
   extern char *key;
#endif /* ?UTIL */

#ifdef VMS
#  define echoff(f)  echo(0)
#  define echon()    echo(1)
   int echo OF((int));
#else
   void echoff OF((int));
   void echon OF((void));
#endif

#if (defined(MSDOS) || defined(OS2)) && (!defined(DOS_OS2))
#  define DOS_OS2
#endif

#ifdef TOPS20
#  define decrypt_member dcrmem
#  define decrypt_byte   dcrbyt
#endif

#endif /* !__crypt_h */
