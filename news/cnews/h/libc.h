#ifndef LIBC_H
#define LIBC_H
/*
 * declarations of (supposedly) standard C library functions and types.
 * we don't declare functions that once returned int but may now return void
 * to avoid fatal but spurious compilation errors.  VOID is an attempt to deal
 * with this transition.
 *
 * The function declarations need to be prototyped to give ansi compilers
 * less gastric distress.
 */

#ifndef VOID
#define VOID void
#endif

/* Unix system calls */
#ifdef A_STABLE_WORLD
extern VOID _exit();
extern int alarm();			/* really unsigned? */
#endif					/* A_STABLE_WORLD */
#ifdef __STDC__
extern int access(char *, int), chown(char *, int, int), fork(void);
extern int symlink(char *, char *);
extern int wait(int *);
extern int getuid(void), geteuid(void), getgid(void), getegid(void);
extern int setuid(int), setgid(int);
extern int execv(char *, char **), execl(char *, char *, ...);
extern int execve(char *, char **, char **), execle(char *, char *, ...);
extern int gethostname(char *, int);
#else
extern int symlink();
extern int getuid(), geteuid(), getgid(), getegid();
extern int setuid(), setgid();
extern int execv(), execl(), execve(), execle();
extern int gethostname();
#endif
extern time_t time();			/* sys/timeb.h? */

extern int errno;			/* errno.h */
extern char **environ;

#include <string.h>

#ifdef A_STABLE_WORLD
extern int fflush(), fputs(), ungetc();	/* stdio.h */
extern int fread(), fwrite(), fseek();	/* stdio.h */
extern int pclose();			/* stdio.h */
extern VOID rewind();			/* stdio.h */
extern VOID exit();			/* stdio.h */
#endif					/* A_STABLE_WORLD */
extern FILE *popen();			/* stdio.h */
/* stdio.h is supposed to declare *printf */

/* these unfortunately cannot be relied upon to be in the right header */
extern struct passwd *getpwnam();	/* pwd.h */
extern struct group *getgrnam();	/* grp.h */
extern char *ctime();			/* time.h */

extern long atol();
extern char *mktemp();
extern char *getenv();

#ifdef __STDC__
extern int putenv(const char *);
extern int getopt(int, char * const *, const char *);
#else
extern int putenv();
extern int getopt();
#endif					/* __STDC__ */
extern int optind;
extern char *optarg;

#include "alloc.h"			/* ugh */
#endif					/* LIBC_H */
