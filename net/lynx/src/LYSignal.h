
#ifndef LYSIGNAL_H
#define LYSIGNAL_H

#include <signal.h>

#ifdef VMS
extern void *vsignal PARAMS((int sig, void (*func)()));
#define signal(a,b) vsignal(a,b) /* use termio.c routines for interrupts */
#endif

#endif /* LYSIGNAL_H */
