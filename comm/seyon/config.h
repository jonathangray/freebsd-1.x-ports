/*
 *
 *   This file is part of the Seyon, Copyright (c) 1992-1993 by
 *   Muhammad M. Saggaf.
 *
 *   See the file COPYING (0-COPYING) for the full copyright notice
 *   of this program.
 *
 */

#ifndef SEYON_CONFIG_H
#define SEYON_CONFIG_H

/*
 * If you are running one of the systems in the next paragraph,
 * skip system configuration and go directly to user configuration
 * after defining the appropriate symbol below if your system
 * does't automatically do that. I've already setup the necessary
 * definitions for you. I've also put user configuration definiftions
 * for some of the systems, check those at the end of system configuraion
 */

/*
 * If your system is not listed below, and you'd like to me to put
 * an entry for it so that you won't have to edit this file in the
 * next verion, send me the keyword your system uses (e.g. linux,
 * SVR4, ultrix, ..etc.) and the changes you made, and I'll make an
 * entry for your system.
 */

/* #define linux */               /* No need, predefined */
/* #define SVR4 */                /* No need, predefined */
/* #define _SVR3 */
/* #define BSD386 */
/* #define __386BSD__ */          /* No need, predefined */
/* #define ultrix */              /* No need, predefined */
/* #define sun */                 /* No need, predefined */
/* #define SUNOS_3 */		      /* SunOS 3.x */
/* #define HPUX */                /* HP-UX 8.0 */
/* #define HPUX9 */               /* HP-UX 9.0 */
/* #define AIXV3 */
/* #define SGI */

/* Sometimes __svr4__ is defined but not SVR4 */
#if !defined(SVR4) && defined(__svr4__)
#define SVR4
#endif

/* This is SunOS 4.x, not 3.x, and not Solaris 2.x */
#if defined(sun) && !defined(SUNOS_3) && !defined(SVR4)
#ifndef SUNOS_4
#define SUNOS_4
#endif			
#endif

#if (defined(hpux) || defined (HPUX9)) && !defined(HPUX)
#define HPUX
#endif

/* Leave these alone */

#define YES  1
#define Yes  YES
#define yes  YES
#define NO   0
#define No   NO
#define no   NO

/*
 * ----------------------------------------
 *   Check those out: 
 * ----------------------------------------
 */

#if defined(linux) || defined(_SVR3) || defined(BSD386) || defined(__386BSD__)
#define HAVE_TERMIOS        YES
#define HAVE_TERMIO         NO
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       YES
#define HAVE_USLEEP         YES
#define LF_PATH             "/var/spool/lock"
#define LF_PREFIX           "LCK.."
#endif

#ifdef SVR4
#define HAVE_TERMIOS        YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       YES
#define HAVE_USLEEP			NO	/* usleep is broken in SVR4 */
#define HAVE_SELECT         YES
#define LF_USE_ASCII_PID    YES
#define LF_USE_DEV_NUMBERS  YES
#define LF_PATH             "/usr/spool/locks"
#define LF_PREFIX           "LK."
#endif

#ifdef ultrix
#define HAVE_TERMIOS        YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       YES
#define HAVE_USLEEP			NO
#define HAVE_SELECT         YES
#endif

#ifdef SUNOS_4
#define HAVE_TERMIOS        YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       NO
#define HAVE_USLEEP			YES
#define LF_USE_ASCII_PID    YES
#define LF_USE_DEV_NUMBERS  NO
#define LF_PATH             "/var/spool/locks"
#define LF_PREFIX           "LCK.."
#endif

#ifdef SUNOS_3
#define HAVE_TERMIOS        NO
#define HAVE_TERMIO         NO
#define HAVE_SGTTYB         YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         NO
#define HAVE_STRERROR       NO
#define HAVE_USLEEP			YES
typedef int pid_t;
typedef int speed_t;
#define LF_USE_ASCII_PID    YES
#define LF_USE_DEV_NUMBERS  NO
#define LF_PATH             "/var/spool/locks"
#define LF_PREFIX           "LCK.."
#endif

#ifdef HPUX
#ifndef HPUX9
#define HAVE_TERMIOS        NO
#define HAVE_TERMIO         YES
#else
#define HAVE_TERMIOS        YES
#endif
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       YES
#define HAVE_USLEEP			NO
#define HAVE_SELECT         YES
#endif

#ifdef AIXV3
#define HAVE_TERMIOS        YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       YES
#define HAVE_USLEEP         YES
#define LF_USE_ASCII_PID    YES
#define LF_USE_DEV_NUMBERS  NO
#define LF_PATH             "/etc/locks"
#define LF_PREFIX           "LCK.."
#endif

#ifdef SGI
#define HAVE_TERMIOS        YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         YES
#define HAVE_STRERROR       YES
#define HAVE_USLEEP			NO
#define HAVE_SELECT         YES
#define LF_USE_ASCII_PID    NO
#define LF_USE_DEV_NUMBERS  NO
#define LF_PATH             "/usr/spool/locks"
#define LF_PREFIX           "LCK.."
#endif

#ifdef Mips
#define HAVE_TERMIOS        NO
#define HAVE_TERMIO         NO
#define HAVE_SGTTYB         YES
#define HAVE_MODEM_CONTROL  YES
#define HAVE_DUP2           YES
#define HAVE_STRSTR         NO
#define HAVE_STRERROR       NO
#define HAVE_USLEEP         YES
#define HAVE_SELECT         YES
#define LF_USE_ASCII_PID    NO
#define LF_USE_DEV_NUMBERS  NO
#define LF_PATH             "/usr/spool/locks"
#define LF_PREFIX           "LCK.."
#define B19200              16
#define B38400              17

typedef int pid_t;
typedef unsigned short speed_t;

extern char *strstr();
extern char *strrchr();
extern char *calloc();
extern char *getenv();
#endif

/*
 * ----------------------------------------
 *   System configuration starts here
 * ----------------------------------------
 */

/* Wether you have POSIX termios */
#ifndef HAVE_TERMIOS
#define HAVE_TERMIOS        YES
#endif

/* Wether you have SYSV termio */
#ifndef HAVE_TERMIO
#define HAVE_TERMIO         NO
#endif

/* Wether you have SGTTYB */
#ifndef HAVE_SGTTYB
#define HAVE_SGTTYB         NO
#endif

/* Wether you have modem line control. Almost all Unix systems have it. */
#ifndef HAVE_MODEM_CONTROL
#define HAVE_MODEM_CONTROL  YES
#endif

/* Wether you have dup2 */
#ifndef HAVE_DUP2
#define HAVE_DUP2           YES
#endif

/* Wether you have strstr */
#ifndef HAVE_STRSTR
#define HAVE_STRSTR         YES
#endif

/* Wether you have strerror */
#ifndef HAVE_STRERROR
#define HAVE_STRERROR       YES
#endif

/* Wether you have usleep */
#ifndef HAVE_USLEEP
#define HAVE_USLEEP         YES
#endif

/* Wether your system has select. If not (highly unlikely), your
   system is not worth calling unix and usleep will be implemented by
   busy waiting. This is only needed if you don't have usleep. */
#ifndef HAVE_SELECT
#define HAVE_SELECT         YES
#endif

/* Uncomment if pids are defined as int in your system */
/* typedef int pid_t; */

/* Uncomment if speed_t not defined in your system */
/* typedef int speed_t; */

/* Uncomment if you don't have strchr and strrchr */
/* 
#define strchr  index
#define strrchr rindex
*/

/*
 * ----------------------------------------
 *   System configuration ends here
 * ----------------------------------------
 */

/*
 * ----------------------------------------
 *   User configuration starts here
 * ----------------------------------------
 */

/*
 * For those, better not edit them directly. Make an entry for your
 * system like the above and put the definitions there, or modify an
 * existing one. If you want to edit them here, make sure they are not
 * defined for your system above, if your system has an entry.
 */

/*
 * Wether you want pids written to lock files as ascii strings (as
 * HDB uucp does) rather than in binary form as other uucp prgrams do
 */
#ifndef LF_USE_ASCII_PID
#define LF_USE_ASCII_PID    NO
#endif

/* 
 * Wether you want lock files to be in the form LK.inode.major.minor
 * (e.g. LK.035.064.008) rather than the more common LCK..base_name
 * (e.g. LCK..ttys0). This is mostly how SVR4 does things.
 */
#ifndef LF_USE_DEV_NUMBERS
#define LF_USE_DEV_NUMBERS  NO
#endif

/* 
 * Define the location of your locks.
 */

#ifndef LF_PATH
#define LF_PATH             "/usr/spool/uucp"
#endif

/* 
 * Normally, you don't have to touch this
 */
#ifndef LF_PREFIX
#if !LF_USE_DEV_NUMBERS
#define LF_PREFIX           "LCK.."
#else
#define LF_PREFIX           "LK."
#endif
#endif /* LF_PREFIX */

/*
 * Wether you want to use speeds not supported by termios/termio
 * (57600 and 115200 bps). This option is currently available to
 * linux only (you need kernel 0.99.3+) since it is O/S-dependent.
 */
#ifdef linux
#ifndef USE_NONSTD_BAUD
#define USE_NONSTD_BAUD     NO
#endif
#endif /* linux */

/*
 * ----------------------------------------
 *   User configuration ends here
 * ----------------------------------------
 */

/* 
 * Don't touch this
 */

#if HAVE_TERMIOS || HAVE_TERMIO
#define HAVE_SGTTYB         NO
#endif

#if !defined(SIGCLD) && defined(SIGCHLD)
#define SIGCLD SIGCHLD
#endif

#define NMSIZE  256		/* file name buffer size */
#define WBSIZE  256		/* input and working buffer size */
/*
 * max number of entries in the dialing directory and protocols file
 */
#define MAX_ENT 256
#define MAX_SEQUICKKEYS 30		/* Maximum number of SeQuickKeys */

#endif
