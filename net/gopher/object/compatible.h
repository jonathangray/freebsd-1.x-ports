/********************************************************************
 * lindner
 * 3.11
 * 1993/08/16 19:40:58
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/compatible.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: compatible.h
 * Header file to make things compatible and define stuff in compatible.c
 *********************************************************************
 * Revision History:
 * compatible.h,v
 * Revision 3.11  1993/08/16  19:40:58  lindner
 * Fixes for Sequent Dynix
 *
 * Revision 3.10  1993/08/03  20:57:42  lindner
 * Really fix it for hocky pucks
 *
 * Revision 3.9  1993/08/03  06:43:48  lindner
 * Compatibility fix for hpux and seteguid
 *
 * Revision 3.8  1993/07/27  05:30:29  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.7  1993/07/20  23:33:27  lindner
 * Fix for NeXTs and pid_t
 *
 * Revision 3.6  1993/07/14  20:37:40  lindner
 * Fixes for VMS
 *
 * Revision 3.5  1993/07/07  19:29:19  lindner
 * Added LINGER compatibility option
 *
 * Revision 3.4  1993/06/22  05:53:19  lindner
 * Added getdtablesize() option
 *
 * Revision 3.3  1993/06/15  06:12:14  lindner
 * Updates for Solaris
 *
 * Revision 3.2  1993/04/15  21:36:33  lindner
 * Emulation of geteuid calls for HPs
 *
 * Revision 3.1.1.1  1993/02/11  18:03:05  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.7  1993/01/14  21:58:02  lindner
 * Added #define that makes compatible compile fine under UCX
 *
 * Revision 1.6  1993/01/11  19:56:20  lindner
 * Fixed prototype for strcasecmp
 *
 * Revision 1.5  1993/01/09  02:25:57  lindner
 * Added definition for SIG_ERR for systems that don't have it.
 *
 * Revision 1.4  1993/01/08  23:17:20  lindner
 * Added more mods for VMS.
 *
 * Revision 1.3  1993/01/06  23:09:44  lindner
 * Added definition for AIX370 for strdup()
 *
 * Revision 1.2  1992/12/31  04:42:02  lindner
 * Changes for VMS, new switch NO_STRCASECMP
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/
/*
 * Protoize a little 
 */

#ifndef compatible_h_
#define compatible_h_

/******************** for SIG_ERR **/
#ifndef SIG_ERR
#  ifdef __STDC__
#    define SIG_ERR         ((void (*) (int))-1)
#  else
#    define SIG_ERR         ((void (*) ())-1)
#  endif        /* __STDC__ */
#endif        /* SIG_ERR */



/******************** for strstr() **/

#if defined(NOSTRSTR) || (defined(mips) && !defined(ultrix)) || defined(sequent) || defined(n16) || defined(sony_news)
#undef  NOSTRSTR
#define NOSTRSTR
char *strstr();
#endif

/******************** for tempnam() **/

#if defined(__convex__) || defined(NeXT) || defined(sequent) || defined(VMS) || defined(NO_TEMPNAM)
#undef  NO_TEMPNAM
#define NO_TEMPNAM
char *tempnam();
#endif

/******************************** for strdup() **/

#if defined(mips) || defined(ultrix) || defined(NeXT) || defined(sony_news) || defined(sequent) || defined (VMS) || defined(_AUX_SOURCE) || defined(_AIX370) || defined(NO_STRDUP) 
#undef  NO_STRDUP
#define NO_STRDUP
char *strdup();
#endif

/********************** For bzero()/bcopy() etc */

#if defined(USG) || defined(NO_BZERO) || defined(_SEQUENT_) || defined(VMS) ||     defined(__svr4__)

#define bzero(a,b)   memset(a,'\0',b)
#define bcopy(a,b,c) memcpy(b,a,c)
     
#endif

/********************* For getwd() */

#if defined(M_XENIX) || defined(hpux) || defined(USG) || defined(NO_GETWD) ||      defined(__svr4__)
# define getwd(a) getcwd(a,sizeof(a))
#endif

/********************* For tzset() */

#if defined(NO_TZSET) || defined(sequent)
#undef  NO_TZSET
#define NO_TZSET
void tzset();
#endif

/******************** For strcasecmp() */

#if defined(NO_STRCASECMP) || defined(VMS) 
#  undef NO_STRCASECMP
#  define NO_STRCASECMP
int strcasecmp();
#endif

/******************** For geteuid() */

#if defined(NO_SETEUID) || defined (__hpux)
# undef   NO_SETEUID
# define  NO_SETEUID


#define seteuid(x) setresuid(-1,x,-1)
#define setegid(x) setresgid(-1,x,-1)  

#endif


#if defined(_SEQUENT_)
# undef   NO_SETEUID
# define  NO_SETEUID

#define seteuid(x) setreuid(-1,x)
#define setegid(x) setregid(-1,x)

#endif

/********************* For getdtablesize() **/
#if defined(NO_GETDTABLESIZE)
#undef NO_GETDTABLESIZE
#define NO_GETDTABLESIZE 1

int getdtablesize();
#endif

/********************* For VMS */

#if defined(VMS)

#  include <stdio.h>

#  define popen(a,b) fopen(a,b)
#  define pclose(a)  fclose(a)
#  define unlink delete

#  define fopen fopen_VMSopt
   FILE *fopen_VMSopt();
#  define open open_VMSopt
   int open_VMSopt();

#  if defined(UCX)
#    define closenet close
#  endif

#else
   /* non-VMS systems don't need a special netclose either */
#  define closenet close

#endif  /** VMS **/


/**********  For linger setsockopt() ***/

#if defined(linux) || defined(_SEQUENT_)
#  define NO_LINGER
#endif

#endif  /* compatible_h_ */

/********** For NeXTs **************/
#ifdef NeXT

typedef int pid_t;

#endif

/********* Systems that can't set proctitles *******/
#if defined(_AUX_SOURCE) || defined(__svr4__) 
#   undef SETPROCTITLE
#endif

