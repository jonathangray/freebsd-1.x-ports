/********************************************************************
 * lindner
 * 3.8
 * 1993/08/19 20:32:59
 * /home/mudhoney/GopherSrc/CVS/gopher+/conf.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: conf.h
 * More configuration parameters.
 *********************************************************************
 * Revision History:
 * conf.h,v
 * Revision 3.8  1993/08/19  20:32:59  lindner
 * add default remoterc, change read timeout to 1 minute
 *
 * Revision 3.7  1993/08/12  06:35:08  lindner
 * Don't override CONF_FILE definition, use mail instead of /bin/mail for VMS
 *
 * Revision 3.6  1993/08/04  22:07:42  lindner
 * Use /bin/mail instead of ucbmail
 *
 * Revision 3.5  1993/07/27  05:35:30  lindner
 * reading material for VMS, dead code removal
 *
 * Revision 3.4  1993/04/15  22:08:51  lindner
 * Remote user mods (Mitra)
 *
 * Revision 3.3  1993/03/18  23:11:16  lindner
 * 1.2b3 release
 *
 * Revision 3.2  1993/02/19  21:25:03  lindner
 * Updated pager command for gopher+ stuff.
 *
 * Revision 3.1.1.1  1993/02/11  18:02:49  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.7  1993/02/09  22:49:34  lindner
 * Fixes for new mapping thing
 *
 * Revision 1.6  1993/01/08  23:04:48  lindner
 * Changed TN3270_COMMAND for Multinet
 *
 * Revision 1.5  1992/12/31  05:32:43  lindner
 * Added mods for VMS
 *
 * Revision 1.4  1992/12/22  21:45:26  lindner
 * Fixed bug with that zcat code I just added...
 *
 * Revision 1.3  1992/12/21  20:27:25  lindner
 * Added #ifdef to make zcat changable..
 *
 * Revision 1.2  1992/12/13  05:56:32  lindner
 * Added options for connection time-out code in the server (mtm)
 *
 * Revision 1.1  1992/12/11  19:01:58  lindner
 * Gopher1.1 Release
 *
 *********************************************************************/

/*
 * Defaults for the client program
 * On startup the client will contact either the gopher server
 * CLIENT1_HOST or CLIENT2_HOST randomly.
 *
 * Set CLIENT2_PORT to 0 if you only want one root machine
 */

#define CLIENT1_HOST "gopher.tc.umn.edu"
#define CLIENT2_HOST "gopher2.tc.umn.edu"

#define CLIENT1_PORT 70
#define CLIENT2_PORT  70


/*
 * Override some defaults for various platforms
 */

#if defined(sun)
#define PLAY_COMMAND "play -v 40 -"
#endif


#if defined(NeXT)
#define NO_VPRINTF
#define PLAY_COMMAND "play -v 40 -"
#endif

#if defined(_SEQUENT_)
#define PRINTER_COMMAND "lp"
#endif

/*
 * TYPE/PAGE is the default VMS utility for displaying text. An alternative
 * is to use EDIT/TPU or better still to acquire MOST via anonymous FTP from
 * narnia.memst.edu
 *
 * The Printer Command may also be setup to use a command procedure to save the
 * file before printing it. This avoids the problem of Gopher removing the
 * temporary file before it can be printed. For example set up a command
 * procedure in a public place (eg  SYSPUB:GOPHERPRINT.COM) containing:-
 *        $ file="sys$scratch:gopher_"+f$extract(21,2,f$time())+".tmp"
 *        $ copy 'p1' 'file'
 *        $ print 'f$trnlnm("GOPHERQUEUE")' /delete/noidentify 'file'
 *  (Note that the logical GOPHERQUEUE can be used to set options like
 *   default queue name or form type eg DEFINE/JOB GOPHERQUEUE "/queue=que1")
 * and then define the Printer Command appropriately:-
 * #  define PRINTER_COMMAND "@SYSPUB:GOPHERPRINT"
 */

#if defined(VMS)
#  define PRINTER_COMMAND "print"
#  define PAGER_COMMAND   "type/page"
#  define PLAY_COMMAND    "- none -"
#  define GOPHERHELP      "sys_local:gopher.hlp"
#  if defined(MULTINET)
#     define TN3270_COMMAND        "telnet/tn3270"
#  endif
#endif



/*
 * Now set the parameters, only if not set above... 
 */
#ifndef PAGER_COMMAND
#define PAGER_COMMAND "more"
#endif

#ifndef MAIL_COMMAND
#  ifdef VMS
#    define MAIL_COMMAND "mail"
#  else
#    define MAIL_COMMAND "/bin/mail"
#  endif
#endif

#ifndef TELNET_COMMAND
#define TELNET_COMMAND "telnet"
#endif

#ifndef TN3270_COMMAND
#define TN3270_COMMAND "tn3270"
#endif

#ifndef PRINTER_COMMAND
#define PRINTER_COMMAND "lpr"
#endif

#ifndef PLAY_COMMAND
#define PLAY_COMMAND "/bin/false"
#endif

#ifndef MIME_COMMAND
#define MIME_COMMAND "metamail -P" 
#endif

#ifndef IMAGE_COMMAND 
#define IMAGE_COMMAND "xloadimage -fork %s"
#endif

#ifndef REMOTERC
#define REMOTERC "/usr/local/lib/gopherrc.remote"
#endif

/****************** gopherd configuration ***********************/

/*
 * This is the default time to cache directory entries on the server.
 */

#define CACHE_TIME 180  /** Default cache time of three minutes. **/


/*
 * The load average at which to restrict connections
 */

#define MAXLOAD 10.0

/*
 * Return type for signal()
 */

#define SIGRETTYPE void

/*
 * Timeout for network reads (1 minute)
 */

#define READTIMEOUT (1 * 60)

/* We need to define this since inetd.conf can only have a few
   arguments, and we need lots of them */

#if !defined(CONF_FILE)
#  define CONF_FILE	"/usr/local/etc/gopherd.conf"
#endif
