/********************************************************************
 * lindner
 * 3.13
 * 1993/08/10 20:28:10
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/serverutil.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: serverutil.c
 * utilities for the server.
 *********************************************************************
 * Revision History:
 * serverutil.c,v
 * Revision 3.13  1993/08/10  20:28:10  lindner
 * return true for non-existant cache file
 *
 * Revision 3.12  1993/08/06  14:30:47  lindner
 * Fixes for better security logging
 *
 * Revision 3.11  1993/08/05  20:46:36  lindner
 * Fix for Gpopen for single quotes and !
 *
 * Revision 3.10  1993/08/04  22:14:51  lindner
 * Mods to use Gpopen
 *
 * Revision 3.9  1993/08/04  22:12:48  lindner
 * Mods to use Gpopen
 *
 * Revision 3.8  1993/07/27  05:27:56  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.7  1993/07/20  23:57:29  lindner
 * Added LOGGopher here, added routine to set proctitle
 *
 * Revision 3.6  1993/07/07  19:34:53  lindner
 * fixed typo in GplusError
 *
 * Revision 3.5  1993/06/22  07:07:14  lindner
 * prettyfication
 *
 * Revision 3.4  1993/04/10  06:07:40  lindner
 * More debug msgs
 *
 * Revision 3.3  1993/04/09  15:12:09  lindner
 * Better error checking on getpeername()
 *
 * Revision 3.2  1993/03/24  20:28:55  lindner
 * Moved some code from gopherd.c and changed error message delivery
 *
 * Revision 3.1.1.1  1993/02/11  18:02:53  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2  1993/02/09  22:16:24  lindner
 * Mods for gopher+ error results.
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/



#include "gopherd.h"
#include "serverutil.h"
#include "Debug.h"

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/*
 * This finds the current peer and the time and  jams it into the
 * logfile (if any) and adds the message at the end
 */
#ifdef __STDC__
LOGGopher(int sockfd, const char *fmt, ...)
#else /* !__STDC__ */
void
LOGGopher(sockfd, fmt, va_alist)
  int sockfd;
  char *fmt;
va_dcl
#endif /* __STDC__ */
{
     va_list args;
     char message[512];
     time_t          Now;
     char            *cp;
                     /* cp + ' ' + host_name + ' : ' + MAXLINE + '\n' + '\0' */
     char            buf[286+MAXLINE];
     struct flock    lock;

#ifdef __STDC__
     va_start(args, fmt);
#else
     va_start(args);
#endif
     
     (void) vsprintf(message, fmt, args);

     if (LOGFileDesc != -1) {
	  
	  lock.l_type = F_WRLCK;
	  lock.l_whence = SEEK_SET;
          lock.l_start = 0L;
          lock.l_len = 0L;
          fcntl(LOGFileDesc, F_SETLKW, &lock);

	  time(&Now);         /* Include this in the lock to make sure */
	  cp = ctime(&Now);   /*  log entries are chronological */
	  ZapCRLF(cp);

          /* someone else may have written to the file since we opened it */
          lseek(LOGFileDesc, 0L, SEEK_END);
  
          sprintf(buf, "%s %d %s : %s\n", cp, getpid(), CurrentPeerName, message);
          write(LOGFileDesc, buf, strlen(buf));
          
          /* unlock the file */
          lock.l_type = F_UNLCK;
          fcntl(LOGFileDesc, F_SETLKW, &lock);
	  
	  Debug("%s", buf);
	  
     }
}


/*
 * Gopher + error mechanism
 *
 * errclass is the type of error
 * text is the first line of error text,
 * moretext is a char array of yet more text to send
 */

void
GplusError(sockfd, errclass, text, moretext)
  int sockfd;
  int errclass;
  char *text;
  char **moretext;
{
     char outputline[256];
     int i;

     sprintf(outputline, "--1\r\n%d %s <%s>\r\n0%s\t\t\t\r\n", 
	     errclass, GDCgetAdmin(Config),
	     GDCgetAdminEmail(Config), text);

     if (writestring(sockfd, outputline)<0) {
	  LOGGopher(sockfd, "Client went away!");
	  exit(-1);
     }

     if (moretext != NULL)
	  for (i=0; moretext[i] != NULL; i++) {
	       writestring(sockfd, moretext[i]);
	       writestring(sockfd, ".\r\n");
	  }

     writestring(sockfd, ".\r\n");

     LOGGopher(sockfd, "%s", text);

     close(sockfd);

     return;
}


/* 
 * This routine cleans up an open file descriptor and sends out a bogus
 * filename with the error message
 */

void
Abortoutput(sockfd, errmsg)
  int sockfd;
  char *errmsg;
{
     GplusError(sockfd, 1, errmsg, NULL);

}

/*
 * only works if non-chroot really...
 */

boolean
Setuid_username(username)
  char *username;
{
     struct passwd *pw;

     if (getuid() != 0)
	  return(FALSE);

     pw = getpwnam(username);
     
     if (!pw) {
	  Debug("Couldn't find user '%s'\n", username);

	  return(FALSE);
     }

     if (setegid(pw->pw_gid) < 0)
	  return(FALSE);

     if (seteuid(pw->pw_uid) < 0)
	  return(FALSE);

     Debug("Successfully changed user privs to '%s'\n", username);

     return(TRUE);
}



/*
 *
 *  Code stolen and heavily hacked from nntp
 *
 * inet_netnames -- return the network, subnet, and host names of
 * our peer process for the Internet domain.
 *
 *      Parameters:     "sock" is our socket
 *                      "host_name"
 *                      is filled in by this routine with the
 *                      corresponding ASCII names of our peer.
 *       
 *                      if there doesn't exist a hostname in DNS etal,
 *                      the IP# will be inserted for the host_name
 *
 *                      "ipnum" is filled in with the ascii IP#
 *      Returns:        Nothing.
 *      Side effects:   None.
 */

void
inet_netnames(sockfd, host_name, ipnum)
  int  sockfd;
  char *host_name;
  char *ipnum;
{
     struct sockaddr_in      sa;
     int                     length;
     u_long                  net_addr;
     struct hostent          *hp;

     length = sizeof(sa);
     if (getpeername(sockfd, (struct sockaddr *)&sa, &length))
	  /** May fail if sockfd has been closed **/
	  return;

     strcpy(ipnum, inet_ntoa(sa.sin_addr));
     strcpy(host_name, inet_ntoa(sa.sin_addr));

     hp = gethostbyaddr((char *) &sa.sin_addr,sizeof (sa.sin_addr), AF_INET);
     
     if (hp != NULL)
	  (void) strcpy(host_name, hp->h_name);

}





/*
 * is_mail_from_line - Is this a legal unix mail "From " line?
 *
 * Given a line of input will check to see if it matches the standard
 * unix mail "from " header format. Returns 0 if it does and <0 if not.
 *
 * 2 - Very strict, also checks that each field contains a legal value.
 *
 * Assumptions: Not having the definitive unix mailbox reference I have
 * assumed that unix mailbox headers follow this format:
 *
 * From <person> <date> <garbage>
 *
 * Where <person> is the address of the sender, being an ordinary
 * string with no white space imbedded in it, and <date> is the date of
 * posting, in ctime(3C) format.
 *
 * This would, on the face of it, seem valid. I (Bernd) have yet to find a
 * unix mailbox header which doesn't follow this format.
 *
 * From: Bernd Wechner (bernd@bhpcpd.kembla.oz.au)
 * Obfuscated by: KFS (as usual)
 */

#define MAX_FIELDS 10

static char legal_day[]         = "SunMonTueWedThuFriSat";
static char legal_month[]       = "JanFebMarAprMayJunJulAugSepOctNovDec";
static int  legal_numbers[]     = { 1, 31, 0, 23, 0, 59, 0, 60, 1969, 2199 };

int is_mail_from_line(line)
  char *line;     /* Line of text to be checked */
{
     char *fields[MAX_FIELDS];
     char *sender_tail;
     register char *lp, **fp;
     register int n, i;
     
     if (strncmp(line, "From ", 5)) return -100;
     
     lp = line + 5;
     /* sender day mon dd hh:mm:ss year */
     for (n = 0, fp = fields; n < MAX_FIELDS; n++) {
	  while (*lp && *lp != '\n' && isascii(*lp) && isspace(*lp)) lp++;
	  if (*lp == '\0' || *lp == '\n') break;
	  *fp++ = lp;
	  while (*lp && isascii(*lp) && !isspace(*lp))
	       if (*lp++ == ':' && (n == 4 || n == 5)) break;
	  if (n == 0) sender_tail = lp;
     }
     
     if (n < 8) return -200-n;
     
     fp = fields;
     
     if (n > 8 && !isdigit(fp[7][0])) fp[7] = fp[8]; /* ... TZ year */
     if (n > 9 && !isdigit(fp[7][0])) fp[7] = fp[9]; /* ... TZ DST year */
     
     fp++;
     for (i = 0; i < 21; i += 3)
	  if (strncmp(*fp, &legal_day[i], 3) == 0) break;
     if (i == 21) return -1;
     
     fp++;
     for (i = 0; i < 36; i += 3)
	  if (strncmp(*fp, &legal_month[i], 3) == 0) break;
     if (i == 36) return -2;
     
     for (i = 0; i < 10; i += 2) {
	  lp = *++fp;
	  if (!isdigit(*lp)) return -20-i;
	  n = atoi(lp);
	  if (n < legal_numbers[i] || legal_numbers[i+1] < n) return -10-i;
     }
     return 0;
}



/* 
 * Return the basename of a filename string, i.e. everything
 * after the last "/" character. 
 */

char *mtm_basename(string)
  char *string;
{
     static   char *buff;
     buff = string + strlen(string); /* start at last char */
     while (*buff != '/' && buff > string)
	  buff--;
     return( (char *) (*buff == '/'? ++buff : buff));
}



/*
 * Cache timeout value.
 *   If cache is less than secs seconds old, it's ok.
 *   If cache is newest, it's ok, otherwise it must be rebuilt.
 * 
 */

boolean
Cachetimedout(cache, secs, dir)
  char *cache;
  int secs;
  char *dir;
{
     STATSTR       buf;
     int           result;
     time_t        now;

     result = rstat(cache, &buf);

     if (result != 0)
	  return(TRUE);

     time(&now);
     
     Debug("Cache now: %d, ", now);
     Debug("cache file: %d\n", buf.st_mtime);
     
     if ( now < (buf.st_mtime + secs))
	  return(FALSE);
     else
	  return(TRUE);

}

/*
 * Returns true (1) for a directory
 *         false (0) for a file
 *         -1 for anything else
 */

boolean
isadir(path)
  char *path;
{
     STATSTR buf;
     int result;

     result = rstat(path, &buf);

     if (result != 0)
	  return(-1);
     
     if (S_ISDIR(buf.st_mode)) {
	  if (! access(path, F_OK))
	       return(1);
	  else
	       return(-1);
     }
     else if (S_ISREG(buf.st_mode))
	  return(0);
     else
	  return(-1);
}


/*
 * This function sets the process title given by ps on a lot of BSDish
 * systems
 */

#ifdef __STDC__
ServerSetArgv(const char *fmt, ...)
#else /* !__STDC__ */
ServerSetArgv(fmt, va_alist)
  char *fmt;
va_dcl
#endif /* __STDC__ */
{
     va_list args;
     register char *p;
     register int i;
     char buf[MAXLINE];

#ifdef SETPROCTITLE

     
# ifdef __STDC__
     va_start(args, fmt);
# else /* !__STDC__ */
     va_start(args);
# endif /* __STDC__ */
     (void) vsprintf(buf, fmt, args);
     
     va_end(args);
     
     /* make ps print "(gopherd)" */
     p = Argv[0];
     *p++ = '-';
     
     i = strlen(buf);
     if (i > LastArgv - p - 2)
     {
	  i = LastArgv - p - 2;
	  buf[i] = '\0';
     }
     (void) strcpy(p, buf);
     p += i;
     while (p < LastArgv)
	  *p++ = ' ';
#endif /* SETPROCTITLE */
     ;
}

FILE*
Gpopen(sockfd, cmd, rw)
  int  sockfd;
  char *cmd;
  char *rw;
{
     int inquote = 0;
     int insquote = 0;
     int i;

     /** Strip out the naughty bits..  **/
     for (i=0; cmd[i] != '\0'; i++) {
	  switch (cmd[i]) {
	  case '"':
	       if (!insquote)
		    inquote = 1-inquote;
	       break;
	       
	  case '\'':
	       if (!inquote)
		    insquote = 1-insquote;
	       break;

	  case '*':
	  case '&':
	  case '|':
	  case ';':
	  case '=':
	  case '?':
	  case '<':
	  case '>':
	  case '(':
	  case ')':
	  case '{':
	  case '}':
	  case '[':
	  case ']':
	  case '^':
	       /*** Stuff that's okay if quoted.. ***/

	       if (!inquote && !insquote) {
		    LOGGopher(sockfd, "Possible Security Violation '%s'", cmd);
		    return(NULL);
	       }
	       
	       break;

	  case '!':
	  case '\\':
	  case '`':
	  case '\n':
	  case '$':
	       /*** Stuff that shouldn't be in there at all! **/

	       LOGGopher(sockfd, "Possible Security Violation '%s'", cmd);
	       return(NULL);

	       break;
	  }
     }

     return(popen(cmd, rw));
}
