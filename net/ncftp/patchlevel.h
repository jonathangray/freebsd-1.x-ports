* Note:  You don't need a version later than 1.7.6 unless 1.7.6 or 1.7.7
  wouldn't compile for you.  If you have either 1.7.7 or 1.7.6 don't bother
  re-installing the program since no functionality has changed since 1.7.6

v1.7.8 - June 30, 1994.  No longer defining TERMH for linux.

v1.7.7 - June 21, 1994.  Deleted a space in front of an " #endif".
  No functionality change whatsoever...

v1.7.6 - June 18, 1994.  Added commands and code to support the 
  PASV command for passive negotiation of the data connection from
  the host server to the client.  This facilitates operation of the
  client software from within a firewall.  (J. B. Harrell)

v1.7.5 - May 28, 1994.  Fixed a rare problem with dimmed text.  Fixed
  compilation problem with Dynix.  Defining the domain name now takes
  precedence over the getdomainname() function.

v1.7.4 - May 16, 1994.  Tweaked hookup() a bit, to (try to) handle
  hosts with multiple addresses better.  Fixed error with GMT offsets.
  Fixed 'addr_t' typo in SVR4 section.  Moved SVR4 block down in sys.h.

v1.7.3 - April 13, 1994.  Fixed minor error in syslog reporting.
  Trying both getpwnam() and getpwuid(), instead of just one of them,
  increasing the probability the program can get your passwd entry.
  Better compatibility with other types of password input devices.

v1.7.2 - April 5, 1994.  Bytes/sec is now correct.  Fixed error when
  NO_VARARGS was defined.  Added support for <varargs.h>.

v1.7.1 - March 27, 1994.  Defining HAS_DOMAINNAME for NeXT.  Term hack can
  share sockets, plus some term stuff added to the Makefile.  Trimmed
  some old stuff from the patchlevel.h file, and putting new versions
  first now. Smarter about determining abbreviations from local hostnames.
  Fixed bug where progress meter would go beserk after trying to get
  a non-existant file.
  
v1.7.0 - March 14, 1994.  More verbose when logging to the system log,
  and making sure that syslog() itself is called with a total of 5
  or less parameters.  Official patch posted which incorporates all
  the fixes to 1.6.0 (i.e. 1.6.1, 1.6.2, ... 1.6.9).
  
v1.6.9 - March 11, 1994.  Added DOMAIN_NAME and Solaris CPP symbols.
  Better handling of getting the domain name, specifically with SunOS.
  BSDi support added.
  
v1.6.8 - March 4, 1994.  Ensuring that tmp files are not public.
  Trying harder to get the real hostname, and fixed problem with
  disappearing progress meters (both due to T. Lindgren).
  
v1.6.7 - February 20, 1994.  Using getpwnam() instead of getpwuid().
  Supporting WWW paths (i.e. ftp://host.name/path/name).
  
v1.6.6 - February 15, 1994.  Prevented scenario of fclosing a NULL FILE *.
  Edited term ftp's hookup() a little.  More defs for linux in sys.h.
  Not updating a recent entry unless you were fully logged in.
  
v1.6.5 - January 6, 1994.  Fixed error with an #ifndef/#endif block having
  whitespace before the #.  No longer confirming "ls >file" actions.
  Changed echo() to Echo().  AIX 3 uses TERMIOS.
  
v1.6.4 - December 30, 1993.  Fixed rare problem with GetDateAndTime.
  confirm() will return true if you're running the init macro. 
  
v1.6.3 - December 28, 1993.  Added a new diagnostic command, memchk,
  to print stats from a special malloc library if you used one.
  Using SIZE and MDTM when the remote site supports it.  Using a new
  set of routines for term (again).
  
v1.6.2 - December 10, 1993.
  Term hack no longer depends on the PASV command (!).  The BROKEN_MEMCPY
  problem worked-around.  More wary of symbolic-link recursion.
  Fixed local path expander.  Fixed inadvertant flushing of the typeahead
  buffer.  Debug mode won't print your password.  Progress meters
  no longer goof up when the file is huge.  Added time-remaining to the
  Philbar.
  
v1.6.1 - November 5, 1993.
  Checking if we have permission to write over a file to fetch.
  A few very minor changes.  BSD no longer trying to use strchr :-)
  
v1.6.0 - October 31, 1993.
  Added "term" support for Linux users.  Better SCO Xenix support.  Added
  -DLINGER, if you have a connection requiring it (so 'puts' to the remote
  system complete).  Added -DNET_ERRNO_H if you need to include
  <net/errno.h>.  Including more headers in sys.h.  Fixed another globulize
  bug.  Fixed a bug in confirm() where prompt was overwriting a str32.
  Added -DNO_CURSES_H if you do not want to try and include <curses.h>.
  Added -DHAS_GETCWD (usually automatic) and HAS_DOMAINNAME.  Logins as
  "ftp" act like "anonymous."  Fixed bug with "open #x".  Making sure you
  defined GZCAT as a string.  Turning off termcap attributes one by one,
  instead of using the turn-off-all-attributes.  A few fixes for the man
  page, including documentation of the progress-meter types.  AIX 2.x,
  AIX 3.x, ISC Unix, Dynix/PTX, and Besta support added to sys.h.  Safer
  use of getwd().  Colon-mode is quieter.  Getuserinfo function tweaked.
  Eliminated unnecessary GetHomeDir function in util.c.  Reworked Gets(),
  since it wasn't always stripping \n's.  Recent file can now read dir
  names with whitespace.  Opening msg uses a larger buffer, because of
  escape codes.  Philbar now prints K/sec stats.

v1.5.6 - September 20, 1993...
v1.5.5 - September 16, 1993...
v1.5.4 - September 14, 1993...
v1.5.3 - September 2, 1993...
v1.5.2 - August 30, 1993...
v1.5.1 - August 29, 1993...
v1.5.0 - August 22, 1993...

v1.0.2 - Jan 17, 1993...
v1.0.1 - December 8, 1992...
v1.0.0 - December 6, 1992. Initial release.
