/* patchlevel.h */

/*
 * v1.0.0 - December 6, 1992.
 *    Initial release.
 *
 * v1.0.1 - December 8, 1992.
 *  Added default value for NCARGS in glob.c for systems that don't define it.
 *  Fixed pdir bug which was caused by me mistakenly adding the page-a-
 *  compressed-file feature to ls instead of page.  Fixed bug in documentation,
 *  which had the same error!  Added spec for Ultrix in sys.h.  Fixed error
 *  in sys.h that recommended -Dconst instead of -Dconst="".  Added GETPASS
 *  compile flag to use getpass() instead of getpass2(), which make compiling
 *  easier if the compiler choked in cmds.c.  Added GETCWDSIZET for systems
 *  whose getcwd() takes a size_t instead of an int.
 *
 * v1.0.2 - Jan 17, 1993.
 *  Using the cpp symbol CONST instead of const to tell
 *  the compiler not to use the 'const' directive.  Checking for __sgi as
 *  well as sgi in sys.h.  Added #ifndef __GNUC__ block to make SunOS users
 *  use gcc.  You can avoid trying to include <unistd.h> by defining
 *  NO_UNISTDH.  Added DG/UX entry in sys.h.  Also added still another cpp
 *  symbol, BAD_INETADDR, which is used if inet_addr returns a structure
 *  instead of a u_long (this is only for DG/UX so far).  Changed long to
 *  int in wait().  Added default value for NCARGS in glob.c.  Added cpp
 *  symbol NO_STDLIBH for systems without <stdlib.h>.  Fixed 'quote.'
 *  Fixed 'size.'  Set all's string variable are printed in double quotes.
 *  Ansi-escapes is init'd to 1 if TERM==xterm.  Fixed 'type tenex.'  Set
 *  verbose makes sure verbose is within bounds and prints messages now.
 *  Better getpass2.  Tries .ncftprc before .netrc.  @N adds a \n to prompt.
 *  ls() is more flexible.  Macdef and $ print current macros if no arg
 *  is given.  getpass2 is now Getpass, and accompanying this are
 *  more cpp symbols SGTTYB and TERMIOS.  Better SCO support.  No longer using
 *  gets(), instead using own Gets() which is safer.  Better termcap support,
 *  or actually curses support, to get ansi-escape sequences.  Using -FC
 *  instead of -CF for ls flags, do avoid a rare conflict.  Progress meters
 *  work better.  Phil Dietz added a cool bar graph one, and I added another
 *  similar to the default percentage meter that shows the # KB transferred,
 *  which will work on systems not supporting SIZE.  Fixed floating point
 *  exception bug in put/mput.  Fixed implicit_cd to work with wuarchive-ish
 *  cd messages.  Added NeXT and DYNIX blocks in sys.h.  
 *  Fixed bug in _cd, that was trying to use wildcards when it
 *  shouldn't.  Fixed bug in macdef.  Fixed small bug in getreply.  Turned
 *  off echoing during the progress-meter.  Added syslogging capability.
 *
 * v1.5.0 - August 22, 1993
 *  Fixed error in CONST block in ftpdefs.h.  Fixed error in sys.h so that when
 *  you compile with -DGETPASS it uses getpass.  'ls' bug with wildcards
 *  fixed.  'ls' enhanced to take multiple remote paths.  Added new cpp symbol
 *  STRICT_PROTOS so I don't have to worry about the correct declarations of
 *  int returning functions.  Moved termcap_init() up.  Changed value of
 *  tcap_plain to "me" from "se."  Defining TERMIOS by default for SunOS.
 *  Edited perror to not print ioctl errors (for SunOS only).  Making sure
 *  we use ioctl only on tty files.  Using <arpa/ftp.h> for SCO to get
 *  MAXPATHLEN instead of <sys/arpa.h>.  386BSD and Pyramid OSx entries added
 *  to sys.h.  Fixed subtle error in FGets macro.  Using private _Strn
 *  string routines, with macros to them instead of the strn routines.
 *  Added support for GNU Readline (not included).  Added BROKEN_MEMCPY
 *  CPP symbol for other systems with same bug as that of SCO (see ftp.c).
 *  Changed mail detection code a bit, hopefully ending the false alarms
 *  that some systems were having.  New CPP symbol, dFTP_PORT, lets you define
 *  the default ftp port if your OS (i.e. ISC unix) is wrong.  Fixed bugs
 *  in 'rstatus' and 'syst;'  apparently no one noticed...  Phased out
 *  mls and mdir since ls does it all now.  Added dPROGRESS to defaults.h to
 *  set the default progress indicator.  Colon-mode wasn't quiet; fixed.
 *  Some new prompt @flags added.  @flags that may result in an empty string
 *  are set so that when they do print something, they tack on a trailing
 *  space.  May have fixed bug that caused 'page' to drop connection when
 *  you quit your pager.  NcFTP tries to prevent itself from becoming a
 *  mindless zombie process.  Fixed bug in 'rename.'  Added another progress
 *  indicator that doesn't use any backspaces.  Firewall support added by
 *  Dave Wolfe (dwolfe@pffft.sps.mot.com).  Saving the entire servent struct,
 *  not just a ptr to it.  Support the DOTDIR env variable.  Fetched files'
 *  modification times now set to that on the server.  Added RO-Var "netrc."
 *  Munged Makefile to support 'install' and 'uninstall,' and passing the
 *  installation parameters as a -D flag.  Awesome new recent site log!
 *  Can type 'open' by itself to print out the sites in our netrc/recent
 *  buffers.  Added show command.  Drastic changes in source code; cmds.c
 *  made smaller by spawning set.c and util.c; ftpdefs.h eliminated.  Improved
 *  on-line help.  Revamped the command table, by eliminating obselete fields
 *  and adding a usage field to print a cmd's usage message.  Removed all the
 *  useless settings of the 'code' variable.  Added tips.c.  Better support
 *  for ~s and environment vars in pathnames.  Ascii transfers can get whole
 *  lines at a time instead of just chars at a time, so ascii transfers may
 *  be a bit faster.  Old FTP commands are acknowledged at least.  Some
 *  additions for Apollo in sys.h.  Code in ftp.c cleaned up a bit, most
 *  notably the obnoxiously long recvrequest() function has been broken up
 *  into quite a few subroutines; it's still too long for my taste, though.
 *  Incorporated some stuff by Tom Dickey.  Enhanced Perror() function to
 *  print more stuff if DB_ERRS is defined.  Added 'ftpcat' mode.  You can
 *  now turnoff the startup msg.  GNU gzip support added.  Added dbprintf()
 *  function to print the debug messages.  Changed mind and decided to
 *  read the whole stream anyway on aborts for better stability;  you can
 *  still try the aborting the stream if you define TryAbort.  Added 'site'
 *  command.  Fixed bug where verbose was left set to V_QUIET when you used
 *  colon-mode.  Printing a warning when you try something like 'ls -t *.Z,'
 *  because wildcards don't work right with ls flags. Verbose and debug can
 *  be set directly from the cmd line (i.e. -D 3, not -DDD).  Verbosity can
 *  be set using their ascii names, in addition to it's number, like 'set
 *  verbose = quiet.'  Removed setpeer from cmds.c, and created new files,
 *  open.{c,h} dedicated to it; broke up setpeer into smaller sub-procs,
 *  and commented whole file (yay!).  Added a new user var, anon-open,
 *  for those folks who don't want anon logins as the default.
 *
 * v1.5.1 - August 29, 1993
 *  Bugs fixed in termcap code, mput, and pwd.  No longer adding blank
 *  lines to readline's history.  Netrc site abbreviations were matched
 *  by strncmp() when it should have been strstr().  Bug fixed in
 *  open's site "menu."  Revised tips a little to encourage .ncftprc instead
 *  of .netrc.  TRY_ABOR documented in the README.  Added stuff to the
 *  DYNIX entry.  Hacks added for SCO322.  Shortened bargraph prog meter
 *  by one char.  Better compat with getline.  Man page fixed by DWS again :)
 *
 * v1.5.2 - August 30, 1993.
 *  Back to using "me" instead of "se" to for termcap_normal.  Fixed Netrc
 *  site abbrev problem in a different way (by getting the fullsite name
 *  before calling ruserpass2).
 *
 * v1.5.3 - September 2, 1993.
 *  Changed 'sig_t' to 'Sig_t.'  Fixed another error in the termcap crap.
 *  Made having mktime() optional, at the expense of setting file dates
 *  to the same as the remote file.  Fixed an error during 'account'
 *  validation.  Added a warning about a bug I haven't fixed yet with
 *  non-UNIX systems hanging after listings.  Fixed bug where colon-mode
 *  sitenames weren't expanded.  Fixed a tip.  Using <readline/readline.h>
 *  and <getline/getline.h> instead of <readline.h> etc.  Fixed daylight
 *  savings time bug.  LocalPath checks $HOME now.
 *
 * v1.5.4 - September 14, 1993.
 *  Fixed bug where non-unix sites were hanging after listings.  Better
 *  SVR4 support.  Fixed bug during an ascii transfer with debug mode
 *  on.  Now checking the system type after a successful login, because
 *  some sites didn't allow commands to be executed until logged in; this
 *  prevents one (the only?) instance of the elusive short-file bug, because
 *  files were being fetched with ascii mode on.  Now checking for half-
 *  bright mode if boldface isn't available.  Numeric-only site abbreviations
 *  no longer accepted, so numbers will be treated only as indices from the
 *  open 'menu.'  You can include <term.h> for the 'tgetstr' prototype,
 *  if you define TERMH.  termcap_get() tweaked.  Fixed bug where macros
 *  from the previous site were still present when you opened a new site.
 *  Fixed bug where colon-mode paths were truncated.  Setting tenex mode
 *  automatically when you open a TOPS20 site.  Looking for <getline.h>
 *  instead of <getline/getline.h>; have to leave <readline/readline.h>,
 *  because that header also includes stuff like <readline/keymaps.h>.
 *  Catman support added to Makefile.  Fixed problem with terminfo, where
 *  $<2> etc., was not being removed from the terminal control strings.
 *
 * v1.5.5 - September 16, 1993.
 *  Fixed a bug where a key function wasn't returning it's results.
 *
 * v1.5.6 - September 20, 1993.
 *  Fixed bug in put, caused by "indent."  Checking for '.gz' extension
 *  for gzip in addition to '.z'.  A little better at preserving the
 *  transfer type.  Changed a syslog() call to use than 6 arguments,
 *  and reporting the full remote path to the system log now.  Have to
 *  explicitly define GZCAT in order to try paging gzip files now.
 *  Setting the current hostname correctly if using the gateway code.
 *  A lot of hacks added for SCO Xenix support: 1.  Can add -DNO_STRSTR
 *  to use own strstr();  2.  Can add -DNO_STRFTIME to make % flags in the
 *  prompt optional;  3. Can add -DNO_RENAME if you do not have rename(),
 *  or rename() doesn't work;  4. Added PTRTYPE if void ptrs are a problem.
 *
 * v1.6.0 - October 31, 1993.
 *  Added "term" support for Linux users.  Better SCO Xenix support.  Added
 *  -DLINGER, if you have a connection requiring it (so 'puts' to the remote
 *  system complete).  Added -DNET_ERRNO_H if you need to include
 *  <net/errno.h>.  Including more headers in sys.h.  Fixed another globulize
 *  bug.  Fixed a bug in confirm() where prompt was overwriting a str32.
 *  Added -DNO_CURSES_H if you do not want to try and include <curses.h>.
 *  Added -DHAS_GETCWD (usually automatic) and HAS_DOMAINNAME.  Logins as
 *  "ftp" act like "anonymous."  Fixed bug with "open #x".  Making sure you
 *  defined GZCAT as a string.  Turning off termcap attributes one by one,
 *  instead of using the turn-off-all-attributes.  A few fixes for the man
 *  page, including documentation of the progress-meter types.  AIX 2.x,
 *  AIX 3.x, ISC Unix, Dynix/PTX, and Besta support added to sys.h.  Safer
 *  use of getwd().  Colon-mode is quieter.  Getuserinfo function tweaked.
 *  Eliminated unnecessary GetHomeDir function in util.c.  Reworked Gets(),
 *  since it wasn't always stripping \n's.  Recent file can now read dir
 *  names with whitespace.  Opening msg uses a larger buffer, because of
 *  escape codes.  Philbar now prints K/sec stats.
 *
 * v1.6.1 - November 5, 1993.
 *  Checking if we have permission to write over a file to fetch.
 *  A few very minor changes.  BSD no longer trying to use strchr :-)
 *
 * v1.6.2 - December 10, 1993.
 *  Term hack no longer depends on the PASV command (!).  The BROKEN_MEMCPY
 *  problem worked-around.  More wary of symbolic-link recursion.
 *  Fixed local path expander.  Fixed inadvertant flushing of the typeahead
 *  buffer.  Debug mode won't print your password.  Progress meters
 *  no longer goof up when the file is huge.  Added time-remaining to the
 *  Philbar.
 *
 * v1.6.3 - December 28, 1993.  Added a new diagnostic command, memchk,
 *  to print stats from a special malloc library if you used one.
 *  Using SIZE and MDTM when the remote site supports it.  Using a new
 *  set of routines for term (again).
 *
 * v1.6.4 - December 30, 1993.  Fixed rare problem with GetDateAndTime.
 *  confirm() will return true if you're running the init macro. 
 *
 * v1.6.5 - January 6, 1994.  Fixed error with an #ifndef/#endif block having
 *  whitespace before the #.  No longer confirming "ls >file" actions.
 *  Changed echo() to Echo().  AIX 3 uses TERMIOS.
 *
 * v1.6.6 - February 15, 1994.  Prevented scenario of fclosing a NULL FILE *.
 *  Edited term ftp's hookup() a little.  More defs for linux in sys.h.
 *  Not updating a recent entry unless you were fully logged in.
 *
 * v1.6.7 - February 20, 1994.  Using getpwnam() instead of getpwuid().
 *  Supporting WWW paths (i.e. ftp://host.name/path/name).
 *
 * v1.6.8 - March 4, 1994.  Ensuring that tmp files are not public.
 *  Trying harder to get the real hostname, and fixed problem with
 *  disappearing progress meters (both due to T. Lindgren).
 *
 * v1.6.9 - March 11, 1994.  Added DOMAIN_NAME and Solaris CPP symbols.
 *  Better handling of getting the domain name, specifically with SunOS.
 *  BSDi support added.
 *
 * v1.7.0 - March 14, 1994.  More verbose when logging to the system log,
 *  and making sure that syslog() itself is called with a total of 5
 *  or less parameters.  Official patch posted which incorporates all
 *  the fixes to 1.6.0 (i.e. 1.6.1, 1.6.2, ... 1.6.9).
 */
