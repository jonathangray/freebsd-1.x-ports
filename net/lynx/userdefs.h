/*
 * Lynx - Hypertext navigation system
 *
 *   (c) Copyright 1992, 1993, 1994 University of Kansas
 */

/*******************************************************************
 * There are three sections to this document
 *  Section 1.  Things you MUST change or verify
 *	Section 1a)  VMS specific things
 *	Section 1b)  UNIX specific things
 *	Section 1c)  ALL Platforms
 *
 *  Section 2.  Things you should probably check!
 *
 *  Section 3.  Things you should only change after you have a good
 *              understanding of the program!
 *
 */

#ifndef USERDEFS_H
#define USERDEFS_H

/*******************************************************************
 * Things you must change
 *  Section 1. 
 */

/*******************************************************************
 * Things you must change  VMS specific
 *  Section 1a). 
 */
#ifdef VMS
/**************************
 * TEMP_SPACE is where Lynx temporary cache files will be placed.
 * Temporary files are removed automatically as long as nothing
 * goes terribly wrong :)
 */
#define TEMP_SPACE "sys$scratch:"

/**************************
 * LYNX_CFG_FILE is the location and name of the lynx system
 * configuration file.
 */
#define LYNX_CFG_FILE "Lynx_Dir:lynx.cfg"

/**************************
 * The EXTENSION_MAP file allows you to map file suffix's to 
 * mime types.
 * These global and personal files can be overriden by lynx.cfg
 */
#define GLOBAL_EXTENSION_MAP "Lynx_Dir:mime.types"
#define PERSONAL_EXTENSION_MAP "mime.types"

/**************************
 * The MAILCAP file allows you to map file MIME types to 
 * external viewers.
 * These global and personal files can be overriden by lynx.cfg
 */ 
#define GLOBAL_MAILCAP "Lynx_Dir:mailcap"
#define PERSONAL_MAILCAP ".mailcap"

/**************************
 * This define will only be used for a default if you do not
 * use a lynx.cfg file
 *
 * the full path and name of the xloadimage command
 * put 'echo' or something like it here if you don't have it;
 * you may also use 'xv' or anything that will handle GIF,
 * TIFF and other popular image formats
 * You must also have a "%s" for the filename
 */
#define XLOADIMAGE_COMMAND "xv %s"

/**************************
 * The full path and name of the standard VMS "mail" command.
 *
 * The mail command will be spawned as a subprocess of lynx
 * and used to send replies and error messages.
 * SYSTEM_MAIL must be able to accept a subject line through
 * the use of the /subject="SUBJECT" option.
 * If your mailer uses another syntax, some hacking of
 * the "mailmsg.c" and "reply_by_mail.c" files may be required.
 */
#define SYSTEM_MAIL "mail"

/*************************
 * below is the argument for a sprintf command that will
 * add "in%""ADDRESS""" to the internet mail address given by the user.
 * It is structured for PMDF's in%"INTERNET_ADDRESS"
 * scheme.   The %s is replaced with the address given by the user.
 */
#define MAIL_ADRS "\"in%%\"\"%s\"\"\""

/*************************
 * The full path and name of the inews program
 *
 * A "mini" inews has been included in the utils directory.
 *
 * define INEWS as "none" if you do not have access to an
 * NNTP server for posting to NEWS from Lynx.
 */
#define INEWS "none"

/*******************************************************************
 * Things you must change  UNIX specific
 *  Section 1b). 
 */
#else     /* UNIX */

/**************************
 * LYNX_CFG_FILE is the location and name of the lynx system
 * configuration file.
 */
#define LYNX_CFG_FILE "/usr/local/lib/lynx.cfg"

/**************************
 * The EXTENSION_MAP file allows you to map file suffix's to 
 * mime types.
 * These global and personal files can be overriden by lynx.cfg
 */
#define GLOBAL_EXTENSION_MAP "/usr/local/lib/mosaic/mime.types"
#define PERSONAL_EXTENSION_MAP ".mime.types"

/**************************
 * The MAILCAP file allows you to map file MIME types to 
 * external viewers.
 * These global and personal files can be overriden by lynx.cfg
 */
#define GLOBAL_MAILCAP "/usr/local/lib/mosaic/mailcap"
#define PERSONAL_MAILCAP ".mailcap"

/*********************
 * LOCAL_DOMAIN is used to determine if a user is local
 * to your campus or organization
 */
#define LOCAL_DOMAIN "localhost"                /* CHANGE THIS! */

/**************************
 * the full path and name of the telnet command
 */
#define TELNET_COMMAND "telnet"

/**************************
 * the full path and name of the tn3270 command
 */
#define TN3270_COMMAND "tn3270"

/**************************
 * the full path and name of the rlogin command
 */
#define RLOGIN_COMMAND "rlogin"

/*************************
 * This define will only be used for a default if you do not
 * use a lynx.cfg file
 *
 * if you don't have xloadimage just set this to "echo" or
 * something else that is harmless, 'xv' also works really well if
 * not better!
 * You must also have a "%s" for the filename, "&" for backgroud is optional
 */
#define XLOADIMAGE_COMMAND "xv %s &"

/*************************
 * The full path and name of the inews program
 *
 * A "mini" inews has been included in the utils directory.
 *
 * set empty or to "none" if you don't have or want it.
 */
#define INEWS "inews"

/**************************
 * For UNIX systems this should be sendmail
 * sendmail should be in /usr/lib 
 *
 * You definitely want sendmail, not mail or elm or something else,
 * except in the case where MMDF is your mail agent.
 * For MMDF you should use submit (SCO)

 */
#ifdef MMDF
#define SYSTEM_MAIL "/usr/mmdf/bin/submit" 
#else
#ifdef __FreeBSD__
#define SYSTEM_MAIL "/usr/sbin/sendmail"
#else
#define SYSTEM_MAIL "/usr/lib/sendmail" 
#endif
#endif /* MMDF */

/**************************
 * A place to put temporary files, it's almost always "/tmp/" on
 * UNIX systems
 */
#define TEMP_SPACE "/tmp/"

#endif /* VMS OR UNIX */

/*************************************************************
 *  Section 1c)   Every platform must change or verify these
 *
 */

/*****************************
 * STARTFILE is the default file if none is specified on the command line 
 * 
 * note: STARTFILE must be a URL.  See the Lynx online help for more
 *       information on URL's
 */
#define STARTFILE "http://www.cc.ukans.edu/about_lynx/www_start.html" /* */
/* #define STARTFILE "http://info.cern.ch/default.html" */
/* #define STARTFILE "http://kufacts.cc.ukans.edu/cwis/kufacts_start.html" */

/*****************************
 *
 * HELPFILE must be defined as a URL and must have a 
 * complete local path name if local 
 * (file://localhost/DIRECTORY/FILENAME
 *  replace DIRECTORY with the current directory path and
 *  FILENAME with the name of the file.
 *  file://localhost/dua#/DIRECTORY/FILENAME on VMS systems.)
 * the default HELPFILE is:
 * http://kufacts.cc.ukans.edu/lynx_help/lynx_help_main.html
 * This file will be updated as needed.
 */
#define HELPFILE "http://www.cc.ukans.edu/lynx_help/lynx_help_main.html"

/*****************************
 * DEFAULT_INDEX_FILE is the default file retrieved when the
 * user presses the 'I' key when viewing any document.
 * An index to your CWIS can be placed here or a document containing
 * pointers to lots of interesting places on the web.
 */
#define DEFAULT_INDEX_FILE "http://info.cern.ch/default.html"

/********************************
* The DEFAULT_CACHE_SIZE specifies the number of WWW documents to be
* cached in memory at one time.  When the number is exceeded the oldest
* document will be removed from memory.
* The cache size may be modified with the command line argument -cache=NUMBER
*
*/
#define DEFAULT_CACHE_SIZE 10

/****************************************************************
 *   Section 2.   Things that you probably want to change or review
 *
 */

/*****************************
 * Enter the name of your anonymous account if you have one
 * as ANONYMOUS_USER.  UNIX systems will use a cuserid
 * or get_login call to determine if the current user is
 * the ANONYMOUS_USER.
 *
 * VMS systems cannot use this feature, so they must specify
 * anonymous accounts using the "-anonymous" command line option.
 *
 * Other systems may use the "-anonymous" option for multiple
 * accounts or precautionary reasons as well.
 *
 * It is very important to have this correctly defined if you 
 * have an anonymous account.  If you do not you will be putting 
 * yourself at GREAT security risk!
 *
 * Later on in this file you can specify privileges for the
 * anonymous account.
 */
#define ANONYMOUS_USER "" 

/******************************
 * SHOW_CURSOR controls whether or not the cursor is hidden
 * or appears over the link.  This is just the default, it
 * can be turned on with the -show_cursor command line option.
 * Showing the cursor is handy if you have really stupid terminals
 * that can't do bold and reverse video at the same time or at all.
 */
#define SHOW_CURSOR FALSE

/*******************************
 * set to FALSE if you don't want users of your anonymous account
 * who are calling from inside your local domain 
 * to be able to telnet back out
 */
#define CAN_ANONYMOUS_INSIDE_DOMAIN_TELNET	  TRUE  

/*******************************
 * set to FALSE if you don't want users of your anonymous
 * account who are calling from outside your
 * local domain to be able to telnet back out
 */
#define CAN_ANONYMOUS_OUTSIDE_DOMAIN_TELNET      TRUE 

/*******************************
 * set to FALSE if you don't want users of your anonymous account
 * who are calling from inside your local domain
 * to be able to read news
 */
#define CAN_ANONYMOUS_INSIDE_DOMAIN_READ_NEWS     TRUE

/*******************************
 * set to FALSE if you don't want users of your anonymous
 * account who are calling from outside your
 * local domain to be able to read news
 */
#define CAN_ANONYMOUS_OUTSIDE_DOMAIN_READ_NEWS    FALSE

/*******************************
 * set to FALSE if you don't want users of your anonymous
 * account to be able to print
 */
#define CAN_ANONYMOUS_PRINT	       FALSE

/*******************************
 * set to FALSE if you don't want users of your anonymous
 * account to be able to goto random URL's. (The 'g' command)
 */
#define CAN_ANONYMOUS_GOTO		TRUE

/*******************************
 * Execution links/scripts configuration.
 *
 * Execution links and scripts allow you to run
 * local programs by activating links within Lynx.
 *
 * An execution link is of the form:
 *
 *     lynxexec:<COMMAND>
 * or:
 *     lynxexec://<COMMAND>
 *
 * where <COMMAND> is a command that Lynx will
 * run when the link is activated.
 * The double-slash should be included if the
 * command begins with an '@', as for executing
 * VMS command files.  Otherwise, the double-
 * slash can be omitted.
 *
 * Execution scripts take the form of a standard
 * URL.  Extension mapping or MIME typing is used
 * to decide if the file is a script and should be
 * executed.  The current extensions are:
 * .csh, .ksh, and .sh on UNIX systems and .com on
 * VMS systems.  Any time a file of this type is
 * accessed Lynx will look at the user's options
 * settings to decide if the script can be executed.
 * Current options include: Only exec files that
 * reside on the local machine and are referenced
 * with a "file://localhost" URL, All execution
 * off, and all execution on.
 *
 * The following definitions will add execution
 * capabilities to Lynx.  You may define none, one
 * or both.
 *
 * I strongly recommend that you define neither one
 * of these since execution links/scripts can represent
 * very serious security risk to your system and its
 * users.  If you do define these I suggest that
 * you only allow users to execute files/scripts
 * that reside on your local machine. 
 *
 * YOU HAVE BEEN WARNED!
 *
 * Note: if you are enabling execution scripts you should
 * also see src/HTInit.c to verify/change the execution
 * script extensions and/or commands.
 */
/* #define EXEC_LINKS  */ 
/* #define EXEC_SCRIPTS  */ 


#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)

/**********
 * if ALLOW_USERS_TO_CHANGE_EXEC_WITHIN_OPTIONS 
 * is defined then the user will be able to change
 * the execution status within the options screen.
 */
/* #define ALLOW_USERS_TO_CHANGE_EXEC_WITHIN_OPTIONS */

/**********
 * if NEVER_ALLOW_REMOTE_EXEC is defined then local execution 
 * of scripts or lynxexec: URL's will only be implemented from
 * HTML files that were accessed via a "file://localhost/" URL,
 * and the options menu for "L)ocal executions links" will only
 * allow toggling between "ALWAYS OFF" and "FOR LOCAL FILES ONLY".
 */
/* #define NEVER_ALLOW_REMOTE_EXEC */

/*****************************
 * These are for executable shell scripts and links.
 * Set to FALSE unless you really know what you're
 * doing.
 *
 * This only applies if you are compiling with EXEC_LINKS or
 * EXEC_SCRIPTS defined.
 *
 * The first two settings:
 * LOCAL_EXECUTION_LINKS_ALWAYS_ON 
 * LOCAL_EXECUTION_LINKS_ON_BUT_NOT_REMOTE
 * specify the DEFAULT setting of the users execution link
 * options, but the user may still change those options.
 * If you do not wish the user to be able to change the
 * execution link settings you may wish to use the commandline option:
 *    -restrictions=exec_frozen
 *
 * LOCAL_EXECUTION_LINKS_ALWAYS_ON will be FALSE
 * if NEVER_ALLOW_REMOTE_EXEC has been defined.
 *
 * if LOCAL_EXECUTION_LINKS_ALWAYS_OFF_FOR_ANONYMOUS is 
 * true all execution links will be disabled when the
 * -anonymous command line option is used.  Anonymous
 * users are not allowed to change the execution options
 * from within the Lynx options menu so you might be able
 * to use this option to enable execution links and set
 * LOCAL_EXECUTION_LINKS_ON_BUT_NOT_REMOTE to TRUE to
 * give anonymous execution link capability without comprimising
 * your system.
 *
 */
#define LOCAL_EXECUTION_LINKS_ALWAYS_ON          FALSE
#define LOCAL_EXECUTION_LINKS_ON_BUT_NOT_REMOTE  FALSE
#define LOCAL_EXECUTION_LINKS_ALWAYS_OFF_FOR_ANONYMOUS FALSE

#endif /*  defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */

/*********************************
 *  MAIL_SYSTEM_ERROR_LOGGING will send a message to the owner of 
 *  the information if there is one, every time
 *  that a document cannot be accessed!
 *
 *  NOTE: This can generate A LOT of mail, be warned.
 *
 *  VMS USERS !!!
 * You will probably want to set 'MAIL_SYSTEM_ERROR_LOGGING' to FALSE.
 * It can be nice to have automatic error logging but this is done
 * by doing a system command and running mail.  This is VERY slow under
 * VMS and just takes too darn long.
 */
#define MAIL_SYSTEM_ERROR_LOGGING   FALSE  /*mail a message for every error?*/

/*********************************
 * VI_KEYS can be turned on by the user in the options
 * screen or the .lynxrc file.  This is just the default.
 */
#define VI_KEYS_ALWAYS_ON           FALSE /* familiar h,j,k, & l */

/*********************************
 * EMACS_KEYS can be turned on by the user in the options
 * screen or the .lynxrc file.  This is just the default.
 */
#define EMACS_KEYS_ALWAYS_ON           FALSE /* familiar ^N, ^P, ^F, ^B */

/*********************************
 * DEFAULT_KEYPAD_MODE specifies whether by default the user
 * has numbers that work like arrows or else numbered links
 * DEFAULT KEYPAD MODE may be set to 
 *	LINKS_ARE_NUMBERED  or
 *	NUMBERS_AS_ARROWS
 */
#define DEFAULT_KEYPAD_MODE	       NUMBERS_AS_ARROWS

/********************************
 * The default search.
 * This is a default that can be overridden by the user!
 */
#define CASE_SENSITIVE_ALWAYS_ON    FALSE /* case sensitive user search */


/****************************************************************
 *   Section 3.   Things that you should not change until you
 *  		  have a good knowledge of the program
 */

#define LYNX_NAME "Lynx"
#define LYNX_VERSION "2.3 BETA"
#ifndef MAXINT
#define MAXINT 2147483647
#endif /*MAXINT*/

/* text strings for certain actions */
/* changing these text strings is a way to customize 
 * your environment to better suit your tastes
 */
#define HELP "Commands: Use arrow keys to move, '?' for help, 'q' to quit, '<-' to go back"
#define MOREHELP \
  "-- press space for more, use arrow keys to move, '?' for help, 'q' to quit"
#define MORE "-- press space for next page --"
#define FORM_LINK_TEXT_MESSAGE "(Text entry field) Enter text. Use UP or DOWN arrows or tab to move off."
#define FORM_LINK_PASSWORD_MESSAGE "(Password entry field) Enter text. Use UP or DOWN arrows or tab to move off."
#define FORM_LINK_CHECKBOX_MESSAGE "(Checkbox Field)   Use right-arrow or <return> to toggle."
#define FORM_LINK_SUBMIT_MESSAGE "(Form submit button)   Use right-arrow or <return> to submit form."
#define FORM_LINK_RESET_MESSAGE "(Form reset button)   Use right-arrow or <return> to reset form to defaults."
#define FORM_LINK_OPTION_LIST_MESSAGE "(Option list)  Hit return and use arrow keys and return to select option"
#define NORMAL_LINK_MESSAGE "(NORMAL LINK)   Use right-arrow or <return> to activate"
#define LINK_NOT_FOUND "The resource requested is not available at this time."
#define WWW_WAIT_MESSAGE "Getting %s"
#define ADVANCED_URL_MESSAGE "URL: %s"
#define WWW_FAIL_MESSAGE "Unable to access WWW file!!!"
#define WWW_INDEX_MESSAGE "This is a searchable index.  Use 's' to search"
#define WWW_INDEX_MORE_MESSAGE "--More--  This is a searchable index.  Use 's' to search"
#define BAD_LINK_NUM_ENTERED "You have entered an invalid link number"
#define SOURCE_HELP "Currently viewing document source.  Press '\\' to return to rendered version"
#define NOVICE_LINE_ONE "  Arrow keys: Up and Down to move. Right to follow a link; Left to go back.  \n"
#define NOVICE_LINE_TWO " H)elp O)ptions P)rint G)o M)ain screen Q)uit /=search [delete]=history list \n"
#define FORM_NOVICELINE_ONE "            Enter text into the field by typing on the keyboard              "
#define FORM_NOVICELINE_TWO "    Ctrl-U to delete all text in field, [Backspace] to delete a character    "

#ifdef DIRED_SUPPORT
#define DIRED_NOVICELINE "  C)reate  D)ownload  E)dit  F)ull menu  M)odify  R)emove  T)ag  U)pload     \n"
#endif

#define MAXBASE 100       /* max length of base directory */
#define MAXHIGHLIGHT 160 /* max length of highlighted text */
#define MAXTARGET 130    /* max length of target string */
#define LINESIZE 1024    /* max length of line to read from file*/
#define MAXFNAME 1280	/* max filename length DDD/FILENAME.EXT */
#define MAXCOMMAND MAXFNAME /* max length of command should be the same */
#define MAXHIST  512	/* number of links we remember in history */
#define MAXLINKS 256	/* max links on one screen */
   /* traversal lookup table file, don't worry about it for now */
#define TRAVERSE_FILE "/homea/local/lynx2-0-8/traverse.file"
#define TRAVERSE_ERRORS "/homea/local/lynx2-0-8/traverse.errors"
#define TRAVERSE_FOUND_FILE "/homea/local/lynx2-0-8/traverse.found"

#endif /* USERDEFS_H */
