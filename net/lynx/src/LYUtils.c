#include "LYCurses.h"
#include "HTUtils.h"
#include "HTParse.h"
#include "tcp.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYGlobalDefs.h"
#include "LYSignal.h"

#ifndef       FD_SETSIZE
#define       FD_SETSIZE   256
#endif


/*
 * highlight (or unhighlight) a given link
 */
PUBLIC void highlight ARGS2(int,flag, int,cur)
{
    char buffer[200];

    if (nlinks > 0) {
	move(links[cur].ly, links[cur].lx);
	if (flag == ON) { 
	   /* makes some terminals work wrong because
	    * they can't handle two attributes at the 
	    * same time
	    */
	   /* start_bold();  */
	   start_reverse();
      	 } else {
	   start_bold();
	 }

      if(links[cur].type == WWW_FORM_LINK_TYPE) {
	int len;
	int avail_space = (LYcols-links[cur].lx)-1;

        LYstrncpy(buffer,links[cur].hightext, 
			(avail_space > links[cur].form->size ? 
				links[cur].form->size : avail_space));
        addstr(buffer);  
  
	len = strlen(buffer);
	for(; len < links[cur].form->size && len < avail_space; len++)
	    addch('_');

      } else {
								
           /* copy into the buffer only what will fit within the
	    * width of the screen
	    */
          LYstrncpy(buffer,links[cur].hightext, LYcols-links[cur].lx-1);
          addstr(buffer);  
      }

      if (flag == ON) 
          stop_reverse();
      stop_bold();

#ifdef FANCY_CURSES
      if(!LYShowCursor)
          move(LYlines-1,LYcols-1);  /* get cursor out of the way */
      else
#endif /* FANCY CURSES */
	  /* never hide the cursor if there's no FANCY CURSES */
          move(links[cur].ly, links[cur].lx);

      if(flag)
          refresh();
    }
}

/*
 * display (or hide) the status line
 */

PUBLIC void statusline ARGS1(char *,text)
{
    char buffer[256];

    if(!text || text==NULL)
	return;

	/* don't print statusline messages if dumping to stdout
	 */
    if(dump_output_immediately)
	return;

    /* make sure text is not longer than COLS */
    strncpy(buffer,text,LYcols-1);

    if(user_mode == NOVICE_MODE)
        move(LYlines-3,0);
    else
        move(LYlines-1,0);
    clrtoeol();
    if (text != NULL) {
	start_reverse();
	addstr(buffer);
	stop_reverse();
    }

    refresh();
}

PUBLIC void noviceline ARGS1(int,more)
{

    if(dump_output_immediately)
	return;

    move(LYlines-2,0);
    stop_reverse();
    clrtoeol();
    addstr(NOVICE_LINE_ONE);
    clrtoeol();
    addstr(NOVICE_LINE_TWO);

#ifdef NOT
    if(is_www_index && more) {
        addstr("This is a searchable index.  Use 's' to search:");
	stop_reverse();
	addstr("                ");
	start_reverse();
        addstr("space for more");

    } else if(is_www_index) {
        addstr("This is a searchable index.  Use 's' to search:");                   
    } else {
        addstr("Type a command or ? for help:");                   

        if(more) {
	    stop_reverse();
	    addstr("                       ");
	    start_reverse();
            addstr("Press space for next page");
	}
    }

#endif /* NOT */

    refresh();
}

PUBLIC int HTCheckForInterrupt()
{
#ifndef VMS
	/* UNIX stuff */
      static struct timeval socket_timeout;
      BOOLEAN first=TRUE;
      static int ret=0;
      static fd_set readfds;

      if(dump_output_immediately)
	  return(FALSE);

      if(first) {
          socket_timeout.tv_sec = 0;
          socket_timeout.tv_usec = 100;
      }

      FD_ZERO(&readfds);
      FD_SET(0, &readfds);
#ifdef __hpux
      ret = select(FD_SETSIZE, (int *)&readfds, NULL, NULL, &socket_timeout);
#else
      ret = select(FD_SETSIZE, &readfds, NULL, NULL, &socket_timeout);
#endif

      if(!FD_ISSET(0,&readfds)) {
	 return(FALSE); 

      } else {
         char cp;
	 if((cp=toupper(LYgetch())) == 'Z' || cp == 7)
	    return(TRUE);
	 else
	    return(FALSE);
      }

#else /* now do VMS stuff */

      extern BOOLEAN HadVMSInterrupt;
      extern int typeahead();
      char cp;

      /** Control-C or Control-Y and a 'N'o reply to exit query **/
      if (HadVMSInterrupt) {
         HadVMSInterrupt = FALSE;
         return((int)TRUE);
      }

      /** Keyboard 'Z' or 'z', or Control-G **/
      else if ((cp=toupper(typeahead())) == 'Z' || cp == 7)
         return((int)TRUE);

      /** Other or no keystrokes **/
      else
         return((int)FALSE);

#endif /* not VMS */
}


/* must recognize a URL and return the type.
 */
PUBLIC int is_url ARGS1(char *,filename)
{
    char *cp=filename;
    char *cp2;

    /* don't crash on an empty argument */
    if (cp == NULL || *cp == '\0')
        return(0);

    /* kill beginning spaces */
    while(isspace(*cp)) cp++;

    if(!strncmp(cp,"news:",5)) {
	return(NEWS_URL_TYPE);

    } else if(!strncmp(cp,"mailto:",7)) {
	return(MAILTO_URL_TYPE);

	/* special internal lynx type */
    } else if(!strncmp(cp,"LYNXPRINT:",9)) {
	return(LYNXPRINT_URL_TYPE);


	/* special internal lynx type */
    } else if(!strncmp(cp,"LYNXDOWNLOAD:",9)) {
	return(LYNXDOWNLOAD_URL_TYPE);

	/* special internal lynx type */
    } else if(!strncmp(cp,"LYNXHIST:",9)) {
	return(LYNXHIST_URL_TYPE);

	/* special internal lynx type to handle exec links :(((((((*/
    } else if(!strncmp(cp,"lynxexec:",9)) {
	return(LYNXEXEC_URL_TYPE);

    } else if(!strncmp(cp,"newspost:",9)) {
	return(NEWSPOST_URL_TYPE);

	/* if it doesn't contain ":/" then it can't be a url 
	 * except for the ones above here
	 */
    } else if(!strstr(cp+3,":/")) {  
	return(0);

    } else if(!strncmp(cp,"http",4)) {
	return(HTTP_URL_TYPE);

    } else if(!strncmp(cp,"file",4)) {
	return(FILE_URL_TYPE);

    } else if(!strncmp(cp,"gopher",6)) {
	if((cp2 = strchr(cp+11,'/')) != NULL) {

	    if(toupper(*(cp2+1)) == 'H' || *(cp2+1) == 'w')
		/* if this is a gopher html type */
	        return(HTML_GOPHER_URL_TYPE);
	    else if(*(cp2+1) == 'T' || *(cp2+1) == '8')
	        return(TELNET_GOPHER_URL_TYPE);
	    else if(*(cp2+1) == '7')
	        return(INDEX_GOPHER_URL_TYPE);
	    else
	        return(GOPHER_URL_TYPE);
	} else {
	    return(GOPHER_URL_TYPE);
	}

    } else if(!strncmp(cp,"ftp",3)) {
	return(FTP_URL_TYPE);

    } else if(!strncmp(cp,"wais",4)) {
	return(WAIS_URL_TYPE);

    } else if(!strncmp(cp,"telnet",6)) {
	return(TELNET_URL_TYPE);

    } else if(!strncmp(cp,"tn3270",6)) {
	return(TN3270_URL_TYPE);

    } else if(!strncmp(cp,"rlogin",6)) {
	return(RLOGIN_URL_TYPE);

    } else if(!strncmp(cp,"afs",3)) {
	return(AFS_URL_TYPE);

    } else if(!strncmp(cp,"prospero",8)) {
	return(PROSPERO_URL_TYPE);

    } else {
	return(0);
    }
}

/*
 * remove backslashes from any string
 */

PUBLIC void remove_backslashes ARGS1(char *,buf)
{
    char *cp;

    for (cp=buf; *cp != '\0' ; cp++) {

	if(*cp != '\\')  /* don't print slashes */
	   *buf = *cp, 
	   buf++;
	else if(*cp == '\\' &&  *(cp+1) == '\\') /*print one slash if there*/
	   *buf = *cp,                        /* are two in a row */
	   buf++;
    }
    *buf = '\0';
}

/*
 * checks to see if the current process is attached via a terminal in the
 * local domain
 *
 */
#ifdef VMS
#define NO_UTMP
#endif

#ifdef sgi
#define NO_UTMP
#endif

PUBLIC BOOLEAN inlocaldomain ()
{
#ifdef NO_UTMP
    return(FALSE);
#else
    int n;
    FILE *fp;
    struct utmp me;
    char *cp, *mytty=NULL;
    char *ttyname();

    if ((cp=ttyname(0)))
	mytty = strrchr(cp, '/');

    if (mytty && (fp=fopen(UTMP_FNAME, "r")) != NULL) {
	    mytty++;
	    do {
		n = fread((char *) &me, sizeof(struct utmp), 1, fp);
	    } while (n>0 && !STREQ(me.ut_line,mytty));
	    (void) fclose(fp);

	    if (n > 0 &&
	        strlen(me.ut_host) > strlen(LOCAL_DOMAIN) &&
	        STREQ(LOCAL_DOMAIN,
		  me.ut_host+strlen(me.ut_host)-strlen(LOCAL_DOMAIN)) )
		return(TRUE);

    }
    return(FALSE);
#endif /* NO_UTMP */
}

/**************
** This bit of code catches window size change signals
**/

#ifdef VMS
#define NO_SIZECHANGE
#endif
#ifdef SNAKE
#define NO_SIZECHANGE
#endif

#ifndef VMS
#include <sys/ioctl.h>
#endif

PUBLIC void size_change ARGS1(int,sig)
{
#ifndef NO_SIZECHANGE
#ifdef TIOCGSIZE
        struct ttysize win;
#else
#  ifdef TIOCGWINSZ
        struct winsize win;
#  endif
#endif

#ifdef TIOCGSIZE
        if (ioctl (0, TIOCGSIZE, &win) == 0) {
                if (win.ts_lines != 0) {
                        LYlines = win.ts_lines - 1;
                }
                if (win.ts_cols != 0) {
                        LYcols = win.ts_cols;
                }
        }
#else
#  ifdef TIOCGWINSZ
        if (ioctl (0, TIOCGWINSZ, &win) == 0) {
                if (win.ws_row != 0) {
                        LYlines = win.ws_row - 1;
                }
                if (win.ws_col != 0) {
                        LYcols = win.ws_col;
                }
        }
#  endif
#endif

#endif NO_SIZECHANGE
     recent_sizechange=TRUE; 
}

/*
 *  CHANGE_SUG_FILENAME -- Foteos Macrides 29-Dec-1993
 *	Upgraded for use with Lynx2.2 - FM 17-Jan-1994
 */

PUBLIC void change_sug_filename ARGS1(char *,fname)
{
     char    *cp, *cp1, *end;
#ifdef VMS
     char *dot;
     int j,k;
#endif /* VMS */

     /*** establish the current end of fname ***/
     end = fname + strlen(fname);

     /*** unescape fname ***/
     HTUnEscape(fname);

     /*** remove everything up the the last_slash if there is one ***/
     if((cp = strrchr(fname,'/')) != NULL && strlen(cp) > 1) {
	 cp1=fname;
	 cp++; /* go past the slash */
	 for(; *cp != '\0'; cp++, cp1++)
	    *cp1 = *cp;

	 *cp1 = '\0'; /* terminate */
     }

     /*** Trim off date-size suffix, if present ***/
     if ((*(end - 1) == ']') && ((cp = strrchr(fname, '[')) != NULL) &&
         (cp > fname) && *(--cp) == ' ')
	  while (*cp == ' ')
	       *(cp--) = '\0';

     /*** Trim off VMS device and/or directory specs, if present ***/
     if ((cp=strchr(fname,'[')) != NULL &&
         (cp1=strrchr(cp,']')) != NULL && strlen(cp1) > 1) {
	  cp1++;
	  for (cp=fname; *cp1 != '\0'; cp1++)
	       *(cp++) = *cp1;
	  *cp = '\0';
     }

#ifdef VMS
     /*** Replace illegal or problem characters ***/
     dot = fname + strlen(fname);
     for (cp = fname; cp < dot; cp++) {

	  /** Replace with underscores **/
	  if (*cp == ' ' || *cp == '/' || *cp == ':' ||
	      *cp == '[' || *cp == ']')
	       *cp = '_';

	  /** Replace with dashes **/
	  else if (*cp == '!' || *cp == '?' || *cp == '\'' || 
	           *cp == ',' || *cp == ':' || *cp == '\"' ||
	           *cp == '+' || *cp == '@' || *cp == '\\' ||
	           *cp == '(' || *cp == ')' || *cp == '=' ||
	           *cp == '<' || *cp == '>' || *cp == '#' ||
	           *cp == '%' || *cp == '*' || *cp == '`' ||
	           *cp == '~' || *cp == '^' || *cp == '|')
	       *cp = '-';
     }

     /** Collapse any serial underscores **/
     cp = fname + 1;
     j = 0;
     while (cp < dot) {
	  if (fname[j] == '_' && *cp == '_')
	       cp++;
	  else
	       fname[++j] = *cp++;
     }
     fname[++j] = '\0';

     /** Collapse any serial dashes **/
     dot = fname + (strlen(fname));
     cp = fname + 1;
     j = 0;
     while (cp < dot) {
          if (fname[j] == '-' && *cp == '-')
	       cp++;
	  else
	       fname[++j] = *cp++;
     }
     fname[++j] = '\0';

     /** Trim any trailing or leading **/
     /** underscrores or dashes       **/
     cp = fname + (strlen(fname)) - 1;
     while (*cp == '_' || *cp == '-')
          *cp-- = '\0';
     if (fname[0] == '_' || fname[0] == '-') {
          dot = fname + (strlen(fname));
          cp = fname;
          while ((*cp == '_' || *cp == '-') && cp < dot)
	       cp++;
	  j = 0;
          while (cp < dot)
	       fname[j++] = *cp++;
	  fname[j] = '\0';
     }

     /** Replace all but the last period with _'s, or second **/
     /** to last if last is followed by a terminal Z or z,   **/
     /** e.g., convert foo.tar.Z to                          **/
     /**               foo.tar_Z                             **/
     j = strlen(fname) - 1;
     if ((dot = strrchr(fname, '.')) != NULL) {
	  if (((fname[j] == 'Z' || fname[j] == 'z') && fname[j-1] == '.') &&
	      (((cp = strchr(fname, '.')) != NULL) && cp < dot)) {
	       *dot = '_';
	       dot = strrchr(fname, '.');
	  }
	  cp = fname;
	  while ((cp = strchr(cp, '.')) != NULL && cp < dot)
	       *cp = '_';

          /** But if the root is > 39 characters, move **/
          /** the period appropriately to the left     **/
	  while (dot - fname > 39) {
	       *dot = '\0';
	       if ((cp = strrchr(fname, '_')) != NULL) {
		    *cp  = '.';
		    *dot = '_';
	       } 
	       else if ((cp = strrchr(fname, '-')) != NULL) {
		    *cp  = '.';
		    *dot = '_';
	       }
	       else {
		    *dot = '_';
		    j = strlen(fname);
		    fname[j+1] = '\0';
		    while (j > 39)
			 fname[j--] = fname[j];
		    fname[j] = '.';
	       }
               dot = strrchr(fname, '.');
	  }

          /** Make sure the extension is < 40 characters **/
          if ((fname + strlen(fname) - dot) > 39)
	       *(dot+40) = '\0';

	  /** Trim trailing dashes or underscores **/
	  j = strlen(fname) - 1;
	  while (fname[j] == '_' || fname[j] == '-')
	       fname[j--] = '\0';
     }
     else {
	  /** No period, so put one on the end, or after   **/
	  /** the 39th character, trimming trailing dashes **/
	  /** or underscrores                              **/
	  if (strlen(fname) > 39)
	       fname[39] = '\0';
	  j = strlen(fname) - 1;
	  while ((fname[j] == '_') || (fname[j] == '-'))
	       j--;
	  fname[++j] = '.';
	  fname[++j] = '\0';
     }

#else /* not VMS  (UNIX) */
     /*** Replace problem characters ***/
     for (cp = fname; *cp != '\0'; cp++) {
	  switch (*cp) {
	  case '\'':
	  case '\"':
	  case '/':
	  case ' ':
	       *cp = '-';
	  }
     }
#endif /* VMS  (UNIX) */

     /** Make sure the rest of the original string in nulled. **/
     cp = fname + strlen(fname);
     while (cp < end)
          *cp++ = '\0';
}

/*
 *	To create standard temporary file names
 */
PUBLIC void tempname ARGS2(char *,namebuffer, int,action)
{
	static int counter = 0;


	if(action == REMOVE_FILES) { /* REMOVE ALL FILES */ 
	    for(; counter > 0; counter--) {
	        sprintf(namebuffer, "%sL%d%u.html", TEMP_SPACE, getpid(), 
								counter-1);
		remove(namebuffer);
	    }
	} else /* add a file */ {
	/*
	 * 	Create name
	 */
	    sprintf(namebuffer, "%sL%d%u.html", TEMP_SPACE, getpid(), 
								counter++);
	}
}

/* convert 4, 6, 2, 8 to left, right, down, up, etc. */
PUBLIC int number2arrows ARGS1(int,number)
{
      switch(number) {
            case '1':
                number=END;
                  break;
            case '2':
                number=DNARROW;
                  break;
            case '3':
                number=PGDOWN;
                  break;
            case '4':
                number=LTARROW;
                  break;
	    case '5':
		number=DO_NOTHING;
		break;
            case '6':
                number=RTARROW;
                  break;
            case '7':
                number=HOME;
                  break;
 	    case '8':
                number=UPARROW;
                  break;
            case '9':
                number=PGUP;
                  break;
      }

      return(number);
}

/*
 * parse_restrictions takes a string of comma-separated restrictions
 * and sets the corresponding flags to restrict the facilities available
 */
PRIVATE char *restrict_name[] = {
       "inside_telnet" ,
       "outside_telnet",
       "suspend"       ,
       "editor"        ,
       "shell"         ,
       "bookmark"      ,
       "option_save"   ,
       "print"         ,
       "download"      ,
       "disk_save"     ,
       "exec"          ,
       "exec_frozen"   ,
       "goto"          ,
       "file_url"      ,
       "news_post"     ,
       (char *) 0     };

	/* restrict_name and restrict_flag structure order
	 * must be maintained exactly!
	 */

PRIVATE BOOLEAN *restrict_flag[] = {
       &no_inside_telnet,
       &no_outside_telnet,
       &no_suspend  ,
       &no_editor   ,
       &no_shell    ,
       &no_bookmark ,
       &no_option_save,
       &no_print    ,
       &no_download ,
       &no_disk_save,
       &no_exec     ,
       &exec_frozen ,
       &no_goto     ,
       &no_file_url ,
       &no_newspost ,
       (BOOLEAN *) 0  };

PUBLIC void parse_restrictions ARGS1(char *,s)
{
      char *p;
      char *word;
      int i;

      if (STREQ("all", s)) {
	   /* set all restrictions */
          for(i=0; restrict_flag[i]; i++) 
              *restrict_flag[i] = TRUE;
          return;
      }

      if (STREQ("default", s)) {
	   /* set all restrictions */
          for(i=0; restrict_flag[i]; i++) 
              *restrict_flag[i] = TRUE;

	   /* reset these to defaults */
           no_inside_telnet = !(CAN_ANONYMOUS_INSIDE_DOMAIN_TELNET);
          no_outside_telnet = !(CAN_ANONYMOUS_OUTSIDE_DOMAIN_TELNET);
                   no_print = !(CAN_ANONYMOUS_PRINT);
		    no_goto = !(CAN_ANONYMOUS_GOTO);
#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
		    no_exec = LOCAL_EXECUTION_LINKS_ALWAYS_OFF_FOR_ANONYMOUS;
#endif
          return;
      }

      p = s;
      while (*p) {
          while (isspace(*p))
              p++;
          if (*p == '\0')
              break;
          word = p;
          while (*p != ',' && *p != '\0')
              p++;
          if (*p)
              *p++ = '\0';

	  for(i=0; restrict_name[i]; i++) 
             if(STREQ(word, restrict_name[i])) {
                *restrict_flag[i] = TRUE;
		break;
	     }
      }
}

#ifdef NEXT
/* no putenv on the next so we use this code instead!
 */

/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can  redistribute it and/or
modify it under the terms of the GNU Library General  Public License as
published by the Free Software Foundation; either  version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it  will be useful,
but WITHOUT ANY WARRANTY; without even the implied  warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library  General Public
License along with the GNU C Library; see the file  COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675  Mass Ave,
Cambridge, MA 02139, USA.  */

#include <sys/types.h>
#include <errno.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#else
extern int errno;
#endif

#if defined(STDC_HEADERS) || defined(USG)
#include <string.h>
#define index strchr
#define bcopy(s, d, n) memcpy((d), (s), (n))
#else /* not (STDC_HEADERS or USG) */
#include <strings.h>
#endif /* STDC_HEADERS or USG */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef NULL
#define NULL 0
#endif

#if !__STDC__
#define const
#endif

extern char **environ;

/* Put STRING, which is of the form "NAME=VALUE", in  the environment.  */
int
putenv (string)
     const char *string;
{
  char *name_end = index (string, '=');
  register size_t size;
  register char **ep;

  if (name_end == NULL)
    {
      /* Remove the variable from the environment.  */
      size = strlen (string);
      for (ep = environ; *ep != NULL; ++ep)
	if (!strncmp (*ep, string, size) && (*ep)[size]  == '=')
	  {
	    while (ep[1] != NULL)
	      {
		ep[0] = ep[1];
		++ep;
	      }
	    *ep = NULL;
	    return 0;
	  }
    }

  size = 0;
  for (ep = environ; *ep != NULL; ++ep)
    if (!strncmp (*ep, string, name_end - string) && (*ep)[name_end - string] == '=')
      break;
    else
      ++size;

  if (*ep == NULL)
    {
      static char **last_environ = NULL;
      char **new_environ = (char **) malloc ((size + 2)  * sizeof (char *));
      if (new_environ == NULL)
	return -1;
      (void) bcopy ((char *) environ, (char *)  new_environ, size * sizeof (char *));
      new_environ[size] = (char *) string;
      new_environ[size + 1] = NULL;
      if (last_environ != NULL)
	free ((char *) last_environ);
      last_environ = new_environ;
      environ = new_environ;
    }
  else
    *ep = (char *) string;

  return 0;
}
#endif NEXT
