/********************************************************************
 * lindner
 * 3.14
 * 1993/08/19 20:52:30
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/special.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: special.c
 * routines to deal with special types of files, compressed, scripts, etc.
 *********************************************************************
 * Revision History:
 * special.c,v
 * Revision 3.14  1993/08/19  20:52:30  lindner
 * Mitra comments
 *
 * Revision 3.13  1993/08/11  22:47:11  lindner
 * Don't let the security stuff trap ask blocks
 *
 * Revision 3.12  1993/08/06  14:30:49  lindner
 * Fixes for better security logging
 *
 * Revision 3.11  1993/08/04  22:14:54  lindner
 * Mods to use Gpopen
 *
 * Revision 3.10  1993/08/04  22:12:51  lindner
 * Mods to use Gpopen
 *
 * Revision 3.9  1993/07/27  05:27:59  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.8  1993/07/25  02:56:51  lindner
 * Fixed iscompressed() to return NULL
 *
 * Revision 3.7  1993/07/23  03:18:13  lindner
 * Mods for using decoder:'s
 *
 * Revision 3.6  1993/07/07  19:35:36  lindner
 * removed extra args to popen()
 *
 * Revision 3.5  1993/06/22  06:58:53  lindner
 * Added a little debug code..
 *
 * Revision 3.4  1993/06/11  16:46:50  lindner
 * Support for gzipped files
 *
 * Revision 3.3  1993/04/09  15:00:19  lindner
 * Fixes for ask shell scripts, ensure that they're run in the current
 * directory of the script.
 *
 * Revision 3.2  1993/03/24  20:24:23  lindner
 * Addition for compressed file support rfopenz()
 *
 * Revision 3.1.1.1  1993/02/11  18:02:53  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.3  1993/02/09  22:15:36  lindner
 * additions for askfile
 *
 * Revision 1.2  1993/01/30  23:57:44  lindner
 * Additions for ASK block support.
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include "gopherd.h"
#include "ext.h"
#include "Debug.h"

char *iscompressed();

/* Check to see if this file needs special treatment before heading
 * back to the client... We will check for:
 *      Encoded files           execute decoder...
 *	Shellscript		if so, "do it"
 *                              (add ask block params if exists..)
 * Note: it would be somewhat non-portable to check of a binary
 *  (we'd need to check for so many different magic numbers; the
 *  shell script designation should be sufficient, since the script
 *  can call an executable anyway
 * Recognized elsewhere:
 *	.snd			needs special processing on client
 *	uuencoded		needs special processing on client
 * Other filetypes we could check for:
 *	GIF		->	Bring up GIF previewer
 *	Postscript	->	Bring up Postscript previewer
 */

static int ispipe;

FILE *
specialfile(sockfd, fp, pathname)
  int sockfd;
  FILE *fp;
  char *pathname;
{
     FILE *pp;
     static char buf[256];
     char  s[256], *cp;
     long i;
     char *decoder;
     
     ispipe = 0;
     
     /* Keep track of where we are */
     i = ftell(fp);
     rewind(fp);
     
     /* Grab the first line or 254 bytes, and rewind */
     if (fgets(s, 255, fp) == NULL)
	  return (FILE *)0;

     fseek(fp, i, 0);
     
     /* Compressed? */
     if ((decoder = iscompressed(pathname)) != NULL) {
	  if (dochroot)
	       sprintf(buf, "%s '%s'", decoder, pathname);
	  else
	       sprintf(buf, "%s '%s/%s'", decoder, Data_Dir, pathname);

	  Debug("Executing decoder %s\n",buf);

	  pp = popen(buf, "r");

	  if (!pp)
	       return (FILE *)0;
	  ispipe = 1;
	  return pp;
     }

     /* Script? */
     if (isshellscript(s)) {
	  s[strlen(s)-1] = '\0';
	  if (dochroot)
	       sprintf(buf, "\"%s\" %s", pathname, (EXECargs == NULL) ? "" : EXECargs);
	  else
	       sprintf(buf, "\"%s/%s\" %s", Data_Dir, pathname, (EXECargs == NULL) ? "" : EXECargs);

	  if (ASKfile != NULL) {		/* Ick this is a global*/
	       strcat(buf, " < ");
	       strcat(buf, ASKfile);
	  }

	  /*
	   * Okay, let's change our working directory for the benefit of
	   * shell script writers everywhere :-)
	   */

	  strcpy(s, pathname);
	  cp = strrchr(s, '/');
	  if (cp != NULL) {
	       *cp = '\0';
	       ;
	  } else
	       strcpy(s, "/");
	  
	  rchdir(s);
	  
	  Debug("Executing %s\n", buf);

	  if (EXECargs) {
	       if (! (pp = Gpopen(sockfd, buf, "r")))
		    return (FILE *)0;
	  }
	  else {
	       if (! (pp = popen(buf, "r")))
		    return (FILE *)0;
	  }
		    
	  ispipe = 1;
	  
	  Debug("Zcat/popen is okay\n",0);
	  

	  return pp;
     }

     return (FILE *)0;
}


char *
iscompressed(pathname)
  char *pathname;
{
     static Extobj *ext = NULL;

     if (pathname == NULL || strlen(pathname)==0)
	  return(NULL);

     if (ext == NULL)
	  ext = EXnew();
     
     if (EXAcasedSearch(Config->Extensions, ext, pathname, EXT_DECODER))
	  return(EXgetDecoder(ext));

     return(NULL);
}

int
isshellscript(s)
  char *s;
{
     if (! strncmp(s, "#!/", 3))
	  return 1;
     else
	  return 0;
}


int
Specialclose(fp)
  FILE *fp;
{
     if (ASKfile != NULL)		/* Ick this is a global*/
	  unlink(ASKfile);
     if (ispipe)
	  return(pclose(fp));
     else
	  return(fclose(fp));
}


