/********************************************************************
 * lindner
 * 3.7
 * 1993/08/23 18:46:17
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/gopherdconf.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: gopherdconf.c
 * Routines to parse the gopherd.conf file.
 *********************************************************************
 * Revision History:
 * gopherdconf.c,v
 * Revision 3.7  1993/08/23  18:46:17  lindner
 * Crude addition of a veronica top-level block
 *
 * Revision 3.6  1993/08/20  18:03:07  lindner
 * Mods to allow gopherd.conf files control ftp gateway access
 *
 * Revision 3.5  1993/07/27  05:27:49  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.4  1993/07/23  03:19:20  lindner
 * Mods for using decoder:'s
 *
 * Revision 3.3  1993/04/15  17:09:22  lindner
 * none
 *
 * Revision 3.2  1993/03/24  20:25:14  lindner
 * Addition for secureusers file
 *
 * Revision 3.1.1.1  1993/02/11  18:02:51  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.3  1993/02/09  22:30:56  lindner
 * Additions for multi-languages
 *
 * Revision 1.2  1993/01/30  23:57:44  lindner
 * Gopher+ stuff
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


#include "gopherdconf.h"
#include "Malloc.h"
#include "String.h"
#include <stdio.h>
#include "util.h"
#include "Debug.h"

/*********************************************/

GDCobj *
GDCnew()
{
     GDCobj *gdc;

     gdc = (GDCobj *) malloc(sizeof(GDCobj));

     gdc->Extensions = EXAnew();

     gdc->Sites      = SiteArrayNew();
     gdc->Securityon   = FALSE;

     gdc->RunFromInetd = FALSE;
     gdc->Caching      = TRUE;

     gdc->Logfile      = STRnew();
     gdc->Data_Dir     = STRnew();
     gdc->Hostname     = STRnew();
     gdc->Port         = 0;
     gdc->chroot       = TRUE;
     gdc->Defaccess    = ACC_FULL;
     gdc->BummerMsg    = STRnew();
     
     gdc->Admin        = STRnew();
     gdc->AdminEmail   = STRnew();

     gdc->Site         = STRnew();
     gdc->Org          = STRnew();
     gdc->Geog         = STRnew();
     gdc->Loc          = STRnew();
     gdc->Lang         = STRnew();
     gdc->TZ           = 0;
     gdc->VeronicaIndex = TRUE;

     gdc->Tixfile      = STRnew();

     STRset(gdc->Logfile, "");
     STRset(gdc->BummerMsg, "");
     STRset(gdc->Hostname, "");

     return(gdc);
}

void
GDCdestroy(gdc)
  GDCobj *gdc;
{
     EXAdestroy(gdc->Extensions);
     SiteArrDestroy(gdc->Sites);

     STRdestroy(gdc->Logfile);
     STRdestroy(gdc->BummerMsg);
     STRdestroy(gdc->Hostname);
     STRdestroy(gdc->Lang);
     STRdestroy(gdc->Tixfile);
     /** etc..**/

     free(gdc);
}


/*
 * Parse Gopherd.conf tokens..
 */

static
boolean
GDCtokens(gdc, token, rest)
  GDCobj *gdc;
  char *token;
  char *rest;
{
     boolean success = TRUE;

     if (strcasecmp(token, "ACCESS") == 0) {
	  int moo;
	  success = SiteProcessLine(gdc->Sites, rest, gdc->Defaccess);
	  moo = SiteAccess(gdc->Sites, "default");
	  if (moo != ACC_UNKNOWN)
	       gdc->Defaccess = moo;

	  gdc->Securityon = TRUE;
     }

     else if (strcasecmp(token, "ADMIN")==0)
	  GDCsetAdmin(gdc, rest);
     else if (strcasecmp(token, "ADMINEMAIL")==0)
	  GDCsetAdminEmail(gdc, rest);
     else if (strcasecmp(token, "HOSTALIAS")==0)
	  GDCsetHostname(gdc, rest);
     else if (strcasecmp(token, "SITE")==0)
	  GDCsetSite(gdc, rest);
     else if (strcasecmp(token, "ORG")==0)
	  GDCsetOrg(gdc, rest);
     else if (strcasecmp(token, "LOC")==0)
	  GDCsetLoc(gdc, rest);
     else if (strcasecmp(token, "LOGFILE")==0)
	  GDCsetLogfile(gdc, rest);
     else if (strcasecmp(token, "GEOG")==0)
	  GDCsetGeog(gdc, rest);
     else if (strcasecmp(token, "BUMMERMSG")==0)
	  GDCsetBummerMsg(gdc, rest);
     else if (strcasecmp(token, "LANGUAGE")==0) {
	  rest = skip_whitespace(rest);
	  GDCsetLang(gdc, rest);
     }
     else if (strcasecmp(token, "VIEWEXT")==0) {
	  success = EXAprocessLine(gdc->Extensions, EXT_VIEW, rest, GDCgetLang(gdc));
     }
     else if (strcasecmp(token, "BLOCKEXT")==0) {
	  success = EXAprocessLine(gdc->Extensions, EXT_BLOCK, rest, NULL);
     }
     else if (strcasecmp(token, "BLOCKREFEXT")==0) {
	  success = EXAprocessLine(gdc->Extensions, EXT_BLOCKREF, rest, NULL);
     }
     else if (strcasecmp(token, "IGNORE")==0) {
	  success = EXAprocessLine(gdc->Extensions, EXT_IGNORE, rest, NULL);
     }
     else if (strcasecmp(token, "DECODER")==0) {
	  success = EXAprocessLine(gdc->Extensions, EXT_DECODER, rest, NULL);
     }
     else if (strcasecmp(token, "SECUREUSERS")==0) {
	  GDCsetTixfile(gdc, rest);
	  /*** Try to open the file ***/
	  /*** TODO ***/
	  ;
     }
     else if (strcasecmp(token, "VERONICAINDEX")==0) {
	  if (strcasecmp(rest, "no")==0)
	       GDCsetShouldIndex(gdc, FALSE);
	  else
	       GDCsetShouldIndex(gdc, TRUE);
     }
     else
	  success = FALSE;
     
     return(success);
}

void
GDCfromFile(gdc, filename)
  GDCobj *gdc;
  char *filename;
{
     FILE *gdcfile;
     char inputline[256];
     char *cp, *token, *restofline;
     boolean success;


     if ((gdcfile = fopen(filename, "r")) == (FILE *) NULL) {
	  printf("Cannot open file '%s'\n", filename);
	  exit(-1);
     }

     while (fgets(inputline, sizeof inputline, gdcfile)!= NULL) {
	  ZapCRLF(inputline);
	  
	  if (*inputline == '#' || *inputline == '\0') /** Ignore comments **/
	       continue;

	  cp = strchr(inputline, ':');
	  if (cp == NULL) {
	       fprintf(stderr, "Bad line '%s'\n", inputline);
	       exit(-1);
	  }
	  *cp = '\0';
	  token      = inputline;
	  restofline = cp+1;
	  while (*restofline == ' ' || *restofline == '\t')
	       restofline++;
	  
	  success = GDCtokens(gdc, token, restofline);
	  if (!success) {
	       fprintf(stderr, "Bad line '%s'\n", inputline);
	       exit(-1);
	  }
     }
     
     fclose(gdcfile);
}


boolean GDCCanSearch(gdc, hostname, ipnum)
  GDCobj *gdc;
  char *hostname, *ipnum;
{
     boolean test;

     Debug("Testing %s/", hostname);
     Debug("%s for searching", ipnum);

     if (gdc->Securityon == FALSE)
	  return(TRUE);

     if ((test = SiteArrCanSearch(gdc->Sites, hostname, ipnum)) != ACC_UNKNOWN)
	  return(test);
     
     if ((gdc->Defaccess & ACC_SEARCH) == ACC_SEARCH)
	  return(TRUE);
     else
	  return(FALSE);

}

boolean
GDCCanRead(gdc, hostname, ipnum)
  GDCobj *gdc;
  char *hostname, *ipnum;
{
     boolean test;

     Debug("Testing %s/", hostname);
     Debug("%s for reading", ipnum);

     if (gdc->Securityon == FALSE)
	  return(TRUE);

     if ((test = SiteArrCanRead(gdc->Sites, hostname, ipnum)) != ACC_UNKNOWN)
	  return(test);
     
     if ((gdc->Defaccess & ACC_READ) == ACC_READ)
	  return(TRUE);
     else
	  return(FALSE);

}


boolean 
GDCCanBrowse(gdc, hostname, ipnum)
  GDCobj *gdc;
  char *hostname, *ipnum;
{
     boolean test;

     Debug("Testing %s/", hostname);
     Debug("%s for browsing", ipnum);

     if (gdc->Securityon == FALSE)
	  return(TRUE);

     if ((test = SiteArrCanBrowse(gdc->Sites, hostname, ipnum)) != ACC_UNKNOWN)
	  return(test);
     
     if ((gdc->Defaccess & ACC_BROWSE) == ACC_BROWSE)
	  return(TRUE);
     else
	  return(FALSE);

}


boolean 
GDCCanFTP(gdc, hostname, ipnum)
  GDCobj *gdc;
  char *hostname, *ipnum;
{
     boolean test;

     Debug("Testing %s/", hostname);
     Debug("%s for ftping", ipnum);

     if (gdc->Securityon == FALSE)
	  return(TRUE);

     if ((test = SiteArrCanFTP(gdc->Sites, hostname, ipnum)) != ACC_UNKNOWN)
	  return(test);
     
     if ((gdc->Defaccess & ACC_FTP) == ACC_FTP)
	  return(TRUE);
     else
	  return(FALSE);

}


/** Is this file ignored? **/
boolean
GDCignore(gdc, fname)
  GDCobj *gdc;
  char *fname;
{
     Extobj *ext;

     ext = EXnew();

     if (EXAsearch(gdc->Extensions, ext, fname, EXT_IGNORE)) {
	  EXdestroy(ext);
	  return(TRUE);
     }
     else
	  EXdestroy(ext);
	  return(FALSE);

}


boolean 
GDCViewExtension(gdc, fext, extin)
  GDCobj *gdc;
  char *fext;
  Extobj **extin;
{
     Extobj *ext;
     char *gplustype;
     
     ext = EXnew();

     if (EXAsearch(gdc->Extensions, ext, fext, EXT_VIEW)) {
	  *extin = ext;
	  return(TRUE);
     }
     else
	  return(FALSE);

}


boolean 
GDCBlockExtension(gdc, fext, ext)
  GDCobj *gdc;
  char *fext;
  Extobj *ext;
{

     if (EXAsearch(gdc->Extensions, ext, fext, EXT_BLOCK)) {
	  return(TRUE);
     }
     else
	  return(FALSE);
}
