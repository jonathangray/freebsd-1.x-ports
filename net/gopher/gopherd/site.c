/********************************************************************
 * lindner
 * 3.3
 * 1993/08/20 18:03:14
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/site.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: site.c
 * Routines to build up a table of hostnames and access levels
 *********************************************************************
 * Revision History:
 * site.c,v
 * Revision 3.3  1993/08/20  18:03:14  lindner
 * Mods to allow gopherd.conf files control ftp gateway access
 *
 * Revision 3.2  1993/07/27  05:27:57  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.1.1.1  1993/02/11  18:02:53  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2  1993/02/09  22:29:23  lindner
 * added util.h to includes
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include "site.h"
#include "Malloc.h"
#include "ctype.h"
#include "util.h"
#include "Debug.h"

static Site *
SiteNew()
{
     Site *temp;

     temp = (Site *) malloc(sizeof(Site));

     if (temp == NULL)
	  return(NULL);

     temp->domain = STRnew();
     STRinit(temp->domain);

     temp->Level = ACC_FULL;
     temp->isnum = FALSE;

     return(temp);
}

static void 
SiteDestroy(site)
  Site *site;
{
     STRdestroy(site->domain);
     free(site);
}

static void
Sitecpy(site1, site2)
  Site *site1, *site2;
{
     STRcpy(site1->domain, site2->domain);
     
     site1->Level = site2->Level;
     site1->isnum = site2->isnum;
}

static void
SiteSet(site, dom, access)
  Site *site;
  char *dom;
  Accesslevel access;
{
     STRset(site->domain, dom);
     site->Level = access;
     
     if (isdigit(*dom))
	  site->isnum = TRUE;
     else
	  site->isnum = FALSE;
}

/******************************************************/

SiteArray *
SiteArrayNew()
{
     SiteArray *temp;
     
     temp = DAnew(20, SiteNew, NULL, SiteDestroy, Sitecpy);
     
     return(temp);
}

void
SiteArrayAdd(sitearr, name, Level)
  SiteArray *sitearr;
  char *name;
  Accesslevel Level;
{
     Site *temp;

     temp = SiteNew();

     SiteSet(temp, name, Level);

     SiteArrPush(sitearr, temp);
     return;
}


Accesslevel
SiteAccess(sitearr, name)
  SiteArray *sitearr;
  char *name;
{
     int i;
     Site *temp;

     if (name == NULL)
	  return(ACC_UNKNOWN);  /*** ??? We need to compare *something* ***/

     for (i=0; i< DAgetTop(sitearr); i++) {
	  
	  temp = SiteArrgetEntry(sitearr, i);

	  Debug("Testing for %s\n", STRget(temp->domain));

	  if (temp->isnum == TRUE) {
	       /*** Check for a match from the beginning ***/
	       int namelen, domainlen;

	       namelen = strlen(name);
	       domainlen = strlen(STRget(temp->domain));

	       if (namelen >domainlen)
		    namelen = domainlen;

	       if (strncmp(name, STRget(temp->domain), namelen) == 0)
		    return(temp->Level);
	  } else {
	       /*** Check for a match from the end ***/
	       /*** It's a domain name ***/
	       int namelen, domainlen;

	       namelen = strlen(name);
	       domainlen = strlen(STRget(temp->domain));


	       /*** don't compare if incoming name is shorter than domain ***/
	       if (domainlen <= namelen)
		    if (strcasecmp((name+namelen-domainlen), 
				   STRget(temp->domain))==0)
			 return(temp->Level);
	       
	  }
     }

     /*** Hmmm, didn't find a match, return -1 ***/
     return(ACC_UNKNOWN);
}


boolean
SiteArrCanRead(sitearr, hostname, ipnum)
  SiteArray *sitearr;
  char *hostname, *ipnum;
{
     Accesslevel level;

     if (hostname != NULL) {
	  level = SiteAccess(sitearr, hostname);
	  Debug("Host siteaccess %d\n", level);
	  if ((level & ACC_READ) == ACC_READ && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }
     
     /** Test the ipnum second **/
     if (ipnum != NULL) {
	  level = SiteAccess(sitearr, ipnum);
	  Debug("IPnum siteaccess %d\n", level);
	  if ((level & ACC_READ) == ACC_READ && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }

     /*** No matches found, so let's return the default ***/

     return(ACC_UNKNOWN);
}



boolean
SiteArrCanBrowse(sitearr, hostname, ipnum)
  SiteArray *sitearr;
  char *hostname, *ipnum;
{
     Accesslevel level;

     if (hostname != NULL) {
	  level = SiteAccess(sitearr, hostname);
	  Debug("Host siteaccess %d\n", level);
	  if ((level & ACC_BROWSE) == ACC_BROWSE && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }
     
     /** Test the ipnum second **/
     if (ipnum != NULL) {
	  level = SiteAccess(sitearr, ipnum);
	  Debug("IPnum siteaccess %d\n", level);
	  if ((level & ACC_BROWSE) == ACC_BROWSE && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }

     Debug("No matches...\n",0);
     /*** No matches found, so let's return unknown ***/
     return(ACC_UNKNOWN);
}



boolean
SiteArrCanSearch(sitearr, hostname, ipnum)
  SiteArray *sitearr;
  char *hostname, *ipnum;
{
     Accesslevel level;

     if (hostname != NULL) {
	  level = SiteAccess(sitearr, hostname);
	  Debug("Host siteaccess %d\n", level);
	  if ((level & ACC_SEARCH) == ACC_SEARCH && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }
     
     /** Test the ipnum second **/
     if (ipnum != NULL) {
	  level = SiteAccess(sitearr, ipnum);
	  Debug("IPnum siteaccess %d\n", level);
	  if ((level & ACC_SEARCH) == ACC_SEARCH && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }

     Debug("No matches...\n",0);
     /*** No matches found, so let's return the default ***/

     return(ACC_UNKNOWN);

}


boolean
SiteArrCanFTP(sitearr, hostname, ipnum)
  SiteArray *sitearr;
  char *hostname, *ipnum;
{
     Accesslevel level;

     if (hostname != NULL) {
	  level = SiteAccess(sitearr, hostname);
	  Debug("Host siteaccess %d\n", level);
	  if ((level & ACC_FTP) == ACC_FTP && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }
     
     /** Test the ipnum second **/
     if (ipnum != NULL) {
	  level = SiteAccess(sitearr, ipnum);
	  Debug("IPnum siteaccess %d\n", level);
	  if ((level & ACC_FTP) == ACC_FTP && level != ACC_UNKNOWN)
	       return(TRUE);
	  else if (level != ACC_UNKNOWN)
	       return(FALSE);
     }

     Debug("No matches...\n",0);
     /*** No matches found, so let's return the default ***/

     return(ACC_UNKNOWN);

}


/*
 * process a site description line.
 */

boolean
SiteProcessLine(sitearr, inputline, defaccess)
  SiteArray   *sitearr;
  char        *inputline;
  Accesslevel defaccess;
{
     char name[256];
     char *namep = name;
     Accesslevel level = defaccess;
     
     /*** the first word is the domain/ip# ***/
     while (*inputline == ' ' || *inputline == '\t')
	  inputline++;

     while (*inputline != ' ' && *inputline != '\t') {
	  *namep = *inputline;
	  namep++;
	  inputline++;
     }
     
     *namep = '\0';  /*** Terminate it ***/

     if (namep == name)
	  return(FALSE); /*** bad line ***/
     
     while (*inputline != '\0') {

	  if (*inputline == ' ' || *inputline == '\t' || *inputline == ',') {
	       inputline++;
	       if (*inputline == '!')
		    inputline++;

	       switch (*inputline) {

	       case 'b':
		    /** Browse **/
		    if (*(inputline -1) == '!')
			 level &= (ACC_SEARCH | ACC_READ | ACC_FTP);
		    else
			 level |= ACC_BROWSE;
		    break;
	       case 'r':
		    /*** Read ***/
		    if (*(inputline -1) == '!')
			 level &= (ACC_BROWSE | ACC_SEARCH | ACC_FTP);
		    else
			 level |= ACC_READ;
		    break;

	       case 's':
		    /*** Search ***/
		    if (*(inputline -1) == '!')
			 level &= (ACC_BROWSE | ACC_READ | ACC_FTP);
		    else
			 level |= ACC_SEARCH;
		    break;

	       case 'f':
		    /*** FTP ***/
		    if (*(inputline -1) == '!')
			 level &= (ACC_BROWSE | ACC_READ | ACC_SEARCH);
		    else
			 level |= ACC_FTP;
		    break;
	       }
	  } else
	       inputline++;
	  
     }

     SiteArrayAdd(sitearr, name, level);
     return(TRUE);
}

