/* ftprc.h */

#ifndef _ftprc_h_
#define _ftprc_h_

/*  $RCSfile: ftprc.h,v $
 *  $Revision: 1.4 $
 *  $Date: 1994/06/01 22:20:15 $
 */

#define NETRC "netrc"
#define FTPRC "ncftprc"

#define RC_DELIM " \n\t,"

typedef struct site *siteptr;
typedef struct site {
	char *name;			/* name (or IP address) of site */
	siteptr next;
} site;

typedef struct recentsite {
	char *name;			/* name (or IP address) of site */
	char *dir;			/* directory we were in last time we called. */
	time_t lastcall;	/* when this site was called last. */
} recentsite;

int thrash_rc(void);
void AddNewSitePtr(char *word);
int ruserpass2(char *host, char **user, char **pass, char **acct);
void GetFullSiteName(char *host, char *lastdir);
void ReadRecentSitesFile(void);
void WriteRecentSitesFile(void);
void AddRecentSite(char *host, char *lastdir);
void UpdateRecentSitesList(char *host, char *lastdir);
void PrintSiteList(void);

#endif
/* eof */
