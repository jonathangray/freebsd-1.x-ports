#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/access.c,v 1.1 1993/07/19 20:04:29 nate Exp $";
#endif

#include "common.h"
#ifdef EXCELAN
#include <netinet/in.h>
#endif
#include <sys/socket.h>

#define	SNETMATCH	1
#define	NETMATCH	2

/*
 * host_access -- determine if the client has permission to
 * read, transfer, and/or post news.  read->transfer.
 * We switch on socket family so as to isolate network dependent code.
 *
 *	Parameters:	"canread" is a pointer to storage for
 *			an integer, which we set to 1 if the
 *			client can read news, 0 otherwise.
 *
 *			"canpost" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can post news, 0 otherwise.
 *
 *			"canxfer" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can transfer news, 0 otherwise.
 *
 *			"gdlist" is a comma separated list of
 *			newsgroups/distributions which the client
 *			can access.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	None.
 */

#ifdef EXCELAN
extern struct sockaddr_in current_peer;
#endif


#ifdef AUTH
extern	int Needauth;
#endif AUTH

host_access(canread, canpost, canxfer, gdlist)
	int		*canread, *canpost, *canxfer;
	char		*gdlist;
{
	int		sockt;
	int		length;
	struct sockaddr	sa;
	int		match = 0;
	int		count;
	char		hostornet[MAXHOSTNAMELEN];
	char		host_name[MAXHOSTNAMELEN];
	char		net_name[MAXHOSTNAMELEN];
	char		snet_name[MAXHOSTNAMELEN];
	char		readperm[MAXBUFLEN];
	char		postperm[MAXBUFLEN];
	char		groups[MAXBUFLEN];
	char		line[MAXBUFLEN];
	register char	*cp;
	register FILE	*acs_fp;

	gdlist[0] = '\0';

#ifdef DEBUG
	*canread = *canpost = *canxfer = 1;
	return;
#endif

	*canread = *canpost = *canxfer = 0;

	sockt = fileno(stdin);
	length = sizeof (sa);

#ifdef EXCELAN
	if (raddr(current_peer.sin_addr) == NULL) {
#else
	if (getpeername(sockt, &sa, &length) < 0) {
#endif
		if (isatty(sockt)) {
			(void) strcpy(hostname, "stdin");
			*canread = 1;
		} else {
#ifdef SYSLOG
			syslog(LOG_ERR, "host_access: getpeername: %m");
#endif
			(void) strcpy(hostname, "unknown");
		}
		return;
	}
#ifdef EXCELAN
	else bcopy(&current_peer,&sa,length);
#endif

	switch (sa.sa_family) {
	case AF_INET:
		inet_netnames(sockt, &sa, net_name, snet_name, host_name);
		break;

#ifdef DECNET
	case AF_DECnet:
		dnet_netnames(sockt, &sa, net_name, snet_name, host_name);
		break;
#endif

	default:
#ifdef SYSLOG
		syslog(LOG_ERR, "unknown address family %ld", sa.sa_family);
#endif
		return;
	};

	/* Normalize host name to lower case */

	for (cp = host_name; *cp; cp++)
		if (isupper(*cp))
			*cp = tolower(*cp);

#ifdef LOG
	syslog(LOG_INFO, "%s connect\n", host_name);
#endif
	(void) strcpy(hostname, host_name);

	/*
	 * We now we have host_name, snet_name, and net_name.
	 * Our strategy at this point is:
	 *
	 * for each line, get the first word
	 *
	 *	If it matches "host_name", we have a direct
	 *		match; parse and return.
	 *
	 *	If it matches "snet_name", we have a subnet match;
	 *		parse and set flags.
	 *
	 *	If it matches "net_name", we have a net match;
	 *		parse and set flags.
	 *
	 *	If it matches the literal "default", note we have
	 *		a net match; parse.
	 */

	acs_fp = fopen(accessfile, "r");
	if (acs_fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "access: fopen %s: %m", accessfile);
#endif
		return;
	}

	while (fgets(line, sizeof(line), acs_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if ((cp = index(line, '#')) != NULL)
			*cp = '\0';
		if (*line == '\0')
			continue;

		count = sscanf(line, "%s %s %s %s",
				hostornet, readperm, postperm, groups);

		if (count < 4) {
			if (count < 3)
				continue;
			groups[0] = '\0';	/* No groups specified */
		}
#ifdef DOMAINMATCH
 		if (domainmatch(hostornet,host_name)) {
#else
		if (!strcasecmp(hostornet, host_name)) {
#endif
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (readperm[0] == 'X'
					     || readperm[0] == 'x');
			if (readperm[0] == 'B' || readperm[0] == 'b')
				*canxfer = *canread = 1;
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
			break;
		}

		if (*snet_name && !strcasecmp(hostornet, snet_name)) {
			match = SNETMATCH;
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (readperm[0] == 'X'
					     || readperm[0] == 'x');
			if (readperm[0] == 'B' || readperm[0] == 'b')
				*canxfer = *canread = 1;
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
		}

		if (match != SNETMATCH && (!strcasecmp(hostornet, net_name) ||
		    !strcasecmp(hostornet, "default"))) {
			match = NETMATCH;
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (readperm[0] == 'X'
					     || readperm[0] == 'x');
			if (readperm[0] == 'B' || readperm[0] == 'b')
				*canxfer = *canread = 1;
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
		}
	}
/*
 * The access check expects there to be spaces between the group names.
 * In the access file, there are commas between the groupnames.
 * Here, we change the commas to spaces.
 */
         {
 	  char *pointer=gdlist;
 	  
 	  while (*pointer)
 	    {
 	      if (*pointer == ',') *pointer=' ';
 	      pointer++;
 	    }
 	}

	(void) fclose(acs_fp);

#ifdef AUTH
	Needauth = 0;
	/* do we require a userid and password for this guy? */
	if (isupper(readperm[0]) || isupper(postperm[0]))
		Needauth = 1;
#endif AUTH
}

#ifdef DOMAINMATCH

domainmatch(domainsuffix,hostname)
char *domainsuffix,*hostname;
{
 	char *i;
	int dlen;
#ifdef SYSLOG
	char * lineptr;
	lineptr = domainsuffix;
#endif

	if (!strcasecmp(domainsuffix,hostname)) 
		return (1);

	if (*domainsuffix++ != '*')
		return (0);

	if (*domainsuffix++ != '.' ){
#ifdef SYSLOG
		syslog(LOG_ERR, "%s: no period following asterisk: %s",
			 accessfile, lineptr);
#endif
		return (0);
	}
	dlen = strlen(domainsuffix);

	hostname += (strlen(hostname)-strlen(domainsuffix));

	if (!strcasecmp(domainsuffix,hostname)) 
		return (1);

	return (0);
}
#endif DOMAINMATCH
