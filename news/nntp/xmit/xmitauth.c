#ifndef lint
static char *rcsid = "$Header: /a/cvs/386BSD/ports/news/nntp/xmit/xmitauth.c,v 1.1 1993/07/19 20:04:35 nate Exp $";
#endif
#include <stdio.h>
#include "../common/conf.h"
#include "../common/nntp.h"
#include "nntpxmit.h"

#ifdef AUTH

extern char Debug;
extern int converse();

FILE *sys;

xmitauth(host)
char *host;
	{
	char remote[64], user[16], pass[16];
	char buf[1024];
	int i;
	char savedebug;

	sys = fopen(PASSFILE, "r");
	if (sys == NULL)
		{
		exit(1);
		}
	
	while(fgets(buf, sizeof(buf), sys))
		{
		if (buf[0] == '#')
			continue;
		
		i = sscanf(buf,"%s %s %s", remote, user, pass);
		/* malformed entry? */
		if (i != 3)
			{
			log(L_NOTICE,"malformed entry in nntp.sys");
			continue;
			}
		
		/* right host? */
		if (!strcasecmp(remote,host))
			break;
		}
	if (feof(sys))
		{
		sprintf(buf,"host %s authinfo not in nntp.sys", host);
		log(L_NOTICE, buf);
		exit(1);
		}
	
	sprintf(buf,"authinfo user %s", user);
	if (converse(buf, sizeof(buf)) != NEED_AUTHDATA)
		{
		log(L_NOTICE, buf);
		exit(1);
		}
	
	/* don't display the password even if debug is on */
	savedebug = Debug;
	Debug = FALSE;

	sprintf(buf,"authinfo pass %s", pass);
	if (converse(buf, sizeof(buf)) != OK_AUTH)
		{
		log(L_NOTICE, buf);
		exit(1);
		}
	
	Debug = savedebug;

	fclose(sys);
	}

#endif AUTH

