/* 
 * postauth: do authorization handshake when posting requires it.
 * Originally by Brian Kantor. Some modifications by Stan Barber.
 */
#ifndef lint
static char * rcsid = "$Header: /a/cvs/386BSD/ports/news/nntp/inews/postauth.c,v 1.1 1993/07/19 20:04:29 nate Exp $";
#endif
#include <stdio.h>
#include "../common/conf.h"
#include "../common/nntp.h"

#ifdef AUTH

extern FILE *passfile;

postauth(host)
char *host;
	{
	char remote[64], user[16], pass[16];
	char buf[1024];
	int i;

	if (passfile == NULL)
		{
		fprintf(stderr,"Posting is not allowed from this system.\n");
		exit(1);
		}

	while(fgets(buf, sizeof(buf), passfile))
		{
		if (buf[0] == '#')
			continue;
		
		i = sscanf(buf,"%s %s %s", remote, user, pass);
		/* malformed entry? */
		if (i != 3)
			{
			fprintf(stderr,"Posting Authorization Denied. File format error.\n");
			continue;
			}
		
		/* right host? */
		if (!strcasecmp(remote,host))
			break;
		}
	if (feof(passfile))
		{
		fprintf(stderr,"Posting to %s is not allowed from this system\n", host);
		exit(1);
		}
	
	sprintf(buf,"authinfo user %s", user);
	if (converse(buf, sizeof(buf)) != NEED_AUTHDATA)
		{
		fprintf(stderr,"%s\n", buf);
		exit(1);
		}
	
	sprintf(buf,"authinfo pass %s", pass);
	if (converse(buf, sizeof(buf)) != OK_AUTH)
		{
		fprintf(stderr,"%s\n", buf);
		exit(1);
		}
	
	fclose(passfile);
	}

int
converse(buf,buflen)
char *buf;
int buflen;
	{
	put_server(buf);
	get_server(buf,buflen);
	return(atoi(buf));
	}
#endif AUTH
