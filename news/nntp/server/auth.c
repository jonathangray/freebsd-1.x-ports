#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/auth.c,v 1.1 1993/07/19 20:04:30 nate Exp $";
#endif

/*
 * Simple user/password authentication
 *
 * client must supply "authinfo user <userid>"
 * followed by "authinfo pass <password>"
 * which will be looked up in the server machine's password file.
 * Password must match userid, userid must have gid matching the
 * /etc/group entry for "nntp"
 *
 * note that passwords travel over the network in plaintext.  This
 * can be a problem but isn't easy to remedy.  At least it's as safe
 * as logging in over the network would be
 *
 */

#include "common.h"
#include <grp.h>

extern timeout();
extern char *crypt();

#ifdef	AUTH

extern int	Needauth;
extern char	User[];

doauth(argc,argv)
int argc;
char *argv[];
	{
	if (argc != 3)
		{
		printf("%d Syntax error\r\n", ERR_CMDSYN);
		fflush(stdout);
		return;
		}

	if (!strcasecmp(argv[0],"authcap"))
		{
		printf("%d authcap not implemented\r\n", ERR_COMMAND);
		fflush(stdout);
		return;
		}

	if (!strcasecmp(argv[0],"authsys"))
		{
		printf("%d authsys not implemented\r\n", ERR_COMMAND);
		fflush(stdout);
		return;
		}

	/* fall through into 'authinfo' */
	if (strcasecmp(argv[0],"authinfo"))
		{
		printf("%d command not recognized\r\n", ERR_COMMAND);
		fflush(stdout);
		return;
		}
	
	if (!Needauth)
		{
		printf("%d Authorization already completed\r\n", ERR_AUTHREJ);
		fflush(stdout);
		return;
		}

	if (!strcasecmp(argv[1],"user"))
		{
		if (strlen(User))
			{
			printf("%d USER already specified\r\n", ERR_AUTHREJ);
			fflush(stdout);
			return;
			}
		getuser(argv[2]);
		return;
		}

	if (!strcasecmp(argv[1],"pass"))
		{
		if (strlen(User) < 1)
			{
			printf("%d USER required first\r\n", ERR_AUTHREJ);
			fflush(stdout);
			return;
			}
		getpass(argv[2]);
		return;
		}
	}

/* get userid and prompt for password */
getuser(p)
char *p;
{
	strncpy(User,p,8);
	User[8] = 0;
	/* get the password */
	printf("%d PASS required\r\n", NEED_AUTHDATA);
	fflush(stdout);
}

/* password */
getpass(p)
char *p;
{
	static char pass[10];
	char *cp, *namep;
	struct passwd *pwd;
	struct group *grp;
	int i;
	int nntpgid;

	strncpy(pass,p,8);
	pass[8] = 0;
	/* check for valid login */
	pwd = getpwnam(User);
	namep = NULL;

	if (pwd != NULL)
		namep = crypt(pass, pwd->pw_passwd);
	
	grp = getgrnam("nntp");

	if (grp == NULL || pwd == NULL || namep == NULL
			|| strcmp(namep, pwd->pw_passwd)
			|| pwd->pw_gid != grp->gr_gid)
		{
#ifdef SYSLOG
		syslog(LOG_ERR, "AUTHENTICATION ERROR");
#endif
		printf("%d Authentication error\r\n",ERR_ACCESS);
		(void) fflush(stdout);
		(void) fflush(stdout);
		exit(1);
		}

#ifdef SYSLOG
#ifdef LOG
	syslog(LOG_INFO, "user %s", User);
#endif
#endif
	printf("%d Authentication accepted\r\n",OK_AUTH);
	fflush(stdout);
	Needauth = 0;
}

#endif AUTH
