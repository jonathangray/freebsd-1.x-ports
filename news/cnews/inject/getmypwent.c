/*
 * getmypwent - return password file entry for invoking user,
 *	using login name first, if any, then uid.
 */

#include <stdio.h>
#include <pwd.h>

/* imports */
extern char *getlogin();
extern struct passwd *getpwuid(), *getpwnam();

/*
 * Note: this is the canonical way to determine your userid,
 * as per V7's getlogin(3).  It sure would be nice if everyone
 * who does only half the job were to do the whole job.
 */
struct passwd *
getmypwent()
{
	register char *login = getlogin();

	return login == NULL? getpwuid(getuid()): getpwnam(login);
}
