/* msgid -- message ID test
 * vix 13feb91 [negative caching]
 * vix 24may90 [written]
 *
 * with mods ken@sdd.hp.com 01jul90
 *
 * $Header: /a/cvs/386BSD/ports/news/cnews/msgidd/msgid.c,v 1.1 1993/08/27 02:47:47 alm Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef hpux
#include <sys/param.h>
#include <libBSD.h>
#endif
#include <sys/socket.h>
#include <sys/un.h>
#include <syslog.h>
#define NEEDMSGS
#include "msgid.h"
#include "../common/conf.h"

#ifdef MSGID

#ifdef WANT_MAIN
char hostname[BUFSIZ];
#else
extern char hostname[];
#endif

#define SERVERTIMEOUT	30

static int s = -1;
static int read_answer();

/*
 * Protocol:
 *    Return value as used here is from the server to us.  Note that this
 *    may not be the same as the return value from msgid().
 *
 *    3 message types:
 *        MCANCEL: Delete an id from the holding queues.  Return value
 *                 is non-zero for failure.
 *        MADD:    Check for dup and add as needed.  Return value is 
 *                 non-zero for dup.
 *        MHOST:   Used to inform the server who is on the other end of this 
 *                 nntpd.  Return value is non-zero for failure.  Used only
 *                 in msgid_init().
 */


/* 
 * returns: 0 for ok, 1 for failure
 */
msgid_init()
{
    char buf[300];
    struct sockaddr_un n;
    static dead_server_count = 0;

    s = socket(PF_UNIX, SOCK_STREAM, 0);
    if (s < 0) {
	syslog(LOG_ERR, "msgid: can't get socket: %m");
	return(1);
    }

    n.sun_family = AF_UNIX;
    (void) strcpy(n.sun_path, SOCKNAME);

    if (0 > connect(s, &n, strlen(n.sun_path) + sizeof n.sun_family)) {
	close(s);
	s = -1;
	/* only syslog every 128 messages, so that dead msgidd doesn't
	 * lead to multi-megabyte syslog files (vix, 13feb91)
	 */
	if (!(dead_server_count++ % 128)) {
	    syslog(LOG_ERR, "msgid: connect to %s: %m", SOCKNAME);
	}
	return(1);
    }

    (void) strcpy(buf, msgs[MHOST]);
    (void) strcat(buf, hostname);
    if (write(s, buf, strlen(buf)) < 0) {
	close(s);
	s = -1;
	syslog(LOG_ERR, "msgid: host message write: %m", SOCKNAME);
	return(1);
    }

    return(read_answer());
}


/* 
 * returns: nonzero = duplicate, return value doesn't mean much for the
 *          MADD or MOLD messages
 */
int
msgid(id, mtype)
    char *id;
    int mtype;
{
    char *cp, buf[256], *rindex();

    if (s == -1 && msgid_init())
	return(0);

    /*
     * We need to do this just because gethistent does it
     * "in place" so add vs old gets fried ...
     *
     * If running Bnews, converts "id" to lower case.
     * If running Cnews, converts "id" per rfc822.
     */
#ifdef CNEWS
    cp = rindex(id, '@');        /* look for @ in message id */
    if (cp != NULL) {
	for(; *cp != '\0'; ++cp)
#else
    {
	for (cp = msg_id; *cp != '\0'; ++cp)
#endif
	    if (isupper(*cp))
		*cp = tolower(*cp);
    }
    (void) strcpy(buf, msgs[mtype]);
    (void) strcat(buf, id);
    if (0 > write(s, buf, strlen(buf))) {
	syslog(LOG_ERR, "msgid: write: %m");
	close(s);
	s = -1;
	return(0);
    }
    return(read_answer()); 
}


static int
read_answer()
{
    unsigned char c;
    fd_set readfds;
    struct timeval to;
    int i;

    FD_ZERO(&readfds);
    FD_SET(s, &readfds);
    to.tv_sec = SERVERTIMEOUT;
    to.tv_usec = 0;
    if ((i = select(s+1, &readfds, NULL, NULL, &to)) < 0) {
	syslog(LOG_ERR, "msgid: select: %m");
	goto bad;
    }
    if (i == 0 || FD_ISSET(s, &readfds) == 0 || (i = read(s, &c, 1)) == 0) {
	syslog(LOG_ERR, "msgid: read timeout");
	goto bad;
    }
    if (i < 0) {
	syslog(LOG_ERR, "msgid: read: %m");
	goto bad;
    }
    if (c)
	return(1);
    return(0);
bad:
    close(s);
    s = -1;
    return 0;
}

#ifdef WANT_MAIN
main(argc, argv)
    int argc;
    char *argv[];
{
    register int n;
    char buf[BUFSIZ], cmd[20], id[BUFSIZ];

    if (gethostname(hostname, BUFSIZ)) {
	perror("hostname");
	exit(1);
    }
    (void) printf("host: %s\n", hostname);

    if (argc != 1) {
	(void) fprintf(stderr, "usage: %s\n", argv[0]);
	exit(1);
    }

#ifdef LOG_DAEMON
    openlog("msgid-test", LOG_PID, LOG_DAEMON);
#else
    openlog("msgid-test", LOG_PID);
#endif

    while (fputs("cmd msgid: ", stdout), fflush(stdout), fgets(buf, BUFSIZ, stdin))
	if ((n = sscanf(buf, "%[^ \t]%*[ \t]%[^\n]", cmd, id)) == 2) {
	    if (strcmp(cmd, "cancel") == 0)
		(void) printf("%s\n", (msgid(id, MCANCEL) ? "failed" : "ok"));
	    else if (strcmp(cmd, "add") == 0)
		(void) printf("%d\n", msgid(id, MADD));
		/* 
		(void) printf("%sduplicate\n", 
			      (msgid(id, MADD) ? "" : "not a "));
		*/
	    else if (strcmp(cmd, "old") == 0)
		(void) printf("%s\n", (msgid(id, MOLD) ? "failed" : "ok"));
	    else
		(void) printf("possible cmds are cancel, add, and old\n");
	} else
	    (void) printf("[%d] possible cmds are cancel, add, and old\n", n);
}
#endif
#endif MSGID
