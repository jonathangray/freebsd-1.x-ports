/**************************************************************************
NAME: Brian Buhrow
DATE: September 17, 1991
PURPOSE: This file contains the interface between the Mush program and the
	POP post-office calls which put mail into the system mailbox.  Mail
	will be placed in the user's mailbox when Mush is first fired up and
	will be checked periodically as Mush is run.
	Note that these routines will only be called if the pre-processor
	variable POP3_SUPPORT is defined.
**************************************************************************/

#ifdef POP3_SUPPORT

#include "config.h"
#include "mush.h"
#include "pop.h"

/*
 * strstr - find first occurrence of wanted in s
 */

char *				/* found string, or NULL if none */
strstr(s, wanted)
char *s;
char *wanted;
{
    char *scan;
    long len;
    char firstc;

    /*
     * The odd placement of the two tests is so "" is findable.
     * Also, we inline the first char for speed.
     * The ++ on scan has been moved down for optimization.
     */
    firstc = *wanted;
    len = strlen(wanted);
    for (scan = s; *scan != firstc || strncmp(scan, wanted, len) != 0; )
	if (*scan++ == '\0')
	    return NULL;
    return scan;
}

/* This routine forms the header line for the From and Date functions below.
 * It was written by John Kammens for use by UCSC's version of UCBmail running
 * on the ATHENA system.
 */
static char *
header(msg, tag)
char *msg, *tag;
{
    char *val, *ptr, *eoh;

    val = malloc(strlen(tag) + 3);
    if (!val)
	return (0);

    sprintf(val, "\n%s:", tag);

    eoh = strstr(msg, "\n\n");
    if (!eoh)
	eoh = (char *) index(msg, '\0');

    ptr = strstr(msg, tag);

    if ((!ptr) || (ptr > eoh)) {
	sprintf(val, "%s:", tag);
	if (!strncmp(val, msg, strlen(val)))
	    ptr = msg;
	else
	    ptr = 0;
    }
    if (!ptr) {
	free(val);
	return (0);
    }
    ptr += strlen(val);

    while (*ptr && ((*ptr == ' ') || (*ptr == '\t')))
	ptr++;

    eoh = (char *) index(ptr, '\n');
    if (!eoh)
	eoh = (char *) index(ptr, '\0');

    val = realloc(val, (eoh - ptr) + 1);
    strncpy(val, ptr, (eoh - ptr));
    val[eoh - ptr] = '\0';

    return (val);
}

/* This routine comes up with the Unix Date line to insert into the mail
 * file.  It was written by John Kammens for use with UCSC's UCBMail on
 * Athena.
 *
 * Modified to get rid of dependence on that hideous B-news yacc parser.
 * Mush already has a perfectly good date parser, let's use it.  -- Bart
 */
static char *
date(msg)
char *msg;
{
    char *real = 0, *machine = 0;
    long t;
    int size;

    real = header(msg, "Date");

    if (real) {
	machine = parse_date(real);
	free(real);
	if (machine) {
	    machine = date_to_ctime(machine);
	}
    }
    if (!machine) {
	t = time((long *)0);
	machine = ctime(&t);
    }
    size = strlen(machine);
    machine[size - 1] = '\0';	/* get rid of newline */
    real = malloc(size);
    if (!real)
	return 0;
    strcpy(real, machine);

    return real;
}

/* This routine comes up with the Unix From line to insert into the mail
 * file.  It was written by John Kammens for use with UCSC's UCBMail on
 * Athena.
 *
 * Modified to use get_name_n_addr() so we don't depend on regexec()
 * being available, which it usually isn't. -- Bart
 */
static char *
from_line(msg)
char *msg;
{
    char *real = header(msg, "From"), *real2;
    char addr[256];

    if (real) {
	addr[0] = 0;
	(void) get_name_n_addr(real, NULL, addr);
	if (addr[0]) {
	    (void) bang_form(real, addr);
	    return real;
	}
    }

    if (!real)
	real = malloc(8);
    if (!real)
	return 0;
    strcpy(real, "unknown");
    return real;
}

/* Retrieves mail from the system post office using the POP interface
 * routines as put together by John Kammens for the UCBmail program under
 * ATHENA.  This routine will be called both when Mush begins executing to
 * check for new mail and when Mush is executing to see if new mail has
 * arrived during the Mush session.
 */
static void 
loadmail()
{
    PopServer postinfo;
    int msgcount, msgsize, i, flags = 0;
    char mailbox[MAXPATHLEN], *msgptr, *dateline, *fromline;
    FILE *mfstream;
    struct stat mfstat;
    static char pass[64];

    *mailbox = (char) NULL;	/* Clear string */
    strcat(mailbox, getenv("HOME"));
    strcat(mailbox, "/");
    strcat(mailbox, MAILFILE);
    /* Get info about post drop */
    if (pass[0])
	flags = POP_NO_GETPASS;
    if (!(postinfo = pop_open(NULL, login, pass, flags))) {
	fprintf(stderr, "Error opening connection with post office: %s\n",
		pop_error);
	return;
    }
    if (pop_stat(postinfo, &msgcount, &msgsize)) {
	fprintf(stderr, "Error collecting status from post office: %s\n",
		pop_error);
	pop_close(postinfo);
	return;
    }
    if (!msgcount) {
	pop_quit(postinfo);
	return;
    }
    /* Begin loading mailbox */
    if (stat(mailbox, &mfstat)) {
	if (errno == ENOENT) {
	    close(open(mailbox, O_WRONLY | O_CREAT | O_EXCL, 0600));
	}
    }
    if (!(mfstream = fopen(mailbox, "a"))) {
	perror("Error opening mailbox in loadmail");
	pop_close(postinfo);
	return;
    }
    for (i = 1; i <= msgcount; i++) {	/* Load new messages */
	msgptr = pop_retrieve(postinfo, i);
	dateline = date(msgptr);
	fromline = from_line(msgptr);
	if (fprintf(mfstream, "\nFrom %s %s\n%s", fromline, dateline, msgptr)
		< (strlen(fromline) + strlen(dateline) + strlen(msgptr))) {
	    fprintf(stderr, "Error writing mailbox file\n");
	    pop_close(postinfo);
	    cleanup(-1);
	}
	free(dateline);
	free(fromline);
	free(msgptr);
	if (pop_delete(postinfo, i)) {
	    fprintf(stderr, "Error deleting message from post office: %s\n",
		    pop_error);
	}
    }
    if (fclose(mfstream) == EOF) {
	perror("Error closing mailbox file in loadmail");
	pop_close(postinfo);
	return;
    }
    if (pop_quit(postinfo)) {
	fprintf(stderr, "Error closing post office: %s\n", pop_error);
    }
    return;
}

/* This routine merely calls loadmail to get mail initially from the post
 * office.  There is no forking, and it is intended to be used when Mush is
 * first started.
 */
void
popgetmail()
{
    loadmail();
}

/* This function calls loadmail, after first forking, releasing stdin,
 * stdout and stderr and ignoring any signals that might be generated.
 * This function is invoked by the sigalrm signal.  The parent resets
 * the alarm and returns.
 */
void 
popchkmail()
{
    static int cpid = 0, numtries = 0;

    if (cpid > 0) {
	if (!kill(cpid, 0)) {
	    numtries++;
	    if (numtries > 10) {
		kill(cpid, SIGKILL);
	    }
	    return;
	} else {
	    numtries = 0;
	}
    }
    if (!(cpid = fork())) {
	/* Ignore several signals for workability */
	signal(SIGCONT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTSTP, SIG_IGN);
	signal(SIGSTOP, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	signal(SIGIO, SIG_IGN);
	signal(SIGPIPE, SIG_IGN);
	loadmail();
	_exit(0);
    } else {
	if (cpid == -1) {
	    fprintf(stderr, "Unable to fork in popchkmail\n");
	}
	return;
    }
}

#endif	/* POP3_SUPPORT */
