/* $Id: nntp.c,v 1.5 1994/02/22 01:48:58 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "util.h"
#include "init.h"
#include "trn.h"
#include "ngdata.h"
#include "rcln.h"
#include "cache.h"
#include "bits.h"
#include "head.h"
#include "final.h"
#include "nntp.h"

#ifdef USE_NNTP

/* try to access the specified group */

bool
nntp_group(group, num)
char *group;
NG_NUM num;
{
    sprintf(ser_line, "GROUP %s", group);
    nntp_command(ser_line);
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	int ser_int = atoi(ser_line);
	if (ser_int != NNTP_NOSUCHGROUP_VAL
	 && ser_int != NNTP_SYNTAX_VAL) {
	    if (ser_int != NNTP_AUTH_NEEDED_VAL && ser_int != NNTP_ACCESS_VAL
	     && ser_int != NNTP_AUTH_REJECT_VAL) {
		fprintf(stderr, "\nServer's response to GROUP %s:\n%s\n",
			group, ser_line);
		finalize(1);
	    }
	}
	return FALSE;
    }
    if (num >= 0) {
	long count, first, last;

	(void) sscanf(ser_line,"%*d%ld%ld%ld",&count,&first,&last);
	/* NNTP mangles the high/low values when no articles are present. */
	if (!count)
	    abs1st[num] = ngmax[num]+1;
	else {
	    abs1st[num] = (ART_NUM)first;
	    if (last > ngmax[num])
		ngmax[num] = (ART_NUM)last;
	}
    }
    return TRUE;
}

/* check on an article's existence */

bool
nntp_stat(artnum)
ART_NUM artnum;
{
    sprintf(ser_line, "STAT %ld", (long)artnum);
    nntp_command(ser_line);
    return (nntp_check(TRUE) == NNTP_CLASS_OK);
}

/* prepare to get the header */

bool
nntp_header(artnum)
ART_NUM artnum;
{
    sprintf(ser_line, "HEAD %ld", (long)artnum);
    nntp_command(ser_line);
    return (nntp_check(TRUE) == NNTP_CLASS_OK);
}

/* copy the body of an article to a temporary file */

FILE *
nntp_body(artnum)
ART_NUM artnum;
{
    char *artname;
    FILE *fp;

    if (!parseheader(artnum))
	return Nullfp;
    artname = nntp_artname();
    if (!(fp = fopen(artname, "w+"))) {
	fprintf(stderr, "\nUnable to write temporary file: '%s'.\n",
		artname);
	finalize(1);
    }
    sprintf(ser_line, "BODY %ld", (long)artnum);
    nntp_command(ser_line);
    if (nntp_check(TRUE) != NNTP_CLASS_OK) {	/* and get it's reaction */
	fclose(fp);
	errno = ENOENT;			/* Simulate file-not-found */
	return Nullfp;
    }
    fwrite(headbuf, 1, strlen(headbuf), fp);
    for (;;) {
	nntp_gets(ser_line, sizeof ser_line);
	if (NNTP_LIST_END(ser_line))
	    break;
	fputs((ser_line[0] == '.' ? ser_line + 1 : ser_line), fp);
	putc('\n', fp);
    }
    fseek(fp, 0L, 0);
    return fp;
}

/* This is a 1-relative list */
static int maxdays[] = { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

time_t
nntp_time()
{
    char *s;
    int year, month, day, hh, mm;
    time_t ss;

    nntp_command("DATE");
    if (nntp_check(FALSE) != NNTP_CLASS_INF)
	return time((time_t*)NULL);

    s = rindex(ser_line, ' ') + 1;
    month = (s[4] - '0') * 10 + (s[5] - '0');
    day = (s[6] - '0') * 10 + (s[7] - '0');
    hh = (s[8] - '0') * 10 + (s[9] - '0');
    mm = (s[10] - '0') * 10 + (s[11] - '0');
    ss = (s[12] - '0') * 10 + (s[13] - '0');
    s[4] = '\0';
    year = atoi(s);

    /* This simple algorithm will be valid until the year 2400 */
    if (year % 4)
	maxdays[2] = 28;
    else
	maxdays[2] = 29;
    if (month < 1 || month > 12 || day < 1 || day > maxdays[month]
     || hh < 0 || hh > 23 || mm < 0 || mm > 59
     || ss < 0 || ss > 59)
	return time((time_t*)NULL);

    for (month--; month; month--)
	day += maxdays[month];

    ss = ((((year-1970) * 365 + (year-1968)/4 + day - 1) * 24L + hh) * 60
	  + mm) * 60 + ss;

    return ss;
}

bool
nntp_newgroups(t)
time_t t;
{
    struct tm *ts;

    ts = gmtime(&t);
    sprintf(ser_line, "NEWGROUPS %02d%02d%02d %02d%02d%02d GMT",
	ts->tm_year % 100, ts->tm_mon+1, ts->tm_mday,
	ts->tm_hour, ts->tm_min, ts->tm_sec);
    nntp_command(ser_line);
    return (nntp_check(TRUE) == NNTP_CLASS_OK);
}

bool
nntp_listgroup()
{
#ifdef NO_LISTGROUP
    static bool listgroup_works = FALSE;
#else
    static bool listgroup_works = TRUE;
#endif

    if (!listgroup_works)
	return FALSE;
    nntp_command("LISTGROUP");
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	listgroup_works = FALSE;
	return FALSE;
    }
    return TRUE;
}

/* similar to nntp_gets, but will make the buffer bigger if necessary */

char *
nntp_get_a_line(original_buffer,buffer_length)
char *original_buffer;
register int buffer_length;
{
    register int bufix = 0;
    register int nextch;
    register char *some_buffer_or_other = original_buffer;

    do {
	if (bufix >= buffer_length) {
	    buffer_length *= 2;
	    if (some_buffer_or_other == original_buffer) {
					/* currently static? */
		some_buffer_or_other = safemalloc((MEM_SIZE)buffer_length+1);
		strncpy(some_buffer_or_other,original_buffer,buffer_length/2);
					/* so we must copy it */
	    }
	    else {			/* just grow in place, if possible */
		some_buffer_or_other = saferealloc(some_buffer_or_other,
		    (MEM_SIZE)buffer_length+1);
	    }
	}
	if ((nextch = getc(ser_rd_fp)) == EOF)
	    return Nullch;
	some_buffer_or_other[bufix++] = (char) nextch;
    } while (nextch && nextch != '\n');
    some_buffer_or_other[bufix] = '\0';
    len_last_line_got = bufix;
    buflen_last_line_got = buffer_length;
    return some_buffer_or_other;
}

char *
nntp_artname()
{
    static char artname[20];
    sprintf(artname,"rrn.%ld",our_pid);
    return artname;
}

char
nntp_handle_timeout(strict)
bool_int strict;
{
    static bool handling_timeout = FALSE;
    extern char last_command[];
    char last_command_save[NNTP_STRLEN];
    char ch;

    if (handling_timeout)
	return NNTP_CLASS_FATAL;
    handling_timeout = TRUE;
    strcpy(last_command_save, last_command);
    nntp_close(FALSE);
    if (!nntp_connect() || (in_ng && !nntp_group(ngname, -1)))
	fatal_error("\n503 Server timed out.\n");
    nntp_command(last_command_save);
    ch = nntp_check(strict);
    handling_timeout = FALSE;
    return ch;
}

/* cleanup the odds and ends associated with NNTP usage */

void
nntp_cleanup()
{
    UNLINK(nntp_artname());
    if (*active_name)
	UNLINK(active_name);
    nntp_close(TRUE);
}

#ifdef USE_XTHREAD

static long rawbytes = -1;	/* bytes remaining to be transfered */

/* nntp_readcheck -- get a line of text from the server, interpreting
** it as a status message for a binary command.  Call this once
** before calling nntp_read() for the actual data transfer.
*/
long
nntp_readcheck()
{
    /* try to get the status line and the status code */
    if (nntp_check(FALSE) != NNTP_CLASS_OK)
	return rawbytes = -1;

    /* try to get the number of bytes being transfered */
    if (sscanf(ser_line, "%*d%ld", &rawbytes) != 1)
	return rawbytes = -1;
    return rawbytes;
}

/* nntp_read -- read data from the server in binary format.  This call must
** be preceeded by an appropriate binary command and an nntp_readcheck call.
*/
long
nntp_read(buf, n)
char *buf;
long n;
{
    /* if no bytes to read, then just return EOF */
    if (rawbytes < 0)
	return 0;

#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif

    /* try to read some data from the server */
    if (rawbytes) {
	n = fread(buf, 1, n > rawbytes ? rawbytes : n, ser_rd_fp);
	rawbytes -= n;
    } else
	n = 0;

    /* if no more left, then fetch the end-of-command signature */
    if (!rawbytes) {
	char buf[5];	/* "\r\n.\r\n" */

	fread(buf, 1, 5, ser_rd_fp);
	rawbytes = -1;
    }
#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    return n;
}
#endif /* USE_XTHREAD */

#endif /* USE_NNTP */
