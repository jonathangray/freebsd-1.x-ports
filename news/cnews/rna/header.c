/*
 * extract/output headers
 */

#include "defs.h"
#include <sys/timeb.h>

extern char systemid[];
extern long now;

char hform[]		 = "%s: %s\n";

/* Mandatory Headers */
char t_relayversion[]	 = "Relay-Version";
char t_postversion[]	 = "Posting-Version";
char t_from[]		 = "From";
char t_date[]		 = "Date";
char t_newsgroups[]	 = "Newsgroups";
char t_subject[]	 = "Subject";
char t_messageid[]	 = "Message-ID";
char t_path[]		 = "Path";

/* Optional Headers */
char t_replyto[]	 = "Reply-To";
char t_sender[]		 = "Sender";
char t_followupto[]	 = "Followup-To";
char t_datereceived[]	 = "Date-Received";
char t_expires[]	 = "Expires";
char t_references[]	 = "References";
char t_control[]	 = "Control";
char t_distribution[]	 = "Distribution";
char t_organization[]	 = "Organization";
char t_lines[]		 = "Lines";

typedef enum ft
{
	f_control, f_date, f_datereceived, f_distribution,
	f_expires, f_followupto, f_from, f_lines, f_messageid,
	f_newsgroups, f_organization, f_path, f_postversion,
	f_references, f_relayversion, f_replyto, f_sender,
	f_subject
}


ftype;

typedef struct field {
	char *f_name;
	ftype	f_type;
} field;

static field fields[] = 
{
	{ t_control, 	f_control	 },
	{ t_date, 		f_date		 },
	{ t_datereceived, 	f_datereceived	 },
	{ t_distribution, 	f_distribution	 },
	{ t_expires, 	f_expires	 },
	{ t_followupto, 	f_followupto	 },
	{ t_from, 		f_from		 },
	{ t_lines, 	f_lines		 },
	{ t_messageid, 	f_messageid	 },
	{ t_newsgroups, 	f_newsgroups	 },
	{ t_organization, 	f_organization	 },
	{ t_path, 		f_path		 },
	{ t_postversion, 	f_postversion	 },
	{ t_references, 	f_references	 },
	{ t_relayversion, 	f_relayversion	 },
	{ t_replyto, 	f_replyto	 },
	{ t_sender, 	f_sender	 },
	{ t_subject, 	f_subject	 }
};


char *weekdays[7] = 
{
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};


char *months[12] = 
{
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};


static
fieldcmp(a, b)
field *a, *b;
{
	return CMP(a->f_name, b->f_name);
}


/*
 * extract headers from file,
 * position file to start of body
 */
gethead(f, hp)
FILE *f;
header *hp;
{
	register char *colon, *space, *s;
	register field	*fp;
	field		af;
	char buf[BUFLEN*2];
	char *hfgets();

	memset((char *) hp, 0, sizeof(header));
	while (hfgets(buf, sizeof(buf), f)) {
		if (buf[0] == '\n')
			return;
		if (isupper(buf[0]) && (colon = strchr(buf, ':')) &&
		    (space = strchr(buf, ' ')) && (colon + 1 == space)) {
			*colon = '\0';
			af.f_name = buf;
			fp = (field * )bsearch((char *)&af, (char *)fields,
				sizeof fields/sizeof fields[0], sizeof fields[0],
				fieldcmp);
			*colon = ':';
		} else
			fp = NULL;
		if (!fp)
			if (hp->h_others)
				hp->h_others = catstr(hp->h_others, buf);
			else
				hp->h_others = newstr(buf);
		else {
			if (colon = strchr(space + 1, '\n'))
				*colon = '\0';
			s = newstr(space + 1);
			switch (fp->f_type) {
			case f_control:		
				hp->h_control = s;	
				break;
			case f_date:		
				hp->h_date = s;		
				break;
			case f_datereceived:	
				hp->h_datereceived = s;	
				break;
			case f_distribution:	
				hp->h_distribution = s;	
				break;
			case f_expires:		
				hp->h_expires = s;	
				break;
			case f_followupto:	
				hp->h_followupto = s;	
				break;
			case f_from:		
				hp->h_from = s;		
				break;
			case f_lines:		
				hp->h_lines = s;	
				break;
			case f_messageid:	
				hp->h_messageid = s;	
				break;
			case f_newsgroups:	
				hp->h_newsgroups = s;	
				break;
			case f_organization:	
				hp->h_organisation = s;	
				break;
			case f_path:		
				hp->h_path = s;		
				break;
			case f_postversion:	
				hp->h_postversion = s;	
				break;
			case f_references:	
				hp->h_references = s;	
				break;
			case f_relayversion:	
				hp->h_relayversion = s;	
				break;
			case f_replyto:		
				hp->h_replyto = s;	
				break;
			case f_sender:		
				hp->h_sender = s;	
				break;
			case f_subject:		
				hp->h_subject = s;	
				break;
			}
		}
	}
}


/*
 * put headers to file
 */
puthead(hp, f, com)
header *hp;
FILE *f;
pheadcom com;			/* actually ignored now */
{
	register char *s;
	char *getunique();
	extern char *getenv();

	if (hp->h_relayversion)
		(void) fprintf(f, hform, t_relayversion, hp->h_relayversion);

	if (hp->h_postversion)
		(void) fprintf(f, hform, t_postversion, hp->h_postversion);

	if (hp->h_from)
		(void) fprintf(f, hform, t_from, hp->h_from);

	if (hp->h_date)
		(void) fprintf(f, hform, t_date, hp->h_date);

	if (hp->h_newsgroups)
		(void) fprintf(f, hform, t_newsgroups, hp->h_newsgroups);

	if (hp->h_subject)
		(void) fprintf(f, hform, t_subject, hp->h_subject);

	if (hp->h_messageid)
		(void) fprintf(f, hform, t_messageid, hp->h_messageid);

	if (hp->h_path)
		(void) fprintf(f, hform, t_path, hp->h_path);

	/* optional */

	if (hp->h_replyto)
		(void) fprintf(f, hform, t_replyto, hp->h_replyto);

	if (hp->h_sender)
		(void) fprintf(f, hform, t_sender, hp->h_sender);

	if (hp->h_followupto)
		(void) fprintf(f, hform, t_followupto, hp->h_followupto);

	if (hp->h_datereceived)
		(void) fprintf(f, hform, t_datereceived, hp->h_datereceived);

	if (hp->h_expires)
		(void) fprintf(f, hform, t_expires, hp->h_expires);

	if (hp->h_references)
		(void) fprintf(f, hform, t_references, hp->h_references);

	if (hp->h_control)
		(void) fprintf(f, hform, t_control, hp->h_control);

	if (hp->h_distribution)
		(void) fprintf(f, hform, t_distribution, hp->h_distribution);

	if (hp->h_organisation)
		(void) fprintf(f, hform, t_organization, hp->h_organisation);

	if (hp->h_lines)
		(void) fprintf(f, hform, t_lines, hp->h_lines);

	if (hp->h_others)
		fputs(hp->h_others, f);
}


/*
 * free all strings allocated to header
 */
freehead(hp)
register header *hp;
{
	if (hp->h_relayversion)	
		free(hp->h_relayversion);
	if (hp->h_postversion)	
		free(hp->h_postversion);
	if (hp->h_from)		
		free(hp->h_from);
	if (hp->h_date)		
		free(hp->h_date);
	if (hp->h_newsgroups)	
		free(hp->h_newsgroups);
	if (hp->h_subject)	
		free(hp->h_subject);
	if (hp->h_messageid)	
		free(hp->h_messageid);
	if (hp->h_path)		
		free(hp->h_path);
	if (hp->h_replyto)	
		free(hp->h_replyto);
	if (hp->h_sender)	
		free(hp->h_sender);
	if (hp->h_followupto)	
		free(hp->h_followupto);
	if (hp->h_datereceived)	
		free(hp->h_datereceived);
	if (hp->h_expires)	
		free(hp->h_expires);
	if (hp->h_references)	
		free(hp->h_references);
	if (hp->h_control)	
		free(hp->h_control);
	if (hp->h_distribution)	
		free(hp->h_distribution);
	if (hp->h_organisation)	
		free(hp->h_organisation);
	if (hp->h_lines)		
		free(hp->h_lines);
	if (hp->h_others)	
		free(hp->h_others);
}


/*
 * hfgets is like fgets, but deals with continuation lines.
 * It also ensures that even if a line that is too long is
 * received, the remainder of the line is thrown away
 * instead of treated like a second line.
 */
char *
hfgets(buf, len, fp)
char *buf;
int len;
FILE *fp;
{
	register int c;
	register char *cp, *tp;

	if ((cp = fgets(buf, len, fp)) == NULL)
		return NULL;

	if (*cp == '\n')
		return cp;

	tp = cp + strlen(cp);
	if (tp[-1] != '\n') {
		/* Line too long - part read didn't fit into a newline */
		while ((c = getc(fp)) != '\n' && c != EOF)
			;
	} else
		*--tp = '\0';	/* clobber newline */

	while ((c = getc(fp)) == ' ' || c == '\t') {
		/* Continuation line. */
		while ((c = getc(fp)) == ' ' || c == '\t')
			;
		if (tp - cp < len) {
			*tp++ = ' ';
			*tp++ = c;
		}
		while ((c = getc(fp)) != '\n' && c != EOF)
			if (tp - cp < len)
				*tp++ = c;
	}
	*tp++ = '\n';
	*tp++ = '\0';
	if (c != EOF)
		ungetc(c, fp);	/* push back first char of next header */
	return cp;
}



/*
 * ascii to time
 * return 0L on error
 */
long
atot(s)
char *s;
{
	struct timeb ftnow;
	extern long getdate();

	ftime(&ftnow);
	return getdate(s, &ftnow);
}
