#include "defs.h"

/*
 * string handling functions
 */
char *
myalloc(size)
int size;
{
	register char *cp;

	extern char *malloc();

	if ((cp = malloc((unsigned) size)) == NIL(char))
		error("No more memory.");
	return cp;
}


char *
myrealloc(ptr, size)
char *ptr;
int size;
{
	register char *cp;

	extern char *realloc();

	if ((cp = realloc(ptr, (unsigned) size)) == NIL(char))
		error("No more memory.");
	return cp;
}


char *
newstr(s)
char *s;
{
	return strcpy(myalloc(strlen(s) + 1), s);
}


char *
newstr2(s1, s2)
char *s1, *s2;
{
	return strcat(strcpy(myalloc(strlen(s1) + strlen(s2) + 1), s1), s2);
}


char *
newstr3(s1, s2, s3)
char *s1, *s2, *s3;
{
	return strcat(strcat(strcpy(myalloc(strlen(s1) + strlen(s2) + strlen(s3) +
	    1), s1), s2), s3);
}


char *
newstr4(s1, s2, s3, s4)
char *s1, *s2, *s3, *s4;
{
	return strcat(strcat(strcat(strcpy(myalloc(strlen(s1) + strlen(s2) +
	    strlen(s3) + strlen(s4) + 1), s1), s2), s3), s4);
}


char *
newstr5(s1, s2, s3, s4, s5)
char *s1, *s2, *s3, *s4, *s5;
{
	return strcat(strcat(strcat(strcat(strcpy(myalloc(strlen(s1) + strlen(s2) +
	    strlen(s3) + strlen(s4) + strlen(s5) + 1), s1), s2), s3), s4), s5);
}


char *
newstr6(s1, s2, s3, s4, s5, s6)
char *s1, *s2, *s3, *s4, *s5, *s6;
{
	return strcat(strcat(strcat(strcat(strcat(strcpy(myalloc(strlen(s1) +
	    strlen(s2) + strlen(s3) + strlen(s4) + strlen(s5) + strlen(s6) + 1),
	     s1), s2), s3), s4), s5), s6);
}


char *
catstr(old, s)
char *old, *s;
{
	return strcat(myrealloc(old, strlen(old) + strlen(s) + 1), s);
}


char *
catstr2(old, s1, s2)
char *old, *s1, *s2;
{
	return strcat(strcat(myrealloc(old, strlen(old) + strlen(s1) + strlen(s2) +
	    1), s1), s2);
}


/*
 * News group matching.
 *
 * nglist is a list of newsgroups.
 * sublist is a list of subscriptions.
 * sublist may have "meta newsgroups" in it.
 * All fields are NGSEPCHAR separated.
 *
 * sublist uses "all" like shell uses "*", and "." like shell uses "/"
 * if subscription X matches Y, it also matches Y.anything
 */
ngmatch(nglist, sublist)
char *nglist, *sublist;
{
	register char *n, *s, *nd, *sd;
	register int rc;

	rc = 0;
	n = nglist;
	while (*n && rc == 0) {
		if (nd = strchr(n, NGSEPCHAR))
			*nd = '\0';
		s = sublist;
		while (*s) {
			if (sd = strchr(s, NGSEPCHAR))
				*sd = '\0';
			if (*s != NEGCHAR)
				rc |= ptrncmp(s, n);
			else
				rc &= ~ptrncmp(s + 1, n);
			if (sd)
				*sd = NGSEPCHAR, s = sd + 1;
			else
				break;
		}
		if (nd)
			*nd = NGSEPCHAR, n = nd + 1;
		else
			break;
	}
	return rc;
}


/*
 * Compare two newsgroups for equality.
 * The first one may be a "meta" newsgroup.
 */
static
ptrncmp(ng1, ng2)
register char *ng1, *ng2;
{

	while (1) {
		if (ng1[0] == 'a' && ng1[1] == 'l' && ng1[2] == 'l' && (ng1[3] ==
		    '\0' || ng1[3] == '.')) {
			if (ng1[3] == '\0')	/* "all" matches anything */
				return 1;
			while (*ng2 && *ng2 != '.')
				ng2++;
			if (*ng2 != '.')		/* "all." doesn't match "xx" */
				return 0;
			ng1 += 4, ng2++;
			continue;
		}
		while (*ng1 && *ng1 != '.' && *ng1 == *ng2)
			ng1++, ng2++;
		if (*ng1 == '.') {
			if (*ng2 != '.' && *ng2 != '\0')
				return 0;	/* "."'s don't line up */
			if (*ng2)
				ng2++;
			ng1++;			/* "."'s line up - keep going */
		} else if (*ng1 == '\0')
			return (*ng2 == '\0' || *ng2 == '.');
			/* full match or X matching X.thing */
		else
			return 0;
	}
	/* NOTREACHED */
}



/*
 * open a file
 */
FILE *
fopenf(name, mode)
char *name, *mode;
{
	register FILE	*f;

	if ((f = fopen(name, mode)) == NULL)
		error("Can't %s %s", *mode == 'r' ? "open" : "create", name);
	return f;
}


/*
 * replace all '.''s with '/'
 */
char *
convg(s)
register char *s;
{
	register char *sav;

	sav = s;
	while (s = strchr(s, '.'))
		*s = '/';
	return sav;
}


/*
 * get a line from stdin
 * trim leading and trailing blanks
 */
char *
mgets()
{
	register char *s;
	static char buf[BUFSIZ];

	fflush(stdout);
	if (fgets(buf, sizeof(buf), stdin) == NULL) {
		(void) printf("\n");
		return NIL(char);
	}
	if (s = strchr(buf, '\n'))
		while (isspace(*s) && s > buf)
			*s-- = '\0';
	else {
		(void) printf("Input line too long.\n");
		return NULL;
	}
	s = buf;
	while (isspace(*s))
		s++;
	return s;
}


/*
 * apply the given function to each member in the newsgroup
 */
/* VARARGS2 */
applyng(ng, func, arg1)
register char *ng;
register int (*func)();
char *arg1;
{
	register char *delim;
	register int err;

	err = 0;
	while (*ng) {
		if (delim = strchr(ng, NGSEPCHAR))
			*delim = '\0';
		err += (*func)(ng, arg1);
		if (delim)
			*delim = NGSEPCHAR, ng = delim + 1;
		else
			break;
	}
	return err;
}


/*
 * generate a return address
 */
char *
getretaddr(hp)
header *hp;
{
	register char *ra;

	extern char *exaddress();

	if (hp->h_replyto)
		ra = exaddress(hp->h_replyto);
	else if (hp->h_from)
		ra = exaddress(hp->h_from);
	else
		ra = NIL(char);
	if (hp->h_path && !ra)
		ra = hp->h_path;
	return ra;
}


/*
 * try and make a proper address
 */
char *
exaddress(addr)
char *addr;
{
	register char *space, *dot, *at;
	register char *raddr;
	extern char mailvia[];

	raddr = NIL(char);
	if (strcmp(mailvia, "<path>") == 0)
		return raddr;
	if (space = strchr(addr, ' '))
		*space = '\0';
	if (mailvia[0] != '\0' && (at = strchr(addr, '@')) != NULL) {
		*at = '\0';
		raddr = newstr5(mailvia, PSEPS, at + 1, PSEPS, addr);
		*at = '@';
	} else
		raddr = newstr(addr);
	if (space)
		*space = ' ';
	return raddr;
}


/*
 * remove extra spaces, and insert separators if necessary in
 * newsgroups specification
 */
convgrps(sp)
register char *sp;
{
	register char *sep = NULL;

	while (*sp) {
		if (sep)
			sp++;
		while (*sp && (isspace(*sp) || *sp == NGSEPCHAR))
			strcpy(sp, sp + 1);
		if (sep)
			*sep = (*sp ? NGSEPCHAR : '\0');
		while (*sp && !isspace(*sp) && *sp != NGSEPCHAR)
			sp++;
		sep = sp;
	}
}
