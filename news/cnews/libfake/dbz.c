/*
 * fakes for using dbm (or old dbz) as if it were dbz
 */
#include <stdio.h>
#include <sys/types.h>
#include <dbz.h>

/* the simple stuff */
dbzfresh(a,b,c,d,e) char *a; long b; { return(dbzagain(a,a)); }
long dbzsize(a) long a; { return(a); }
dbzincore(n) int n; { return(0); }
dbzdebug(n) int n; { return(0); }
long dbztrim(a) long a; { return(a); }
int dbzsync() { return(0); }

/*
 - dbzagain - like dbminit but creates files
 */
int
dbzagain(a, b)
char *a;
char *b;
{
	char dirname[200];
	char pagname[200];
	FILE *p;
	FILE *d;

	sprintf(dirname, "%s.dir", a);
	sprintf(pagname, "%s.pag", a);
	p = fopen(dirname, "w");
	d = fopen(pagname, "w");
	if (p != NULL)
		(void) fclose(p);
	if (d != NULL)
		(void) fclose(d);
	if (p == NULL || d == NULL)
		return(-1);

	return(dbminit(a));
}

/*
 - dbzfetch - fetch() with case mapping built in
 *
 * Uses C News rfc822ize().  Assumes keys are strings.
 */
datum
dbzfetch(key)
datum key;
{
	char buffer[DBZMAXKEY + 1];
	datum mappedkey;

	(void) strcpy(buffer, key.dptr);
	(void) rfc822ize(buffer);
	mappedkey.dptr = buffer;
	mappedkey.dsize = key.dsize;

	return(fetch(mappedkey));
}

/*
 - dbzstore - store() with case mapping built in
 *
 * Uses C News rfc822ize().  Assumes keys are strings.
 */
int
dbzstore(key, data)
datum key;
datum data;
{
	char buffer[DBZMAXKEY + 1];
	datum mappedkey;

	(void) strcpy(buffer, key.dptr);
	(void) rfc822ize(buffer);
	mappedkey.dptr = buffer;
	mappedkey.dsize = key.dsize;

	return(store(mappedkey, data));
}
