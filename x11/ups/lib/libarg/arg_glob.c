/* arg_glob.c - do globbing of multi-component paths */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_glob_sccsid[] = "@(#)arg_glob.c	1.10 18/9/92 (UKC)";

#include <sys/types.h>

#include <sys/param.h>	/* for MAXPATHLEN */

#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "arg.h"

static int rglob PROTO((char *dir, int dirlen, char *pat, dvec_t dv, dirfuncs_t *dirfuncs));
static int cmp PROTO((const void *el1, const void *el2));

static int
rglob(dir, dirlen, pat, dv, dirfuncs)
char *dir;
int dirlen;
char *pat;
dvec_t dv;
dirfuncs_t *dirfuncs;
{
	long dirid;
	char *enddir, *endpat;
	const char *name;
	int status, freelen;

	if (!(*dirfuncs->df_isdir)(dir))
		return 0;
	if (*pat == '\0') {
		add_to_dvec(dv, strsave(dir));
		return 0;
	}
	if ((*dirfuncs->df_opendir)(dir, &dirid) != 0)
		return 0;
	endpat = strchr(pat, '/');
	if (endpat != NULL)
		*endpat = '\0';
	enddir = dir + strlen(dir);
	if (enddir != dir && enddir[-1] != '/')
		*enddir++ = '/';
	status = 0;
	freelen = dirlen - (enddir - dir);
	while (status == 0 && (name = (*dirfuncs->df_readdir)(dirid)) != NULL) {
		if (arg_match(name, pat) && strlen(name) < freelen) {
			(void) strcpy(enddir, name);
			if (endpat != NULL)
				status = rglob(dir, dirlen, endpat+1, dv, dirfuncs);
			else
				add_to_dvec(dv, strsave(dir));
		}
	}
	if (endpat != NULL)
		*endpat = '/';
	*enddir = '\0';
	(*dirfuncs->df_closedir)(dirid);
	return status;
}

/*  Glue between qsort(3) and strcmp(3)
 */
static int
cmp(el1,el2)
#ifdef __STDC__
const void *el1, *el2;
#else
char *el1, *el2;
#endif
{
	return strcmp(*(char **)el1,*(char **)el2);
}	 

int
arg_has_globchars(s)
register const char *s;
{
	for (; *s != '\0'; s++)
		if (*s == PAT_STAR || *s == PAT_QUERY || *s == PAT_SEQ)
			return TRUE;
	return FALSE;
}
		
/*  Glob a compiled pattern.
 */
dvec_t
arg_gen_glob_cpat(cpat, buf, buflen, dirfuncs)
const char *cpat;
char *buf;
int buflen;
dirfuncs_t *dirfuncs;
{
	dvec_t dv;
	char *writeable_cpat;
	const char **vec;
	int veclen;

	dv = make_dvec();
	if (!arg_has_globchars(cpat))
		add_to_dvec(dv, strsave(cpat));
	else {
		if (*cpat == '/') {
			*buf = '/';
			buf[1] = '\0';
			cpat++;
		}
		else
			*buf = '\0';

		writeable_cpat = strsave(cpat);
		(void) rglob(buf, buflen, writeable_cpat, dv, dirfuncs);
		free(writeable_cpat);
		vec = get_dvec_vec(dv);
		veclen = get_dvec_size(dv);
		qsort((char *)vec, veclen, sizeof(char *), cmp);
	}
	add_to_dvec(dv, (char *)NULL);
	return dv;
}

dvec_t
arg_gen_glob(p_pat, dirfuncs)
const char **p_pat;
dirfuncs_t *dirfuncs;
{
	dvec_t bdv, adv, gdv;
	char *buf, *fpat;
	const char *cpat, **gstr, **brstr;

	if ((cpat = arg_do_quoting(p_pat, "[]~{,}*?")) == NULL)
		return 0;
	if ((bdv = arg_expand_braces(cpat, PAT_OPENBR, PAT_CLOSEBR, PAT_COMMA)) == 0)
		return 0;
	buf = e_malloc(MAXPATHLEN);
	adv = make_dvec();
	for (brstr = get_dvec_vec(bdv); *brstr != NULL; brstr++) {
		if ((fpat = arg_expand_twiddle(*brstr, PAT_TWIDDLE)) == NULL)
			break;
		gdv = arg_gen_glob_cpat(fpat, buf, MAXPATHLEN, dirfuncs);
		for (gstr = get_dvec_vec(gdv); *gstr != NULL; gstr++)
			add_to_dvec(adv, *gstr);
		free_dvec(gdv);
		if (fpat != *brstr)
			free(fpat);
	}
	free(buf);
	free_dvec_and_strings(bdv);
	add_to_dvec(adv, (char *)NULL);
	return adv;
}
