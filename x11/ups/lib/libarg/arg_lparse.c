/* arg_lparse.c - do shell type parsing of a command line into arguments */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_lparse_sccsid[] = "@(#)arg_lparse.c	1.10 18/9/92 (UKC)";

#include <stdio.h>

#include <local/ukcprog.h>

#include "arg.h"

static void skip_white PROTO((const char **p_s));

static void
skip_white(p_s)
const char **p_s;
{
	const char *s;

	s = *p_s;
	while(*s != '\0' && (*s == ' ' || *s == '\t' || *s == '\n'))
		s++;
	*p_s = s;
}

int
arg_lparse(s, p_dv, p_rdlist)
const char *s;
dvec_t *p_dv;
long *p_rdlist;
{
	dvec_t adv, gdv;
	redirtype_t redirtype;
	int fd, res;
	int *fdaddr;
	const char **args;
	const char *save_s;
	long rdlist;

	rdlist = (long)(int *)NULL;
	res = -1;
	adv = make_dvec();
	gdv = 0;
	for (;;) {
		skip_white(&s);
		if (*s == '\0') {
			res = 0;
			break;
		}
		fdaddr = NULL;
		if ((redirtype = arg_get_redir(&s, &fdaddr, &rdlist)) == RD_ERROR)
			break;
		save_s = s;
		if (fdaddr != NULL) {
			skip_white(&s);
			if ((gdv = arg_glob(&s)) == 0)
				break;
			args = get_dvec_vec(gdv);
			if (*args == NULL) {
				errf("missing filename for redirect");
				break;
			}
			if (args[1] != NULL) {
				errf("`%.*s': ambiguous", s - save_s, save_s);
				break;
			}
			if ((fd = arg_open_redir_file(*args, redirtype)) == -1)
				break;
			*fdaddr = fd;
			free_dvec_and_strings(gdv);
			gdv = 0;
		}
		else if (redirtype == RD_NOT_REDIR) {
			if ((gdv = arg_glob(&s)) == 0)
				break;
			args = get_dvec_vec(gdv);
			if (*args == NULL) {
				errf("No files match filename pattern `%.*s'",
								s - save_s, save_s);
				break;
			}
			for (; *args != NULL; args++)
				add_to_dvec(adv, *args);
			free_dvec(gdv);
			gdv = 0;
		}
			
	}
	if (gdv != 0)
		free_dvec(gdv);
	if (res != 0) {
		arg_tidy_redirs_in_parent(rdlist);
		free_dvec(adv);
		*p_dv = 0;
		*p_rdlist = 0;
	}
	else {
		add_to_dvec(adv, (char *)NULL);
		*p_dv = adv;
		*p_rdlist = rdlist;
	}
	return res;
}

