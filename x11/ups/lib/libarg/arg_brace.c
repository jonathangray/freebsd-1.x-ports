/* arg_brace.c - expand csh type a{b,c} notation */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_brace_sccsid[] = "@(#)arg_brace.c	1.10 26/4/92 (UKC)";


#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>
#include <local/ukcprog.h>

#include "arg.h"

typedef struct {
	dvec_t db_dv;
	int db_left;
	int db_right;
	int db_sep;
} do_braces_arg_t;

static int do_braces PROTO((char *dbuf, const char *src, char *dst,
			    bool no_curlies, do_braces_arg_t *db));

/*  Expand csh type {a,b,c} notation (see csh(1)). Nested braces
 *  are handled.
 *
 *  Call add_to_dvec for each string of the expanded form.
 *  Return 0 for success, -1 and give an error message on failure.
 *
 *  metas are the special characters - left, seperator, right (normally "{,}").
 *  These arguments are provided so that arg_expand_braces() can be used on
 *  preprocessed patterns which have had their metacharacters mapped.
 */
static int
do_braces(dbuf, src, dst, no_curlies, db)
char *dbuf;
const char *src;
char *dst;
int no_curlies;
do_braces_arg_t *db;
{
	const char *newsrc;
	char *newdst, *newdbuf;
	int level, res;
	bool new_no_curlies;
	
	while (*src != db->db_left && *src != '\0') {
		if ((*dst++ = *src++) == db->db_right) {
			errf("unmatched }");
			return -1;
		}
	}

	if (*src == '\0') {
		*dst = '\0';
		if (no_curlies) {
			add_to_dvec(db->db_dv, strsave(dbuf));
			res = 0;
		}
		else {
			newdbuf = e_malloc(strlen(dbuf) + 1);
			res = do_braces(newdbuf, dbuf, newdbuf, TRUE, db);
			free(newdbuf);
		}
		return res;
	}
	newsrc = ++src;	/* skip db->db_left */
	newdst = dst;
	level = 1;
	while ((*newsrc != db->db_right || --level > 0) && *newsrc != '\0')
		if (*newsrc++ == db->db_left)
			level++;
	if (*newsrc != db->db_right || level > 0) {
		errf("unmatched {");
		return -1;
	}
	newsrc++;
	level = 1;
	new_no_curlies = TRUE;
	while (src < newsrc) {
		if ((*src == db->db_sep && level==1) ||
				(*src == db->db_right && --level == 0)) {
			if (do_braces(dbuf,newsrc,newdst,no_curlies && 
						new_no_curlies, db) == -1)
				return -1;
			src++;
			newdst = dst;
			new_no_curlies = TRUE;
		}
		else {
			if ((*newdst++ = *src++) == db->db_left) {
				level++;
				new_no_curlies = FALSE;
			}
		}
	}
	return 0;
}

/*  Public interface to do_braces().
 *
 *  Return a dvec type vector of strings on success.
 *
 *  Return 0 and give a message if do_braces() fails.
 */
dvec_t
arg_expand_braces(s, lch, rch, sep)
const char *s;
int lch, rch, sep;
{
	char *buf;
	int res;
	do_braces_arg_t dbbuf;

	buf = e_malloc(strlen(s) + 1);
	dbbuf.db_dv = make_dvec();
	dbbuf.db_left = lch;
	dbbuf.db_right = rch;
	dbbuf.db_sep = sep;
	res = do_braces(buf, s, buf, TRUE, &dbbuf);
	free(buf);
	if (res != 0) {
		free_dvec_and_strings(dbbuf.db_dv);
		return 0;
	}
	add_to_dvec(dbbuf.db_dv, (char *)NULL);
	return dbbuf.db_dv;
}
