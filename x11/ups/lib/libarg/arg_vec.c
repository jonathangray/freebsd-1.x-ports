/* arg_vec.c - implementation of variable size vectors of char pointers */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char arg_vec_sccsid[] = "@(#)arg_vec.c	1.9 26/4/92 (UKC)";


#include <stdlib.h>

#include <local/ukcprog.h>

#include "arg.h"

/*  Variable size vector of strings (could do with C++ here)
 *
 *  This structure is accessed only through functions in this file - we
 *  cast to/from dvec_t (which is typedef'ed to long) returning or
 *  accepting pointers to this.
 */
typedef struct idvecst {
	int idv_vecsize;
	const char **idv_vec;
	int idv_index;
} idvec_t;

dvec_t
make_dvec()
{
	idvec_t *idv;

	idv = (idvec_t *) e_malloc(sizeof(idvec_t));
	idv->idv_vecsize = 16;
	idv->idv_vec = (const char **)e_malloc(idv->idv_vecsize * sizeof(char *));
	idv->idv_index = 0;
	return (dvec_t)idv;
}

void
add_to_dvec(dv, s)
dvec_t dv;
const char *s;
{
	idvec_t *idv;

	idv = (idvec_t *) dv;
	if (idv->idv_index == idv->idv_vecsize) {
		idv->idv_vecsize *= 2;
		idv->idv_vec = (const char **) e_realloc((char *)idv->idv_vec,
						  idv->idv_vecsize * sizeof(char *));
	}
	idv->idv_vec[idv->idv_index++] = s;
}

int
get_dvec_size(dv)
dvec_t dv;
{
	return ((idvec_t *)dv)->idv_index;
}

const char **
get_dvec_vec(dv)
dvec_t dv;
{
	return ((idvec_t *)dv)->idv_vec;
}

void
free_dvec(dv)
dvec_t dv;
{
	idvec_t *idv;

	idv = (idvec_t *) dv;
	free((char *)idv->idv_vec);
	free((char *)idv);
}

/*  Free a dvec and the strings it holds.
 *
 *  BUG: the strings in a dvec are "const char *", but we cast them
 *       to "char *" to pass them to free.  Thus this routine assumes
 *       that all the strings added were malloc'ed.  We don't want
 *       to make the dvec strings "char *", as this would stop us
 *       adding non writeable strings.
 */
void
free_dvec_and_strings(dv)
dvec_t dv;
{
	idvec_t *idv;
	const char **sptr, **lim;

	idv = (idvec_t *) dv;
	sptr = idv->idv_vec;
	lim = sptr + idv->idv_index;
	for (; sptr < lim; sptr++)
		if (*sptr != NULL)
			free((char *)*sptr);
	free_dvec(dv);
}
