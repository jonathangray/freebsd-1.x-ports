#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <batch.h>
#include <version.h>
#include <pv.h>
#include <symbol.h>
#include <access.h>
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)sort.c	8.1	12/31/84)

void
sortfile(char *infile, desc_t *d, int del)
{
	char	out[MAX_NAME_SIZE + 4];
	char	*temp;
	int	len;
	int	i;

	if (del) {
		concat(DEL_OUT, Fileset, out);
		if ((Del_outfp = fopen(out, "w")) == NULL)
			syserr("can't open %s", out);
		fclose(Del_outfp);
	} else {
		concat(REPL_OUT, Fileset, out);
		if ((Repl_outfp = fopen(out, "w")) == NULL)
			syserr("can't open %s", out);
		fclose(Repl_outfp);
	}

	flush_rel(d, TRUE);
	resetacc(NULL);

	len = strlen(Fileset) + 1;
	/*
	temp = (char *) need(Qbuf, len);
	*/
	temp = xalloc(len, 1, 1);
	bmove(Fileset, temp, len);

	initp();
	setp(PV_STR, temp, 0);
	setp(PV_STR, infile, 0);
	setp(PV_STR, out, 0);

	setp(PV_STR, d->d_r.r_id, 0);
	setp(PV_STR, d->d_r.r_owner, 0);
	setp(PV_INT, &d->d_r.r_spec, sizeof(d->d_r.r_spec));
	setp(PV_INT, &d->d_r.r_indexed, sizeof(d->d_r.r_indexed));
	setp(PV_INT, &d->d_r.r_status2, sizeof(d->d_r.r_status2));
	setp(PV_INT, &d->d_r.r_status, sizeof(d->d_r.r_status));
	setp(PV_INT, &d->d_r.r_savetime, sizeof(d->d_r.r_savetime));
	setp(PV_INT, &d->d_r.r_tupc, sizeof(d->d_r.r_tupc));
	setp(PV_INT, &d->d_r.r_attrc, sizeof(d->d_r.r_attrc));
	setp(PV_INT, &d->d_r.r_width, sizeof(d->d_r.r_width));
	setp(PV_INT, &d->d_r.r_primc, sizeof(d->d_r.r_primc));
	setp(PV_INT, &d->d_r.r_free, sizeof(d->d_r.r_free));
	setp(PV_INT, &d->d_r.r_modtime, sizeof(d->d_r.r_modtime));
	/* whether or not relation is ordered is irrelevant */
	i = 0;
	setp(PV_INT, &i, sizeof(i));

	setp(PV_STR, d->d_rangevar, 0);
	setp(PV_INT, &d->d_fd, sizeof(d->d_fd));
	setp(PV_INT, &d->d_opened, sizeof(d->d_opened));
	setp(PV_INT, &d->d_addc, sizeof(d->d_addc));
	setp(PV_INT, &d->d_tid, sizeof(d->d_tid));
	for (i = 0; i <= d->d_r.r_attrc; ++i) {
		setp(PV_INT, &d->d_off[i], sizeof(d->d_off[i]));
		setp(PV_INT, &d->d_fmt[i], sizeof(d->d_fmt[i]));
		setp(PV_INT, &d->d_len[i], sizeof(d->d_len[i]));
		setp(PV_INT, &d->d_iskey[i], sizeof(d->d_iskey[i]));
		setp(PV_INT, &d->d_given[i], sizeof(d->d_given[i]));
	}

	call(mdKSORT, NULL);

	flush_rel(d, TRUE);
	resetacc(NULL);

	if (del)
		fclose(Del_outfp);
	else
		fclose(Repl_outfp);
	cfree(temp);
}
