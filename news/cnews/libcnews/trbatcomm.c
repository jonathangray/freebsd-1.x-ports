/*
 * transmit batch file management (common support code)
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "msgs.h"
#include "trbatch.h"

struct batchfile batchfile[NOPENBFS];	/* try to keep open always */
static struct batchfile fakebatf;	/* for non-cached batch files */
/*
 * More than one pointer in ordtobfs may point at a given batchfile,
 * to permit sharing of open batch files among multiple sys entries.
 * ordtobfs[ordinal # of batch sys entry] -> (usually open) batch file,
 * if the index is in range.
 */
struct batchfile *ordtobfs[NOPENBFS];

/*
 * any stream-specific set up (reserved for future use)
 */
bfsetup(bf)
register struct batchfile *bf;
{
	if (!fnlockfile(bf->bf_str))	/* UUNET: lock against exploder */
		warning("can't lock master batch file `%s'", bf->bf_name);
}

/* ARGSUSED ord */
statust
bffkclose(ord)			/* close current (ord's) batchfile, if fake */
int ord;
{
	register statust status = ST_OKAY;

	if (fakebatf.bf_str != NULL)
		status |= bfclose(&fakebatf);
	return status;
}

statust
bfclose(bf)
register struct batchfile *bf;
{
	register statust status = ST_OKAY;

	if (nfclose(bf->bf_str) == EOF)
		status = prfulldisk(bf->bf_name);
	bf->bf_str = NULL;	/* prevent accidents; mark as closed */
	return status;
}

struct batchfile *
fakebf(stream, name)
FILE *stream;
char *name;
{
	fakebatf.bf_name = name;
	fakebatf.bf_str = stream;
	return &fakebatf;
}

/*
 * a performance hack: only fflush bf->bf_str every FLUSHEVERY calls.
 */
int
bfflush(bf)
register struct batchfile *bf;
{
	register int ret = 0;

	if (--bf->bf_lines <= 0) {
		bf->bf_lines = FLUSHEVERY;
		ret = fflush(bf->bf_str);
	}
	return ret;
}

statust
bfrclose()				/* close an arbitrary batchfile */
{
	register struct batchfile *bf;
	register statust status = ST_OKAY;

	for (bf = batchfile; bf <= lastbf; bf++)
		if (bf->bf_str != NULL) {
			status |= bfclose(bf);
			break;
		}
	return status;
}

statust
bfrealclose()				/* close all open batch files */
{
	register struct batchfile *bf;
	register statust status = ST_OKAY;

	for (bf = batchfile; bf <= lastbf; bf++) {
		if (bf->bf_str != NULL)		/* batch file stream open */
			status |= bfclose(bf);
		nnfree(&bf->bf_name);
		nnfree(&bf->bf_msgid);
#ifdef notdef
		bf->bf_ref = 0;
#endif
		ordtobfs[bf - batchfile] = NULL;	/* unmap batch file */
	}
	return status;
}
