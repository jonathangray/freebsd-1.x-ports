/*
**  BATCH.H -- batch file declarations.
**
**	Version:
**		@(#)batch.h	8.1	12/31/84
*/
#ifndef INGRES_BATCH_H_
#define INGRES_BATCH_H_

#include "gconst.h"

#define	BATCHSIZE	506	/* available buffer space */
#define	IDSIZE		6	/* size of file id */

struct batchbuf {
	char	b_file[IDSIZE];	/* unique file name identifier */
	char	b_buf[BATCHSIZE];	/* buffer for batch storage */
};

/* this struct describes a secondary index domain */
struct si_doms {
	short	sd_off;		/* offset in primary tuple */
	short	sd_savedoff;	/* offset in saved tuple-old */
	short	sd_domsize;	/* width of the domain */
				/* if zero then domain not used */
};

struct batchhd {
	char	b_dbname[MAXFILENAMESIZ + 1];	/* data base name */
	char	b_relname[MAX_NAME_SIZE + 1];	/* relation name */
	char	b_user[USERCODE_SIZE];	/* ingres user code */
	long	b_updc	;	/* actual number of tuples to be updated */
	short	b_updtype;	/* type of update */
	short	b_oldtidsize;	/* width of old_tuple_id field */
	short	b_oldtupsize;	/* width of old tuple */
	short	b_newtidsize;	/* width of new_tuple_id field */
	short	b_newtupsize;	/* width of new tuple */
	short	b_domc;	/* number of sec. index domains affected */
	struct si_doms	b_domv[MAX_DOMAINS+1];	/* entry for each domain with sec. index */
};

short	Batch_fp;	/* file descriptor for batch file */
short	Batch_cnt;	/* number of bytes taken from the current buffer */
short	Batch_dirty;	/* used during update to mark a dirty page */
short	Batch_lread;	/* number of bytes last read in readbatch() */
short	Batch_recovery;	/* TRUE is this is recovery, else FALSE */

extern char	*Fileset;	/* unique id of batch maker */
struct batchbuf	Batchbuf;
struct batchhd	Batchhd;

FILE	*Repl_infp;
FILE	*Repl_outfp;

int	Del_cnt;
FILE	*Del_infp;
FILE	*Del_outfp;

#define	MODBATCH	"_SYSmod"
#define	MODTEMP		"_SYSnewr"
#define	ISAM_SORTED	"_SYSsort"
#define	ISAM_DESC	"_SYSdesc"
#define	ISAM_SPOOL	"_SYSspol"
#define	MOD_PREBATCH	"_SYSpreb"
#define	BTREESEC	"_SYSbsec"
#define	STEMP		"_SYSstemp"
#define	REPL_IN		"_SYSr_in"
#define	REPL_DESC	"_SYSr_desc"
#define	REPL_OUT	"_SYSr_out"
#define	DEL_IN		"_SYSd_in"
#define	DEL_DESC	"_SYSd_desc"
#define	DEL_OUT		"_SYSd_out"

#endif /* INGRES_BATCH_H_ */
