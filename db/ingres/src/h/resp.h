/*
**  RESP.H -- response blocks
**
**	Version:
**		@(#)resp.h	8.1	12/31/84
*/
#ifndef INGRES_RESP_H_
#define INGRES_RESP_H_

typedef struct resp {
	short	resp_resp;	/* the function value */
	char	resp_active;	/* > 0 if in use */
	long	resp_time;	/* elapsed time */
	long	resp_tups;	/* count of tuples touched */
	long	resp_pread;	/* pages read */
	long	resp_pwrit;	/* pages written */
	/* paramv_t	resp_rval;	 the module return value */
} resp_t;

#endif /* !INGRES_RESP_H_ */
