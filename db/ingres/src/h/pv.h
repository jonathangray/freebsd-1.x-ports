/*
**  PV.H -- definitions for parameter vectors
**
**	Version:
**		@(#)pv.h	8.2	5/30/88
*/
#ifndef INGRES_PV_H_
#define INGRES_PV_H_

/* setable constants */
#define	PV_MAXPC	500	/* maximum number of parameters */

/* the parameter vector type */
typedef struct {
	short	pv_type;	/* the type, see below */
	short	pv_len;		/* the length of the value */
	union {
		short			pv_int;		/* PV_INT */
		struct querytree	*pv_qtree;	/* PV_QTREE */
		char			*pv_str;	/* PV_STR */
		char			*pv_tuple;	/* PV_TUPLE */
	} pv_val;
}  paramv_t;

/* pv_type values */
#define	PV_EOF		0	/* end of list */
#define	PV_INT		1	/* integer */
#define	PV_STR		2	/* string */
#define	PV_QTREE	3	/* query tree */
#define	PV_TUPLE	4	/* tuple */

#endif /* !INGRES_PV_H_ */
