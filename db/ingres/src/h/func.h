/*
**  FUNC.H -- declarations for function headers.
**
**	Version:
**		@(#)func.h	8.1	12/31/84
*/
#ifndef INGRES_FUNC_H_
#define INGRES_FUNC_H_

/* the function definition struct */
typedef struct fn_def {
	char		*fn_name;	/* the name of the function */
	int		(*fn_fn)();	/* a pointer to the actual function */
	void		(*fn_initfn)();	/* initialization function */
	void		(*fn_cleanup)();/* interrupt cleanup function */
	char		*fn_gptr;	/* pointer to global space */
	unsigned	fn_gsize;	/* size of global space */
	short		*fn_tvect;	/* the trace vector itself */
	short		fn_tsize;	/* size of trace vector */
	char		fn_tflag;	/* the trace flag letter */
	char		fn_active;	/* > 0 if active */
} func_t;

extern func_t	*FuncVect[];
extern int	NumFunc;	/* the number of functions */

#endif /* !INGRES_FUNC_H_ */
