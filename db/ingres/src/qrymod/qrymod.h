/*
**  QRYMOD.H -- Query Modification header file.
**
**	Contains the manifest constants and global variables
**	used by the query modification process.
**
**	Version:
**		@(#)qrymod.h	8.1	12/31/84
*/

extern desc_t	Treedes;	/* descriptor for tree catalog */


struct {
	short	qm_newresvar;	/* new result variable number */
}  Qm;


/*********************************************************************
**								    **
**  The following stuff is used by the protection algorithm only.   **
**								    **
*********************************************************************/
/* maximum query mode for p_opset (<--> sizeof(Proopmap) - 1) */
#define	MAXPROQM	4
