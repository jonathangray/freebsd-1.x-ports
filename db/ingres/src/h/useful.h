/*
**  USEFUL.H -- useful stuff.
**
**	Version:
**		@(#)useful.h	8.7	5/30/88
*/
#ifndef INGRES_USEFUL_H_
#define INGRES_USEFUL_H_

#ifndef TRUE
#define TRUE	1		/* logical one, true, yes, ok, etc.*/
#define FALSE	0		/* logical zero, false, no, nop, etc. */
#endif /* !TRUE */

typedef char	bool;		/* the boolean type */

#ifndef BITISSET
#define	BITISSET(bit, word)	((bit) & (word))
#define	BITSET(bit, word)	word |= bit
#define	BITCLR(bit, word)	word &= ~bit
#endif /* BITISSET */

#ifndef min
#define	min(a, b)	(((a) < (b))? (a): (b))
#define	max(a, b)	(((a) > (b))? (a): (b))
#endif /* min */

#ifndef HELP_RELINFO
/* macros for the HELP function */
#define HELP_RELINFO	0	/* info about a relation */
#define HELP_MANSEC	1	/* manual section */
#define HELP_DELLIST	2	/* list of delimitors */
#define HELP_RELLIST	3	/* list of relations */
#define HELP_ALLRELINFO	4	/* info about all relations */
#define HELP_ALLDELLIST	5	/* list of all delimitors */
#endif /* HELP_RELINFO */

/* The following are BNF grammar delimiter types used by parser/tree.c */
#define	RE_ONE		0
#define	RE_ZEROMORE	1
#define	RE_LBRACKET	'['
#define	RE_RBRACKET	']'
#define	RE_LBRACE	'{'
#define	RE_RBRACE	'}'

/*
** Various manifest constants storage constants.
*/
#define	MAXF4		2147483647.0
#define	MINF4		-2147483648.0

#define	MAXI2		127
#define	MINI2		-128

#define	MAXI4		32767
#define	MINI4		-32768

#define	MAXI8		2147483647
#define	MINI8		-2147483648


#define	I1MASK		0377		/* mask out sign extension that occurs
					**  when a c1 or i1 field is converted
					**  to an i2 field. 
					*/

/* modes for dest_const in dbu/rmqm.c */
#define DESTPROT	5	/* destroy permission */
#define	DESTINTEG	6	/* destroy integrity constraint */

/* modes for the return values of replace() in iutil/replace.c */
#define	NEWTUP		0
#define	DUPTUP		1	/* new tuple duplicate of returned tid*/
#define	DELTUP		2	/* tuple identified by tid deleted */
#define	BADLID		3	

/* macro for gutil/cat.c */
#define	BLOCK_SZ	512

/* macros for return codes of initucode and initdbpath */
#define	NODB		1
#define	NOACCESS	2
#define	INVALIDUSR	3
#define	NODBNAME	4
#define	INDIRECT	5
#define	INDNODB		6

#define	DBEXIST		0
#define	PTR2DB		1
#define	NODBS		2
#define	PTR2NODBS	3

/* macros for bitmap in dbu/pr_prot.c */
#define BITMAP_SZ	4
#define	NUMSHIFTS	32

/* macros for conversion routines in gutil */
#define	CONCAT_BUFSZ	101	/* buffer for string concats */
#define	LONG_BUFSZ	30	/* buffer for long conversion*/
#define	CHAR_SZ		7	/* buffer for int to ascii conversion */

#endif /* !INGRES_USEFUL_H_ */
