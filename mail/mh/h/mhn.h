/* mhn.h - definitions for mhn & friends */
/* @(#)mhn.h,v 1.1.1.1 1993/01/30 04:40:24 jtc Exp */

#define	VRSN_FIELD	"MIME-Version"
#define	VRSN_VALUE	"1.0"

#define	XXX_FIELD_PRF	"Content-"

#define	TYPE_FIELD	"Content-Type"

#define	ENCODING_FIELD	"Content-Transfer-Encoding"

#define	ID_FIELD	"Content-ID"

#define	DESCR_FIELD	"Content-Description"


#define	isatom(c) \
    	(!isspace (c) \
	    && !iscntrl (c) \
	    && (c) != '(' \
	    && (c) != ')' \
	    && (c) != '<' \
	    && (c) != '>' \
	    && (c) != '@' \
	    && (c) != ',' \
	    && (c) != ';' \
	    && (c) != ':' \
	    && (c) != '\\' \
	    && (c) != '"' \
	    && (c) != '.' \
	    && (c) != '[' \
	    && (c) != ']')

#define	istoken(c) \
    	(!isspace (c) \
	    && !iscntrl (c) \
	    && (c) != '(' \
	    && (c) != ')' \
	    && (c) != '<' \
	    && (c) != '>' \
	    && (c) != '@' \
	    && (c) != ',' \
	    && (c) != ';' \
	    && (c) != ':' \
	    && (c) != '\\' \
	    && (c) != '"' \
	    && (c) != '/' \
	    && (c) != '[' \
	    && (c) != ']' \
	    && (c) != '?' \
	    && (c) != '=')

/* MTR: removed now, since likely to go away in the future
	    && (c) != '.' \
 */

/*  */

#define	CPERLIN	76
#define	BPERLIN	(CPERLIN / 4)
#define	LPERMSG	632
#define	CPERMSG	(LPERMSG * CPERLIN)

/*  */

#if	defined(BSD42) || defined(SOCKETS)
#define	FTP
#endif
