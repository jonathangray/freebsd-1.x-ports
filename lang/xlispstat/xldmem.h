#ifndef XLDMEM_H
#define XLDMEM_H

/* xldmem.h - dynamic memory definitions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

/* small fixnum range */
#define SFIXMIN		(-128)
#define SFIXMAX		255
#define SFIXSIZE	384

/* character range */
#define CHARMIN		0
#define CHARMAX		255
#define CHARSIZE	256

/* new node access macros */
#define ntype(x)	((x)->n_type)

/* cons access macros */
#define car(x)		((x)->n_car)
#define cdr(x)		((x)->n_cdr)
#define rplaca(x,y)	((x)->n_car = (y))
#define rplacd(x,y)	((x)->n_cdr = (y))

/* symbol access macros */
#define getvalue(x)	 ((x)->n_vdata[0])
#define setvalue(x,v)	 ((x)->n_vdata[0] = (v))
#define getfunction(x)	 ((x)->n_vdata[1])
#define setfunction(x,v) ((x)->n_vdata[1] = (v))
#define getplist(x)	 ((x)->n_vdata[2])
#define setplist(x,v)	 ((x)->n_vdata[2] = (v))
#define getpname(x)	 ((x)->n_vdata[3])
#define setpname(x,v)	 ((x)->n_vdata[3] = (v))
#define SYMSIZE		4

/* closure access macros */
#define getname(x)     	((x)->n_vdata[0])
#define setname(x,v)   	((x)->n_vdata[0] = (v))
#define gettype(x)    	((x)->n_vdata[1])
#define settype(x,v)  	((x)->n_vdata[1] = (v))
#define getargs(x)     	((x)->n_vdata[2])
#define setargs(x,v)   	((x)->n_vdata[2] = (v))
#define getoargs(x)    	((x)->n_vdata[3])
#define setoargs(x,v)  	((x)->n_vdata[3] = (v))
#define getrest(x)     	((x)->n_vdata[4])
#define setrest(x,v)   	((x)->n_vdata[4] = (v))
#define getkargs(x)    	((x)->n_vdata[5])
#define setkargs(x,v)  	((x)->n_vdata[5] = (v))
#define getaargs(x)    	((x)->n_vdata[6])
#define setaargs(x,v)  	((x)->n_vdata[6] = (v))
#define getbody(x)     	((x)->n_vdata[7])
#define setbody(x,v)   	((x)->n_vdata[7] = (v))
#define getenv(x)	((x)->n_vdata[8])
#define setenv(x,v)	((x)->n_vdata[8] = (v))
#define getfenv(x)	((x)->n_vdata[9])
#define setfenv(x,v)	((x)->n_vdata[9] = (v))
#define getlambda(x)	((x)->n_vdata[10])
#define setlambda(x,v)	((x)->n_vdata[10] = (v))
#define CLOSIZE		11

/* vector access macros */
#define getsize(x)	((x)->n_vsize)
#define getelement(x,i)	((x)->n_vdata[i])
#define setelement(x,i,v) ((x)->n_vdata[i] = (v))
#define setconstant(x, c) ((x)->n_vconst = (c))  /* L. Tierney */
#define isconstant(x) ((x)->n_vconst)            /* L. Tierney */

/* object access macros */
#define getclass(x)	((x)->n_vdata[0])
#define getivar(x,i)	((x)->n_vdata[i+1])
#define setivar(x,i,v)	((x)->n_vdata[i+1] = (v))

/* subr/fsubr access macros */
#define getsubr(x)	((x)->n_subr)
#define getoffset(x)	((x)->n_offset)

/* fixnum/flonum/char access macros */
#define getfixnum(x)    ((x)->n_fixnum)
#define getflonum(x)	((x)->n_flonum)
#define getchcode(x)	((x)->n_chcode)

/* string access macros */
#define getstring(x)	((x)->n_string)
#define getslength(x)	((x)->n_strlen)

/* file stream access macros */
#define getfile(x)	((x)->n_fp)
#define setfile(x,v)	((x)->n_fp = (v))
#define getsavech(x)	((x)->n_savech)
#define setsavech(x,v)	((x)->n_savech = (v))

/* unnamed stream access macros */
#define gethead(x)	((x)->n_car)
#define sethead(x,v)	((x)->n_car = (v))
#define gettail(x)	((x)->n_cdr)
#define settail(x,v)	((x)->n_cdr = (v))

/* allocated data access macros *//* L. Tierney */
#define getadaddr(x)	((x)->n_adaddr)
#define getadreloc(x)	((int)((x)->n_adreloc))
#define getadsize(x)	(((x)->n_adsize))

/* node types */
#define FREE	0
#define SUBR	1
#define FSUBR	2
#define CONS	3
#define SYMBOL	4
#define FIXNUM	5
#define FLONUM	6
#define STRING	7
#define OBJECT	8
#define STREAM	9
#define VECTOR	10
#define CLOSURE	11
#define CHAR	12
#define USTREAM	13
#define COMPLEX 14            /* L. Tierney */
#define DISPLACED_ARRAY 15    /* L. Tierney */
#define ALLOCATED_DATA 16     /* L. Tierney */
#define STRUCT	17

/* subr/fsubr node */
#define n_subr		n_info.n_xsubr.xs_subr
#define n_offset	n_info.n_xsubr.xs_offset

/* cons node */
#define n_car		n_info.n_xcons.xc_car
#define n_cdr		n_info.n_xcons.xc_cdr

/* fixnum node */
#define n_fixnum	n_info.n_xfixnum.xf_fixnum

/* flonum node */
#define n_flonum	n_info.n_xflonum.xf_flonum
/* character node */
#define n_chcode	n_info.n_xchar.xc_chcode

/* string node */
#define n_string	n_info.n_xstring.xs_string
#define n_strlen	n_info.n_xstring.xs_length

/* stream node */
#define n_fp		n_info.n_xstream.xs_fp
#define n_savech	n_info.n_xstream.xs_savech

/* vector/object node */
#define n_vsize		n_info.n_xvector.xv_size
#define n_vdata		n_info.n_xvector.xv_data
#define n_vconst	n_info.n_xvector.xv_const  /* L. Tierney */

/* allocated data node *//* L. Tierney */
#define n_adaddr	n_info.n_xadata.xa_addr
#define n_adreloc	n_info.n_xadata.xa_reloc
#define n_adsize	n_info.n_xadata.xa_size

/* node structure */
typedef struct node {
    char n_type;		/* type of node */
    char n_flags;		/* flag bits */
    union ninfo { 		/* value */
	struct xsubr {		/* subr/fsubr node */
#ifdef ANSI
	    struct node *(*xs_subr)(void);	/* function pointer */
#else
	    struct node *(*xs_subr)();	/* function pointer */
#endif
	    int xs_offset;		/* offset into funtab */
	} n_xsubr;
	struct xcons {		/* cons node */
	    struct node *xc_car;	/* the car pointer */
	    struct node *xc_cdr;	/* the cdr pointer */
	} n_xcons;
	struct xfixnum {	/* fixnum node */
	    FIXTYPE xf_fixnum;		/* fixnum value */
	} n_xfixnum;
	struct xflonum {	/* flonum node */
	    FLOTYPE xf_flonum;		/* flonum value */
	} n_xflonum;
	struct xchar {		/* character node */
	    int xc_chcode;		/* character code */
	} n_xchar;
	struct xstring {	/* string node */
	    int xs_length;		/* string length */
	    unsigned char *xs_string;	/* string pointer */
	} n_xstring;
	struct xstream { 	/* stream node */
	    FILE *xs_fp;		/* the file pointer */
	    int xs_savech;		/* lookahead character */
	} n_xstream;
	struct xvector {	/* vector/object/symbol/structure node */
	    int xv_size;		/* vector size */
	    struct node **xv_data;	/* vector data */
	    char xv_const;              /* constant flag for symbols - L. Tierney */
	} n_xvector;
	struct xadata {
	    char xa_reloc;
	    char *xa_addr;
	    long xa_size;
	} n_xadata;
    } n_info;
} *LVAL;

/* memory segment structure definition */
typedef struct segment {
    int sg_size;
    struct segment *sg_next;
    struct node sg_nodes[1];
} SEGMENT;

/* memory allocation functions */
extern LVAL cons();		/* (cons x y) */
extern LVAL cvsymbol();       	/* convert a string to a symbol */
extern LVAL cvstring();       	/* convert a string */
extern LVAL cvfile();		/* convert a FILE * to a file */
extern LVAL cvsubr();		/* convert a function to a subr/fsubr */
extern LVAL cvfixnum();       	/* convert a fixnum */
extern LVAL cvflonum();       	/* convert a flonum */
extern LVAL cvchar();		/* convert a character */

extern LVAL newstring();	/* create a new string */
extern LVAL newvector();	/* create a new vector */
extern LVAL newobject();	/* create a new object */
extern LVAL newclosure();	/* create a new closure */
extern LVAL newustream();	/* create a new unnamed stream */
extern LVAL newcomplex();       /* L. Tierney */
extern LVAL newadata();		/* convert an internal data pointer - L. Tirney*/
extern LVAL newstruct();	/* create a new structure */

#endif XLDMEM_H
