/* -*-C-*- typedefs.h */
/*-->typedefs*/
/**********************************************************************/
/****************************** typedefs ******************************/
/**********************************************************************/

#if    IBM_PC_LATTICE
typedef char BOOLEAN;			/* value in (FALSE,TRUE) */
#else
typedef unsigned char BOOLEAN;		/* value in (FALSE,TRUE) */
#endif

#define  FALSE		  0
#define  TRUE		  1

#if    IBM_PC_LATTICE
typedef unsigned BYTE;			/* unsigned 8-bit integers */
#else
typedef unsigned char BYTE;		/* unsigned 8-bit integers */
#endif

typedef short int INT8;			/* signed  8-bit integers (C has no
					  "signed char" type) yet */
typedef short int INT16;		/* signed 16-bit integers */
typedef long      INT32;		/* signed 32-bit integers */

#if    IBM_PC_LATTICE
typedef unsigned UNSIGN16;		/* unsigned 16-bit integers */
#else
typedef unsigned short UNSIGN16;	/* unsigned 16-bit integers */
#endif

#if    IBM_PC_LATTICE
typedef long UNSIGN32;			/* unsigned 32-bit integers */
#else
typedef unsigned long UNSIGN32;		/* unsigned 32-bit integers */
#endif

typedef short int COORDINATE;		/* signed integer (x,y) coordinate */
					/* variable in bit map */

#if    PCC_20
/* typedef int void; */			/* void added locally to stdio.h */
#endif

#if    IBM_PC_LATTICE
/* typedef int void; */			/* not needed with version 3 */
#endif
