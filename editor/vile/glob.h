/*
 *	glob.h
 *
 * $Log: glob.h,v $
 * Revision 1.1  1994/02/01 03:29:24  jkh
 * Initial revision
 *
 * Revision 1.4  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.3  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.2  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.1  1993/04/21  14:38:41  pgf
 * glob mode support
 *
 * Revision 1.0  1993/04/20  12:14:39  pgf
 * Initial revision
 *
 */

/*
 * Special characters used in globbing
 */
#define	GLOB_MULTI	'*'
#define	GLOB_SINGLE	'?'
#define	GLOB_ELLIPSIS	"..."
#define	GLOB_RANGE	"[]"

/*
 * Configuration options
 */
#define	OPT_GLOB_ENVIRON	ENVFUNC && !SMALLER
#define	OPT_GLOB_ELLIPSIS	VMS || UNIX || (MSDOS && !SMALLER)
#define	OPT_GLOB_PIPE		UNIX
#define	OPT_GLOB_RANGE		UNIX || (MSDOS && !SMALLER)

#if !UNIX
extern	int	glob_needed P((char **));
#endif
extern	char **	glob_expand P((char **));
extern	char **	glob_string P((char *));
extern	int	glob_length P((char **));
extern	char **	glob_free   P((char **));

#if !UNIX
extern	void	expand_wild_args P(( int * , char ***));
#endif

extern	int	doglob P(( char * ));
