/*
 *	$Header: /a/cvs/386BSD/ports/db/ingres/src/h/sre.h,v 1.1 1994/06/19 04:07:48 alm Exp $
 *	%W% %E% %U%
 *
 *	Modified by agc for the wi editor, and the ineterpreter.
 *	This only amounts to ANSI-ising the function definitions, via
 *	the PROTOARGS and PROTOTYPE arguments in port.h
 *	I also included regmagic.h in regexp.h to cut down on the number
 *	of files.
 */
/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#ifndef SRE_H
#define SRE_H

#define NSUBEXP  10

typedef struct srestr {
	char	*startp[NSUBEXP];
	char	*endp[NSUBEXP];
	int	regsize;		/* Internal use only. */
	char	regstart;		/* Internal use only. */
	char	reganch;		/* Internal use only. */
	char	*regmust;		/* Internal use only. */
	int	regmlen;		/* Internal use only. */
	char	program[1];		/* Unwarranted chumminess with compiler. */
} SRE;

SRE *SREcomp(char *s);
int SREexec(SRE *prog, char *string);
void SREsub(SRE *prog, char *source, char *dest);
void SREerror(char *s);
void SREfree(SRE *sp);
int SREsize(SRE *prog);

/*
 *	From here to the end of file used to be in "regmagic.h" - agc
 */
 
/*
 * The first byte of the SRE internal "program" is actually this magic
 * number; the start node begins in the second byte.
 */

#define	SRE_MAGIC	0234

#endif /* !SRE_H */
