#ifndef lint
static char *rcsid =
   "$Header: /a/cvs/386BSD/ports/lang/franz/franz/fpipe.c,v 1.1 1994/03/18 16:26:17 jkh Exp $";
#endif


/*					-[Sat Jan 29 12:44:16 1983 by jkf]-
 * 	fpipe.c				$Locker:  $
 * pipe creation
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include <signal.h>

FILE *fpipe(info)
FILE *info[2];
{
	register FILE *p;
	int fd[2];

	if(0 > pipe(fd)) return( (FILE *) -1);

	if(NULL==(p = fdopen(fd[0],"r"))) {
		close(fd[0]);
		close(fd[1]);
		return( (FILE *) -1);
	}
	info[0] = p;
	if(NULL==(p = fdopen(fd[1],"w"))) {
		close(fd[0]);
		close(fd[1]);
		return( (FILE *) -1);
	}
	info[1] = p;

	return((FILE *) 2); /*indicate sucess*/
}
/* Nioreset *************************************************************/

lispval
Nioreset() {
#ifndef	RTPORTS
	register FILE *p;

	for(p = &_iob[3]; p < _iob + _NFILE; p++) {
		if(p->_flag & (_IOWRT | _IOREAD)) fclose(p);
		}
#else	RTPORTS
	lispval NiorUtil();

	_fwalk(NiorUtil);
#endif	RTPORTS
	return(nil);
}

#ifdef RTPORTS
FILE FILEdummy;

static lispval
NiorUtil(p)
FILE *p;
{
	lispval handy;
	if(p==stdin||p==stdout||p==stderr)
		return(0);
	fclose(p);
	handy = P(p);
	if(TYPE(handy)==PORT) {
		handy->p = &FILEdummy;
	}
	return(nil);
}
FILE **xports;

#define LOTS (LBPG/(sizeof (FILE *)))
lispval P(p)
FILE *p;
{
	register FILE **q;
	extern int fakettsize;

	if(xports==((FILE **) 0)) {
		/* this is gross.  I don't want to change csegment -- kls */
		xports = (FILE **) csegment(OTHER,LOTS,0);
		SETTYPE(xports,PORT,31);
		for(q = xports; q < xports + LOTS; q++) {
			*q = &FILEdummy;
		}
	}
	for(q = xports; q < xports + LOTS; q++) {
		if(*q==p) return ((lispval)q);
		if(*q==&FILEdummy) {
			*q = p;
			return ((lispval)q);
		}
	}
	/* Heavens above knows this could be disasterous in makevals() */
	error("Ran out of Ports",FALSE);
}

#endif	RTPORTS

FILE *
fstopen(base,count,flag)
char *base;
char *flag;
{
	register FILE *p = fdopen(0,flag);

#ifdef torek_stdio
	p->_flags |= __SSTR;
	if(flag[0] == 'r')
	    p->_r = p->_bf._size = count;
	else
	    p->_w = p->_bf._size = count;
	p->_p = p->_bf._base = base;
	p->_file = -1;
#else
	p->_flag |= _IOSTRG;
	p->_cnt = count;
	p->_ptr = p->_base = base;
	p->_file = -1;
#endif
	return(p);
}

#ifdef SPISFP
#ifndef i386_4_3
char *
alloca(howmuch)
register int howmuch;
{
	howmuch += 3 ;
	howmuch >>= 2;
	xsp -= howmuch;
	if (xsp < xstack) {
		xsp += howmuch;
		xserr();
	}
	return((char *) xsp);
}
#endif
#endif