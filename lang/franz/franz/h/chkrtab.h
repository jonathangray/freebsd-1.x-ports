/*					-[Sat Jan 29 13:53:19 1983 by jkf]-
 * 	chkrtab.h			$Locker:  $
 * check if read table valid 
 *
 * $Header: /a/cvs/386BSD/ports/lang/franz/franz/h/chkrtab.h,v 1.1 1994/03/18 16:26:19 jkh Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
#define chkrtab(p);	\
	if(p!=lastrtab){ if(TYPE(p)!=ARRAY && TYPE(p->ar.data)!=INT) rtaberr();\
			else {lastrtab=p;ctable=(unsigned char*)p->ar.data;}}
extern lispval lastrtab;
