/*					-[Sat Jan 29 14:02:34 1983 by jkf]-
 * 	vaxframe.h			$Locker:  $
 * vax calling frame definition
 *
 * $Header: /a/cvs/386BSD/ports/lang/franz/franz/h/vaxframe.h,v 1.1 1994/03/18 16:26:19 jkh Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */

struct machframe {
	lispval	(*handler)();
	long	mask;
	lispval	*ap;
struct 	machframe	*fp;
	lispval	(*pc)();
	lispval	*r6;
	lispval *r7;
};
