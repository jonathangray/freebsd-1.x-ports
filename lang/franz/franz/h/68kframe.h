/*
 * $Header: /a/cvs/386BSD/ports/lang/franz/franz/h/68kframe.h,v 1.1 1994/03/18 16:26:19 jkh Exp $
 * $Locker:  $
 * machine stack frame
 */
struct machframe {
struct 	machframe	*fp;
	lispval	(*pc)();
	lispval ap[1];
};
