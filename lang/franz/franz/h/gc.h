/*					-[Sat Jan 29 13:56:06 1983 by jkf]-
 * 	gc.h			$Locker:  $
 * garbage collector metering definitions
 *
 * $Header: /a/cvs/386BSD/ports/lang/franz/franz/h/gc.h,v 1.1 1994/03/18 16:26:19 jkh Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
struct gchead
	{  int version;	/* version number of this dump file */
	   int lowdata;	/* low address of sharable lisp data */
	   int dummy,dummy2,dummy3; 	/* to be used later	*/
	};

