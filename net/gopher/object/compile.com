$!********************************************************************
$! lindner
$! 3.2
$! 1993/05/18 15:06:34
$! /home/mudhoney/GopherSrc/CVS/gopher+/object/compile.com,v
$! Exp
$!
$! Paul Lindner, University of Minnesota CIS.
$!
$! Copyright 1991, 1992 by the Regents of the University of Minnesota
$! see the file "Copyright" in the distribution for conditions of use.
$!********************************************************************
$! MODULE: compile.com
$! compiling script for VMS
$!*********************************************************************
$! Revision History:
$! compile.com,v
$!Revision 3.2  1993/05/18  15:06:34  lindner
$!Fixed CVS/RCS comment leaders...
$!
$! Revision 3.1  1993/05/05  18:44:07  lindner
$! new VMS files
$!
$! Revision 1.1  1992/12/31  05:19:12  lindner
$! Initial revision
$!
$!
$!********************************************************************/

$ vo = 'f$verify(0)'
$ IF P1 .EQS. ""
$ THEN
$    type/nopage sys$input:
     Usage:
	     @compile UCX
	     @compile WOLLONGONG
	     @compile MULTINET
	     @compile CMUIP
             @compile NETLIB
$    exit
$ ENDIF
$!
$ ON CONTROL_Y THEN GOTO CLEANUP
$ ON ERROR THEN GOTO CLEANUP
$!
$ IF f$trnlnm("VAXCMSG") .eqs. "DECC$MSG"
$!
$ THEN
$ vo1 = f$verify(1)
$! DECC/Alpha:
$    cc := cc/prefix=(all,except=(connect,gethostbyname,htons,inet_addr,-
                                  setsockopt,socket))-
             /warning=(disable=implicitfunc)-
             /define=('P1'=1)

$ vo1 = 'f$verify(0)'
$ ELSE
$ vo1 = f$verify(1)
$! VAXC:
$    cc := cc/define=('P1'=1)
$ vo1 = 'f$verify(0)'
$ ENDIF
$ vo1 = f$verify(1)
$!
$ cc compatible.c
$ cc daarray.c
$ cc gdgopherdir.c
$ cc gsgopherobj.c
$ cc strstring.c
$ cc util.c
$ cc blblock.c
$ cc VIews.c
$ cc Sockets.c
$ cc Debug.c
$!
$ cc getopt.c
$!
$ vo1 = 'f$verify(0)'
$ CLEANUP:
$    vo1 = f$verify(vo)
$exit
