$!********************************************************************
$! lindner
$! 3.6
$! 1993/08/19 20:22:21
$! /home/mudhoney/GopherSrc/CVS/gopher+/gopher/compile.com,v
$! Exp
$!
$! Paul Lindner, University of Minnesota CIS.
$!
$! Copyright 1991, 1992 by the Regents of the University of Minnesota
$! see the file "Copyright" in the distribution for conditions of use.
$!********************************************************************
$! MODULE: compile.com
$! compiling script for VMS gopher client
$!*********************************************************************
$! Revision History:
$! compile.com,v
$! Revision 3.6  1993/08/19  20:22:21  lindner
$! mods for openvms
$!
$! Revision 3.5  1993/08/16  18:48:33  lindner
$! Alpha and DECC mods
$!
$! Revision 3.4  1993/08/05  03:24:37  lindner
$! Changes for CMUIP and NETLIB
$!
$! Revision 3.3  1993/06/22  06:12:53  lindner
$! mods for DEC C
$!
$! Revision 3.2  1993/05/20  06:02:51  lindner
$! updates for VMS compatibility
$!
$! Revision 3.1.1.1  1993/02/11  18:02:56  lindner
$! Gopher+1.2beta release
$!
$! Revision 1.2  1993/01/07  22:51:06  lindner
$! Added download.c
$!
$! Revision 1.1  1992/12/31  06:08:20  lindner
$! Initial revision
$!
$!
$!********************************************************************/
$!
$ vg = 'f$verify(0)'
$! lindner
$! 3.4
$! 1993/08/05 03:24:37
$! /home/mudhoney/GopherSrc/CVS/gopher+/gopher/compile.com,v
$! Exp
$!
$! Paul Lindner, University of Minnesota CIS.
$!
$! Copyright 1991, 1992 by the Regents of the University of Minnesota
$! see the file "Copyright" in the distribution for conditions of use.
$!********************************************************************
$! MODULE: compile.com
$! compiling script for VMS gopher client
$!*********************************************************************
$! Revision History:
$! compile.com,v
$! Revision 3.6  1993/08/19  20:22:21  lindner
$! mods for openvms
$!
$! Revision 3.5  1993/08/16  18:48:33  lindner
$! Alpha and DECC mods
$!
$! Revision 3.4  1993/08/05  03:24:37  lindner
$! Changes for CMUIP and NETLIB
$!
$! Revision 3.3  1993/06/22  06:12:53  lindner
$! mods for DEC C
$!
$! Revision 3.2  1993/05/20  06:02:51  lindner
$! updates for VMS compatibility
$!
$! Revision 3.1.1.1  1993/02/11  18:02:56  lindner
$! Gopher+1.2beta release
$!
$! Revision 1.2  1993/01/07  22:51:06  lindner
$! Added download.c
$!
$! Revision 1.1  1992/12/31  06:08:20  lindner
$! Initial revision
$!
$!
$!********************************************************************/
$!
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
$ THEN
$ vg1 = f$verify(1)
$! DECC/Alpha:
$    define/nolog C$USER_INCLUDE [-],[-.object]
$    define/nolog DECC$USER_INCLUDE [-],[-.object]
$    cc := cc/prefix=(all,except=(connect,gethostbyname,htons,inet_addr,-
                                  setsockopt,socket))-
             /warning=(disable=implicitfunc)-
             /define=('P1'=1)
$ vg1 = 'f$verify(0)'
$ ELSE
$ vg1 = f$verify(1)
$! VAXC:
$    cc := cc/include=([-],[-.object])-
	     /define=('P1'=1)
$ vg1 = 'f$verify(0)'
$ ENDIF
$ vg1 = f$verify(1)
$!
$ cc cso.c
$ cc curcurses.c
$ cc globals.c
$ cc gopher.c
$ cc gopherrc.c
$ cc html.c
$ cc html2.c
$ cc hymoo.c
$ cc manager.c
$ cc ourutils.c
$ cc subprocs.c
$ cc download.c
$!
$ vg1 = 'f$verify(0)'
$ CLEANUP:
$    vg1 = f$verify(vg)
$exit
