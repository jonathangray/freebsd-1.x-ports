$!********************************************************************
$! lindner
$! 3.4
$! 1993/08/23 02:54:42
$! /home/mudhoney/GopherSrc/CVS/gopher+/gopher/link.com,v
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
$! link.com,v
$! Revision 3.4  1993/08/23  02:54:42  lindner
$! Fix for vmsopts
$!
$! Revision 3.3  1993/08/16  18:48:37  lindner
$! Alpha and DECC mods
$!
$! Revision 3.2  1993/08/05  03:24:39  lindner
$! Changes for CMUIP and NETLIB
$!
$! Revision 3.1.1.1  1993/02/11  18:02:59  lindner
$! Gopher+1.2beta release
$!
$! Revision 1.2  1993/01/07  22:54:37  lindner
$! Added download.obj
$!
$! Revision 1.1  1992/12/31  06:10:07  lindner
$! Initial revision
$!
$!
$!********************************************************************/
$!
$ IF P1 .EQS. ""
$ THEN
$    type/nopage sys$input:
     Usage:
	    @link UCX
	    @link WOLLONGONG
	    @link MULTINET
	    @link CMUIP
	    @link NETLIB
$    exit
$ ENDIF
$!
$ ON CONTROL_Y THEN GOTO CLEANUP
$ ON ERROR THEN GOTO CLEANUP
$!
$ IF f$trnlnm("VAXCMSG") .eqs. "DECC$MSG"
$ THEN
$    optfile := "''P1'_DECC" ! DECC/Alpha
$ ELSE
$    optfile := "''P1'_VAXC" ! VAXC
$ ENDIF
$!
$ vgl = f$verify(1)
$ link gopher.obj,manager.obj,globals.obj,ourutils.obj,cso.obj,html.obj,-
       html2.obj,CURcurses.obj,hymoo.obj,gopherrc.obj,download.obj,-
       [-.object]libgopher/lib,-
       sys$library:vaxccurse/lib,-
       sys$disk:[.vmsopts]'optfile'/opt
$ vgl = 'f$verify(0)'
$!
$ CLEANUP:
$    vgl = f$verify(vg)
$exit
