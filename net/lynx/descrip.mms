!	Make LYNX hypertext browser under VMS
!       =====================================
!
! History:
!  1/1/93  creation at KU (Lou montulli@ukanaix.cc.ukans.edu). 
!  4/12/93 (seb@lns61.tn.cornell.edu)
!           modified to support either UCX or MULTINET
!  12/13/93 (macrides@sci.wfeb.edu)
!	     Added conditional compilations for VAXC vs. DECC
!	     (dependencies not yet specified; this is just a
!	      "starter", should anyone want to do it well).
!
! Instructions:
!	Use the correct command line for your TCP/IP implementation:
!
!	$ MMS/MACRO=(MULTINET=1)		for VAXC - MultiNet
!	$ MMS/MACRO=(WIN_TCP=1)			for VAXC - Wollongong TCP/IP
!	$ MMS/MACRO=(UCX=1)			for VAXC - UCX
!	$ MMS/MACRO=(MULTINET=1,DEC_C=1)	for DECC - MultiNet
!	$ MMS/MACRO=(WIN_TCP=1,DEC_C=1)		for DECC - Wollongong TCP/IP
!	$ MMS/MACRO=(UCX=1,DEC_C=1)		for DECC - UCX
!
.IFDEF MULTINET
TCP=MULTINET
.ENDIF
.IFDEF WIN_TCP
TCP=WIN_TCP
.ENDIF
.IFDEF UCX
TCP=UCX
.ENDIF

.IFDEF TCP
.ELSE
TCP=MULTINET	!Default to MultiNet
.ENDIF

lynx.exe : sources

sources :
	SET DEFAULT [.src]
.IFDEF DEC_C
	mms /MACRO=($(TCP)=1,DEC_C=1)
.ELSE
	mms /MACRO=($(TCP)=1)
.ENDIF
	SET DEFAULT [-]

!lynx.hlp : lynx.rnh
!        RUNOFF lynx.rnh
