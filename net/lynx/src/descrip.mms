!       Make LYNX hypertext browser under VMS
!       =====================================
!
! History:
!  1/1/93  creation at KU (Lou montulli@ukanaix.cc.ukans.edu). 
!  4/12/93 (seb@lns61.tn.cornell.edu)
!           modified to support either UCX or MULTINET
!  12/2/93 modified to support Lynx rewrite
!  12/13/93 (macrides@sci.wfeb.edu)
!	     Added conditional compilations for VAXC vs. DECC
!	     (dependencies not yet specified; this is just a
!	      "starter", should anyone want to do it well).
!
! Instructions:
!       Use the correct command line for your TCP/IP implementation:
!
!	$ MMS/MACRO=(MULTINET=1)		for VAXC - MultiNet
!	$ MMS/MACRO=(WIN_TCP=1)			for VAXC - Wollongong TCP/IP
!	$ MMS/MACRO=(UCX=1)			for VAXC - UCX
!	$ MMS/MACRO=(MULTINET=1,DEC_C=1)	for DECC - MultiNet
!	$ MMS/MACRO=(WIN_TCP=1,DEC_C=1)		for DECC - Wollongong TCP/IP
!	$ MMS/MACRO=(UCX=1,DEC_C=1)		for DECC - UCX
!
OBJS=  LYClean.obj, LYShowInfo.obj, LYEdit.obj, LYStrings.obj, \
LYMail.obj, HTAlert.obj, GridText.obj, LYGetFile.obj, \
LYMain.obj, LYMainLoop.obj, LYCurses.obj, LYBookmark.obj, LYUtils.obj, \
LYOptions.obj, LYReadCFG.obj, LYSearch.obj, LYHistory.obj, \
LYForms.obj, LYPrint.obj, LYrcFile.obj, LYDownload.obj, LYNews.obj, \
LYKeymap.obj, LYUpload.obj, \
HTML.obj, HTFWriter.obj, HTInit.obj, DefaultStyles.obj, 

.IFDEF MULTINET
WWWLIB= [-.WWW.Library.Implementation]WWWLib_MULTINET.olb
.IFDEF DEC_C
TCPFLAGS=/stand=vaxc/prefix=ansi/def=(DEBUG,ACCESS_AUTH,MULTINET,__VMS_CURSES)
OPT=[]MULTINET_DECC.opt
.ELSE
TCPFLAGS=/define=(DEBUG,ACCESS_AUTH,MULTINET)
OPT=[]MULTINET_VAXC.opt
.ENDIF
.ENDIF
.IFDEF WIN_TCP
WWWLIB= [-.WWW.Library.Implementation]WWWLib_WIN_TCP.olb
.IFDEF DEC_C
TCPFLAGS=/stand=vaxc/prefix=ansi/def=(DEBUG,ACCESS_AUTH,WIN_TCP,__VMS_CURSES)
OPT=[]WIN_TCP_DECC.opt
.ELSE
TCPFLAGS=/define=(DEBUG,ACCESS_AUTH,WIN_TCP)
OPT=[]WIN_TCP_DECC.opt
.ENDIF
.ENDIF
.IFDEF UCX
WWWLIB= [-.WWW.Library.Implementation]WWWLib_UCX.olb
.IFDEF DEC_C
TCPFLAGS=/stand=vaxc/prefix=all/def=(DEBUG,ACCESS_AUTH,UCX,_VMS_CURSES)
OPT=[]UCX_DECC.opt
.ELSE
TCPFLAGS=/define=(DEBUG,ACCESS_AUTH,UCX)
OPT=[]UCX_VAXC.opt
.ENDIF
.ENDIF

.IFDEF TCPFLAGS
.ELSE
WWWLIB= [-.WWW.Library.Implementation]WWWLib_MULTINET.olb !Default to MultiNet
.IFDEF DEC_C
TCPFLAGS=/stand=vaxc/prefix=ansi/define=(DEBUG,ACCESS_AUTH,MULTINET,_VMS_CURSES)
OPT=[]MULTINET_DECC.opt
.ELSE
TCPFLAGS=/define=(DEBUG,ACCESS_AUTH,MULTINET)
OPT=[]MULTINET_VAXC.opt
.ENDIF
.ENDIF

CFLAGS=$(TCPFLAGS)/include=([-],[-.WWW.Library.Implementation])


lynx :   $(OBJS) !$(WWWLIB)
	link /executable=lynx.exe $(OBJS), $(WWWLIB)/lib, $(OPT)/opt
	copy lynx.exe [-]lynx.exe
