$ v = 'f$verify(0)'
$!			LIBMAKE.COM
$!
$!   Command file to build the WWWLibrary on VMS systems.
$!
$!   13=Dec-1993	F.Macrides		macrides@sci.wfeb.edu
$!	Mods for conditional compilations with VAXC versus DECC
$!   10-Dec-1993	F.Macrides		macrides@sci.wfeb.edu
$!	Initial version, for WWWLibrary v2.14 with Lynx v2.1
$!
$ ON CONTROL_Y THEN GOTO CLEANUP
$ ON ERROR THEN GOTO CLEANUP
$ agent = 0
$ IF P1 .EQS. ""
$ THEN
$ 	write sys$output "Acceptable TCP/IP agents are"
$ 	write sys$output " [1] MultiNet (default)"
$ 	write sys$output " [2] UCX"
$ 	write sys$output " [3] WIN_TCP"
$ 	write sys$output " [4] DECNET"
$ 	read sys$command/prompt="Agent [1,2,3,4] (RETURN = [1]) " agent
$ ENDIF
$ if agent .eq. 1 .or. agent .eqs. "" .or. p1 .eqs. "MULTINET" then -
    transport = "MULTINET"
$ if agent .eq. 2 .or. p1 .eqs. "UCX" then transport = "UCX"
$ if agent .eq. 3 .or. p1 .eqs. "WIN_TCP" then transport = "WIN_TCP"
$ if agent .eq. 4 .or. p1 .eqs. "DECNET" then transport = "DECNET"
$!
$ IF f$trnlnm("VAXCMSG") .eqs. "DECC$MSG"
$ THEN
$  v1 = f$verify(1)
$! DECC:
$  v1 = 'f$verify(0)'
$  If transport .eqs. "UCX"
$  Then
$  v1 = f$verify(1)
$!
$ cc/stand=vaxc/prefix=all-
    /DEFINE=(DEBUG,ACCESS_AUTH,'transport',VC="""2.14""")-
    /INCLUDE=([-.Implementation]) -
    [-.Implementation]HTString.c
$!
$ cc := cc/stand=vaxc/prefix=all-
	  /DEFINE=(DEBUG,ACCESS_AUTH,'transport')-
	  /INCLUDE=([-.Implementation])
$!
$  v1 = 'f$verify(0)'
$  Else
$  v1 = f$verify(1)
$!
$ cc/stand=vaxc/prefix=ansi-
    /DEFINE=(DEBUG,ACCESS_AUTH,'transport',VC="""2.14""")-
    /INCLUDE=([-.Implementation]) -
    [-.Implementation]HTString.c
$!
$ cc := cc/stand=vaxc/prefix=ansi-
	  /DEFINE=(DEBUG,ACCESS_AUTH,'transport')-
	  /INCLUDE=([-.Implementation])
$!
$  v1 = 'f$verify(0)'
$  EndIf
$ ELSE
$  v1 = f$verify(1)
$! VAXC:
$!
$ cc/DEFINE=(DEBUG,ACCESS_AUTH,'transport',VC="""2.14""")-
    /INCLUDE=([-.Implementation]) -
    [-.Implementation]HTString.c
$!
$ cc := cc/DEFINE=(DEBUG,ACCESS_AUTH,'transport')-
	  /INCLUDE=([-.Implementation])
$!
$ v1 = 'f$verify(0)'
$ ENDIF
$ v1 = f$verify(1)
$ cc [-.Implementation]HTParse.c
$ cc [-.Implementation]HTAccess.c
$ cc [-.Implementation]HTTP.c
$ cc [-.Implementation]HTFile.c
$ cc [-.Implementation]HTBTree.c
$ cc [-.Implementation]HTFTP.c
$ cc [-.Implementation]HTTCP.c
$ cc [-.Implementation]SGML.c
$ cc [-.Implementation]HTML.c
$ cc [-.Implementation]HTMLDTD.c
$ cc [-.Implementation]HTChunk.c
$ cc [-.Implementation]HTPlain.c
$ cc [-.Implementation]HTWriter.c
$ cc [-.Implementation]HTFWriter.c
$ cc [-.Implementation]HTMLGen.c
$ cc [-.Implementation]HTAtom.c
$ cc [-.Implementation]HTAnchor.c
$ cc [-.Implementation]HTStyle.c
$ cc [-.Implementation]HTList.c
$ cc [-.Implementation]HTAlert.c
$ cc [-.Implementation]HTRules.c
$ cc [-.Implementation]HTFormat.c
$ cc [-.Implementation]HTInit.c
$ cc [-.Implementation]HTMIME.c
$ cc [-.Implementation]HTHistory.c
$ cc [-.Implementation]HTNews.c
$ cc [-.Implementation]HTGopher.c
$ cc [-.Implementation]HTTelnet.c
$ cc [-.Implementation]HTWSRC.c
$ cc [-.Implementation]HTAAUtil.c
$ cc [-.Implementation]HTAABrow.c
$ cc [-.Implementation]HTAAServ.c
$ cc [-.Implementation]HTAAFile.c
$ cc [-.Implementation]HTPasswd.c
$ cc [-.Implementation]HTGroup.c
$ cc [-.Implementation]HTACL.c
$ cc [-.Implementation]HTAuth.c
$ cc [-.Implementation]HTAAProt.c
$ cc [-.Implementation]HTAssoc.c
$ cc [-.Implementation]HTLex.c
$ cc [-.Implementation]HTUU.c
$ cc [-.Implementation]HTVMSUtils.c
$ cc [-.Implementation]getpass.c
$ cc [-.Implementation]getline.c
$ cc [-.Implementation]crypt.c
$ cc [-.Implementation]crypt_util.c
$ v1 = 'f$verify(0)'
$!
$! I might use this again someday? -- LM
$!
$!cc [-.Implementation]HTFIREWALLGATE.c
$!    
$ v1 = f$verify(1)
$!
$! Need port to VMS of freeWAIS library to use HTWAIS.c
$! In the meantime, WAIS databases can be accessed via the gateway.
$!
$!cc [-.Implementation]HTWAIS.c
$!    
$ If f$search("[-.Implementation]WWWLib_''transport'.olb") .eqs. "" Then -
    LIBRARY/Create [-.Implementation]WWWLib_'transport'.olb
$ LIBRARY/Replace [-.Implementation]WWWLib_'transport'.olb *.obj
$!
$ v1 = 'f$verify(0)'
$ CLEANUP:
$    v1 = f$verify(v)
$exit
