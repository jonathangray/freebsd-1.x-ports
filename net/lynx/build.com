$ v = 'f$verify(0)'
$!			BUILD.COM
$!
$!   Command file to build LYNX.EXE on VMS systems.
$!   Also invokes build of the WWWLibrary if its
$!    object library does not already exist.
$!
$!   13=Dec-1993	F.Macrides		macrides@sci.wfeb.edu
$!	Mods for conditional compilations with VAXC versus DECC
$!   10-Dec-1993	F.Macrides		macrides@sci.wfeb.edu
$!	Initial version, for Lynx v2.1
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
$ 	read sys$command/prompt="Agent [1,2,3] (RETURN = [1]) " agent
$ ENDIF
$ if agent .eq. 1 .or. agent .eqs. "" .or. p1 .eqs. "MULTINET" then -
    option = "MULTINET"
$ if agent .eq. 2 .or. p1 .eqs. "UCX" then option = "UCX"
$ if agent .eq. 3 .or. p1 .eqs. "WIN_TCP" then option = "WIN_TCP"
$!
$ IF f$search("[.WWW.Library.Implementation]WWWLib_''option'.olb") .eqs. ""
$ THEN
$ v1 = f$verify(1)
$!
$!	Build the WWWLibrary
$!
$ set default [.WWW.Library.VMS]
$ v1 = 'f$verify(0)'
$ @libmake 'option'
$ v1 = f$verify(1)
$ set default [-.-.-]
$ v1 = 'f$verify(0)'
$ ENDIF
$ v1 = f$verify(1)
$!
$!	Compile the Lynx [.SRC] modules
$!
$ set default [.SRC]
$ v1 = 'f$verify(0)'
$ IF f$trnlnm("VAXCMSG") .eqs. "DECC$MSG"
$ THEN
$  compiler := "DECC"
$  v1 = f$verify(1)
$! DECC:
$  v1 = 'f$verify(0)'
$  If option .eqs. "UCX"
$  Then
$  v1 = f$verify(1)
$  cc := cc/stand=vaxc/prefix=all-
	   /DEFINE=(DEBUG,ACCESS_AUTH,'option',__VMS_CURSES)-
	   /INCLUDE=([-],[-.WWW.Library.Implementation]) 
$  v1 = 'f$verify(0)'
$  Else
$  v1 = f$verify(1)
$  cc := cc/stand=vaxc/prefix=ansi-
	   /DEFINE=(DEBUG,ACCESS_AUTH,'option',__VMS_CURSES)-
	   /INCLUDE=([-],[-.WWW.Library.Implementation]) 
$  v1 = 'f$verify(0)'
$  EndIf
$ ELSE
$  compiler := "VAXC"
$  v1 = f$verify(1)
$! VAXC:
$  cc := cc/DEFINE=(DEBUG,ACCESS_AUTH,'option')-
	   /INCLUDE=([-],[-.WWW.Library.Implementation]) 
$  v1 = 'f$verify(0)'
$ ENDIF
$ v1 = f$verify(1)
$!
$ cc DefaultStyles
$ cc GridText
$ cc HTAlert
$ cc HTFWriter
$ cc HTInit
$ cc HTML
$ cc LYBookmark
$ cc LYClean
$ cc LYCurses
$ cc LYEdit
$ cc LYForms
$ cc LYGetFile
$ cc LYHistory
$ cc LYMail
$ cc LYMain
$ cc LYMainLoop
$ cc LYOptions
$ cc LYPrint
$ cc LYReadCFG
$ cc LYSearch
$ cc LYShowInfo
$ cc LYStrings
$ cc LYUtils
$ cc LYrcFile
$ cc LYDownload
$ cc LYNews
$ cc LYKeymap
$ cc LYUpload
$!
$!	Link the objects and libaries.
$!
$ link/exe=lynx.exe -
DefaultStyles.obj, -
GridText.obj, -
HTAlert.obj, -
HTFWriter.obj, -
HTInit.obj, -
HTML.obj, -
LYBookmark.obj, -
LYClean.obj, -
LYCurses.obj, -
LYEdit.obj, -
LYForms.obj, -
LYGetFile.obj, -
LYHistory.obj, -
LYMail.obj, -
LYMain.obj, -
LYMainLoop.obj, -
LYOptions.obj, -
LYPrint.obj, -
LYReadCFG.obj, -
LYSearch.obj, -
LYShowInfo.obj, -
LYStrings.obj, -
LYUtils.obj, -
LYrcFile.obj, -
LYDownload.obj, -
LYNews.obj, -
LYKeymap.obj, -
LYUpload.obj, -
[-.WWW.Library.Implementation]WWWLib_'option'.olb/library, -
[]'option'_'compiler'.opt/opt
$!
$!	Copy the executable to the top directory and restore the default.
$!
$ copy lynx.exe [-]
$ set def [-]
$!
$ v1 = 'f$verify(0)'
$!
$!  Translate LYNX.RNH to LYNX.HLP for inclusion in the system HELP library
$!
$! runoff lynx.rnh
$!
$ CLEANUP:
$    v1 = 'f$verify(0)'
$    write sys$output "Default directory:"
$    show default
$    v1 = f$verify(v)
$ exit
