$! See imakefile. for information about options				      !
$ HSCORE_FILE = F$TRNLNM("SYS$DISK") + F$DIRECTORY() + "xjewel.scores"
$ defs:=DECWM,ICON_WINDOW,""""HSCORE_FILE=""""""'HSCORE_FILE'"""""" """"
$ write sys$output "HIGH SCORE FILE IS:",HSCORE_FILE
$ write sys$output "BUILDING JEWEL..."
$ write sys$output " game"
$ cc GAME.C /define=('defs')
$ write sys$output " help"
$ cc HELP.C /define=('defs')
$ write sys$output " hscore"
$ cc HSCORE.C /define=('defs')
$ write sys$output " intro"
$ cc INTRO.C /define=('defs')
$ write sys$output " jewel"
$ cc JEWEL.C /define=('defs')
$ write sys$output " logic"
$ cc LOGIC.C /define=('defs')
$ write sys$output " panel"
$ cc PANEL.C /define=('defs')
$ write sys$output " vmsstubs"
$ cc VMSSTUBS.C /define=('defs')
$ write sys$output " xhscore"
$ cc XHSCORE.C /define=('defs')
$ write sys$output " xw"
$ cc XW.C /define=('defs')
$ write sys$output "LINKING..."
$ link /EXEC=XJEWEL JEWEL.OBJ,GAME.OBJ,HELP.OBJ,HSCORE.OBJ,INTRO.OBJ, -
LOGIC.OBJ,PANEL.OBJ,VMSSTUBS.OBJ,XHSCORE.OBJ,XW.OBJ, SYS$INPUT/opt
SYS$LIBRARY:DECW$XLIBSHR.EXE/SHARE
SYS$LIBRARY:VAXCRTL/LIBR
$ xjewel := run xjewel.exe
$ write sys$output "FONTS..."
$ set def [.bitmaps]
$ font seven_seg.bdf
$ run sys$system:decw$mkfontdir
$ set def [.-]
$ fini:
$ write sys$output "HELPFILE..."
$ helpfile = F$TRNLNM("SYS$DISK") + F$DIRECTORY() + "xjewel.hlb"
$ if (F$SEARCH(helpfile) .nes. "") then  delete /nolog/nocon 'helpfile';*
$ lib/help/create/replace 'helpfile' xjewel.help 
$ define/nolog HLP$LIBRARY 'helpfile'
$ purge
$ purge [.bitmaps]
$ if (F$SEARCH(HSCORE_FILE) .nes "") then delete /nolog/nocon 'HSCORE_FILE';*
$! xjewel will create a new score file with open permissions
