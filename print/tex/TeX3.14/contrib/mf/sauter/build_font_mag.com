$! This command file is used by BUILD_FONT to build a Computer Modern
$! font file at a single magnification.  It causes the file SAUTER.MF
$! to be prefixed to every font build.  This file defines the LNOthree
$! mode and redefines BYE so as to exactly reproduce the distributed
$! .TFM files.  If you are not using the LN03 you will want to modify
$! this file to add your own output devices, and modify the mode= below.
$!
$! There are sections commented out that would produce a .PL file and
$! a printed representation of the GFT file.  You may find these
$! useful if you are interested in examining the Computer Modern fonts
$! in detail.
$!
$! The main result of executing the commands written by this file
$! is a .TFM and a PXL file for a particular Computer Modern font
$! at a particular magnification.
$!
$! p1 = font file name, p2 = magnification * 1500, p3 = mag * resolution
$! p4 = magnification
$!
$ write w "$ run mf$exe:mf"
$ write w "\input sauter; mode=LNOthree; nodisplays; "
$ write w " mag := ", "''p4'", "; input ", "''p1'"
$ if "''p4'" .eqs. "1.0" then write w "$ rename sauter.tfm ", -
          "''p1'", ".tfm/log"
$ if "''p4'" .nes. "1.0" then write w "$ delete sauter.tfm.*/log"
$ write w "$ rename sauter.lis ", "''p1'", ".", "''p3'", "lis/log"
$ write w "$ rename sauter.","''p3'","gf ","''p1'",".","''p3'","gf/log"
$ write w "$ run mf$exe:gftopxl"
$ write w "''p1'", ".", "''p3'", "gf"
$ write w "''p1'", ".", "''p2'", "pxl"
$ write w "$convert/fdl=tex$:forpxl ", "''p1'", ".", "''p2'", "pxl ", -
          "''p1'", ".", "''p2'", "pxl"
$ write w "$ run tex$:newffc"
$ write w "rpxl ","''p1'",".","''p2'","pxl"
$ write w "wstr ","''p1'",".","''p2'","pxl"
$ write w "exit"
$ write w "$! run mf$exe:gftype"
$ write w "$!''p1'", ".", "''p3'", "gf"
$ write w "$!''p1'", ".", "''p3'", "gft"
$ write w "$!n"
$ write w "$!y"
$ write w "$! run tex$:tftopl"
$ write w "$!''p1'", ".tfm"
$ write w "$!''p1'", ".pl"
$ write w "$ purge ", "''p1'", ".tfm/log"
$ write w "$ delete ", "''p1'", ".", "''p3'", "gf.*/log"
$ write w "$! purge ", "''p1'", ".", "''p3'", "gf/log"
$ write w "$! delete ", "''p1'", ".", "''p2'", "pxl.*/log"
$ write w "$ purge ", "''p1'", ".", "''p2'", "pxl/log"
$ write w "$! purge ", "''p1'", ".", "''p3'", "gft/log"
$ write w "$! purge ", "''p1'", ".pl/log"
$ write w "$ purge ", "''p1'", ".", "''p3'", "lis/log"

