$! This command file is used by BUILD_VAR_FONT to build a Computer Modern
$! font family at a single point size and magnification.  It causes the 
$! file SAUTER.MF to be prefixed to every font build.  This file defines 
$! the LNOthree mode and redefines BYE so as to exactly reproduce the 
$! distributed .TFM files.  If you are not using the LN03 you will want 
$! to modify this file to add your own output devices, and modify the 
$! mode= below.
$!
$! There are sections commented out that would produce a .PL file and
$! a printed representation of the GF file.  You may find these
$! useful if you are interested in examining the Computer Modern fonts
$! in detail.
$!
$! The main result of executing the commands written by this file
$! is a .TFM and a PXL file for a particular Computer Modern font
$! family at a particular point size and magnification.
$!
$! p1 = font name (e.g., CMR10), p2 = family name (e.g., CMR),
$! p3 = design size in points (e.g, 10),
$! p4 = magnification * 1500, p5 = mag * resolution, p6 = magnification
$!
$ open x build_font_'p1'_'p5'.mf /write
$ write x " input sauter; mode=LNOthree; nodisplays; "
$ write x " design_size := ", "''p3'", ";"
$ write x " mag := ","''p6'", ";"
$ write x " input build_", "''p2'", ";"
$ close x
$ write w "$ run mf$exe:mf"
$ write w "\input build_font_", "''p1'", "_", "''p5'", ";"
$ if "''p6'" .eqs. "1.0" then write w "$ rename build_font_", "''p1'", "_", -
          "''p5'", ".tfm ", "''p1'", ".tfm/log"
$ if "''p6'" .nes. "1.0" then write w "$ delete build_font_", "''p1'", "_", -
          "''p5'", ".tfm.*/log"
$ write w "$ rename build_font_", "''p1'", "_", "''p5'", ".", "''p5'", "gf", -
          " ", "''p1'", ".", "''p5'", "gf/log"
$ write w "$ rename build_font_", "''p1'", "_", "''p5'", ".", "lis", -
          " ", "''p1'", ".", "''p5'", "lis/log"
$ write w "$ delete build_font_", "''p1'", "_", "''p5'", ".mf.*/log"
$ write w "$! run mf$exe:gftype"
$ write w "$!''p1'",".", "''p5'", "gf"
$ write w "$!''p1'",".", "''p5'", "gft"
$ write w "$!n"
$ write w "$!y"
$ if "''p6'" .nes. "1.0" then goto nopl1
$ write w "$! run tex$:tftopl"
$ write w "$!''p1'", ".tfm"
$ write w "$!''p1'", ".pl"
$nopl1:
$ write w "$ run mf$exe:gftopxl"
$ write w "''p1'",".", "''p5'", "gf"
$ write w "''p1'", ".", "''p4'", "pxl"
$ write w "$convert/fdl=tex$:forpxl ", "''p1'", ".", "''p4'", "pxl ", -
          "''p1'", ".", "''p4'", "pxl"
$ write w "$ run tex$:newffc"
$ write w "rpxl ","''p1'",".","''p4'","pxl"
$ write w "wstr ","''p1'",".","''p4'","pxl"
$ write w "exit"
$ write w "$! purge ", "''p1'", ".", "''p5'", "gf/log"
$ write w "$ delete ", "''p1'", ".", "''p5'", "gf.*/log"
$ write w "$! purge ", "''p1'", ".", "''p5'", "gft/log"
$ write w "$ purge ", "''p1'", ".", "''p4'", "pxl/log"
$ if "''p6'" .nes. "1.0" then goto nopl2
$ write w "$!purge ", "''p1'", ".pl/log"
$ write w "$ purge ", "''p1'", ".tfm/log"
$nopl2:
$ write w "$ purge "''p1'", ".", "''p5'", "lis/log"
