$! This command file is used by BUILD_ALL to build all magnifications
$! of a particular Computer Modern font.  All of the standard magnifications
$! are built.  A non-standard magnification is commented out as
$! an illustration.
$! P1 = font file name
$ open w build_'p1'_temp.com /write
$@build_font_mag 'p1' 1500 300 1.0   ! magstep0
$@build_font_mag 'p1' 1642 329 1.095 ! magstephalf
$@build_font_mag 'p1' 1800 360 1.2   ! magstep1
$@build_font_mag 'p1' 2160 432 1.44  ! magstep2
$@build_font_mag 'p1' 2592 518 1.728 ! magstep3
$@build_font_mag 'p1' 3111 622 2.074 ! magstep4
$@build_font_mag 'p1' 3732 746 2.488 ! magstep5
$!@build_font_mag 'p1' 6000 1200 4    ! highly magnified
$ close w
$@build_'p1'_temp.com
$ delete/log build_'p1'_temp.com.*
