$! This command file is used by BUILD_ALL_CM_FONTS to build all magnifications
$! of a particular Computer Modern font family at a particular point size.  
$! All of the standard magnifications are built.  A non-standard magnification 
$! is commented out as an illustration.
$!
$! p1 = font name (e.g., CMR10), p2 = family name (e.g., CMR),
$! p3 = design size in points (e.g, 10),
$!
$ open w 'p1'_font_build.com /write
$@build_var_font_mag 'p1' 'p2' 'p3' 1500 300 1.0   ! magstep0
$@build_var_font_mag 'p1' 'p2' 'p3' 1642 329 1.095 ! magstephalf
$@build_var_font_mag 'p1' 'p2' 'p3' 1800 360 1.2   ! magstep1
$@build_var_font_mag 'p1' 'p2' 'p3' 2160 432 1.44  ! magstep2
$@build_var_font_mag 'p1' 'p2' 'p3' 2592 518 1.728 ! magstep3
$@build_var_font_mag 'p1' 'p2' 'p3' 3111 622 2.074 ! magstep4
$@build_var_font_mag 'p1' 'p2' 'p3' 3732 746 2.488 ! magstep5
$!@build_var_font_mag 'p1' 'p2' 'p3' 6000 1200 4    ! highly magnified
$ close w
$@'p1'_font_build.com
$ delete 'p1'_font_build.com.*/log

