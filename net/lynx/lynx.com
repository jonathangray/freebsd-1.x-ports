! set up lynx as a command so that it will accept command line arguments
! fill in your location where it says "sys$commn:[syspubl]"
$ lynx:==$sys$common:[syspubl]lynx.exe
!
! fill in another gateway if you wish
!define "WWW_wais_GATEWAY" "http://info.cern.ch:8001/"
define "WWW_wais_GATEWAY" "http://www.ncsa.uiuc.edu:8001/"
!
! fill in your NNTP news server here
define "NNTPSERVER" "news"
