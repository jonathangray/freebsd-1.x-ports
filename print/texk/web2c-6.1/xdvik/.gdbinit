define redo
symbol-file xdvi
exec-file xdvi
end

#set args $tdvi/test/story
#set args $tdvi/test/epsf
#set args $tdvi/test/long
#set args $tdvi/test/nosize
#set args $tdvi/test/tixflier

#set args -fg white -bg white $tdvi/test/story
#set args +maketexpk $tdvi/test/maketex
#set args $tdvi/test/mag11
set args $tdvi/test/dynafig
#set args $eplain/test/rex

#set env TEXFONTS .:/w/norm/deep1/mac.archive.um//:/w/norm/deep2/fonts//
#set args /w/norm/deep1/mac.archive.um/testfonts.dvi

