define redo
symbol-file dvips
exec-file dvips
end

define aredo
symbol-file afm2tfm
exec-file afm2tfm
end

#set env TEXCONFIG /tmp
#set args table -o /dev/null

#set env PKFONTS /envvar/pk:
#set env DVIPSHEADERS /envvar/headers:
#set env TEXCONFIG /usr/local/lib/tex/dvips
#set args $tdvi/test/story -o /dev/null
set args $tdvi/test/accenttest -o /dev/null

#set args -D 300 $tdvi/test/mag11 -o /dev/null

#set env TEXFONTS .:/w/norm/deep1/mac.archive.um//:/w/norm/deep2/fonts//
#set args -M /w/norm/deep1/mac.archive.um/testfonts.dvi
