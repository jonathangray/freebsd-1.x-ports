#! /bin/sh

# @(#)imake.sh	1.1 17/9/92 (UKC)

# imake.sh

[ -f orig_makefiles.tar ] || {
	echo Saving original makefiles ...
	tar cf orig_makefiles.tar `find . -name Makefile -print`
}

echo Extracting Imakefiles ...
sh imakefiles.shar

echo Making makefiles with xmkmf ... 
xmkmf -a
