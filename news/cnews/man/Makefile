check:	cmp
cmp:
	-for f in *.1 ; do echo --- $$f --- ; diff /usr/man/man1 $$f ; done
	-for f in *.5 ; do echo --- $$f --- ; diff /usr/man/man5 $$f ; done
	-for f in *.8 ; do echo --- $$f --- ; diff /usr/man/man8 $$f ; done
