I = ../include
INCLS = $(I)/alloc.h $(I)/case.h $(I)/config.h $(I)/dbz.h $(I)/fgetmfs.h \
$(I)/fgetfln.h \
$(I)/fixerrno.h $(I)/hash.h $(I)/hdbm.h $(I)/libc.h $(I)/msgs.h $(I)/news.h \
$(I)/ngmatch.h $(I)/trbatch.h

all:	$(INCLS)

$(I)/alloc.h:	alloc.h
	cp alloc.h $@
$(I)/case.h:	case.h
	cp case.h $@
$(I)/config.h:	config.h
	cp config.h $@
$(I)/dbz.h:	dbz.h
	cp dbz.h $@
$(I)/fgetmfs.h:	fgetmfs.h
	cp fgetmfs.h $@
$(I)/fgetfln.h:	fgetfln.h
	cp fgetfln.h $@
$(I)/fixerrno.h:	fixerrno.h
	cp fixerrno.h $@
$(I)/hash.h: hash.h
	cp hash.h $@
$(I)/hdbm.h: hdbm.h
	cp hdbm.h $@
$(I)/libc.h:	libc.h
	cp libc.h $@
$(I)/msgs.h: msgs.h
	cp msgs.h $@
$(I)/news.h:	news.h newshsed
	sed -f newshsed news.h >$@
$(I)/ngmatch.h: ngmatch.h
	cp ngmatch.h $@
$(I)/trbatch.h: trbatch.h
	cp trbatch.h $@

clean:
	rm -f newshsed
