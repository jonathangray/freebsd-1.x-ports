# This file makes TeX in its various incarnations.
#
# The file ../common/texmf.h uses the symbol `TeX' to define certain
# things that are different for TeX and Metafont, hence the addition to
# CFLAGS.

SITEDIR=..
CC=gcc
OPT=-g
CFLAGS=$(OPT) -I$(SITEDIR) -DTeX
LDFLAGS=
LOADLIBES=-lcposix
SHELL=/bin/sh
LN=ln

# The formats we know how to make.
# 
formats=amstex.fmt etex.fmt latex.fmt slitex.fmt tex.fmt \
texinfo.fmt picplus.fmt


makeargs=SITEDIR="$(SITEDIR)" CC="$(CC)" OPT="$(OPT)" \
  LDFLAGS="$(LDFLAGS)" LOADLIBES="$(LOADLIBES)"

.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
		$(CC) $(CFLAGS) -c $*.c

# Routines used everywhere.
# 
commondefines=../common/common.defines ../common/texmf.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o

# Routines used in both TeX and Metafont.
# 
extrac=../common/texmf.c
extrah=../common/texmf.h ../common/endian.h
extrao=texmf.o

# Routines used in TeX, Metafont, and BibTeX.
# 
fileioc=../common/fileio.c
fileioh=../common/fileio.h
fileioo=fileio.o

# We don't add `$(commono)' here, since it doesn't depend on anything in
# this directory, and we write a dependency for `$(objs)' below.
# 
objs=$(fileioo) tex0.o tex1.o tex2.o tex3.o tex4.o tex5.o tex6.o tex7.o tex8.o
iobjs=iextra.o initex.o $(objs)
vobjs=tex-extra.o itex.o $(objs)


default:	all

all:		initex virtex
triptrap:	triptex

initex:		stamp-convert $(iobjs) $(commono)
		$(CC) -o initex $(LDFLAGS) $(iobjs) $(commono) $(LOADLIBES)

virtex:		stamp-convert $(vobjs) $(commono)
		$(CC) -o virtex $(LDFLAGS) $(vobjs) $(commono) $(LOADLIBES)

run-trip:
		rm -f trip.tex
		-$(LN) TeXtrip/trip.tex .
		../texware/pltotf TeXtrip/trip.pl trip.tfm
		-$(SHELL) -c '. ./tripenv; \
                  ./triptex < TeXtrip/trip1.in > /dev/null 2>&1'
		-diff TeXtrip/tripin.log trip.log
		-$(SHELL) -c '. ./tripenv; \
                  ./triptex < TeXtrip/trip2.in > trip.fot'
		-diff TeXtrip/trip.log trip.log
		-diff TeXtrip/trip.fot trip.fot
		$(SHELL) -c '. ./tripenv; \
		  ../texware/dvitype trip.dvi < TeXtrip/dvitype.in > trip.typ'
		-diff TeXtrip/trip.typ trip.typ


# Make format files automatically.
# 
fmts: $(formats)

amstex.fmt: tex.fmt
	./initex \&./tex amstex \\dump

etex.fmt: tex.fmt
	touch eplain.aux # Makes cross-reference warnings work right.
	./initex \&./tex eplain \\dump
	mv eplain.fmt etex.fmt
	mv eplain.log etex.log

# Until LaTeX and SLiTeX are updated, we must use a hacked version, that
# sets the new parameters to reasonable values.
# 
latex.fmt:
	./initex lplain \\dump
	mv lplain.fmt latex.fmt
	mv lplain.log latex.log

picplus.fmt: tex.fmt
	./initex \&./tex picplus \\dump

slitex.fmt:
	./initex splain \\dump
	mv splain.fmt slitex.fmt
	mv splain.log slitex.log

tex.fmt:
	./initex plain \\dump
	mv plain.fmt tex.fmt
	mv plain.log tex.log

# texinfo changes the escape character from `\' to `@'.
# 
texinfo.fmt: tex.fmt
	./initex \&./tex texinfo @dump


# The `ini_to_trip' script changes texd.h, instead of the change file, so
# retangling is not necessary.  After changing constants, making a
# triptex means making an initex.
# 
triptex:	stamp-trip $(iobjs) $(commono)
		make $(makeargs) initex
		mv initex triptex

stamp-trip:	stamp-convert
		rm -f *.o
		$(SHELL) ./ini_to_trip texd.h
		touch stamp-trip

# tangle produces tex.p and tex.pool.
# 
tex.p:		tex.web ctex.ch
		../web/tangle tex.web ctex.ch

# The convert script produces tex[0-8].c, itex.c, coerce.h, and texd.h.
# The (hand-coded) memory.h is included in one of tex?.c files; it isn't
# really necessary to `convert' again if it changes, only to recompile
# those files, but describing that relationship is ugly to do.
# (Furthermore, memory.h is stable.)
# 
stamp-convert:	tex.p memory.h fixcoerce.h $(commondefines)
		$(SHELL) ./convert
		touch stamp-convert

# The (hand-coded) file $(extrac) and the (generated) file itex.c have
# #ifdefs for INITEX, so we compile them differently.
# 
iextra.o:	$(extrac) texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -I. -DINITEX -DINI -c $(extrac)
		mv $(extrao) iextra.o

initex.o:       itex.c texd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -DINITEX -c itex.c && mv itex.o initex.o

$(objs):	stamp-convert texd.h
tex-extra.o:	$(extrac) texd.h
		rm -f tex-extra.c
		$(LN) $(extrac) tex-extra.c
		$(CC) $(CFLAGS) -I. -c tex-extra.c

$(fileioo):	$(fileioc) $(fileioh)
		$(CC) $(CFLAGS) -I. -c $(fileioc)

tex0.o:		tex0.c
tex1.o:		tex1.c
tex2.o:		tex2.c
tex3.o:		tex3.c
tex4.o:		tex4.c
tex5.o:		tex5.c
tex6.o:		tex6.c
tex7.o:		tex7.c
tex8.o:		tex8.c


clean:
		rm -f stamp-* *.o triptex initex virtex
		rm -f texd.h coerce.h *.log *.fmt *.aux
		rm -f 8terminal.tex trip.* tripos.tex dvitype.out
		rm -f tex-extra.c

veryclean:	clean
		rm -f tex?.c itex.c tex.p tex.pool texput.*
		rm -f \#*\# *~ *.bak *.ckp core
