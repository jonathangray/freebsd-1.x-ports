#
#       Makefile for the idl distribution
#
#       @(#)Makefile	2.2 93/03/08 00:57:56
#
PUB      = /users/ftp/pub/WWW/SML/idl

#
#       Make the idl processor
#
SOURCE   = idl.sml idl-0.93.sml
SMLLIB   = lib/list.sig.sml     lib/list.sml            \
	   lib/streamio.sig.sml lib/streamio.sml        \
	   lib/Import.sml

idl:    $(SOURCE) $(SMLLIB);  echo 'use "idl.sml"; save "/tmp/idl";' | sml


#
#       Make the idl distribution
#
#
#               Define Subdirectories and read the makefiles
#
IDLLIBDIR = ./Xidl
EGDIR   = ./Xamples


ETC     = Makefile idl.doc triv.idl triv.sml triv.c README RELEASE
SOURCES = $(SOURCE) $(SMLLIB) $(EXAMPLES)

dist:   $(SOURCES) $(ETC) ;
	(cd $(IDLLIBDIR); make DISTRIB)
	(cd $(EGDIR);     make DISTRIB)
	tar -cvf -                              \
		$(SOURCES)                      \
		$(ETC)                          \
		$(IDLLIBDIR)/*.c                \
		$(IDLLIBDIR)/*.h                \
		$(IDLLIBDIR)/[Mm]akefile        \
		$(IDLLIBDIR)/*.sml              \
		$(EGDIR)/*.idl                  \
		$(EGDIR)/*.run.sml              \
		$(EGDIR)/[Mm]akefile            \
		| compress > /tmp/idl.tar.Z
	mv /tmp/idl.tar.Z $(PUB)
	cp README         $(PUB)

#
#       Make the idl library
#
libidl:  ; (cd $(XIDL); $(MAKE))
#
#
#
