
SHELL	= /bin/sh

CFLAGS	= -O

##  Where the readdir() and friends are, if not in your C library.
#DIRLIB	= -lndir

##  Use these two lines if you use ranlib...
AR_OBJ	= $(LIB_OBJ)
RANLIB	= ranlib lib.a
##  ...or use these two if you need tsort instead...
#AR_OBJ	= `lorder $(LIB_OBJ) | tsort`
#RANLIB	= @echo
##  ...or these two if you need neither.
#AR_OBJ	= $(LIB_OBJ)
#RANLIB	= @echo

##  Where executables should be put.
#DESTDIR	= /usr/local/bin
DESTDIR	= /usr/local/bin

##  The "foo" manpage will get put in $(MANDIR)/foo.$1
#MANDIR	= /usr/man/man1
#1	= 1
#MANDIR	= /usr/man/u_man/manl
#1	= 1L
MANDIR	= /usr/local/man/man1
1	= 1

##
##  END OF CONFIGURATION SECTION
##

##  Header files.
HDRS	= shar.h config.h
LIB	= lib.a

##  Programs and documentation.
PROGRAMS= unshar
MANPAGES= unshar.$1

##  Our library of utility functions.
LIB_SRC	= glue.c parser.c lcwd.c lexec.c lfiles.c lhost.c llib.c lmem.c luser.c
LIB_OBJ	= glue.o parser.o lcwd.o lexec.o lfiles.o lhost.o llib.o lmem.o luser.o


all:		$(PROGRAMS) $(MANPAGES)
	touch all

##  You might want to change these actions...
install:	all
	install -c -s -m 755 $(PROGRAMS) $(DESTDIR)
	install -c $(MANPAGES) $(MANDIR)

clean:
	rm -f *.[oa] *.$1 *.BAK $(PROGRAMS)
	rm -f lint lib.ln tags core foo a.out Part?? all install


##  Creating manpages.
.SUFFIXES:	.$1 .man
.man.$1:
	@rm -f $@
	cp $< $@
	chmod 444 $@


##  Programs.
unshar:		unshar.o $(HDRS) $(LIB)
	@rm -f $@
	$(CC) $(CFLAGS) -o unshar unshar.o $(LIB) $(DIRLIB)


##  Support your local library.
lib.a:		$(LIB_OBJ)
	@rm -f $@
	ar r lib.a $(AR_OBJ)
	$(RANLIB)
$(LIB_OBJ):	$(HDRS)
