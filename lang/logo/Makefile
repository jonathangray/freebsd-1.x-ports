INSTDIR   = /usr/local

CC	  = cc
INCLUDES  = -I/usr/X386/include
LIBS      = -L/usr/X386/lib -ltermcap -lX11 -lm
DEFINES   =
CCOPTIONS = -O3

LDOPTIONS = $(LIBS)
CFLAGS	  = $(CCOPTIONS) $(DEFINES) $(INCLUDES)

OBJS 	  = coms.o error.o eval.o files.o graphics.o init.o intern.o \
	    libloc.o lists.o logodata.o main.o math.o mem.o paren.o parse.o \
	    print.o term.o wrksp.o xgraphics.o

SRCS	  = coms.c error.c eval.c files.c graphics.c init.c intern.c \
	    libloc.c lists.c logodata.c main.c math.c mem.c paren.c parse.c \
	    print.c term.c wrksp.c xgraphics.c   

HDRS	  = globals.h logo.h xgraphics.h

logo:	$(OBJS) logolib
	$(CC) -o logo $(OBJS) $(LDOPTIONS)

xgraphics.o:	xgraphics.h
graphics.o:	xgraphics.h

tags:$(SRCS)
	ctags -t $(SRCS) $(HDRS)

# Use this to run in-place
#libloc.c:
#	echo 'char *libloc="'`pwd`'/logolib";' > libloc.c

# Use this to assume an installation.
libloc.c:
	echo 'char *libloc="$(INSTDIR)/lib/logolib";' > libloc.c

logolib:
	chmod +x makelib
	./makelib

package:	install pkg/+PLIST pkg/+DESC pkg/+ONELINE
	pkg_create -f pkg/+PLIST -d pkg/+DESC -c pkg/+ONELINE logo_bin

install: logo
	install -c logo $(INSTDIR)/bin
	-@mkdir -p $(INSTDIR)/lib
	cp -r logolib $(INSTDIR)/lib

clean:
	rm -rf logo *.o libloc.c
