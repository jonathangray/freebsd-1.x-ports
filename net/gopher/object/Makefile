include ../Makefile.config

CCFLAGS	= $(OPT) $(COMPAT) $(GSYSTYPE) $(DEBUGGING) -I. 

OBJS	= STRstring.o GDgopherdir.o GSgopherobj.o \
	  DAarray.o   VIews.o       BLblock.o \
	  Sockets.o   util.o        compatible.o    Debug.o


TARGET	= libgopher.a libgopher.so.0.1

all: shared $(TARGET)

#
# Make shared library directories
#
shared:
	@-if [ -f /lib/ld.so -a "$(CC)" = "cc" ]; then \
		mkdir shared-sun; \
	fi
	touch shared


.c.o: $(OBJS)
	$(CC) $(CCFLAGS) $(PIC) -c $<
	@-if [ -d shared-sun ]; then \
		echo "Building shared version of $@"; \
		$(CC) $(CCFLAGS) -pic -o shared-sun/$@ -c $<; \
	fi

#
# Stuff for shared libraries
#

libgopher.so.0.1: $(OBJS)
	@-if [ -d shared-sun ]; then \
		echo "Building shared library $@"; \
		cd shared-sun; \
		rm -f $@; \
		ld -o $@ -assert pure-text $(OBJS);\
		mv $@ ..; \
	fi

libgopher.sa.0.1: $(OBJS)
	;


libgopher.a: $(OBJS)
	rm -f libgopher.a
	ar rcv libgopher.a $(OBJS)
	$(RANLIB) libgopher.a

clean:
	-rm -f $(TARGET) $(OBJS) *.out *~ core TAGS \
	       *.sa.* shared
	-rm -rf shared-sun

#STRstring.o: 	STRstring.h
#GDgopherdir.o: 	GDgopherdir.h
#GSgopherobj.o:	GSgopherobj.h

#
#install shared libraries if need be..
#
install:
	@-if [ -f *.so.* ]; then \
	  echo "Installing Sun shared gopher library" ;\
	  $(INSTALL) libgopher.so.0.1 $(CLIENTLIB);\
	  $(INSTALL) libgopher.a $(CLIENTLIB); \
	fi
