#
# @(#)Makefile	1.1
#
# Makefile for gopher filter client
#


include ../Makefile.config

OBJS	= gophfilt.o

CCFLAGS	= $(OPT) $(GSYSTYPE) $(INCLUDES)

TARGET	= gophfilt

all:	$(TARGET)

.c.o:
	$(CC) $(CCFLAGS) -c $<

../object/libgopher.a:
	(cd ..; $(MAKE) $(MFLAGS) objects)

$(TARGET) : $(OBJS) ../object/libgopher.a
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(CLIENTLIBS) 

install : all
	-mv $(CLIENTDIR)/$(TARGET) $(CLIENTDIR)/$(TARGET).old
	$(INSTALL) -c $(TARGET) $(CLIENTDIR)

clean:
	-rm -f $(TARGET) $(OBJS) *.out *~ core

$(OBJS): ../object/GSgopherobj.h ../conf.h
