#********************************************************************
# lindner
# 3.8
# 1993/08/10 20:26:21
# /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/Makefile,v
# Exp
#
# Paul Lindner, University of Minnesota CIS.
#
# Copyright 1991, 1992 by the Regents of the University of Minnesota
# see the file "Copyright" in the distribution for conditions of use.
#********************************************************************
# MODULE: Makefile
# Makefile for gopher server
#*********************************************************************
# Revision History:
# Makefile,v
# Revision 3.8  1993/08/10  20:26:21  lindner
# Added fi for gopherindex install
#
# Revision 3.7  1993/08/05  22:20:19  lindner
# Only install gopherindex if built
#
# Revision 3.6  1993/08/04  22:12:01  lindner
# Rebuild server if patchlevel.h changes
#
# Revision 3.5  1993/07/27  05:27:34  lindner
# Mondo Debug overhaul from Mitra
#
# Revision 3.4  1993/07/07  19:41:00  lindner
# removed popen stuff
#
# Revision 3.3  1993/03/26  19:45:38  lindner
# Fix for parameters to makegopherindex script
#
# Revision 3.2  1993/03/24  20:17:00  lindner
# Additions for gopherindex
#
# Revision 3.1.1.1  1993/02/11  18:02:50  lindner
# Gopher+1.2beta release
#
# Revision 1.5  1993/02/09  21:35:55  lindner
# Fixed install target, save old gopherd.conf file
#
# Revision 1.4  1992/12/29  23:28:31  lindner
# Added mindexd.o to OBJS for fan-out integration.
#
# Revision 1.3  1992/12/21  20:44:37  lindner
# Added kernutils.o to OBJS (from mtm)
#
# Revision 1.2  1992/12/21  20:21:49  lindner
# Added ../bin/libftw.a to WAISGATEOBJS..
#
# Revision 1.1  1992/12/10  23:15:15  lindner
# gopher 1.1 release
#
#
#********************************************************************/
include ../Makefile.config

#
# Type of full-text searching to use....  WAIS
# requires that you have a built wais distribution.
#
# Next requires that you have the NeXTtext.tar.Z libraries.
#

LIBS	= $(SERVERLIBS)

NEXTLIBS	= -ltext -lbtree


#############################################################################

NEXTOBJ = NeXTindex.o

WAISGATEOBJ = ../ui/source.o ../bin/libftw.a

# Order changed, or won't work on SCO3.2.4 
WAISOBJ = ../bin/inv.a  ../bin/client.a ../bin/wais.a $(WAISGATEOBJ)

CCFLAGS	= $(OPT) -DDATA_DIRECTORY=\"$(SERVERDATA)\" $(DEBUGGING) \
	-DDOMAIN_NAME=\"$(DOMAIN)\" \
	-DGOPHER_PORT=$(SERVERPORT) \
	$(SEARCH) \
	$(SERVEROPTS) \
	$(GSYSTYPE) \
	$(INCLUDES)

OBJS	= error.o globals.o gopherd.o daemon.o special.o \
	dedot.o openers.o index.o Waisindex.o serverutil.o ftp.o \
	waisgopher.o ext.o site.o gopherdconf.o kernutils.o mindexd.o \
	authenticate.o tix.o command.o 

TARGET	= gopherd

all:
	(cd ..; $(MAKE) $(MFLAGS) server)

#
# Special rule for NeXT text indexing, can't use gcc (yet...)
#
NeXTindex.o : NeXTindex.c
	cc $(CCFLAGS) -ObjC -I. -c NeXTindex.c


#
# Special rule for wais gateway
#
waisgopher.o: waisgopher.c
	$(CC) $(CCFLAGS) -c waisgopher.c

gopherd.o: gopherd.c ../patchlevel.h
	$(CC) $(CCFLAGS) -DCONF_FILE=\"$(SERVERDIR)/gopherd.conf\" -c gopherd.c


.c.o:
	$(CC) $(CCFLAGS) -c $<

globals.o : globals.h

$(OBJS) : ../conf.h


next: $(OBJS) $(NEXTOBJ)
	$(CC) $(LDFLAGS) -o $(TARGET) $(OBJS) $(NEXTOBJ) $(DLOBJS) \
		$(LIBS) $(NEXTLIBS)
	./makegopherindex next $(SERVERDATA)

wais: $(OBJS)
	$(CC) $(LDFLAGS) -o $(TARGET) $(OBJS) $(WAISOBJ) $(DLOBJS) $(LIBS)
	./makegopherindex wais $(SERVERDATA)

nextwais: $(OBJS) $(NEXTOBJ)
	$(CC) $(LDFLAGS) -o $(TARGET) $(OBJS) $(NEXTOBJ) $(LIBS) \
		$(WAISOBJ) $(DLOBJS) $(NEXTLIBS)
	./makegopherindex next $(SERVERDATA)

none: $(OBJS) 
	$(CC) $(LDFLAGS) -o $(TARGET) $(OBJS) $(DLOBJS) $(LIBS)

install : all
	-mv $(SERVERDIR)/$(TARGET) $(SERVERDIR)/$(TARGET).old
	-mv $(SERVERDIR)/gopherd.conf $(SERVERDIR)/gopherd.conf.old
	$(INSTALL)  $(TARGET) $(SERVERDIR)
	$(INSTALL)  gopherd.conf $(SERVERDIR)
	if [ -f gopherindex ]; then \
		$(INSTALL)  gopherindex $(SERVERDIR); \
	fi
	-rm	$(SERVERDIR)/gopherls $(SERVERDIR)/gindexd
	-ln     -s $(SERVERDIR)/$(TARGET) $(SERVERDIR)/gopherls
	-ln     -s $(SERVERDIR)/$(TARGET) $(SERVERDIR)/gindexd
	
clean:
	-rm -f $(TARGET) $(OBJS) $(NEXTOBJ) *.out *~ core gopherindex

