#********************************************************************
# lindner
# 3.5
# 1993/08/04 22:06:35
# /home/mudhoney/GopherSrc/CVS/gopher+/gopher/Makefile,v
# Exp
#
# Paul Lindner, University of Minnesota CIS.
#
# Copyright 1991, 1992 by the Regents of the University of Minnesota
# see the file "Copyright" in the distribution for conditions of use.
#********************************************************************
# MODULE: Makefile
# Makefile for gopher client
#*********************************************************************
# Revision History:
# Makefile,v
# Revision 3.5  1993/08/04  22:06:35  lindner
# Rebuild client if patchlevel.h changes
#
# Revision 3.4  1993/07/27  05:28:45  lindner
# Mondo Debug overhaul from Mitra
#
# Revision 3.3  1993/04/15  21:28:25  lindner
# Mods for remote access
#
# Revision 3.2  1993/02/19  21:03:13  lindner
# Added stuff for global RC file.
#
# Revision 3.1.1.1  1993/02/11  18:02:57  lindner
# Gopher+1.2beta release
#
# Revision 1.4  1993/02/09  21:34:47  lindner
# Fixed install target
#
# Revision 1.3  1993/01/07  22:51:44  lindner
# Added download.o to list of objects
#
# Revision 1.2  1992/12/31  04:01:06  lindner
# moved html.c to html2.c for VMS
#
# Revision 1.1  1992/12/10  23:32:16  lindner
# gopher 1.1 release
#
#********************************************************************/


include ../Makefile.config

OBJS	= manager.o gopher.o globals.o ourutils.o cso.o \
	subprocs.o html2.o HTML.o CURcurses.o hymoo.o gopherrc.o download.o

CCFLAGS	= $(OPT) $(DEBUGGING) $(GSYSTYPE) $(INCLUDES) $(CLIENTOPTS) -DGOPHERHELP=\"$(CLIENTLIB)/gopher.hlp\" -DGLOBALRC=\"$(CLIENTLIB)/gopher.rc\" -DREMOTERC=\"$(CLIENTLIB)/gopherremote.rc\"

TARGET	= gopher

all:	$(TARGET)

.c.o:
	$(CC) $(CCFLAGS) -c $<

globals.o : globals.h version.h

../object/libgopher.a:
	(cd ..; $(MAKE) $(MFLAGS) objects)

$(TARGET) : $(OBJS) ../object/libgopher.a
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(CLIENTLIBS) 

install : all
	-mv $(CLIENTDIR)/$(TARGET) $(CLIENTDIR)/$(TARGET).old
	$(INSTALL) $(TARGET) $(CLIENTDIR)
	$(INSTALL) gopher.rc $(CLIENTLIB)
	$(INSTALL) gopherremote.rc $(CLIENTLIB)
	$(INSTALL) gopher.hlp $(CLIENTLIB)

clean:
	-rm -f $(TARGET) $(OBJS) *.out *~ core

$(OBJS): gopher.h ../conf.h
globals.o: globals.h
gopherrc.o: ../patchlevel.h
manager.o: ../patchlevel.h
