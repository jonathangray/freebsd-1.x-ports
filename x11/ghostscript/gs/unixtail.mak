#    Copyright (C) 1990, 1992, 1993 Aladdin Enterprises.  All rights reserved.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Partial makefile for Ghostscript, common to all Unix configurations.

# This is the last part of the makefile for Unix configurations.
# Since Unix make doesn't have an 'include' facility, we concatenate
# the various parts of the makefile together by brute force (in tar_cat).

# The following prevents GNU make from constructing argument lists that
# include all environment variables, which can easily be longer than
# brain-damaged system V allows.

.NOEXPORT:

# -------------------------------- Library -------------------------------- #

## The Unix platforms

# We have to include a test for the existence of sys/time.h,
# because some System V platforms don't have it.

# All reasonable Unix platforms.
unix__=gp_nofb.$(OBJ) gp_unix.$(OBJ) gdevpipe.$(OBJ)
unix_.dev: $(unix__)
	$(SHP)gssetmod unix_ $(unix__)
	$(SHP)gsaddmod unix_ -fdev pipe

gp_unix.$(OBJ): gp_unix.c $(AK) $(memory__h) $(string__h) $(gx_h) $(gp_h) \
 $(gsutil_h) $(stat__h) $(time__h)
	if ( test -f /usr/include/sys/time.h ) then $(CCC) gp_unix.c;\
	else $(CCC) -DNOSYSTIME gp_unix.c; fi

gdevpipe.$(OBJ): gdevpipe.c $(AK) $(stdio__h) $(gstypes_h) \
  $(filedev_h) $(stream_h)

# Brain-damaged System V platforms.
sysv__=gp_nofb.$(OBJ) gp_unix.$(OBJ) gp_sysv.$(OBJ)
sysv_.dev: $(sysv__)
	$(SHP)gssetmod sysv_ $(sysv__)

gp_sysv.$(OBJ): gp_sysv.c $(time__h) $(AK)
	if ( test -f /usr/include/sys/time.h ) then $(CCC) gp_sysv.c;\
	else $(CCC) -DNOSYSTIME gp_sysv.c; fi

# -------------------------- Auxiliary programs --------------------------- #

ansi2knr$(XE): ansi2knr.c $(stdio__h) $(string__h) $(malloc__h)
	$(CC) -o ansi2knr$(XE) $(CFLAGS) ansi2knr.c

echogs$(XE): echogs.c
	$(CC) -o echogs$(XE) $(CFLAGS) echogs.c

# On the RS/6000 (at least), compiling genarch.c with gcc with -O
# produces a buggy executable.
genarch$(XE): genarch.c
	$(CC) -o genarch$(XE) genarch.c

genconf$(XE): genconf.c
	$(CC) -o genconf$(XE) genconf.c

# ----------------------------- Main program ------------------------------ #

BEGINFILES=
CCBEGIN=$(CCC) *.c

# Interpreter main program

GSUNIX=gs.$(OBJ) gsmain.$(OBJ) $(INT) $(LIBGS)

# The second call on echogs writes a \.  This is the only
# way to do it that works with all flavors of shell!
$(GS)$(XE): $(GSUNIX) ld.tr echogs $(ALL_DEVS)
	./echogs -n - $(CC) $(LDFLAGS) $(XLIBDIRS) -o gs $(GSUNIX) >_temp_
	./echogs -x 205c >>_temp_
	cat ld.tr >>_temp_
	./echogs - $(EXTRALIBS) -lm >>_temp_
	$(SH) <_temp_

# Installation

TAGS:
	etags -t *.c *.h

docdir=$(gsdatadir)/doc
exdir=$(gsdatadir)/examples

install: $(GS)
	-mkdir $(bindir)
	for f in $(GS) gsbj gsdj gslj gslp gsnd bdftops font2c ps2ascii ps2epsi ; do $(INSTALL_PROGRAM) $$f $(bindir)/$$f ; done
	-mkdir $(datadir)
	-mkdir $(gsdatadir)
	for f in README gslp.ps gs_init.ps gs_dps1.ps gs_fonts.ps gs_lev2.ps gs_statd.ps gs_type0.ps gs_dbt_e.ps gs_sym_e.ps quit.ps Fontmap uglyr.gsf bdftops.ps decrypt.ps font2c.ps impath.ps landscap.ps level1.ps prfont.ps ps2ascii.ps ps2epsi.ps ps2image.ps pstoppm.ps showpage.ps type1ops.ps wrfont.ps ; do $(INSTALL_DATA) $$f $(gsdatadir)/$$f ; done
	-mkdir $(docdir)
	for f in NEWS ansi2knr.1 devices.doc drivers.doc fonts.doc gs.1 hershey.doc history.doc humor.doc language.doc lib.doc make.doc ps2epsi.doc psfiles.doc readme.doc use.doc xfonts.doc ; do $(INSTALL_DATA) $$f $(docdir)/$$f ; done
	-mkdir $(exdir)
	for f in chess.ps cheq.ps colorcir.ps golfer.ps escher.ps snowflak.ps tiger.ps ; do $(INSTALL_DATA) $$f $(exdir)/$$f ; done
