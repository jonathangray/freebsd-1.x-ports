# This Makefile installs services server Tcl/Tk scripts. 
#
# BINDIR	-- directory for binaries.
# LIBDIR	-- directory for tcl libraries.
# SCRIPT_DIR	-- directory for service scripts.
# MANDIR	-- directory for manual pages.
# WISH		-- Pathname to the wish program
# dpwish	-- Pathname to the dpwish program
#

BINDIR		= /usr/local/bin
LIBDIR		= /usr/local/lib/tcl
SCRIPT_DIR	= ${LIBDIR}/services
MANDIR		= /usr/local/man
WISH		= ${BINDIR}/wish
DPWISH		= ${BINDIR}/dpwish

CC = cc
CFLAGS = -O

default: install

install:	phoenix
	@echo "Installing phoenix"
	@cp phoenix $(BINDIR)
	@chmod 755 $(BINDIR)/phoenix

	@echo "Creating scripts"
	@if [ ! -d $(SCRIPT_DIR) ] ; then \
	    echo "Making directory $(SCRIPT_DIR)"; \
	    mkdir $(SCRIPT_DIR); \
	    chmod 755 $(SCRIPT_DIR); \
	    else true; \
	fi;
	@${DPWISH} -notk -f install-scripts.tcl $(SCRIPT_DIR) $(BINDIR)
	@cp killservice.tcl $(SCRIPT_DIR)
	@cp rmservice.tcl $(SCRIPT_DIR)
	@cp lservices.tcl $(SCRIPT_DIR)
	@cp serviced.tcl $(SCRIPT_DIR)

	@echo "Installing library files"
	@cp library/*.tcl $(LIBDIR)
	@echo "Making tclIndex"
	@echo auto_mkindex $(LIBDIR) '*.tcl' > tmp
	@echo exit >> tmp
	@$(WISH) -f tmp
	@/bin/rm -f tmp

	@echo "Installing man pages"
	@if [ ! -d $(MANDIR)/man1 ] ; then \
	    echo "Making directory $(MANDIR)/man1"; \
	    mkdir $(MANDIR)/man1; \
	    chmod 755 $(MANDIR)/man1; \
	    else true; \
	fi;
	@cp services.1 $(MANDIR)/man1
	@rm  $(MANDIR)/man1/killservice.1
	@rm  $(MANDIR)/man1/lservices.1
	@rm  $(MANDIR)/man1/rmservice.1
	@rm  $(MANDIR)/man1/serviced.1
	@ln -s $(MANDIR)/man1/services.1  $(MANDIR)/man1/killservice.1
	@ln -s $(MANDIR)/man1/services.1  $(MANDIR)/man1/lservices.1
	@ln -s $(MANDIR)/man1/services.1  $(MANDIR)/man1/rmservice.1
	@ln -s $(MANDIR)/man1/services.1  $(MANDIR)/man1/serviced.1
	@if [ ! -d $(MANDIR)/mann ] ; then \
	    echo "Making directory $(MANDIR)/mann"; \
	    mkdir $(MANDIR)/mann; \
	    chmod 755 $(MANDIR)/mann; \
	    else true; \
	fi;
	@cp services.n $(MANDIR)/mann

phoenix:	phoenix.o
	$(CC) -o phoenix phoenix.o

clean:
	rm -f killservice lservices rmservice serviced phoenix *.o
