srcdir=/usr/ports/editor/emacs/src
VPATH=/usr/ports/editor/emacs/src
CC=gcc
CPP=gcc -E
CFLAGS=-g -O 
C_SWITCH_SYSTEM=
LN_S=ln -s
dot = .
lispdir = ${srcdir}/$(dot)$(dot)/lisp/
libsrc = $(dot)$(dot)/lib-src/
etc = $(dot)$(dot)/etc/
shortnamesdir = $(dot)$(dot)/shortnames/
cppdir = $(dot)$(dot)/cpp/
oldXMenudir = $(dot)$(dot)/oldXMenu/
config_h = config.h
SHELL=/bin/sh
STARTFILES = crt0.o 
ALL_CFLAGS=-Demacs -DHAVE_CONFIG_H $(MYCPPFLAG) -I. -I${srcdir}       -I/usr/X386/include      ${CFLAGS}
.c.o:
	$(CC) -c $(CPPFLAGS) $(ALL_CFLAGS) $<
XOBJ= xterm.o xfns.o xfaces.o xmenu.o xselect.o xrdb.o
OLDXMENU= libXMenu11.a
LIBXMENU= $(OLDXMENU)
LIBX= $(LIBXMENU) -L/usr/X386/lib  -lX11     
GNULIB_VAR = `$(CC) -print-libgcc-file-name` 
LD=cc 
LDFLAGS =      
obj=    dispnew.o frame.o scroll.o xdisp.o window.o 	term.o cm.o $(XOBJ) 	emacs.o keyboard.o macros.o keymap.o sysdep.o 	buffer.o filelock.o insdel.o marker.o intervals.o textprop.o  	minibuf.o fileio.o dired.o filemode.o 	cmds.o casetab.o casefiddle.o indent.o search.o regex.o undo.o 	alloc.o data.o doc.o editfns.o callint.o 	eval.o floatfns.o fns.o print.o lread.o 	abbrev.o syntax.o unexfreebsd.o  mocklisp.o bytecode.o 	process.o callproc.o 	doprnt.o  
termcapobj = termcap.o tparam.o
mallocobj = gmalloc.o ralloc.o vm-limit.o
allocaobj =
otherobj= $(termcapobj) lastfile.o $(mallocobj) $(allocaobj)
lisp= 	${lispdir}abbrev.elc 	${lispdir}buff-menu.elc 	${lispdir}byte-run.elc 	${lispdir}c-mode.elc 	${lispdir}files.elc 	${lispdir}fill.elc 	${lispdir}float-sup.elc  	${lispdir}frame.elc ${lispdir}mouse.elc ${lispdir}select.elc ${lispdir}scroll-bar.elc  	${lispdir}help.elc 	${lispdir}indent.elc 	${lispdir}isearch.elc 	${lispdir}lisp-mode.elc 	${lispdir}lisp.elc 	${lispdir}loaddefs.el 	${lispdir}map-ynp.elc 	${lispdir}page.elc 	${lispdir}paragraphs.elc 	${lispdir}paths.el 	${lispdir}register.elc 	${lispdir}replace.elc 	${lispdir}simple.elc 	${lispdir}startup.elc 	${lispdir}subr.elc 	${lispdir}text-mode.elc 	${lispdir}vc-hooks.elc 	  	${lispdir}window.elc
LIBES = $(LIBX) -lutil           $(GNULIB_VAR) -lm  -lc  $(GNULIB_VAR)
all: emacs  
emacs: temacs ${etc}DOC ${lisp}
	-if [ -w ${srcdir}/../lisp ]; then 	  w=`pwd`; cd ${srcdir}; $${w}/temacs -batch -l ../lisp/inc-vers; 	else true; fi
	./temacs -batch -l loadup dump
${etc}DOC: ${libsrc}make-docfile ${obj} ${lisp}    
	rm -f ${etc}DOC
	${libsrc}make-docfile -d ${srcdir} ${obj}     ${lisp} 	  ${lispdir}version.el > ${etc}DOC
${libsrc}make-docfile:
	cd ${libsrc}; ${MAKE} ${MFLAGS} make-docfile ../arch-lib
temacs:   $(LOCALCPP) $(SHORT) $(STARTFILES) $(OLDXMENU) ${obj} ${otherobj}     prefix-args
	$(LD) `./prefix-args -Xlinker  ${STARTFLAGS} ${LDFLAGS} `      -o temacs ${STARTFILES} ${obj} ${otherobj}          ${LIBES}
prefix-args: prefix-args.c $(config_h)
	$(CC) $(ALL_CFLAGS) ${srcdir}/prefix-args.c -o prefix-args
CPP = $(CC) -E
$(OLDXMENU): really-oldXMenu
	-rm -f $(OLDXMENU)  
	${LN_S} ${oldXMenudir}libXMenu11.a $(OLDXMENU)
C_SWITCH_MACHINE_1 =  
C_SWITCH_SYSTEM_1 =  
C_SWITCH_SITE_1 =  
C_SWITCH_X_SITE_1 = -I/usr/X386/include 
C_SWITCH_X_MACHINE_1 =  
C_SWITCH_X_SYSTEM_1 =  
really-oldXMenu:
	cd ${oldXMenudir}; ${MAKE} ${MFLAGS}       CC='${CC}' CFLAGS='${CFLAGS}'     "C_SWITCH_X_SITE=$(C_SWITCH_X_SITE_1)"     "C_SWITCH_X_MACHINE=$(C_SWITCH_X_MACHINE_1)"     "C_SWITCH_X_SYSTEM=$(C_SWITCH_X_SYSTEM_1)"     "C_SWITCH_SITE=$(C_SWITCH_SITE_1)"     "C_SWITCH_MACHINE=$(C_SWITCH_MACHINE_1)"     "C_SWITCH_SYSTEM=$(C_SWITCH_SYSTEM_1)"
	@true   
paths.h: paths.h.in
	@echo "The file paths.h needs to be set up from paths.h.in."
	@echo "Consult the file \`INSTALL' for instructions for building Emacs."
	exit 1
config.h: config.h.in
	@echo "The file config.h needs to be set up from config.h.in."
	@echo "Consult the file \`INSTALL' for instructions for building Emacs."
	exit 1
abbrev.o : abbrev.c buffer.h commands.h $(config_h) 
buffer.o : buffer.c syntax.h buffer.h commands.h window.h    intervals.h  blockinput.h $(config_h) 
callint.o : callint.c window.h commands.h buffer.h mocklisp.h    keyboard.h $(config_h)
callproc.o : callproc.c paths.h buffer.h commands.h $(config_h) process.h
casefiddle.o : casefiddle.c syntax.h commands.h buffer.h $(config_h) 
casetab.o : casetab.c buffer.h $(config_h)
cm.o : cm.c cm.h termhooks.h $(config_h)
cmds.o : cmds.c syntax.h buffer.h commands.h $(config_h) 
pre-crt0.o : pre-crt0.c
crt0.o : crt0.c $(config_h)
	$(CC) -c $(ALL_CFLAGS)    ${srcdir}/crt0.c
dired.o : dired.c commands.h buffer.h $(config_h) regex.h
dispnew.o : dispnew.c commands.h frame.h window.h buffer.h dispextern.h    termchar.h termopts.h termhooks.h cm.h disptab.h systty.h systime.h $(config_h)
doc.o : doc.c $(config_h) paths.h buffer.h keyboard.h
doprnt.o : doprnt.c
editfns.o : editfns.c window.h buffer.h systime.h intervals.h  $(config_h) 
emacs.o : emacs.c commands.h systty.h syssignal.h process.h intervals.h  $(config_h) 
fileio.o : fileio.c window.h buffer.h systime.h intervals.h  $(config_h) 
filelock.o : filelock.c buffer.h paths.h $(config_h)
filemode.o : filemode.c  $(config_h)
getloadavg.o : getloadavg.c $(config_h)
indent.o : indent.c frame.h window.h indent.h buffer.h $(config_h) termchar.h    termopts.h disptab.h
insdel.o : insdel.c window.h buffer.h intervals.h  blockinput.h $(config_h)
keyboard.o : keyboard.c termchar.h termhooks.h termopts.h buffer.h    commands.h frame.h window.h macros.h disptab.h keyboard.h syssignal.h    systty.h systime.h dispextern.h intervals.h blockinput.h $(config_h) 
keymap.o : keymap.c buffer.h commands.h keyboard.h termhooks.h blockinput.h    $(config_h) 
lastfile.o : lastfile.c  $(config_h)
macros.o : macros.c window.h buffer.h commands.h macros.h $(config_h)
malloc.o : malloc.c $(config_h)
gmalloc.o : gmalloc.c $(config_h)
ralloc.o : ralloc.c $(config_h)
vm-limit.o : vm-limit.c mem-limits.h $(config_h) 
marker.o : marker.c buffer.h $(config_h) 
minibuf.o : minibuf.c syntax.h dispextern.h frame.h window.h    buffer.h commands.h $(config_h) 
mocklisp.o : mocklisp.c buffer.h $(config_h)
process.o : process.c process.h buffer.h window.h termhooks.h termopts.h    commands.h syssignal.h systime.h systty.h syswait.h frame.h $(config_h) 
regex.o : regex.c syntax.h buffer.h $(config_h) regex.h 
frame.o : frame.c xterm.h window.h frame.h termhooks.h commands.h keyboard.h    buffer.h $(config_h)
scroll.o : scroll.c termchar.h $(config_h) dispextern.h frame.h
search.o : search.c regex.h commands.h buffer.h syntax.h blockinput.h $(config_h) 
syntax.o : syntax.c syntax.h buffer.h commands.h $(config_h) 
sysdep.o : sysdep.c $(config_h) dispextern.h termhooks.h termchar.h termopts.h    frame.h syssignal.h systty.h systime.h syswait.h blockinput.h window.h
term.o : term.c termchar.h termhooks.h termopts.h $(config_h) cm.h frame.h    disptab.h
termcap.o : termcap.c $(config_h)
terminfo.o : terminfo.c $(config_h)
tparam.o : tparam.c $(config_h)
undo.o : undo.c buffer.h commands.h $(config_h) 
unexfreebsd.o  : unexfreebsd.c  $(config_h)
window.o : window.c indent.h commands.h frame.h window.h buffer.h termchar.h    termhooks.h disptab.h keyboard.h $(config_h)
xdisp.o : xdisp.c macros.h commands.h indent.h buffer.h dispextern.h    termchar.h frame.h window.h disptab.h termhooks.h $(config_h) 
xfaces.o : xfaces.c dispextern.h frame.h xterm.h buffer.h blockinput.h    window.h $(config_h) 
xfns.o : xfns.c buffer.h frame.h window.h keyboard.h xterm.h    blockinput.h $(config_h)
xmenu.o : xmenu.c xterm.h window.h frame.h keyboard.h blockinput.h $(config_h)
xterm.o : xterm.c xterm.h termhooks.h termopts.h termchar.h window.h   dispextern.h frame.h disptab.h blockinput.h systime.h   gnu.h sink.h sinkmask.h $(config_h)
xselect.o : xselect.c dispextern.h frame.h xterm.h blockinput.h $(config_h) 
xrdb.o : xrdb.c $(config_h)
hftctl.o : hftctl.c $(config_h)
alloc.o : alloc.c frame.h window.h buffer.h  puresize.h syssignal.h
alloc.o : blockinput.h $(config_h) intervals.h 
bytecode.o : bytecode.c buffer.h $(config_h)
data.o : data.c buffer.h puresize.h syssignal.h $(config_h)
eval.o : eval.c commands.h keyboard.h blockinput.h $(config_h)
floatfns.o : floatfns.c $(config_h)
fns.o : fns.c commands.h $(config_h) frame.h buffer.h keyboard.h intervals.h 
print.o : print.c process.h frame.h window.h buffer.h $(config_h)
lread.o : lread.c commands.h keyboard.h buffer.h paths.h $(config_h)  termhooks.h
textprop.o : textprop.c buffer.h intervals.h $(config_h)
intervals.o : intervals.c buffer.h intervals.h puresize.h $(config_h)
sunfns.o  : sunfns.c buffer.h $(config_h)
${libsrc}emacstool: ${libsrc}emacstool.c
	cd ${libsrc}; ${MAKE} ${MFLAGS} emacstool
mostlyclean:
	rm -f temacs prefix-args xmakefile* core \#* *.o libXMenu11.a
	rm -f ../etc/DOC
clean: mostlyclean
	rm -f emacs-* emacs
distclean: clean
	rm -f paths.h config.h Makefile Makefile.in ../etc/DOC-*
realclean: distclean
	rm -f TAGS
versionclean:
	-rm -f emacs emacs-* ../etc/DOC*
extraclean: distclean
	-rm -f *~ \#* m/?*~ s/?*~
SOURCES = *.[ch] [sm]/?* COPYING ymakefile 	config.h.in README COPYING ChangeLog vms.pp-trans
unlock:
	chmod u+w $(SOURCES)
relock:
	chmod -w $(SOURCES)
	chmod +w paths.h
tagsfiles = [a-z]*.h [a-z]*.c ../lisp/[a-z]*.el ../lisp/term/[a-z]*.el
TAGS: $(tagsfiles)
	etags $(tagsfiles)
tags: TAGS
.PHONY: tags
