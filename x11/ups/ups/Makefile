# @(#)Makefile	1.41 19/9/92 (UKC)

# Optional multi-architecture compilation support
D = ${OBJDIR}${SLASH}
S = ${SRCDIR}${SLASH}
LIBSRC = ../lib
LIBOBJDIR = $D
L = ${LIBOBJDIR}

UPS_BINARY = $Dups
CX_BINARY = $Dcx

target: oups

all: $Dups

# Installation parameters.  Note that these will usually be overridden
# by the ones in the top level Makefile (one directory above here).
MANDIR = /usr/X386/man/man1
MANSUFFIX = 1
BIN = /usr/local/bin
WBIN = /usr/local/wbin
X11BIN = /usr/local/wbin/X11

# Flags for cc. CCFLAGS is read from the environment.
DEPS_DEVELHDRS = -Idevelhdrs
DEVELHDRS = -I$Sdevelhdrs
CFLAGS = ${CCFLAGS} ${DEVELHDRS} ${INCLUDEDIRS} ${ARCH_CFLAGS} ${MORE_CFLAGS}

CCLINE = ${IN_OBJDIR} ${CC} -c ${CFLAGS} $S

LINKCC = ${CC}
LINKCFLAGS = ${CFLAGS}

STOBJS = $Dst_lookup.o $Dst_stab.o $Dst_shlib.o $Dst_fmap.o $Dst_parse.o $Dst_read.o $Dst_skim.o $Dst_cb.o $Dst_te.o
STSRCS =  st_lookup.c    st_stab.c   st_shlib.c   st_fmap.c   st_parse.c   st_read.c   st_skim.c   st_cb.c   st_te.c

VAOBJS = $Dva_decl.o $Dva_edit.o $Dva_menu.o $Dva_type.o $Dva_val.o $Dva_expr.o
VASRCS =   va_decl.c   va_edit.c   va_menu.c   va_type.c   va_val.c   va_expr.c

UIOBJS = $Dui_iploop.o $Dui_layout.o $Dui_menu.o $Dui_misc.o $Dui_src.o
UISRCS =   ui_iploop.c   ui_layout.c   ui_menu.c   ui_misc.c   ui_src.c

UI2OBJS = $Dstate.o $Dreg.o $Dcursors.o $Dmenudata.o $Dtdr.o
UI2SRCS =   state.c   reg.c   cursors.c   menudata.c   tdr.c

DBOBJS = $Dbp.o $Dtext.o $Dcore.o $Dproc.o $Dexec.o $Dtrun_ss.o $Dtrun_j.o $Dstack.o $Ddata.o
DBSRCS =   bp.c   text.c   core.c   proc.c   exec.c   trun_ss.c   trun_j.c   stack.c     data.c

ASOBJS = $Das_mi.o $Das_vax.o $Das_68020.o $Das_mips.o $Das_clipper.o $Das_sparc.o
ASSRCS =   as_mi.c   as_vax.c   as_68020.c   as_mips.c   as_clipper.c   as_sparc.c

OBJOBJS = $Dobj_bpt.o $Dobj_signal.o $Dobj_misc.o $Dobj_buildf.o $Dobj_target.o $Dobj_env.o $Dobj_stack.o
OBJSRCS =   obj_bpt.c   obj_signal.c   obj_misc.c   obj_buildf.c   obj_target.c     obj_env.c   obj_stack.c

CIOBJS = $Dci_parse.o $Dci_types.o $Dci_lex.o $Dci_stm.o \
	 $Dci_decl.o $Dci_func.o $Dci_util.o $Dci_expr.o $Dci_showexpr.o \
	 $Dci_constexpr.o $Dci_compile.o $Dci_compile_expr.o $Dci_opcodes.o \
	 $Dci_execute.o $Dci_init.o

CISRCS =              ci_types.c   ci_lex.c   ci_stm.c \
	 ci_decl.c   ci_func.c   ci_util.c   ci_expr.c   ci_showexpr.c \
	 ci_constexpr.c   ci_compile.c   ci_compile_expr.c ci_opcodes.c \
	 ci_execute.c     ci_init.c

RESTOBJS = $Dexpr.o $Dsrc.o $Doutput.o $Dprintf.o $Dtextwin.o $Dups.o
RESTSRCS = expr.c   src.c   output.c   printf.c   textwin.c   ups.c

OBJS =    ${UIOBJS} ${STOBJS} ${VAOBJS} ${OBJOBJS} ${UI2OBJS} \
          ${DBOBJS} ${RESTOBJS} ${ASOBJS} ${CIOBJS} $Dsccsdata.o

STDSRCS = ${UISRCS} ${STSRCS} ${VASRCS} ${OBJSRCS} ${UI2SRCS} \
          ${DBSRCS} ${RESTSRCS} ${ASSRCS} ${CISRCS} cx.c cx_builtins.c

CX_OBJS = $Dcx.o $Dcx_builtins.o ${CIOBJS}

CSRCS = ${STDSRCS} ci_parse.c	# C source files
HSRCS = ${STDSRCS} ci_parse.y	# Actual source files

TGT_MENUS = tgt_start_men.c tgt_next_men.c tgt_step_men.c \
	    tgt_cont_men.c tgt_stop_men.c tgt_exit_men.c
MENUS = bphmen.c bptmen.c cbhmen.c cblockmen.c cmdmen.c coremen.c \
	filmen.c gblmen.c sghmen.c shmen.c sigmen.c srcmen.c outmen.c \
	${TGT_MENUS} varmen.c envheadmen.c envmen.c

CURSORS = cu_dead cu_dead_mask cu_display cu_display_mask cu_drag_box cu_drag_box_mask cu_menu cu_menu_mask cu_sbar cu_sbar_mask cu_sc_pressed cu_sc_pressed_mask cu_scroll_down cu_scroll_down_mask cu_scroll_up cu_scroll_up_mask cu_wait cu_wait_mask cu_line cu_line_mask cu_dragbox cu_dragbox_mask

HDRS = \
	as.h breakpoint.h ci.h ci_compile.h ci_compile_expr.h \
	ci_constexpr.h ci_decl.h ci_expr.h ci_func.h ci_init.h ci_lex.h \
	ci_machine.h ci_parse.h ci_showexpr.h ci_stm.h ci_tokens.h \
	ci_types.h ci_util.h core.h cursors.h cx_builtins.h data.h \
	debug.h exec.h expr.h menudata.h mreg.h obj_bpt.h obj_buildf.h \
	obj_env.h obj_misc.h obj_signal.h obj_stack.h obj_target.h \
	objtypes.h output.h preamble.h printf.h proc.h reg.h sccsdata.h \
	src.h st_priv.h stack.h state.h symtab.h tdr.h text.h textwin.h \
	trun.h ui.h ui_priv.h ups.h va.h va_priv.h mips_frame.h

DOCS = doc/ups.man doc/porting.ms

FILES = ${HDRS} ${HSRCS} ${DOCS} Makefile 

CIHDRS = ci_constexpr.h ci_decl.h ci_expr.h ci_func.h ci_lex.h \
	 ci_parse.h ci_showexpr.h ci_stm.h ci_tokens.h ci_types.h ci_util.h

#
#  The following section gives the locations of various libraries.
#  There are usually several definitions with all but one commented out.
#

UKCLIB = ${LIBSRC}/libukcprog/$Llibukcprog.a	# Development ukcprog library
#UKCLIB = -lukcprog				# Installed ukcprog library

MTRLIB = ${LIBSRC}/libmtrprog/$Llibmtrprog.a	# Development mtrprog library
#MTRLIB = -lmtrprog				# Installed mtrprog library

OBJLIB = ${LIBSRC}/libobj/$Llibobj.a		# Development object library.
#OBJLIB = -lobj					# Installed object library.

ARGLIB = ${LIBSRC}/libarg/$Llibarg.a		# Development arg library.
#ARGLIB = -larg					# Installed arg library.

MENLIB = ${LIBSRC}/libMen3/$LlibMen3wn.a	# Development menu library.
#MENLIB = -lMen3wn				# Installed menu library.

X11WNLIB = ${LIBSRC}/libx11wn/$LX11/libwn.a	# Development X11 wn library
#X11WNLIB = -lx11wn				# Installed X11 wn library

X11LIB = -lX11					# Installed X11 Xlib library.

X10XLIB = -lX					# Installed X10 Xlib library.

COMMONLIBS = ${OBJLIB} ${MENLIB} ${ARGLIB} ${MTRLIB} ${UKCLIB}

X11LIBS = ${COMMONLIBS} ${X11WNLIB} ${X11LIB}
X11LIBDEPS =  ${COMMONLIBS} ${X11WNLIB}

X11LINTLIBS = -larg -lobj -lMen3wn -lx11wn -lX11

ocx: ${CX_BINARY}
oups: ${UPS_BINARY}

#CXLIBS = ${MTRLIB} ${UKCLIB} ${MENLIB} ${X11WNLIB} ${X11LIB}
CXLIBS = ${MTRLIB} ${UKCLIB}
CXLIBDEPS = ${CXLIBS}

# Use $D/. so we don't get a null target when $D is undefined.
# We say sh -c to avoid exit status problems from the || construct.
$D/.:
	(IFS=/; for i in $D; do test -d $$i || mkdir $$i; cd $$i; done)

${CX_BINARY}: $D/. ${CX_OBJS} ${CXLIBDEPS}
	rm $@ > $@
	${LINKCC} ${LINKFLAGS} ${LINKCFLAGS} -o $@ ${CX_OBJS} ${CXLIBS} ${STDLIBS}

# The following dependencies are for building ups in parallel on an MP system
uiobjs: ${UIOBJS}
stobjs: ${STOBJS}
vaobjs: ${VAOBJS}
objobjs: ${OBJOBJS}
ui2objs: ${UI2OBJS}
dbobjs: ${DBOBJS}
restobjs: ${RESTOBJS}
asobjs: ${ASOBJS}
ciobjs: ${CIOBJS}

objs: ${OBJS}

${UPS_BINARY}: $D/. ${OBJS} ${X11LIBDEPS}
	@rm $@ > $@
	@echo Linking $@ ...
	@${LINKCC} ${LINKFLAGS} ${LINKCFLAGS} -o $@ ${OBJS} ${X11LIBS} ${STDLIBS}

st: ${STOBJS}

list:
	@echo README ${FILES} ci_parse.c sccsdata.c | tr ' ' '\012'
	@echo doc/sample.c
	@echo README GLUEMENUS ${MENUS} | tr ' ' '\012' | sed 's;^;menus/;'
	@echo README START END ${CURSORS} | tr ' ' '\012' | sed 's;^;cursors/;'

tags: ${CSRCS} ${HDRS}
	ctags -tw ${CSRCS} ${HDRS} \
		${LIBSRC}/libarg/*.[ch] \
		${LIBSRC}/libobj/o_*.[ch] \
		${LIBSRC}/libMen3/*.[ch] \
		${LIBSRC}/libx11wn/wn_*.[ch] \
		${LIBSRC}/libmtrprog/*.[ch] \
		${LIBSRC}/libukcprog/*.[ch]
	rep ci_parse\\.c ci_parse.y tags

lint:
	lint ${DEPS_DEVELHDRS} ${LINTFLAGS} ${ARCH_CFLAGS} ${CSRCS} # ${X11LINTLIBS}

count: ${HSRCS} ${HDRS}
	wc ${HDRS} ${HSRCS}
	@echo `egrep -v '^[ 	]*$$' ${HSRCS} ${HDRS} | wc -l` non blank lines

ci_count: ${CIHDRS} ${CISRCS} ci_parse.y
	wc -l ${CIHDRS} ${CISRCS} ci_parse.y

deps: ${CSRCS} sccsdata.c
	@deps ${DEPS_DEVELHDRS} ${CSRCS} sccsdata.c

cursorfile:
	cd cursors; cat START ${CURSORS} END > ../cursors.c

menusfile:
	cd menus; ./GLUEMENUS ${MENUS} > ../menudata.c
	
veryclean: clean
	rm -f tags ci_parse.c

clean:
	rm -f ${OBJS} $Dups core

checklist: ${FILES} ci_parse.c sccsdata.c
	@checklist

install:
	@echo You must run make install in the directory above this one. 1>&2

checkinstall: checkxinstall

checkxinstall: ${X11BIN}/ups ${MANDIR}/ups.${MANSUFFIX}

installcx: ${BIN}/cx

${X11BIN}/ups: $Dups
	-test -f $@ && mv -f $@ ${X11BIN}/old/ups
	install -c -m 755 -o bin -s $? $@

${BIN}/cx: ${CX_BINARY}
	install -c -m 755 -o bin -s $? $@

${MANDIR}/ups.${MANSUFFIX}: doc/ups.man
	install -c -m 644 -o root $? $@

#  Special dependency for ci_parse.y
ci_parse.c: ci_parse.y
	${IN_OBJDIR} yacc -d $Sci_parse.y
	-sed '1,5d' ci_tokens.h | cmp - $Dy.tab.h || { \
		echo '/* ci_tokens.h - yacc generation token #defines */';   \
		echo;							     \
		echo '/* Copyright 1989 Kent Software Tools Centre */';	     \
		echo;							     \
		echo '/* %'W'% %'G'% (UKC) */';				     \
		cat $Dy.tab.h;						     \
	} > ci_tokens.h
	sed -e '/^extern char \*malloc(), \*realloc();/d'		     \
	    -e 's/malloc(/malloc((size_t)/'				     \
	    -e '/goto skip_init;/,/skip_init:/d'			     \
	    -e '/yynewstate:/,/goto yy_newstate/d'			     \
	    -e 's/\(yymaxdepth \* sizeof(YYSTYPE)\));$$/(size_t)(\1));/'     \
	    -e 's/\(yymaxdepth \* sizeof(int)\));$$/(size_t)(\1));/'	     \
	    -e '/^# *line/d'					     \
		$Dy.tab.c > ci_parse.c
	# cp $Dy.tab.c ci_parse.c

# .o header dependencies, generated by make deps
$Dui_iploop.o: ui_iploop.c cursors.h ups.h symtab.h ui.h tdr.h src.h ui_priv.h \
  data.h expr.h state.h as.h reg.h debug.h 
	${CCLINE}ui_iploop.c
$Dui_layout.o: ui_layout.c cursors.h reg.h ui.h tdr.h textwin.h ups.h symtab.h \
  src.h output.h ui_priv.h state.h menudata.h debug.h sccsdata.h 
	${CCLINE}ui_layout.c
$Dui_menu.o: ui_menu.c objtypes.h ui.h ui_priv.h exec.h obj_target.h reg.h tdr.h \
  state.h menudata.h cursors.h 
	${CCLINE}ui_menu.c
$Dui_misc.o: ui_misc.c ups.h debug.h objtypes.h symtab.h ci.h ui.h ui_priv.h \
  exec.h data.h obj_stack.h state.h tdr.h 
	${CCLINE}ui_misc.c
$Dui_src.o: ui_src.c ups.h symtab.h src.h tdr.h ui.h ui_priv.h cursors.h menudata.h \
  expr.h obj_bpt.h exec.h state.h 
	${CCLINE}ui_src.c
$Dst_lookup.o: st_lookup.c ups.h symtab.h st_priv.h 
	${CCLINE}st_lookup.c
$Dst_stab.o: st_stab.c ups.h symtab.h ci.h st_priv.h text.h obj_bpt.h src.h proc.h 
	${CCLINE}st_stab.c
$Dst_shlib.o: st_shlib.c ups.h symtab.h st_priv.h data.h 
	${CCLINE}st_shlib.c
$Dst_fmap.o: st_fmap.c ups.h symtab.h st_priv.h 
	${CCLINE}st_fmap.c
$Dst_parse.o: st_parse.c ups.h symtab.h ci.h st_priv.h 
	${CCLINE}st_parse.c
$Dst_read.o: st_read.c ups.h symtab.h st_priv.h 
	${CCLINE}st_read.c
$Dst_skim.o: st_skim.c ups.h symtab.h st_priv.h text.h ci.h 
	${CCLINE}st_skim.c
$Dst_cb.o: st_cb.c ups.h symtab.h st_priv.h ci.h 
	${CCLINE}st_cb.c
$Dst_te.o: st_te.c ups.h symtab.h text.h ci.h preamble.h st_priv.h 
	${CCLINE}st_te.c
$Dva_decl.o: va_decl.c ups.h symtab.h ci.h data.h va.h va_priv.h 
	${CCLINE}va_decl.c
$Dva_edit.o: va_edit.c ups.h symtab.h objtypes.h obj_buildf.h obj_target.h state.h \
  va.h ci.h data.h va_priv.h ui.h tdr.h 
	${CCLINE}va_edit.c
$Dva_menu.o: va_menu.c objtypes.h ups.h symtab.h data.h va.h menudata.h va_priv.h \
  ui.h obj_stack.h tdr.h 
	${CCLINE}va_menu.c
$Dva_type.o: va_type.c ups.h symtab.h data.h va.h va_priv.h ci.h 
	${CCLINE}va_type.c
$Dva_val.o: va_val.c ups.h symtab.h data.h va.h va_priv.h 
	${CCLINE}va_val.c
$Dva_expr.o: va_expr.c objtypes.h ups.h symtab.h ci.h ui.h va.h data.h exec.h \
  obj_stack.h menudata.h va_priv.h tdr.h 
	${CCLINE}va_expr.c
$Dobj_bpt.o: obj_bpt.c objtypes.h ups.h symtab.h breakpoint.h ci.h src.h ui.h \
  text.h exec.h data.h obj_bpt.h menudata.h state.h debug.h tdr.h 
	${CCLINE}obj_bpt.c
$Dobj_signal.o: obj_signal.c objtypes.h ui.h obj_signal.h menudata.h tdr.h 
	${CCLINE}obj_signal.c
$Dobj_misc.o: obj_misc.c ups.h symtab.h objtypes.h obj_target.h obj_misc.h ui.h \
  va.h obj_stack.h menudata.h tdr.h 
	${CCLINE}obj_misc.c
$Dobj_buildf.o: obj_buildf.c ups.h symtab.h objtypes.h obj_buildf.h obj_misc.h \
  obj_target.h obj_bpt.h obj_signal.h obj_env.h obj_stack.h va.h ui.h exec.h core.h \
  expr.h state.h 
	${CCLINE}obj_buildf.c
$Dobj_target.o: obj_target.c ups.h objtypes.h obj_target.h obj_env.h exec.h ui.h \
  obj_stack.h state.h tdr.h 
	${CCLINE}obj_target.c
$Dobj_env.o: obj_env.c objtypes.h ui.h obj_env.h menudata.h tdr.h 
	${CCLINE}obj_env.c
$Dobj_stack.o: obj_stack.c objtypes.h ups.h symtab.h obj_stack.h proc.h core.h \
  data.h preamble.h ui.h va.h obj_signal.h obj_misc.h stack.h menudata.h debug.h \
  tdr.h 
	${CCLINE}obj_stack.c
$Dstate.o: state.c src.h output.h reg.h obj_target.h state.h 
	${CCLINE}state.c
$Dreg.o: reg.c reg.h cursors.h debug.h tdr.h 
	${CCLINE}reg.c
$Dcursors.o: cursors.c cursors.h 
	${CCLINE}cursors.c
$Dmenudata.o: menudata.c 
	${CCLINE}menudata.c
$Dtdr.o: tdr.c ui.h cursors.h reg.h ups.h symtab.h exec.h objtypes.h obj_misc.h \
  obj_target.h obj_bpt.h obj_signal.h obj_env.h obj_stack.h va.h menudata.h src.h \
  expr.h output.h state.h tdr.h sccsdata.h 
	${CCLINE}tdr.c
$Dbp.o: bp.c ups.h proc.h breakpoint.h 
	${CCLINE}bp.c
$Dtext.o: text.c ups.h symtab.h text.h preamble.h 
	${CCLINE}text.c
$Dcore.o: core.c ups.h mreg.h proc.h core.h 
	${CCLINE}core.c
$Dproc.o: proc.c ups.h ui.h ci.h proc.h breakpoint.h mreg.h core.h debug.h as.h 
	${CCLINE}proc.c
$Dexec.o: exec.c ups.h symtab.h ci.h cursors.h proc.h core.h text.h exec.h breakpoint.h \
  obj_stack.h obj_target.h obj_signal.h obj_buildf.h obj_bpt.h trun.h printf.h \
  ui.h menudata.h tdr.h state.h 
	${CCLINE}exec.c
$Dtrun_ss.o: trun_ss.c ups.h symtab.h proc.h breakpoint.h trun.h text.h exec.h \
  obj_stack.h obj_signal.h obj_bpt.h 
	${CCLINE}trun_ss.c
$Dtrun_j.o: trun_j.c ups.h symtab.h proc.h breakpoint.h as.h trun.h data.h exec.h \
  obj_stack.h text.h obj_signal.h obj_bpt.h 
	${CCLINE}trun_j.c
$Dstack.o: stack.c ups.h symtab.h proc.h data.h text.h preamble.h stack.h 
	${CCLINE}stack.c
$Ddata.o: data.c ups.h data.h proc.h obj_stack.h obj_target.h state.h 
	${CCLINE}data.c
$Dexpr.o: expr.c objtypes.h ups.h symtab.h ui.h va.h obj_stack.h obj_misc.h obj_target.h \
  src.h state.h expr.h tdr.h 
	${CCLINE}expr.c
$Dsrc.o: src.c ups.h symtab.h textwin.h src.h 
	${CCLINE}src.c
$Doutput.o: output.c textwin.h output.h tdr.h 
	${CCLINE}output.c
$Dprintf.o: printf.c ups.h symtab.h ci.h proc.h printf.h output.h exec.h data.h \
  ui.h state.h tdr.h 
	${CCLINE}printf.c
$Dtextwin.o: textwin.c textwin.h 
	${CCLINE}textwin.c
$Dups.o: ups.c ups.h symtab.h reg.h ui.h tdr.h core.h exec.h obj_target.h obj_buildf.h \
  obj_bpt.h state.h debug.h 
	${CCLINE}ups.c
$Das_mi.o: as_mi.c ups.h symtab.h trun.h as.h text.h debug.h 
	${CCLINE}as_mi.c
$Das_vax.o: as_vax.c 
	${CCLINE}as_vax.c
$Das_68020.o: as_68020.c 
	${CCLINE}as_68020.c
$Das_mips.o: as_mips.c 
	${CCLINE}as_mips.c
$Das_clipper.o: as_clipper.c 
	${CCLINE}as_clipper.c
$Das_sparc.o: as_sparc.c ups.h as.h debug.h proc.h 
	${CCLINE}as_sparc.c
$Dci_types.o: ci_types.c ups.h symtab.h ci.h ci_parse.h ci_util.h ci_types.h 
	${CCLINE}ci_types.c
$Dci_lex.o: ci_lex.c ups.h symtab.h ci.h ci_parse.h ci_util.h ci_lex.h ci_tokens.h 
	${CCLINE}ci_lex.c
$Dci_stm.o: ci_stm.c ups.h symtab.h ci.h ci_parse.h ci_util.h ci_stm.h ci_func.h \
  ci_types.h ci_expr.h ci_constexpr.h ci_showexpr.h 
	${CCLINE}ci_stm.c
$Dci_decl.o: ci_decl.c ups.h symtab.h ci.h ci_parse.h ci_decl.h ci_types.h ci_util.h \
  ci_lex.h ci_init.h 
	${CCLINE}ci_decl.c
$Dci_func.o: ci_func.c ups.h symtab.h expr.h ci.h ci_parse.h ci_util.h ci_decl.h \
  ci_func.h ci_types.h ci_expr.h 
	${CCLINE}ci_func.c
$Dci_util.o: ci_util.c ups.h symtab.h expr.h ci.h ci_parse.h ci_expr.h ci_constexpr.h \
  ci_util.h 
	${CCLINE}ci_util.c
$Dci_expr.o: ci_expr.c ups.h symtab.h ci.h ci_parse.h ci_util.h ci_expr.h ci_func.h \
  ci_types.h ci_decl.h ci_showexpr.h ci_constexpr.h 
	${CCLINE}ci_expr.c
$Dci_showexpr.o: ci_showexpr.c ups.h symtab.h ci.h ci_parse.h ci_showexpr.h ci_types.h \
  ci_util.h 
	${CCLINE}ci_showexpr.c
$Dci_constexpr.o: ci_constexpr.c ups.h symtab.h ci.h ci_parse.h ci_showexpr.h \
  ci_constexpr.h ci_types.h ci_util.h 
	${CCLINE}ci_constexpr.c
$Dci_compile.o: ci_compile.c ups.h symtab.h ci.h ci_parse.h ci_types.h ci_machine.h \
  ci_compile.h ci_compile_expr.h ci_util.h 
	${CCLINE}ci_compile.c
$Dci_compile_expr.o: ci_compile_expr.c ups.h symtab.h ci_parse.h ci_types.h ci_constexpr.h \
  ci.h ci_machine.h ci_compile.h ci_compile_expr.h ci_util.h 
	${CCLINE}ci_compile_expr.c
$Dci_opcodes.o: ci_opcodes.c ups.h symtab.h ci.h ci_parse.h ci_types.h ci_machine.h \
  ci_compile.h ci_util.h 
	${CCLINE}ci_opcodes.c
$Dci_execute.o: ci_execute.c ci.h ci_machine.h 
	${CCLINE}ci_execute.c
$Dci_init.o: ci_init.c ups.h symtab.h ci.h ci_parse.h ci_init.h ci_expr.h ci_constexpr.h \
  ci_util.h ci_types.h 
	${CCLINE}ci_init.c
$Dcx.o: cx.c ups.h symtab.h ci.h cx_builtins.h 
	${CCLINE}cx.c
$Dcx_builtins.o: cx_builtins.c ups.h symtab.h ci.h cx_builtins.h 
	${CCLINE}cx_builtins.c
$Dci_parse.o: ci_parse.c ups.h symtab.h ci.h ci_parse.h ci_types.h ci_util.h \
  ci_decl.h ci_func.h ci_expr.h ci_stm.h ci_lex.h 
	${CCLINE}ci_parse.c
$Dsccsdata.o: sccsdata.c sccsdata.h 
	${CCLINE}sccsdata.c
