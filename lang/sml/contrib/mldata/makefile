#
#       MLDATA makefile
#       @(#)makefile	2.5 93/02/12 12:02:50
#
PUB      = /users/ftp/pub/WWW/SML/mldata
GRAMMAR  = mlkernel.mlg
PARSER   = mlkernel.tab.sml mlkernel.sym.sml mlkernel.tab.data
SOURCES  = mlkernel.sml mlkernel.pretty.sml mlkernel.syntax.sml \
	   mlkernel.write.sml utils.sml
ETC      = makefile README doc.doc prettyprinter.sml
LIB      = lib/list.sig.sml     lib/list.sml            \
	   lib/streamio.sig.sml lib/streamio.sml        \
	   lib/sml-lr.sig.sml   lib/sml-lr.sml          \
	   lib/extend.sig.sml   lib/extend.sml          \
	   lib/Import.sml
#
#       System generation
#
mldata: $(PARSER) $(SOURCES) $(LIB);  echo 'use "mlkernel.sml"; export()' | sml

#
#       Distribution generation
#
dist:   $(PARSER) ;
	tar -cvf - $(SOURCES) $(LIB) mlkernel.mlg $(ETC) $(PARSER) | compress > /tmp/mldata.tar.Z
	mv /tmp/mldata.tar.Z $(PUB)
	cp README            $(PUB)

#
#       PARSER table generation
#       Someone ought to convert the mllama-fodder into ml-yacc fodder
#
#
$(PARSER):              $(GRAMMAR); mllama mlkernel
mlkernel.write.sml:     mlkernel.syntax.sml;
			mldata --comments               \
			       -wide                    \
			       -string prstring         \
			       -pre    pr               \
			       -prim   pr               \
			       -hom    prlayer  mlkernel.syntax.sml > mlkernel.write.sml
#
#
#
