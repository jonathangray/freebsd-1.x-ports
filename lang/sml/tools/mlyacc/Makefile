# Makefile for creating mlyacc.sml and base.sml files.
# Created: David Tarditi (tarditi@cs.cmu.edu) 12/4/89
#
# Description:
#   This makefile can create two different files:
#	smlyacc.sml: SML file for ML-Yacc.  It comes in two variants: 
#                     one to create a stand-alone image and one to be run
#                     from the interactive system.
#	base.sml: SML file which must be loaded before any files produced
#		  smlyacc are loaded.  This file contains background
#		  signatures, the polymorphic parser structure, and the
#		  functors to join lexers and parsers.
#
#		  It comes in two variants: one with a vanilla parser
#		  and another with an error-correcting parser
#
#    The makefile does not have an entry to create base.sml -- the user
#    must explicitly do this.  This forces the user to consciously choose
#    the parser to be created.

# create smlyacc file from parts

SRC = src
OBJ = base.sml $(SRC)/utils.sig $(SRC)/sigs.sml $(SRC)/absyn.sig \
   $(SRC)/hdr.sml $(SRC)/yacc.grm.sig $(SRC)/yacc.grm.sml $(SRC)/yacc.lex.sml \
   $(SRC)/parse.sml $(SRC)/utils.sml $(SRC)/grammar.sml $(SRC)/core.sml \
   $(SRC)/coreutils.sml $(SRC)/graph.sml $(SRC)/look.sml \
   $(SRC)/lalr.sml $(SRC)/mklrtable.sml $(SRC)/mkprstruct.sml \
   $(SRC)/shrink.sml $(SRC)/verbose.sml $(SRC)/absyn.sml $(SRC)/yacc.sml \
   $(SRC)/link.sml
BASESRC = $(SRC)/base.sig $(SRC)/stream.sml $(SRC)/lrtable.sml $(SRC)/join.sml

interactive: $(OBJ)
	 cat $(OBJ) > smlyacc.sml


stand-alone: $(OBJ) $(SRC)/export.sml
	cat $(OBJ)  $(SRC)/export.sml > smlyacc.sml

# create error correcting parser

ec:	$(BASESRC) $(SRC)/parser2.sml
	cat $(BASESRC) $(SRC)/parser2.sml > base.sml

# create vanilla parser

vanilla: $(BASESRC) $(SRC)/parser1.sml
	cat $(BASESRC) $(SRC)/parser1.sml > base.sml
