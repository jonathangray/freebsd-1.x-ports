##
#
# Makefile for the cb package of xview
#
# @(#)Makefile	50.12 93/06/28
#
##

#
# Copyright (c) 1990 by Sun Microsystems, Inc.
#

SOURCEDIR		= ../../..

include ${SOURCEDIR}/config/Master.cfg

LIBPARENT		= xview
LIBPART			= imcb
TOOL			=
DELIVERABLE		= deliverable
STATIC			= static
SHARED			= shared
SOURCES			= stat_imcb.c
SOURCES.sa		= 
OBJECTS			= ${SOURCES:%.c=%.o}
OBJECTS.sa		= ${SOURCES.sa:%.c=%.o}
HEADERS			= ${HEADERS.public} ${HEADERS.private}
HEADERS.public		= 
HEADERS.private		=
DERIVEDFILES		=

INCLUDE			= -I${SOURCEDIR}/include

MAKE.install.target	= # install_h

INSTALL.headers.dir	= ${DEST.include.xview}

PIC			= ${BIGPIC}

include ${RULESET.libchild}
