# Generated automatically from Makefile.in by configure.
#   Copyright (C) 1988, 1990, 1991, 1992 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

# target name
PROG=	calc

# compiler specifics
CC =	cc
CFLAGS = -g -I$(srcdir) -I.
CALC = calc

# directory specifics
srcdir = .
VPATH = .

# testsuite specifics
RUNTEST= runtest
RUNTESTFLAGS=

##  ---  NOTHING BELOW HERE SHOULD REQUIRE MODIFCATIONS  ---  ##

SRCS=	calc.c

OBJS=	calc.o

all:	${PROG}

calc.o: calc.c

.c.o:
	${CC} ${CFLAGS} -I$(srcdir) -I. -c $<

${PROG}: ${OBJS} ${DPADD}
	${CC} ${LDFLAGS} ${CFLAGS} -o $@ ${OBJS} ${DPADD} ${LDADD}

Makefile: ${srcdir}/Makefile.in
	${SHELL} config.status

check:	site.exp all
	rootme=`pwd`; export rootme; \
	${RUNTEST} ${RUNTESTFLAGS} --tool ${PROG} CALC=${PROG} --srcdir ${srcdir}/testsuite

site.exp: ./config.status Makefile
	@echo "Making a new config file..."
	-@rm -f ./tmp?
	@touch site.exp

	-@mv site.exp site.bak
	@echo "## these variables are automatically generated by make ##" > ./tmp0
	@echo "# Do not edit here. If you wish to override these values" >> ./tmp0
	@echo "# add them to the last section" >> ./tmp0
	@echo "set tool calc" >> ./tmp0
	@echo "set srcdir ${srcdir}" >> ./tmp0
	@echo "set objdir `pwd`" >> ./tmp0
	@echo "## All variables above are generated by configure. Do Not Edit ##" >> ./tmp0
		@cat ./tmp0 > site.exp
	@cat site.bak | sed \
			-e '1,/^## All variables above are.*##/ d' >> site.exp
	-@rm -f ./tmp?

install: ${PROG}

clean:
	rm -f a.out [Ee]rrs tags mklog core ${OBJS} ${PROG} 

distclean: clean
	rm -f config.status Makefile calc.h calc.log calc.plog calc.psum
	rm -f calc.sum site.exp
