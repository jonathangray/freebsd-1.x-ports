#
# Makefile for nextispell.
#
# Created by Moritz Willers.
#
# 4. Januar 1994
# Version 0.2
#

NAME = nextispell

CFLAGS = -O -Wall

all: ${NAME}
	if [ -d ${NAME}.service ]; then rm -Rf ${NAME}.service/; else mkdir ${NAME}.service; fi
	mv ${NAME} ${NAME}.service/
	if [ -f services ]; then touch services; else cp services.template  services; fi
	mv services ${NAME}.service/
	strip ${NAME}.service/${NAME}

${NAME}: nextispell.m
	if [ -f configure.h ]; then touch configure.h; else cp configure.h.template  configure.h; fi
	cc ${CFLAGS} -o ${NAME} nextispell.m -lNeXT_s
	rm -f configure.h

install: all
	if [ -d ${HOME}/Library/Services/${NAME}.service ]; then rm -Rf ${HOME}/Library/Services/${NAME}.service/; fi
	cp -r ${NAME}.service ${HOME}/Library/Services/
	rm -Rf ${NAME}.service
	echo "Don't forget to execute: make_services"
clean:
	rm -Rf ${NAME}.service
	
