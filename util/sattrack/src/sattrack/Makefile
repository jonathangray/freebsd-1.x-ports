#
# makefile for sattrack package
#

LDADD+= -lmsun   
CFLAGS+=  -pipe -I../include
BINDIR = /usr/local/sattrack
MANDIR = /usr/local/man
MAN1 = 

PROG = sattrack
SRCS = calendar.c getUnixTime.c readnorad.c satcalc.c satcity.c satdate.c \
       satdisp.c satread.c sattrack.c 


.include <bsd.prog.mk>
