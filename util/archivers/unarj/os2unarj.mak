# Makefile for unarj.exe for OS/2 Microsoft C 6.0

CC=cl -AS -W4 -Ze -Oaxz -F 1000 -Lp -D_OS2

all:	      unarj.exe

unarj.obj:    unarj.c	unarj.h

decode.obj:   decode.c	unarj.h

environ.obj:  environ.c unarj.h

unarj.exe:    unarj.obj decode.obj environ.obj unarj.def
     $(CC)    unarj decode environ unarj.def

# end of makefile
