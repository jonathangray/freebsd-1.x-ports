#
#	Fig2dev Driver Library Makefile
#
.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<
INCLUDES = -I..
CFLAGS = -g $(INCLUDES)
#CFLAGS =  -O $(INCLUDES)

OBJ =  genbox.o genepic.o genlatex.o genpic.o genpictex.o genps.o\
	genpstex.o gentextyl.o gentpic.o
LIB = lib$(TARGET_ARCH).a

$(LIB) : $(OBJ)
	ar rc $(LIB) $(OBJ)
	ranlib $(LIB)

$(OBJ) : ../object.h ../fig2dev.h 

#########
tidy: 
	rm -f *.o

clean :
	rm -f *.o core lib*.a

