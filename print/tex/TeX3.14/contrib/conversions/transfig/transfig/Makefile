#	TransFig : Fig code translation Makefile creation program
#
#	Micah Beck
#

OBJ = transfig.o txfile.o mkfile.o sys.o ../fig2dev/getopt.c

#CFLAGS =  -g
CFLAGS = -O

TARGET = transfig$(TARGET_ARCH)

$(TARGET): $(OBJ)
	$(CC) $(OBJ) -lm -o $(TARGET)

$(OBJ): transfig.h

#########
tidy: 
	rm -f *.o

clean: 
	rm -f *.o transfig transfig-* core
