#	Fig2dev : General Fig code translation program
#
#	Micah Beck
#	Modified from f2ps
#

#
# to include drivers for the following languages, 
# add the corresponding symbol to the DRIVERS list
#
# DRIVERS = -DEPIC -DLATEX -DPIC -DPICTEX -DPS -DPSTEX -DTEXTYL -DTPIC
#
DRIVERS = -DEPIC -DLATEX -DPIC -DPICTEX -DPS -DPSTEX -DTEXTYL -DTPIC
.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) $(DRIVERS) -c $<


FIGOBJ = arrow.o bound.o free.o read.o read1_3.o latex_line.o
OBJ = fig2dev.o getopt.o psfonts.o $(FIGOBJ)

LIB = lib$(TARGET_ARCH).a
LIBS = dev/$(LIB)

CFLAGS =  -g
#CFLAGS = -O

TARGET = fig2dev$(TARGET_ARCH)

$(TARGET): $(OBJ) $(LIBS) 
	$(CC) $(OBJ) $(LIBS) -lm -o $(TARGET)

$(LIBS): always
	(cd dev; make CC="$(CC)" CFLAGS="$(CFLAGS)")

$(OBJ): object.h fig2dev.h drivers.h


#########
always:

tidy: 
	rm -f *.o
	(cd dev; make tidy)

clean : 
	rm -f *.o fig2dev fig2dev-* core
	(cd dev; make clean)
