CFLAGS=
LFLAGS=

all: fontmag fe genwidth pxltorst vertorst

fontmag: fontmag.o fe_read.o fe_write.o out.o
	cc $(LFLAGS) -o fontmag fontmag.o fe_read.o fe_write.o out.o

fe: fe_main.o fe_read.o
	cc $(LFLAGS) -o fe fe_main.o fe_read.o

genwidth: genwidth.o fe_read.o
	cc $(LFLAGS) -o genwidth genwidth.o fe_read.o

pxltorst: pxltorst.o out.o
	cc $(LFLAGS) -o pxltorst pxltorst.o out.o

vertorst: vertorst.o out.o
	cc $(LFLAGS) -o vertorst vertorst.o out.o

out.o genwidth.o fontmag.o pxltorst.o vertorst.o: rst.h

fe_read.o fe_write.o fe_main.o: rst.h

clean:
	rm -f *.o fontmag genwidth pxltorst vertorst fe
