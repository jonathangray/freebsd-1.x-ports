### MPW Make file for the librle library
# Description file for librle library. ($rlib)

# DEST is where to install the library for programs outside the Raster Toolkit.
# RI is the include directory for RLE files.
RI = ::include:

COptions	=  -d macintosh_os -d NO_OPEN_PIPES -d USE_STDLIB_H 6
		-d USE_STDARG -d USE_PROTOTYPES -d USE_BSTRING -i "{RI}"

OBJS = bstring.c.o rle_addhist.c.o rle_putcom.c.o rle_row_alc.c.o 6
	buildmap.c.o rle_getcom.c.o rle_putraw.c.o Runput.c.o 6
	colorquant.c.o rle_getraw.c.o rle_putrow.c.o scanargs.c.o 6
	dither.c.o rle_getrow.c.o rle_raw_alc.c.o sVsetlinebuf.c.o  6
	float_to_exp.c.o rle_global.c.o rle_rawrow.c.o vaxshort.c.o  6
	getopt.c.o  rle_open_f.c.o rle_getskip.c.o

librle	D	librle.o

librle.o D {objs}
	echo "Building library"
	Lib {objs} -o librle.o

.c.o	D	.c
	echo {Default}
	C {Default}.c {COptions}  


# Dependencies on .h files:
# DO NOT DELETE THIS LINE
Runput.o D {RI}rle.h {RI}rle_code.h {RI}rle_put.h
buildmap.o D {RI}rle.h
float_to_exp.o D {RI}rle.h
rle_getcom.o D {RI}rle.h
rle_getraw.o D {RI}rle.h {RI}rle_raw.h
rle_getrow.o D {RI}rle.h {RI}rle_code.h
rle_global.o D {RI}rle.h {RI}rle_put.h
rle_putcom.o D {RI}rle.h
rle_putraw.o D {RI}rle.h {RI}rle_put.h {RI}rle_raw.h
rle_putrow.o D {RI}rle.h {RI}rle_put.h
rle_raw_alc.o D {RI}rle.h {RI}rle_raw.h
rle_rawrow.o D {RI}rle.h {RI}rle_raw.h
rle_row_alc.o D {RI}rle.h
rle_getskip.o D {RI}rle.h {RI}rle_raw.h
