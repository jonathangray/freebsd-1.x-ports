#	@(#)Makefile	5.7 (Berkeley) 6/27/91
LIBDIR= /usr/local/lib
NOPROFILE= no

SHLIB_MAJOR= 1
SHLIB_MINOR= 0

# BLAS sources
LIB=cor
#NOPROFILE=1
SRCS =	caxpy.f cblas1.f cblas2.f ccopy.f cdotc.f cdotu.f cgbmv.f cgemv.f \
	cgerc.f cgeru.f chbmv.f chemv.f cher.f cher2.f chpmv.f chpr.f \
	chpr2.f cmach.f crotg.f cscal.f csrot.f csscal.f cswap.f ctbmv.f \
	ctbsv.f ctpmv.f ctpsv.f ctrmv.f ctrsv.f d1mach.f dasum.f daxpy.f \
	dblas1.f dblas2.f dcopy.f ddot.f dgbmv.f dgemv.f dger.f dmach.f \
	dnrm2.f drot.f drotg.f dsbmv.f dscal.f dspmv.f dspr.f dspr2.f \
	dswap.f dsymv.f dsyr.f dsyr2.f dtbmv.f dtbsv.f dtpmv.f dtpsv.f \
	dtrmv.f dtrsv.f esblas2.f esgbmv.f esgemv.f esger.f essbmv.f \
	esspmv.f esspr.f esspr2.f essymv.f essyr.f essyr2.f estbmv.f \
	estbsv.f estpmv.f estpsv.f estrmv.f estrsv.f fdump.f i1mach.f \
	icamax.f idamax.f isamax.f lsame.f r1mach.f sasum.f saxpy.f \
	sblas1.f sblas2.f scasum.f scnrm2.f scopy.f sdot.f sgbmv.f sgemv.f \
	sger.f smach.f snrm2.f srot.f srotg.f ssbmv.f sscal.f sspmv.f \
	sspr.f sspr2.f sswap.f ssymv.f ssyr.f ssyr2.f stbmv.f stbsv.f \
	stpmv.f stpsv.f strmv.f strsv.f xerbla.f
CLEANFILES+= *.c

package:	install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST blas_bin

.include <bsd.lib.mk>
