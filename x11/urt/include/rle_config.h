/* rle_config.h
 * 
 * Automatically generated by make-config-h script.
 * DO NOT EDIT THIS FILE.
 * Edit include/makefile.src and the configuration file instead.
 */

#define X11 X11
#define GIF GIF
#define GRAYFILES GRAYFILES
#define MACPAINT MACPAINT
#define PBMPLUS PBMPLUS
#define POSTSCRIPT POSTSCRIPT
#define TARGA TARGA
#define TIFF TIFF
#define ALL_MAN ALL_MAN
#define CONST_DECL CONST_DECL
#define NO_MAKE_MAKEFILE NO_MAKE_MAKEFILE
#define SYS_TIME_H SYS_TIME_H
#define USE_PROTOTYPES USE_PROTOTYPES
#define USE_RANDOM USE_RANDOM
#define USE_STDARG USE_STDARG
#define USE_STDLIB_H USE_STDLIB_H
#define VOID_STAR VOID_STAR
/***************** From rle_config.tlr *****************/

/* CONST_DECL must be defined as 'const' or nothing. */
#ifdef CONST_DECL
#undef CONST_DECL
#define CONST_DECL const

#else
#define CONST_DECL

#endif

/* A define for getx11. */
#ifndef XLIBINT_H
#define XLIBINT_H_NOT_AVAILABLE
#endif
