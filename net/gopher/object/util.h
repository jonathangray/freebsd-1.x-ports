/********************************************************************
 * lindner
 * 3.2
 * 1993/03/18 22:28:46
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/util.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: util.h
 * Header file for routines in util.c
 *********************************************************************
 * Revision History:
 * util.h,v
 * Revision 3.2  1993/03/18  22:28:46  lindner
 * Updated protos
 *
 * Revision 3.1.1.1  1993/02/11  18:03:06  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1992/12/21  19:43:15  lindner
 * Added prototype for skip_whitespace
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


/*
 * Definitions of stuff in util.c
 */
int readn(/*fd, ptr, nbytes*/);
int writen(/*fd, ptr, nbytes*/);
int    writestring(/*fd, stringptr*/);
int    readline(/*fd, ptr, maxlen*/);
int    readfield(/*fd, ptr, maxlen*/);
int    sreadword(/*input, output, maxlen*/);
void   ZapCRLF(/*char * */);
char   from_hex(/* char */);
char   *to_hex(/* char */);
void   Fromhexstr( /* char, char */);
void   Tohexstr(/* char *, char * */);
void   Hexall(/* char *, char * */);
char   *strcasestr(/* char *, char * */);
char   *skip_whitespace(/* char * */);
