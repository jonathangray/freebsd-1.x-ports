/* $Header: /a/cvs/386BSD/ports/shell/tcsh/mi.varargs.h,v 1.1.1.2 1994/07/05 20:39:55 ache Exp $ */
/*
 * mi.varargs.h: Correct varargs for minix
 */
#ifndef _h_mi_varargs
#define _h_mi_varargs

typedef char *va_list;

#define  va_dcl		int va_alist;
#define  va_start(p)	(p) = (va_list) &va_alist;
#define  va_arg(p,type)	( (type *) ((p)+=sizeof(type)) )[-1]
#define  va_end(p)

#endif /* _h_mi_varargs */
