/********************************************************************
 * lindner
 * 3.4
 * 1993/07/30 19:10:09
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/openers.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: openers.c
 * Secure file access routines.
 *********************************************************************
 * Revision History:
 * openers.h,v
 * Revision 3.4  1993/07/30  19:10:09  lindner
 * Fix for ultrix
 *
 * Revision 3.3  1993/07/23  03:17:10  lindner
 * added stat.h
 *
 * Revision 3.2  1993/03/24  20:29:45  lindner
 * Added Dirent.h, needed for those darn directory routines.
 *
 * Revision 3.1.1.1  1993/02/11  18:02:52  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

/*
 * Declarations for openers.c routines.
 */

#include <stdio.h>
#include <sys/types.h>
#include "Dirent.h"
#include <sys/stat.h>

/* restricted versions */
extern	int	ropen();
extern	FILE	*rfopen();
extern  FILE    *rfopenz();
extern	int	rstat();
extern	DIR	*ropendir();
extern	int	rchdir();

/* unrestricted versions */
extern	int	uopen();
extern	FILE	*ufopen();
extern	int	ustat();
extern	DIR	*uopendir();
extern	int	uchdir();
