/*  -*- Mode: C;  -*-
 * File: file_.c
 * Author: Chu-Cheow Lim and Stephen M. Omohundro
 * Copyright (C) International Computer Science Institute, 1990, 1991, 1992, 1993 
 *n
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.
 * 
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 *          Oscar Bosman (oscar@csis.dit.csiro.au)
 * 
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION:  C functions to support file.sa.
 **
 ** HISTORY:
 ** Last edited: Oct 29 23:30 1993 (hws)
 **  Oct 29 23:29 1993 (hws): undone, that change does not work with cross compilation.
 **  Aug 23 12:30 1993 (oscar): ANSI C v. K&R unsigned int format handled in C code
 **  Apr 22 11:56 1993 (hws): added unsigned int output
 ** Created: Thu Apr 22 11:53:54 1993 (hws)
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 */

#include "all_.h"
#include <stdio.h>

ptr stdin_() {return ((ptr)stdin);}
ptr stdout_() {return ((ptr)stdout);}
ptr stderr_() {return ((ptr)stderr);}

static int scanf_val=1;		/* What the last scanf returned. */
/* Return and clear scanf_val. */
int scanf_val_() {int sv=scanf_val; scanf_val=1; return(sv);}

/* A function version of the macro getc. */
int get_ci_(fp) FILE *fp;{return(getc(fp));}

/* Check if eof has been read. */
bool check_eof_(fp) FILE *fp;
{if (feof(fp)) {return((bool)1);} return((bool)0);}

/* Read an int from the file fp. */
int fscanfi_(fp) FILE *fp;
{int i; fscanf(fp, "%d", &i); return (i);}
/* Since the resetting of "scanf_val" appears not to be working, 
we ignore it for now.*/
/*{int i; scanf_val=fscanf(fp, "%d", &i); return (i);} */

/* Read a double from the file fp. */
double fscanfd_(fp) FILE *fp;
{double d; fscanf(fp, "%lf", &d); return (d);}
/* Since the resetting of "scanf_val" appears not to be working, 
we ignore it for now.*/
/*{double d; scanf_val=fscanf(fp, "%lf", &d); return (d);} */

/* Print a char, int, string, or double onto file. */
fprintfi_(fp,in) FILE *fp; int in; {fprintf(fp,"%d",in);}
fprintfs_(fp,st) FILE *fp; char *st; {fprintf(fp,"%s",st);}
fprintfd_(fp,dou) FILE *fp; double dou; {fprintf(fp,"%lf",dou);}

/* Open a file for reading, writing, or appending. */
ptr fopenr_(s) char *s; {return((ptr)fopen(s,"r"));} 
ptr fopenw_(s) char *s; {return((ptr)fopen(s,"w"));} 
ptr fopena_(s) char *s; {return((ptr)fopen(s,"a"));} 

/* Open pipes for reading and writing. */
ptr popenr_(c) char *c; {return((ptr)popen(c,"r"));}
ptr popenw_(c) char *c; {return((ptr)popen(c,"w"));}

