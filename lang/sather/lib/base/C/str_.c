/* * Last edited: May 29 23:26 1991 (bilmes) */
/* File: sather/lib/base/C/str_.c
   Author: Chu-Cheow Lim and Stephen M. Omohundro
   Created: Thu Sep 20 14:41:17 1990
   Copyright (C) International Computer Science Institute, 1990

   COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
   and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
   LICENSE contained in the file: "sather/doc/license.txt" of the Sather
   distribution. The license is also available from ICSI, 1947 Center
   St., Suite 600, Berkeley CA 94704, USA.

   C functions to support str.sa.
*/

#include "all_.h"
#include <stdio.h>

/* Insert the digits of the integer `i' into string `s' at char `l'. */
sprintfi(s,i,l)
     char *s; int i,l;
{sprintf(s+l,"%d",i);}

/* Insert the digits of the double (or real) `d' into string `s' with 
   precision `p' at char `l'. If `p' is zero, insert integer part. */
sprintfd(s,d,p,l)
     char *s; double d; int p,l;
{if (p==0) sprintf(s+l,"%d",(int)d); else sprintf(s+l,"%.*f",p,d);}

/* The integer represented at the head of the string `sp'. */
int sscanfi(sp,l)
     char *sp; int l;
{int i; sscanf(sp+l, "%d", &i); return (i);} 

/* The double represented at the head of the string `sp'. */
double sscanfd(sp,l)
     char *sp; int l;
{double d; sscanf(sp+l, "%lf", &d); return (d);} 
