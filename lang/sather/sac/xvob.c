/* + Sather to C interface code generated Thu May 23 14:10:13 1991 from xvob.
--+
--+ preserve-comment=t.
--+ sather-c-type-alist=
--+   ((INT . int) (REAL . float) (DOUBLE . double) 
--+    (CHAR . char) (BOOL . char) (CKEY . int) 
--+    (CHAN . int) (CARRAY . "ptr *") (CSTRAY . "ptr *") 
--+    (OTHERWISE . ptr) )
--+
--+ The code consists of three parts.
--+ xvob.sa contains Sather and C classes importing from C.
--+ xvob.c contains the corresponding C functions to go with
--+ the Sather interpreter.
--+ xvob.macros contains macros to be included in the
--+ .sather file. Use (include) xvob.macros there.
--+ If you refer to C structures, the xvob.c file must always
--+ be included, with the macros file, too.
--+----------------------------------------------------------------------------
 */

#include <xvob.h> /* This file should contain all relevant includes. */

/*  PRESERVED FILE HEADER  */
/* 
-- -*- Mode: Sather; -*-
-- File: xvob.sa
-- Author: Heinz Schmidt (hws@icsi.berkeley.edu)
-- Created: Mon Dec 17 08:37:25 1990
-- Copyright (C) 1990, International Computer Science Institute
--*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--* FUNCTION:  Sather to XView interface. 
--*            Some callbacks receive arguments that they are expected to 
--*            update and use in calls to Xview or to return updated.
--*            Many functions want to be macros in the optimizing code.
--*
--* CLASSES: C_RECT
--* 
--* RELATED PACKAGES: cob.sa, ccob.sa
--*
--*  [XvPM2] O'Reilly: XView Programming Manual, Version 2.0
--*          X Window System Guide, Vol. 7
--*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */
/*  END FILE HEADER  */

/* 
--*
--* FOREIGN STRUCTURES
--* 
-- Points and rectangles are used all over the place, for instance when we 
-- are called back to repaint parts of the window; or to define a clipping 
-- region to Xview. 
 */

/*
 * C ADT for rect structure to go with Sather interpreter.
 */

     /* -- an XView rectangle. */
   
 /* Constructor */
struct rect *create_c_rect(left,right,width,height)
int left;
int right;
int width;
int height;
{ struct rect *cob;
  cob = (struct rect *) malloc(sizeof(*cob));
  cob->r_left = left;
  cob->r_right = right;
  cob->r_width = width;
  cob->r_height = height;
  return (cob);
};

 /* Readers */
int c_rect_left(cob) struct rect *cob; { return ((int)(cob->r_left));};
int c_rect_right(cob) struct rect *cob; { return ((int)(cob->r_right));};
int c_rect_width(cob) struct rect *cob; { return ((int)(cob->r_width));};
int c_rect_height(cob) struct rect *cob; { return ((int)(cob->r_height));};

/* Writers */
void set_c_rect_left(cob,x) struct rect *cob; int x; { cob->r_left = x;};
void set_c_rect_right(cob,x) struct rect *cob; int x; { cob->r_right = x;};
void set_c_rect_width(cob,x) struct rect *cob; int x; { cob->r_width = x;};
void set_c_rect_height(cob,x) struct rect *cob; int x; { cob->r_height = x;};



int xv_create20(a,b,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) 
     /* -- xv_create, more parameters for objects that want many init 
     -- properties or die with segmentation faults when touched the first time. */
   
int a;
int b;
ptr b;
ptr c;
ptr d;
ptr e;
ptr f;
ptr g;
ptr h;
ptr i;
ptr j;
ptr k;
ptr l;
ptr m;
ptr n;
ptr o;
ptr p;
ptr q;
ptr r;
ptr s;
ptr t;
{
  return((int)(xv_create(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)));
 };

void sxv_initialize(argc,argv) 
     /* -- initializes XView and Sather-XView package.
     -- argc,argv can be used to pass on command line arguments.
     -- xv_init is called and read .Xdefaults if present. */
   
int argc;
ptr * argv;
{
  xv_init(XV_INIT_ARGS,argc,argv,NULL);
 };
