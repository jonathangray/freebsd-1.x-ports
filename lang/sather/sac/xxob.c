/* + Sather to C interface code generated Fri May 24 08:25:07 1991 from xxob.
--+
--+ preserve-comment=t.
--+ sather-c-type-alist=
--+   ((INT . int) (REAL . float) (DOUBLE . double) 
--+    (CHAR . char) (BOOL . char) (CKEY . int) 
--+    (CHAN . int) (CARRAY . "ptr *") (CSTRAY . "ptr *") 
--+    (OTHERWISE . ptr) )
--+
--+ The code consists of three parts.
--+ xxob.sa contains Sather and C classes importing from C.
--+ xxob.c contains the corresponding C functions to go with
--+ the Sather interpreter.
--+ xxob.macros contains macros to be included in the
--+ .sather file. Use (include) xxob.macros there.
--+ If you refer to C structures, the xxob.c file must always
--+ be included, with the macros file, too.
--+----------------------------------------------------------------------------
 */

#include <xxob.h> /* This file should contain all relevant includes. */

/*  PRESERVED FILE HEADER  */
/* 
--* Last edited: Apr 14 13:01 1991 (hws)
-- -*- Mode: Sather; -*-
-- File: xvob.sa
-- Author: Heinz Schmidt (hws@icsi.berkeley.edu)
-- Created: Mon Dec 17 08:37:25 1990
-- Copyright (C) 1990, International Computer Science Institute
--*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--* FUNCTION:  Sather2C interface for accessing Xview data.
--*            Some callbacks receive arguments that they are expected to 
--*            update and use in calls to Xview or to return updated.
--* 
--* NOTE: This is a high-level specification of a foreign function interface
--*   for Sather. / separates corresponding Sather and C names.
--*
--* CLASSES: C_RECT
--* 
--* RELATED PACKAGES: cob.sa, xvaux.sa
--*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */
/*  END FILE HEADER  */

/* 
-- FOREIGN RECT.
-- Points and rectangles are used all over the place, for instance when we 
-- are called back to repaint parts of the window; or to define a clipping 
-- region to the Xview. 
 */

/*
 * C ADT for rect structure to go with Sather interpreter.
 */

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



/* 
--
-- Subst C functions
--
 */
ptr carray_get(a,n) 
ptr * a;
int n;
{
  return((ptr)(a[n]));
 };

void carray_set(a,n,v) 
ptr * a;
int n;
ptr v;
{
  a[n] =v;
 };
