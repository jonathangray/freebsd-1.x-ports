/* + Sather to C interface code generated Sun Nov 24 16:18:02 1991 from ccob.
--+
--+ preserve-comment=t.
--+ sather-c-type-alist=
--+   ((INT . int) (REAL . float) (DOUBLE . double) 
--+    (CHAR . char) (BOOL . char) (CKEY . int) 
--+    (CHAN . int) (CARRAY . "ptr *") (CSTRAY . "ptr *") 
--+    (OTHERWISE . ptr) )
--+
--+ The code consists of three parts.
--+ ccob.sa contains Sather and C classes importing from C.
--+ ccob.c contains the corresponding C functions to go with
--+ the Sather interpreter.
--+ ccob.macros contains macros to be included in the
--+ .sather file. Use (include) ccob.macros there.
--+ If you refer to C structures, the ccob.c file must always
--+ be included, with the macros file, too.
--+----------------------------------------------------------------------------
 */

#include <ccob.h> /* This file should contain all relevant includes. */

/*  PRESERVED FILE HEADER  */
/* 
-- -*- Mode: Sather;  -*-
-- File: ccob.sac
-- Author: Heinz Schmidt (hws@ICSI.Berkeley.EDU)
-- Copyright (C) International Computer Science Institute, 1991
--*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--* FUNCTION:  COMMON C OBJECTS. A foreign function ADT in support of 
--*            COB classes.
--* 
--* NOTE: The .sac version of this code cannot be used directly with the 
--*   Sather compiler. The file contains foreign abstract type definitions. 
--*   to be converted to Sather and C code.
--*
--* RELATED FILES: cob.sa, cob.c
--*
--* HISTORY:
 ** Last edited: Nov 24 16:18 1991 (hws)
--* Created: Mon Dec 17 08:37:25 1990
--*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */
/*  END FILE HEADER  */

/* 
--*
--* From Sather runtime: Strings
--*
 */
ptr str_ptr(s) 
ptr s;
{
  return((ptr)(str_ptr_(s)));
 };

/* 
--*  
--* Instances
--*
 */
int ob_base_size(typ) 
int typ;
{
  return((int)(safe_ob_base_size_(typ,"C::ob_base_size access error.")));
 };

/* 
--*  
--* Sather INTs and COBs -- the macro versions cast only
--*
 */
ptr to_cob(ob) 
ptr ob;
{
  return((ptr)(ob));
 };
int to_int(i) 
ptr i;
{
  return((int)(i));
 };
ptr from_int(i) 
int i;
{
  return((ptr)(i));
 };

/* 
--*
--*  C strings
--*
-- We rely on Sather runtime fns str_ptr_ and makestr_
 */
ptr to_cstr(s) 
ptr s;
{
  return((ptr)(strdup_((ptr)str_ptr_(s))));
 };
ptr to_str(s) 
ptr s;
{
  return((ptr)(makestr_(s)));
 };

/* 
-- consider using to_str and OUT instead.
 */
void print_cstr(s) 
ptr s;
{
  printf(s);
 };

/* 
-- printing
 */
void print_cstray(s) 
ptr * s;
{
    int i;  for (i = 0;s[i] != 0; i++)    printf("%s\n",s[i]);
 };

/* 
--*
--* C arrays
--*
 */
ptr * create_carray(len) 
int len;
{
  return((ptr *)(malloc(4*len)));
 };
ptr carray_get(array,index) 
ptr * array;
int index;
{
  return((ptr)(array[index]));
 };

void carray_set(array,index,val) 
ptr * array;
int index;
ptr val;
{
  array[index] = val;
 };
