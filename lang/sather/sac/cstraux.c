/*  -*- Mode: C;  -*-
 * File: cob.c
 * Author: Heinz Schmidt (hws@ICSI.Berkeley.EDU)
 * Copyright (C) International Computer Science Institute, 1990, 1991
 *
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION: Safe string data sharing between C and Sather for passing 
 **           data back and forth for the COB related types.
 **           Copy them using malloc so Saher GC does not interfere with
 **           remote C functions.
 ** 
 ** SUMMARY:  to_c_str, to_c_strar, print_c_str, print_c_strar,
 **           to_c_array
 **
 ** RELATED FILES: sac/cob.sa, sac/ccob.sa
 **          
 ** HISTORY: 
 ** Last edited: May 29 14:14 1991 (hws)
 **  Sep  1 15:46 1993 (oscar): use our (GC safe) strdup_ instead of strdup.
 **  May 29 10:05 1991 (hws): adapted file names.
 ** Created: Thu Oct 18 14:35:06 1990 (hws)
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include <string.h>
#include <stdio.h>
#include "all_.h"
 
/* uncomment for C tst below; but use include above to get str_ptr_ of Sather. 
#define SAT_STROFF 8                            
char *str_ptr_(s)
char *s;
{
  return ((s) + SAT_STROFF);
}
*/

void print_c_str(s)                                  /* only used for poking around in C strs */
char *s;                                             /* use STR::from_c_str in Sather */
{ 
  printf(s);
};

ptr from_c_str(s)
char *s;
{
  return(makestr_(s));                               /* a function in the Sather runtime */
};

char *to_c_str(s)                                    /* pass a copy of sather string to C */
char *s;
{ 
  return (strdup_(str_ptr_(s))); 
};

void print_c_strar(s)
char *s[];                  
{
  int i;
  for (i = 0; s[i] != NULL; i++) 
    printf("%s\n",s[i]);
}
 
char *make_int_array(len)
int len;
{
  char *newarr = (char *)malloc(4*len);
  return newarr;
};

void int_array_set(arr,n,i)
int n,i,arr[];
{
  arr[n] = i;
  if ( i == 0 ) print_c_strar(arr[1]);
};

#define TOKBUFFLEN 64

char *strtok_cstray(s1,s2)                                /* split the corresponding c_string up */
char *s1;                                              /* used for instance to pass argv info */
char *s2;
{ char *st1 = to_c_str(s1);
  char *st2 = to_c_str(s2);                                         
  char *tokarr[TOKBUFFLEN];
  char *nxtok;
  char *savearr;
  register int i;
  i = 0;                         
  nxtok = strtok(st1,st2);
  tokarr[i] = nxtok; 
  while ( nxtok != NULL && i < TOKBUFFLEN - 1) 
    { 
      i++;
      nxtok = strtok(NULL,st2); 
      tokarr[i] = nxtok; 
    };
  if ( i == TOKBUFFLEN - 1 && nxtok != NULL)            /* hit the limit */
    { 
      fprintf(stderr,"Sather cstraux.c'to_c_strar' ERROR: token buffer length %d exceeded.\n",
	      TOKBUFFLEN );
      tokarr[i] = NULL;                                 /* terminate array */
    };  
  i = (i+1)*4;
  savearr = (char*) malloc(i);                          /* save array */
  memcpy(savearr,tokarr,i);
  return (savearr);
}

/* TEST 

char *strange = "           a b c d e f g h i j k l m n o p q r s t u ";
char *sep     = "          ";
void
main(argc,argv)
int argc;
char *argv[];
{ 
  char *array[10];
  array[0] = "alpha";
  array[1] = "beta";
  array[2] = "gamma";
  array[3] = NULL;
  printf("ARGV: \n"); print_c_strar(argv); 
  printf("ARRAY: \n"); 
  print_c_strar(array);
  printf("CSTR: \n"); 
  print_c_strar((char *)
		to_c_strar(strange,sep));  
}

*/
