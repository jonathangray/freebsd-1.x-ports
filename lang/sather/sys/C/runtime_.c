/*  -*- Mode: C;  -*-
 * File: runtime_.c
 * Author: Author: Stephen M. Omohundro
 * Copyright (C) International Computer Science Institute, 1991, 1992, 1993 
 *
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
 ** FUNCTION: Runtime code for every Sather program.
 **
 ** RCS: $Id: runtime_.c,v 1.1 1994/02/12 03:23:35 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 19:19 1993 (hws)
 **  Sep  1 10:13 1993 (oscar): move exception handler error messages here
 **  Aug 30 15:45 1993 (oscar): add error_exit (was c_macro in compiler/.sather)
 **  May 28 04:24 1993 (hws): don't use uninitialized tracestk pointer
 **  Apr 17 08:29 1993 (hws): ANSI C err_quit: add "void"
 **  Feb 23 11:49 1993 (hws): RT_ERROR raises exception
 **  (bilmes): Added extend3_(), extend4_(),new3_(),new4()_,deep_copy_()
 **  16 Oct 90 (clim): New versions of runtime routines with 
 **    a. Void object check
 **    b. Array bound check 
 **  16 Sept 90 (clim): Runtime routines with GC:
 **    new_, new1_, new2_, new3_, new4_, extend1_, extend2_, extend3_, 
 **    extend4_, copy_, deep_copy_, makestr_, str_ptr_, atomic_p_, type_ 
 ** Created: Wed May 22 14:12:40 1991
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
 */

/* Most of the runtime routines with GC will take an extra argument
   saying whether the object dealt with contains pointer. */

#include "all_.h"
#include <memory.h>
#include <string.h>
#include <stdio.h>

#include "macros_.h"

#define RT_ERROR(code) eh_signal_sux_(code)

extern ptr eh_signal_sux_();

/* NOTE: The declaration of "feat_table_", like "attr_table_", is generated
   by the compiler. */ 
extern int* feat_table_[];
extern int* attr_table_[];
extern int* des_table_[];  /* Whether a class is a descendent of another */

extern int num_classes_;
extern int dispatch_table_size_;
extern int dispatch_table_[];

/* Initialized as C strings */
extern char* prog_name_;
extern char* prog_dir_;

/* Largest name index in current program */
extern int max_name_index_;

int OB_ici;
int ARRAY_ici;
int ARRAY2_ici;
int ARRAY3_ici;
int ARRAY4_ici;
int BOOL_ici;
int C_ici;
int CHAR_ici;
int DOUBLE_ici;
int ERR_ici;
int FILE_ici;
int IN_ici;
int INT_ici;
int OUT_ici;
int REAL_ici;
int SELF_TYPE_ici;
int STR_ici;
int STR_CURS_ici;
int STR_CURSOR_ici;
int STR_SCAN_ici;
int SYS_ici;
int FOB_ici;
int SUX_ici;
int UNDEFINE_ici;
int MONITOR_ici;
int MONITOR0_ici;
int LAST_PREDEF_ici;

/*shared*/ int HASH_TAB_n_;
/*shared*/ ptr HASH_TAB_arr_;
/*shared*/ int HASH_TAB_mask_;
/*shared*/ int HASH_TAB_entries_;

char GC_is_on_;

/* Initializes runtime information */
rt_init_()
{
  /* For the moment, we only need these classes. The choice of numbering
   is at random and must be less than 25. */
#ifdef GC_
  GC_init();
#endif
  ARRAY_ici = 1;
  ARRAY2_ici = 2;
  ARRAY3_ici = 3;
  ARRAY4_ici = 4;
  OB_ici = 5;
  C_ici = 6;
  ERR_ici = 7;
  IN_ici = 8;
  OUT_ici = 9;
  SELF_TYPE_ici = 10;
  FILE_ici = 11;
  STR_CURS_ici = 12; /* this should go away soon. */
  STR_CURSOR_ici = 12;
  STR_SCAN_ici = 12;
  SYS_ici = 13;
  CHAR_ici = 14;
  INT_ici = 15;
  BOOL_ici = 16;
  REAL_ici = 17;
  DOUBLE_ici = 18;
  UNDEFINE_ici = 19;
  STR_ici = 20;
  FOB_ici = 21;
  SUX_ici = 22;
  MONITOR_ici = 23;
  MONITOR0_ici = 24;
  LAST_PREDEF_ici = 22;

  HASH_TAB_n_ = 6;
  HASH_TAB_mask_ = (INT15_lshift_(1,HASH_TAB_n_) - 2);
  HASH_TAB_entries_ = 0;
#ifdef GC_
  GC_is_on_ = 1;
#else
  GC_is_on_ = 0;
#endif
  TRACE_BACK_ = EH_TRACE_LIM_;			/* want a trace on failure? */
						/* overwritten by program main if necessary */
}

/* Print error message when out of memory and exit. */
void out_of_mem_(r_name)
ptr r_name;			/* Routine name */
{  
  fprintf(stderr, OUT_OF_MEM_MSG_, r_name);
  RT_ERROR(OUT_OF_MEM_ERR_);
}

/* Print error message and exit. */
void error_exit(s)
ptr s;
{
    fprintf(stderr, s);
    exit(1);
}

/* Error messages from exception handler */
void EH_print_exception_error1(type, obj)
int type, obj;
{
    fprintf(stderr, 
	    "  Sather unhandled exception type: %d, object %d\n", type, obj);
}

void EH_print_exception_error2(type, tp_name, obj, ob_name)
int type;
char *tp_name;
int obj;
char *ob_name;
{
    fprintf(stderr, 
	    "  Sather unhandled exception type: %d (%s), object: %d / %s", 
	    type, tp_name, obj, ob_name);
}

/* Return a pointer to a zero initialized object of given class. */
ptr generic_new_(ici,s1,s2,s3,s4,atomic_p,nargs)
int ici,s1,s2,s3,s4;
int atomic_p;
int nargs;
{
  if (ob_arr_dim_(ici) != nargs) {
    fprintf(stderr, DISP_NEW_MSG_, nargs);
    RT_ERROR(DISP_NEW_ERR_);
  }
  switch (ob_arr_dim_(ici)) {
  case 0: return (new_(ici,atomic_p));
  case 1: return (new1_(ici,s1,atomic_p));
  case 2: return (new2_(ici,s1,s2,atomic_p));
  case 3: return (new3_(ici,s1,s2,s3,atomic_p));
  case 4: return (new4_(ici,s1,s2,s3,s4,atomic_p));
  }
}

/* Return a pointer to a zero initialized object of given non-array class. */
ptr new_(ici, atomic_p)
int ici;
int atomic_p;
{
  ptr res;
  int nbytes;
#ifdef GC_
  if (atomic_p) {
    nbytes=ob_base_size_(ici);
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(ob_base_size_(ici));
  }
#else
  res=(ptr)calloc(ob_base_size_(ici),1);
#endif

#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new_");
#endif
  TYPE_(res)=ici;
  return(res);
}

/* New for ARRAY classes. */
ptr new1_(ici,as1,atomic_p)
int ici,as1;
int atomic_p;
{
  ptr res;
  int nbytes;
#ifdef GC_
  if (atomic_p) {
    nbytes=ob_base_size_(ici)+SI_+as1*ctype_size_(ob_arr_ctype_(ici));
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(ob_base_size_(ici)+SI_+as1*ctype_size_(ob_arr_ctype_(ici)));
  }
#else
  res=(ptr)calloc(ob_base_size_(ici)+SI_+as1*ctype_size_(ob_arr_ctype_(ici)),1);
#endif

#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new1_");
#endif
  TYPE_(res)=ici; 
  (*((int *)(res+ob_base_size_(ici))))=as1; /* fill in asize */
  return(res);
}

/* New for ARRAY2 classes. Access through stored offsets instead of mult. */
ptr new2_(ici,as1,as2,atomic_p)
     int ici,as1,as2;
     int atomic_p;
{
  ptr res;
  int *tp;
  int i,csz,bs;
  int nbytes;

  csz=ctype_size_(ob_arr_ctype_(ici));
  bs=ob_base_size_(ici);
#ifdef GC_
  if (atomic_p) {
    nbytes=bs+2*SI_+SI_*as1+as1*as2*csz;
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(bs+2*SI_+SI_*as1+as1*as2*csz);
  }
#else
  res=(ptr)calloc(bs+2*SI_+SI_*as1+as1*as2*csz,1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new2_");
#endif
  TYPE_(res)=ici;
  tp=(int*)(res+bs);	/* ptr to asize1 */
  *tp++=as1;		/* fill in asize1 */
  *tp++=as2;	        /* fill in asize2 */
  for(i=0; i<as1; i++)		/* fill in the offsets */
    *tp++=bs+2*SI_+SI_*as1+csz*i*as2;
  return(res);
}

/* New for ARRAY3 classes. Do access by multiplication. */
ptr new3_(ici,as1,as2,as3,atomic_p)
     int ici,as1,as2,as3;
     int atomic_p;
{
  ptr res;
  int bs;
  int *tp;
  int i;
  int csz;
  int nbytes;

  bs=ob_base_size_(ici);
  csz = ctype_size_(ob_arr_ctype_(ici));
#ifdef GC_
  nbytes=bs+3*SI_+as1*sizeof(int)+as1*as2*sizeof(int)+as1*as2*as3*csz;
  if (atomic_p) {
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(nbytes);
  }
#else
  res =(ptr)calloc(bs+3*SI_+
		   as1*sizeof(int)+
		   as1*as2*sizeof(int)+
		   as1*as2*as3*csz,1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new3_");
#endif
  TYPE_(res)=ici;
  tp=(int*)((char*)res+bs);		/* ptr to asize1 */
  *tp++=as1;		/* fill in asize1 */
  *tp++=as2;	        /* fill in asize2 */
  *tp++=as3;	        /* fill in asize3 */
  for (i=0;i<as1;i++)
    *tp++ = bs+3*SI_+
      as1*sizeof(int)+
	i*as2*sizeof(int);
  for (i=0;i<as1*as2;i++)
      *tp++ = bs+3*SI_+
	as1*sizeof(int)+
	  as1*as2*sizeof(int)+
	    i*csz*as3;
  return(res);
}

/* New for ARRAY4 classes. Do access by multiplication. */
ptr new4_(ici,as1,as2,as3,as4,atomic_p)
     int ici,as1,as2,as3,as4;
     int atomic_p;
{
  ptr res;
  int *tp;
  int bs;
  int i;
  int csz;
  int nbytes;

  bs=ob_base_size_(ici);
  csz = ctype_size_(ob_arr_ctype_(ici));
#ifdef GC_
  nbytes = bs+4*SI_+as1*sizeof(int)+as1*as2*sizeof(int)+
    as1*as2*as3*sizeof(int)+as1*as2*as3*as4*csz;

  if (atomic_p) {
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(nbytes);
  }
#else
   res=(ptr)calloc(bs+4*SI_+
	           as1*sizeof(int)+
		   as1*as2*sizeof(int)+
		   as1*as2*as3*sizeof(int)+
		   as1*as2*as3*as4*csz,1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new4_");
#endif
  TYPE_(res)=ici;
  tp=(int*)((char*)res+bs);		/* ptr to asize1 */
  *tp++=as1;		/* fill in asize1 */
  *tp++=as2;	        /* fill in asize2 */
  *tp++=as3;	        /* fill in asize3 */
  *tp++=as4;            /* fill in asize4 */
  for (i=0;i<as1;i++)
    *tp++ = bs+4*SI_+
      as1*sizeof(int)+
	i*as2*sizeof(int);
  for (i=0;i<as1*as2;i++)
    *tp++ = bs+4*SI_+
      as1*sizeof(int)+
	as1*as2*sizeof(int)+
	    i*as3*sizeof(int);
  for (i=0;i<as1*as2*as3;i++)
    *tp++ = bs+4*SI_+
      as1*sizeof(int)+
	as1*as2*sizeof(int)+
	  as1*as2*as3*sizeof(int)+
	    i*csz*as4;
  return(res);
}

/* Determines if an object contains pointer to other objects */
int atomic_p_(p)
ptr p;
{
  int tp,atn,i;
  int res;

  if (p==NULL) return 0;	/* Conservative guess */
  tp=TYPE_(p); 
  if (ob_arr_dim_(tp)==0) {
    res=1; atn=ob_attr_num_(tp);
    for (i=0; i<atn; i++) {
      if (ob_attr_ctype_(tp,i) == CTYPE_PTR_) {
	res=0; break;
      }
    }
  }
  else {			/* Array objects */
    res=(ob_arr_ctype_(tp) != CTYPE_PTR_);
  }
  return (res);
}

/* Return the size of p in bytes. (p must be non-null) */
int ob_size_(p)
     ptr p;
{
  int tp,bs,as1,as2,as3,as4;

  tp = TYPE_(p); bs=ob_base_size_(tp);
  switch(ob_arr_dim_(tp))
    {
    case 0: 
      return(bs);
    case 1: 
      as1=(*((int *)(p+bs)));
      return(bs+1*SI_+
	     as1*ctype_size_(ob_arr_ctype_(tp)));
    case 2:
      as1=(*((int *)(p+bs)));
      as2=(*((int *)(p+bs+sizeof(int))));
      return(bs+2*SI_+
	     sizeof(int)*as1+
	     as1 * as2 * ctype_size_(ob_arr_ctype_(tp)));
    case 3:
      as1=(*((int *)(p+bs)));
      as2=(*((int *)(p+bs+sizeof(int))));
      as3=(*((int *)(p+bs+2*sizeof(int))));
      return(bs+3*SI_+
	     sizeof(int)*as1+
	     sizeof(int)*as1*as2+
	     as1 *
	     as2 *
	     as3 * ctype_size_(ob_arr_ctype_(tp)));
    case 4:
      as1=(*((int *)(p+bs)));
      as2=(*((int *)(p+bs+sizeof(int))));
      as3=(*((int *)(p+bs+2*sizeof(int))));
      as4=(*((int *)(p+bs+3*sizeof(int))));
      return(bs+4*SI_+
	     sizeof(int)*as1+
	     sizeof(int)*as1*as2+
	     sizeof(int)*as1*as2*as3+
	     as1 *
	     as2 *
	     as3 *
	     as4 * ctype_size_(ob_arr_ctype_(tp)));
    }
}


/* Return an exact copy of p. */
ptr copy_(p,atomic_p)
ptr p;
int atomic_p;
{
  int sz;
  ptr res;

  if (p==NULL) return NULL;
  sz=ob_size_(p);
#ifdef GC_
  if (atomic_p) {
    res=(ptr)gc_malloc_atomic(sz);
    bzero(res,sz);
  }
  else {
    res=(ptr)gc_malloc(sz);
  }
#else
  res=(ptr)calloc(sz,1);
#endif
#ifdef SAFE_RT_
  if (res == NULL) out_of_mem_("copy_");
#endif
  memcpy(res,p,sz);
  return(res);
}

/* Make a copy of an ARRAY, extending the array part to ns if larger. */
ptr extend1_(p,ns,atomic_p)
     ptr p;
     int ns;
     int atomic_p;
{
  ptr res;
  int tp,nsz,osz,bs,asz,os;

  if (p==NULL) return NULL;
  tp=TYPE_(p);
  bs=ob_base_size_(tp);		/* end of object part */
  os=(*((int *)(p+bs)));	/* old array size */
  if (ns<=os) return(p);	/* don't do anything if asked to shrink */
  asz=ctype_size_(ob_arr_ctype_(tp)); /* size of array entries */
  res=new1_(tp,ns,atomic_p);
  osz=bs+SI_+os*asz;		/* old object size in bytes */
  nsz=bs+SI_+ns*asz;		/* new object size in bytes */
  memcpy(res,p,osz);		/* copy as much as is needed */
  (*((int *)(res+bs)))=ns;	/* fill in asize */
  return(res);
}

/* Make a copy of an ARRAY2, extending the array part to ns1,ns2. */
ptr extend2_(p,ns1,ns2,atomic_p)
     ptr p;
     int ns1,ns2;
     int atomic_p;
{
  ptr res,bna,boa;
  int tp,bs,csz,os1,os2,cs1,cs2;
  int i;

  if (p==NULL) return NULL;
  tp=TYPE_(p);
  bs=ob_base_size_(tp);
  os1=(*((int *)(p+bs)));	/* old asize1 */
  os2=(*((int *)(p+bs+SI_)));	/* old asize2 */
  if (os1>=ns1 && os2>=ns2) return(p); /* return old one if asked to shrink */
  csz=ctype_size_(ob_arr_ctype_(tp)); /* size of array entries */
  res=new2_(tp,ns1,ns2,atomic_p);
  memcpy(res,p,bs);		/* just copy the early stuff. */
  (*((int *)(res+bs)))=ns1;	/* fill in asize1 */  
  (*((int *)(res+bs+SI_)))=ns2;	/* fill in asize2 */  
  cs1=(os1<ns1?os1:ns1); /* amounts to copy */ 
  cs2=(os2<ns2?os2:ns2); 
  cs2 *= csz;            /* adjust to cs2 is size in bytes to copy */
  boa=p+bs+2*SI_+SI_*os1;	/* base of old array */
  bna=res+bs+2*SI_+SI_*ns1;	/* base of new array */
  for(i=0;i<cs1; i++)
    memcpy(bna+i*ns2*csz, boa+i*os2*csz, cs2); /* copy the right amount */
  return(res);
}

/* Make a copy of an ARRAY3, extending the array part to ns1,ns2,ns3. */
ptr extend3_(p,ns1,ns2,ns3,atomic_p)
     ptr p;
     int ns1,ns2,ns3;
     int atomic_p;
{
  ptr res,bna,boa;
  int tp,bs,asz,os1,os2,os3,cs1,cs2,cs3;
  int i,j;

  if (p==NULL) return NULL;
  tp=TYPE_(p);
  bs=ob_base_size_(tp);
  os1=(*((int *)(p+bs)));	/* old asize1 */
  os2=(*((int *)(p+bs+1*SI_)));	/* old asize2 */ 
  os3=(*((int *)(p+bs+2*SI_)));	/* old asize3 */
 
  if (os1>=ns1 && os2>=ns2 && os3>=ns3) 
    return(p); /* return old one if asked to shrink */
  asz=ctype_size_(ob_arr_ctype_(tp)); /* size of array entries */
  res=new3_(tp,ns1,ns2,ns3,atomic_p);
  memcpy(res,p,bs);		/* just copy the early stuff. */
  cs1=(os1<ns1?os1:ns1); /* amounts to copy */ 
  cs2=(os2<ns2?os2:ns2);
  cs3=(os3<ns3?os3:ns3); 
  cs3 *= asz;
  boa=p+bs+3*SI_+SI_*os1+SI_*os1*os2;	/* base of old array */
  bna=res+bs+3*SI_+SI_*ns1+SI_*ns1*ns2;	/* base of new array */

  for(i=0;i<cs1; i++)
     for(j=0;j<cs2; j++)
       memcpy(bna+(ns2*i+j)*ns3*asz, boa+(os2*i+j)*os3*asz, cs3); /* copy the right amount */
  return(res);
}

/* Make a copy of an ARRAY4, extending the array part to ns1,ns2,ns3,ns4. */
ptr extend4_(p,ns1,ns2,ns3,ns4,atomic_p)
     ptr p;
     int ns1,ns2,ns3,ns4;
     int atomic_p;
{
  ptr res,bna,boa;
  int tp,bs,asz,os1,os2,os3,os4,cs1,cs2,cs3,cs4;
  int i,j,k;

  if (p==NULL) return NULL;
  tp=TYPE_(p);
  bs=ob_base_size_(tp);
  os1=(*((int *)(p+bs)));	/* old asize1 */
  os2=(*((int *)(p+bs+SI_)));	/* old asize2 */ 
  os3=(*((int *)(p+bs+2*SI_)));	/* old asize3 */
  os4=(*((int *)(p+bs+3*SI_)));	/* old asize4 */
 
  if (os1>=ns1 && os2>=ns2 && os3>=ns3 && os4>=ns4) 
    return(p); /* return old one if asked to shrink */
  asz=ctype_size_(ob_arr_ctype_(tp)); /* size of array entries */
  res=new4_(tp,ns1,ns2,ns3,ns4,atomic_p);
  memcpy(res,p,bs);		/* just copy the early stuff. */
  cs1=(os1<ns1?os1:ns1); /* amounts to copy */ 
  cs2=(os2<ns2?os2:ns2);
  cs3=(os3<ns3?os3:ns3); 
  cs4=(os4<ns4?os4:ns4); 
  cs4 *= asz;
  boa=p+bs+4*SI_+SI_*os1+SI_*os1*os2+SI_*os1*os2*os3;	/* base of old array */
  bna=res+bs+4*SI_+SI_*ns1+SI_*ns1*ns2+SI_*ns1*ns2*ns3;	/* base of new array */

  for(i=0;i<cs1;i++)
    for(j=0;j<cs2;j++)
      for(k=0;k<cs3;k++)
	memcpy(bna+(i*ns2*ns3+j*ns3+k)*ns4*asz, 
	       boa+(i*os2*os3+j*os3+k)*os4*asz, 
		    cs4); /* copy the right amount */
  return(res);
} 

/* Get the dispatch_table_ with name nm in class cls. Error if not found.
   dispatch_table_size_ is defined in main.c along with the table. The first
   14 bits of the key will be the class, the last 18 the name. */
int get_dispatch_(cls,nm)
     int cls, nm;
{
  unsigned int hsh,key;

  key=(((unsigned int)nm)<<14)+cls;
  hsh=(((key*key)%dispatch_table_size_)>>1)<<1;
  while(1)
    {
      if (dispatch_table_[hsh]==0)
	{
	  fprintf(stderr, ILL_DISP_MSG_, cls, nm);
	  RT_ERROR(ILL_DISP_ERR_);
	}
      if (dispatch_table_[hsh]==key) return(dispatch_table_[hsh+1]);
      hsh+=2;
      if (hsh>=dispatch_table_size_) hsh=0;
    }
}

/* This code is used to get the dispatch for "f.expr" where f:$OB.  It is
   similar to "get_dispatch_" except that the routine continues on when the
   dispatch value is missing. */
int ob_get_dispatch_(cls,nm)
     int cls, nm;
{
  unsigned int hsh,key;
  unsigned int firsthsh;

  key=(((unsigned int)nm)<<14)+cls;
  hsh=(((key*key)%dispatch_table_size_)>>1)<<1;
  firsthsh = hsh;
  while(1)
    {
      if (dispatch_table_[hsh]==0)
	{fprintf(stderr, "(Warning) : Unknown dispatch reference ignored, class %d, name ind %d.\n", cls, nm); return 0; }
      if (dispatch_table_[hsh]==key) return(dispatch_table_[hsh+1]);
      hsh+=2;
      if (hsh>=dispatch_table_size_) hsh=0;
      if (firsthsh == hsh) {
	fprintf(stderr, "(Warning) : Unknown dispatch reference ignored, class %d, name ind %d.\n", cls, nm);
	return 0;
      }
    }
}

/* The data structure STR cannot contain the string "aaa" unless we 
   allocate more space.  */
ptr makestr_(str)
ptr str;
{
  int i;
  int bs;
  int len;
  ptr res;
  if (str != 0) {
    len = strlen((char *)str);
  }
  else {
    len = 0;
  }
  res = new1_(STR_ici,len+1,1);
  i = 0;
  bs = ob_base_size_(STR_ici) + SI_; /* offset to where to store actual string */
  while (i < len) {
    res[bs+i] = str[i];
    i++;
  } 
  res[bs+i] = '\0';
  return (res);
}

/* return a pointer to the string portion of 's' which should be a STR object */ 
ptr str_ptr_(s)
ptr s;
{
  return (ptr)(& ((int*)s)[2]);
}

/*======================================================================
  This portion is translated from Sather-generated C code.
  Written by: Jeff Bilmes
  Installed: Chu-Cheow Lim 
  Changes: Heinz Schmidt, some unix require explicit function type "void"
----------------------------------------------------------------------*/ 

/*
#define dcHashGet_ HASH_TAB_hashGet_
#define dcHashInsert_ HASH_TAB_hashInsert_
#define dcHashInit_ HASH_TAB_hashInit_
*/
/* The above macros are in "all_.h", because they constitute part of
   interface to the runtime routines. */

/*----------------------------------------------------------------------*/

/* HASH_TAB.c: Sather class: DEEP_COPY_HASHER */

extern int ptr2key();
#define ptr2key(ptr,mask)  (((unsigned)(ptr)>>2)&mask)

void HASH_TAB_hashInit_();
ptr HASH_TAB_hashGet_();
void HASH_TAB_hashInsert_();


void HASH_TAB_hashInit_(self__ )
ptr self__;
{
   int i__;

   if (HASH_TAB_arr_ == 0) {
     HASH_TAB_arr_ = new1_(1,INT15_lshift_(1,HASH_TAB_n_),0);
   }
   i__ = 0;
   while (1) {
      if ((i__ >= IATT_(HASH_TAB_arr_,4))) break;
      PATT_(HASH_TAB_arr_, 8 + (4 * (i__))) = 0;
      i__ = (i__ + 1);
   }
}

ptr HASH_TAB_hashGet_(self__ ,ob__)
ptr self__;
ptr ob__;
{
   ptr res__ = 0;
   int key__;
   int keyOrig__;

   key__ = ptr2key(ob__,HASH_TAB_mask_);
   keyOrig__ = key__;
   if ((key__ >= IATT_(HASH_TAB_arr_,4))) {
      key__ = 0;
   }
   else {
   }
   while (1) {
      if ((PATT_(HASH_TAB_arr_, 8 + (4 * (key__))) == ob__)) {
         res__ = PATT_(HASH_TAB_arr_, 8 + (4 * ((key__ + 1))));
         return (res__);
      }
      else {
         key__ = (key__ + 2);
         if ((key__ >= IATT_(HASH_TAB_arr_,4))) {
            key__ = 0;
         }
         else {
         }
         if ((key__ == keyOrig__)) {
            res__ = 0;
            return (res__);
         }
         else {
         }
      }
   }
}

void HASH_TAB_hashInsert_(self__ ,ob__,entry__)
ptr self__;
ptr ob__;
ptr entry__;
{
   int key__;
   int i__;
   ptr oldArr__;

   key__ = ptr2key(ob__,HASH_TAB_mask_);
   if ((key__ >= IATT_(HASH_TAB_arr_,4))) {
      key__ = 0;
   }
   else {
   }
   while (1) {
      if ((PATT_(HASH_TAB_arr_, 8 + (4 * (key__))) == 0)) {
         PATT_(HASH_TAB_arr_, 8 + (4 * (key__))) = ob__;
         PATT_(HASH_TAB_arr_, 8 + (4 * ((key__ + 1)))) = entry__;
         HASH_TAB_entries_ = (HASH_TAB_entries_ + 1);
         goto goto_tag_75_;
      }
      else {
         key__ = (key__ + 2);
         if ((key__ >= IATT_(HASH_TAB_arr_,4))) {
            key__ = 0;
         }
         else {
         }
      }
   }
goto_tag_75_: ;
   if (((HASH_TAB_entries_ * 4) > IATT_(HASH_TAB_arr_,4))) {
      HASH_TAB_n_ = (HASH_TAB_n_ + 1);
      i__ = 0;
      HASH_TAB_mask_ = (INT15_lshift_(1,HASH_TAB_n_) - 2);
      oldArr__ = HASH_TAB_arr_;
      HASH_TAB_arr_ = new1_(1,INT15_lshift_(1,HASH_TAB_n_),0);
      HASH_TAB_entries_ = 0;
      i__ = 0;
      while (1) {
         if ((i__ >= IATT_(oldArr__,4))) break;
         if ((PATT_(oldArr__, 8 + (4 * (i__))) != 0)) {
            HASH_TAB_hashInsert_(self__,PATT_(oldArr__, 8 + (4 * (i__))),PATT_(oldArr__, 8 + (4 * ((i__ + 1)))));
         }
         else {
         }
         i__ = (i__ + 2);
      }
   }
   else {
   }
}

/*----------------------------------------------------------------------
  This portion consists of "pure" C code.
----------------------------------------------------------------------*/

ptr deep_copy_doit_();

/* Do a copy of all objects accessible from p. */
ptr deep_copy_(p)
ptr p;
{
    ptr res;

    if (p == NULL)
	return NULL;

    /* Initialize hash table */
    dcHashInit_(0);

    res = deep_copy_doit_(p);

    /* Clean up hash table */
    /* (don't need to do anything) */

    return res;
}


ptr deep_copy_doit_(p)
ptr p;
{
  ptr tmp;
  ptr ptrp;
  int i,type_p;
  if ((tmp = dcHashGet_(0,p)) != NULL)
    return tmp; /* Object already copied, return ptr to new ob */
  
  tmp = copy_(p,0);
  dcHashInsert_(0,p,tmp);
  
  /*
   **	Let "ptrp" = Pointer to each object attribute in p which is a ptr object.
   **	    then do 
   **         Object attribute = deep_copy_doit_(ptrp);
   */
  type_p = TYPE_(tmp);
  for (i=0;i<ob_attr_num_(type_p);i++) {
    if (ob_attr_ctype_(type_p,i) == CTYPE_PTR_) {
      /* ob_attr_offset should offset to mult of 4 in ptr */
      ptrp = PATT_(tmp,ob_attr_offset_(type_p,i)); 
      /* Treat 1 as a special void pointer. */
      if (ptrp != NULL)
	PATT_(tmp,ob_attr_offset_(type_p,i)) = deep_copy_doit_(ptrp);
    }
  }
  return tmp;
}

/*----------------------------------------------------------------------
  Routines/variables for runtime checking. Most of these functions are
  only called when checking enabled. Don't worry printing if someone
  also handles the RT_ERROR raised exception. Printout goes to stderr.
  Without -chk no printout.
----------------------------------------------------------------------*/

ptr err_in_file_;		/* C string */
int err_at_line_;		/* line number */
int err_in_cls_;		/* Class number */
int err_name_ind_;		/* Name index */
int err_attr_ind_;		/* Attribute index */
int misc_int_;			/* Miscellaneous integer for error printing */
char IN_ASSERTION_;             /* Whether we are executing in assertions */
                                /* We avoid checking assertions of assertions */

void err_print_loc_(ln)
int ln;
{ 
  if ( err_in_file_ == 0 ) err_in_file_ = "unknown file";
  fprintf(stderr, "\"%s\", line %d: ", err_in_file_, ln); 
}

void arr_out_of_bound_(i,value,bound,ln)
int i, value, bound, ln;
{
  err_print_loc_(ln);
  fprintf(stderr, ARR_BOUND_MSG_, i+1, value, bound);
  RT_ERROR(ARR_BOUND_ERR_);
}

/* 
 * RT_ERROR can raise an exception. If someone is going to handle it
 * we better do not print.
 */

void err_quit_(err_type,ln)
int err_type;			/* Kind of error */
int ln;				/* Line where error occurs */
{
  err_print_loc_(ln);

  switch (err_type) {
  case VOID_OBJ_ERR_:
    fprintf(stderr, VOID_ERR_MSG_);
    break;
  case INVALD_CLS_ERR_:
    fprintf(stderr, INVALD_CLS_MSG_, err_in_cls_);
    break;
  case NO_ATTR_TAB_ERR_:
    fprintf(stderr, NO_ATTR_TAB_MSG_, err_in_cls_);
    break;
  case ILL_DISP_ERR_:
    fprintf(stderr, ILL_DISP_MSG_, err_in_cls_, err_name_ind_);
    break;
  case MISS_DISP_ERR_:
    fprintf(stderr, MISS_DISP_MSG_, err_in_cls_, err_name_ind_);
    break;
  case INVALD_ATTR_ERR_:
    fprintf(stderr, INVALD_ATTR_MSG_, err_attr_ind_, err_in_cls_,err_name_ind_);
    break;
  case DISP_NEW_ERR_:
    fprintf(stderr, DISP_NEW_MSG_, misc_int_);
    break;
  case B_FILE_MISSING_ERR_:
    fprintf(stderr, B_FILE_MISSING_MSG_);
    break;
  case NO_FEAT_TAB_ERR_:
    fprintf(stderr, NO_FEAT_TAB_MSG_, err_in_cls_);
    break;
  case INVALD_FEAT_ERR_:
    fprintf(stderr, INVALD_FEAT_MSG_, err_name_ind_, err_in_cls_);
    break;
  case TYPE_ASSERT_ERR_:
    fprintf(stderr, TYPE_ASSERT_MSG_);
  }

  RT_ERROR(err_type);
}

/*----------------------------------------------------------------------*/
/* Other runtime routines with various runtime checks */

/* Return a pointer to a zero initialized object of given class. */
ptr safe_generic_new_(ici,s1,s2,s3,s4,atomic_p,nargs,ln)
int ici,s1,s2,s3,s4;
int atomic_p;
int nargs,ln;
{
  misc_int_=nargs; 
  if (safe_ob_arr_dim_(ici) != nargs) err_quit_(DISP_NEW_ERR_,ln);
  switch (ob_arr_dim_(ici)) {
  case 0: return (safe_new_(ici,atomic_p,ln));
  case 1: return (safe_new1_(ici,s1,atomic_p,ln));
  case 2: return (safe_new2_(ici,s1,s2,atomic_p,ln));
  case 3: return (safe_new3_(ici,s1,s2,s3,atomic_p,ln));
  case 4: return (safe_new4_(ici,s1,s2,s3,s4,atomic_p,ln));
  }
}

/* Return a pointer to a zero initialized object of given non-array class. */
ptr safe_new_(ici,atomic_p,ln)
int ici;
int atomic_p;
int ln;
{
  ptr res;
  int nbytes;

  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
#ifdef GC_
  if (atomic_p) {
    nbytes=ob_base_size_(ici);
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else res=(ptr)gc_malloc(ob_base_size_(ici));
#else
  res=(ptr)calloc(ob_base_size_(ici),1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new_");
#endif
  TYPE_(res)=ici;
  return(res);
}

/* New for ARRAY classes. */
ptr safe_new1_(ici,as1,atomic_p,ln)
int ici,as1;
int atomic_p;
int ln;
{
  ptr res;
  int nbytes;

  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
#ifdef GC_
  if (atomic_p) {
    nbytes=ob_base_size_(ici)+4+as1*ctype_size_(ob_arr_ctype_(ici));
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(ob_base_size_(ici)+4+as1*ctype_size_(ob_arr_ctype_(ici)));
  }
#else
  res=(ptr)calloc(ob_base_size_(ici)+4+as1*ctype_size_(ob_arr_ctype_(ici)),1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new1_");
#endif
  TYPE_(res)=ici; 
  (*((int *)(res+ob_base_size_(ici))))=as1; /* fill in asize */
  return(res);
}

/* New for ARRAY2 classes. Access through stored offsets instead of mult. */
ptr safe_new2_(ici,as1,as2,atomic_p,ln)
int ici,as1,as2;
int atomic_p;
int ln;
{
  ptr tp,res;
  int i,csz,bs;
  int nbytes;

  err_in_cls_=ici; 
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  csz=ctype_size_(ob_arr_ctype_(ici));
  bs=ob_base_size_(ici);
#ifdef GC_
  if (atomic_p) {
    nbytes=bs+8+4*as1+as1*as2*csz;
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else res=(ptr)gc_malloc(bs+8+4*as1+as1*as2*csz);
#else
  res=(ptr)calloc(bs+8+4*as1+as1*as2*csz,1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new2_");
#endif
  TYPE_(res)=ici;
  tp=res+bs;			/* ptr to asize1 */
  (*((int *)tp))=as1;		/* fill in asize1 */
  tp+=4;  (*((int *)tp))=as2;	/* fill in asize2 */
  tp+=4;			/* head of row offsets  */
  for(i=0; i<as1; i++)		/* fill in the offsets */
    (*((int *)(tp+4*i)))=bs+8+as1*4+csz*i*as2;
  return(res);
}

/* New for ARRAY3 classes. Do access by multiplication. */
ptr safe_new3_(ici,as1,as2,as3,atomic_p,ln)
int ici,as1,as2,as3;
int atomic_p;
int ln;
{
  ptr res;
  int bs;
  int *tp;
  int i;
  int csz;
  int nbytes;

  err_in_cls_=ici; 
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  bs=ob_base_size_(ici);
  csz = ctype_size_(ob_arr_ctype_(ici));
#ifdef GC_
  nbytes=bs+12+as1*sizeof(int)+as1*as2*sizeof(int)+as1*as2*as3*csz;
  if (atomic_p) {
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(nbytes);
  }
#else
  res =(ptr)calloc(bs+12+
		   as1*sizeof(int)+
		   as1*as2*sizeof(int)+
		   as1*as2*as3*csz,1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new3_");
#endif
  TYPE_(res)=ici;
  tp=(int*)((char*)res+bs);		/* ptr to asize1 */
  *tp++=as1;		/* fill in asize1 */
  *tp++=as2;	        /* fill in asize2 */
  *tp++=as3;	        /* fill in asize3 */
  for (i=0;i<as1;i++)
    *tp++ = bs+12+
      as1*sizeof(int)+
	i*as2*sizeof(int);
  for (i=0;i<as1*as2;i++)
      *tp++ = bs+12+
	as1*sizeof(int)+
	  as1*as2*sizeof(int)+
	    i*csz*as3;
  return(res);
}

/* New for ARRAY4 classes. Do access by multiplication. */
ptr safe_new4_(ici,as1,as2,as3,as4,atomic_p,ln)
int ici,as1,as2,as3,as4;
int atomic_p;
int ln;
{
  ptr res;
  int *tp;
  int bs;
  int i;
  int csz;
  int nbytes;

  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  bs=ob_base_size_(ici);
  csz = ctype_size_(ob_arr_ctype_(ici));
#ifdef GC_
  nbytes = bs+16+as1*sizeof(int)+as1*as2*sizeof(int)+
    as1*as2*as3*sizeof(int)+as1*as2*as3*as4*csz;

  if (atomic_p) {
    res=(ptr)gc_malloc_atomic(nbytes);
    bzero(res,nbytes);
  }
  else {
    res=(ptr)gc_malloc(nbytes);
  }
#else
   res=(ptr)calloc(bs+16+
	           as1*sizeof(int)+
		   as1*as2*sizeof(int)+
		   as1*as2*as3*sizeof(int)+
		   as1*as2*as3*as4*csz,1);
#endif
#ifdef SAFE_RT_
  if (res==0) out_of_mem_("new4_");
#endif
  TYPE_(res)=ici;
  tp=(int*)((char*)res+bs);		/* ptr to asize1 */
  *tp++=as1;		/* fill in asize1 */
  *tp++=as2;	        /* fill in asize2 */
  *tp++=as3;	        /* fill in asize3 */
  *tp++=as4;            /* fill in asize4 */
  for (i=0;i<as1;i++)
    *tp++ = bs+16+
      as1*sizeof(int)+
	i*as2*sizeof(int);
  for (i=0;i<as1*as2;i++)
    *tp++ = bs+16+
      as1*sizeof(int)+
	as1*as2*sizeof(int)+
	    i*as3*sizeof(int);
  for (i=0;i<as1*as2*as3;i++)
    *tp++ = bs+16+
      as1*sizeof(int)+
	as1*as2*sizeof(int)+
	  as1*as2*as3*sizeof(int)+
	    i*csz*as4;
  return(res);
}

/*----------------------------------------------------------------------*/
/* Get the dispatch_table_ with name nm in class cls. Error if not found.
   dispatch_table_size_ is defined in main.c along with the table. The first
   14 bits of the key will be the class, the last 18 the name. */

int safe_get_dispatch_(cls,nm,ln)
int cls, nm, ln;
{
  unsigned int hsh,key;
  unsigned int firsthsh;

  err_in_cls_=cls;		/* In case we need them for error msg */
  err_name_ind_=nm;
  key=(((unsigned int)nm)<<14)+cls;
  hsh=(((key*key)%dispatch_table_size_)>>1)<<1;
  firsthsh = hsh;
  while(1)
    {
      if (dispatch_table_[hsh]==0) err_quit_(ILL_DISP_ERR_,ln);
      if (dispatch_table_[hsh]==key) return(dispatch_table_[hsh+1]);
      hsh+=2;
      if (hsh>=dispatch_table_size_) hsh=0;
      if (firsthsh == hsh) err_quit_(MISS_DISP_ERR_,ln);
    }
}

/*----------------------------------------------------------------------*/

int safe_ob_base_size_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  return (attr_table_[ici][0]);
}

int safe_ob_arr_dim_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  return (attr_table_[ici][1]);
}

int safe_ob_arr_ctype_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  return (attr_table_[ici][2]);
}

int safe_ob_attr_num_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  return (attr_table_[ici][3]);
}

int safe_ob_attr_ctype_(ici,j,ln)
int ici,j,ln;
{
  err_in_cls_=ici;
  err_attr_ind_=j;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  if ((j < 0) || (j >= ob_attr_num_(ici))) err_quit_(INVALD_ATTR_ERR_,ln);
  return (attr_table_[ici][4+j]);
}

int safe_ob_attr_offset_(ici,j,ln)
int ici,j,ln;
{
  err_in_cls_=ici;
  err_attr_ind_=j;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (attr_table_[ici]==0) err_quit_(NO_ATTR_TAB_ERR_,ln);
  if ((j < 0) || (j >= ob_attr_num_(ici))) err_quit_(INVALD_ATTR_ERR_,ln);
  return (attr_table_[ici][4+ob_attr_num_(ici)+j]);
}

/*----------------------------------------------------------------------*/

/*
  Support routines for SYS
  Author: Chu-Cheow Lim
  Date: Nov 27 1990
*/

/* Number of features in class */
int safe_cl_feat_num_(ici,ln)
int ici, ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  return (cl_feat_num_(ici));
}


/* C type of Sather class */
int safe_cl_ctype_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  return (cl_ctype_(ici));
}


/* Sather class of array element, 0 for non-array */
int safe_cl_arr_satype_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  return (cl_arr_satype_(ici));
}


/* Full name of class, returned as pointer to string (cast to int). */
int safe_cl_fullname_(ici,ln)
int ici,ln;
{
  err_in_cls_=ici;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  return (cl_fullname_(ici));
}


/* Name index of feature */
int safe_cl_feat_name_(ici,f,ln)
int ici, f, ln;
{
  err_in_cls_=ici;
  err_name_ind_=f;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  if ((f < 0) || (f >= cl_feat_num_(ici))) err_quit_(INVALD_FEAT_ERR_,ln);
  return (cl_feat_name_(ici,f));
}


/* Category of feature */
int safe_cl_feat_cat_(ici,f,ln)
int ici, f, ln;
{
  err_in_cls_=ici;
  err_name_ind_=f;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  if ((f < 0) || (f >= cl_feat_num_(ici))) err_quit_(INVALD_FEAT_ERR_,ln);
  return (cl_feat_cat_(ici,f));
}


/* Sather class of shared/constant/attribute, 0 for routine */
int safe_cl_feat_satype_(ici,f,ln)
int ici, f, ln;
{
  err_in_cls_=ici;
  err_name_ind_=f;
  if ((ici <= 0) || (ici > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (feat_table_[ici]==0) err_quit_(NO_FEAT_TAB_ERR_,ln);
  if ((f < 0) || (f >= cl_feat_num_(ici))) err_quit_(INVALD_FEAT_ERR_,ln);
  return (cl_feat_satype_(ici,f));
}


/*
  Support routines for MIRROR class:
  Author: Jeff Bilmes
  Date: Dec 5, 1990
*/

/* return true if class given by ici is a basic type */
/* ??? do I need to check here if ici is derived from basic  type??? */
char safe_arr_type_is_basic_(ici)
     int ici;
{
  int type = safe_cl_arr_satype_(ici,0);
  return (type == CHAR_ici ||
	  type == INT_ici  ||
	  type == BOOL_ici ||
	  type == REAL_ici ||
	  type == DOUBLE_ici);
}

/* return array dimension size of ob */
int safe_arr_d_size_(p,d)
     ptr p;
     int d;
{
  int obDim;
  if (d < 1 || d > 4)
    return 0;
  obDim = safe_ob_arr_dim_(TYPE_(p));
  if (d > obDim)
    return 0;
  switch (d) {
    case 1:
      return ARRD1_(p);
      break;
    case 2:
      return ARRD2_(p);
      break;
    case 3:
      return ARRD3_(p);
      break;
    case 4:
      return ARRD4_(p);
      break;
    default:
      break;
    }
  return 0;
}


/* given an array entry, return string form */
ptr strVal_(p,iattr,lastIndex)
     ptr p;
     int iattr,lastIndex;
{
  char buff[100];
  int saType = safe_cl_arr_satype_(TYPE_(p),0);

  if (saType == CHAR_ici)
    sprintf(buff,"%c",CATT_(p,iattr+SC_*lastIndex));
  else if (saType == INT_ici)
    sprintf(buff,"%d",IATT_(p,iattr+SI_*lastIndex));
  else if (saType == BOOL_ici)
    sprintf(buff,"%s",CATT_(p,iattr+SC_*lastIndex) ? "T" : "F");
  else if (saType == REAL_ici)
    sprintf(buff,"%f",FATT_(p,iattr+SF_*lastIndex));
  else if (saType == DOUBLE_ici)
    sprintf(buff,"%f",DATT_(p,iattr+SD_*lastIndex));
  else if (PATT_(p,iattr+SP_*lastIndex) == NULL)
    sprintf(buff,"<void>");
  else if (saType == STR_ici) {
    ptr tmp = copy_(PATT_(p,iattr+SP_*lastIndex),0);
    int len = ARRD1_(tmp);
    /* strip off last new line */
    if (CATT_(tmp,ARR1_(tmp,len-1)+SC_*(len-2)) == '\n') {
      CATT_(tmp,ARR1_(tmp,len-1)+SC_*(len-2)) = '\0';
      ARRD1_(tmp) --;
    }
    return tmp;
  } else {
    ptr tmp = PATT_(p,iattr+SP_*lastIndex);
    sprintf(buff,"<0x%X>",(int)tmp);
  }
  return makestr_(buff);
}


/* return value of array in string form */
ptr safe_arr_str_val_(p,i,j,k,l)
     ptr p;
     int i,j,k,l;
{
  int type_p = TYPE_(p);
  int dim;
  if (ob_arr_dim_(type_p) == 0)
    return NULL;
  switch (ob_arr_dim_(type_p)) {
    case 1:
      return strVal_(p,ARR1_(p,i),i);
      break;
    case 2:
      return strVal_(p,ARR2_(p,i,j),j);
      break;
    case 3:
      return strVal_(p,ARR3_(p,i,j,k),k);
      break;
    case 4:
      return strVal_(p,ARR4_(p,i,j,k,l),l);
      break;
    default:
      break;
    }
  return NULL;
}



/* return value of array element in $ALL form if it is not basic */
ptr safe_arr_val_(p,i,j,k,l)
     ptr p;
     int i,j,k,l;
{
  int type_p = TYPE_(p);
  if (safe_arr_type_is_basic_(TYPE_(p)))
      return NULL;
  switch (ob_arr_dim_(type_p)) {
    case 1:
      if (i<0 || i>ARRD1_(p)) return NULL;
      return PATT_(p,ARR1_(p,i)+SP_*i);
      break;
    case 2:
      if (i<0 || j<0 || i>ARRD1_(p) || j>ARRD2_(p)) return NULL;
      return PATT_(p,ARR2_(p,i,j)+SP_*j);
      break;
    case 3:
      if (i<0 || j<0 || k<0 || i>ARRD1_(p) || j>ARRD2_(p) || k>ARRD3_(p)) 
	return NULL;
      return PATT_(p,ARR3_(p,i,j,k)+SP_*k);
      break;
    case 4:
      if (i<0 || j<0 || k<0 || l<0 || 
	  i>ARRD1_(p) || j>ARRD2_(p) || k>ARRD3_(p) || l>ARRD4_(p))
	return NULL;
      return PATT_(p,ARR4_(p,i,j,k,l)+SP_*l);
      break;
    default:
      break;
    }
  return NULL;
}


/* given an array entry, convert string form to value */
/* array entries must be basic */
char setStrVal_(p,iattr,lastIndex,str)
     ptr p;
     int iattr,lastIndex;
     char* str;
{
  union all {
    char c;
    int i;
    float f;
    double d;
  } allVal;
  int saType = safe_cl_arr_satype_(TYPE_(p),0);

  if (saType == CHAR_ici) {
    if (sscanf(str,"%c",&allVal.c) == 0)
      return 0;
    CATT_(p,iattr+SC_*lastIndex) = allVal.c;
  } else if (saType == INT_ici) {
    if (sscanf(str,"%i",&allVal.i) == 0)
      return 0;
    IATT_(p,iattr+SI_*lastIndex) = allVal.i;
  } else if (saType == BOOL_ici) {
    if (sscanf(str,"%c",&allVal.c) == 0)
      return 0;
    if (allVal.c == 'T')
      CATT_(p,iattr+SC_*lastIndex) = (char)1;
    else if (allVal.c == 'F')
      CATT_(p,iattr+SC_*lastIndex) = (char)0;
    else
      return 0;
  } else if (saType == REAL_ici) {
    if (sscanf(str,"%f",&allVal.f) == 0)
      return 0;
    FATT_(p,iattr+SF_*lastIndex) = allVal.f;
  } else if (saType == DOUBLE_ici) {
    if (sscanf(str,"%lf",&allVal.d) == 0)
      return 0;
    DATT_(p,iattr+SD_*lastIndex) = allVal.d;
  }
  return 1;
}



/* set array element to value given by object */
char safe_set_arr_val_(p,i,j,k,l,val)
     ptr p;
     int i,j,k,l;
     ptr val;
{
  int type_p = TYPE_(p);
  int dim;
  if (ob_arr_dim_(type_p) == 0)
    return 0;
  if (safe_arr_type_is_basic_(TYPE_(p)))
      return 0;
  switch (ob_arr_dim_(type_p)) {
    case 1:
      PATT_(p,ARR1_(p,i)+SP_*i) = val;
      break;
    case 2:
      PATT_(p,ARR2_(p,i,j)+SP_*j) = val;
      break;
    case 3:
      PATT_(p,ARR3_(p,i,j,k)+SP_*k) = val;
      break;
    case 4:
      PATT_(p,ARR4_(p,i,j,k,l)+SP_*l) = val;
      break;
    default:
      break;
    }
  return 1;
}


/* set array element to value given by string. */
char safe_set_arr_str_val_(p,i,j,k,l,str)
     ptr p;
     int i,j,k,l;
     ptr str;
{
  int type_p = TYPE_(p);
  int dim;
  if (ob_arr_dim_(type_p) == 0)
    return 0;
  if (!safe_arr_type_is_basic_(TYPE_(p)))
      return 0;
  switch (ob_arr_dim_(type_p)) {
    case 1:
      return setStrVal_(p,ARR1_(p,i),i,STR2C_(str));
      break;
    case 2:
      return setStrVal_(p,ARR2_(p,i,j),j,STR2C_(str));
      break;
    case 3:
      return setStrVal_(p,ARR3_(p,i,j,k),k,STR2C_(str));
      break;
    case 4:
      return setStrVal_(p,ARR4_(p,i,j,k,l),l,STR2C_(str));
      break;
    default:
      break;
    }
  return 1;
}



/* return true if feature f of ci is a basic type */
char safe_f_basic_(ci,f)
     int ci,f;
{
  int ici = safe_cl_feat_satype_(ci,f,0);
  return (ici == CHAR_ici ||
	  ici == INT_ici  ||
	  ici == BOOL_ici ||
	  ici == REAL_ici ||
	  ici == DOUBLE_ici);
}

/* return value of feature f of object in $ALL format */
ptr safe_f_val_(p,f)
     ptr p;
     int f;
{
  int loc;
  int category;
  int type_p = TYPE_(p);
  if (safe_f_basic_(type_p,f))
    return NULL; /* can't be basic */
  if ((category = safe_cl_feat_cat_(type_p,f,0)) == F_ROUTINE)
    return NULL; /* can't be a routine */

  loc = safe_get_dispatch_(type_p,safe_cl_feat_name_(type_p,f,0),0);
  if (category == F_ATTRIBUTE)
    return PATT_(p,loc); /* loc is offset */
  else
    return *(ptr*)loc; /* loc is address of global variable */
}

/* return sather STR form of attribute given by feature num i of p */
ptr safe_f_str_val_(p,i)
     ptr p;
     int i;
{
  int loc;
  int category;
  void *tmp;
  char buff[100];
  int saType;
  
  if ((category = safe_cl_feat_cat_(TYPE_(p),i,0)) == F_ROUTINE)
    return makestr_("<NOVAL>"); /* can't be a routine */
  loc = safe_get_dispatch_(TYPE_(p),safe_cl_feat_name_(TYPE_(p),i,0),0);
  if (category == F_ATTRIBUTE)
    tmp = (void*) &PATT_(p,loc); /* loc is offset */
  else
    tmp = (void*) loc; /* loc is address of global variable */

  saType = safe_cl_feat_satype_(TYPE_(p),i,0);
  if (saType == CHAR_ici)
    sprintf(buff,"%c",*(char*)tmp);
  else if (saType == INT_ici)
    sprintf(buff,"%i",*(int*)tmp);
  else if (saType == BOOL_ici)
    sprintf(buff,"%c",*(char*)tmp ? 'T' : 'F');
  else if (saType == REAL_ici)
    sprintf(buff,"%f",*(float*)tmp);
  else if (saType == DOUBLE_ici)
    sprintf(buff,"%f",*(double*)tmp);
  else if ( *(ptr*)tmp == NULL)
    sprintf(buff,"<void>");
  else if (saType == STR_ici) {
    ptr tmptmp = copy_(*(ptr*)tmp,0);
    int len = ARRD1_(tmptmp);
    /* strip off last new line */
    if (CATT_(tmptmp,ARR1_(tmptmp,len-1)+SC_*(len-2)) == '\n') {
      CATT_(tmptmp,ARR1_(tmptmp,len-1)+SC_*(len-2)) = '\0';
      ARRD1_(tmptmp) --;
    }
    return tmptmp;
  } else
      sprintf(buff,"<0x%X>",*(int*)tmp);
  return makestr_(buff);
}

/* set feature given by feature number f of ob to value */
/* represented by val, or return false if bad format */
char safe_set_f_str_val_(p,f,val)
     ptr p;
     int f;
     ptr val;
{
  int loc;
  int category;
  void *tmp;
  char buff[100];
  int saType;
  char *str = STR2C_(val);
  int type_p = TYPE_(p);
  union all {
    char c;
    int i;
    float f;
    double d;
  } allVal;
  
  if (!safe_f_basic_(type_p,f))
    return NULL; /* must be basic */
  if ((category = safe_cl_feat_cat_(type_p,f,0)) == F_ROUTINE)
    return NULL; /* can't be a routine */
  loc = safe_get_dispatch_(type_p,safe_cl_feat_name_(type_p,f,0),0);
  if (category == F_ATTRIBUTE)
    tmp = (void*) &PATT_(p,loc); /* loc is offset */
  else
    tmp = (void*) loc; /* loc is address of global variable */

  saType = safe_cl_feat_satype_(TYPE_(p),f,0);
  if (saType == CHAR_ici) {
    if (sscanf(str,"%c",&allVal.c) != 1)
      return 0;
    *(char*)tmp = allVal.c;
  } else if (saType == INT_ici) {
    if (sscanf(str,"%i",&allVal.i) != 1)
      return 0;
    *(int*)tmp = allVal.i;
  } else if (saType == BOOL_ici) {
    if (*str == 'T')
      *(char*)tmp = 1;
    else if (*str == 'F')
      *(char*)tmp = 0;
    else
      return 0;
  } else if (saType == REAL_ici) {
    if (sscanf(str,"%f",&allVal.f) != 1)
      return 0;
    *(float*)tmp = allVal.f;
  } else if (saType == DOUBLE_ici) {
    if (sscanf(str,"%lf",&allVal.d) != 1)
      return 0;
    *(double*)tmp = allVal.d;
  } else
    return 0;
}

/* if not basic, set feature f of ob to val */
char safe_set_f_val_(ob,f,val)
     ptr ob;
     int f;
     ptr val;
{
  int type_p = TYPE_(ob);
  int category;
  ptr* tmp;
  int loc;

  if (safe_f_basic_(type_p,f))
    return 0; /* can't be basic */
  if ((category = safe_cl_feat_cat_(type_p,f,0)) == F_ROUTINE)
    return NULL; /* can't be a routine */
  loc = safe_get_dispatch_(type_p,safe_cl_feat_name_(type_p,f,0),0);
  if (category == F_ATTRIBUTE)
     *((ptr*)&PATT_(ob,loc)) = val;
  else
     *((ptr*)loc) = val;
  return 1;
}


/*----------------------------------------------------------------------*/

void type_mismatch_(i,j,ln)
int i,j,ln;
{
  err_print_loc_(ln);
  fprintf(stderr, TYPE_MISMATCH_MSG_, i, j);
  RT_ERROR(TYPE_MISMATCH_ERR_);
}

int curr_rt_type_;

int bit_i_[] = {
0x1, 0x2, 0x4, 0x8, 
0x10, 0x20, 0x40, 0x80, 
0x100, 0x200, 0x400, 0x800, 
0x1000, 0x2000, 0x4000, 0x8000, 
0x10000, 0x20000, 0x40000, 0x80000, 
0x100000, 0x200000, 0x400000, 0x800000, 
0x1000000, 0x2000000, 0x4000000, 0x8000000, 
0x10000000, 0x20000000, 0x40000000, 0x80000000, 
};

char safe_is_a_des_of_(i,j,ln)
int i,j,ln;
{
  err_in_cls_=i;
  if ((i <= 0) || (i > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  err_in_cls_=j;
  if ((j <= 0) || (j > num_classes_)) err_quit_(INVALD_CLS_ERR_,ln);
  if (des_table_[j]==0) err_quit_(NO_DES_TAB_ERR_,ln);
  return ((bit_i_[i%32] & des_table_[j][i/32]) != 0);
}


/* EOF runtime_.c */
