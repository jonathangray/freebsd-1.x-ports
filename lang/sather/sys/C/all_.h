/* File: sather/sys/C/all_.h
   Author: Stephen M. Omohundro.
   Machine independent conversion: Jeff Bilmes
   Copyright (C) International Computer Science Institute, 1991, 1992, 1993
 
   COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
   and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
   LICENSE contained in the file: "sather/doc/license.txt" of the Sather
   distribution. The license is also available from ICSI, 1947 Center
   St., Suite 600, Berkeley CA 94704, USA.

   Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
            Oscar Bosman (oscar@csis.dit.csiro.au)
   (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
   Australia, 1992, 1993.
   The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
   to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 
 *  FUNCTION:  Include file for all Sather generated code.
 ** RCS: $Id: all_.h,v 1.1 1994/02/12 03:23:35 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 19:24 1993 (hws)
 **  Oct 15 23:47 1993 (hws): improve -chk error file name handling with CATCH
 **  Sep  6 17:11 1993 (hws): pull GC #defines into all.h
 **  Sep  3 23:29 1993 (hws): need to include stdio.h for K&R C
 **  Sep  1 19:40 1993 (oscar): undo setjmp.h fudge, need to include it, noone will call it directly
 **  Sep  1 16:08 1993 (oscar): use our (GC safe) strdup_ instead of strdup
 **  Sep  1 14:02 1993 (oscar): EH_OBJ in EH_THROW needs to be a ptr
 **  Aug 31 11:57 1993 (oscar): fudge setjmp.h stuff for exception handling
 **  Aug 30 15:30 1993 (oscar): remove stdio.h stuff 
 **  May 27 23:37 1993 (oscar): adapted to work with GC 2.6 and greater
 **  May 12 18:32 1993 (hws): define EH constants for resumption or trace back
 **  Apr 17 07:39 1993 (hws): ANSI C "void" err_quit()
 */ 

#ifndef SATHER_RT_
#define SATHER_RT_

#ifdef sequent
#   define SHARED_DATA_ shared
#   define PRIVATE_DATA_ private
#endif /* sequent */

#ifdef sparc
#   define SHARED_DATA_
#   define PRIVATE_DATA_
#endif /* sparc */

#include "all.h"

/* To enhance portability, we define names for Sather types and base voids. */
#define S_int_VOID_ 0
#define S_float_VOID_ 0.0
#define S_double_VOID_ ((double)0.0)
#define S_char_VOID_ ((char)0)
#define S_bool_VOID_ ((char)0)
#define S_ptr_VOID_ ((ptr)NULL)

#define EH_NO_TRACE_ 0
#define EH_TRACE_ 1
#define EH_INTERACT_ 2
#define EH_TRACE_LIM_ 10

typedef void (*ROUTINE_)();
typedef int SATHER_INT_;
typedef ptr SATHER_OBJ_;
typedef float SATHER_REAL_;
typedef double SATHER_DOUBLE_;
typedef char SATHER_CHAR_;
typedef char SATHER_BOOL_;

/* Union type for dispatched value */
union dtype_ {
  int intval__;			/* Attribute offsets */
  int* intptr__;		/* Pointers to constants/shareds */
  char* charptr__;
  float* floatptr__;
  ptr* ptrptr__;
  int (*fn_int__)();		/* Function pointers */
  char (*fn_char__)();
  float (*fn_float__)();
  double (*fn_double__)();
  ptr (*fn_ptr__)();
  void (*fn_void__)();
};

/* Accessor to different union fields */
#define INTVAL_(v) ((v).intval__)

#define INTPTR_(v) (*((v).intptr__))
#define CHARPTR_(v) (*((v).charptr__))
#define FLOATPTR_(v) (*((v).floatptr__))
#define DOUBLEPTR_(v) (*((v).doubleptr__))
#define PTRPTR_(v) (*((v).ptrptr__))

/* Dispatched functions returning different types */
#define IFN_(fn) (*((fn).fn_int__))
#define CFN_(fn) (*((fn).fn_char__))
#define FFN_(fn) (*((fn).fn_float__))
#define DFN_(fn) (*((fn).fn_double__))
#define PFN_(fn) (*((fn).fn_ptr__))
#define VFN_(fn) (*((fn).fn_void__))

/* Default initial value for any data */
#ifndef NULL
#   define NULL 0
#endif

/* Since we are quitting, we do not have to save on a function call. */
extern void err_quit_();
extern void arr_out_of_bound_();
extern void type_mismatch_();
extern ptr err_in_file_;
extern int SUX_ici;

#define VOID_OBJ_ERR_        33 	/* Void object access */
#define INVALD_CLS_ERR_      34 	/* Invalid class number */
#define NO_ATTR_TAB_ERR_     35         /* Missing attribute table */
#define ILL_DISP_ERR_        36 	/* Illegal dispatch */
#define MISS_DISP_ERR_       37 	/* Missing dispatch */
#define INVALD_ATTR_ERR_     38 	/* Invalid attribute */
#define DISP_NEW_ERR_        39 	/* Error with new on dispatched object */
#define ARR_BOUND_ERR_       40 	/* Error with new on dispatched object */
#define B_FILE_MISSING_ERR_  41         /* Missing file for browser */
#define NO_FEAT_TAB_ERR_     42         /* Missing feature table */
#define INVALD_FEAT_ERR_     43         /* Feature does not exist in class */
#define TYPE_MISMATCH_ERR_   44         /* Types do not conform */ 
#define NO_DES_TAB_ERR_      45         /* Missing descendent table */
#define OUT_OF_MEM_ERR_      46         /* Out of memory */
#define EH_VOID_OBJ_ERR_     47         /* Void exception object */
#define EH_VOID_TYPE_ERR_    48         /* Void exception  type */
#define TYPE_ASSERT_ERR_     49         /* Assertion violation */


#define OUT_OF_MEM_MSG_     "Out of memory in runtime routine (%s)\n"

#define NO_ATTR_TAB_MSG_    "Missing attribute table for class #%d\n"
#define NO_FEAT_TAB_MSG_    "Missing feature table for class #%d\n"
#define B_FILE_MISSING_MSG_ "Missing browser file; check compiler option\n"
#define NO_DES_TAB_MSG_     "Missing descendent table for class %d; check compiler option\n"
#define MISS_DISP_MSG_      "Missing dispatch reference, class %d, name index %d\n"

#define VOID_ERR_MSG_       "Void object access\n"
#define INVALD_CLS_MSG_     "Bad class index %d\n"
#define INVALD_FEAT_MSG_    "Bad feature index %d in class #%d\n"
#define DISP_NEW_MSG_       "Bad dispatch to \"new\", wrong number of args (%d)\n"
#define ILL_DISP_MSG_       "Bad dispatch reference, class %d, name index %d\n"
#define INVALD_ATTR_MSG_    "Bad attribute #%d in class %d, object %d\n"

#define ARR_BOUND_MSG_      "Out of bounds array index %d, size %d\n"
#define TYPE_MISMATCH_MSG_  "Type violation, #%d does not conform to type #%d\n"
#define EH_VOID_OBJ_MSG_    "Void exception object raised\n"
#define EH_VOID_TYPE_MSG_   "Void exception type, object #%d\n"
#define TYPE_ASSERT_MSG_    "Assertion violation\n"

extern ptr safe_generic_new_();
extern ptr safe_new_();
extern ptr safe_new1_();
extern ptr safe_new2_();
extern ptr safe_new3_();
extern ptr safe_new4_();
extern int safe_get_dispatch_();
extern int safe_ob_base_size_();
extern int safe_ob_arr_dim_();
extern int safe_ob_arr_ctype_();
extern int safe_ob_attr_num_();
extern int safe_ob_attr_ctype_();
extern int safe_ob_attr_offset_();

extern int safe_cl_feat_num_();
extern int safe_cl_ctype_();
extern int safe_cl_arr_satype_();
extern int safe_cl_fullname_();	/* C string address. */
extern int safe_cl_feat_name_();
extern int safe_cl_feat_cat_();
extern int safe_cl_feat_satype_();

/* support for MIRROR class */
extern char safe_arr_type_is_basic();
extern int safe_arr_d_size();
extern ptr safe_arr_str_val();
extern ptr safe_arr_val();
extern char safe_set_arr_str_val();
extern char safe_f_basic();
extern ptr safe_f_val();
extern ptr safe_f_str_val();
extern char safe_set_f_str_val();


#define VOID_TST_(x,ln) {if ((x)==S_ptr_VOID_) err_quit_(VOID_OBJ_ERR_,ln);}
#define ARR_BOUND_TST_(i,x,bound,ln) \
{if (((x) < 0) || ((x) >= (bound))) arr_out_of_bound_(i,x,bound,ln);}

/* The type tag of an object. */
#define TYPE_(o) (*((int *)(o)))

/* C types */
#define CTYPE_PTR_ 1
#define CTYPE_CHAR_ 2
#define CTYPE_INT_ 3
#define CTYPE_FLOAT_ 4
#define CTYPE_DOUBLE_ 5

/* categories of features */
#define F_ATTRIBUTE 1
#define F_ROUTINE   2
#define F_SHARED    3
#define F_CONSTANT  4

/* Make key for hash table */
#define DISPATCH_KEY_(c,n) ((((unsigned int)n)<<14)+c)

/* shorthand size of data types in C */
#define SC_ sizeof(char)
#define SS_ sizeof(short)
#define SI_ sizeof(int)
#define SL_ sizeof(long)
#define SF_ sizeof(float)
#define SD_ sizeof(double)
#define SP_ sizeof(ptr)

/* The size in bytes of the ctypes 1,2,3,4,5
 1 => ptr
 2 => char
 3 => int
 4 => float
 5 => double */

#define ctype_size_(c) ((c)==CTYPE_PTR_?  \
			   SP_:           \
			 ((c)==CTYPE_CHAR_? \
			   SC_:             \
			 ((c)==CTYPE_INT_?  \
			   SI_:             \
			 ((c)==CTYPE_FLOAT_?   \
			   SF_:                \
			  /* else */ SD_))))

#define CATT_(ob,off) (*((char *)((ob)+(off))))
#define IATT_(ob,off) (*((int *)((ob)+(off))))
#define FATT_(ob,off) (*((float *)((ob)+(off))))
#define DATT_(ob,off) (*((double *)((ob)+(off))))
#define PATT_(ob,off) (*((ptr *)((ob)+(off))))

/* definitions for array access from C:
** to use (for example, to get to element i,j,k,l of a double 4d array p
** do: (note, last index in ARR macros is for uniformity).
**        DATT_(p,ARR4_(p,i,j,k,l)+SD_*l);
*/
#define ARR1_(ob_,indx1_) \
	 (ob_base_size_(TYPE_(ob_)) + 1*SI_)

#define ARR2_(ob_,indx1_,indx2_) \
	 IATT_((ob_), \
	       ob_base_size_(TYPE_(ob_)) + 2*SI_ + (SI_ * (indx1_)))

#define ARR3_(ob_,indx1_,indx2_,indx3_) \
	 IATT_((ob_), \
	       IATT_((ob_), \
		     ob_base_size_(TYPE_(ob_)) + 3*SI_ + (SI_ * (indx1_))) \
	       + (SI_ * (indx2_)))

#define ARR4_(ob_,indx1_,indx2_,indx3_,indx4_) \
	 IATT_((ob_), \
	       IATT_((ob_), \
		     IATT_((ob_), \
			   ob_base_size_(TYPE_(ob_)) + 4*SI_ + (SI_ * (indx1_))) \
		     + (SI_ * (indx2_))) \
	       + (SI_ * (indx3_)))

/* array dimension sizes from C */
#define ARRD1_(ob_) \
    (*((int *)((ptr)(ob_)+ob_base_size_(TYPE_(ob_)))))
#define ARRD2_(ob_) \
    (*((int *)((ptr)(ob_)+SI_+ob_base_size_(TYPE_(ob_)))))
#define ARRD3_(ob_) \
    (*((int *)((ptr)(ob_)+2*SI_+ob_base_size_(TYPE_(ob_)))))
#define ARRD4_(ob_) \
    (*((int *)((ptr)(ob_)+3*SI_+ob_base_size_(TYPE_(ob_)))))

/* convert string object to C char* string. Macro form. */
#define STR2C_(p) (char*)((char*)(p)+2*SI_)

/* Access to attr_table_. This is actually defined in main.c */
extern int num_classes_;
extern int* attr_table_[];

#define ob_base_size_(i) (attr_table_[i][0])
#define ob_arr_dim_(i) (attr_table_[i][1])
#define ob_arr_ctype_(i) (attr_table_[i][2])
#define ob_attr_num_(i) (attr_table_[i][3])
#define ob_attr_ctype_(i,j) (attr_table_[i][4+j])
#define ob_attr_offset_(i,j) (attr_table_[i][4+ob_attr_num_(i)+j])

/* Access to dispatch_table_. */
#ifndef SATHER_DISPATCH_ 
extern int dispatch_table_size_;
extern int dispatch_table_[];
extern int get_dispatch_();	/* given cls and nm returns value. */
extern int ob_get_dispatch_();	/* given cls and nm returns value. */
#define SATHER_DISPATCH_
#endif

/* Make sure glt is type of ob and glv holds val for nm. */
#define cache_dispatch_(ob,nm,glt,glv) \
if(TYPE_(ob)!=glt) {glt=TYPE_(ob); glv=get_dispatch_(glt,nm);}

/* Similar to "cache_dispatch_" but for use in "f.expr" (f:$OB). */
#define ob_cache_dispatch_(ob,nm,glt,glv) \
if(TYPE_(ob)!=glt) {glt=TYPE_(ob); glv=ob_get_dispatch_(glt,nm);}

/* Same as above but for array references. glv holds base size. */
#define array_dispatch_(ob,glt,glv) \
if(TYPE_(ob)!=glt) {glt=TYPE_(ob); glv=ob_base_size_(glt);}

/* These have to be implemented as macros because temporary variables
   are updated. */
#define safe_cache_dispatch_(ob,nm,glt,glv,ln) \
if (TYPE_(ob)!=glt) {glt=TYPE_(ob); glv=safe_get_dispatch_(glt,nm,ln);}

/* Support for 'assert' statement */

#ifndef __STDC__ 
#    include <stdio.h>
#endif

/* 
NOTE:
-----
This macro definition is preprocessor-dependent. 
*/ 
extern char IN_ASSERTION_;

#define assert_(x,ln,file) {if (!IN_ASSERTION_) { \
             IN_ASSERTION_=1;\
             if (!(x)) { \
               err_in_file_=file;\
               err_quit_(TYPE_ASSERT_ERR_,ln);};\
             IN_ASSERTION_=0; };}      					       

#define SATHER_STR_(t,s,v,st) \
static struct { int tp_; int sz_; char st_[s]; } v = { t, s, st }
#define SATHER_STR1_(t,s,v,st) \
struct { int tp_; int sz_; char st_[s]; } v = { t, s, st }

#define dcHashGet_ HASH_TAB_hashGet_
#define dcHashInsert_ HASH_TAB_hashInsert_
#define dcHashInit_ HASH_TAB_hashInit_

/* Runtime support routines */
extern int OB_ici;
extern int ARRAY_ici;
extern int ARRAY2_ici;
extern int ARRAY3_ici;
extern int ARRAY4_ici;
extern int BOOL_ici;
extern int C_ici;
extern int CHAR_ici;
extern int DOUBLE_ici;
extern int ERR_ici;
extern int FILE_ici;
extern int IN_ici;
extern int INT_ici;
extern int OUT_ici;
extern int REAL_ici;
extern int SELF_TYPE_ici;
extern int STR_ici;
extern int STR_CURSOR_ici;
extern int SYS_ici;
extern int FOB_ici;
extern int UNDEFINE_ici;
extern int MONITOR_ici;
extern int MONITOR0_ici;
extern int LAST_PREDEF_ici;

extern rt_init_();
extern ptr generic_new_();
extern ptr new_();
extern ptr new1_();
extern ptr new2_();
extern ptr new3_();
extern ptr new4_();
extern ptr copy_();
extern ptr extend1_();
extern ptr extend2_();
extern ptr extend3_();
extern ptr extend4_();
extern ptr deep_copy_();
extern int get_dispatch_();
extern int atomic_p_();
extern int type_();

/* Conversion between Sather and C strings */
extern ptr str_ptr_();
extern ptr makestr_();

#ifndef SATHER_SYS_
#  define SATHER_SYS_

extern int* feat_table_[];

#  define cl_feat_num_(i) (feat_table_[i][0])
#  define cl_ctype_(i) (feat_table_[i][1])
#  define cl_arr_satype_(i) (feat_table_[i][2])
#  define cl_fullname_(i) (feat_table_[i][3])
#  define cl_feat_name_(i,j) (feat_table_[i][4 + j])
#  define cl_feat_cat_(i,j) (feat_table_[i][4 + cl_feat_num_(i) + j])
#  define cl_feat_satype_(i,j) (feat_table_[i][4 + (2 * cl_feat_num_(i)) + j])

/* Initialized as C strings */
extern char* prog_name_;
extern char* prog_dir_;
extern int** des_relation_;

extern int max_name_index_;

#endif

extern int TRACE_BACK_;

#define UPDATE_EXEC_INFO_(fn,cn,rn) ptr err_in_file_here__ = fn ; \
                                    ptr err_in_file_caller__; \
                                    err_in_file_caller__ = err_in_file_ ; \
				    err_in_file_ = err_in_file_here__
#define RESTORE_EXEC_INFO_ err_in_file_ = err_in_file_caller__
#define CATCH_EXEC_INFO_ err_in_file_ = err_in_file_here__
#define SHARED_UPDATE_EXEC_INFO_(fn,cn) err_in_file_ = fn

/*
  Runtime type checking
  Author: Chu-Cheow Lim
  Date: Nov 30 1990
*/
extern char safe_is_a_des_of_();
extern int bit_i_[];
extern curr_rt_type_;
extern int* des_table_[];  /* Whether a class is a descendent of another */

/* There is no need to check for void object, because if the runtime
   checks are on, void object test code would have been generated to
   execute before the type-checking code. */

/* **********  MACHINE DEPENDANT **********  */
#define BITS_PER_CHAR 8
/* num of bits in an integer */
#define IBITS (sizeof(int)*BITS_PER_CHAR) 
#define IS_A_DES_OF_(i,j) ((bit_i_[(i)%IBITS] & des_table_[j][(i)/IBITS]) != 0)

/* Reserve a range of numbers (0-10) for void objects.  The reason is described
   in "~clim/l/sather/scomp/rt_typecheck.note" */

/* We have two different type-checking macros, one for dispatched type and
   the other for non-dispatched type. */
#define TYPECHK1_(obj,j,ln) \
{if ((((int)obj) >= 0) && (((int)obj) <= 10)) {} \
else { \
  curr_rt_type_ = TYPE_(obj); \
  if (curr_rt_type_!=(j)) \
     type_mismatch_(curr_rt_type_,j,ln); \
     } \
}

#define TYPECHK2_(obj,j,ln) \
{if ((((int)obj) >= 0) && (((int)obj) <= 10)) {} \
else { \
  curr_rt_type_ = TYPE_(obj); \
  if ((curr_rt_type_!=-(j)) && (!(IS_A_DES_OF_(curr_rt_type_,-(j))))) \
     type_mismatch_(curr_rt_type_,-(j),ln); \
     } \
}

#include "except_.h"

#endif

