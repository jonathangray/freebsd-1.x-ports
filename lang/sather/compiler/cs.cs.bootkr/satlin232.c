/* satlin232.c : Sather class: SATLINEEXP, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern ptr str_ptr_();
extern int sstrcmp();
#define sstrcmp(s1, s2) strcmp(s1, s2)

extern ptr CFI249_create_();
extern ARR250_clear_();
extern ptr FIL11_s_();
extern ptr FIL11_i_();
extern ptr FIL11_nl_();
#include "macros_.h"



int SAT232_comp_();
ptr SAT232_initialize_();
/*constant*/ int SAT232_defLength_ = 20;
ptr SAT232_create_();
SAT232_add_();
SAT232_print_();
extern int attr_ent_SAT232[];

int SAT232_comp_(self__,slep__)
ptr self__;
ptr slep__;
{
   int res__ = S_int_VOID_;
   ptr    sle__ = 0;
   int    val__ = S_int_VOID_;

   sle__ = (ptr)slep__;
   val__ = (int)sstrcmp(str_ptr_(PATT_(self__,8)),str_ptr_(PATT_(sle__,8)));
   if ((val__ < 0)) {
      res__ = (int)(- 1);
   }
   else {
      if ((val__ > 0)) {
         res__ = (int)1;
      }
      else {
         if ((IATT_(self__,12) < IATT_(sle__,12))) {
            res__ = (int)(- 1);
         }
         else {
            if ((IATT_(self__,12) > IATT_(sle__,12))) {
               res__ = (int)1;
            }
            else {
               res__ = (int)0;
            }
         }
      }
   }

   ret0__:
   return (res__);
}

ptr SAT232_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr SAT232_create_(self__,sfn__,sln__)
ptr self__;
ptr sfn__;
int sln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(232,0);
   PATT_(res__,4) = (ptr)new1_(250,20,0);
   IATT_(res__,16) = (int)0;
   PATT_(res__,8) = (ptr)sfn__;
   IATT_(res__,12) = (int)sln__;

   ret0__:
   return (res__);
}

SAT232_add_(self__,cfname__,ln__)
ptr self__;
ptr cfname__;
int ln__;
{
   ptr    at__ = 0;
   ptr    cfe__ = 0;

   if ((IATT_(self__,16) >= IATT_(PATT_(self__,4),4))) {
      at__ = (ptr)PATT_(self__,4);
      PATT_(self__,4) = (ptr)extend1_(at__,(2 * IATT_(at__,4)),0);
      ARR250_clear_(at__);
   }
   else {
   }
   cfe__ = (ptr)CFI249_create_(cfe__,cfname__,ln__);
   PATT_(PATT_(self__,4), 8 + ((IATT_(self__,16)) << 2)) = (ptr)cfe__;
   IATT_(self__,16) = (int)(IATT_(self__,16) + 1);

   ret0__:
   return;
}

SAT232_print_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,2,ls2189_,":");
   SATHER_STR_(20,2,ls2190_," ");
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   (void)FIL11_i_(FIL11_s_(FIL11_s_(outfile__,PATT_(self__,8)),(ptr)(&ls2189_)),IATT_(self__,12));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_4840_;
      }
      else {
      }
      (void)FIL11_i_(FIL11_s_(FIL11_s_(FIL11_s_(outfile__,(ptr)(&ls2190_)),PATT_(PATT_(PATT_(self__,4), 8 + ((i__) << 2)),4)),(ptr)(&ls2189_)),IATT_(PATT_(PATT_(self__,4), 8 + ((i__) << 2)),8));
      i__ = (int)(i__ + 1);
   }
goto_tag_4840_: ;
   (void)FIL11_nl_(outfile__);

   ret0__:
   return;
}

