/* sux___22.c : Sather class: SUX, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;
extern ptr eh_signal_doc_str_();
extern eh_expect_signal_();
extern eh_signals_init_();
extern ptr eh_signal_type_str_();

extern ptr STR20_from_c_str_();
#include "macros_.h"



/*constant*/ char SUX22_initialized_;
SUX22_expect_();
char SUX22_signals_init_();
ptr SUX22_create_();
ptr SUX22_signal_type_();
ptr SUX22_signal_doc_();
extern int attr_ent_SUX22[];

SUX22_expect_(self__,b__)
ptr self__;
int b__;
{

   eh_expect_signal_(b__);

   ret0__:
   return;
}

char SUX22_signals_init_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;
   eh_signals_init_();

   ret0__:
   return (res__);
}

ptr SUX22_create_(self__,b__)
ptr self__;
int b__;
{
   ptr res__ = 0;

   if ((! SUX22_initialized_)) {
      (void)SUX22_signals_init_(0);
   }
   else {
   }
   res__ = (ptr)new_(22,1);
   IATT_(res__,4) = (int)b__;

   ret0__:
   return (res__);
}

ptr SUX22_signal_type_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_from_c_str_(0,eh_signal_type_str_(IATT_(self__,4)));

   ret0__:
   return (res__);
}

ptr SUX22_signal_doc_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_from_c_str_(0,eh_signal_doc_str_(IATT_(self__,4)));

   ret0__:
   return (res__);
}

