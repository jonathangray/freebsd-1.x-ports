/* key___212.c : Sather class: KEY, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

#include "macros_.h"



int KEY212_hash_();
char KEY212_is_equal_();
ptr KEY212_initialize_();
extern int attr_ent_KEY212[];

int KEY212_hash_(self__,max__)
ptr self__;
int max__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

char KEY212_is_equal_(self__,k__)
ptr self__;
ptr k__;
{
   char res__ = S_char_VOID_;
   ptr gl4778_;
   static int gl4779_;
   static union dtype_ gl4780_;
   ptr gl4781_;
   int gl484_;

   gl4778_ = k__;
   gl484_ = TYPE_(gl4778_);
   gl4781_ = self__;
   res__ = (char)(gl484_ == (212));

   ret0__:
   return (res__);
}

ptr KEY212_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

