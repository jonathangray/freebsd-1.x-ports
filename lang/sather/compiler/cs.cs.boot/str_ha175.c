/* str_ha175.c : Sather class: STR_HASH_MAP_CURSOR{INT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr GEN189_next_(ptr self__);
extern ptr GEN189_create_(ptr self__, ptr nt__);
extern ptr GEN189_first_(ptr self__);
extern ptr GEN189_item_(ptr self__);
#include "macros_.h"



ptr STR175_create_(ptr self__, ptr m__);
char STR175_is_done_(ptr self__);
int STR175_first_(ptr self__);
ptr STR175_key_(ptr self__);
int STR175_item_(ptr self__);
int STR175_next_(ptr self__);
ptr STR175_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_STR175[];

ptr STR175_create_(ptr self__, ptr m__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(175,0);
   PATT_(res__,4) = (ptr)GEN189_create_(0,PATT_(m__,4));

   ret0__:
   return (res__);
}

char STR175_is_done_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)CATT_(PATT_(self__,4),4);

   ret0__:
   return (res__);
}

int STR175_first_(ptr self__)
{
   int res__ = S_int_VOID_;

   (void)GEN189_first_(PATT_(self__,4));
   res__ = (int)STR175_item_(self__);

   ret0__:
   return (res__);
}

ptr STR175_key_(ptr self__)
{
   ptr res__ = 0;

   if ((! CATT_(PATT_(self__,4),4))) {
      res__ = (ptr)PATT_(GEN189_item_(PATT_(self__,4)),4);
   }
   else {
   }

   ret0__:
   return (res__);
}

int STR175_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((! CATT_(PATT_(self__,4),4))) {
      res__ = (int)IATT_(GEN189_item_(PATT_(self__,4)),12);
   }
   else {
   }

   ret0__:
   return (res__);
}

int STR175_next_(ptr self__)
{
   int res__ = S_int_VOID_;

   (void)GEN189_next_(PATT_(self__,4));
   res__ = (int)STR175_item_(self__);

   ret0__:
   return (res__);
}

ptr STR175_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

