/* str_ha78.c : Sather class: STR_HASH_MAP{INT}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR173_create_();
extern ptr GEN174_get_();
extern GEN174_insert_();
extern GEN174_delete_();
extern GEN174_clear_();
extern ptr STR175_create_();
extern STR173_set_hash_();
extern ptr GEN174_create_();
extern char GEN174_is_empty_();
#include "macros_.h"



ptr STR78_create_();
int STR78_size_();
int STR78_get_();
char STR78_test_();
STR78_insert_();
STR78_delete_();
STR78_clear_();
ptr STR78_cursor_();
char STR78_is_empty_();
ptr STR78_initialize_();
extern int attr_ent_STR78[];

ptr STR78_create_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl981_;

   res__ = (ptr)new_(78,0);
   PATT_(res__,4) = (ptr)GEN174_create_(PATT_(res__,4));
   gl981_ = PATT_(res__,8);
   PATT_(res__,8) = (ptr)new_(173,0);

   ret0__:
   return (res__);
}

int STR78_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(PATT_(self__,4),4);

   ret0__:
   return (res__);
}

int STR78_get_(self__,s__)
ptr self__;
ptr s__;
{
   int res__ = S_int_VOID_;
   ptr    te__ = 0;

   PATT_(PATT_(self__,8),4) = (ptr)s__;
   STR173_set_hash_(PATT_(self__,8));
   te__ = (ptr)GEN174_get_(PATT_(self__,4),PATT_(self__,8));
   if ((te__ != 0)) {
      res__ = (int)IATT_(te__,12);
   }
   else {
   }

   ret0__:
   return (res__);
}

char STR78_test_(self__,s__)
ptr self__;
ptr s__;
{
   char res__ = S_char_VOID_;
   ptr    te__ = 0;

   PATT_(PATT_(self__,8),4) = (ptr)s__;
   STR173_set_hash_(PATT_(self__,8));
   te__ = (ptr)GEN174_get_(PATT_(self__,4),PATT_(self__,8));
   if ((te__ != 0)) {
      res__ = (char)1;
   }
   else {
   }

   ret0__:
   return (res__);
}

STR78_insert_(self__,s__,v__)
ptr self__;
ptr s__;
int v__;
{

   GEN174_insert_(PATT_(self__,4),STR173_create_(0,s__,v__));

   ret0__:
   return;
}

STR78_delete_(self__,s__)
ptr self__;
ptr s__;
{

   PATT_(PATT_(self__,8),4) = (ptr)s__;
   STR173_set_hash_(PATT_(self__,8));
   GEN174_delete_(PATT_(self__,4),PATT_(self__,8));

   ret0__:
   return;
}

STR78_clear_(self__)
ptr self__;
{

   GEN174_clear_(PATT_(self__,4));

   ret0__:
   return;
}

ptr STR78_cursor_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR175_create_(res__,self__);

   ret0__:
   return (res__);
}

char STR78_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)GEN174_is_empty_(PATT_(self__,4));

   ret0__:
   return (res__);
}

ptr STR78_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

