/* hash_t230.c : Sather class: HASH_TABLE{$SEMANTOB}, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_u_mod_(int self__, int i__);
#include "macros_.h"



/*constant*/ int HAS230_def_table_size_ = 7;
ptr HAS230_create_(ptr self__, int size__);
int HAS230_hash_(ptr self__, int nm__);
ptr HAS230_get_obj_(ptr self__, int nm__);
char HAS230_add_obj_(ptr self__, int nm__, ptr ob__);
void HAS230_add_unique_obj_(ptr self__, int nm__, ptr ob__);
void HAS230_double_tables_(ptr self__);
char HAS230_remove_obj_(ptr self__, int nm__);
ptr HAS230_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_HAS230[];

ptr HAS230_create_(ptr self__, int size__)
{
   ptr res__ = 0;
   ptr gl4827_;
   ptr gl4828_;
   ptr gl4829_;

   res__ = (ptr)new_(230,0);
   if ((size__ <= 0)) {
      size__ = (int)7;
   }
   else {
   }
   IATT_(res__,16) = (int)size__;
   IATT_(res__,20) = (int)(size__ * 2);
   gl4827_ = PATT_(res__,4);
   PATT_(res__,4) = (ptr)new1_(163,(2 * size__),1);
   gl4828_ = PATT_(res__,8);
   PATT_(res__,8) = (ptr)new1_(163,(2 * size__),1);
   gl4829_ = PATT_(res__,12);
   PATT_(res__,12) = (ptr)new1_(246,size__,0);
   IATT_(res__,24) = (int)0;

   ret0__:
   return (res__);
}

int HAS230_hash_(ptr self__, int nm__)
{
   int res__ = S_int_VOID_;

   res__ = (int)INT15_u_mod_(nm__,IATT_(self__,20));

   ret0__:
   return (res__);
}

ptr HAS230_get_obj_(ptr self__, int nm__)
{
   ptr res__ = 0;
   int    insert__ = S_int_VOID_;

   insert__ = (int)HAS230_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            res__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2));
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return (res__);
}

char HAS230_add_obj_(ptr self__, int nm__, ptr ob__)
{
   char res__ = S_char_VOID_;
   int    insert__ = S_int_VOID_;

   if ((IATT_(self__,24) >= IATT_(self__,16))) {
      HAS230_double_tables_(self__);
   }
   else {
   }
   insert__ = (int)HAS230_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)ob__;
         IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (int)nm__;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
         IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)ob__;
            IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
            IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
            res__ = (char)1;
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return (res__);
}

void HAS230_add_unique_obj_(ptr self__, int nm__, ptr ob__)
{
   int    insert__ = S_int_VOID_;
   ptr    tmp_ob__ = 0;

   if ((IATT_(self__,24) >= IATT_(self__,16))) {
      HAS230_double_tables_(self__);
   }
   else {
   }
   insert__ = (int)HAS230_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         tmp_ob__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2));
         while (1) {
            if ((tmp_ob__ == 0)) {
               goto goto_tag_4830_;
            }
            else {
            }
            IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
            if ((IATT_(self__,24) == IATT_(self__,16))) {
               HAS230_double_tables_(self__);
               HAS230_add_unique_obj_(self__,nm__,ob__);
               goto ret0__;
            }
            else {
            }
            tmp_ob__ = (ptr)PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2));
         }
      goto_tag_4830_: ;
         PATT_(PATT_(self__,12), 8 + ((IATT_(self__,24)) << 2)) = (ptr)ob__;
         IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) = (int)nm__;
         IATT_(PATT_(self__,8), 8 + ((insert__) << 2)) = (int)IATT_(self__,24);
         IATT_(self__,24) = (int)(IATT_(self__,24) + 1);
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)ob__;
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return;
}

void HAS230_double_tables_(ptr self__)
{
   ptr gl4831_;
   ptr gl4832_;
   int    old_names_size__ = S_int_VOID_;
   ptr    nnames__ = 0;
   ptr    nindices__ = 0;
   int    i__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   int    insert__ = S_int_VOID_;

   old_names_size__ = (int)IATT_(self__,20);
   IATT_(self__,16) = (int)(2 * IATT_(self__,16));
   IATT_(self__,20) = (int)(2 * IATT_(self__,20));
   gl4831_ = PATT_(self__,4);
   nnames__ = (ptr)new1_(163,IATT_(self__,20),1);
   gl4832_ = PATT_(self__,8);
   nindices__ = (ptr)new1_(163,IATT_(self__,20),1);
   PATT_(self__,12) = (ptr)extend1_(PATT_(self__,12),IATT_(self__,16),0);
   i__ = (int)0;
   while (1) {
      if ((i__ >= old_names_size__)) {
         goto goto_tag_4833_;
      }
      else {
      }
      if ((IATT_(PATT_(self__,4), 8 + ((i__) << 2)) > 0)) {
         nm__ = (int)IATT_(PATT_(self__,4), 8 + ((i__) << 2));
         insert__ = (int)HAS230_hash_(self__,nm__);
         while (1) {
            if ((IATT_(nnames__, 8 + ((insert__) << 2)) == 0)) {
               IATT_(nnames__, 8 + ((insert__) << 2)) = (int)nm__;
               IATT_(nindices__, 8 + ((insert__) << 2)) = (int)IATT_(PATT_(self__,8), 8 + ((i__) << 2));
               goto goto_tag_4834_;
            }
            else {
               insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
            }
         }
      goto_tag_4834_: ;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4833_: ;
   PATT_(self__,4) = (ptr)nnames__;
   PATT_(self__,8) = (ptr)nindices__;

   ret0__:
   return;
}

char HAS230_remove_obj_(ptr self__, int nm__)
{
   char res__ = S_char_VOID_;
   int    insert__ = S_int_VOID_;

   insert__ = (int)HAS230_hash_(self__,nm__);
   while (1) {
      if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == 0)) {
         goto ret0__;
      }
      else {
         if ((IATT_(PATT_(self__,4), 8 + ((insert__) << 2)) == nm__)) {
            PATT_(PATT_(self__,12), 8 + ((IATT_(PATT_(self__,8), 8 + ((insert__) << 2))) << 2)) = (ptr)0;
            res__ = (char)1;
            goto ret0__;
         }
         else {
            insert__ = (int)INT15_u_mod_((insert__ + 1),IATT_(self__,20));
         }
      }
   }

   ret0__:
   return (res__);
}

ptr HAS230_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

