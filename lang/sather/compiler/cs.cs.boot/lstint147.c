/* lstint147.c : Sather class: LSTINT_KEY, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr OUT80_c_(ptr self__, char ch__);
extern ptr OUT80_i_(ptr self__, int in__);
extern int INT15_u_mod_(int self__, int i__);
#include "macros_.h"



int LST147_hash_(ptr self__, int max__);
char LST147_is_equal_(ptr self__, ptr k__);
ptr LST147_initialize_(ptr self__, ptr initarg__);
void LST147_clear_(ptr self__);
/*constant*/ int LST147_def_init_size_ = 5;
ptr LST147_create_(ptr self__, int init_size__);
ptr LST147_push_(ptr self__, int e__);
int LST147_size_(ptr self__);
char LST147_is_empty_(ptr self__);
int LST147_pop_(ptr self__);
int LST147_top_(ptr self__);
void LST147_init_iterate_(ptr self__);
int LST147_curr_item_(ptr self__);
int LST147_next_item_(ptr self__);
int LST147_prev_item_(ptr self__);
ptr LST147_push_unique_(ptr self__, int e__);
ptr LST147_append_(ptr self__, ptr list__);
ptr LST147_union_(ptr self__, ptr list__);
char LST147_not_in_(ptr self__, int e__);
int LST147_contains_(ptr self__, int e__);
void LST147_key_print_(ptr self__, ptr outfile__);
extern int attr_ent_LST147[];

int LST147_hash_(ptr self__, int max__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    sum__ = S_int_VOID_;

   i__ = (int)0;
   sum__ = (int)0;
   while (1) {
      if ((i__ >= LST147_size_(self__))) {
         goto goto_tag_3777_;
      }
      else {
      }
      sum__ = (int)(sum__ + IATT_(self__, 16 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3777_: ;
   res__ = (int)INT15_u_mod_((sum__ * sum__),max__);

   ret0__:
   return (res__);
}

char LST147_is_equal_(ptr self__, ptr k__)
{
   char res__ = S_char_VOID_;
   int    ksz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   res__ = (char)1;
   ksz__ = S_int_VOID_;
   if ((k__ != 0)) {
      ksz__ = (int)IATT_(k__,4);
   }
   else {
   }
   if ((LST147_size_(self__) != ksz__)) {
      res__ = (char)0;
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= LST147_size_(self__))) {
         goto goto_tag_3778_;
      }
      else {
      }
      if ((IATT_(self__, 16 + ((i__) << 2)) != IATT_(k__, 16 + ((i__) << 2)))) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3778_: ;

   ret0__:
   return (res__);
}

ptr LST147_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void LST147_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,12))) {
         goto goto_tag_3779_;
      }
      else {
      }
      IATT_(self__, 16 + ((i__) << 2)) = (int)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_3779_: ;

   ret0__:
   return;
}

ptr LST147_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(147,5,1);
   }
   else {
      res__ = (ptr)new1_(147,init_size__,1);
   }

   ret0__:
   return (res__);
}

ptr LST147_push_(ptr self__, int e__)
{
   ptr res__ = 0;

   if ((IATT_(self__,4) < IATT_(self__,12))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,12)),1);
   }
   IATT_(res__, 16 + ((IATT_(self__,4)) << 2)) = (int)e__;
   IATT_(res__,4) = (int)(IATT_(res__,4) + 1);

   ret0__:
   return (res__);
}

int LST147_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__,4);
   }

   ret0__:
   return (res__);
}

char LST147_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,4) == 0);

   ret0__:
   return (res__);
}

int LST147_pop_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (LST147_is_empty_(self__)) {
      res__ = (int)0;
   }
   else {
      IATT_(self__,4) = (int)(IATT_(self__,4) - 1);
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,4)) << 2));
      IATT_(self__, 16 + ((IATT_(self__,4)) << 2)) = (int)0;
   }

   ret0__:
   return (res__);
}

int LST147_top_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (LST147_is_empty_(self__)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__, 16 + (((IATT_(self__,4) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LST147_init_iterate_(ptr self__)
{

   IATT_(self__,8) = (int)0;

   ret0__:
   return;
}

int LST147_curr_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((IATT_(self__,8) < IATT_(self__,4))) {
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

int LST147_next_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (((IATT_(self__,8) + 1) < IATT_(self__,4))) {
      IATT_(self__,8) = (int)(IATT_(self__,8) + 1);
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

int LST147_prev_item_(ptr self__)
{
   int res__ = S_int_VOID_;

   if (((IATT_(self__,8) - 1) >= 0)) {
      IATT_(self__,8) = (int)(IATT_(self__,8) - 1);
      res__ = (int)IATT_(self__, 16 + ((IATT_(self__,8)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST147_push_unique_(ptr self__, int e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST147_contains_(self__,e__);
   if ((k__ >= 0)) {
      IATT_(self__, 16 + ((k__) << 2)) = (int)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST147_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST147_append_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   if ((list__ == 0)) {
      res__ = (ptr)self__;
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)self__;
   i__ = (int)0;
   sz__ = (int)IATT_(list__,4);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3780_;
      }
      else {
      }
      res__ = (ptr)LST147_push_(res__,IATT_(list__, 16 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3780_: ;

   ret0__:
   return (res__);
}

ptr LST147_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,4);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3781_;
      }
      else {
      }
      if (LST147_not_in_(self__,IATT_(list__, 16 + ((i__) << 2)))) {
         res__ = (ptr)LST147_push_(res__,IATT_(list__, 16 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3781_: ;

   ret0__:
   return (res__);
}

char LST147_not_in_(ptr self__, int e__)
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (char)1;
   while (1) {
      if ((i__ >= IATT_(self__,4))) {
         goto goto_tag_3782_;
      }
      else {
      }
      if ((IATT_(self__, 16 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3782_: ;

   ret0__:
   return (res__);
}

int LST147_contains_(ptr self__, int e__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (int)(- 1);
   while (1) {
      if ((i__ >= IATT_(self__,4))) {
         goto goto_tag_3783_;
      }
      else {
      }
      if ((IATT_(self__, 16 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3783_: ;

   ret0__:
   return (res__);
}

void LST147_key_print_(ptr self__, ptr outfile__)
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   (void)OUT80_c_(outfile__,'[');
   while (1) {
      if ((i__ >= LST147_size_(self__))) {
         goto goto_tag_3784_;
      }
      else {
      }
      (void)OUT80_c_(OUT80_i_(outfile__,IATT_(self__, 16 + ((i__) << 2))),',');
      i__ = (int)(i__ + 1);
   }
goto_tag_3784_: ;
   (void)OUT80_c_(outfile__,']');

   ret0__:
   return;
}

