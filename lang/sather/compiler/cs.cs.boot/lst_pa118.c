/* lst_pa118.c : Sather class: LST_PARSEROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr LST192_create_(ptr self__, int init_size__);
extern ptr LST192_push_(ptr self__, ptr e__);
extern ptr PAR116_pcopy_(ptr self__, ptr pl__, ptr pi__);
#include "macros_.h"



/*constant*/ int LST118_print_indent_ = 2;
ptr LST118_create_(ptr self__, int init_size__);
void LST118_out_of_line_(ptr self__, ptr fn__);
ptr LST118_dup_(ptr self__);
void LST118_put_kwdname_(ptr self__, int nm__);
ptr LST118_sather_code_(ptr self__);
ptr LST118_initialize_(ptr self__, ptr initarg__);
ptr LST118_pcopy_(ptr self__, ptr pl__, ptr pi__);
void LST118_clear_(ptr self__);
/*constant*/ int LST118_def_init_size_ = 5;
ptr LST118_push_(ptr self__, ptr e__);
int LST118_size_(ptr self__);
char LST118_is_empty_(ptr self__);
ptr LST118_pop_(ptr self__);
ptr LST118_top_(ptr self__);
void LST118_init_iterate_(ptr self__);
ptr LST118_curr_item_(ptr self__);
ptr LST118_next_item_(ptr self__);
ptr LST118_prev_item_(ptr self__);
ptr LST118_push_unique_(ptr self__, ptr e__);
ptr LST118_append_(ptr self__, ptr list__);
ptr LST118_union_(ptr self__, ptr list__);
char LST118_not_in_(ptr self__, ptr e__);
int LST118_contains_(ptr self__, ptr e__);
extern int attr_ent_LST118[];

ptr LST118_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(118,5,0);
   }
   else {
      res__ = (ptr)new1_(118,init_size__,0);
   }

   ret0__:
   return (res__);
}

void LST118_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LST118_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void LST118_put_kwdname_(ptr self__, int nm__)
{
   ptr gl2931_;
   static int gl2932_;
   static union dtype_ gl2933_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl2931_ = x__;
   cache_dispatch_(gl2931_,796,gl2932_,INTVAL_(gl2933_));
   IATT_(gl2931_,INTVAL_(gl2933_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST118_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST118_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LST118_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl2934_;
   static int gl2935_;
   static union dtype_ gl2936_;
   ptr gl290_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST192_create_(res__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2937_;
      }
      else {
      }
      gl2934_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl2934_,407,gl2935_,INTVAL_(gl2936_));
      gl290_ = PFN_(gl2936_)(gl2934_,pl__,pi__);
      res__ = (ptr)LST192_push_(res__,gl290_);
      i__ = (int)(i__ + 1);
   }
goto_tag_2937_: ;

   ret0__:
   return (res__);
}

void LST118_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,20))) {
         goto goto_tag_2938_;
      }
      else {
      }
      PATT_(self__, 24 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_2938_: ;

   ret0__:
   return;
}

ptr LST118_push_(ptr self__, ptr e__)
{
   ptr res__ = 0;

   if ((IATT_(self__,12) < IATT_(self__,20))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,20)),0);
   }
   PATT_(res__, 24 + ((IATT_(self__,12)) << 2)) = (ptr)e__;
   IATT_(res__,12) = (int)(IATT_(res__,12) + 1);

   ret0__:
   return (res__);
}

int LST118_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__,12);
   }

   ret0__:
   return (res__);
}

char LST118_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST118_pop_(ptr self__)
{
   ptr res__ = 0;

   if (LST118_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,12) = (int)(IATT_(self__,12) - 1);
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,12)) << 2));
      PATT_(self__, 24 + ((IATT_(self__,12)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LST118_top_(ptr self__)
{
   ptr res__ = 0;

   if (LST118_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 24 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LST118_init_iterate_(ptr self__)
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST118_curr_item_(ptr self__)
{
   ptr res__ = 0;

   if ((IATT_(self__,16) < IATT_(self__,12))) {
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST118_next_item_(ptr self__)
{
   ptr res__ = 0;

   if (((IATT_(self__,16) + 1) < IATT_(self__,12))) {
      IATT_(self__,16) = (int)(IATT_(self__,16) + 1);
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST118_prev_item_(ptr self__)
{
   ptr res__ = 0;

   if (((IATT_(self__,16) - 1) >= 0)) {
      IATT_(self__,16) = (int)(IATT_(self__,16) - 1);
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST118_push_unique_(ptr self__, ptr e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST118_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 24 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST118_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST118_append_(ptr self__, ptr list__)
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
   sz__ = (int)IATT_(list__,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_2939_;
      }
      else {
      }
      res__ = (ptr)LST118_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_2939_: ;

   ret0__:
   return (res__);
}

ptr LST118_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_2940_;
      }
      else {
      }
      if (LST118_not_in_(self__,PATT_(list__, 24 + ((i__) << 2)))) {
         res__ = (ptr)LST118_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2940_: ;

   ret0__:
   return (res__);
}

char LST118_not_in_(ptr self__, ptr e__)
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
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2941_;
      }
      else {
      }
      if ((PATT_(self__, 24 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2941_: ;

   ret0__:
   return (res__);
}

int LST118_contains_(ptr self__, ptr e__)
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
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_2942_;
      }
      else {
      }
      if ((PATT_(self__, 24 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2942_: ;

   ret0__:
   return (res__);
}

