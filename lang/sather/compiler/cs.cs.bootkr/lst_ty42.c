/* lst_ty42.c : Sather class: LST_TYPEOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr TYP114_pcopy_();
extern ptr LST102_create_();
extern ptr LST102_push_();
#include "macros_.h"



/*constant*/ int LST42_print_indent_ = 2;
ptr LST42_create_();
LST42_out_of_line_();
ptr LST42_dup_();
LST42_put_kwdname_();
ptr LST42_sather_code_();
ptr LST42_initialize_();
ptr LST42_pcopy_();
LST42_clear_();
/*constant*/ int LST42_def_init_size_ = 5;
ptr LST42_push_();
int LST42_size_();
char LST42_is_empty_();
ptr LST42_pop_();
ptr LST42_top_();
LST42_init_iterate_();
ptr LST42_curr_item_();
ptr LST42_next_item_();
ptr LST42_prev_item_();
ptr LST42_push_unique_();
ptr LST42_append_();
ptr LST42_union_();
char LST42_not_in_();
int LST42_contains_();
extern int attr_ent_LST42[];

ptr LST42_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(42,5,0);
   }
   else {
      res__ = (ptr)new1_(42,init_size__,0);
   }

   ret0__:
   return (res__);
}

LST42_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LST42_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

LST42_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl657_;
   static int gl658_;
   static union dtype_ gl659_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl657_ = x__;
   cache_dispatch_(gl657_,796,gl658_,INTVAL_(gl659_));
   IATT_(gl657_,INTVAL_(gl659_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST42_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST42_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LST42_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl660_;
   static int gl661_;
   static union dtype_ gl662_;
   ptr gl17_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST102_create_(res__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_663_;
      }
      else {
      }
      gl660_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl660_,407,gl661_,INTVAL_(gl662_));
      gl17_ = PFN_(gl662_)(gl660_,pl__,pi__);
      res__ = (ptr)LST102_push_(res__,gl17_);
      i__ = (int)(i__ + 1);
   }
goto_tag_663_: ;

   ret0__:
   return (res__);
}

LST42_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,20))) {
         goto goto_tag_664_;
      }
      else {
      }
      PATT_(self__, 24 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_664_: ;

   ret0__:
   return;
}

ptr LST42_push_(self__,e__)
ptr self__;
ptr e__;
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

int LST42_size_(self__)
ptr self__;
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

char LST42_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST42_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST42_is_empty_(self__)) {
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

ptr LST42_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST42_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 24 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LST42_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST42_curr_item_(self__)
ptr self__;
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

ptr LST42_next_item_(self__)
ptr self__;
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

ptr LST42_prev_item_(self__)
ptr self__;
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

ptr LST42_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST42_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 24 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST42_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST42_append_(self__,list__)
ptr self__;
ptr list__;
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
         goto goto_tag_665_;
      }
      else {
      }
      res__ = (ptr)LST42_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_665_: ;

   ret0__:
   return (res__);
}

ptr LST42_union_(self__,list__)
ptr self__;
ptr list__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_666_;
      }
      else {
      }
      if (LST42_not_in_(self__,PATT_(list__, 24 + ((i__) << 2)))) {
         res__ = (ptr)LST42_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_666_: ;

   ret0__:
   return (res__);
}

char LST42_not_in_(self__,e__)
ptr self__;
ptr e__;
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
         goto goto_tag_667_;
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
goto_tag_667_: ;

   ret0__:
   return (res__);
}

int LST42_contains_(self__,e__)
ptr self__;
ptr e__;
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
         goto goto_tag_668_;
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
goto_tag_668_: ;

   ret0__:
   return (res__);
}

