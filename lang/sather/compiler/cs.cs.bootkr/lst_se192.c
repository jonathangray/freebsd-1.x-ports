/* lst_se192.c : Sather class: LST_SEMANTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern SEM188_out_of_line_();
extern ptr SEM188_dup_();
extern SEM188_resolve_predef_types_();
extern SEM188_semant_();
#include "macros_.h"



/*constant*/ int LST192_print_indent_ = 2;
ptr LST192_create_();
LST192_out_of_line_();
ptr LST192_dup_();
LST192_put_kwdname_();
ptr LST192_sather_code_();
ptr LST192_initialize_();
LST192_resolve_predef_types_();
LST192_semant_();
ptr LST192_typeof_();
int LST192_get_offset_();
LST192_cprint_offset_();
ptr LST192_get_constval_();
LST192_cont_cprint_code_();
LST192_cprint_cname_();
LST192_cprint_extern_();
LST192_cprint_access_value_();
LST192_cprint_init_code_();
LST192_clear_();
/*constant*/ int LST192_def_init_size_ = 5;
ptr LST192_push_();
int LST192_size_();
char LST192_is_empty_();
ptr LST192_pop_();
ptr LST192_top_();
LST192_init_iterate_();
ptr LST192_curr_item_();
ptr LST192_next_item_();
ptr LST192_prev_item_();
ptr LST192_push_unique_();
ptr LST192_append_();
ptr LST192_union_();
char LST192_not_in_();
int LST192_contains_();
LST192_cprint_code_();
extern int attr_ent_LST192[];

ptr LST192_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(192,5,0);
   }
   else {
      res__ = (ptr)new1_(192,init_size__,0);
   }

   ret0__:
   return (res__);
}

LST192_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl4683_;
   static int gl4684_;
   static union dtype_ gl4685_;
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_4686_;
      }
      else {
      }
      gl4683_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl4683_,518,gl4684_,INTVAL_(gl4685_));
      VFN_(gl4685_)(gl4683_,fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_4686_: ;

   ret0__:
   return;
}

ptr LST192_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl4687_;
   static int gl4688_;
   static union dtype_ gl4689_;
   ptr gl481_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST192_create_(self__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_4690_;
      }
      else {
      }
      gl4687_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl4687_,471,gl4688_,INTVAL_(gl4689_));
      gl481_ = PFN_(gl4689_)(gl4687_);
      res__ = (ptr)LST192_push_(res__,gl481_);
      i__ = (int)(i__ + 1);
   }
goto_tag_4690_: ;

   ret0__:
   return (res__);
}

LST192_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4691_;
   static int gl4692_;
   static union dtype_ gl4693_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4691_ = x__;
   cache_dispatch_(gl4691_,796,gl4692_,INTVAL_(gl4693_));
   IATT_(gl4691_,INTVAL_(gl4693_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST192_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST192_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

LST192_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl4694_;
   static int gl4695_;
   static union dtype_ gl4696_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_4697_;
      }
      else {
      }
      gl4694_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl4694_,522,gl4695_,INTVAL_(gl4696_));
      VFN_(gl4696_)(gl4694_,index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_4697_: ;

   ret0__:
   return;
}

LST192_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   ptr gl4698_;
   static int gl4699_;
   static union dtype_ gl4700_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_4701_;
      }
      else {
      }
      gl4698_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl4698_,588,gl4699_,INTVAL_(gl4700_));
      VFN_(gl4700_)(gl4698_,symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_4701_: ;

   ret0__:
   return;
}

ptr LST192_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LST192_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

LST192_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr LST192_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

LST192_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

LST192_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST192_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST192_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST192_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST192_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,20))) {
         goto goto_tag_4702_;
      }
      else {
      }
      PATT_(self__, 24 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_4702_: ;

   ret0__:
   return;
}

ptr LST192_push_(self__,e__)
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

int LST192_size_(self__)
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

char LST192_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST192_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST192_is_empty_(self__)) {
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

ptr LST192_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST192_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 24 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LST192_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST192_curr_item_(self__)
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

ptr LST192_next_item_(self__)
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

ptr LST192_prev_item_(self__)
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

ptr LST192_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST192_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 24 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST192_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST192_append_(self__,list__)
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
         goto goto_tag_4703_;
      }
      else {
      }
      res__ = (ptr)LST192_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_4703_: ;

   ret0__:
   return (res__);
}

ptr LST192_union_(self__,list__)
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
         goto goto_tag_4704_;
      }
      else {
      }
      if (LST192_not_in_(self__,PATT_(list__, 24 + ((i__) << 2)))) {
         res__ = (ptr)LST192_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4704_: ;

   ret0__:
   return (res__);
}

char LST192_not_in_(self__,e__)
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
         goto goto_tag_4705_;
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
goto_tag_4705_: ;

   ret0__:
   return (res__);
}

int LST192_contains_(self__,e__)
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
         goto goto_tag_4706_;
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
goto_tag_4706_: ;

   ret0__:
   return (res__);
}

LST192_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

