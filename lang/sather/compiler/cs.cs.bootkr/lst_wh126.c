/* lst_wh126.c : Sather class: LST_WHEN_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr LIS98_append_();
extern WHE136_out_of_line_();
extern ptr WHE136_dup_();
extern WHE136_resolve_predef_types_();
extern WHE136_semant_();
extern WHE136_cprint_code_();
extern WHE136_validate_dispatches_and_get_ext_strs_();
extern ptr WHE136_gen_temps_();
extern WHE136_gen_goto_tags_();
extern int GLO94_global_key_();
#include "macros_.h"



/*constant*/ int LST126_print_indent_ = 2;
ptr LST126_create_();
LST126_out_of_line_();
ptr LST126_dup_();
LST126_put_kwdname_();
ptr LST126_sather_code_();
ptr LST126_initialize_();
LST126_resolve_predef_types_();
LST126_semant_();
ptr LST126_typeof_();
int LST126_get_offset_();
LST126_cprint_offset_();
ptr LST126_get_constval_();
LST126_cont_cprint_code_();
LST126_cprint_cname_();
LST126_cprint_extern_();
LST126_cprint_access_value_();
LST126_cprint_init_code_();
LST126_clear_();
/*constant*/ int LST126_def_init_size_ = 5;
ptr LST126_push_();
int LST126_size_();
char LST126_is_empty_();
ptr LST126_pop_();
ptr LST126_top_();
LST126_init_iterate_();
ptr LST126_curr_item_();
ptr LST126_next_item_();
ptr LST126_prev_item_();
ptr LST126_push_unique_();
ptr LST126_append_();
ptr LST126_union_();
char LST126_not_in_();
int LST126_contains_();
LST126_cprint_code_();
int LST126_get_goto_tag_();
LST126_validate_dispatches_and_get_ext_strs_();
ptr LST126_gen_temps_();
LST126_gen_goto_tags_();
extern int attr_ent_LST126[];

ptr LST126_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(126,5,0);
   }
   else {
      res__ = (ptr)new1_(126,init_size__,0);
   }

   ret0__:
   return (res__);
}

LST126_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3081_;
      }
      else {
      }
      WHE136_out_of_line_(PATT_(self__, 28 + ((i__) << 2)),fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3081_: ;

   ret0__:
   return;
}

ptr LST126_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST126_create_(self__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3082_;
      }
      else {
      }
      res__ = (ptr)LST126_push_(res__,WHE136_dup_(PATT_(self__, 28 + ((i__) << 2))));
      i__ = (int)(i__ + 1);
   }
goto_tag_3082_: ;

   ret0__:
   return (res__);
}

LST126_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3083_;
   static int gl3084_;
   static union dtype_ gl3085_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3083_ = x__;
   cache_dispatch_(gl3083_,796,gl3084_,INTVAL_(gl3085_));
   IATT_(gl3083_,INTVAL_(gl3085_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST126_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST126_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

LST126_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3086_;
      }
      else {
      }
      WHE136_resolve_predef_types_(PATT_(self__, 28 + ((i__) << 2)),index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3086_: ;

   ret0__:
   return;
}

LST126_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3087_;
      }
      else {
      }
      WHE136_semant_(PATT_(self__, 28 + ((i__) << 2)),symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3087_: ;

   ret0__:
   return;
}

ptr LST126_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LST126_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

LST126_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr LST126_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

LST126_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

LST126_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST126_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST126_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST126_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST126_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,24))) {
         goto goto_tag_3088_;
      }
      else {
      }
      PATT_(self__, 28 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_3088_: ;

   ret0__:
   return;
}

ptr LST126_push_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;

   if ((IATT_(self__,12) < IATT_(self__,24))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,24)),0);
   }
   PATT_(res__, 28 + ((IATT_(self__,12)) << 2)) = (ptr)e__;
   IATT_(res__,12) = (int)(IATT_(res__,12) + 1);

   ret0__:
   return (res__);
}

int LST126_size_(self__)
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

char LST126_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST126_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST126_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,12) = (int)(IATT_(self__,12) - 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,12)) << 2));
      PATT_(self__, 28 + ((IATT_(self__,12)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LST126_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST126_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 28 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LST126_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST126_curr_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((IATT_(self__,16) < IATT_(self__,12))) {
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST126_next_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,16) + 1) < IATT_(self__,12))) {
      IATT_(self__,16) = (int)(IATT_(self__,16) + 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST126_prev_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,16) - 1) >= 0)) {
      IATT_(self__,16) = (int)(IATT_(self__,16) - 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST126_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST126_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 28 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST126_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST126_append_(self__,list__)
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
         goto goto_tag_3089_;
      }
      else {
      }
      res__ = (ptr)LST126_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3089_: ;

   ret0__:
   return (res__);
}

ptr LST126_union_(self__,list__)
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
         goto goto_tag_3090_;
      }
      else {
      }
      if (LST126_not_in_(self__,PATT_(list__, 28 + ((i__) << 2)))) {
         res__ = (ptr)LST126_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3090_: ;

   ret0__:
   return (res__);
}

char LST126_not_in_(self__,e__)
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
         goto goto_tag_3091_;
      }
      else {
      }
      if ((PATT_(self__, 28 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3091_: ;

   ret0__:
   return (res__);
}

int LST126_contains_(self__,e__)
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
         goto goto_tag_3092_;
      }
      else {
      }
      if ((PATT_(self__, 28 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3092_: ;

   ret0__:
   return (res__);
}

LST126_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3093_;
      }
      else {
      }
      WHE136_cprint_code_(PATT_(self__, 28 + ((i__) << 2)),outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3093_: ;

   ret0__:
   return;
}

int LST126_get_goto_tag_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((! (IATT_(self__,20) > 0))) {
      IATT_(self__,20) = (int)GLO94_global_key_(0);
   }
   else {
   }
   res__ = (int)IATT_(self__,20);

   ret0__:
   return (res__);
}

LST126_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3094_;
      }
      else {
      }
      WHE136_validate_dispatches_and_get_ext_strs_(PATT_(self__, 28 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3094_: ;

   ret0__:
   return;
}

ptr LST126_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3095_;
      }
      else {
      }
      if ((res__ == 0)) {
         res__ = (ptr)WHE136_gen_temps_(PATT_(self__, 28 + ((i__) << 2)));
      }
      else {
         res__ = (ptr)LIS98_append_(res__,WHE136_gen_temps_(PATT_(self__, 28 + ((i__) << 2))));
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3095_: ;

   ret0__:
   return (res__);
}

LST126_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{
   int    i__ = S_int_VOID_;
   ptr    bl__ = 0;

   i__ = (int)0;
   bl__ = (ptr)block__;
   if ((IATT_(self__,8) != 0)) {
      bl__ = (ptr)self__;
   }
   else {
   }
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3096_;
      }
      else {
      }
      WHE136_gen_goto_tags_(PATT_(self__, 28 + ((i__) << 2)),bl__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3096_: ;

   ret0__:
   return;
}

