/* lst_st123.c : Sather class: LST_STMTOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr LIS98_append_();
extern ptr STM199_dup_();
extern STM199_resolve_predef_types_();
extern STM199_out_of_line_();
extern STM199_semant_();
extern STM199_cprint_code_();
extern STM199_validate_dispatches_and_get_ext_strs_();
extern ptr STM199_gen_temps_();
extern STM199_gen_goto_tags_();
extern int GLO94_global_key_();
#include "macros_.h"



/*constant*/ int LST123_print_indent_ = 2;
ptr LST123_create_();
LST123_out_of_line_();
ptr LST123_dup_();
LST123_put_kwdname_();
ptr LST123_sather_code_();
ptr LST123_initialize_();
LST123_resolve_predef_types_();
LST123_semant_();
ptr LST123_typeof_();
int LST123_get_offset_();
LST123_cprint_offset_();
ptr LST123_get_constval_();
LST123_cont_cprint_code_();
LST123_cprint_cname_();
LST123_cprint_extern_();
LST123_cprint_access_value_();
LST123_cprint_init_code_();
LST123_clear_();
/*constant*/ int LST123_def_init_size_ = 5;
ptr LST123_push_();
int LST123_size_();
char LST123_is_empty_();
ptr LST123_pop_();
ptr LST123_top_();
LST123_init_iterate_();
ptr LST123_curr_item_();
ptr LST123_next_item_();
ptr LST123_prev_item_();
ptr LST123_push_unique_();
ptr LST123_append_();
ptr LST123_union_();
char LST123_not_in_();
int LST123_contains_();
LST123_cprint_code_();
int LST123_get_goto_tag_();
LST123_validate_dispatches_and_get_ext_strs_();
ptr LST123_gen_temps_();
LST123_gen_goto_tags_();
extern int attr_ent_LST123[];

ptr LST123_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(123,5,0);
   }
   else {
      res__ = (ptr)new1_(123,init_size__,0);
   }

   ret0__:
   return (res__);
}

LST123_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl3018_;
   static int gl3019_;
   static union dtype_ gl3020_;
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3021_;
      }
      else {
      }
      gl3018_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3018_,518,gl3019_,INTVAL_(gl3020_));
      VFN_(gl3020_)(gl3018_,fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3021_: ;

   ret0__:
   return;
}

ptr LST123_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3022_;
   static int gl3023_;
   static union dtype_ gl3024_;
   ptr gl297_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST123_create_(self__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3025_;
      }
      else {
      }
      gl3022_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3022_,471,gl3023_,INTVAL_(gl3024_));
      gl297_ = PFN_(gl3024_)(gl3022_);
      res__ = (ptr)LST123_push_(res__,gl297_);
      i__ = (int)(i__ + 1);
   }
goto_tag_3025_: ;

   ret0__:
   return (res__);
}

LST123_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3026_;
   static int gl3027_;
   static union dtype_ gl3028_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3026_ = x__;
   cache_dispatch_(gl3026_,796,gl3027_,INTVAL_(gl3028_));
   IATT_(gl3026_,INTVAL_(gl3028_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST123_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST123_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

LST123_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl3029_;
   static int gl3030_;
   static union dtype_ gl3031_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3032_;
      }
      else {
      }
      gl3029_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3029_,522,gl3030_,INTVAL_(gl3031_));
      VFN_(gl3031_)(gl3029_,index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3032_: ;

   ret0__:
   return;
}

LST123_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   ptr gl3033_;
   static int gl3034_;
   static union dtype_ gl3035_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3036_;
      }
      else {
      }
      gl3033_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3033_,588,gl3034_,INTVAL_(gl3035_));
      VFN_(gl3035_)(gl3033_,symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3036_: ;

   ret0__:
   return;
}

ptr LST123_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LST123_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

LST123_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr LST123_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

LST123_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

LST123_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST123_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST123_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST123_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST123_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,24))) {
         goto goto_tag_3037_;
      }
      else {
      }
      PATT_(self__, 28 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_3037_: ;

   ret0__:
   return;
}

ptr LST123_push_(self__,e__)
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

int LST123_size_(self__)
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

char LST123_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST123_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST123_is_empty_(self__)) {
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

ptr LST123_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST123_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 28 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LST123_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST123_curr_item_(self__)
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

ptr LST123_next_item_(self__)
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

ptr LST123_prev_item_(self__)
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

ptr LST123_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST123_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 28 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST123_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST123_append_(self__,list__)
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
         goto goto_tag_3038_;
      }
      else {
      }
      res__ = (ptr)LST123_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3038_: ;

   ret0__:
   return (res__);
}

ptr LST123_union_(self__,list__)
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
         goto goto_tag_3039_;
      }
      else {
      }
      if (LST123_not_in_(self__,PATT_(list__, 28 + ((i__) << 2)))) {
         res__ = (ptr)LST123_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3039_: ;

   ret0__:
   return (res__);
}

char LST123_not_in_(self__,e__)
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
         goto goto_tag_3040_;
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
goto_tag_3040_: ;

   ret0__:
   return (res__);
}

int LST123_contains_(self__,e__)
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
         goto goto_tag_3041_;
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
goto_tag_3041_: ;

   ret0__:
   return (res__);
}

LST123_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl3042_;
   static int gl3043_;
   static union dtype_ gl3044_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3045_;
      }
      else {
      }
      gl3042_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3042_,1552,gl3043_,INTVAL_(gl3044_));
      VFN_(gl3044_)(gl3042_,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3045_: ;

   ret0__:
   return;
}

int LST123_get_goto_tag_(self__)
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

LST123_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   ptr gl3046_;
   static int gl3047_;
   static union dtype_ gl3048_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3049_;
      }
      else {
      }
      gl3046_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3046_,609,gl3047_,INTVAL_(gl3048_));
      VFN_(gl3048_)(gl3046_);
      i__ = (int)(i__ + 1);
   }
goto_tag_3049_: ;

   ret0__:
   return;
}

ptr LST123_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3050_;
   static int gl3051_;
   static union dtype_ gl3052_;
   ptr gl3053_;
   static int gl3054_;
   static union dtype_ gl3055_;
   ptr gl298_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3056_;
      }
      else {
      }
      if ((res__ == 0)) {
         gl3050_ = PATT_(self__, 28 + ((i__) << 2));
         cache_dispatch_(gl3050_,1582,gl3051_,INTVAL_(gl3052_));
         res__ = (ptr)PFN_(gl3052_)(gl3050_);
      }
      else {
         gl3053_ = PATT_(self__, 28 + ((i__) << 2));
         cache_dispatch_(gl3053_,1582,gl3054_,INTVAL_(gl3055_));
         gl298_ = PFN_(gl3055_)(gl3053_);
         res__ = (ptr)LIS98_append_(res__,gl298_);
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3056_: ;

   ret0__:
   return (res__);
}

LST123_gen_goto_tags_(self__,block__)
ptr self__;
ptr block__;
{
   ptr gl3057_;
   static int gl3058_;
   static union dtype_ gl3059_;
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
         goto goto_tag_3060_;
      }
      else {
      }
      gl3057_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl3057_,607,gl3058_,INTVAL_(gl3059_));
      VFN_(gl3059_)(gl3057_,bl__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3060_: ;

   ret0__:
   return;
}

