/* lst_de129.c : Sather class: LST_DECLOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr SAT99_indent_();
extern ptr SAT99_s_();
extern ptr SAT99_c_();
extern DEC205_semant_();
extern DEC205_cprint_decln_();
extern DEC205_out_of_line_();
extern ptr DEC205_dup_();
extern DEC205_resolve_predef_types_();
extern DEC205_validate_dispatches_and_get_ext_strs_();
extern DEC205_cprint_cname_();
#include "macros_.h"



/*constant*/ int LST129_print_indent_ = 2;
ptr LST129_create_();
LST129_out_of_line_();
ptr LST129_dup_();
LST129_put_kwdname_();
ptr LST129_sather_code_();
ptr LST129_initialize_();
LST129_resolve_predef_types_();
LST129_semant_();
ptr LST129_typeof_();
int LST129_get_offset_();
LST129_cprint_offset_();
ptr LST129_get_constval_();
LST129_cont_cprint_code_();
LST129_cprint_cname_();
LST129_cprint_extern_();
LST129_cprint_access_value_();
LST129_cprint_init_code_();
LST129_clear_();
/*constant*/ int LST129_def_init_size_ = 5;
ptr LST129_push_();
int LST129_size_();
char LST129_is_empty_();
ptr LST129_pop_();
ptr LST129_top_();
LST129_init_iterate_();
ptr LST129_curr_item_();
ptr LST129_next_item_();
ptr LST129_prev_item_();
ptr LST129_push_unique_();
ptr LST129_append_();
ptr LST129_union_();
char LST129_not_in_();
int LST129_contains_();
LST129_cprint_code_();
LST129_validate_dispatches_and_get_ext_strs_();
LST129_cprint_names_();
LST129_check_separator_code_();
extern int attr_ent_LST129[];

ptr LST129_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(129,5,0);
   }
   else {
      res__ = (ptr)new1_(129,init_size__,0);
   }

   ret0__:
   return (res__);
}

LST129_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl3108_;
   static int gl3109_;
   static union dtype_ gl3110_;
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3111_;
      }
      else {
      }
      gl3108_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3108_,518,gl3109_,INTVAL_(gl3110_));
      VFN_(gl3110_)(gl3108_,fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3111_: ;

   ret0__:
   return;
}

ptr LST129_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl3112_;
   static int gl3113_;
   static union dtype_ gl3114_;
   ptr gl303_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST129_create_(self__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3115_;
      }
      else {
      }
      gl3112_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3112_,471,gl3113_,INTVAL_(gl3114_));
      gl303_ = PFN_(gl3114_)(gl3112_);
      res__ = (ptr)LST129_push_(res__,gl303_);
      i__ = (int)(i__ + 1);
   }
goto_tag_3115_: ;

   ret0__:
   return (res__);
}

LST129_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl3116_;
   static int gl3117_;
   static union dtype_ gl3118_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3116_ = x__;
   cache_dispatch_(gl3116_,796,gl3117_,INTVAL_(gl3118_));
   IATT_(gl3116_,INTVAL_(gl3118_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST129_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST129_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

LST129_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl3119_;
   static int gl3120_;
   static union dtype_ gl3121_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3122_;
      }
      else {
      }
      gl3119_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3119_,522,gl3120_,INTVAL_(gl3121_));
      VFN_(gl3121_)(gl3119_,index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3122_: ;

   ret0__:
   return;
}

LST129_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   ptr gl3123_;
   static int gl3124_;
   static union dtype_ gl3125_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3126_;
      }
      else {
      }
      gl3123_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3123_,588,gl3124_,INTVAL_(gl3125_));
      VFN_(gl3125_)(gl3123_,symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3126_: ;

   ret0__:
   return;
}

ptr LST129_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LST129_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

LST129_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr LST129_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

LST129_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

LST129_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST129_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST129_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST129_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST129_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,28))) {
         goto goto_tag_3127_;
      }
      else {
      }
      PATT_(self__, 32 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_3127_: ;

   ret0__:
   return;
}

ptr LST129_push_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;

   if ((IATT_(self__,12) < IATT_(self__,28))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,28)),0);
   }
   PATT_(res__, 32 + ((IATT_(self__,12)) << 2)) = (ptr)e__;
   IATT_(res__,12) = (int)(IATT_(res__,12) + 1);

   ret0__:
   return (res__);
}

int LST129_size_(self__)
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

char LST129_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST129_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST129_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,12) = (int)(IATT_(self__,12) - 1);
      res__ = (ptr)PATT_(self__, 32 + ((IATT_(self__,12)) << 2));
      PATT_(self__, 32 + ((IATT_(self__,12)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LST129_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST129_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 32 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LST129_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST129_curr_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((IATT_(self__,16) < IATT_(self__,12))) {
      res__ = (ptr)PATT_(self__, 32 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST129_next_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,16) + 1) < IATT_(self__,12))) {
      IATT_(self__,16) = (int)(IATT_(self__,16) + 1);
      res__ = (ptr)PATT_(self__, 32 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST129_prev_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,16) - 1) >= 0)) {
      IATT_(self__,16) = (int)(IATT_(self__,16) - 1);
      res__ = (ptr)PATT_(self__, 32 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST129_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST129_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 32 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST129_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST129_append_(self__,list__)
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
         goto goto_tag_3128_;
      }
      else {
      }
      res__ = (ptr)LST129_push_(res__,PATT_(list__, 32 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3128_: ;

   ret0__:
   return (res__);
}

ptr LST129_union_(self__,list__)
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
         goto goto_tag_3129_;
      }
      else {
      }
      if (LST129_not_in_(self__,PATT_(list__, 32 + ((i__) << 2)))) {
         res__ = (ptr)LST129_push_(res__,PATT_(list__, 32 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3129_: ;

   ret0__:
   return (res__);
}

char LST129_not_in_(self__,e__)
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
         goto goto_tag_3130_;
      }
      else {
      }
      if ((PATT_(self__, 32 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3130_: ;

   ret0__:
   return (res__);
}

int LST129_contains_(self__,e__)
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
         goto goto_tag_3131_;
      }
      else {
      }
      if ((PATT_(self__, 32 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3131_: ;

   ret0__:
   return (res__);
}

LST129_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl3132_;
   static int gl3133_;
   static union dtype_ gl3134_;
   ptr gl3135_;
   static int gl3136_;
   static union dtype_ gl3137_;
   int    i__ = S_int_VOID_;

   LST129_check_separator_code_(self__);
   i__ = (int)0;
   while (1) {
      if ((i__ >= (IATT_(self__,12) - 1))) {
         goto goto_tag_3138_;
      }
      else {
      }
      (void)SAT99_indent_(outfile__);
      gl3132_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3132_,654,gl3133_,INTVAL_(gl3134_));
      VFN_(gl3134_)(gl3132_,outfile__);
      (void)SAT99_s_(outfile__,PATT_(self__,20));
      i__ = (int)(i__ + 1);
   }
goto_tag_3138_: ;
   if ((i__ < IATT_(self__,12))) {
      (void)SAT99_indent_(outfile__);
      gl3135_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3135_,654,gl3136_,INTVAL_(gl3137_));
      VFN_(gl3137_)(gl3135_,outfile__);
      (void)SAT99_s_(outfile__,PATT_(self__,24));
   }
   else {
   }

   ret0__:
   return;
}

LST129_validate_dispatches_and_get_ext_strs_(self__)
ptr self__;
{
   ptr gl3139_;
   static int gl3140_;
   static union dtype_ gl3141_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3142_;
      }
      else {
      }
      gl3139_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3139_,609,gl3140_,INTVAL_(gl3141_));
      VFN_(gl3141_)(gl3139_);
      i__ = (int)(i__ + 1);
   }
goto_tag_3142_: ;

   ret0__:
   return;
}

LST129_cprint_names_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl3143_;
   static int gl3144_;
   static union dtype_ gl3145_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3146_;
      }
      else {
      }
      gl3143_ = PATT_(self__, 32 + ((i__) << 2));
      cache_dispatch_(gl3143_,808,gl3144_,INTVAL_(gl3145_));
      VFN_(gl3145_)(gl3143_,outfile__);
      i__ = (int)(i__ + 1);
      if ((i__ < IATT_(self__,12))) {
         (void)SAT99_c_(outfile__,',');
      }
      else {
      }
   }
goto_tag_3146_: ;

   ret0__:
   return;
}

LST129_check_separator_code_(self__)
ptr self__;
{
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,1,ls1016_,"");

   if ((PATT_(self__,20) == 0)) {
      PATT_(self__,20) = (ptr)(ptr)(&ls650_);
   }
   else {
   }
   if ((PATT_(self__,24) == 0)) {
      if ((CATT_(PATT_(self__,20), 8 + ((0))) == ',')) {
         PATT_(self__,24) = (ptr)(ptr)(&ls1016_);
      }
      else {
         PATT_(self__,24) = (ptr)PATT_(self__,20);
      }
   }
   else {
   }

   ret0__:
   return;
}

