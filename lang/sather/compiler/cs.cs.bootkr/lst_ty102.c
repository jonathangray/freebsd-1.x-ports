/* lst_ty102.c : Sather class: LST_TYPEOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr SAT99_s_();
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern /*constant*/ int C_T168_c_ptr_;
extern int INT15_min_();
extern CLA148_cprint_ctype_();
extern TYP149_out_of_line_();
extern ptr TYP149_dup_();
extern TYP149_resolve_predef_types_();
extern TYP149_semant_();
extern char TYP149_conforms_to_();
extern char TYP149_param_type_conforms_to_();
extern ptr STR20_create_();
extern ptr TYP149_full_name_();
extern ptr STR20_s_();
#include "macros_.h"



/*constant*/ int LST102_print_indent_ = 2;
ptr LST102_create_();
LST102_out_of_line_();
ptr LST102_dup_();
LST102_put_kwdname_();
ptr LST102_sather_code_();
ptr LST102_initialize_();
LST102_resolve_predef_types_();
LST102_semant_();
ptr LST102_typeof_();
int LST102_get_offset_();
LST102_cprint_offset_();
ptr LST102_get_constval_();
LST102_cont_cprint_code_();
LST102_cprint_cname_();
LST102_cprint_extern_();
LST102_cprint_access_value_();
LST102_cprint_init_code_();
int LST102_inst_ind_();
ptr LST102_inst_cls_();
char LST102_is_dispatched_();
ptr LST102_dispatched_();
ptr LST102_undispatched_();
char LST102_arithtype_();
char LST102_int_type_p_();
char LST102_bool_type_p_();
char LST102_char_type_p_();
char LST102_real_type_p_();
char LST102_double_type_p_();
char LST102_str_type_p_();
char LST102_nonptr_p_();
ptr LST102_resolve_arithtype_();
int LST102_ctype_();
LST102_cprint_ctype_();
LST102_cprint_void_();
char LST102_array_type_p_();
char LST102_array2_type_p_();
char LST102_array3_type_p_();
char LST102_array4_type_p_();
ptr LST102_rettype_();
ptr LST102_paramstype_();
char LST102_conforms_to_();
char LST102_param_type_conforms_to_();
ptr LST102_full_name_();
LST102_clear_();
/*constant*/ int LST102_def_init_size_ = 5;
ptr LST102_push_();
int LST102_size_();
char LST102_is_empty_();
ptr LST102_pop_();
ptr LST102_top_();
LST102_init_iterate_();
ptr LST102_curr_item_();
ptr LST102_next_item_();
ptr LST102_prev_item_();
ptr LST102_push_unique_();
ptr LST102_append_();
ptr LST102_union_();
char LST102_not_in_();
int LST102_contains_();
LST102_cprint_code_();
extern int attr_ent_LST102[];

ptr LST102_create_(self__,init_size__)
ptr self__;
int init_size__;
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(102,5,0);
   }
   else {
      res__ = (ptr)new1_(102,init_size__,0);
   }

   ret0__:
   return (res__);
}

LST102_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl1195_;
   static int gl1196_;
   static union dtype_ gl1197_;
   int    i__ = S_int_VOID_;

   IATT_(self__,12) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,12));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1198_;
      }
      else {
      }
      gl1195_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1195_,518,gl1196_,INTVAL_(gl1197_));
      VFN_(gl1197_)(gl1195_,fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_1198_: ;

   ret0__:
   return;
}

ptr LST102_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1199_;
   static int gl1200_;
   static union dtype_ gl1201_;
   ptr gl96_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST102_create_(self__,IATT_(self__,16));
   while (1) {
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1202_;
      }
      else {
      }
      gl1199_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1199_,471,gl1200_,INTVAL_(gl1201_));
      gl96_ = PFN_(gl1201_)(gl1199_);
      res__ = (ptr)LST102_push_(res__,gl96_);
      i__ = (int)(i__ + 1);
   }
goto_tag_1202_: ;

   ret0__:
   return (res__);
}

LST102_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl1203_;
   static int gl1204_;
   static union dtype_ gl1205_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1203_ = x__;
   cache_dispatch_(gl1203_,796,gl1204_,INTVAL_(gl1205_));
   IATT_(gl1203_,INTVAL_(gl1205_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST102_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST102_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

LST102_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl1206_;
   static int gl1207_;
   static union dtype_ gl1208_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1209_;
      }
      else {
      }
      gl1206_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1206_,522,gl1207_,INTVAL_(gl1208_));
      VFN_(gl1208_)(gl1206_,index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_1209_: ;

   ret0__:
   return;
}

LST102_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   ptr gl1210_;
   static int gl1211_;
   static union dtype_ gl1212_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1213_;
      }
      else {
      }
      gl1210_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1210_,588,gl1211_,INTVAL_(gl1212_));
      VFN_(gl1212_)(gl1210_,symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_1213_: ;

   ret0__:
   return;
}

ptr LST102_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LST102_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

LST102_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr LST102_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

LST102_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

LST102_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST102_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST102_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

LST102_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

int LST102_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ptr LST102_inst_cls_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

char LST102_is_dispatched_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr LST102_dispatched_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LST102_undispatched_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char LST102_arithtype_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_int_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_bool_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_char_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_real_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_double_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_str_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_nonptr_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr LST102_resolve_arithtype_(self__,tp__)
ptr self__;
ptr tp__;
{
   ptr res__ = 0;

   res__ = (ptr)GLO68_int_typeob_s_;

   ret0__:
   return (res__);
}

int LST102_ctype_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)1;

   ret0__:
   return (res__);
}

LST102_cprint_ctype_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   CLA148_cprint_ctype_(LST102_inst_cls_(self__),outfile__);

   ret0__:
   return;
}

LST102_cprint_void_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls2693_,"S_");
   SATHER_STR_(20,7,ls2694_,"_VOID_");
   SATHER_STR_(20,2,ls1692_,"0");

   if (LST102_nonptr_p_(self__)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2693_));
      LST102_cprint_ctype_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2694_));
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls1692_));
   }

   ret0__:
   return;
}

char LST102_array_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_array2_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_array3_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char LST102_array4_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr LST102_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST102_paramstype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

char LST102_conforms_to_(self__,tpp__)
ptr self__;
ptr tpp__;
{
   char res__ = S_char_VOID_;
   ptr gl1214_;
   static int gl1215_;
   static union dtype_ gl1216_;
   char gl97_;
   ptr    tp__ = 0;
   int    n__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   tp__ = (ptr)tpp__;
   if ((tp__ == 0)) {
      if ((self__ == 0)) {
         res__ = (char)1;
      }
      else {
         if ((IATT_(self__,16) == 0)) {
            res__ = (char)1;
         }
         else {
         }
      }
      goto ret0__;
   }
   else {
   }
   res__ = (char)1;
   n__ = (int)IATT_(self__,16);
   if ((IATT_(tp__,16) != IATT_(self__,16))) {
      res__ = (char)0;
      n__ = (int)INT15_min_(IATT_(self__,16),IATT_(tp__,16));
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= n__)) {
         goto goto_tag_1217_;
      }
      else {
      }
      gl1214_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1214_,903,gl1215_,INTVAL_(gl1216_));
      gl97_ = CFN_(gl1216_)(gl1214_,PATT_(tp__, 28 + ((i__) << 2)));
      if ((! gl97_)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1217_: ;

   ret0__:
   return (res__);
}

char LST102_param_type_conforms_to_(self__,tpp__)
ptr self__;
ptr tpp__;
{
   char res__ = S_char_VOID_;
   ptr gl1218_;
   static int gl1219_;
   static union dtype_ gl1220_;
   char gl98_;
   ptr    tp__ = 0;
   int    n__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   tp__ = (ptr)tpp__;
   if ((tp__ == 0)) {
      if ((self__ == 0)) {
         res__ = (char)1;
      }
      else {
         if ((IATT_(self__,16) == 0)) {
            res__ = (char)1;
         }
         else {
         }
      }
      goto ret0__;
   }
   else {
   }
   res__ = (char)1;
   n__ = (int)IATT_(self__,16);
   if ((IATT_(tp__,16) != IATT_(self__,16))) {
      res__ = (char)0;
      n__ = (int)INT15_min_(IATT_(self__,16),IATT_(tp__,16));
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= n__)) {
         goto goto_tag_1221_;
      }
      else {
      }
      gl1218_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1218_,555,gl1219_,INTVAL_(gl1220_));
      gl98_ = CFN_(gl1220_)(gl1218_,PATT_(tp__, 28 + ((i__) << 2)));
      if ((! gl98_)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1221_: ;

   ret0__:
   return (res__);
}

ptr LST102_full_name_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,4,ls2334_," X ");
   ptr gl1222_;
   static int gl1223_;
   static union dtype_ gl1224_;
   ptr gl99_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)STR20_create_(0);
   while (1) {
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1225_;
      }
      else {
      }
      gl1222_ = PATT_(self__, 28 + ((i__) << 2));
      cache_dispatch_(gl1222_,426,gl1223_,INTVAL_(gl1224_));
      gl99_ = PFN_(gl1224_)(gl1222_);
      res__ = (ptr)STR20_s_(res__,gl99_);
      i__ = (int)(i__ + 1);
      if ((i__ < IATT_(self__,16))) {
         res__ = (ptr)STR20_s_(res__,(ptr)(&ls2334_));
      }
      else {
      }
   }
goto_tag_1225_: ;

   ret0__:
   return (res__);
}

LST102_clear_(self__)
ptr self__;
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,24))) {
         goto goto_tag_1226_;
      }
      else {
      }
      PATT_(self__, 28 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_1226_: ;

   ret0__:
   return;
}

ptr LST102_push_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;

   if ((IATT_(self__,16) < IATT_(self__,24))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,24)),0);
   }
   PATT_(res__, 28 + ((IATT_(self__,16)) << 2)) = (ptr)e__;
   IATT_(res__,16) = (int)(IATT_(res__,16) + 1);

   ret0__:
   return (res__);
}

int LST102_size_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__,16);
   }

   ret0__:
   return (res__);
}

char LST102_is_empty_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,16) == 0);

   ret0__:
   return (res__);
}

ptr LST102_pop_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST102_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,16) = (int)(IATT_(self__,16) - 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,16)) << 2));
      PATT_(self__, 28 + ((IATT_(self__,16)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LST102_top_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (LST102_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 28 + (((IATT_(self__,16) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

LST102_init_iterate_(self__)
ptr self__;
{

   IATT_(self__,20) = (int)0;

   ret0__:
   return;
}

ptr LST102_curr_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((IATT_(self__,20) < IATT_(self__,16))) {
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,20)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST102_next_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,20) + 1) < IATT_(self__,16))) {
      IATT_(self__,20) = (int)(IATT_(self__,20) + 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,20)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST102_prev_item_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,20) - 1) >= 0)) {
      IATT_(self__,20) = (int)(IATT_(self__,20) - 1);
      res__ = (ptr)PATT_(self__, 28 + ((IATT_(self__,20)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST102_push_unique_(self__,e__)
ptr self__;
ptr e__;
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST102_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 28 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST102_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST102_append_(self__,list__)
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
   sz__ = (int)IATT_(list__,16);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1227_;
      }
      else {
      }
      res__ = (ptr)LST102_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_1227_: ;

   ret0__:
   return (res__);
}

ptr LST102_union_(self__,list__)
ptr self__;
ptr list__;
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,16);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1228_;
      }
      else {
      }
      if (LST102_not_in_(self__,PATT_(list__, 28 + ((i__) << 2)))) {
         res__ = (ptr)LST102_push_(res__,PATT_(list__, 28 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1228_: ;

   ret0__:
   return (res__);
}

char LST102_not_in_(self__,e__)
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
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1229_;
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
goto_tag_1229_: ;

   ret0__:
   return (res__);
}

int LST102_contains_(self__,e__)
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
      if ((i__ >= IATT_(self__,16))) {
         goto goto_tag_1230_;
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
goto_tag_1230_: ;

   ret0__:
   return (res__);
}

LST102_cprint_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

