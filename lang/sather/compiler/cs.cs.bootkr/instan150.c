/* instan150.c : Sather class: INSTANT_TYPEOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_s_();
extern CLA148_mark_is_used_();
extern ptr CLA148_full_name_();
extern char TYP149_double_type_p_();
extern char TYP149_real_type_p_();
extern char TYP149_int_type_p_();
extern CLA148_cprint_ctype_();
extern int TYP149_inst_ind_();
extern int TYP149_ctype_();
extern char TYP149_is_dispatched_();
extern char TYP149_nonptr_p_();
extern ptr STR20_c_();
extern char INT164_get_();
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ int C_T168_c_void_;
extern /*shared*/ ptr GLO68_void_classob_s_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ ptr GLO68_double_typeob_s_;
extern /*shared*/ ptr GLO68_real_typeob_s_;
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*constant*/ int RES71_array_ind_;
extern /*constant*/ int RES71_array2_ind_;
extern /*constant*/ int RES71_array3_ind_;
extern /*constant*/ int RES71_array4_ind_;
extern /*shared*/ int GLO68_rt_type_;
extern /*shared*/ ptr GLO68_voidtype_s_;
extern ptr CLA93_at_index_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern /*constant*/ int RES97_OB_ici_;
extern /*constant*/ int RES97_C_ici_;
extern /*constant*/ int RES97_SYS_ici_;
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern /*constant*/ int RES97_SELF_TYPE_ici_;
extern /*constant*/ int RES97_INT_ici_;
extern /*constant*/ int RES97_REAL_ici_;
extern /*constant*/ int RES97_DOUBLE_ici_;
extern /*constant*/ int RES97_BOOL_ici_;
extern /*constant*/ int RES97_CHAR_ici_;
extern /*constant*/ int RES97_STR_ici_;
extern ptr SAT99_s_();
extern char LST102_param_type_conforms_to_();
extern /*constant*/ int RES97_FOB_ici_;
#include "macros_.h"



/*constant*/ int INS150_print_indent_ = 2;
ptr INS150_create_();
INS150_out_of_line_();
ptr INS150_dup_();
INS150_put_kwdname_();
ptr INS150_sather_code_();
ptr INS150_initialize_();
INS150_resolve_predef_types_();
INS150_semant_();
ptr INS150_typeof_();
int INS150_get_offset_();
INS150_cprint_offset_();
ptr INS150_get_constval_();
INS150_cont_cprint_code_();
INS150_cprint_cname_();
INS150_cprint_extern_();
INS150_cprint_access_value_();
INS150_cprint_init_code_();
int INS150_inst_ind_();
ptr INS150_inst_cls_();
char INS150_is_dispatched_();
ptr INS150_dispatched_();
ptr INS150_undispatched_();
char INS150_arithtype_();
char INS150_int_type_p_();
char INS150_bool_type_p_();
char INS150_char_type_p_();
char INS150_real_type_p_();
char INS150_double_type_p_();
char INS150_str_type_p_();
char INS150_nonptr_p_();
ptr INS150_resolve_arithtype_();
int INS150_ctype_();
INS150_cprint_ctype_();
INS150_cprint_void_();
char INS150_array_type_p_();
char INS150_array2_type_p_();
char INS150_array3_type_p_();
char INS150_array4_type_p_();
ptr INS150_rettype_();
ptr INS150_paramstype_();
char INS150_conforms_to_();
char INS150_param_type_conforms_to_();
ptr INS150_full_name_();
INS150_update_rt_type_();
int INS150_my_basic_type_();
extern int attr_ent_INS150[];

ptr INS150_create_(self__,index__,ln__)
ptr self__;
int index__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(150,1);
   IATT_(res__,12) = (int)index__;
   IATT_(res__,8) = (int)ln__;
   IATT_(res__,20) = (int)(- 1);

   ret0__:
   return (res__);
}

INS150_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,8) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,8));

   ret0__:
   return;
}

ptr INS150_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)INS150_create_(self__,IATT_(self__,12),IATT_(self__,8));

   ret0__:
   return (res__);
}

INS150_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl4048_;
   static int gl4049_;
   static union dtype_ gl4050_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4048_ = x__;
   cache_dispatch_(gl4048_,796,gl4049_,INTVAL_(gl4050_));
   IATT_(gl4048_,INTVAL_(gl4050_)) = (int)nm__;

   ret0__:
   return;
}

ptr INS150_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr INS150_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

INS150_resolve_predef_types_(self__,new_index__)
ptr self__;
int new_index__;
{
   SATHER_STR_(20,34,ls2698_,"(INSTANT_TYPEOB_S): Unknown class");
   SATHER_STR_(20,37,ls2699_,"(INSTANT_TYPEOB_S): Abstract class \"");
   SATHER_STR_(20,31,ls2700_,"\" is usable as dispatched only");
   SATHER_STR_(20,21,ls2701_,"(INSTANT_TYPEOB_S):\"");
   SATHER_STR_(20,20,ls2702_,"(INSTANT_TYPEOB_S):");
   SATHER_STR_(20,37,ls2703_," is an invalid type for declarations");
   SATHER_STR_(20,68,ls2704_,"(INSTANT_TYPEOB_S): $SAME is illegal; usually you really want SAME.");
   ptr    cls__ = 0;
   int    i__ = S_int_VOID_;

   cls__ = (ptr)INS150_inst_cls_(self__);
   if ((cls__ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,8),STR20_s_(STR20_create_(0),(ptr)(&ls2698_)));
   }
   else {
      CLA148_mark_is_used_(cls__);
   }
   if ((CATT_(cls__,4) & (! INS150_is_dispatched_(self__)))) {
      ERR96_format_error_msg_(0,IATT_(self__,8),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2699_)),CLA148_full_name_(INS150_inst_cls_(self__))),(ptr)(&ls2700_)));
   }
   else {
   }
   i__ = (int)INS150_inst_ind_(self__);
   if (((i__ == RES97_OB_ici_) & (! INS150_is_dispatched_(self__)))) {
      ERR96_format_error_msg_(0,IATT_(self__,8),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2701_)),CLA148_full_name_(INS150_inst_cls_(self__))),(ptr)(&ls2700_)));
   }
   else {
   }
   if ((((i__ == RES97_C_ici_) | (i__ == RES97_SYS_ici_)) | (i__ == RES97_UNDEFINE_ici_))) {
      ERR96_format_error_msg_(0,IATT_(self__,8),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2702_)),CLA148_full_name_(INS150_inst_cls_(self__))),(ptr)(&ls2703_)));
      IATT_(self__,12) = (int)RES97_OB_ici_;
      (void)INS150_dispatched_(self__);
   }
   else {
      if ((i__ == RES97_SELF_TYPE_ici_)) {
         if (INS150_is_dispatched_(self__)) {
            IATT_(self__,12) = (int)new_index__;
            (void)INS150_dispatched_(self__);
            ERR96_format_error_msg_(0,IATT_(self__,8),STR20_s_(STR20_create_(0),(ptr)(&ls2704_)));
         }
         else {
            IATT_(self__,12) = (int)new_index__;
            CATT_(self__,4) = (char)1;
         }
      }
      else {
      }
   }

   ret0__:
   return;
}

INS150_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{


   ret0__:
   return;
}

ptr INS150_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int INS150_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

INS150_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr INS150_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

INS150_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{


   ret0__:
   return;
}

INS150_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

INS150_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

INS150_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

INS150_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

int INS150_inst_ind_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,12);
   if ((res__ < 0)) {
      res__ = (int)(- res__);
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr INS150_inst_cls_(self__)
ptr self__;
{
   ptr res__ = 0;

   if ((IATT_(self__,12) == 0)) {
      res__ = (ptr)GLO68_void_classob_s_;
   }
   else {
      if ((IATT_(self__,12) < 0)) {
         res__ = (ptr)CLA93_at_index_(GLO68_class_inst_,(- IATT_(self__,12)));
      }
      else {
         res__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(self__,12));
      }
   }

   ret0__:
   return (res__);
}

char INS150_is_dispatched_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   if (((IATT_(self__,12) < 0) | CATT_(self__,5))) {
      res__ = (char)1;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr INS150_dispatched_(self__)
ptr self__;
{
   ptr res__ = 0;

   CATT_(self__,5) = (char)1;
   if ((IATT_(self__,12) > 0)) {
      IATT_(self__,12) = (int)(- IATT_(self__,12));
   }
   else {
   }
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr INS150_undispatched_(self__)
ptr self__;
{
   ptr res__ = 0;

   CATT_(self__,5) = (char)0;
   if ((IATT_(self__,12) < 0)) {
      IATT_(self__,12) = (int)(- IATT_(self__,12));
   }
   else {
   }
   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

char INS150_arithtype_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;
   int    in__ = S_int_VOID_;

   co__ = (ptr)INS150_inst_cls_(self__);
   in__ = (int)INS150_inst_ind_(self__);
   if ((((in__ == RES97_INT_ici_) | (in__ == RES97_REAL_ici_)) | (in__ == RES97_DOUBLE_ici_))) {
      res__ = (char)1;
   }
   else {
      if ((((IATT_(co__,96) == RES97_INT_ici_) | (IATT_(co__,96) == RES97_REAL_ici_)) | (IATT_(co__,96) == RES97_DOUBLE_ici_))) {
         res__ = (char)1;
      }
      else {
         res__ = (char)0;
      }
   }

   ret0__:
   return (res__);
}

char INS150_int_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)((INS150_inst_ind_(self__) == RES97_INT_ici_) | (IATT_(co__,96) == RES97_INT_ici_));

   ret0__:
   return (res__);
}

char INS150_bool_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)((INS150_inst_ind_(self__) == RES97_BOOL_ici_) | (IATT_(co__,96) == RES97_BOOL_ici_));

   ret0__:
   return (res__);
}

char INS150_char_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)((INS150_inst_ind_(self__) == RES97_CHAR_ici_) | (IATT_(co__,96) == RES97_CHAR_ici_));

   ret0__:
   return (res__);
}

char INS150_real_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)((INS150_inst_ind_(self__) == RES97_REAL_ici_) | (IATT_(co__,96) == RES97_REAL_ici_));

   ret0__:
   return (res__);
}

char INS150_double_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)((INS150_inst_ind_(self__) == RES97_DOUBLE_ici_) | (IATT_(co__,96) == RES97_DOUBLE_ici_));

   ret0__:
   return (res__);
}

char INS150_str_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)((INS150_inst_ind_(self__) == RES97_STR_ici_) | (IATT_(co__,96) == RES97_STR_ici_));

   ret0__:
   return (res__);
}

char INS150_nonptr_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;
   int    basic__ = S_int_VOID_;
   int    in__ = S_int_VOID_;

   co__ = (ptr)INS150_inst_cls_(self__);
   basic__ = (int)IATT_(co__,96);
   in__ = (int)INS150_inst_ind_(self__);
   if ((((((in__ == RES97_BOOL_ici_) | (in__ == RES97_CHAR_ici_)) | (in__ == RES97_INT_ici_)) | (in__ == RES97_REAL_ici_)) | (in__ == RES97_DOUBLE_ici_))) {
      res__ = (char)1;
   }
   else {
      if ((((((basic__ == RES97_BOOL_ici_) | (basic__ == RES97_CHAR_ici_)) | (basic__ == RES97_INT_ici_)) | (basic__ == RES97_REAL_ici_)) | (basic__ == RES97_DOUBLE_ici_))) {
         res__ = (char)1;
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

ptr INS150_resolve_arithtype_(self__,tp__)
ptr self__;
ptr tp__;
{
   ptr res__ = 0;
   ptr gl4051_;
   static int gl4052_;
   static union dtype_ gl4053_;
   char gl410_;
   ptr gl4054_;
   static int gl4055_;
   static union dtype_ gl4056_;
   char gl411_;
   ptr gl4057_;
   static int gl4058_;
   static union dtype_ gl4059_;
   char gl412_;

   gl4051_ = tp__;
   cache_dispatch_(gl4051_,1887,gl4052_,INTVAL_(gl4053_));
   gl410_ = CFN_(gl4053_)(gl4051_);
   if ((gl410_ | INS150_double_type_p_(self__))) {
      res__ = (ptr)GLO68_double_typeob_s_;
   }
   else {
      gl4054_ = tp__;
      cache_dispatch_(gl4054_,1886,gl4055_,INTVAL_(gl4056_));
      gl411_ = CFN_(gl4056_)(gl4054_);
      if ((gl411_ | INS150_real_type_p_(self__))) {
         res__ = (ptr)GLO68_real_typeob_s_;
      }
      else {
         gl4057_ = tp__;
         cache_dispatch_(gl4057_,1778,gl4058_,INTVAL_(gl4059_));
         gl412_ = CFN_(gl4059_)(gl4057_);
         if ((gl412_ & INS150_int_type_p_(self__))) {
            res__ = (ptr)GLO68_int_typeob_s_;
         }
         else {
            res__ = (ptr)GLO68_ob_typeob_s_;
         }
      }
   }

   ret0__:
   return (res__);
}

int INS150_ctype_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(INS150_inst_cls_(self__),104);

   ret0__:
   return (res__);
}

INS150_cprint_ctype_(self__,outfile__)
ptr self__;
ptr outfile__;
{

   CLA148_cprint_ctype_(INS150_inst_cls_(self__),outfile__);

   ret0__:
   return;
}

INS150_cprint_void_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls2693_,"S_");
   SATHER_STR_(20,7,ls2694_,"_VOID_");
   SATHER_STR_(20,2,ls1692_,"0");

   if (INS150_nonptr_p_(self__)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2693_));
      INS150_cprint_ctype_(self__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls2694_));
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls1692_));
   }

   ret0__:
   return;
}

char INS150_array_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;
   ptr    bco__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)(IATT_(PATT_(co__,28),12) == 38);
   if ((! res__)) {
      if ((IATT_(co__,96) != 0)) {
         bco__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(co__,96));
         res__ = (char)(IATT_(PATT_(bco__,28),12) == 38);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

char INS150_array2_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;
   ptr    bco__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)(IATT_(PATT_(co__,28),12) == 39);
   if ((! res__)) {
      if ((IATT_(co__,96) != 0)) {
         bco__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(co__,96));
         res__ = (char)(IATT_(PATT_(bco__,28),12) == 39);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

char INS150_array3_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;
   ptr    bco__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)(IATT_(PATT_(co__,28),12) == 40);
   if ((! res__)) {
      if (((IATT_(co__,96) != 0) & (! res__))) {
         bco__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(co__,96));
         res__ = (char)(IATT_(PATT_(bco__,28),12) == 40);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

char INS150_array4_type_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr    co__ = 0;
   ptr    bco__ = 0;

   co__ = (ptr)INS150_inst_cls_(self__);
   res__ = (char)(IATT_(PATT_(co__,28),12) == 41);
   if ((! res__)) {
      if ((IATT_(co__,96) != 0)) {
         bco__ = (ptr)CLA93_at_index_(GLO68_class_inst_,IATT_(co__,96));
         res__ = (char)(IATT_(PATT_(bco__,28),12) == 41);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr INS150_rettype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr INS150_paramstype_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

char INS150_conforms_to_(self__,tp__)
ptr self__;
ptr tp__;
{
   char res__ = S_char_VOID_;
   ptr gl4060_;
   static int gl4061_;
   static union dtype_ gl4062_;
   ptr gl4063_;
   static int gl4064_;
   static union dtype_ gl4065_;
   ptr gl4066_;
   static int gl4067_;
   static union dtype_ gl4068_;
   ptr gl4069_;
   static int gl4070_;
   static union dtype_ gl4071_;
   ptr gl4072_;
   static int gl4073_;
   static union dtype_ gl4074_;
   ptr gl4075_;
   static int gl4076_;
   static union dtype_ gl4077_;
   char gl413_;
   int    my_index__ = S_int_VOID_;
   int    other_index__ = S_int_VOID_;
   int    other_ctype__ = S_int_VOID_;
   int    my_ctype__ = S_int_VOID_;
   ptr    co2__ = 0;
   ptr    co3__ = 0;
   int    psz__ = S_int_VOID_;
   ptr    co_self__ = 0;
   ptr    co_other__ = 0;
   ptr    co1__ = 0;

   GLO68_rt_type_ = (int)0;
   if ((tp__ == 0)) {
      if ((self__ == 0)) {
         res__ = (char)1;
      }
      else {
      }
      goto ret0__;
   }
   else {
   }
   if ((tp__ == GLO68_voidtype_s_)) {
      goto ret0__;
   }
   else {
   }
   if ((self__ == GLO68_voidtype_s_)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   my_index__ = (int)INS150_inst_ind_(self__);
   gl4060_ = tp__;
   cache_dispatch_(gl4060_,1547,gl4061_,INTVAL_(gl4062_));
   other_index__ = (int)IFN_(gl4062_)(gl4060_);
   gl4063_ = tp__;
   cache_dispatch_(gl4063_,1887,gl4064_,INTVAL_(gl4065_));
   if (CFN_(gl4065_)(gl4063_)) {
      if ((INS150_real_type_p_(self__) | INS150_int_type_p_(self__))) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
   }
   else {
   }
   gl4066_ = tp__;
   cache_dispatch_(gl4066_,1886,gl4067_,INTVAL_(gl4068_));
   if (CFN_(gl4068_)(gl4066_)) {
      if ((INS150_int_type_p_(self__) | INS150_double_type_p_(self__))) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
   }
   else {
   }
   gl4069_ = tp__;
   cache_dispatch_(gl4069_,374,gl4070_,INTVAL_(gl4071_));
   other_ctype__ = (int)IFN_(gl4071_)(gl4069_);
   my_ctype__ = (int)INS150_ctype_(self__);
   if (((((other_ctype__ != 1) & (other_ctype__ != 6)) & (my_ctype__ != 1)) & (my_ctype__ != 6))) {
      if ((other_ctype__ == my_ctype__)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
   }
   else {
   }
   gl4072_ = tp__;
   cache_dispatch_(gl4072_,1511,gl4073_,INTVAL_(gl4074_));
   if (CFN_(gl4074_)(gl4072_)) {
      if ((other_index__ == RES97_OB_ici_)) {
         if ((! INS150_nonptr_p_(self__))) {
            res__ = (char)1;
         }
         else {
         }
         goto ret0__;
      }
      else {
      }
      if ((my_index__ == other_index__)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
      co2__ = (ptr)CLA93_at_index_(GLO68_class_inst_,other_index__);
      if (INT164_get_(PATT_(co2__,76),my_index__)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
      if (INS150_is_dispatched_(self__)) {
         co3__ = (ptr)CLA93_at_index_(GLO68_class_inst_,my_index__);
         if (INT164_get_(PATT_(co3__,76),other_index__)) {
            res__ = (char)1;
            INS150_update_rt_type_(self__,(- other_index__));
            goto ret0__;
         }
         else {
         }
      }
      else {
      }
   }
   else {
      if ((my_index__ == other_index__)) {
         res__ = (char)1;
         if (INS150_is_dispatched_(self__)) {
            INS150_update_rt_type_(self__,other_index__);
         }
         else {
         }
         goto ret0__;
      }
      else {
      }
      if ((! INS150_is_dispatched_(self__))) {
         psz__ = S_int_VOID_;
         co_self__ = (ptr)CLA93_at_index_(GLO68_class_inst_,my_index__);
         co_other__ = (ptr)CLA93_at_index_(GLO68_class_inst_,other_index__);
         if ((PATT_(co_self__,28) == PATT_(co_other__,28))) {
            if ((CATT_(co_self__,5) & CATT_(co_other__,5))) {
               if (LST102_param_type_conforms_to_(PATT_(co_self__,40),PATT_(co_other__,40))) {
                  res__ = (char)1;
                  goto ret0__;
               }
               else {
               }
            }
            else {
            }
         }
         else {
         }
         goto ret0__;
      }
      else {
      }
      if ((my_index__ == RES97_OB_ici_)) {
         gl4075_ = tp__;
         cache_dispatch_(gl4075_,1549,gl4076_,INTVAL_(gl4077_));
         gl413_ = CFN_(gl4077_)(gl4075_);
         if ((! gl413_)) {
            res__ = (char)1;
            INS150_update_rt_type_(self__,other_index__);
         }
         else {
         }
         goto ret0__;
      }
      else {
      }
      co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,my_index__);
      if (INT164_get_(PATT_(co1__,76),other_index__)) {
         res__ = (char)1;
         INS150_update_rt_type_(self__,other_index__);
         goto ret0__;
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

char INS150_param_type_conforms_to_(self__,other__)
ptr self__;
ptr other__;
{
   char res__ = S_char_VOID_;
   ptr gl4078_;
   static int gl4079_;
   static union dtype_ gl4080_;
   ptr gl4081_;
   static int gl4082_;
   static union dtype_ gl4083_;
   char gl414_;
   int    my_index__ = S_int_VOID_;
   int    other_index__ = S_int_VOID_;
   ptr    other_co__ = 0;

   GLO68_rt_type_ = (int)0;
   if ((other__ == 0)) {
      if ((self__ == 0)) {
         res__ = (char)1;
      }
      else {
      }
      goto ret0__;
   }
   else {
   }
   if ((other__ == GLO68_voidtype_s_)) {
      goto ret0__;
   }
   else {
   }
   if ((self__ == GLO68_voidtype_s_)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   my_index__ = (int)INS150_inst_ind_(self__);
   gl4078_ = other__;
   cache_dispatch_(gl4078_,1547,gl4079_,INTVAL_(gl4080_));
   other_index__ = (int)IFN_(gl4080_)(gl4078_);
   gl4081_ = other__;
   cache_dispatch_(gl4081_,1511,gl4082_,INTVAL_(gl4083_));
   gl414_ = CFN_(gl4083_)(gl4081_);
   if ((! gl414_)) {
      if ((! INS150_is_dispatched_(self__))) {
         if ((my_index__ == other_index__)) {
            res__ = (char)1;
            goto ret0__;
         }
         else {
         }
      }
      else {
      }
   }
   else {
      if ((my_index__ == other_index__)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
      other_co__ = (ptr)CLA93_at_index_(GLO68_class_inst_,other_index__);
      if (INT164_get_(PATT_(other_co__,76),my_index__)) {
         res__ = (char)1;
         goto ret0__;
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

ptr INS150_full_name_(self__)
ptr self__;
{
   ptr res__ = 0;

   if (((IATT_(self__,12) < 0) | CATT_(self__,5))) {
      res__ = (ptr)STR20_s_(STR20_c_(STR20_create_(0),'$'),CLA148_full_name_(INS150_inst_cls_(self__)));
   }
   else {
      res__ = (ptr)CLA148_full_name_(INS150_inst_cls_(self__));
   }

   ret0__:
   return (res__);
}

INS150_update_rt_type_(self__,index__)
ptr self__;
int index__;
{

   if ((INS150_my_basic_type_(self__) != RES97_FOB_ici_)) {
      GLO68_rt_type_ = (int)index__;
   }
   else {
   }

   ret0__:
   return;
}

int INS150_my_basic_type_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;

   if ((IATT_(self__,20) == (- 1))) {
      IATT_(self__,20) = (int)IATT_(INS150_inst_cls_(self__),96);
   }
   else {
   }
   res__ = (int)IATT_(self__,20);

   ret0__:
   return (res__);
}

