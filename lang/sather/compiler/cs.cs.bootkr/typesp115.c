/* typesp115.c : Sather class: TYPESPEC_ARGS_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr INT15_to_s_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*constant*/ int RES71_res_ind_;
extern /*constant*/ int RES71_self_ind_;
extern /*constant*/ int RES71_void_ind_;
extern /*constant*/ int RES71_copy_ind_;
extern /*constant*/ int RES71_extend_ind_;
extern /*constant*/ int RES71_asize_ind_;
extern /*constant*/ int RES71_asize1_ind_;
extern /*constant*/ int RES71_asize2_ind_;
extern /*constant*/ int RES71_asize3_ind_;
extern /*constant*/ int RES71_asize4_ind_;
extern /*constant*/ int RES71_type_ind_;
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern /*constant*/ int RES71_new_ind_;
extern /*shared*/ char GLO68_pre_semant_;
extern /*shared*/ ptr GLO68_other_cnames_;
extern /*shared*/ int GLO68_g_tag_;
extern GLO94_check_f_ob_();
extern int GLO94_global_key_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_compiler_error_msg_();
extern /*constant*/ int RES97_OB_ici_;
extern ERR96_format_error_msg_();
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern /*constant*/ int RES97_SELF_TYPE_ici_;
extern ERR96_format_error_exit_();
extern /*constant*/ int RES97_FOB_ici_;
extern char GLO94_conform_tst_();
extern /*constant*/ int RES97_C_ici_;
extern int GLO94_key_of_class_feat_();
extern ptr SAT99_i_();
extern ptr SAT99_c_();
extern ptr SAT99_s_();
extern char GLO94_check_is_on_();
extern ptr LIS98_push_();
extern ptr LIS98_create_();
extern ptr INT106_create_();
extern GLO94_cprint_curr_exp_code_();
extern GLO94_cprint_ctemp_name_();
extern ptr SAT99_indent_();
extern char EXP117_to_be_pre_evaluated_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern ptr LST120_create_();
extern LST120_out_of_line_();
extern ptr LST120_dup_();
extern ptr LST120_sather_code_();
extern LST120_resolve_predef_types_();
extern LST120_semant_();
extern LST120_cprint_init_code_();
extern char LST120_valid_init_expr_();
extern ptr LST120_gen_temps_();
extern LST120_get_ext_strs_();
extern ptr SAT99_inc_ln_();
extern CLA148_mark_is_used_();
extern TYP149_out_of_line_();
extern ptr TYP149_dup_();
extern ptr TYP149_sather_code_();
extern int INS150_inst_ind_();
extern ptr INS150_dispatched_();
extern char INS150_is_dispatched_();
extern ptr TYP149_inst_cls_();
extern ptr CLA148_full_name_();
extern char TYP149_is_dispatched_();
extern ptr TYP149_undispatched_();
extern int TYP149_inst_ind_();
extern int TYP149_ctype_();
extern char TYP149_nonptr_p_();
extern char TYP149_int_type_p_();
extern ptr TYP149_paramstype_();
extern char INT164_get_();
extern ptr INT164_insert_();
extern ptr TYP149_full_name_();
extern ptr TYP149_rettype_();
extern int CON151_is_a_str_const_();
extern char TYP149_array_type_p_();
extern char TYP149_array2_type_p_();
extern char TYP149_array3_type_p_();
extern char TYP149_array4_type_p_();
extern char TYP149_real_type_p_();
extern char TYP149_double_type_p_();
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ ptr C_T168_c_ptr_name_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ ptr C_T168_c_char_name_;
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ ptr C_T168_c_int_name_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ ptr C_T168_c_float_name_;
extern /*constant*/ int C_T168_c_double_;
extern /*constant*/ ptr C_T168_c_double_name_;
extern ptr SYM186_get_feature_();
extern PRI187_cprint_new_();
extern ptr SEM188_typeof_();
extern SEM188_cprint_init_code_();
extern ptr SEM188_get_constval_();
extern SEM188_cprint_access_value_();
extern SEM188_cprint_cname_();
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int TYP115_print_indent_ = 2;
ptr TYP115_create_();
TYP115_out_of_line_();
ptr TYP115_dup_();
TYP115_put_kwdname_();
ptr TYP115_sather_code_();
ptr TYP115_initialize_();
TYP115_resolve_predef_types_();
TYP115_semant_();
ptr TYP115_typeof_();
int TYP115_get_offset_();
TYP115_cprint_offset_();
ptr TYP115_get_constval_();
TYP115_cont_cprint_code_();
TYP115_cprint_cname_();
TYP115_cprint_extern_();
TYP115_cprint_access_value_();
TYP115_cprint_init_code_();
char TYP115_valid_init_expr_();
char TYP115_assignable_p_();
ptr TYP115_gen_temps_();
ptr TYP115_expr_eval_constant_();
TYP115_get_ext_strs_();
char TYP115_to_be_pre_evaluated_();
char TYP115_access_value_only_();
TYP115_fob_error_();
TYP115_cprint_pre_code_();
TYP115_cprint_act_code_();
TYP115_cprint_cast_code_();
TYP115_cprint_ith_arg_pre_code_();
TYP115_cprint_ith_arg_act_code_();
/*constant*/ int TYP115_cont1_ = 1;
/*constant*/ int TYP115_cont2_ = 2;
/*constant*/ int TYP115_cont3_ = 3;
/*constant*/ int TYP115_cont4_ = 4;
/*constant*/ int TYP115_cont5_ = 5;
/*shared*/ char TYP115_init_expr_explicit_p_;
extern int attr_ent_TYP115[];

ptr TYP115_create_(self__,ts__,f__,alst__,ln__)
ptr self__;
ptr ts__;
int f__;
ptr alst__;
int ln__;
{
   ptr res__ = 0;
   int    sz__ = S_int_VOID_;

   res__ = (ptr)new_(115,0);
   PATT_(res__,28) = (ptr)ts__;
   IATT_(res__,32) = (int)f__;
   sz__ = S_int_VOID_;
   if ((alst__ != 0)) {
      PATT_(res__,36) = (ptr)alst__;
      sz__ = (int)IATT_(alst__,12);
   }
   else {
      PATT_(res__,36) = (ptr)LST120_create_(0,1);
   }
   IATT_(res__,60) = (int)ln__;
   PATT_(res__,52) = (ptr)new1_(163,sz__,1);
   PATT_(res__,56) = (ptr)new1_(163,sz__,1);

   ret0__:
   return (res__);
}

TYP115_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl2679_;
   static int gl2680_;
   static union dtype_ gl2681_;

   IATT_(self__,60) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,60));
   gl2679_ = PATT_(self__,28);
   cache_dispatch_(gl2679_,518,gl2680_,INTVAL_(gl2681_));
   VFN_(gl2681_)(gl2679_,fn__);
   LST120_out_of_line_(PATT_(self__,36),fn__);

   ret0__:
   return;
}

ptr TYP115_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl2682_;
   static int gl2683_;
   static union dtype_ gl2684_;
   ptr gl254_;
   ptr gl2685_;
   static int gl2686_;
   static union dtype_ gl2687_;
   ptr gl255_;

   if ((PATT_(self__,36) != 0)) {
      gl2682_ = PATT_(self__,28);
      cache_dispatch_(gl2682_,471,gl2683_,INTVAL_(gl2684_));
      gl254_ = PFN_(gl2684_)(gl2682_);
      res__ = (ptr)TYP115_create_(self__,gl254_,IATT_(self__,32),LST120_dup_(PATT_(self__,36)),IATT_(self__,60));
   }
   else {
      gl2685_ = PATT_(self__,28);
      cache_dispatch_(gl2685_,471,gl2686_,INTVAL_(gl2687_));
      gl255_ = PFN_(gl2687_)(gl2685_);
      res__ = (ptr)TYP115_create_(self__,gl255_,IATT_(self__,32),0,IATT_(self__,60));
   }

   ret0__:
   return (res__);
}

TYP115_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl2688_;
   static int gl2689_;
   static union dtype_ gl2690_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl2688_ = x__;
   cache_dispatch_(gl2688_,796,gl2689_,INTVAL_(gl2690_));
   IATT_(gl2688_,INTVAL_(gl2690_)) = (int)nm__;

   ret0__:
   return;
}

ptr TYP115_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,3,ls2054_,"::");
   ptr gl2691_;
   static int gl2692_;
   static union dtype_ gl2693_;
   ptr gl256_;

   gl2691_ = PATT_(self__,28);
   cache_dispatch_(gl2691_,801,gl2692_,INTVAL_(gl2693_));
   gl256_ = PFN_(gl2693_)(gl2691_);
   res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),gl256_),(ptr)(&ls2054_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32)));
   if ((PATT_(self__,36) != 0)) {
      res__ = (ptr)STR20_c_(STR20_s_(STR20_c_(res__,'('),LST120_sather_code_(PATT_(self__,36))),')');
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr TYP115_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

TYP115_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   SATHER_STR_(20,23,ls1673_,"TYPESPEC_ARGS_EXPROB_S");
   SATHER_STR_(20,25,ls2056_," in resolve_predef_types");
   SATHER_STR_(20,44,ls2057_,"(TYPESPEC_ARGS_EXPROB_S): Invalid use of OB");
   SATHER_STR_(20,50,ls2058_,"(TYPESPEC_ARGS_EXPROB_S): Invalid use of UNDEFINE");
   SATHER_STR_(20,51,ls2059_,"(TYPESPEC_ARGS_EXPROB_S): Unexpected missing class");
   SATHER_STR_(20,43,ls2060_,"(TYPESPEC_ARGS_EXPROB_S): Abstract class \"");
   SATHER_STR_(20,15,ls2061_,"\" used in call");
   ptr gl2694_;
   static int gl2695_;
   static union dtype_ gl2696_;
   int gl257_;
   ptr gl2697_;
   static int gl2698_;
   static union dtype_ gl2699_;
   ptr    i_typeob_s__ = 0;
   int    i__ = S_int_VOID_;
   ptr    cls__ = 0;

   i_typeob_s__ = (ptr)PATT_(self__,28);
   gl2694_ = PATT_(self__,28);
   gl257_ = TYPE_(gl2694_);
   if ((gl257_ != 150)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls1673_),STR20_s_(STR20_create_(0),(ptr)(&ls2056_)));
   }
   else {
   }
   i__ = (int)INS150_inst_ind_(i_typeob_s__);
   if ((i__ == RES97_OB_ici_)) {
      ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2057_)));
   }
   else {
      if ((i__ == RES97_UNDEFINE_ici_)) {
         ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2058_)));
         IATT_(i_typeob_s__,12) = (int)RES97_OB_ici_;
         PATT_(self__,28) = (ptr)INS150_dispatched_(i_typeob_s__);
      }
      else {
         if ((i__ == RES97_SELF_TYPE_ici_)) {
            if (INS150_is_dispatched_(i_typeob_s__)) {
               IATT_(i_typeob_s__,12) = (int)index__;
               PATT_(self__,28) = (ptr)INS150_dispatched_(i_typeob_s__);
            }
            else {
               IATT_(i_typeob_s__,12) = (int)index__;
            }
         }
         else {
         }
      }
   }
   gl2697_ = PATT_(self__,28);
   cache_dispatch_(gl2697_,418,gl2698_,INTVAL_(gl2699_));
   cls__ = (ptr)PFN_(gl2699_)(gl2697_);
   if ((cls__ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2059_)));
   }
   else {
      CLA148_mark_is_used_(cls__);
      if (CATT_(cls__,4)) {
         ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2060_)),CLA148_full_name_(cls__)),(ptr)(&ls2061_)));
      }
      else {
      }
   }
   if ((PATT_(self__,36) != 0)) {
      LST120_resolve_predef_types_(PATT_(self__,36),index__);
   }
   else {
   }

   ret0__:
   return;
}

TYP115_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,87,ls2064_,"(TYPESPEC_ARGS_EXPROB_S): Unexpected dispatched class for reference to class feature \"");
   SATHER_STR_(20,52,ls2066_,"(TYPESPEC_ARGS_EXPROB_S): Class has no symbol table");
   SATHER_STR_(20,28,ls2062_,"(TYPESPEC_ARGS_EXPROB_S): \"");
   SATHER_STR_(20,29,ls2067_,"\" is private routine feature");
   SATHER_STR_(20,31,ls2068_,"\" is private attribute feature");
   SATHER_STR_(20,30,ls2069_,"\" is private constant feature");
   SATHER_STR_(20,28,ls2070_,"\" is private shared feature");
   SATHER_STR_(20,32,ls2071_,"(TYPESPEC_ARGS_EXPROB_S): Arg #");
   SATHER_STR_(20,20,ls1807_," has no return type");
   SATHER_STR_(20,31,ls1926_,"\" is invalid in dot expression");
   SATHER_STR_(20,67,ls2072_,"(TYPESPEC_ARGS_EXPROB_S): Unexpected arguments supplied for \"type\"");
   SATHER_STR_(20,63,ls2073_,"(TYPESPEC_ARGS_EXPROB_S): Cannot use non-object type for \"new\"");
   SATHER_STR_(20,25,ls2074_,"\" gets non-integer arg #");
   SATHER_STR_(20,39,ls2075_,"(TYPESPEC_ARGS_EXPROB_S): \"new\" needs ");
   SATHER_STR_(20,27,ls1932_," argument(s) but is given ");
   SATHER_STR_(20,13,ls1933_," argument(s)");
   SATHER_STR_(20,31,ls2076_,"\" is an unknown identifier in ");
   SATHER_STR_(20,23,ls1673_,"TYPESPEC_ARGS_EXPROB_S");
   SATHER_STR_(20,32,ls2077_,"Invalid reference to variable \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,68,ls2078_,"(TYPESPEC_ARGS_EXPROB_S): Invalid external reference to attribute \"");
   SATHER_STR_(20,31,ls2079_,"\"'s unexpected args ignored [\"");
   SATHER_STR_(20,23,ls2080_,"\" is a shared/constant");
   SATHER_STR_(20,32,ls1879_,"\" gets incorrect number of args");
   SATHER_STR_(20,10,ls1882_,"<UNKNOWN>");
   SATHER_STR_(20,37,ls2081_,"(TYPESPEC_ARGS_EXPROB_S): Argument #");
   SATHER_STR_(20,22,ls1883_," has incorrect type (");
   SATHER_STR_(20,8,ls1884_,") for \"");
   SATHER_STR_(20,4,ls1885_,"\" (");
   SATHER_STR_(20,24,ls1735_,"\" has unknown referrent");
   ptr gl2700_;
   static int gl2701_;
   static union dtype_ gl2702_;
   ptr gl2703_;
   static int gl2704_;
   static union dtype_ gl2705_;
   ptr gl2706_;
   static int gl2707_;
   static union dtype_ gl2708_;
   ptr gl2709_;
   static int gl2710_;
   static union dtype_ gl2711_;
   ptr gl2712_;
   static int gl2713_;
   static union dtype_ gl2714_;
   ptr gl2715_;
   static int gl2716_;
   static union dtype_ gl2717_;
   ptr gl2718_;
   static int gl2719_;
   static union dtype_ gl2720_;
   ptr gl259_;
   ptr gl2721_;
   static int gl2722_;
   static union dtype_ gl2723_;
   ptr gl2724_;
   static int gl2725_;
   static union dtype_ gl2726_;
   ptr gl2727_;
   static int gl2728_;
   static union dtype_ gl2729_;
   ptr gl2730_;
   static int gl2731_;
   static union dtype_ gl2732_;
   ptr gl2733_;
   static int gl2734_;
   static union dtype_ gl2735_;
   ptr gl2736_;
   static int gl2737_;
   static union dtype_ gl2738_;
   ptr gl2739_;
   static int gl2740_;
   static union dtype_ gl2741_;
   ptr gl2742_;
   static int gl2743_;
   static union dtype_ gl2744_;
   char gl262_;
   ptr gl2745_;
   static int gl2746_;
   static union dtype_ gl2747_;
   ptr gl2748_;
   static int gl2749_;
   static union dtype_ gl2750_;
   ptr gl2751_;
   static int gl2752_;
   static union dtype_ gl2753_;
   int gl263_;
   ptr gl2754_;
   static int gl2755_;
   static union dtype_ gl2756_;
   ptr gl2757_;
   static int gl2758_;
   static union dtype_ gl2759_;
   ptr gl2760_;
   static int gl2761_;
   static union dtype_ gl2762_;
   ptr gl2763_;
   static int gl2764_;
   static union dtype_ gl2765_;
   ptr gl2766_;
   static int gl2767_;
   static union dtype_ gl2768_;
   ptr gl265_;
   ptr gl2769_;
   static int gl2770_;
   static union dtype_ gl2771_;
   ptr gl266_;
   ptr gl2772_;
   static int gl2773_;
   static union dtype_ gl2774_;
   ptr gl2775_;
   static int gl2776_;
   static union dtype_ gl2777_;
   ptr gl2778_;
   static int gl2779_;
   static union dtype_ gl2780_;
   ptr gl2781_;
   static int gl2782_;
   static union dtype_ gl2783_;
   ptr gl2784_;
   static int gl2785_;
   static union dtype_ gl2786_;
   ptr gl2787_;
   static int gl2788_;
   static union dtype_ gl2789_;
   ptr    obj_cls__ = 0;
   ptr    alt_symtab__ = 0;
   ptr    rout_feat__ = 0;
   ptr    attr_feat__ = 0;
   ptr    const_feat__ = 0;
   ptr    shared_feat__ = 0;
   int    curr_callee__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    descendents__ = 0;
   int    sz__ = S_int_VOID_;
   int    index__ = S_int_VOID_;
   int    num_args__ = S_int_VOID_;
   int    i_258_ = S_int_VOID_;
   int    i_260_ = S_int_VOID_;
   int    i_261_ = S_int_VOID_;
   int    i_264_ = S_int_VOID_;
   ptr    ptypes__ = 0;
   int    num_params__ = S_int_VOID_;
   ptr    tn1__ = 0;
   ptr    tn2__ = 0;
   ptr    co__ = 0;
   int    cls_ind__ = S_int_VOID_;
   int    k__ = S_int_VOID_;

   gl2700_ = PATT_(self__,28);
   cache_dispatch_(gl2700_,1511,gl2701_,INTVAL_(gl2702_));
   if (CFN_(gl2702_)(gl2700_)) {
      ERR96_format_error_msg_(0,IATT_(self__,60),STR20_c_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2064_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),'"'));
      gl2703_ = PATT_(self__,28);
      cache_dispatch_(gl2703_,2065,gl2704_,INTVAL_(gl2705_));
      PATT_(self__,28) = (ptr)PFN_(gl2705_)(gl2703_);
   }
   else {
   }
   gl2706_ = PATT_(self__,28);
   cache_dispatch_(gl2706_,418,gl2707_,INTVAL_(gl2708_));
   obj_cls__ = (ptr)PFN_(gl2708_)(gl2706_);
   alt_symtab__ = (ptr)PATT_(obj_cls__,48);
   if ((alt_symtab__ == 0)) {
      ERR96_format_error_exit_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2066_)));
   }
   else {
   }
   if ((IATT_(obj_cls__,96) == RES97_FOB_ici_)) {
      GLO94_check_f_ob_(0,self__,IATT_(self__,32),alt_symtab__);
   }
   else {
   }
   if ((PATT_(self__,40) == 0)) {
      PATT_(self__,40) = (ptr)SYM186_get_feature_(alt_symtab__,IATT_(self__,32));
      if ((PATT_(self__,40) != 0)) {
         gl2709_ = PATT_(self__,40);
         switch (TYPE_(gl2709_)) {
            case (156) :
               rout_feat__ = (ptr)PATT_(self__,40);
               CATT_(rout_feat__,11) = (char)1;
               if ((CATT_(rout_feat__,5) & (PATT_(rout_feat__,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2067_)));
               }
               else {
               }
               break;
            case (153) :
               attr_feat__ = (ptr)PATT_(self__,40);
               CATT_(attr_feat__,11) = (char)1;
               if ((CATT_(attr_feat__,5) & (PATT_(attr_feat__,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2068_)));
               }
               else {
               }
               break;
            case (151) :
               const_feat__ = (ptr)PATT_(self__,40);
               CATT_(const_feat__,11) = (char)1;
               if ((CATT_(const_feat__,5) & (PATT_(const_feat__,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2069_)));
               }
               else {
               }
               break;
            case (152) :
               shared_feat__ = (ptr)PATT_(self__,40);
               CATT_(shared_feat__,11) = (char)1;
               if ((CATT_(shared_feat__,5) & (PATT_(shared_feat__,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2070_)));
               }
               else {
               }
               break;
            default:
               ;
               ;
         }
      }
      else {
      }
   }
   else {
   }
   if ((PATT_(self__,40) != 0)) {
      gl2712_ = PATT_(self__,28);
      cache_dispatch_(gl2712_,1547,gl2713_,INTVAL_(gl2714_));
      curr_callee__ = (int)IFN_(gl2714_)(gl2712_);
      if ((! INT164_get_(PATT_(GLO68_curr_class_inst_,136),curr_callee__))) {
         PATT_(GLO68_curr_class_inst_,136) = (ptr)INT164_insert_(PATT_(GLO68_curr_class_inst_,136),curr_callee__);
      }
      else {
      }
      if ((! INT164_get_(PATT_(obj_cls__,140),IATT_(GLO68_curr_class_inst_,20)))) {
         PATT_(obj_cls__,140) = (ptr)INT164_insert_(PATT_(obj_cls__,140),IATT_(GLO68_curr_class_inst_,20));
      }
      else {
      }
      gl2715_ = PATT_(self__,28);
      cache_dispatch_(gl2715_,1511,gl2716_,INTVAL_(gl2717_));
      if (CFN_(gl2717_)(gl2715_)) {
         i__ = (int)0;
         descendents__ = (ptr)PATT_(obj_cls__,76);
         sz__ = (int)IATT_(descendents__,12);
         while (1) {
            if ((i__ >= sz__)) {
               goto goto_tag_2790_;
            }
            else {
            }
            index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
            if ((index__ > 0)) {
               PATT_(GLO68_curr_class_inst_,136) = (ptr)INT164_insert_(PATT_(GLO68_curr_class_inst_,136),index__);
            }
            else {
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_2790_: ;
      }
      else {
      }
   }
   else {
   }
   num_args__ = (int)0;
   if ((PATT_(self__,36) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,36),12);
      LST120_semant_(PATT_(self__,36),symtab__);
   }
   else {
   }
   i_258_ = S_int_VOID_;
   while (1) {
      if ((i_258_ >= num_args__)) {
         goto goto_tag_2791_;
      }
      else {
      }
      gl2718_ = PATT_(PATT_(self__,36), 28 + ((i_258_) << 2));
      cache_dispatch_(gl2718_,1577,gl2719_,INTVAL_(gl2720_));
      gl259_ = PATT_(gl2718_,INTVAL_(gl2720_));
      if ((gl259_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls2071_)),(i_258_ + 1)),(ptr)(&ls1807_)));
         gl2721_ = PATT_(PATT_(self__,36), 28 + ((i_258_) << 2));
         cache_dispatch_(gl2721_,1577,gl2722_,INTVAL_(gl2723_));
         PATT_(gl2721_,INTVAL_(gl2723_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
      }
      i_258_ = (int)(i_258_ + 1);
   }
goto_tag_2791_: ;
   i_260_ = (int)0;
   while (1) {
      if ((i_260_ >= num_args__)) {
         goto goto_tag_2792_;
      }
      else {
      }
      gl2724_ = PATT_(PATT_(self__,36), 28 + ((i_260_) << 2));
      cache_dispatch_(gl2724_,1678,gl2725_,INTVAL_(gl2726_));
      if (CFN_(gl2726_)(gl2724_)) {
         IATT_(PATT_(self__,52), 8 + ((i_260_) << 2)) = (int)GLO94_global_key_(0);
         gl2730_ = PATT_(PATT_(self__,36), 28 + ((i_260_) << 2));
         cache_dispatch_(gl2730_,1577,gl2731_,INTVAL_(gl2732_));
         gl2727_ = PATT_(gl2730_,INTVAL_(gl2732_));
         cache_dispatch_(gl2727_,374,gl2728_,INTVAL_(gl2729_));
         IATT_(PATT_(self__,56), 8 + ((i_260_) << 2)) = (int)IFN_(gl2729_)(gl2727_);
         CATT_(self__,4) = (char)1;
      }
      else {
      }
      i_260_ = (int)(i_260_ + 1);
   }
goto_tag_2792_: ;
   if ((PATT_(self__,40) == 0)) {
      switch (IATT_(self__,32)) {
         case (33) :
         case (34) :
         case (37) :
         case (28) :
         case (30) :
         case (23) :
         case (24) :
         case (25) :
         case (26) :
         case (27) :
            ERR96_format_error_exit_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls1926_)));
            break;
         case (32) :
            if ((num_args__ != 0)) {
               ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2072_)));
               PATT_(self__,36) = (ptr)0;
            }
            else {
            }
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            goto ret0__;
            break;
         case (31) :
            PATT_(self__,12) = (ptr)PATT_(self__,28);
            gl2733_ = PATT_(self__,12);
            cache_dispatch_(gl2733_,1549,gl2734_,INTVAL_(gl2735_));
            if (CFN_(gl2735_)(gl2733_)) {
               ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2073_)));
            }
            else {
            }
            gl2736_ = PATT_(self__,12);
            cache_dispatch_(gl2736_,418,gl2737_,INTVAL_(gl2738_));
            if ((num_args__ == IATT_(PFN_(gl2738_)(gl2736_),100))) {
               i_261_ = (int)0;
               while (1) {
                  if ((i_261_ >= num_args__)) {
                     goto goto_tag_2793_;
                  }
                  else {
                  }
                  gl2742_ = PATT_(PATT_(self__,36), 28 + ((i_261_) << 2));
                  cache_dispatch_(gl2742_,1577,gl2743_,INTVAL_(gl2744_));
                  gl2739_ = PATT_(gl2742_,INTVAL_(gl2744_));
                  cache_dispatch_(gl2739_,1778,gl2740_,INTVAL_(gl2741_));
                  gl262_ = CFN_(gl2741_)(gl2739_);
                  if ((! gl262_)) {
                     ERR96_format_error_msg_(0,IATT_(self__,60),STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2074_)),i_261_));
                  }
                  else {
                  }
                  i_261_ = (int)(i_261_ + 1);
               }
            goto_tag_2793_: ;
            }
            else {
               gl2745_ = PATT_(self__,12);
               cache_dispatch_(gl2745_,418,gl2746_,INTVAL_(gl2747_));
               ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls2075_)),IATT_(PFN_(gl2747_)(gl2745_),100)),(ptr)(&ls1932_)),num_args__),(ptr)(&ls1933_)));
            }
            goto ret0__;
            break;
         default:
            gl2748_ = PATT_(self__,28);
            cache_dispatch_(gl2748_,1547,gl2749_,INTVAL_(gl2750_));
            gl263_ = IFN_(gl2750_)(gl2748_);
            gl2751_ = PATT_(self__,28);
            cache_dispatch_(gl2751_,418,gl2752_,INTVAL_(gl2753_));
            ERR96_format_error_msg_(0,IATT_(self__,60),STR20_c_(STR20_i_(STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2076_)),CLA148_full_name_(PFN_(gl2753_)(gl2751_))),'('),gl263_),')'));
            PATT_(self__,12) = (ptr)GLO68_ob_typeob_s_;
            ;
      }
   }
   else {
      gl2754_ = PATT_(self__,40);
      switch (TYPE_(gl2754_)) {
         case (145) :
         case (154) :
            ERR96_compiler_error_msg_(0,(ptr)(&ls1673_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2077_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls785_)));
            break;
         case (153) :
            ERR96_format_error_exit_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2078_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls785_)));
            break;
         case (151) :
         case (152) :
            if ((num_args__ > 0)) {
               ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2079_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2080_)));
               PATT_(self__,36) = (ptr)0;
            }
            else {
            }
            gl2757_ = PATT_(self__,40);
            cache_dispatch_(gl2757_,661,gl2758_,INTVAL_(gl2759_));
            PATT_(self__,12) = (ptr)PFN_(gl2759_)(gl2757_);
            break;
         case (156) :
            i_264_ = (int)0;
            gl2763_ = PATT_(self__,40);
            cache_dispatch_(gl2763_,661,gl2764_,INTVAL_(gl2765_));
            gl2760_ = PFN_(gl2765_)(gl2763_);
            cache_dispatch_(gl2760_,1709,gl2761_,INTVAL_(gl2762_));
            ptypes__ = (ptr)PFN_(gl2762_)(gl2760_);
            num_params__ = (int)0;
            if ((ptypes__ != 0)) {
               num_params__ = (int)IATT_(ptypes__,16);
            }
            else {
            }
            if ((num_params__ != num_args__)) {
               ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls1879_)));
               if ((num_params__ < num_args__)) {
                  num_args__ = (int)num_params__;
               }
               else {
               }
            }
            else {
            }
            while (1) {
               if ((i_264_ >= num_args__)) {
                  goto goto_tag_2794_;
               }
               else {
               }
               gl2766_ = PATT_(PATT_(self__,36), 28 + ((i_264_) << 2));
               cache_dispatch_(gl2766_,1577,gl2767_,INTVAL_(gl2768_));
               gl265_ = PATT_(gl2766_,INTVAL_(gl2768_));
               if ((! GLO94_conform_tst_(0,gl265_,PATT_(ptypes__, 28 + ((i_264_) << 2)),PATT_(PATT_(self__,36), 28 + ((i_264_) << 2))))) {
                  tn1__ = S_ptr_VOID_;
                  tn2__ = S_ptr_VOID_;
                  gl2769_ = PATT_(PATT_(self__,36), 28 + ((i_264_) << 2));
                  cache_dispatch_(gl2769_,1577,gl2770_,INTVAL_(gl2771_));
                  gl266_ = PATT_(gl2769_,INTVAL_(gl2771_));
                  if ((gl266_ != 0)) {
                     gl2775_ = PATT_(PATT_(self__,36), 28 + ((i_264_) << 2));
                     cache_dispatch_(gl2775_,1577,gl2776_,INTVAL_(gl2777_));
                     gl2772_ = PATT_(gl2775_,INTVAL_(gl2777_));
                     cache_dispatch_(gl2772_,426,gl2773_,INTVAL_(gl2774_));
                     tn1__ = (ptr)PFN_(gl2774_)(gl2772_);
                  }
                  else {
                     tn1__ = (ptr)(ptr)(&ls1882_);
                  }
                  if ((PATT_(ptypes__, 28 + ((i_264_) << 2)) != 0)) {
                     gl2778_ = PATT_(ptypes__, 28 + ((i_264_) << 2));
                     cache_dispatch_(gl2778_,426,gl2779_,INTVAL_(gl2780_));
                     tn2__ = (ptr)PFN_(gl2780_)(gl2778_);
                  }
                  else {
                     tn2__ = (ptr)(ptr)(&ls1882_);
                  }
                  ERR96_format_error_msg_(0,IATT_(self__,60),STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls2081_)),(i_264_ + 1)),(ptr)(&ls1883_)),tn1__),(ptr)(&ls1884_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls1885_)),tn2__),')'));
               }
               else {
               }
               i_264_ = (int)(i_264_ + 1);
            }
         goto_tag_2794_: ;
            gl2784_ = PATT_(self__,40);
            cache_dispatch_(gl2784_,661,gl2785_,INTVAL_(gl2786_));
            gl2781_ = PFN_(gl2786_)(gl2784_);
            cache_dispatch_(gl2781_,1705,gl2782_,INTVAL_(gl2783_));
            PATT_(self__,12) = (ptr)PFN_(gl2783_)(gl2781_);
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls1735_)));
            ;
      }
      co__ = (ptr)PATT_(symtab__,4);
      gl2787_ = PATT_(self__,28);
      cache_dispatch_(gl2787_,418,gl2788_,INTVAL_(gl2789_));
      cls_ind__ = (int)IATT_(PFN_(gl2789_)(gl2787_),20);
      if ((cls_ind__ == RES97_C_ici_)) {
         if (GLO68_pre_semant_) {
            GLO68_other_cnames_ = (ptr)INT164_insert_(GLO68_other_cnames_,IATT_(self__,32));
         }
         else {
         }
         PATT_(co__,112) = (ptr)INT164_insert_(PATT_(co__,112),IATT_(self__,32));
      }
      else {
         if ((cls_ind__ != IATT_(PATT_(symtab__,4),20))) {
            k__ = (int)GLO94_key_of_class_feat_(0,cls_ind__,IATT_(self__,32));
            PATT_(co__,116) = (ptr)INT164_insert_(PATT_(co__,116),k__);
         }
         else {
         }
      }
   }

   ret0__:
   return;
}

ptr TYP115_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int TYP115_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

TYP115_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr TYP115_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

TYP115_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{
   SATHER_STR_(20,4,ls1999_,",3,");
   SATHER_STR_(20,6,ls2001_,",3,3,");
   SATHER_STR_(20,8,ls2004_,",3,3,3,");
   SATHER_STR_(20,10,ls2006_,",3,3,3,3,");
   SATHER_STR_(20,23,ls1673_,"TYPESPEC_ARGS_EXPROB_S");
   SATHER_STR_(20,23,ls1565_,"Unknown continuation #");
   ptr gl2795_;
   static int gl2796_;
   static union dtype_ gl2797_;
   ptr gl2798_;
   static int gl2799_;
   static union dtype_ gl2800_;
   int gl267_;
   ptr gl2801_;
   static int gl2802_;
   static union dtype_ gl2803_;
   int gl268_;
   ptr gl2804_;
   static int gl2805_;
   static union dtype_ gl2806_;
   ptr gl2807_;
   static int gl2808_;
   static union dtype_ gl2809_;
   int gl270_;
   ptr gl2810_;
   static int gl2811_;
   static union dtype_ gl2812_;
   ptr gl2813_;
   static int gl2814_;
   static union dtype_ gl2815_;
   int gl272_;
   ptr gl2816_;
   static int gl2817_;
   static union dtype_ gl2818_;
   ptr gl2819_;
   static int gl2820_;
   static union dtype_ gl2821_;
   int gl274_;
   ptr gl2822_;
   static int gl2823_;
   static union dtype_ gl2824_;
   int    num_args__ = S_int_VOID_;
   int    num_args_269_ = S_int_VOID_;
   int    num_args_271_ = S_int_VOID_;
   int    num_args_273_ = S_int_VOID_;

   switch (cont__) {
      case (1) :
         gl2795_ = PATT_(self__,12);
         cache_dispatch_(gl2795_,418,gl2796_,INTVAL_(gl2797_));
         gl2798_ = PATT_(self__,12);
         cache_dispatch_(gl2798_,1547,gl2799_,INTVAL_(gl2800_));
         gl267_ = IFN_(gl2800_)(gl2798_);
         (void)SAT99_i_(SAT99_c_(SAT99_i_(outfile__,gl267_),','),IATT_(PFN_(gl2797_)(gl2795_),60));
         break;
      case (2) :
         num_args__ = S_int_VOID_;
         if ((PATT_(self__,36) != 0)) {
            num_args__ = (int)IATT_(PATT_(self__,36),12);
         }
         else {
         }
         gl2801_ = PATT_(self__,12);
         cache_dispatch_(gl2801_,1547,gl2802_,INTVAL_(gl2803_));
         gl268_ = IFN_(gl2803_)(gl2801_);
         (void)SAT99_i_(outfile__,gl268_);
         if ((num_args__ != 1)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls1999_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2804_ = PATT_(self__,12);
         cache_dispatch_(gl2804_,418,gl2805_,INTVAL_(gl2806_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2806_)(gl2804_),60));
         break;
      case (3) :
         num_args_269_ = S_int_VOID_;
         if ((PATT_(self__,36) != 0)) {
            num_args_269_ = (int)IATT_(PATT_(self__,36),12);
         }
         else {
         }
         gl2807_ = PATT_(self__,12);
         cache_dispatch_(gl2807_,1547,gl2808_,INTVAL_(gl2809_));
         gl270_ = IFN_(gl2809_)(gl2807_);
         (void)SAT99_i_(outfile__,gl270_);
         if ((num_args_269_ != 2)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2001_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2810_ = PATT_(self__,12);
         cache_dispatch_(gl2810_,418,gl2811_,INTVAL_(gl2812_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2812_)(gl2810_),60));
         break;
      case (4) :
         num_args_271_ = S_int_VOID_;
         if ((PATT_(self__,36) != 0)) {
            num_args_271_ = (int)IATT_(PATT_(self__,36),12);
         }
         else {
         }
         gl2813_ = PATT_(self__,12);
         cache_dispatch_(gl2813_,1547,gl2814_,INTVAL_(gl2815_));
         gl272_ = IFN_(gl2815_)(gl2813_);
         (void)SAT99_i_(outfile__,gl272_);
         if ((num_args_271_ != 3)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2004_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,2,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2816_ = PATT_(self__,12);
         cache_dispatch_(gl2816_,418,gl2817_,INTVAL_(gl2818_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2818_)(gl2816_),60));
         break;
      case (5) :
         num_args_273_ = S_int_VOID_;
         if ((PATT_(self__,36) != 0)) {
            num_args_273_ = (int)IATT_(PATT_(self__,36),12);
         }
         else {
         }
         gl2819_ = PATT_(self__,12);
         cache_dispatch_(gl2819_,1547,gl2820_,INTVAL_(gl2821_));
         gl274_ = IFN_(gl2821_)(gl2819_);
         (void)SAT99_i_(outfile__,gl274_);
         if ((num_args_273_ != 4)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2006_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,2,outfile__);
            (void)SAT99_c_(outfile__,',');
            TYP115_cprint_ith_arg_act_code_(self__,3,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2822_ = PATT_(self__,12);
         cache_dispatch_(gl2822_,418,gl2823_,INTVAL_(gl2824_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2824_)(gl2822_),60));
         break;
      default:
         ERR96_compiler_error_msg_(0,(ptr)(&ls1673_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1565_)),cont__));
         ;
   }

   ret0__:
   return;
}

TYP115_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

TYP115_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

TYP115_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

TYP115_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl2825_;
   static int gl2826_;
   static union dtype_ gl2827_;

   LST120_cprint_init_code_(PATT_(self__,36),outfile__);
   if ((PATT_(self__,40) != 0)) {
      TYP115_init_expr_explicit_p_ = (char)1;
      gl2825_ = PATT_(self__,40);
      cache_dispatch_(gl2825_,724,gl2826_,INTVAL_(gl2827_));
      VFN_(gl2827_)(gl2825_,outfile__);
      TYP115_init_expr_explicit_p_ = (char)0;
   }
   else {
   }

   ret0__:
   return;
}

char TYP115_valid_init_expr_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl2828_;
   static int gl2829_;
   static union dtype_ gl2830_;
   int gl275_;
   ptr gl2831_;
   static int gl2832_;
   static union dtype_ gl2833_;
   int gl276_;
   ptr gl2834_;
   static int gl2835_;
   static union dtype_ gl2836_;
   int gl277_;

   if ((PATT_(self__,40) != 0)) {
      gl2828_ = PATT_(self__,40);
      gl275_ = TYPE_(gl2828_);
      gl2831_ = PATT_(self__,40);
      gl276_ = TYPE_(gl2831_);
      gl2834_ = PATT_(self__,40);
      gl277_ = TYPE_(gl2834_);
      if ((((gl275_ == 151) | (gl276_ == 152)) | (gl277_ == 156))) {
         if ((PATT_(self__,36) != 0)) {
            res__ = (char)LST120_valid_init_expr_(PATT_(self__,36));
         }
         else {
            res__ = (char)1;
         }
      }
      else {
         res__ = (char)0;
      }
   }
   else {
      switch (IATT_(self__,32)) {
         case (31) :
         case (32) :
            res__ = (char)LST120_valid_init_expr_(PATT_(self__,36));
            break;
         default:
            ;
      }
   }

   ret0__:
   return (res__);
}

char TYP115_assignable_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,38,ls2082_,"(TYPESPEC_ARGS_EXPROB_S): Assigning \"");
   SATHER_STR_(20,13,ls2083_,"\" is invalid");
   SATHER_STR_(20,28,ls2084_,"\" (Unknown class reference)");
   ptr gl2837_;
   static int gl2838_;
   static union dtype_ gl2839_;
   int gl278_;
   ptr gl2840_;
   static int gl2841_;
   static union dtype_ gl2842_;
   int gl279_;
   ptr gl2843_;
   static int gl2844_;
   static union dtype_ gl2845_;
   int gl280_;
   ptr gl2846_;
   static int gl2847_;
   static union dtype_ gl2848_;
   int gl281_;

   if ((PATT_(self__,40) == 0)) {
      switch (IATT_(self__,32)) {
         case (23) :
         case (24) :
         case (25) :
         case (26) :
         case (27) :
         case (28) :
         case (30) :
         case (31) :
         case (32) :
            res__ = (char)0;
            ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2082_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2083_)));
            break;
         default:
            ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2082_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2084_)));
            res__ = (char)0;
            ;
      }
      goto ret0__;
   }
   else {
   }
   gl2837_ = PATT_(self__,28);
   cache_dispatch_(gl2837_,1547,gl2838_,INTVAL_(gl2839_));
   gl278_ = IFN_(gl2839_)(gl2837_);
   if ((gl278_ == RES97_C_ici_)) {
      gl2840_ = PATT_(self__,40);
      gl279_ = TYPE_(gl2840_);
      gl2843_ = PATT_(self__,40);
      gl280_ = TYPE_(gl2843_);
      res__ = (char)((gl279_ == 152) | (gl280_ == 153));
   }
   else {
      gl2846_ = PATT_(self__,40);
      gl281_ = TYPE_(gl2846_);
      res__ = (char)(gl281_ == 152);
   }

   ret0__:
   return (res__);
}

ptr TYP115_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl2849_;
   static int gl2850_;
   static union dtype_ gl2851_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   res__ = (ptr)LST120_gen_temps_(PATT_(self__,36));
   if ((PATT_(self__,36) != 0)) {
      res__ = (ptr)LST120_gen_temps_(PATT_(self__,36));
   }
   else {
   }
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
         IATT_(self__,44) = (int)GLO94_global_key_(0);
         gl2849_ = PATT_(self__,12);
         cache_dispatch_(gl2849_,374,gl2850_,INTVAL_(gl2851_));
         IATT_(self__,48) = (int)IFN_(gl2851_)(gl2849_);
         if ((res__ != 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,44)),IATT_(self__,48));
         }
         else {
            res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,44)),IATT_(self__,48));
         }
      }
      else {
      }
   }
   else {
   }
   if (CATT_(self__,4)) {
      i__ = (int)0;
      sz__ = (int)IATT_(PATT_(self__,36),12);
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_2852_;
         }
         else {
         }
         if ((IATT_(PATT_(self__,52), 8 + ((i__) << 2)) > 0)) {
            if ((res__ != 0)) {
               res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(PATT_(self__,52), 8 + ((i__) << 2))),IATT_(PATT_(self__,56), 8 + ((i__) << 2)));
            }
            else {
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(PATT_(self__,52), 8 + ((i__) << 2))),IATT_(PATT_(self__,56), 8 + ((i__) << 2)));
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_2852_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr TYP115_expr_eval_constant_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,28,ls2062_,"(TYPESPEC_ARGS_EXPROB_S): \"");
   SATHER_STR_(20,44,ls2063_,"\" is invalid in constant feature definition");
   ptr gl2853_;
   static int gl2854_;
   static union dtype_ gl2855_;
   ptr gl2856_;
   static int gl2857_;
   static union dtype_ gl2858_;
   int gl282_;
   ptr gl2859_;
   static int gl2860_;
   static union dtype_ gl2861_;

   if ((PATT_(self__,40) == 0)) {
      switch (IATT_(self__,32)) {
         case (32) :
            gl2853_ = PATT_(self__,28);
            cache_dispatch_(gl2853_,418,gl2854_,INTVAL_(gl2855_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_(IATT_(PFN_(gl2855_)(gl2853_),20)),IATT_(self__,60));
            break;
         case (31) :
            res__ = (ptr)0;
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2062_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2063_)));
            ;
      }
      goto ret0__;
   }
   else {
   }
   gl2856_ = PATT_(self__,40);
   gl282_ = TYPE_(gl2856_);
   if ((gl282_ == 151)) {
      gl2859_ = PATT_(self__,40);
      cache_dispatch_(gl2859_,804,gl2860_,INTVAL_(gl2861_));
      res__ = (ptr)PFN_(gl2861_)(gl2859_);
   }
   else {
   }

   ret0__:
   return (res__);
}

TYP115_get_ext_strs_(self__)
ptr self__;
{
   ptr gl2862_;
   static int gl2863_;
   static union dtype_ gl2864_;
   ptr gl2865_;
   static int gl2866_;
   static union dtype_ gl2867_;
   int gl283_;
   ptr    co__ = 0;
   int    cls_ind__ = S_int_VOID_;
   ptr    const_feat__ = 0;
   int    temp__ = S_int_VOID_;

   LST120_get_ext_strs_(PATT_(self__,36));
   gl2862_ = PATT_(self__,28);
   cache_dispatch_(gl2862_,418,gl2863_,INTVAL_(gl2864_));
   co__ = (ptr)PFN_(gl2864_)(gl2862_);
   cls_ind__ = (int)IATT_(co__,20);
   if ((cls_ind__ == RES97_C_ici_)) {
   }
   else {
      if ((cls_ind__ != IATT_(GLO68_curr_class_inst_,20))) {
         if ((PATT_(self__,40) != 0)) {
            gl2865_ = PATT_(self__,40);
            gl283_ = TYPE_(gl2865_);
            if ((gl283_ == 151)) {
               const_feat__ = (ptr)PATT_(self__,40);
               temp__ = (int)CON151_is_a_str_const_(const_feat__);
               if ((temp__ > 0)) {
                  PATT_(GLO68_curr_class_inst_,120) = (ptr)INT164_insert_(PATT_(GLO68_curr_class_inst_,120),temp__);
               }
               else {
               }
            }
            else {
            }
         }
         else {
         }
      }
      else {
      }
   }

   ret0__:
   return;
}

char TYP115_to_be_pre_evaluated_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char TYP115_access_value_only_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl2868_;
   static int gl2869_;
   static union dtype_ gl2870_;
   int gl284_;

   if ((PATT_(self__,40) != 0)) {
      gl2868_ = PATT_(self__,40);
      gl284_ = TYPE_(gl2868_);
      res__ = (char)(gl284_ != 156);
   }
   else {
   }

   ret0__:
   return (res__);
}

TYP115_fob_error_(self__,op_name__,cls_name__)
ptr self__;
ptr op_name__;
ptr cls_name__;
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

TYP115_cprint_pre_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,36),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_2871_;
      }
      else {
      }
      TYP115_cprint_ith_arg_pre_code_(self__,i__,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_2871_: ;
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
         GLO94_cprint_curr_exp_code_(0,self__,IATT_(self__,44),outfile__);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

TYP115_cprint_act_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,23,ls1673_,"TYPESPEC_ARGS_EXPROB_S");
   SATHER_STR_(20,26,ls1727_,"Invalid printing count = ");
   SATHER_STR_(20,4,ls1728_," > ");
   SATHER_STR_(20,27,ls2085_," in TYPESPEC_ARGS_EXPROB_S");
   SATHER_STR_(20,123,ls2086_,"(TYPESPEC_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on array object; default size provided");
   SATHER_STR_(20,48,ls2087_,"/* new is not applicable to non-pointer type */");
   SATHER_STR_(20,23,ls2088_,"/* Invalid reference \"");
   SATHER_STR_(20,5,ls2028_,"\" */");
   SATHER_STR_(20,31,ls2089_,"/* Unknown feature reference \"");
   SATHER_STR_(20,4,ls2091_,"0.0");
   SATHER_STR_(20,2,ls1692_,"0");
   ptr gl2872_;
   static int gl2873_;
   static union dtype_ gl2874_;
   int gl285_;
   ptr gl2875_;
   static int gl2876_;
   static union dtype_ gl2877_;
   ptr gl2878_;
   static int gl2879_;
   static union dtype_ gl2880_;
   ptr gl2881_;
   static int gl2882_;
   static union dtype_ gl2883_;
   ptr gl2884_;
   static int gl2885_;
   static union dtype_ gl2886_;
   ptr gl2887_;
   static int gl2888_;
   static union dtype_ gl2889_;
   ptr gl2890_;
   static int gl2891_;
   static union dtype_ gl2892_;
   int gl286_;
   ptr gl2893_;
   static int gl2894_;
   static union dtype_ gl2895_;
   ptr gl2896_;
   static int gl2897_;
   static union dtype_ gl2898_;
   ptr gl2899_;
   static int gl2900_;
   static union dtype_ gl2901_;
   ptr gl2902_;
   static int gl2903_;
   static union dtype_ gl2904_;
   ptr gl2905_;
   static int gl2906_;
   static union dtype_ gl2907_;
   ptr gl2908_;
   static int gl2909_;
   static union dtype_ gl2910_;
   char gl287_;
   char gl288_;
   int    num_args__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   char    is_float_or_double__ = S_char_VOID_;
   int    i_289_ = S_int_VOID_;

   if ((IATT_(self__,64) > GLO68_g_tag_)) {
      if ((IATT_(self__,44) > 0)) {
         GLO94_cprint_ctemp_name_(0,IATT_(self__,44),outfile__);
         goto ret0__;
      }
      else {
         ERR96_compiler_error_msg_(0,(ptr)(&ls1673_),STR20_s_(STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1727_)),IATT_(self__,64)),(ptr)(&ls1728_)),GLO68_g_tag_),(ptr)(&ls2085_)));
      }
   }
   else {
   }
   IATT_(self__,64) = (int)(IATT_(self__,64) + 1);
   num_args__ = (int)0;
   if ((PATT_(self__,36) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,36),12);
   }
   else {
   }
   if ((PATT_(self__,40) == 0)) {
      switch (IATT_(self__,32)) {
         case (32) :
            gl2872_ = PATT_(self__,28);
            cache_dispatch_(gl2872_,1547,gl2873_,INTVAL_(gl2874_));
            gl285_ = IFN_(gl2874_)(gl2872_);
            (void)SAT99_i_(outfile__,gl285_);
            break;
         case (31) :
            gl2875_ = PATT_(self__,12);
            cache_dispatch_(gl2875_,1893,gl2876_,INTVAL_(gl2877_));
            if (CFN_(gl2877_)(gl2875_)) {
               if ((num_args__ != 1)) {
                  ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2086_)));
               }
               else {
               }
               PRI187_cprint_new_(0,outfile__,1,2,self__);
            }
            else {
               gl2878_ = PATT_(self__,12);
               cache_dispatch_(gl2878_,1894,gl2879_,INTVAL_(gl2880_));
               if (CFN_(gl2880_)(gl2878_)) {
                  if ((num_args__ != 2)) {
                     ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2086_)));
                  }
                  else {
                  }
                  PRI187_cprint_new_(0,outfile__,2,3,self__);
               }
               else {
                  gl2881_ = PATT_(self__,12);
                  cache_dispatch_(gl2881_,1895,gl2882_,INTVAL_(gl2883_));
                  if (CFN_(gl2883_)(gl2881_)) {
                     if ((num_args__ != 3)) {
                        ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2086_)));
                     }
                     else {
                     }
                     PRI187_cprint_new_(0,outfile__,3,4,self__);
                  }
                  else {
                     gl2884_ = PATT_(self__,12);
                     cache_dispatch_(gl2884_,1896,gl2885_,INTVAL_(gl2886_));
                     if (CFN_(gl2886_)(gl2884_)) {
                        if ((num_args__ != 4)) {
                           ERR96_format_error_msg_(0,IATT_(self__,60),STR20_s_(STR20_create_(0),(ptr)(&ls2086_)));
                        }
                        else {
                        }
                        PRI187_cprint_new_(0,outfile__,4,5,self__);
                     }
                     else {
                        gl2887_ = PATT_(self__,12);
                        cache_dispatch_(gl2887_,1549,gl2888_,INTVAL_(gl2889_));
                        if (CFN_(gl2889_)(gl2887_)) {
                           (void)SAT99_s_(outfile__,(ptr)(&ls2087_));
                        }
                        else {
                           PRI187_cprint_new_(0,outfile__,0,1,self__);
                        }
                     }
                  }
               }
            }
            break;
         default:
            (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2088_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2028_));
            ;
      }
   }
   else {
      TYP115_cprint_cast_code_(self__,outfile__);
      gl2890_ = PATT_(self__,28);
      cache_dispatch_(gl2890_,1547,gl2891_,INTVAL_(gl2892_));
      gl286_ = IFN_(gl2892_)(gl2890_);
      if ((gl286_ == RES97_C_ici_)) {
         gl2893_ = PATT_(self__,40);
         switch (TYPE_(gl2893_)) {
            case (151) :
            case (152) :
               (void)SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,32)));
               break;
            case (156) :
               (void)SAT99_c_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),'(');
               i__ = S_int_VOID_;
               while (1) {
                  if ((i__ >= num_args__)) {
                     goto goto_tag_2911_;
                  }
                  else {
                  }
                  TYP115_cprint_ith_arg_act_code_(self__,i__,outfile__);
                  i__ = (int)(i__ + 1);
                  if ((i__ < num_args__)) {
                     (void)SAT99_c_(outfile__,',');
                  }
                  else {
                  }
               }
            goto_tag_2911_: ;
               (void)SAT99_c_(outfile__,')');
               break;
            default:
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2089_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2028_));
               ;
         }
      }
      else {
         gl2896_ = PATT_(self__,40);
         switch (TYPE_(gl2896_)) {
            case (151) :
            case (152) :
               gl2899_ = PATT_(self__,40);
               cache_dispatch_(gl2899_,809,gl2900_,INTVAL_(gl2901_));
               VFN_(gl2901_)(gl2899_,outfile__);
               break;
            case (156) :
               gl2902_ = PATT_(self__,40);
               cache_dispatch_(gl2902_,808,gl2903_,INTVAL_(gl2904_));
               VFN_(gl2904_)(gl2902_,outfile__);
               (void)SAT99_c_(outfile__,'(');
               gl2905_ = PATT_(self__,28);
               cache_dispatch_(gl2905_,1886,gl2906_,INTVAL_(gl2907_));
               gl287_ = CFN_(gl2907_)(gl2905_);
               gl2908_ = PATT_(self__,28);
               cache_dispatch_(gl2908_,1887,gl2909_,INTVAL_(gl2910_));
               gl288_ = CFN_(gl2910_)(gl2908_);
               is_float_or_double__ = (char)(gl287_ | gl288_);
               if (is_float_or_double__) {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2091_));
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls1692_));
               }
               if ((num_args__ >= 1)) {
                  i_289_ = S_int_VOID_;
                  while (1) {
                     if ((i_289_ >= num_args__)) {
                        goto goto_tag_2912_;
                     }
                     else {
                     }
                     (void)SAT99_c_(outfile__,',');
                     TYP115_cprint_ith_arg_act_code_(self__,i_289_,outfile__);
                     i_289_ = (int)(i_289_ + 1);
                  }
               goto_tag_2912_: ;
               }
               else {
               }
               (void)SAT99_c_(outfile__,')');
               break;
            default:
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2089_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,32))),(ptr)(&ls2028_));
               ;
         }
      }
   }

   ret0__:
   return;
}

TYP115_cprint_cast_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,2,ls1493_,"(");
   SATHER_STR_(20,9,ls350_,"EXPROB_S");
   SATHER_STR_(20,20,ls1685_,"Invalid C type cast");
   SATHER_STR_(20,2,ls1278_,")");

   if ((IATT_(self__,20) != 0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls1493_));
      switch (IATT_(self__,20)) {
         case (1) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1215_));
            break;
         case (2) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1216_));
            break;
         case (3) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1217_));
            break;
         case (4) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1218_));
            break;
         case (5) :
            (void)SAT99_s_(outfile__,(ptr)(&gs1219_));
            break;
         default:
            ERR96_compiler_error_msg_(0,(ptr)(&ls350_),(ptr)(&ls1685_));
            ;
      }
      (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
   }
   else {
   }

   ret0__:
   return;
}

TYP115_cprint_ith_arg_pre_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl2913_;
   static int gl2914_;
   static union dtype_ gl2915_;
   ptr gl2916_;
   static int gl2917_;
   static union dtype_ gl2918_;
   ptr    ith_arg__ = 0;
   int    ith_temp__ = S_int_VOID_;

   ith_arg__ = (ptr)PATT_(PATT_(self__,36), 28 + ((i__) << 2));
   ith_temp__ = (int)IATT_(PATT_(self__,52), 8 + ((i__) << 2));
   gl2913_ = ith_arg__;
   cache_dispatch_(gl2913_,1589,gl2914_,INTVAL_(gl2915_));
   VFN_(gl2915_)(gl2913_,outfile__);
   if ((ith_temp__ > 0)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl2916_ = ith_arg__;
      cache_dispatch_(gl2916_,965,gl2917_,INTVAL_(gl2918_));
      VFN_(gl2918_)(gl2916_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }

   ret0__:
   return;
}

TYP115_cprint_ith_arg_act_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   ptr gl2919_;
   static int gl2920_;
   static union dtype_ gl2921_;
   int    ith_temp__ = S_int_VOID_;

   ith_temp__ = (int)IATT_(PATT_(self__,52), 8 + ((i__) << 2));
   if ((ith_temp__ > 0)) {
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
   }
   else {
      gl2919_ = PATT_(PATT_(self__,36), 28 + ((i__) << 2));
      cache_dispatch_(gl2919_,965,gl2920_,INTVAL_(gl2921_));
      VFN_(gl2921_)(gl2919_,outfile__);
   }

   ret0__:
   return;
}

