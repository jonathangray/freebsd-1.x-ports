/* id_arg112.c : Sather class: ID_ARGS_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*constant*/ int RES71_asize_ind_;
extern /*constant*/ int RES71_asize1_ind_;
extern /*constant*/ int RES71_asize2_ind_;
extern /*constant*/ int RES71_asize3_ind_;
extern /*constant*/ int RES71_asize4_ind_;
extern /*constant*/ int RES71_type_ind_;
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern /*constant*/ int RES71_copy_ind_;
extern /*constant*/ int RES71_res_ind_;
extern /*constant*/ int RES71_self_ind_;
extern /*constant*/ int RES71_void_ind_;
extern /*constant*/ int RES71_new_ind_;
extern /*constant*/ int RES71_extend_ind_;
extern /*shared*/ int GLO68_g_tag_;
extern GLO94_check_f_ob_();
extern int GLO94_global_key_();
extern int ERR96_out_of_line_err_info_();
extern /*constant*/ int RES97_FOB_ici_;
extern ERR96_format_error_msg_();
extern char GLO94_conform_tst_();
extern ERR96_format_error_exit_();
extern ptr SAT99_i_();
extern ptr SAT99_c_();
extern ERR96_compiler_error_msg_();
extern char GLO94_check_is_on_();
extern ptr LIS98_push_();
extern ptr LIS98_create_();
extern GLO94_cprint_curr_exp_code_();
extern GLO94_cprint_ctemp_name_();
extern ptr SAT99_s_();
extern char GLO94_cprint_ref_to_self_();
extern ERR96_format_warning_msg_();
extern ptr SAT99_indent_();
extern ptr SAT99_inc_ln_();
extern char EXP117_to_be_pre_evaluated_();
extern EXP117_cprint_pre_code_();
extern EXP117_cprint_act_code_();
extern LST120_get_ext_strs_();
extern LST120_out_of_line_();
extern ptr LST120_dup_();
extern ptr LST120_sather_code_();
extern LST120_resolve_predef_types_();
extern LST120_semant_();
extern LST120_cprint_init_code_();
extern char LST120_valid_init_expr_();
extern ptr LST120_gen_temps_();
extern int TYP149_ctype_();
extern ptr INS150_create_();
extern char TYP149_int_type_p_();
extern ptr TYP149_paramstype_();
extern ptr TYP149_full_name_();
extern char TYP149_real_type_p_();
extern char TYP149_double_type_p_();
extern ptr ROU156_typeof_();
extern ptr TYP149_rettype_();
extern ptr TYP149_inst_cls_();
extern char TYP149_array_type_p_();
extern char TYP149_array2_type_p_();
extern char TYP149_array3_type_p_();
extern char TYP149_array4_type_p_();
extern /*constant*/ int C_T168_c_double_;
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ ptr C_T168_c_ptr_name_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ ptr C_T168_c_char_name_;
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ ptr C_T168_c_int_name_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ ptr C_T168_c_float_name_;
extern /*constant*/ ptr C_T168_c_double_name_;
extern ptr SYM186_get_sym_();
extern PRI187_cprint_new_();
extern SEM188_cprint_init_code_();
extern SEM188_cprint_cname_();
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int ID_112_print_indent_ = 2;
ptr ID_112_create_();
ID_112_out_of_line_();
ptr ID_112_dup_();
ID_112_put_kwdname_();
ptr ID_112_sather_code_();
ptr ID_112_initialize_();
ID_112_resolve_predef_types_();
ID_112_semant_();
ptr ID_112_typeof_();
int ID_112_get_offset_();
ID_112_cprint_offset_();
ptr ID_112_get_constval_();
ID_112_cont_cprint_code_();
ID_112_cprint_cname_();
ID_112_cprint_extern_();
ID_112_cprint_access_value_();
ID_112_cprint_init_code_();
char ID_112_valid_init_expr_();
char ID_112_assignable_p_();
ptr ID_112_gen_temps_();
ptr ID_112_expr_eval_constant_();
ID_112_get_ext_strs_();
char ID_112_to_be_pre_evaluated_();
char ID_112_access_value_only_();
ID_112_fob_error_();
ID_112_cprint_pre_code_();
ID_112_cprint_act_code_();
ID_112_cprint_cast_code_();
ID_112_cprint_ith_arg_pre_code_();
ID_112_cprint_ith_arg_act_code_();
/*constant*/ int ID_112_cont1_ = 1;
extern int attr_ent_ID_112[];

ptr ID_112_create_(self__,id__,alst__,ln__)
ptr self__;
int id__;
ptr alst__;
int ln__;
{
   ptr res__ = 0;
   int    sz__ = S_int_VOID_;

   res__ = (ptr)new_(112,0);
   IATT_(res__,28) = (int)id__;
   PATT_(res__,32) = (ptr)alst__;
   IATT_(res__,56) = (int)ln__;
   sz__ = (int)IATT_(alst__,12);
   PATT_(res__,48) = (ptr)new1_(163,sz__,1);
   PATT_(res__,52) = (ptr)new1_(163,sz__,1);

   ret0__:
   return (res__);
}

ID_112_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,56) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,56));
   LST120_out_of_line_(PATT_(self__,32),fn__);

   ret0__:
   return;
}

ptr ID_112_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)ID_112_create_(self__,IATT_(self__,28),LST120_dup_(PATT_(self__,32)),IATT_(self__,56));

   ret0__:
   return (res__);
}

ID_112_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl1830_;
   static int gl1831_;
   static union dtype_ gl1832_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1830_ = x__;
   cache_dispatch_(gl1830_,796,gl1831_,INTVAL_(gl1832_));
   IATT_(gl1830_,INTVAL_(gl1832_)) = (int)nm__;

   ret0__:
   return;
}

ptr ID_112_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)STR20_c_(STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),'('),LST120_sather_code_(PATT_(self__,32))),')');

   ret0__:
   return (res__);
}

ptr ID_112_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ID_112_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{

   LST120_resolve_predef_types_(PATT_(self__,32),index__);

   ret0__:
   return;
}

ID_112_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,31,ls1867_,"(ID_ARGS_EXPROB_S): Argument #");
   SATHER_STR_(20,39,ls1868_," has no return type in call expression");
   SATHER_STR_(20,22,ls1869_,"(ID_ARGS_EXPROB_S): \"");
   SATHER_STR_(20,26,ls1870_,"\" does not need arguments");
   SATHER_STR_(20,39,ls1871_,"(ID_ARGS_EXPROB_S): Incorrect use of \"");
   SATHER_STR_(20,19,ls1872_,"\" -- Args ignored\n");
   SATHER_STR_(20,11,ls1874_,"\" expects ");
   SATHER_STR_(20,25,ls1875_," arguments but is given ");
   SATHER_STR_(20,30,ls1876_,"\" gets non-integer argument #");
   SATHER_STR_(20,27,ls1877_,"\" is an unknown identifier");
   SATHER_STR_(20,32,ls1879_,"\" gets incorrect number of args");
   SATHER_STR_(20,10,ls1882_,"<UNKNOWN>");
   SATHER_STR_(20,22,ls1883_," has incorrect type (");
   SATHER_STR_(20,8,ls1884_,") for \"");
   SATHER_STR_(20,4,ls1885_,"\" (");
   SATHER_STR_(20,52,ls1888_,"(ID_ARGS_EXPROB_S): Expected routine call not found");
   ptr gl1833_;
   static int gl1834_;
   static union dtype_ gl1835_;
   ptr gl149_;
   ptr gl1836_;
   static int gl1837_;
   static union dtype_ gl1838_;
   ptr gl1839_;
   static int gl1840_;
   static union dtype_ gl1841_;
   ptr gl1842_;
   static int gl1843_;
   static union dtype_ gl1844_;
   ptr gl1845_;
   static int gl1846_;
   static union dtype_ gl1847_;
   ptr gl1848_;
   static int gl1849_;
   static union dtype_ gl1850_;
   ptr gl1851_;
   static int gl1852_;
   static union dtype_ gl1853_;
   char gl152_;
   ptr gl1854_;
   static int gl1855_;
   static union dtype_ gl1856_;
   int gl153_;
   ptr gl1857_;
   static int gl1858_;
   static union dtype_ gl1859_;
   ptr gl1860_;
   static int gl1861_;
   static union dtype_ gl1862_;
   ptr gl156_;
   ptr gl1863_;
   static int gl1864_;
   static union dtype_ gl1865_;
   ptr gl157_;
   ptr gl1866_;
   static int gl1867_;
   static union dtype_ gl1868_;
   ptr gl1869_;
   static int gl1870_;
   static union dtype_ gl1871_;
   ptr gl1872_;
   static int gl1873_;
   static union dtype_ gl1874_;
   ptr gl1875_;
   static int gl1876_;
   static union dtype_ gl1877_;
   ptr gl1878_;
   static int gl1879_;
   static union dtype_ gl1880_;
   ptr gl1881_;
   static int gl1882_;
   static union dtype_ gl1883_;
   ptr gl1884_;
   static int gl1885_;
   static union dtype_ gl1886_;
   char gl158_;
   char gl159_;
   char gl160_;
   ptr gl1887_;
   static int gl1888_;
   static union dtype_ gl1889_;
   ptr gl1890_;
   static int gl1891_;
   static union dtype_ gl1892_;
   int    num_args__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    i_150_ = S_int_VOID_;
   int    asz__ = S_int_VOID_;
   int    i_151_ = S_int_VOID_;
   ptr    rout_feat__ = 0;
   ptr    ptypes__ = 0;
   int    num_params__ = S_int_VOID_;
   int    i_154_ = S_int_VOID_;
   int    num_args_155_ = S_int_VOID_;
   ptr    tn1__ = 0;
   ptr    tn2__ = 0;

   if ((PATT_(self__,36) == 0)) {
      PATT_(self__,36) = (ptr)SYM186_get_sym_(symtab__,IATT_(self__,28));
   }
   else {
   }
   if ((IATT_(PATT_(symtab__,4),96) == RES97_FOB_ici_)) {
      GLO94_check_f_ob_(0,self__,IATT_(self__,28),symtab__);
   }
   else {
   }
   LST120_semant_(PATT_(self__,32),symtab__);
   num_args__ = (int)IATT_(PATT_(self__,32),12);
   i__ = S_int_VOID_;
   while (1) {
      if ((i__ >= num_args__)) {
         goto goto_tag_1893_;
      }
      else {
      }
      gl1833_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
      cache_dispatch_(gl1833_,1577,gl1834_,INTVAL_(gl1835_));
      gl149_ = PATT_(gl1833_,INTVAL_(gl1835_));
      if ((gl149_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1867_)),(i__ + 1)),(ptr)(&ls1868_)));
         gl1836_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
         cache_dispatch_(gl1836_,1577,gl1837_,INTVAL_(gl1838_));
         PATT_(gl1836_,INTVAL_(gl1838_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1893_: ;
   i_150_ = (int)0;
   while (1) {
      if ((i_150_ >= num_args__)) {
         goto goto_tag_1894_;
      }
      else {
      }
      gl1839_ = PATT_(PATT_(self__,32), 28 + ((i_150_) << 2));
      cache_dispatch_(gl1839_,1678,gl1840_,INTVAL_(gl1841_));
      if (CFN_(gl1841_)(gl1839_)) {
         IATT_(PATT_(self__,48), 8 + ((i_150_) << 2)) = (int)GLO94_global_key_(0);
         gl1845_ = PATT_(PATT_(self__,32), 28 + ((i_150_) << 2));
         cache_dispatch_(gl1845_,1577,gl1846_,INTVAL_(gl1847_));
         gl1842_ = PATT_(gl1845_,INTVAL_(gl1847_));
         cache_dispatch_(gl1842_,374,gl1843_,INTVAL_(gl1844_));
         IATT_(PATT_(self__,52), 8 + ((i_150_) << 2)) = (int)IFN_(gl1844_)(gl1842_);
         CATT_(self__,4) = (char)1;
      }
      else {
      }
      i_150_ = (int)(i_150_ + 1);
   }
goto_tag_1894_: ;
   if ((PATT_(self__,36) == 0)) {
      switch (IATT_(self__,28)) {
         case (23) :
         case (24) :
         case (25) :
         case (26) :
         case (27) :
         case (32) :
            ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1869_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1870_)));
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            break;
         case (28) :
            ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1869_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1870_)));
            PATT_(self__,12) = (ptr)INS150_create_(0,IATT_(PATT_(symtab__,4),20),IATT_(self__,56));
            break;
         case (33) :
         case (34) :
         case (37) :
            ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1871_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1872_)));
            PATT_(self__,12) = (ptr)INS150_create_(0,IATT_(PATT_(symtab__,4),20),IATT_(self__,56));
            break;
         case (31) :
         case (30) :
            asz__ = (int)IATT_(PATT_(self__,32),12);
            if ((asz__ != IATT_(PATT_(symtab__,4),100))) {
               ERR96_format_error_msg_(0,IATT_(self__,56),STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1869_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1874_)),IATT_(PATT_(symtab__,4),100)),(ptr)(&ls1875_)),asz__));
            }
            else {
            }
            i_151_ = (int)0;
            while (1) {
               if ((i_151_ >= asz__)) {
                  goto goto_tag_1895_;
               }
               else {
               }
               gl1851_ = PATT_(PATT_(self__,32), 28 + ((i_151_) << 2));
               cache_dispatch_(gl1851_,1577,gl1852_,INTVAL_(gl1853_));
               gl1848_ = PATT_(gl1851_,INTVAL_(gl1853_));
               cache_dispatch_(gl1848_,1778,gl1849_,INTVAL_(gl1850_));
               gl152_ = CFN_(gl1850_)(gl1848_);
               if ((! gl152_)) {
                  ERR96_format_error_msg_(0,IATT_(self__,56),STR20_i_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1869_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1876_)),(i_151_ + 1)));
               }
               else {
               }
               i_151_ = (int)(i_151_ + 1);
            }
         goto_tag_1895_: ;
            PATT_(self__,12) = (ptr)INS150_create_(0,IATT_(PATT_(symtab__,4),20),IATT_(self__,56));
            break;
         default:
            ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1869_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1877_)));
            PATT_(self__,12) = (ptr)GLO68_ob_typeob_s_;
            ;
      }
   }
   else {
      gl1854_ = PATT_(self__,36);
      gl153_ = TYPE_(gl1854_);
      if ((gl153_ == 156)) {
         rout_feat__ = (ptr)PATT_(self__,36);
         CATT_(rout_feat__,11) = (char)1;
         gl1857_ = ROU156_typeof_(rout_feat__);
         cache_dispatch_(gl1857_,1709,gl1858_,INTVAL_(gl1859_));
         ptypes__ = (ptr)PFN_(gl1859_)(gl1857_);
         num_params__ = (int)0;
         if ((ptypes__ != 0)) {
            num_params__ = (int)IATT_(ptypes__,16);
         }
         else {
         }
         i_154_ = (int)0;
         num_args_155_ = (int)IATT_(PATT_(self__,32),12);
         if ((num_params__ != num_args_155_)) {
            ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1869_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1879_)));
         }
         else {
         }
         if ((num_params__ < num_args_155_)) {
            num_args_155_ = (int)num_params__;
         }
         else {
         }
         while (1) {
            if ((i_154_ >= num_args_155_)) {
               goto goto_tag_1896_;
            }
            else {
            }
            gl1860_ = PATT_(PATT_(self__,32), 28 + ((i_154_) << 2));
            cache_dispatch_(gl1860_,1577,gl1861_,INTVAL_(gl1862_));
            gl156_ = PATT_(gl1860_,INTVAL_(gl1862_));
            if ((! GLO94_conform_tst_(0,gl156_,PATT_(ptypes__, 28 + ((i_154_) << 2)),PATT_(PATT_(self__,32), 28 + ((i_154_) << 2))))) {
               tn1__ = S_ptr_VOID_;
               tn2__ = S_ptr_VOID_;
               gl1863_ = PATT_(PATT_(self__,32), 28 + ((i_154_) << 2));
               cache_dispatch_(gl1863_,1577,gl1864_,INTVAL_(gl1865_));
               gl157_ = PATT_(gl1863_,INTVAL_(gl1865_));
               if ((gl157_ != 0)) {
                  gl1869_ = PATT_(PATT_(self__,32), 28 + ((i_154_) << 2));
                  cache_dispatch_(gl1869_,1577,gl1870_,INTVAL_(gl1871_));
                  gl1866_ = PATT_(gl1869_,INTVAL_(gl1871_));
                  cache_dispatch_(gl1866_,426,gl1867_,INTVAL_(gl1868_));
                  tn1__ = (ptr)PFN_(gl1868_)(gl1866_);
               }
               else {
                  tn1__ = (ptr)(ptr)(&ls1882_);
               }
               if ((PATT_(ptypes__, 28 + ((i_154_) << 2)) != 0)) {
                  gl1872_ = PATT_(ptypes__, 28 + ((i_154_) << 2));
                  cache_dispatch_(gl1872_,426,gl1873_,INTVAL_(gl1874_));
                  tn2__ = (ptr)PFN_(gl1874_)(gl1872_);
               }
               else {
                  tn2__ = (ptr)(ptr)(&ls1882_);
               }
               ERR96_format_error_msg_(0,IATT_(self__,56),STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1867_)),(i_154_ + 1)),(ptr)(&ls1883_)),tn1__),(ptr)(&ls1884_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1885_)),tn2__),')'));
            }
            else {
               gl1878_ = PATT_(PATT_(self__,32), 28 + ((i_154_) << 2));
               cache_dispatch_(gl1878_,1577,gl1879_,INTVAL_(gl1880_));
               gl1875_ = PATT_(gl1878_,INTVAL_(gl1880_));
               cache_dispatch_(gl1875_,1778,gl1876_,INTVAL_(gl1877_));
               gl160_ = CFN_(gl1877_)(gl1875_);
               gl1881_ = PATT_(ptypes__, 28 + ((i_154_) << 2));
               cache_dispatch_(gl1881_,1886,gl1882_,INTVAL_(gl1883_));
               gl158_ = CFN_(gl1883_)(gl1881_);
               gl1884_ = PATT_(ptypes__, 28 + ((i_154_) << 2));
               cache_dispatch_(gl1884_,1887,gl1885_,INTVAL_(gl1886_));
               gl159_ = CFN_(gl1886_)(gl1884_);
               if ((gl160_ & (gl158_ | gl159_))) {
                  gl1887_ = PATT_(PATT_(self__,32), 28 + ((i_154_) << 2));
                  cache_dispatch_(gl1887_,1675,gl1888_,INTVAL_(gl1889_));
                  IATT_(gl1887_,INTVAL_(gl1889_)) = (int)5;
               }
               else {
               }
            }
            i_154_ = (int)(i_154_ + 1);
         }
      goto_tag_1896_: ;
         gl1890_ = ROU156_typeof_(rout_feat__);
         cache_dispatch_(gl1890_,1705,gl1891_,INTVAL_(gl1892_));
         PATT_(self__,12) = (ptr)PFN_(gl1892_)(gl1890_);
      }
      else {
         ERR96_format_error_exit_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1888_)));
      }
   }

   ret0__:
   return;
}

ptr ID_112_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int ID_112_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ID_112_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr ID_112_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ID_112_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{
   SATHER_STR_(20,17,ls1663_,"ID_ARGS_EXPROB_S");
   SATHER_STR_(20,22,ls1892_,"Unknown continuation#");
   ptr gl1897_;
   static int gl1898_;
   static union dtype_ gl1899_;
   ptr    co__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   switch (cont__) {
      case (1) :
         gl1897_ = PATT_(self__,12);
         cache_dispatch_(gl1897_,418,gl1898_,INTVAL_(gl1899_));
         co__ = (ptr)PFN_(gl1899_)(gl1897_);
         (void)SAT99_c_(SAT99_i_(outfile__,IATT_(co__,20)),',');
         i__ = S_int_VOID_;
         sz__ = (int)IATT_(PATT_(self__,32),12);
         while (1) {
            if ((i__ >= sz__)) {
               goto goto_tag_1900_;
            }
            else {
            }
            ID_112_cprint_ith_arg_act_code_(self__,i__,outfile__);
            (void)SAT99_c_(outfile__,',');
            i__ = (int)(i__ + 1);
         }
      goto_tag_1900_: ;
         (void)SAT99_i_(outfile__,IATT_(co__,60));
         break;
      default:
         ERR96_compiler_error_msg_(0,(ptr)(&ls1663_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1892_)),cont__));
         ;
   }

   ret0__:
   return;
}

ID_112_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ID_112_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ID_112_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ID_112_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl1901_;
   static int gl1902_;
   static union dtype_ gl1903_;
   int gl161_;
   ptr gl1904_;
   static int gl1905_;
   static union dtype_ gl1906_;
   int gl162_;
   ptr gl1907_;
   static int gl1908_;
   static union dtype_ gl1909_;

   LST120_cprint_init_code_(PATT_(self__,32),outfile__);
   gl1901_ = PATT_(self__,36);
   gl161_ = TYPE_(gl1901_);
   gl1904_ = PATT_(self__,36);
   gl162_ = TYPE_(gl1904_);
   if (((gl161_ == 152) | (gl162_ == 151))) {
      gl1907_ = PATT_(self__,36);
      cache_dispatch_(gl1907_,724,gl1908_,INTVAL_(gl1909_));
      VFN_(gl1909_)(gl1907_,outfile__);
   }
   else {
   }

   ret0__:
   return;
}

char ID_112_valid_init_expr_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl1910_;
   static int gl1911_;
   static union dtype_ gl1912_;
   int gl163_;

   if ((PATT_(self__,36) != 0)) {
      gl1910_ = PATT_(self__,36);
      gl163_ = TYPE_(gl1910_);
      if ((! (gl163_ == 156))) {
         res__ = (char)0;
      }
      else {
         res__ = (char)LST120_valid_init_expr_(PATT_(self__,32));
      }
   }
   else {
      switch (IATT_(self__,28)) {
         case (31) :
            res__ = (char)LST120_valid_init_expr_(PATT_(self__,32));
            break;
         default:
            ;
      }
   }

   ret0__:
   return (res__);
}

char ID_112_assignable_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr ID_112_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1913_;
   static int gl1914_;
   static union dtype_ gl1915_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   res__ = (ptr)LST120_gen_temps_(PATT_(self__,32));
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
         IATT_(self__,40) = (int)GLO94_global_key_(0);
         gl1913_ = PATT_(self__,12);
         cache_dispatch_(gl1913_,374,gl1914_,INTVAL_(gl1915_));
         IATT_(self__,44) = (int)IFN_(gl1915_)(gl1913_);
         if ((res__ != 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,40)),IATT_(self__,44));
         }
         else {
            res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,40)),IATT_(self__,44));
         }
      }
      else {
      }
   }
   else {
   }
   if (CATT_(self__,4)) {
      i__ = (int)0;
      sz__ = (int)IATT_(PATT_(self__,32),12);
      while (1) {
         if ((i__ >= sz__)) {
            goto goto_tag_1916_;
         }
         else {
         }
         if ((IATT_(PATT_(self__,48), 8 + ((i__) << 2)) > 0)) {
            if ((res__ != 0)) {
               res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(PATT_(self__,48), 8 + ((i__) << 2))),IATT_(PATT_(self__,52), 8 + ((i__) << 2)));
            }
            else {
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(PATT_(self__,48), 8 + ((i__) << 2))),IATT_(PATT_(self__,52), 8 + ((i__) << 2)));
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1916_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ID_112_expr_eval_constant_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ID_112_get_ext_strs_(self__)
ptr self__;
{

   LST120_get_ext_strs_(PATT_(self__,32));

   ret0__:
   return;
}

char ID_112_to_be_pre_evaluated_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ID_112_access_value_only_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl1917_;
   static int gl1918_;
   static union dtype_ gl1919_;
   int gl164_;

   if ((PATT_(self__,36) != 0)) {
      gl1917_ = PATT_(self__,36);
      gl164_ = TYPE_(gl1917_);
      res__ = (char)(gl164_ != 156);
   }
   else {
   }

   ret0__:
   return (res__);
}

ID_112_fob_error_(self__,op_name__,cls_name__)
ptr self__;
ptr op_name__;
ptr cls_name__;
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

ID_112_cprint_pre_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = S_int_VOID_;
   sz__ = (int)IATT_(PATT_(self__,32),12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_1920_;
      }
      else {
      }
      ID_112_cprint_ith_arg_pre_code_(self__,i__,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_1920_: ;
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
         GLO94_cprint_curr_exp_code_(0,self__,IATT_(self__,40),outfile__);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

ID_112_cprint_act_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,17,ls1663_,"ID_ARGS_EXPROB_S");
   SATHER_STR_(20,26,ls1727_,"Invalid printing count = ");
   SATHER_STR_(20,4,ls1728_," > ");
   SATHER_STR_(20,10,ls1897_,"extend1_(");
   SATHER_STR_(20,42,ls1898_,"(ID_ARGS_EXPROB_S): Extending void object");
   SATHER_STR_(20,10,ls1899_,"extend2_(");
   SATHER_STR_(20,2,ls659_,",");
   SATHER_STR_(20,10,ls1900_,"extend3_(");
   SATHER_STR_(20,10,ls1901_,"extend4_(");
   SATHER_STR_(20,6,ls1732_,"res__");
   SATHER_STR_(20,37,ls1902_,"(ID_ARGS_EXPROB_S): Unknown use of \"");
   SATHER_STR_(20,28,ls1903_,"\" (a reserved feature name)");
   SATHER_STR_(20,79,ls1904_,"(ID_ARGS_EXPROB_S): Unexpected arguments supplied for \"type\" feature on object");
   SATHER_STR_(20,50,ls1905_,"(ID_ARGS_EXPROB_S): Routine call with void object");
   ptr gl1921_;
   static int gl1922_;
   static union dtype_ gl1923_;
   ptr gl1924_;
   static int gl1925_;
   static union dtype_ gl1926_;
   ptr gl1927_;
   static int gl1928_;
   static union dtype_ gl1929_;
   ptr gl1930_;
   static int gl1931_;
   static union dtype_ gl1932_;
   ptr gl1933_;
   static int gl1934_;
   static union dtype_ gl1935_;
   ptr gl1936_;
   static int gl1937_;
   static union dtype_ gl1938_;
   ptr gl1939_;
   static int gl1940_;
   static union dtype_ gl1941_;
   ptr gl1942_;
   static int gl1943_;
   static union dtype_ gl1944_;
   ptr gl1945_;
   static int gl1946_;
   static union dtype_ gl1947_;
   ptr gl1948_;
   static int gl1949_;
   static union dtype_ gl1950_;
   ptr gl1951_;
   static int gl1952_;
   static union dtype_ gl1953_;
   ptr    co__ = 0;
   int    i__ = S_int_VOID_;
   int    asz__ = S_int_VOID_;

   if ((IATT_(self__,60) > GLO68_g_tag_)) {
      if ((IATT_(self__,40) > 0)) {
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         goto ret0__;
      }
      else {
         ERR96_compiler_error_msg_(0,(ptr)(&ls1663_),STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1727_)),IATT_(self__,60)),(ptr)(&ls1728_)),GLO68_g_tag_));
      }
   }
   else {
   }
   IATT_(self__,60) = (int)(IATT_(self__,60) + 1);
   if ((PATT_(self__,36) == 0)) {
      co__ = S_ptr_VOID_;
      switch (IATT_(self__,28)) {
         case (31) :
            gl1921_ = PATT_(self__,12);
            cache_dispatch_(gl1921_,418,gl1922_,INTVAL_(gl1923_));
            co__ = (ptr)PFN_(gl1923_)(gl1921_);
            gl1924_ = PATT_(self__,12);
            cache_dispatch_(gl1924_,1893,gl1925_,INTVAL_(gl1926_));
            if (CFN_(gl1926_)(gl1924_)) {
               PRI187_cprint_new_(0,outfile__,1,1,self__);
            }
            else {
               gl1927_ = PATT_(self__,12);
               cache_dispatch_(gl1927_,1894,gl1928_,INTVAL_(gl1929_));
               if (CFN_(gl1929_)(gl1927_)) {
                  PRI187_cprint_new_(0,outfile__,2,1,self__);
               }
               else {
                  gl1930_ = PATT_(self__,12);
                  cache_dispatch_(gl1930_,1895,gl1931_,INTVAL_(gl1932_));
                  if (CFN_(gl1932_)(gl1930_)) {
                     PRI187_cprint_new_(0,outfile__,3,1,self__);
                  }
                  else {
                     gl1933_ = PATT_(self__,12);
                     cache_dispatch_(gl1933_,1896,gl1934_,INTVAL_(gl1935_));
                     if (CFN_(gl1935_)(gl1933_)) {
                        PRI187_cprint_new_(0,outfile__,4,1,self__);
                     }
                     else {
                     }
                  }
               }
            }
            goto ret0__;
            break;
         case (30) :
            gl1936_ = PATT_(self__,12);
            cache_dispatch_(gl1936_,418,gl1937_,INTVAL_(gl1938_));
            co__ = (ptr)PFN_(gl1938_)(gl1936_);
            gl1939_ = PATT_(self__,12);
            cache_dispatch_(gl1939_,1893,gl1940_,INTVAL_(gl1941_));
            if (CFN_(gl1941_)(gl1939_)) {
               (void)SAT99_s_(outfile__,(ptr)(&ls1897_));
               if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
                  ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1898_)));
               }
               else {
               }
               (void)SAT99_c_(outfile__,',');
               ID_112_cprint_ith_arg_act_code_(self__,0,outfile__);
               (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(co__,60)),')');
            }
            else {
               gl1942_ = PATT_(self__,12);
               cache_dispatch_(gl1942_,1894,gl1943_,INTVAL_(gl1944_));
               if (CFN_(gl1944_)(gl1942_)) {
                  (void)SAT99_s_(outfile__,(ptr)(&ls1899_));
                  if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
                     ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1898_)));
                  }
                  else {
                  }
                  (void)SAT99_c_(outfile__,',');
                  ID_112_cprint_ith_arg_act_code_(self__,0,outfile__);
                  (void)SAT99_s_(outfile__,(ptr)(&ls659_));
                  ID_112_cprint_ith_arg_act_code_(self__,1,outfile__);
                  (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(co__,60)),')');
               }
               else {
                  gl1945_ = PATT_(self__,12);
                  cache_dispatch_(gl1945_,1895,gl1946_,INTVAL_(gl1947_));
                  if (CFN_(gl1947_)(gl1945_)) {
                     (void)SAT99_s_(outfile__,(ptr)(&ls1900_));
                     if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
                        ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1898_)));
                     }
                     else {
                     }
                     (void)SAT99_c_(outfile__,',');
                     ID_112_cprint_ith_arg_act_code_(self__,0,outfile__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls659_));
                     ID_112_cprint_ith_arg_act_code_(self__,1,outfile__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls659_));
                     ID_112_cprint_ith_arg_act_code_(self__,2,outfile__);
                     (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(co__,60)),')');
                  }
                  else {
                     gl1948_ = PATT_(self__,12);
                     cache_dispatch_(gl1948_,1896,gl1949_,INTVAL_(gl1950_));
                     if (CFN_(gl1950_)(gl1948_)) {
                        (void)SAT99_s_(outfile__,(ptr)(&ls1901_));
                        if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
                           ERR96_format_error_msg_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1898_)));
                        }
                        else {
                        }
                        (void)SAT99_c_(outfile__,',');
                        ID_112_cprint_ith_arg_act_code_(self__,0,outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls659_));
                        ID_112_cprint_ith_arg_act_code_(self__,1,outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls659_));
                        ID_112_cprint_ith_arg_act_code_(self__,2,outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls659_));
                        ID_112_cprint_ith_arg_act_code_(self__,3,outfile__);
                        (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(co__,60)),')');
                     }
                     else {
                     }
                  }
               }
            }
            goto ret0__;
            break;
         case (33) :
            (void)SAT99_s_(outfile__,(ptr)(&ls1732_));
            break;
         case (34) :
            (void)GLO94_cprint_ref_to_self_(0,outfile__);
            break;
         case (37) :
            (void)SAT99_c_(outfile__,'0');
            break;
         case (23) :
         case (24) :
         case (25) :
         case (26) :
         case (27) :
         case (28) :
            ERR96_format_error_exit_(0,IATT_(self__,56),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1902_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1903_)));
            break;
         case (32) :
            ERR96_format_error_exit_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1904_)));
            break;
         default:
            ;
            ;
      }
      goto ret0__;
   }
   else {
      ID_112_cprint_cast_code_(self__,outfile__);
      gl1951_ = PATT_(self__,36);
      cache_dispatch_(gl1951_,808,gl1952_,INTVAL_(gl1953_));
      VFN_(gl1953_)(gl1951_,outfile__);
      (void)SAT99_c_(outfile__,'(');
      if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
         ERR96_format_warning_msg_(0,IATT_(self__,56),STR20_s_(STR20_create_(0),(ptr)(&ls1905_)));
      }
      else {
      }
      i__ = (int)0;
      asz__ = (int)IATT_(PATT_(self__,32),12);
      while (1) {
         if ((i__ >= asz__)) {
            goto goto_tag_1954_;
         }
         else {
         }
         (void)SAT99_c_(outfile__,',');
         ID_112_cprint_ith_arg_act_code_(self__,i__,outfile__);
         i__ = (int)(i__ + 1);
      }
   goto_tag_1954_: ;
      (void)SAT99_c_(outfile__,')');
   }

   ret0__:
   return;
}

ID_112_cprint_cast_code_(self__,outfile__)
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

ID_112_cprint_ith_arg_pre_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl1955_;
   static int gl1956_;
   static union dtype_ gl1957_;
   ptr gl1958_;
   static int gl1959_;
   static union dtype_ gl1960_;
   ptr    ith_arg__ = 0;
   int    ith_temp__ = S_int_VOID_;

   ith_arg__ = (ptr)PATT_(PATT_(self__,32), 28 + ((i__) << 2));
   ith_temp__ = (int)IATT_(PATT_(self__,48), 8 + ((i__) << 2));
   gl1955_ = ith_arg__;
   cache_dispatch_(gl1955_,1589,gl1956_,INTVAL_(gl1957_));
   VFN_(gl1957_)(gl1955_,outfile__);
   if ((ith_temp__ > 0)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl1958_ = ith_arg__;
      cache_dispatch_(gl1958_,965,gl1959_,INTVAL_(gl1960_));
      VFN_(gl1960_)(gl1958_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }

   ret0__:
   return;
}

ID_112_cprint_ith_arg_act_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   ptr gl1961_;
   static int gl1962_;
   static union dtype_ gl1963_;
   int    ith_temp__ = S_int_VOID_;

   ith_temp__ = (int)IATT_(PATT_(self__,48), 8 + ((i__) << 2));
   if ((ith_temp__ > 0)) {
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
   }
   else {
      gl1961_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
      cache_dispatch_(gl1961_,965,gl1962_,INTVAL_(gl1963_));
      VFN_(gl1963_)(gl1961_,outfile__);
   }

   ret0__:
   return;
}

