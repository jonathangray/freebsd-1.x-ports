/* expr_a113.c : Sather class: EXPR_ARGS_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_min_();
extern ptr INT15_to_s_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_();
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*shared*/ char GLO68_semant_lval_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*constant*/ int RES71_res_ind_;
extern /*constant*/ int RES71_self_ind_;
extern /*constant*/ int RES71_void_ind_;
extern /*constant*/ int RES71_type_ind_;
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern /*constant*/ int RES71_copy_ind_;
extern /*constant*/ int RES71_extend_ind_;
extern /*constant*/ int RES71_new_ind_;
extern /*constant*/ int RES71_asize_ind_;
extern /*constant*/ int RES71_asize1_ind_;
extern /*constant*/ int RES71_asize2_ind_;
extern /*constant*/ int RES71_asize3_ind_;
extern /*constant*/ int RES71_asize4_ind_;
extern /*shared*/ ptr GLO68_double_typeob_s_;
extern /*shared*/ char GLO68_pre_semant_;
extern /*shared*/ ptr GLO68_other_cnames_;
extern /*shared*/ int GLO68_g_tag_;
extern /*shared*/ ptr GLO68_class_inst_;
extern ptr CLA93_at_index_();
extern GLO94_check_f_ob_();
extern int GLO94_global_key_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern ERR96_format_error_exit_();
extern /*constant*/ int RES97_FOB_ici_;
extern ERR96_format_warning_msg_();
extern ERR96_compiler_error_msg_();
extern char GLO94_conform_tst_();
extern /*constant*/ int RES97_REAL_ici_;
extern /*constant*/ int RES97_C_ici_;
extern int GLO94_key_of_class_feat_();
extern GLO94_cprint_ctemp_name_();
extern ptr SAT99_s_();
extern char GLO94_check_is_on_();
extern ptr SAT99_c_();
extern ptr SAT99_i_();
extern ptr LIS98_create_();
extern ptr LIS98_push_();
extern ptr LIS98_append_();
extern ptr INT106_create_();
extern ptr SAT99_indent_();
extern ptr SAT99_inc_ln_();
extern char EXP117_access_value_only_();
extern char EXP117_to_be_pre_evaluated_();
extern EXP117_semant_();
extern EXP117_out_of_line_();
extern ptr EXP117_dup_();
extern ptr EXP117_sather_code_();
extern ptr LST120_create_();
extern LST120_out_of_line_();
extern ptr LST120_dup_();
extern ptr LST120_sather_code_();
extern EXP117_resolve_predef_types_();
extern LST120_resolve_predef_types_();
extern LST120_semant_();
extern EXP117_cprint_act_code_();
extern EXP117_cprint_init_code_();
extern LST120_cprint_init_code_();
extern char EXP117_valid_init_expr_();
extern char LST120_valid_init_expr_();
extern ptr LST120_gen_temps_();
extern ptr EXP117_gen_temps_();
extern EXP117_get_ext_strs_();
extern LST120_get_ext_strs_();
extern EXP117_cprint_pre_code_();
extern GLO94_cprint_curr_exp_code_();
extern /*constant*/ int RES97_OB_ici_;
extern ptr CLA148_full_name_();
extern ptr TYP149_inst_cls_();
extern int TYP149_inst_ind_();
extern CON151_consistent_defs_();
extern SHA152_consistent_defs_();
extern ATT153_consistent_defs_();
extern ptr TYP149_full_name_();
extern int TYP149_ctype_();
extern ROU156_consistent_defs_();
extern char TYP149_array_type_p_();
extern char TYP149_array2_type_p_();
extern char TYP149_array3_type_p_();
extern char TYP149_array4_type_p_();
extern char TYP149_int_type_p_();
extern char TYP149_nonptr_p_();
extern char TYP149_is_dispatched_();
extern char INT164_get_();
extern ptr INT164_insert_();
extern ptr TYP149_paramstype_();
extern char TYP149_real_type_p_();
extern char TYP149_double_type_p_();
extern /*constant*/ int C_T168_c_double_;
extern ptr TYP149_rettype_();
extern ptr INS150_inst_cls_();
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ int C_T168_c_void_;
extern int CON151_is_a_str_const_();
extern char TYP149_bool_type_p_();
extern char TYP149_char_type_p_();
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ ptr C_T168_c_ptr_name_;
extern /*constant*/ ptr C_T168_c_char_name_;
extern /*constant*/ ptr C_T168_c_int_name_;
extern /*constant*/ ptr C_T168_c_float_name_;
extern /*constant*/ ptr C_T168_c_double_name_;
extern int INS150_inst_ind_();
extern ptr SYM186_get_feature_();
extern PRI187_cprint_type_access_();
extern ptr SEM188_typeof_();
extern SEM188_cprint_offset_();
extern PRI187_cprint_atomic_check_();
extern SEM188_cprint_init_code_();
extern ptr SEM188_get_constval_();
extern PRI187_cprint_void_tst_();
extern PRI187_cprint_array_dispatch_();
extern PRI187_cprint_cache_dispatch_();
extern PRI187_cprint_new_();
extern PRI187_cprint_copy_();
extern PRI187_cprint_int_attr_access_();
extern PRI187_cprint_ptr_attr_access_();
extern PRI187_cprint_char_attr_access_();
extern PRI187_cprint_float_attr_access_();
extern PRI187_cprint_double_attr_access_();
extern SEM188_cprint_access_value_();
extern SEM188_cprint_cname_();
extern ROU156_update_used_in_dispatch_();
extern PRI187_cprint_ob_cache_dispatch_();
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int EXP113_print_indent_ = 2;
ptr EXP113_create_();
EXP113_out_of_line_();
ptr EXP113_dup_();
EXP113_put_kwdname_();
ptr EXP113_sather_code_();
ptr EXP113_initialize_();
EXP113_resolve_predef_types_();
EXP113_semant_();
ptr EXP113_typeof_();
int EXP113_get_offset_();
EXP113_cprint_offset_();
ptr EXP113_get_constval_();
EXP113_cont_cprint_code_();
EXP113_cprint_cname_();
EXP113_cprint_extern_();
EXP113_cprint_access_value_();
EXP113_cprint_init_code_();
char EXP113_valid_init_expr_();
char EXP113_assignable_p_();
ptr EXP113_gen_temps_();
ptr EXP113_expr_eval_constant_();
EXP113_get_ext_strs_();
char EXP113_to_be_pre_evaluated_();
char EXP113_access_value_only_();
EXP113_fob_error_();
EXP113_cprint_pre_code_();
EXP113_cprint_act_code_();
EXP113_cprint_cast_code_();
char EXP113_handle_spec_expr_();
char EXP113_handle_spec_expr_pre_code_();
char EXP113_handle_spec_expr_act_code_();
EXP113_cprint_ith_arg_pre_code_();
EXP113_cprint_ith_arg_act_code_();
char EXP113_requires_dispatched_call_();
/*constant*/ int EXP113_cont1_ = 1;
/*constant*/ int EXP113_cont2_ = 2;
/*constant*/ int EXP113_cont3_ = 3;
/*constant*/ int EXP113_cont4_ = 4;
/*constant*/ int EXP113_cont5_ = 5;
/*constant*/ int EXP113_cont6_ = 6;
/*constant*/ int EXP113_cont7_ = 7;
/*constant*/ int EXP113_cont8_ = 8;
/*constant*/ int EXP113_cont9_ = 9;
/*constant*/ int EXP113_cont10_ = 10;
/*constant*/ int EXP113_cont11_ = 11;
/*constant*/ int EXP113_cont12_ = 12;
/*constant*/ int EXP113_cont13_ = 13;
/*constant*/ int EXP113_cont14_ = 14;
/*constant*/ int EXP113_cont17_ = 17;
/*constant*/ int EXP113_cont19_ = 19;
/*constant*/ int EXP113_cont21_ = 21;
/*constant*/ int EXP113_cont23_ = 23;
/*constant*/ int EXP113_cont15_ = 15;
/*constant*/ int EXP113_cont16_ = 16;
/*constant*/ int EXP113_cont18_ = 18;
/*constant*/ int EXP113_cont20_ = 20;
/*constant*/ int EXP113_cont22_ = 22;
/*constant*/ int EXP113_cont24_ = 24;
/*constant*/ int EXP113_cont25_ = 25;
extern int attr_ent_EXP113[];

ptr EXP113_create_(self__,o__,f__,alst__,ln__)
ptr self__;
ptr o__;
int f__;
ptr alst__;
int ln__;
{
   ptr res__ = 0;
   int    sz__ = S_int_VOID_;

   res__ = (ptr)new_(113,0);
   PATT_(res__,24) = (ptr)o__;
   IATT_(res__,28) = (int)f__;
   sz__ = S_int_VOID_;
   if ((alst__ != 0)) {
      PATT_(res__,32) = (ptr)alst__;
      sz__ = (int)IATT_(alst__,12);
   }
   else {
      PATT_(res__,32) = (ptr)LST120_create_(0,1);
   }
   IATT_(res__,80) = (int)ln__;
   PATT_(res__,72) = (ptr)new1_(163,sz__,1);
   PATT_(res__,76) = (ptr)new1_(163,sz__,1);

   ret0__:
   return (res__);
}

EXP113_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl1964_;
   static int gl1965_;
   static union dtype_ gl1966_;

   IATT_(self__,80) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,80));
   gl1964_ = PATT_(self__,24);
   cache_dispatch_(gl1964_,518,gl1965_,INTVAL_(gl1966_));
   VFN_(gl1966_)(gl1964_,fn__);
   LST120_out_of_line_(PATT_(self__,32),fn__);

   ret0__:
   return;
}

ptr EXP113_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1967_;
   static int gl1968_;
   static union dtype_ gl1969_;
   ptr gl165_;

   gl1967_ = PATT_(self__,24);
   cache_dispatch_(gl1967_,471,gl1968_,INTVAL_(gl1969_));
   gl165_ = PFN_(gl1969_)(gl1967_);
   res__ = (ptr)EXP113_create_(self__,gl165_,IATT_(self__,28),LST120_dup_(PATT_(self__,32)),IATT_(self__,80));

   ret0__:
   return (res__);
}

EXP113_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl1970_;
   static int gl1971_;
   static union dtype_ gl1972_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1970_ = x__;
   cache_dispatch_(gl1970_,796,gl1971_,INTVAL_(gl1972_));
   IATT_(gl1970_,INTVAL_(gl1972_)) = (int)nm__;

   ret0__:
   return;
}

ptr EXP113_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1973_;
   static int gl1974_;
   static union dtype_ gl1975_;
   ptr gl166_;

   gl1973_ = PATT_(self__,24);
   cache_dispatch_(gl1973_,801,gl1974_,INTVAL_(gl1975_));
   gl166_ = PFN_(gl1975_)(gl1973_);
   res__ = (ptr)STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),gl166_),'.'),STR69_at_index_(GLO68_str_table_,IATT_(self__,28)));
   if ((PATT_(self__,32) != 0)) {
      res__ = (ptr)STR20_c_(STR20_s_(STR20_c_(res__,'('),LST120_sather_code_(PATT_(self__,32))),')');
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr EXP113_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

EXP113_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl1976_;
   static int gl1977_;
   static union dtype_ gl1978_;

   gl1976_ = PATT_(self__,24);
   cache_dispatch_(gl1976_,522,gl1977_,INTVAL_(gl1978_));
   VFN_(gl1978_)(gl1976_,index__);
   LST120_resolve_predef_types_(PATT_(self__,32),index__);

   ret0__:
   return;
}

EXP113_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,32,ls1909_,"(EXPR_ARGS_EXPROB_S): Feature \"");
   SATHER_STR_(20,33,ls1910_,"\" applied to unknown object type");
   SATHER_STR_(20,48,ls1913_,"(EXPR_ARGS_EXPROB_S): Class has no symbol table");
   SATHER_STR_(20,32,ls1916_,"(EXPR_ARGS_EXPROB_S): Routine \"");
   SATHER_STR_(20,29,ls1917_,"\" is supposed to be private\n");
   SATHER_STR_(20,34,ls1918_,"(EXPR_ARGS_EXPROB_S): Attribute \"");
   SATHER_STR_(20,33,ls1919_,"(EXPR_ARGS_EXPROB_S): Constant \"");
   SATHER_STR_(20,31,ls1920_,"(EXPR_ARGS_EXPROB_S): Shared \"");
   SATHER_STR_(20,33,ls1923_,"(EXPR_ARGS_EXPROB_S): Argument #");
   SATHER_STR_(20,20,ls1807_," has no return type");
   SATHER_STR_(20,42,ls1924_,"(EXPR_ARGS_EXPROB_S): Cannot dispatch on ");
   SATHER_STR_(20,27,ls1925_," which is a foreign object");
   SATHER_STR_(20,24,ls1906_,"(EXPR_ARGS_EXPROB_S): \"");
   SATHER_STR_(20,31,ls1926_,"\" is invalid in dot expression");
   SATHER_STR_(20,63,ls1927_,"(EXPR_ARGS_EXPROB_S): Unexpected arguments supplied for \"type\"");
   SATHER_STR_(20,27,ls1928_,"\" does not expect argument");
   SATHER_STR_(20,61,ls1929_,"(EXPR_ARGS_EXPROB_S): Cannot use non-array type for \"extend\"");
   SATHER_STR_(20,28,ls1930_," is non-integer in \"extend\"");
   SATHER_STR_(20,38,ls1931_,"(EXPR_ARGS_EXPROB_S): \"extend\" needs ");
   SATHER_STR_(20,27,ls1932_," argument(s) but is given ");
   SATHER_STR_(20,13,ls1933_," argument(s)");
   SATHER_STR_(20,54,ls1934_,"(EXPR_ARGS_EXPROB_S): Cannot use basic type for \"new\"");
   SATHER_STR_(20,141,ls1936_,"(EXPR_ARGS_EXPROB_S): Applying \"new\" on non-array dispatched object with arguments; compiler cannot verify exact number of arguments needed\n");
   SATHER_STR_(20,26,ls1938_," is non-integer in \"new\"\n");
   SATHER_STR_(20,35,ls1939_,"(EXPR_ARGS_EXPROB_S): \"new\" needs ");
   SATHER_STR_(20,25,ls1875_," arguments but is given ");
   SATHER_STR_(20,39,ls1940_,"(EXPR_ARGS_EXPROB_S): Abstract class \"");
   SATHER_STR_(20,21,ls1696_,"\" used in \"new\" call");
   SATHER_STR_(20,28,ls1941_,"\"'s unexpected args ignored");
   SATHER_STR_(20,33,ls1942_,"(EXPR_ARGS_EXPROB_S): Applying \"");
   SATHER_STR_(20,22,ls1943_,"\" to non-array object");
   SATHER_STR_(20,75,ls1944_,"(EXPR_ARGS_EXPROB_S): Applying \"asize2\" to non-array or 1-dim array object");
   SATHER_STR_(20,26,ls1945_,"\" to inappropriate object");
   SATHER_STR_(20,70,ls1946_,"(EXPR_ARGS_EXPROB_S): Applying \"asize4\" to inappropriate-sized object");
   SATHER_STR_(20,43,ls1948_,"(EXPR_ARGS_EXPROB_S): Unknown identifier \"");
   SATHER_STR_(20,7,ls1708_,"\" used");
   SATHER_STR_(20,19,ls1667_,"EXPR_ARGS_EXPROB_S");
   SATHER_STR_(20,47,ls1949_,"Dotted expression cannot refer to a variable \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,51,ls1950_,"(EXPR_ARGS_EXPROB_S): Unexpected arguments ignored");
   SATHER_STR_(20,32,ls1879_,"\" gets incorrect number of args");
   SATHER_STR_(20,10,ls1882_,"<UNKNOWN>");
   SATHER_STR_(20,22,ls1883_," has incorrect type (");
   SATHER_STR_(20,8,ls1884_,") for \"");
   SATHER_STR_(20,4,ls1885_,"\" (");
   SATHER_STR_(20,24,ls1735_,"\" has unknown referrent");
   ptr gl1979_;
   static int gl1980_;
   static union dtype_ gl1981_;
   ptr gl1982_;
   static int gl1983_;
   static union dtype_ gl1984_;
   ptr gl1985_;
   static int gl1986_;
   static union dtype_ gl1987_;
   ptr gl1988_;
   static int gl1989_;
   static union dtype_ gl1990_;
   ptr gl1991_;
   static int gl1992_;
   static union dtype_ gl1993_;
   ptr gl1994_;
   static int gl1995_;
   static union dtype_ gl1996_;
   ptr gl1997_;
   static int gl1998_;
   static union dtype_ gl1999_;
   ptr gl2000_;
   static int gl2001_;
   static union dtype_ gl2002_;
   ptr gl172_;
   ptr gl2003_;
   static int gl2004_;
   static union dtype_ gl2005_;
   ptr gl2006_;
   static int gl2007_;
   static union dtype_ gl2008_;
   ptr gl2009_;
   static int gl2010_;
   static union dtype_ gl2011_;
   ptr gl173_;
   ptr gl2012_;
   static int gl2013_;
   static union dtype_ gl2014_;
   char gl175_;
   ptr gl2015_;
   static int gl2016_;
   static union dtype_ gl2017_;
   ptr gl2018_;
   static int gl2019_;
   static union dtype_ gl2020_;
   ptr gl2021_;
   static int gl2022_;
   static union dtype_ gl2023_;
   ptr gl2024_;
   static int gl2025_;
   static union dtype_ gl2026_;
   ptr gl2027_;
   static int gl2028_;
   static union dtype_ gl2029_;
   ptr gl2030_;
   static int gl2031_;
   static union dtype_ gl2032_;
   ptr gl2033_;
   static int gl2034_;
   static union dtype_ gl2035_;
   char gl177_;
   char gl178_;
   ptr gl2036_;
   static int gl2037_;
   static union dtype_ gl2038_;
   char gl179_;
   ptr gl2039_;
   static int gl2040_;
   static union dtype_ gl2041_;
   char gl180_;
   ptr gl2042_;
   static int gl2043_;
   static union dtype_ gl2044_;
   ptr gl2045_;
   static int gl2046_;
   static union dtype_ gl2047_;
   ptr gl2048_;
   static int gl2049_;
   static union dtype_ gl2050_;
   char gl182_;
   ptr gl2051_;
   static int gl2052_;
   static union dtype_ gl2053_;
   ptr gl2054_;
   static int gl2055_;
   static union dtype_ gl2056_;
   ptr gl2057_;
   static int gl2058_;
   static union dtype_ gl2059_;
   ptr gl2060_;
   static int gl2061_;
   static union dtype_ gl2062_;
   ptr gl2063_;
   static int gl2064_;
   static union dtype_ gl2065_;
   ptr gl2066_;
   static int gl2067_;
   static union dtype_ gl2068_;
   ptr gl2069_;
   static int gl2070_;
   static union dtype_ gl2071_;
   ptr gl2072_;
   static int gl2073_;
   static union dtype_ gl2074_;
   char gl184_;
   ptr gl2075_;
   static int gl2076_;
   static union dtype_ gl2077_;
   ptr gl2078_;
   static int gl2079_;
   static union dtype_ gl2080_;
   ptr gl2081_;
   static int gl2082_;
   static union dtype_ gl2083_;
   ptr gl2084_;
   static int gl2085_;
   static union dtype_ gl2086_;
   ptr gl2087_;
   static int gl2088_;
   static union dtype_ gl2089_;
   char gl185_;
   char gl186_;
   ptr gl2090_;
   static int gl2091_;
   static union dtype_ gl2092_;
   char gl187_;
   ptr gl2093_;
   static int gl2094_;
   static union dtype_ gl2095_;
   char gl188_;
   ptr gl2096_;
   static int gl2097_;
   static union dtype_ gl2098_;
   ptr gl2099_;
   static int gl2100_;
   static union dtype_ gl2101_;
   char gl189_;
   char gl190_;
   ptr gl2102_;
   static int gl2103_;
   static union dtype_ gl2104_;
   char gl191_;
   ptr gl2105_;
   static int gl2106_;
   static union dtype_ gl2107_;
   ptr gl2108_;
   static int gl2109_;
   static union dtype_ gl2110_;
   char gl192_;
   char gl193_;
   ptr gl2111_;
   static int gl2112_;
   static union dtype_ gl2113_;
   ptr gl2114_;
   static int gl2115_;
   static union dtype_ gl2116_;
   ptr gl2117_;
   static int gl2118_;
   static union dtype_ gl2119_;
   ptr gl2120_;
   static int gl2121_;
   static union dtype_ gl2122_;
   ptr gl2123_;
   static int gl2124_;
   static union dtype_ gl2125_;
   ptr gl2126_;
   static int gl2127_;
   static union dtype_ gl2128_;
   ptr gl195_;
   ptr gl2129_;
   static int gl2130_;
   static union dtype_ gl2131_;
   ptr gl196_;
   ptr gl2132_;
   static int gl2133_;
   static union dtype_ gl2134_;
   ptr gl2135_;
   static int gl2136_;
   static union dtype_ gl2137_;
   ptr gl2138_;
   static int gl2139_;
   static union dtype_ gl2140_;
   ptr gl2141_;
   static int gl2142_;
   static union dtype_ gl2143_;
   ptr gl2144_;
   static int gl2145_;
   static union dtype_ gl2146_;
   ptr gl2147_;
   static int gl2148_;
   static union dtype_ gl2149_;
   ptr gl2150_;
   static int gl2151_;
   static union dtype_ gl2152_;
   char gl197_;
   char gl198_;
   char gl199_;
   ptr gl2153_;
   static int gl2154_;
   static union dtype_ gl2155_;
   ptr gl2156_;
   static int gl2157_;
   static union dtype_ gl2158_;
   ptr gl2159_;
   static int gl2160_;
   static union dtype_ gl2161_;
   ptr gl2162_;
   static int gl2163_;
   static union dtype_ gl2164_;
   int gl200_;
   ptr gl2165_;
   static int gl2166_;
   static union dtype_ gl2167_;
   ptr gl2168_;
   static int gl2169_;
   static union dtype_ gl2170_;
   int gl201_;
   ptr gl2171_;
   static int gl2172_;
   static union dtype_ gl2173_;
   ptr gl2174_;
   static int gl2175_;
   static union dtype_ gl2176_;
   int gl202_;
   ptr gl2177_;
   static int gl2178_;
   static union dtype_ gl2179_;
   ptr    obj_res_type__ = 0;
   ptr    obj_cls__ = 0;
   ptr    alt_symtab__ = 0;
   ptr    rout_feat__ = 0;
   ptr    attr_feat__ = 0;
   ptr    const_feat__ = 0;
   ptr    shared_feat__ = 0;
   ptr    rout_feat_167_ = 0;
   ptr    attr_feat_168_ = 0;
   ptr    const_feat_169_ = 0;
   ptr    shared_feat_170_ = 0;
   int    curr_callee__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    descendents__ = 0;
   int    sz__ = S_int_VOID_;
   int    index__ = S_int_VOID_;
   int    num_args__ = S_int_VOID_;
   int    i_171_ = S_int_VOID_;
   int    i_174_ = S_int_VOID_;
   int    i_176_ = S_int_VOID_;
   int    i_181_ = S_int_VOID_;
   char    ignore_num_args__ = S_char_VOID_;
   int    i_183_ = S_int_VOID_;
   int    min__ = S_int_VOID_;
   int    i_194_ = S_int_VOID_;
   ptr    ptypes__ = 0;
   int    num_params__ = S_int_VOID_;
   ptr    tn1__ = 0;
   ptr    tn2__ = 0;
   ptr    co__ = 0;
   int    cls_ind__ = S_int_VOID_;
   int    k__ = S_int_VOID_;

   gl1979_ = PATT_(self__,24);
   cache_dispatch_(gl1979_,588,gl1980_,INTVAL_(gl1981_));
   VFN_(gl1981_)(gl1979_,symtab__);
   gl1982_ = PATT_(self__,24);
   cache_dispatch_(gl1982_,1577,gl1983_,INTVAL_(gl1984_));
   obj_res_type__ = (ptr)PATT_(gl1982_,INTVAL_(gl1984_));
   if ((obj_res_type__ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1909_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1910_)));
      gl1985_ = PATT_(self__,24);
      cache_dispatch_(gl1985_,1577,gl1986_,INTVAL_(gl1987_));
      PATT_(gl1985_,INTVAL_(gl1987_)) = (ptr)GLO68_ob_typeob_s_;
      obj_res_type__ = (ptr)GLO68_ob_typeob_s_;
   }
   else {
   }
   gl1988_ = obj_res_type__;
   cache_dispatch_(gl1988_,418,gl1989_,INTVAL_(gl1990_));
   obj_cls__ = (ptr)PFN_(gl1990_)(gl1988_);
   alt_symtab__ = (ptr)PATT_(obj_cls__,48);
   if ((alt_symtab__ == 0)) {
      ERR96_format_error_exit_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1913_)));
   }
   else {
   }
   if ((IATT_(obj_cls__,96) == RES97_FOB_ici_)) {
      GLO94_check_f_ob_(0,self__,IATT_(self__,28),alt_symtab__);
   }
   else {
   }
   if ((PATT_(self__,36) == 0)) {
      PATT_(self__,36) = (ptr)SYM186_get_feature_(alt_symtab__,IATT_(self__,28));
      if (EXP113_requires_dispatched_call_(self__)) {
         if ((PATT_(self__,36) != 0)) {
            gl1991_ = PATT_(self__,36);
            switch (TYPE_(gl1991_)) {
               case (156) :
                  rout_feat__ = (ptr)PATT_(self__,36);
                  ROU156_consistent_defs_(rout_feat__,IATT_(self__,28),GLO68_semant_lval_);
                  break;
               case (153) :
                  attr_feat__ = (ptr)PATT_(self__,36);
                  ATT153_consistent_defs_(attr_feat__,IATT_(self__,28),GLO68_semant_lval_);
                  break;
               case (151) :
                  const_feat__ = (ptr)PATT_(self__,36);
                  CON151_consistent_defs_(const_feat__,IATT_(self__,28),GLO68_semant_lval_);
                  break;
               case (152) :
                  shared_feat__ = (ptr)PATT_(self__,36);
                  SHA152_consistent_defs_(shared_feat__,IATT_(self__,28),GLO68_semant_lval_);
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
      if ((PATT_(self__,36) != 0)) {
         gl1994_ = PATT_(self__,36);
         switch (TYPE_(gl1994_)) {
            case (156) :
               rout_feat_167_ = (ptr)PATT_(self__,36);
               CATT_(rout_feat_167_,11) = (char)1;
               if ((CATT_(rout_feat_167_,5) & (PATT_(rout_feat_167_,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1916_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1917_)));
               }
               else {
               }
               break;
            case (153) :
               attr_feat_168_ = (ptr)PATT_(self__,36);
               CATT_(attr_feat_168_,11) = (char)1;
               if ((CATT_(attr_feat_168_,5) & (PATT_(attr_feat_168_,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1918_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1917_)));
               }
               else {
               }
               break;
            case (151) :
               const_feat_169_ = (ptr)PATT_(self__,36);
               CATT_(const_feat_169_,11) = (char)1;
               if ((CATT_(const_feat_169_,5) & (PATT_(const_feat_169_,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1919_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1917_)));
               }
               else {
               }
               break;
            case (152) :
               shared_feat_170_ = (ptr)PATT_(self__,36);
               CATT_(shared_feat_170_,11) = (char)1;
               if ((CATT_(shared_feat_170_,5) & (PATT_(shared_feat_170_,32) != GLO68_curr_class_inst_))) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1920_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1917_)));
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
   if ((PATT_(self__,36) != 0)) {
      gl1997_ = obj_res_type__;
      cache_dispatch_(gl1997_,1547,gl1998_,INTVAL_(gl1999_));
      curr_callee__ = (int)IFN_(gl1999_)(gl1997_);
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
      if (EXP113_requires_dispatched_call_(self__)) {
         i__ = S_int_VOID_;
         descendents__ = (ptr)PATT_(obj_cls__,76);
         sz__ = (int)IATT_(descendents__,12);
         while (1) {
            if ((i__ >= sz__)) {
               goto goto_tag_2180_;
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
      goto_tag_2180_: ;
      }
      else {
      }
   }
   else {
   }
   num_args__ = (int)0;
   if ((PATT_(self__,32) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,32),12);
      LST120_semant_(PATT_(self__,32),symtab__);
   }
   else {
   }
   i_171_ = S_int_VOID_;
   while (1) {
      if ((i_171_ >= num_args__)) {
         goto goto_tag_2181_;
      }
      else {
      }
      gl2000_ = PATT_(PATT_(self__,32), 28 + ((i_171_) << 2));
      cache_dispatch_(gl2000_,1577,gl2001_,INTVAL_(gl2002_));
      gl172_ = PATT_(gl2000_,INTVAL_(gl2002_));
      if ((gl172_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1923_)),(i_171_ + 1)),(ptr)(&ls1807_)));
         gl2003_ = PATT_(PATT_(self__,32), 28 + ((i_171_) << 2));
         cache_dispatch_(gl2003_,1577,gl2004_,INTVAL_(gl2005_));
         PATT_(gl2003_,INTVAL_(gl2005_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
      }
      i_171_ = (int)(i_171_ + 1);
   }
goto_tag_2181_: ;
   if (EXP113_requires_dispatched_call_(self__)) {
      gl2006_ = obj_res_type__;
      cache_dispatch_(gl2006_,418,gl2007_,INTVAL_(gl2008_));
      if ((IATT_(PFN_(gl2008_)(gl2006_),96) == RES97_FOB_ici_)) {
         gl2009_ = obj_res_type__;
         cache_dispatch_(gl2009_,426,gl2010_,INTVAL_(gl2011_));
         gl173_ = PFN_(gl2011_)(gl2009_);
         ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1924_)),gl173_),(ptr)(&ls1925_)));
      }
      else {
      }
      i_174_ = (int)0;
      while (1) {
         if ((i_174_ >= num_args__)) {
            goto goto_tag_2182_;
         }
         else {
         }
         gl2012_ = PATT_(PATT_(self__,32), 28 + ((i_174_) << 2));
         cache_dispatch_(gl2012_,1679,gl2013_,INTVAL_(gl2014_));
         gl175_ = CFN_(gl2014_)(gl2012_);
         if ((! gl175_)) {
            IATT_(PATT_(self__,72), 8 + ((i_174_) << 2)) = (int)GLO94_global_key_(0);
            gl2018_ = PATT_(PATT_(self__,32), 28 + ((i_174_) << 2));
            cache_dispatch_(gl2018_,1577,gl2019_,INTVAL_(gl2020_));
            gl2015_ = PATT_(gl2018_,INTVAL_(gl2020_));
            cache_dispatch_(gl2015_,374,gl2016_,INTVAL_(gl2017_));
            IATT_(PATT_(self__,76), 8 + ((i_174_) << 2)) = (int)IFN_(gl2017_)(gl2015_);
            CATT_(self__,4) = (char)1;
         }
         else {
         }
         i_174_ = (int)(i_174_ + 1);
      }
   goto_tag_2182_: ;
   }
   else {
      i_176_ = (int)0;
      while (1) {
         if ((i_176_ >= num_args__)) {
            goto goto_tag_2183_;
         }
         else {
         }
         gl2021_ = PATT_(PATT_(self__,32), 28 + ((i_176_) << 2));
         cache_dispatch_(gl2021_,1678,gl2022_,INTVAL_(gl2023_));
         if (CFN_(gl2023_)(gl2021_)) {
            IATT_(PATT_(self__,72), 8 + ((i_176_) << 2)) = (int)GLO94_global_key_(0);
            gl2027_ = PATT_(PATT_(self__,32), 28 + ((i_176_) << 2));
            cache_dispatch_(gl2027_,1577,gl2028_,INTVAL_(gl2029_));
            gl2024_ = PATT_(gl2027_,INTVAL_(gl2029_));
            cache_dispatch_(gl2024_,374,gl2025_,INTVAL_(gl2026_));
            IATT_(PATT_(self__,76), 8 + ((i_176_) << 2)) = (int)IFN_(gl2026_)(gl2024_);
            CATT_(self__,4) = (char)1;
         }
         else {
         }
         i_176_ = (int)(i_176_ + 1);
      }
   goto_tag_2183_: ;
   }
   if ((PATT_(self__,36) == 0)) {
      switch (IATT_(self__,28)) {
         case (33) :
         case (34) :
         case (37) :
            ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1926_)));
            PATT_(self__,8) = (ptr)GLO68_ob_typeob_s_;
            break;
         case (32) :
            if ((num_args__ == 0)) {
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1927_)));
               PATT_(self__,32) = (ptr)0;
            }
            PATT_(self__,8) = (ptr)GLO68_int_typeob_s_;
            goto ret0__;
            break;
         case (28) :
            PATT_(self__,8) = (ptr)obj_res_type__;
            if ((num_args__ == 0)) {
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1928_)));
               PATT_(self__,32) = (ptr)0;
            }
            goto ret0__;
            break;
         case (30) :
            PATT_(self__,8) = (ptr)obj_res_type__;
            gl2030_ = PATT_(self__,8);
            cache_dispatch_(gl2030_,1893,gl2031_,INTVAL_(gl2032_));
            gl177_ = CFN_(gl2032_)(gl2030_);
            gl2033_ = PATT_(self__,8);
            cache_dispatch_(gl2033_,1894,gl2034_,INTVAL_(gl2035_));
            gl178_ = CFN_(gl2035_)(gl2033_);
            gl2036_ = PATT_(self__,8);
            cache_dispatch_(gl2036_,1895,gl2037_,INTVAL_(gl2038_));
            gl179_ = CFN_(gl2038_)(gl2036_);
            gl2039_ = PATT_(self__,8);
            cache_dispatch_(gl2039_,1896,gl2040_,INTVAL_(gl2041_));
            gl180_ = CFN_(gl2041_)(gl2039_);
            if ((! (((gl177_ | gl178_) | gl179_) | gl180_))) {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1929_)));
            }
            else {
            }
            gl2042_ = PATT_(self__,8);
            cache_dispatch_(gl2042_,418,gl2043_,INTVAL_(gl2044_));
            if ((num_args__ == IATT_(PFN_(gl2044_)(gl2042_),100))) {
               i_181_ = (int)0;
               while (1) {
                  if ((i_181_ >= num_args__)) {
                     goto goto_tag_2184_;
                  }
                  else {
                  }
                  gl2048_ = PATT_(PATT_(self__,32), 28 + ((i_181_) << 2));
                  cache_dispatch_(gl2048_,1577,gl2049_,INTVAL_(gl2050_));
                  gl2045_ = PATT_(gl2048_,INTVAL_(gl2050_));
                  cache_dispatch_(gl2045_,1778,gl2046_,INTVAL_(gl2047_));
                  gl182_ = CFN_(gl2047_)(gl2045_);
                  if ((! gl182_)) {
                     ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1923_)),(i_181_ + 1)),(ptr)(&ls1930_)));
                  }
                  else {
                  }
                  i_181_ = (int)(i_181_ + 1);
               }
            goto_tag_2184_: ;
            }
            else {
               gl2051_ = PATT_(self__,8);
               cache_dispatch_(gl2051_,418,gl2052_,INTVAL_(gl2053_));
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1931_)),IATT_(PFN_(gl2053_)(gl2051_),100)),(ptr)(&ls1932_)),num_args__),(ptr)(&ls1933_)));
            }
            goto ret0__;
            break;
         case (31) :
            PATT_(self__,8) = (ptr)obj_res_type__;
            gl2054_ = PATT_(self__,8);
            cache_dispatch_(gl2054_,1549,gl2055_,INTVAL_(gl2056_));
            if (CFN_(gl2056_)(gl2054_)) {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1934_)));
            }
            else {
            }
            ignore_num_args__ = S_char_VOID_;
            gl2057_ = PATT_(self__,8);
            cache_dispatch_(gl2057_,1511,gl2058_,INTVAL_(gl2059_));
            if (CFN_(gl2059_)(gl2057_)) {
               ignore_num_args__ = (char)1;
               gl2060_ = PATT_(self__,8);
               cache_dispatch_(gl2060_,418,gl2061_,INTVAL_(gl2062_));
               if (((IATT_(PFN_(gl2062_)(gl2060_),100) == 0) & (num_args__ > 0))) {
                  ERR96_format_warning_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1936_)));
               }
               else {
               }
            }
            else {
            }
            gl2063_ = PATT_(self__,8);
            cache_dispatch_(gl2063_,418,gl2064_,INTVAL_(gl2065_));
            if (((num_args__ == IATT_(PFN_(gl2065_)(gl2063_),100)) | ignore_num_args__)) {
               i_183_ = (int)0;
               gl2066_ = PATT_(self__,8);
               cache_dispatch_(gl2066_,418,gl2067_,INTVAL_(gl2068_));
               min__ = (int)INT15_min_(num_args__,IATT_(PFN_(gl2068_)(gl2066_),100));
               while (1) {
                  if ((i_183_ >= num_args__)) {
                     goto goto_tag_2185_;
                  }
                  else {
                  }
                  gl2072_ = PATT_(PATT_(self__,32), 28 + ((i_183_) << 2));
                  cache_dispatch_(gl2072_,1577,gl2073_,INTVAL_(gl2074_));
                  gl2069_ = PATT_(gl2072_,INTVAL_(gl2074_));
                  cache_dispatch_(gl2069_,1778,gl2070_,INTVAL_(gl2071_));
                  gl184_ = CFN_(gl2071_)(gl2069_);
                  if ((! gl184_)) {
                     ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1923_)),(i_183_ + 1)),(ptr)(&ls1938_)));
                  }
                  else {
                  }
                  i_183_ = (int)(i_183_ + 1);
               }
            goto_tag_2185_: ;
            }
            else {
               gl2075_ = PATT_(self__,8);
               cache_dispatch_(gl2075_,418,gl2076_,INTVAL_(gl2077_));
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1939_)),IATT_(PFN_(gl2077_)(gl2075_),100)),(ptr)(&ls1875_)),num_args__));
            }
            gl2078_ = PATT_(self__,8);
            cache_dispatch_(gl2078_,418,gl2079_,INTVAL_(gl2080_));
            if (CATT_(PFN_(gl2080_)(gl2078_),4)) {
               gl2081_ = PATT_(self__,8);
               cache_dispatch_(gl2081_,418,gl2082_,INTVAL_(gl2083_));
               ERR96_format_error_exit_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1940_)),CLA148_full_name_(PFN_(gl2083_)(gl2081_))),(ptr)(&ls1696_)));
            }
            else {
            }
            goto ret0__;
            break;
         case (23) :
         case (24) :
            gl2084_ = obj_res_type__;
            cache_dispatch_(gl2084_,1893,gl2085_,INTVAL_(gl2086_));
            gl185_ = CFN_(gl2086_)(gl2084_);
            gl2087_ = obj_res_type__;
            cache_dispatch_(gl2087_,1894,gl2088_,INTVAL_(gl2089_));
            gl186_ = CFN_(gl2089_)(gl2087_);
            gl2090_ = obj_res_type__;
            cache_dispatch_(gl2090_,1895,gl2091_,INTVAL_(gl2092_));
            gl187_ = CFN_(gl2092_)(gl2090_);
            gl2093_ = obj_res_type__;
            cache_dispatch_(gl2093_,1896,gl2094_,INTVAL_(gl2095_));
            gl188_ = CFN_(gl2095_)(gl2093_);
            if ((((gl185_ | gl186_) | gl187_) | gl188_)) {
               if ((num_args__ > 0)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1941_)));
                  PATT_(self__,32) = (ptr)0;
               }
               else {
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1942_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1943_)));
            }
            PATT_(self__,8) = (ptr)GLO68_int_typeob_s_;
            break;
         case (25) :
            gl2096_ = obj_res_type__;
            cache_dispatch_(gl2096_,1894,gl2097_,INTVAL_(gl2098_));
            gl189_ = CFN_(gl2098_)(gl2096_);
            gl2099_ = obj_res_type__;
            cache_dispatch_(gl2099_,1895,gl2100_,INTVAL_(gl2101_));
            gl190_ = CFN_(gl2101_)(gl2099_);
            gl2102_ = obj_res_type__;
            cache_dispatch_(gl2102_,1896,gl2103_,INTVAL_(gl2104_));
            gl191_ = CFN_(gl2104_)(gl2102_);
            if (((gl189_ | gl190_) | gl191_)) {
               if ((num_args__ > 0)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1941_)));
                  PATT_(self__,32) = (ptr)0;
               }
               else {
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1944_)));
            }
            PATT_(self__,8) = (ptr)GLO68_int_typeob_s_;
            break;
         case (26) :
            gl2105_ = obj_res_type__;
            cache_dispatch_(gl2105_,1895,gl2106_,INTVAL_(gl2107_));
            gl192_ = CFN_(gl2107_)(gl2105_);
            gl2108_ = obj_res_type__;
            cache_dispatch_(gl2108_,1896,gl2109_,INTVAL_(gl2110_));
            gl193_ = CFN_(gl2110_)(gl2108_);
            if ((gl192_ | gl193_)) {
               if ((num_args__ > 0)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1941_)));
                  PATT_(self__,32) = (ptr)0;
               }
               else {
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1942_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1945_)));
            }
            PATT_(self__,8) = (ptr)GLO68_int_typeob_s_;
            break;
         case (27) :
            gl2111_ = obj_res_type__;
            cache_dispatch_(gl2111_,1896,gl2112_,INTVAL_(gl2113_));
            if (CFN_(gl2113_)(gl2111_)) {
               if ((num_args__ > 0)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1941_)));
                  PATT_(self__,32) = (ptr)0;
               }
               else {
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1946_)));
            }
            PATT_(self__,8) = (ptr)GLO68_int_typeob_s_;
            break;
         default:
            if ((! EXP113_handle_spec_expr_(self__))) {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1948_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1708_)));
               PATT_(self__,8) = (ptr)GLO68_ob_typeob_s_;
            }
            else {
            }
            ;
      }
   }
   else {
      gl2114_ = PATT_(self__,36);
      switch (TYPE_(gl2114_)) {
         case (145) :
         case (154) :
            ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1949_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls785_)));
            break;
         case (151) :
         case (152) :
         case (153) :
            if ((num_args__ > 0)) {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls1950_)));
               PATT_(self__,32) = (ptr)0;
            }
            else {
            }
            gl2117_ = PATT_(self__,36);
            cache_dispatch_(gl2117_,661,gl2118_,INTVAL_(gl2119_));
            PATT_(self__,8) = (ptr)PFN_(gl2119_)(gl2117_);
            break;
         case (156) :
            i_194_ = (int)0;
            gl2123_ = PATT_(self__,36);
            cache_dispatch_(gl2123_,661,gl2124_,INTVAL_(gl2125_));
            gl2120_ = PFN_(gl2125_)(gl2123_);
            cache_dispatch_(gl2120_,1709,gl2121_,INTVAL_(gl2122_));
            ptypes__ = (ptr)PFN_(gl2122_)(gl2120_);
            num_params__ = (int)0;
            if ((ptypes__ != 0)) {
               num_params__ = (int)IATT_(ptypes__,16);
            }
            else {
            }
            if ((num_params__ != num_args__)) {
               ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1879_)));
            }
            else {
            }
            if ((num_params__ < num_args__)) {
               num_args__ = (int)num_params__;
            }
            else {
            }
            while (1) {
               if ((i_194_ >= num_args__)) {
                  goto goto_tag_2186_;
               }
               else {
               }
               gl2126_ = PATT_(PATT_(self__,32), 28 + ((i_194_) << 2));
               cache_dispatch_(gl2126_,1577,gl2127_,INTVAL_(gl2128_));
               gl195_ = PATT_(gl2126_,INTVAL_(gl2128_));
               if ((! GLO94_conform_tst_(0,gl195_,PATT_(ptypes__, 28 + ((i_194_) << 2)),PATT_(PATT_(self__,32), 28 + ((i_194_) << 2))))) {
                  tn1__ = S_ptr_VOID_;
                  tn2__ = S_ptr_VOID_;
                  gl2129_ = PATT_(PATT_(self__,32), 28 + ((i_194_) << 2));
                  cache_dispatch_(gl2129_,1577,gl2130_,INTVAL_(gl2131_));
                  gl196_ = PATT_(gl2129_,INTVAL_(gl2131_));
                  if ((gl196_ != 0)) {
                     gl2135_ = PATT_(PATT_(self__,32), 28 + ((i_194_) << 2));
                     cache_dispatch_(gl2135_,1577,gl2136_,INTVAL_(gl2137_));
                     gl2132_ = PATT_(gl2135_,INTVAL_(gl2137_));
                     cache_dispatch_(gl2132_,426,gl2133_,INTVAL_(gl2134_));
                     tn1__ = (ptr)PFN_(gl2134_)(gl2132_);
                  }
                  else {
                     tn1__ = (ptr)(ptr)(&ls1882_);
                  }
                  if ((PATT_(ptypes__, 28 + ((i_194_) << 2)) != 0)) {
                     gl2138_ = PATT_(ptypes__, 28 + ((i_194_) << 2));
                     cache_dispatch_(gl2138_,426,gl2139_,INTVAL_(gl2140_));
                     tn2__ = (ptr)PFN_(gl2140_)(gl2138_);
                  }
                  else {
                     tn2__ = (ptr)(ptr)(&ls1882_);
                  }
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1923_)),(i_194_ + 1)),(ptr)(&ls1883_)),tn1__),(ptr)(&ls1884_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1885_)),tn2__),')'));
               }
               else {
                  gl2144_ = PATT_(PATT_(self__,32), 28 + ((i_194_) << 2));
                  cache_dispatch_(gl2144_,1577,gl2145_,INTVAL_(gl2146_));
                  gl2141_ = PATT_(gl2144_,INTVAL_(gl2146_));
                  cache_dispatch_(gl2141_,1778,gl2142_,INTVAL_(gl2143_));
                  gl199_ = CFN_(gl2143_)(gl2141_);
                  gl2147_ = PATT_(ptypes__, 28 + ((i_194_) << 2));
                  cache_dispatch_(gl2147_,1886,gl2148_,INTVAL_(gl2149_));
                  gl197_ = CFN_(gl2149_)(gl2147_);
                  gl2150_ = PATT_(ptypes__, 28 + ((i_194_) << 2));
                  cache_dispatch_(gl2150_,1887,gl2151_,INTVAL_(gl2152_));
                  gl198_ = CFN_(gl2152_)(gl2150_);
                  if ((gl199_ & (gl197_ | gl198_))) {
                     gl2153_ = PATT_(PATT_(self__,32), 28 + ((i_194_) << 2));
                     cache_dispatch_(gl2153_,1675,gl2154_,INTVAL_(gl2155_));
                     IATT_(gl2153_,INTVAL_(gl2155_)) = (int)5;
                  }
                  else {
                  }
               }
               i_194_ = (int)(i_194_ + 1);
            }
         goto_tag_2186_: ;
            gl2159_ = PATT_(self__,36);
            cache_dispatch_(gl2159_,661,gl2160_,INTVAL_(gl2161_));
            gl2156_ = PFN_(gl2161_)(gl2159_);
            cache_dispatch_(gl2156_,1705,gl2157_,INTVAL_(gl2158_));
            PATT_(self__,8) = (ptr)PFN_(gl2158_)(gl2156_);
            break;
         default:
            gl2162_ = PATT_(self__,24);
            gl200_ = TYPE_(gl2162_);
            gl2168_ = PATT_(self__,24);
            cache_dispatch_(gl2168_,1577,gl2169_,INTVAL_(gl2170_));
            gl2165_ = PATT_(gl2168_,INTVAL_(gl2170_));
            cache_dispatch_(gl2165_,1547,gl2166_,INTVAL_(gl2167_));
            gl201_ = IFN_(gl2167_)(gl2165_);
            if (((gl200_ == 107) & (gl201_ == RES97_REAL_ici_))) {
               gl2171_ = PATT_(self__,24);
               cache_dispatch_(gl2171_,1577,gl2172_,INTVAL_(gl2173_));
               PATT_(gl2171_,INTVAL_(gl2173_)) = (ptr)GLO68_double_typeob_s_;
               EXP113_semant_(self__,PATT_(INS150_inst_cls_(GLO68_double_typeob_s_),48));
            }
            else {
            }
            if ((PATT_(self__,36) == 0)) {
               ERR96_format_error_exit_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1735_)));
            }
            else {
            }
            ;
      }
      gl2174_ = PATT_(self__,36);
      gl202_ = TYPE_(gl2174_);
      if ((gl202_ != 153)) {
         co__ = (ptr)PATT_(symtab__,4);
         gl2177_ = obj_res_type__;
         cache_dispatch_(gl2177_,418,gl2178_,INTVAL_(gl2179_));
         cls_ind__ = (int)IATT_(PFN_(gl2179_)(gl2177_),20);
         if ((cls_ind__ == RES97_C_ici_)) {
            if (GLO68_pre_semant_) {
               GLO68_other_cnames_ = (ptr)INT164_insert_(GLO68_other_cnames_,IATT_(self__,28));
            }
            else {
            }
            PATT_(co__,112) = (ptr)INT164_insert_(PATT_(co__,112),IATT_(self__,28));
         }
         else {
            if ((cls_ind__ != IATT_(PATT_(symtab__,4),20))) {
               k__ = (int)GLO94_key_of_class_feat_(0,cls_ind__,IATT_(self__,28));
               PATT_(co__,116) = (ptr)INT164_insert_(PATT_(co__,116),k__);
            }
            else {
            }
         }
      }
      else {
      }
   }

   ret0__:
   return;
}

ptr EXP113_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,8);

   ret0__:
   return (res__);
}

int EXP113_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

EXP113_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr EXP113_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

EXP113_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{
   SATHER_STR_(20,10,ls1853_,",INTVAL_(");
   SATHER_STR_(20,2,ls1278_,")");
   SATHER_STR_(20,4,ls1854_,")+4");
   SATHER_STR_(20,4,ls1855_,")+8");
   SATHER_STR_(20,5,ls1856_,")+12");
   SATHER_STR_(20,3,ls1997_,"1,");
   SATHER_STR_(20,4,ls1999_,",3,");
   SATHER_STR_(20,4,ls2000_,",5,");
   SATHER_STR_(20,6,ls2001_,",3,3,");
   SATHER_STR_(20,6,ls2002_,",4,4,");
   SATHER_STR_(20,8,ls2003_,",2,2,2,");
   SATHER_STR_(20,8,ls2004_,",3,3,3,");
   SATHER_STR_(20,10,ls2005_,",2,2,2,2,");
   SATHER_STR_(20,10,ls2006_,",3,3,3,3,");
   SATHER_STR_(20,19,ls1667_,"EXPR_ARGS_EXPROB_S");
   SATHER_STR_(20,23,ls1565_,"Unknown continuation #");
   ptr gl2187_;
   static int gl2188_;
   static union dtype_ gl2189_;
   ptr gl2190_;
   static int gl2191_;
   static union dtype_ gl2192_;
   ptr gl2193_;
   static int gl2194_;
   static union dtype_ gl2195_;
   ptr gl2196_;
   static int gl2197_;
   static union dtype_ gl2198_;
   ptr gl2199_;
   static int gl2200_;
   static union dtype_ gl2201_;
   ptr gl2202_;
   static int gl2203_;
   static union dtype_ gl2204_;
   ptr gl2205_;
   static int gl2206_;
   static union dtype_ gl2207_;
   ptr gl2208_;
   static int gl2209_;
   static union dtype_ gl2210_;
   ptr gl2211_;
   static int gl2212_;
   static union dtype_ gl2213_;
   ptr gl2214_;
   static int gl2215_;
   static union dtype_ gl2216_;
   ptr gl2217_;
   static int gl2218_;
   static union dtype_ gl2219_;
   ptr gl2220_;
   static int gl2221_;
   static union dtype_ gl2222_;
   ptr gl2223_;
   static int gl2224_;
   static union dtype_ gl2225_;
   ptr gl2226_;
   static int gl2227_;
   static union dtype_ gl2228_;
   ptr gl2229_;
   static int gl2230_;
   static union dtype_ gl2231_;
   ptr gl2232_;
   static int gl2233_;
   static union dtype_ gl2234_;
   ptr gl2235_;
   static int gl2236_;
   static union dtype_ gl2237_;
   ptr gl2238_;
   static int gl2239_;
   static union dtype_ gl2240_;
   ptr gl2241_;
   static int gl2242_;
   static union dtype_ gl2243_;
   ptr gl2244_;
   static int gl2245_;
   static union dtype_ gl2246_;
   ptr gl2247_;
   static int gl2248_;
   static union dtype_ gl2249_;
   ptr gl2250_;
   static int gl2251_;
   static union dtype_ gl2252_;
   ptr gl2253_;
   static int gl2254_;
   static union dtype_ gl2255_;
   ptr gl2256_;
   static int gl2257_;
   static union dtype_ gl2258_;
   ptr gl2259_;
   static int gl2260_;
   static union dtype_ gl2261_;
   ptr gl2262_;
   static int gl2263_;
   static union dtype_ gl2264_;
   ptr gl2265_;
   static int gl2266_;
   static union dtype_ gl2267_;
   ptr gl2268_;
   static int gl2269_;
   static union dtype_ gl2270_;
   ptr gl2271_;
   static int gl2272_;
   static union dtype_ gl2273_;
   ptr gl2274_;
   static int gl2275_;
   static union dtype_ gl2276_;
   ptr gl2277_;
   static int gl2278_;
   static union dtype_ gl2279_;
   ptr gl2280_;
   static int gl2281_;
   static union dtype_ gl2282_;
   ptr gl2283_;
   static int gl2284_;
   static union dtype_ gl2285_;
   ptr gl2286_;
   static int gl2287_;
   static union dtype_ gl2288_;
   ptr gl2289_;
   static int gl2290_;
   static union dtype_ gl2291_;
   ptr gl2292_;
   static int gl2293_;
   static union dtype_ gl2294_;
   ptr gl2295_;
   static int gl2296_;
   static union dtype_ gl2297_;
   ptr gl2298_;
   static int gl2299_;
   static union dtype_ gl2300_;
   int    num_args__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    num_args_203_ = S_int_VOID_;
   int    num_args_204_ = S_int_VOID_;
   int    num_args_205_ = S_int_VOID_;
   int    num_args_206_ = S_int_VOID_;
   int    num_args_207_ = S_int_VOID_;
   int    num_args_208_ = S_int_VOID_;
   int    num_args_209_ = S_int_VOID_;
   int    num_args_210_ = S_int_VOID_;

   switch (cont__) {
      case (1) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
         break;
      case (2) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1854_));
         break;
      case (3) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1855_));
         break;
      case (4) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1856_));
         break;
      case (5) :
         if (GLO94_check_is_on_(0)) {
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         }
         else {
            gl2187_ = PATT_(self__,24);
            cache_dispatch_(gl2187_,965,gl2188_,INTVAL_(gl2189_));
            VFN_(gl2189_)(gl2187_,outfile__);
         }
         gl2193_ = PATT_(self__,24);
         cache_dispatch_(gl2193_,1577,gl2194_,INTVAL_(gl2195_));
         gl2190_ = PATT_(gl2193_,INTVAL_(gl2195_));
         cache_dispatch_(gl2190_,418,gl2191_,INTVAL_(gl2192_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl2192_)(gl2190_),108));
         break;
      case (6) :
         if (GLO94_check_is_on_(0)) {
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         }
         else {
            gl2196_ = PATT_(self__,24);
            cache_dispatch_(gl2196_,965,gl2197_,INTVAL_(gl2198_));
            VFN_(gl2198_)(gl2196_,outfile__);
         }
         gl2202_ = PATT_(self__,24);
         cache_dispatch_(gl2202_,1577,gl2203_,INTVAL_(gl2204_));
         gl2199_ = PATT_(gl2202_,INTVAL_(gl2204_));
         cache_dispatch_(gl2199_,418,gl2200_,INTVAL_(gl2201_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(PFN_(gl2201_)(gl2199_),108) + 4));
         break;
      case (7) :
         if (GLO94_check_is_on_(0)) {
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         }
         else {
            gl2205_ = PATT_(self__,24);
            cache_dispatch_(gl2205_,965,gl2206_,INTVAL_(gl2207_));
            VFN_(gl2207_)(gl2205_,outfile__);
         }
         gl2211_ = PATT_(self__,24);
         cache_dispatch_(gl2211_,1577,gl2212_,INTVAL_(gl2213_));
         gl2208_ = PATT_(gl2211_,INTVAL_(gl2213_));
         cache_dispatch_(gl2208_,418,gl2209_,INTVAL_(gl2210_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(PFN_(gl2210_)(gl2208_),108) + 8));
         break;
      case (8) :
         if (GLO94_check_is_on_(0)) {
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         }
         else {
            gl2214_ = PATT_(self__,24);
            cache_dispatch_(gl2214_,965,gl2215_,INTVAL_(gl2216_));
            VFN_(gl2216_)(gl2214_,outfile__);
         }
         gl2220_ = PATT_(self__,24);
         cache_dispatch_(gl2220_,1577,gl2221_,INTVAL_(gl2222_));
         gl2217_ = PATT_(gl2220_,INTVAL_(gl2222_));
         cache_dispatch_(gl2217_,418,gl2218_,INTVAL_(gl2219_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(PFN_(gl2219_)(gl2217_),108) + 12));
         break;
      case (9) :
         if (GLO94_check_is_on_(0)) {
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         }
         else {
            gl2223_ = PATT_(self__,24);
            cache_dispatch_(gl2223_,965,gl2224_,INTVAL_(gl2225_));
            VFN_(gl2225_)(gl2223_,outfile__);
         }
         (void)SAT99_c_(outfile__,',');
         gl2226_ = PATT_(self__,36);
         cache_dispatch_(gl2226_,662,gl2227_,INTVAL_(gl2228_));
         VFN_(gl2228_)(gl2226_,outfile__);
         break;
      case (10) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         break;
      case (11) :
         gl2229_ = PATT_(self__,24);
         cache_dispatch_(gl2229_,965,gl2230_,INTVAL_(gl2231_));
         VFN_(gl2231_)(gl2229_,outfile__);
         break;
      case (12) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(self__,28)),',');
         GLO94_cprint_ctemp_name_(0,IATT_(self__,48),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_c_(outfile__,')');
         break;
      case (13) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_c_(outfile__,',');
         GLO94_cprint_ctemp_name_(0,IATT_(self__,48),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_c_(outfile__,')');
         break;
      case (14) :
         PRI187_cprint_type_access_(0,outfile__,10,self__);
         num_args__ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args__ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         (void)SAT99_c_(outfile__,',');
         i__ = (int)0;
         while (1) {
            if ((i__ >= 4)) {
               goto goto_tag_2301_;
            }
            else {
            }
            if ((i__ >= num_args__)) {
               (void)SAT99_s_(outfile__,(ptr)(&ls1997_));
            }
            else {
               EXP113_cprint_ith_arg_act_code_(self__,i__,outfile__);
               (void)SAT99_c_(outfile__,',');
            }
            i__ = (int)(i__ + 1);
         }
      goto_tag_2301_: ;
         PRI187_cprint_atomic_check_(0,outfile__,10,self__);
         (void)SAT99_i_(SAT99_c_(outfile__,','),num_args__);
         break;
      case (15) :
         gl2235_ = PATT_(self__,24);
         cache_dispatch_(gl2235_,1577,gl2236_,INTVAL_(gl2237_));
         gl2232_ = PATT_(gl2235_,INTVAL_(gl2237_));
         cache_dispatch_(gl2232_,418,gl2233_,INTVAL_(gl2234_));
         gl2241_ = PATT_(self__,24);
         cache_dispatch_(gl2241_,1577,gl2242_,INTVAL_(gl2243_));
         gl2238_ = PATT_(gl2241_,INTVAL_(gl2243_));
         cache_dispatch_(gl2238_,418,gl2239_,INTVAL_(gl2240_));
         (void)SAT99_i_(SAT99_c_(SAT99_i_(outfile__,IATT_(PFN_(gl2240_)(gl2238_),20)),','),IATT_(PFN_(gl2234_)(gl2232_),60));
         break;
      case (16) :
         num_args_203_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_203_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         gl2247_ = PATT_(self__,24);
         cache_dispatch_(gl2247_,1577,gl2248_,INTVAL_(gl2249_));
         gl2244_ = PATT_(gl2247_,INTVAL_(gl2249_));
         cache_dispatch_(gl2244_,418,gl2245_,INTVAL_(gl2246_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2246_)(gl2244_),20));
         if ((num_args_203_ != 1)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls1999_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2253_ = PATT_(self__,24);
         cache_dispatch_(gl2253_,1577,gl2254_,INTVAL_(gl2255_));
         gl2250_ = PATT_(gl2253_,INTVAL_(gl2255_));
         cache_dispatch_(gl2250_,418,gl2251_,INTVAL_(gl2252_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2252_)(gl2250_),60));
         break;
      case (17) :
         num_args_204_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_204_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         PRI187_cprint_type_access_(0,outfile__,10,self__);
         if ((num_args_204_ != 1)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2000_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         PRI187_cprint_atomic_check_(0,outfile__,10,self__);
         break;
      case (18) :
         num_args_205_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_205_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         gl2259_ = PATT_(self__,24);
         cache_dispatch_(gl2259_,1577,gl2260_,INTVAL_(gl2261_));
         gl2256_ = PATT_(gl2259_,INTVAL_(gl2261_));
         cache_dispatch_(gl2256_,418,gl2257_,INTVAL_(gl2258_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2258_)(gl2256_),20));
         if ((num_args_205_ != 2)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2001_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2265_ = PATT_(self__,24);
         cache_dispatch_(gl2265_,1577,gl2266_,INTVAL_(gl2267_));
         gl2262_ = PATT_(gl2265_,INTVAL_(gl2267_));
         cache_dispatch_(gl2262_,418,gl2263_,INTVAL_(gl2264_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2264_)(gl2262_),60));
         break;
      case (19) :
         num_args_206_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_206_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         PRI187_cprint_type_access_(0,outfile__,10,self__);
         if ((num_args_206_ != 2)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2002_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         PRI187_cprint_atomic_check_(0,outfile__,10,self__);
         break;
      case (20) :
         num_args_207_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_207_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         gl2271_ = PATT_(self__,24);
         cache_dispatch_(gl2271_,1577,gl2272_,INTVAL_(gl2273_));
         gl2268_ = PATT_(gl2271_,INTVAL_(gl2273_));
         cache_dispatch_(gl2268_,418,gl2269_,INTVAL_(gl2270_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2270_)(gl2268_),20));
         if ((num_args_207_ != 3)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2003_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2277_ = PATT_(self__,24);
         cache_dispatch_(gl2277_,1577,gl2278_,INTVAL_(gl2279_));
         gl2274_ = PATT_(gl2277_,INTVAL_(gl2279_));
         cache_dispatch_(gl2274_,418,gl2275_,INTVAL_(gl2276_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2276_)(gl2274_),60));
         break;
      case (21) :
         num_args_208_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_208_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         PRI187_cprint_type_access_(0,outfile__,10,self__);
         if ((num_args_208_ != 3)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2004_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         PRI187_cprint_atomic_check_(0,outfile__,1,self__);
         break;
      case (22) :
         num_args_209_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_209_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         gl2283_ = PATT_(self__,24);
         cache_dispatch_(gl2283_,1577,gl2284_,INTVAL_(gl2285_));
         gl2280_ = PATT_(gl2283_,INTVAL_(gl2285_));
         cache_dispatch_(gl2280_,418,gl2281_,INTVAL_(gl2282_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2282_)(gl2280_),20));
         if ((num_args_209_ != 4)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2005_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,3,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         gl2289_ = PATT_(self__,24);
         cache_dispatch_(gl2289_,1577,gl2290_,INTVAL_(gl2291_));
         gl2286_ = PATT_(gl2289_,INTVAL_(gl2291_));
         cache_dispatch_(gl2286_,418,gl2287_,INTVAL_(gl2288_));
         (void)SAT99_i_(outfile__,IATT_(PFN_(gl2288_)(gl2286_),60));
         break;
      case (23) :
         num_args_210_ = S_int_VOID_;
         if ((PATT_(self__,32) != 0)) {
            num_args_210_ = (int)IATT_(PATT_(self__,32),12);
         }
         else {
         }
         PRI187_cprint_type_access_(0,outfile__,10,self__);
         if ((num_args_210_ != 4)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2006_));
         }
         else {
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
            (void)SAT99_c_(outfile__,',');
            EXP113_cprint_ith_arg_act_code_(self__,3,outfile__);
            (void)SAT99_c_(outfile__,',');
         }
         PRI187_cprint_atomic_check_(0,outfile__,10,self__);
         break;
      case (24) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_c_(outfile__,',');
         PRI187_cprint_atomic_check_(0,outfile__,10,self__);
         break;
      case (25) :
         gl2292_ = PATT_(self__,24);
         cache_dispatch_(gl2292_,965,gl2293_,INTVAL_(gl2294_));
         VFN_(gl2294_)(gl2292_,outfile__);
         gl2298_ = PATT_(self__,24);
         cache_dispatch_(gl2298_,1577,gl2299_,INTVAL_(gl2300_));
         gl2295_ = PATT_(gl2298_,INTVAL_(gl2300_));
         cache_dispatch_(gl2295_,418,gl2296_,INTVAL_(gl2297_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl2297_)(gl2295_),60));
         break;
      default:
         ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1565_)),cont__));
         ;
   }

   ret0__:
   return;
}

EXP113_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

EXP113_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

EXP113_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

EXP113_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl2302_;
   static int gl2303_;
   static union dtype_ gl2304_;
   ptr gl2305_;
   static int gl2306_;
   static union dtype_ gl2307_;

   gl2302_ = PATT_(self__,24);
   cache_dispatch_(gl2302_,724,gl2303_,INTVAL_(gl2304_));
   VFN_(gl2304_)(gl2302_,outfile__);
   LST120_cprint_init_code_(PATT_(self__,32),outfile__);
   if ((PATT_(self__,36) != 0)) {
      gl2305_ = PATT_(self__,36);
      cache_dispatch_(gl2305_,724,gl2306_,INTVAL_(gl2307_));
      VFN_(gl2307_)(gl2305_,outfile__);
   }
   else {
   }

   ret0__:
   return;
}

char EXP113_valid_init_expr_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl2308_;
   static int gl2309_;
   static union dtype_ gl2310_;
   ptr gl2311_;
   static int gl2312_;
   static union dtype_ gl2313_;
   int gl211_;
   ptr gl2314_;
   static int gl2315_;
   static union dtype_ gl2316_;
   int gl212_;
   ptr gl2317_;
   static int gl2318_;
   static union dtype_ gl2319_;
   int gl213_;

   gl2308_ = PATT_(self__,24);
   cache_dispatch_(gl2308_,1580,gl2309_,INTVAL_(gl2310_));
   res__ = (char)CFN_(gl2310_)(gl2308_);
   if (res__) {
      if ((PATT_(self__,36) != 0)) {
         gl2311_ = PATT_(self__,36);
         gl211_ = TYPE_(gl2311_);
         gl2314_ = PATT_(self__,36);
         gl212_ = TYPE_(gl2314_);
         gl2317_ = PATT_(self__,36);
         gl213_ = TYPE_(gl2317_);
         if ((((gl211_ == 151) | (gl212_ == 152)) | (gl213_ == 156))) {
            if ((PATT_(self__,32) != 0)) {
               res__ = (char)LST120_valid_init_expr_(PATT_(self__,32));
            }
            else {
            }
         }
         else {
            res__ = (char)0;
         }
      }
      else {
         switch (IATT_(self__,28)) {
            case (28) :
            case (32) :
            case (30) :
            case (31) :
            case (23) :
            case (24) :
            case (25) :
            case (26) :
            case (27) :
               res__ = (char)LST120_valid_init_expr_(PATT_(self__,32));
               break;
            default:
               ;
         }
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

char EXP113_assignable_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,19,ls1667_,"EXPR_ARGS_EXPROB_S");
   SATHER_STR_(20,30,ls1980_,"Unrecognized referrent type \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr gl2320_;
   static int gl2321_;
   static union dtype_ gl2322_;
   int gl214_;
   ptr gl2323_;
   static int gl2324_;
   static union dtype_ gl2325_;
   int gl215_;
   ptr gl2326_;
   static int gl2327_;
   static union dtype_ gl2328_;
   int gl216_;
   ptr gl2329_;
   static int gl2330_;
   static union dtype_ gl2331_;
   int gl217_;
   ptr gl2332_;
   static int gl2333_;
   static union dtype_ gl2334_;
   int gl218_;
   ptr gl2335_;
   static int gl2336_;
   static union dtype_ gl2337_;
   int gl219_;

   if ((PATT_(self__,36) != 0)) {
      gl2320_ = PATT_(self__,36);
      gl214_ = TYPE_(gl2320_);
      gl2323_ = PATT_(self__,36);
      gl215_ = TYPE_(gl2323_);
      gl2326_ = PATT_(self__,36);
      gl216_ = TYPE_(gl2326_);
      gl2329_ = PATT_(self__,36);
      gl217_ = TYPE_(gl2329_);
      if (((((gl214_ == 154) | (gl215_ == 145)) | (gl216_ == 156)) | (gl217_ == 151))) {
         res__ = (char)0;
      }
      else {
         gl2332_ = PATT_(self__,36);
         gl218_ = TYPE_(gl2332_);
         gl2335_ = PATT_(self__,36);
         gl219_ = TYPE_(gl2335_);
         if (((gl218_ == 153) | (gl219_ == 152))) {
            res__ = (char)1;
         }
         else {
            ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1980_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls785_)));
         }
      }
   }
   else {
      res__ = (char)0;
   }

   ret0__:
   return (res__);
}

ptr EXP113_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl2338_;
   static int gl2339_;
   static union dtype_ gl2340_;
   ptr gl2341_;
   static int gl2342_;
   static union dtype_ gl2343_;
   ptr gl2344_;
   static int gl2345_;
   static union dtype_ gl2346_;
   ptr gl2347_;
   static int gl2348_;
   static union dtype_ gl2349_;
   ptr gl2350_;
   static int gl2351_;
   static union dtype_ gl2352_;
   ptr gl2353_;
   static int gl2354_;
   static union dtype_ gl2355_;
   ptr gl2356_;
   static int gl2357_;
   static union dtype_ gl2358_;
   ptr gl2359_;
   static int gl2360_;
   static union dtype_ gl2361_;
   ptr gl220_;
   ptr gl2362_;
   static int gl2363_;
   static union dtype_ gl2364_;
   ptr gl2365_;
   static int gl2366_;
   static union dtype_ gl2367_;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   if ((IATT_(self__,40) < 0)) {
   }
   else {
      if ((IATT_(self__,40) == 0)) {
         if (EXP113_requires_dispatched_call_(self__)) {
            IATT_(self__,40) = (int)GLO94_global_key_(0);
            gl2341_ = PATT_(self__,24);
            cache_dispatch_(gl2341_,1577,gl2342_,INTVAL_(gl2343_));
            gl2338_ = PATT_(gl2341_,INTVAL_(gl2343_));
            cache_dispatch_(gl2338_,374,gl2339_,INTVAL_(gl2340_));
            IATT_(self__,44) = (int)IFN_(gl2340_)(gl2338_);
            IATT_(self__,48) = (int)GLO94_global_key_(0);
            IATT_(self__,52) = (int)3;
            IATT_(self__,56) = (int)GLO94_global_key_(0);
            if ((PATT_(self__,8) != 0)) {
               gl2344_ = PATT_(self__,8);
               cache_dispatch_(gl2344_,374,gl2345_,INTVAL_(gl2346_));
               IATT_(self__,60) = (int)IFN_(gl2346_)(gl2344_);
            }
            else {
               IATT_(self__,60) = (int)6;
            }
            res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_push_(LIS98_push_(LIS98_push_(LIS98_push_(LIS98_create_(0,6),IATT_(self__,40)),IATT_(self__,44)),(- IATT_(self__,48))),IATT_(self__,52)),(- IATT_(self__,56))),IATT_(self__,60));
         }
         else {
            IATT_(self__,40) = (int)(- 1);
         }
      }
      else {
         res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,6),IATT_(self__,40)),IATT_(self__,44));
         if ((IATT_(self__,48) > 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,(- IATT_(self__,48))),IATT_(self__,52));
         }
         else {
         }
         if ((IATT_(self__,56) > 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,(- IATT_(self__,56))),IATT_(self__,60));
         }
         else {
         }
      }
   }
   if (((PATT_(self__,36) == 0) & ((IATT_(self__,28) == 31) | (IATT_(self__,28) == 32)))) {
      if ((IATT_(self__,40) == (- 1))) {
         IATT_(self__,40) = (int)GLO94_global_key_(0);
         gl2350_ = PATT_(self__,24);
         cache_dispatch_(gl2350_,1577,gl2351_,INTVAL_(gl2352_));
         gl2347_ = PATT_(gl2350_,INTVAL_(gl2352_));
         cache_dispatch_(gl2347_,374,gl2348_,INTVAL_(gl2349_));
         IATT_(self__,44) = (int)IFN_(gl2349_)(gl2347_);
         res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,40)),IATT_(self__,44));
      }
      else {
      }
   }
   else {
   }
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,40) == (- 1))) {
         IATT_(self__,40) = (int)GLO94_global_key_(0);
         gl2356_ = PATT_(self__,24);
         cache_dispatch_(gl2356_,1577,gl2357_,INTVAL_(gl2358_));
         gl2353_ = PATT_(gl2356_,INTVAL_(gl2358_));
         cache_dispatch_(gl2353_,374,gl2354_,INTVAL_(gl2355_));
         IATT_(self__,44) = (int)IFN_(gl2355_)(gl2353_);
         res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,40)),IATT_(self__,44));
      }
      else {
      }
   }
   else {
   }
   if ((res__ != 0)) {
      res__ = (ptr)LIS98_append_(res__,LST120_gen_temps_(PATT_(self__,32)));
   }
   else {
      res__ = (ptr)LST120_gen_temps_(PATT_(self__,32));
   }
   if ((res__ != 0)) {
      gl2359_ = PATT_(self__,24);
      cache_dispatch_(gl2359_,1582,gl2360_,INTVAL_(gl2361_));
      gl220_ = PFN_(gl2361_)(gl2359_);
      res__ = (ptr)LIS98_append_(res__,gl220_);
   }
   else {
      gl2362_ = PATT_(self__,24);
      cache_dispatch_(gl2362_,1582,gl2363_,INTVAL_(gl2364_));
      res__ = (ptr)PFN_(gl2364_)(gl2362_);
   }
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,20) != 0)) {
         IATT_(self__,64) = (int)GLO94_global_key_(0);
         gl2365_ = PATT_(self__,8);
         cache_dispatch_(gl2365_,374,gl2366_,INTVAL_(gl2367_));
         IATT_(self__,68) = (int)IFN_(gl2367_)(gl2365_);
         if ((res__ != 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,64)),IATT_(self__,68));
         }
         else {
            res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,64)),IATT_(self__,68));
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
            goto goto_tag_2368_;
         }
         else {
         }
         if ((IATT_(PATT_(self__,72), 8 + ((i__) << 2)) > 0)) {
            if ((res__ != 0)) {
               res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(PATT_(self__,72), 8 + ((i__) << 2))),IATT_(PATT_(self__,76), 8 + ((i__) << 2)));
            }
            else {
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(PATT_(self__,72), 8 + ((i__) << 2))),IATT_(PATT_(self__,76), 8 + ((i__) << 2)));
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_2368_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr EXP113_expr_eval_constant_(self__)
ptr self__;
{
   ptr res__ = 0;
   SATHER_STR_(20,24,ls1906_,"(EXPR_ARGS_EXPROB_S): \"");
   SATHER_STR_(20,58,ls1907_,"\" is an unknown identifier in constant feature definition");
   ptr gl2369_;
   static int gl2370_;
   static union dtype_ gl2371_;
   ptr gl2372_;
   static int gl2373_;
   static union dtype_ gl2374_;
   ptr gl2375_;
   static int gl2376_;
   static union dtype_ gl2377_;
   int gl221_;
   ptr gl2378_;
   static int gl2379_;
   static union dtype_ gl2380_;

   if ((PATT_(self__,36) == 0)) {
      switch (IATT_(self__,28)) {
         case (32) :
            gl2372_ = PATT_(self__,24);
            cache_dispatch_(gl2372_,1577,gl2373_,INTVAL_(gl2374_));
            gl2369_ = PATT_(gl2372_,INTVAL_(gl2374_));
            cache_dispatch_(gl2369_,418,gl2370_,INTVAL_(gl2371_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_(IATT_(PFN_(gl2371_)(gl2369_),20)),IATT_(self__,80));
            break;
         case (31) :
            res__ = (ptr)0;
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1906_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1907_)));
            ;
      }
      goto ret0__;
   }
   else {
   }
   gl2375_ = PATT_(self__,36);
   gl221_ = TYPE_(gl2375_);
   if ((gl221_ == 151)) {
      gl2378_ = PATT_(self__,36);
      cache_dispatch_(gl2378_,804,gl2379_,INTVAL_(gl2380_));
      res__ = (ptr)PFN_(gl2380_)(gl2378_);
   }
   else {
   }

   ret0__:
   return (res__);
}

EXP113_get_ext_strs_(self__)
ptr self__;
{
   ptr gl2381_;
   static int gl2382_;
   static union dtype_ gl2383_;
   ptr gl2384_;
   static int gl2385_;
   static union dtype_ gl2386_;
   ptr gl2387_;
   static int gl2388_;
   static union dtype_ gl2389_;
   int gl222_;
   ptr    co__ = 0;
   int    cls_ind__ = S_int_VOID_;
   ptr    const_feat__ = 0;
   int    temp__ = S_int_VOID_;

   gl2381_ = PATT_(self__,24);
   cache_dispatch_(gl2381_,1677,gl2382_,INTVAL_(gl2383_));
   VFN_(gl2383_)(gl2381_);
   LST120_get_ext_strs_(PATT_(self__,32));
   if ((PATT_(self__,8) != 0)) {
      gl2384_ = PATT_(self__,8);
      cache_dispatch_(gl2384_,418,gl2385_,INTVAL_(gl2386_));
      co__ = (ptr)PFN_(gl2386_)(gl2384_);
      cls_ind__ = (int)IATT_(co__,20);
      if ((cls_ind__ == RES97_C_ici_)) {
      }
      else {
         if ((cls_ind__ != IATT_(GLO68_curr_class_inst_,20))) {
            if ((PATT_(self__,36) != 0)) {
               gl2387_ = PATT_(self__,36);
               gl222_ = TYPE_(gl2387_);
               if ((gl222_ == 151)) {
                  const_feat__ = (ptr)PATT_(self__,36);
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
   }
   else {
   }

   ret0__:
   return;
}

char EXP113_to_be_pre_evaluated_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)EXP113_requires_dispatched_call_(self__);

   ret0__:
   return (res__);
}

char EXP113_access_value_only_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl2390_;
   static int gl2391_;
   static union dtype_ gl2392_;
   int gl223_;

   if ((PATT_(self__,36) != 0)) {
      gl2390_ = PATT_(self__,36);
      gl223_ = TYPE_(gl2390_);
      res__ = (char)(gl223_ != 156);
   }
   else {
   }

   ret0__:
   return (res__);
}

EXP113_fob_error_(self__,op_name__,cls_name__)
ptr self__;
ptr op_name__;
ptr cls_name__;
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

EXP113_cprint_pre_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl2393_;
   static int gl2394_;
   static union dtype_ gl2395_;
   ptr gl2396_;
   static int gl2397_;
   static union dtype_ gl2398_;
   ptr gl2399_;
   static int gl2400_;
   static union dtype_ gl2401_;
   ptr gl2402_;
   static int gl2403_;
   static union dtype_ gl2404_;
   ptr gl2405_;
   static int gl2406_;
   static union dtype_ gl2407_;
   int gl224_;
   ptr gl2408_;
   static int gl2409_;
   static union dtype_ gl2410_;
   int    num_args__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   num_args__ = (int)0;
   if ((PATT_(self__,32) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= num_args__)) {
         goto goto_tag_2411_;
      }
      else {
      }
      EXP113_cprint_ith_arg_pre_code_(self__,i__,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_2411_: ;
   gl2393_ = PATT_(self__,24);
   cache_dispatch_(gl2393_,1589,gl2394_,INTVAL_(gl2395_));
   VFN_(gl2395_)(gl2393_,outfile__);
   if (EXP113_requires_dispatched_call_(self__)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl2396_ = PATT_(self__,24);
      cache_dispatch_(gl2396_,965,gl2397_,INTVAL_(gl2398_));
      VFN_(gl2398_)(gl2396_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      if (GLO94_check_is_on_(0)) {
         if ((PATT_(self__,36) == 0)) {
            switch (IATT_(self__,28)) {
               case (30) :
               case (28) :
                  break;
               case (31) :
               case (23) :
               case (24) :
               case (25) :
               case (26) :
               case (27) :
               case (32) :
                  PRI187_cprint_void_tst_(0,outfile__,10,self__);
                  break;
               default:
                  ;
                  ;
            }
         }
         else {
            PRI187_cprint_void_tst_(0,outfile__,10,self__);
         }
      }
      else {
      }
      if ((PATT_(self__,36) == 0)) {
         switch (IATT_(self__,28)) {
            case (23) :
            case (24) :
            case (25) :
            case (26) :
            case (27) :
               PRI187_cprint_array_dispatch_(0,outfile__,13,self__);
               break;
            case (32) :
            case (31) :
               break;
            default:
               (void)EXP113_handle_spec_expr_pre_code_(self__,outfile__);
               ;
         }
         goto ret0__;
      }
      else {
         PRI187_cprint_cache_dispatch_(0,outfile__,12,self__);
      }
   }
   else {
      if (((PATT_(self__,36) == 0) & ((IATT_(self__,28) == 31) | (IATT_(self__,28) == 32)))) {
         (void)SAT99_indent_(outfile__);
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls964_));
         gl2399_ = PATT_(self__,24);
         cache_dispatch_(gl2399_,965,gl2400_,INTVAL_(gl2401_));
         VFN_(gl2401_)(gl2399_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
         goto ret0__;
      }
      else {
      }
      if (GLO94_check_is_on_(0)) {
         if ((PATT_(self__,36) == 0)) {
            switch (IATT_(self__,28)) {
               case (30) :
               case (28) :
               case (31) :
                  break;
               case (23) :
               case (24) :
               case (25) :
               case (26) :
               case (27) :
               case (32) :
                  (void)SAT99_indent_(outfile__);
                  GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                  (void)SAT99_s_(outfile__,(ptr)(&ls964_));
                  gl2402_ = PATT_(self__,24);
                  cache_dispatch_(gl2402_,965,gl2403_,INTVAL_(gl2404_));
                  VFN_(gl2404_)(gl2402_,outfile__);
                  (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
                  PRI187_cprint_void_tst_(0,outfile__,10,self__);
                  break;
               default:
                  ;
                  ;
            }
         }
         else {
            gl2405_ = PATT_(self__,36);
            gl224_ = TYPE_(gl2405_);
            if ((gl224_ == 153)) {
               (void)SAT99_indent_(outfile__);
               GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
               (void)SAT99_s_(outfile__,(ptr)(&ls964_));
               gl2408_ = PATT_(self__,24);
               cache_dispatch_(gl2408_,965,gl2409_,INTVAL_(gl2410_));
               VFN_(gl2410_)(gl2408_,outfile__);
               (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
               PRI187_cprint_void_tst_(0,outfile__,10,self__);
            }
            else {
            }
         }
      }
      else {
      }
   }
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,20) != 0)) {
         GLO94_cprint_curr_exp_code_(0,self__,IATT_(self__,64),outfile__);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

EXP113_cprint_act_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,19,ls1667_,"EXPR_ARGS_EXPROB_S");
   SATHER_STR_(20,26,ls1727_,"Invalid printing count = ");
   SATHER_STR_(20,4,ls1728_," > ");
   SATHER_STR_(20,58,ls2007_,"(EXPR_ARGS_EXPROB_S): Applying \"new\" to dispatched object");
   SATHER_STR_(20,59,ls2008_,"(EXPR_ARGS_EXPROB_S): Applying \"new\" on a non-object class");
   SATHER_STR_(20,130,ls2009_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on 1-dimensional array object; default sizes used");
   SATHER_STR_(20,131,ls2010_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on 2-dimensional array object; default sizes used\n");
   SATHER_STR_(20,131,ls2011_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on 3-dimensional array object; default sizes used\n");
   SATHER_STR_(20,130,ls2012_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on 4-dimensional array object; default sizes used");
   SATHER_STR_(20,132,ls2013_,"(EXPR_ARGS_EXPROB_S):Incorrect number of arguments supplied for \"extend\" operator on 1-dimensional array object; default sizes used");
   SATHER_STR_(20,10,ls1897_,"extend1_(");
   SATHER_STR_(20,14,ls2014_,",5,atomic_p_(");
   SATHER_STR_(20,3,ls1858_,"))");
   SATHER_STR_(20,12,ls2015_,",atomic_p_(");
   SATHER_STR_(20,133,ls2016_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"extend\" operator on 2-dimensional array object; default sizes used");
   SATHER_STR_(20,10,ls1899_,"extend2_(");
   SATHER_STR_(20,16,ls2017_,",4,4,atomic_p_(");
   SATHER_STR_(20,133,ls2018_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"extend\" operator on 3-dimensional array object; default sizes used");
   SATHER_STR_(20,10,ls1900_,"extend3_(");
   SATHER_STR_(20,18,ls2019_,",3,3,3,atomic_p_(");
   SATHER_STR_(20,133,ls2020_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"extend\" operator on 4-dimensional array object; default sizes used");
   SATHER_STR_(20,10,ls1901_,"extend4_(");
   SATHER_STR_(20,20,ls2021_,",3,3,3,3,atomic_p_(");
   SATHER_STR_(20,54,ls2022_,"/* \"extend\" is not applicable for non-array object */");
   SATHER_STR_(20,4,ls1765_," * ");
   SATHER_STR_(20,36,ls2023_,"/* Invalid reference to \"asize1\" */");
   SATHER_STR_(20,36,ls2024_,"/* Invalid reference to \"asize2\" */");
   SATHER_STR_(20,36,ls2025_,"/* Invalid reference to \"asize3\" */");
   SATHER_STR_(20,36,ls2026_,"/* Invalid reference to \"asize4\" */");
   SATHER_STR_(20,32,ls2027_,"/* Invalid dot expression for \"");
   SATHER_STR_(20,5,ls2028_,"\" */");
   SATHER_STR_(20,78,ls2030_,"/* Cannot get type of an object dispatched on a descendent of a basic type */");
   SATHER_STR_(20,25,ls2031_,"/* Unknown identifer : \"");
   SATHER_STR_(20,9,ls2032_,"PTRPTR_(");
   SATHER_STR_(20,9,ls2033_,"INTPTR_(");
   SATHER_STR_(20,10,ls2034_,"CHARPTR_(");
   SATHER_STR_(20,11,ls2035_,"FLOATPTR_(");
   SATHER_STR_(20,12,ls2036_,"DOUBLEPTR_(");
   SATHER_STR_(20,6,ls2037_,"PFN_(");
   SATHER_STR_(20,6,ls2038_,"CFN_(");
   SATHER_STR_(20,6,ls2039_,"IFN_(");
   SATHER_STR_(20,6,ls2040_,"FFN_(");
   SATHER_STR_(20,6,ls2041_,"DFN_(");
   SATHER_STR_(20,6,ls2042_,"VFN_(");
   SATHER_STR_(20,3,ls1978_,")(");
   SATHER_STR_(20,26,ls2043_,"/* Invalid reference to \"");
   SATHER_STR_(20,119,ls2044_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on array object; default size provided");
   SATHER_STR_(20,130,ls2045_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on 2-dimensional array object; default sizes used");
   SATHER_STR_(20,130,ls2046_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"new\" operator on 3-dimensional array object; default sizes used");
   SATHER_STR_(20,133,ls2047_,"(EXPR_ARGS_EXPROB_S): Incorrect number of arguments supplied for \"extend\" operator on 1-dimensional array object; default sizes used");
   SATHER_STR_(20,4,ls2000_,",5,");
   SATHER_STR_(20,6,ls2002_,",4,4,");
   SATHER_STR_(20,8,ls2004_,",3,3,3,");
   SATHER_STR_(20,10,ls2006_,",3,3,3,3,");
   SATHER_STR_(20,32,ls2048_,"/* asize of non-array object */");
   SATHER_STR_(20,33,ls2049_,"/* asize1 of non-array object */");
   SATHER_STR_(20,42,ls2050_,"/* asize2 of 1-dim or non-array object */");
   SATHER_STR_(20,38,ls2051_,"/* asize3 of 2-or-fewer-dim object */");
   SATHER_STR_(20,38,ls2052_,"/* asize4 of 3-or-fewer-dim object */");
   SATHER_STR_(20,26,ls2053_,"/* Unknown identifier : \"");
   ptr gl2412_;
   static int gl2413_;
   static union dtype_ gl2414_;
   ptr gl2415_;
   static int gl2416_;
   static union dtype_ gl2417_;
   ptr gl2418_;
   static int gl2419_;
   static union dtype_ gl2420_;
   ptr gl2421_;
   static int gl2422_;
   static union dtype_ gl2423_;
   ptr gl2424_;
   static int gl2425_;
   static union dtype_ gl2426_;
   ptr gl2427_;
   static int gl2428_;
   static union dtype_ gl2429_;
   ptr gl2430_;
   static int gl2431_;
   static union dtype_ gl2432_;
   ptr gl2433_;
   static int gl2434_;
   static union dtype_ gl2435_;
   ptr gl2436_;
   static int gl2437_;
   static union dtype_ gl2438_;
   ptr gl2439_;
   static int gl2440_;
   static union dtype_ gl2441_;
   ptr gl2442_;
   static int gl2443_;
   static union dtype_ gl2444_;
   ptr gl2445_;
   static int gl2446_;
   static union dtype_ gl2447_;
   ptr gl2448_;
   static int gl2449_;
   static union dtype_ gl2450_;
   ptr gl2451_;
   static int gl2452_;
   static union dtype_ gl2453_;
   ptr gl2454_;
   static int gl2455_;
   static union dtype_ gl2456_;
   ptr gl2457_;
   static int gl2458_;
   static union dtype_ gl2459_;
   char gl225_;
   char gl226_;
   ptr gl2460_;
   static int gl2461_;
   static union dtype_ gl2462_;
   char gl227_;
   ptr gl2463_;
   static int gl2464_;
   static union dtype_ gl2465_;
   char gl228_;
   ptr gl2466_;
   static int gl2467_;
   static union dtype_ gl2468_;
   ptr gl2469_;
   static int gl2470_;
   static union dtype_ gl2471_;
   char gl229_;
   char gl230_;
   ptr gl2472_;
   static int gl2473_;
   static union dtype_ gl2474_;
   char gl231_;
   ptr gl2475_;
   static int gl2476_;
   static union dtype_ gl2477_;
   ptr gl2478_;
   static int gl2479_;
   static union dtype_ gl2480_;
   char gl232_;
   char gl233_;
   ptr gl2481_;
   static int gl2482_;
   static union dtype_ gl2483_;
   ptr gl2484_;
   static int gl2485_;
   static union dtype_ gl2486_;
   ptr gl2487_;
   static int gl2488_;
   static union dtype_ gl2489_;
   char gl234_;
   char gl235_;
   ptr gl2490_;
   static int gl2491_;
   static union dtype_ gl2492_;
   char gl236_;
   ptr gl2493_;
   static int gl2494_;
   static union dtype_ gl2495_;
   char gl237_;
   ptr gl2496_;
   static int gl2497_;
   static union dtype_ gl2498_;
   char gl238_;
   ptr gl2499_;
   static int gl2500_;
   static union dtype_ gl2501_;
   ptr gl2502_;
   static int gl2503_;
   static union dtype_ gl2504_;
   ptr gl2505_;
   static int gl2506_;
   static union dtype_ gl2507_;
   ptr gl2508_;
   static int gl2509_;
   static union dtype_ gl2510_;
   ptr gl2511_;
   static int gl2512_;
   static union dtype_ gl2513_;
   int gl239_;
   ptr gl2514_;
   static int gl2515_;
   static union dtype_ gl2516_;
   ptr gl2517_;
   static int gl2518_;
   static union dtype_ gl2519_;
   ptr gl2520_;
   static int gl2521_;
   static union dtype_ gl2522_;
   ptr gl2523_;
   static int gl2524_;
   static union dtype_ gl2525_;
   ptr gl2526_;
   static int gl2527_;
   static union dtype_ gl2528_;
   ptr gl2529_;
   static int gl2530_;
   static union dtype_ gl2531_;
   ptr gl2532_;
   static int gl2533_;
   static union dtype_ gl2534_;
   ptr gl2535_;
   static int gl2536_;
   static union dtype_ gl2537_;
   ptr gl2538_;
   static int gl2539_;
   static union dtype_ gl2540_;
   ptr gl2541_;
   static int gl2542_;
   static union dtype_ gl2543_;
   ptr gl2544_;
   static int gl2545_;
   static union dtype_ gl2546_;
   ptr gl2547_;
   static int gl2548_;
   static union dtype_ gl2549_;
   ptr gl2550_;
   static int gl2551_;
   static union dtype_ gl2552_;
   ptr gl2553_;
   static int gl2554_;
   static union dtype_ gl2555_;
   ptr gl2556_;
   static int gl2557_;
   static union dtype_ gl2558_;
   ptr gl2559_;
   static int gl2560_;
   static union dtype_ gl2561_;
   ptr gl2562_;
   static int gl2563_;
   static union dtype_ gl2564_;
   ptr gl2565_;
   static int gl2566_;
   static union dtype_ gl2567_;
   ptr gl2568_;
   static int gl2569_;
   static union dtype_ gl2570_;
   ptr gl2571_;
   static int gl2572_;
   static union dtype_ gl2573_;
   ptr gl2574_;
   static int gl2575_;
   static union dtype_ gl2576_;
   ptr gl2577_;
   static int gl2578_;
   static union dtype_ gl2579_;
   ptr gl2580_;
   static int gl2581_;
   static union dtype_ gl2582_;
   ptr gl2583_;
   static int gl2584_;
   static union dtype_ gl2585_;
   ptr gl2586_;
   static int gl2587_;
   static union dtype_ gl2588_;
   ptr gl2589_;
   static int gl2590_;
   static union dtype_ gl2591_;
   ptr gl2592_;
   static int gl2593_;
   static union dtype_ gl2594_;
   char gl240_;
   char gl241_;
   ptr gl2595_;
   static int gl2596_;
   static union dtype_ gl2597_;
   char gl242_;
   ptr gl2598_;
   static int gl2599_;
   static union dtype_ gl2600_;
   char gl243_;
   ptr gl2601_;
   static int gl2602_;
   static union dtype_ gl2603_;
   ptr gl2604_;
   static int gl2605_;
   static union dtype_ gl2606_;
   char gl244_;
   char gl245_;
   ptr gl2607_;
   static int gl2608_;
   static union dtype_ gl2609_;
   char gl246_;
   ptr gl2610_;
   static int gl2611_;
   static union dtype_ gl2612_;
   ptr gl2613_;
   static int gl2614_;
   static union dtype_ gl2615_;
   char gl247_;
   char gl248_;
   ptr gl2616_;
   static int gl2617_;
   static union dtype_ gl2618_;
   ptr gl2619_;
   static int gl2620_;
   static union dtype_ gl2621_;
   ptr gl2622_;
   static int gl2623_;
   static union dtype_ gl2624_;
   ptr gl2625_;
   static int gl2626_;
   static union dtype_ gl2627_;
   ptr gl2628_;
   static int gl2629_;
   static union dtype_ gl2630_;
   ptr gl2631_;
   static int gl2632_;
   static union dtype_ gl2633_;
   ptr gl2634_;
   static int gl2635_;
   static union dtype_ gl2636_;
   int    num_args__ = S_int_VOID_;
   ptr    obj_res_type__ = 0;
   int    i__ = S_int_VOID_;
   int    i_249_ = S_int_VOID_;

   if ((IATT_(self__,84) > GLO68_g_tag_)) {
      if ((IATT_(self__,64) > 0)) {
         GLO94_cprint_ctemp_name_(0,IATT_(self__,64),outfile__);
         goto ret0__;
      }
      else {
         ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1727_)),IATT_(self__,84)),(ptr)(&ls1728_)),GLO68_g_tag_));
      }
   }
   else {
   }
   IATT_(self__,84) = (int)(IATT_(self__,84) + 1);
   num_args__ = (int)0;
   if ((PATT_(self__,32) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   EXP113_cprint_cast_code_(self__,outfile__);
   gl2412_ = PATT_(self__,24);
   cache_dispatch_(gl2412_,1577,gl2413_,INTVAL_(gl2414_));
   obj_res_type__ = (ptr)PATT_(gl2412_,INTVAL_(gl2414_));
   if (EXP113_requires_dispatched_call_(self__)) {
      if ((PATT_(self__,36) == 0)) {
         switch (IATT_(self__,28)) {
            case (31) :
               ERR96_format_warning_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2007_)));
               gl2415_ = obj_res_type__;
               cache_dispatch_(gl2415_,1549,gl2416_,INTVAL_(gl2417_));
               if (CFN_(gl2417_)(gl2415_)) {
                  ERR96_format_error_exit_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2008_)));
               }
               else {
               }
               gl2418_ = obj_res_type__;
               cache_dispatch_(gl2418_,1893,gl2419_,INTVAL_(gl2420_));
               if (CFN_(gl2420_)(gl2418_)) {
                  if ((num_args__ != 1)) {
                     ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2009_)));
                  }
                  else {
                  }
                  PRI187_cprint_new_(0,outfile__,1,17,self__);
               }
               else {
                  gl2421_ = obj_res_type__;
                  cache_dispatch_(gl2421_,1894,gl2422_,INTVAL_(gl2423_));
                  if (CFN_(gl2423_)(gl2421_)) {
                     if ((num_args__ != 2)) {
                        ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2010_)));
                     }
                     else {
                     }
                     PRI187_cprint_new_(0,outfile__,2,19,self__);
                  }
                  else {
                     gl2424_ = obj_res_type__;
                     cache_dispatch_(gl2424_,1895,gl2425_,INTVAL_(gl2426_));
                     if (CFN_(gl2426_)(gl2424_)) {
                        if ((num_args__ != 3)) {
                           ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2011_)));
                        }
                        else {
                        }
                        PRI187_cprint_new_(0,outfile__,3,21,self__);
                     }
                     else {
                        gl2427_ = obj_res_type__;
                        cache_dispatch_(gl2427_,1896,gl2428_,INTVAL_(gl2429_));
                        if (CFN_(gl2429_)(gl2427_)) {
                           if ((num_args__ != 4)) {
                              ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2012_)));
                           }
                           else {
                           }
                           PRI187_cprint_new_(0,outfile__,4,23,self__);
                        }
                        else {
                           PRI187_cprint_new_(0,outfile__,(- 1),14,self__);
                        }
                     }
                  }
               }
               break;
            case (30) :
               gl2430_ = obj_res_type__;
               cache_dispatch_(gl2430_,1893,gl2431_,INTVAL_(gl2432_));
               if (CFN_(gl2432_)(gl2430_)) {
                  if ((num_args__ != 1)) {
                     ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2013_)));
                     (void)SAT99_s_(outfile__,(ptr)(&ls1897_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls2014_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                  }
                  else {
                     (void)SAT99_s_(outfile__,(ptr)(&ls1897_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                     (void)SAT99_c_(outfile__,',');
                     EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls2015_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                  }
               }
               else {
                  gl2433_ = obj_res_type__;
                  cache_dispatch_(gl2433_,1894,gl2434_,INTVAL_(gl2435_));
                  if (CFN_(gl2435_)(gl2433_)) {
                     if ((num_args__ != 2)) {
                        ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2016_)));
                        (void)SAT99_s_(outfile__,(ptr)(&ls1899_));
                        GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls2017_));
                        GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                     }
                     else {
                        (void)SAT99_s_(outfile__,(ptr)(&ls1899_));
                        GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                        (void)SAT99_c_(outfile__,',');
                        EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                        (void)SAT99_c_(outfile__,',');
                        EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls2015_));
                        GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                     }
                  }
                  else {
                     gl2436_ = obj_res_type__;
                     cache_dispatch_(gl2436_,1895,gl2437_,INTVAL_(gl2438_));
                     if (CFN_(gl2438_)(gl2436_)) {
                        if ((num_args__ != 3)) {
                           ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2018_)));
                           (void)SAT99_s_(outfile__,(ptr)(&ls1900_));
                           GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls2019_));
                           GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                        }
                        else {
                           (void)SAT99_s_(outfile__,(ptr)(&ls1900_));
                           GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                           (void)SAT99_c_(outfile__,',');
                           EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                           (void)SAT99_c_(outfile__,',');
                           EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
                           (void)SAT99_c_(outfile__,',');
                           EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls2015_));
                           GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                        }
                     }
                     else {
                        gl2439_ = obj_res_type__;
                        cache_dispatch_(gl2439_,1896,gl2440_,INTVAL_(gl2441_));
                        if (CFN_(gl2441_)(gl2439_)) {
                           if ((num_args__ != 4)) {
                              ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2020_)));
                              (void)SAT99_s_(outfile__,(ptr)(&ls1901_));
                              GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                              (void)SAT99_s_(outfile__,(ptr)(&ls2021_));
                              GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                              (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                           }
                           else {
                              (void)SAT99_s_(outfile__,(ptr)(&ls1901_));
                              GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,3,outfile__);
                              (void)SAT99_s_(outfile__,(ptr)(&ls2015_));
                              GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
                              (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
                           }
                        }
                        else {
                           (void)SAT99_s_(outfile__,(ptr)(&ls2022_));
                        }
                     }
                  }
               }
               break;
            case (28) :
               PRI187_cprint_copy_(0,outfile__,24,self__);
               break;
            case (23) :
               gl2442_ = obj_res_type__;
               cache_dispatch_(gl2442_,1893,gl2443_,INTVAL_(gl2444_));
               if (CFN_(gl2444_)(gl2442_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
               }
               else {
                  gl2445_ = obj_res_type__;
                  cache_dispatch_(gl2445_,1894,gl2446_,INTVAL_(gl2447_));
                  if (CFN_(gl2447_)(gl2445_)) {
                     (void)SAT99_c_(outfile__,'(');
                     PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
                     (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
                     PRI187_cprint_int_attr_access_(0,outfile__,2,self__);
                     (void)SAT99_c_(outfile__,')');
                  }
                  else {
                     gl2448_ = obj_res_type__;
                     cache_dispatch_(gl2448_,1895,gl2449_,INTVAL_(gl2450_));
                     if (CFN_(gl2450_)(gl2448_)) {
                        (void)SAT99_c_(outfile__,'(');
                        PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
                        PRI187_cprint_int_attr_access_(0,outfile__,2,self__);
                        (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
                        PRI187_cprint_int_attr_access_(0,outfile__,3,self__);
                        (void)SAT99_c_(outfile__,')');
                     }
                     else {
                        gl2451_ = obj_res_type__;
                        cache_dispatch_(gl2451_,1896,gl2452_,INTVAL_(gl2453_));
                        if (CFN_(gl2453_)(gl2451_)) {
                           (void)SAT99_c_(outfile__,'(');
                           PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
                           PRI187_cprint_int_attr_access_(0,outfile__,2,self__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
                           PRI187_cprint_int_attr_access_(0,outfile__,3,self__);
                           (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
                           PRI187_cprint_int_attr_access_(0,outfile__,4,self__);
                           (void)SAT99_c_(outfile__,')');
                        }
                        else {
                        }
                     }
                  }
               }
               break;
            case (24) :
               gl2454_ = obj_res_type__;
               cache_dispatch_(gl2454_,1893,gl2455_,INTVAL_(gl2456_));
               gl225_ = CFN_(gl2456_)(gl2454_);
               gl2457_ = obj_res_type__;
               cache_dispatch_(gl2457_,1894,gl2458_,INTVAL_(gl2459_));
               gl226_ = CFN_(gl2459_)(gl2457_);
               gl2460_ = obj_res_type__;
               cache_dispatch_(gl2460_,1895,gl2461_,INTVAL_(gl2462_));
               gl227_ = CFN_(gl2462_)(gl2460_);
               gl2463_ = obj_res_type__;
               cache_dispatch_(gl2463_,1896,gl2464_,INTVAL_(gl2465_));
               gl228_ = CFN_(gl2465_)(gl2463_);
               if ((((gl225_ | gl226_) | gl227_) | gl228_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2023_));
               }
               break;
            case (25) :
               gl2466_ = obj_res_type__;
               cache_dispatch_(gl2466_,1894,gl2467_,INTVAL_(gl2468_));
               gl229_ = CFN_(gl2468_)(gl2466_);
               gl2469_ = obj_res_type__;
               cache_dispatch_(gl2469_,1895,gl2470_,INTVAL_(gl2471_));
               gl230_ = CFN_(gl2471_)(gl2469_);
               gl2472_ = obj_res_type__;
               cache_dispatch_(gl2472_,1896,gl2473_,INTVAL_(gl2474_));
               gl231_ = CFN_(gl2474_)(gl2472_);
               if (((gl229_ | gl230_) | gl231_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,2,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2024_));
               }
               break;
            case (26) :
               gl2475_ = obj_res_type__;
               cache_dispatch_(gl2475_,1895,gl2476_,INTVAL_(gl2477_));
               gl232_ = CFN_(gl2477_)(gl2475_);
               gl2478_ = obj_res_type__;
               cache_dispatch_(gl2478_,1896,gl2479_,INTVAL_(gl2480_));
               gl233_ = CFN_(gl2480_)(gl2478_);
               if ((gl232_ | gl233_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,3,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2025_));
               }
               break;
            case (27) :
               gl2481_ = obj_res_type__;
               cache_dispatch_(gl2481_,1896,gl2482_,INTVAL_(gl2483_));
               if (CFN_(gl2483_)(gl2481_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,4,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2026_));
               }
               break;
            case (33) :
            case (34) :
            case (37) :
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2027_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls2028_));
               break;
            case (32) :
               gl2484_ = obj_res_type__;
               cache_dispatch_(gl2484_,1778,gl2485_,INTVAL_(gl2486_));
               gl234_ = CFN_(gl2486_)(gl2484_);
               gl2487_ = obj_res_type__;
               cache_dispatch_(gl2487_,1768,gl2488_,INTVAL_(gl2489_));
               gl235_ = CFN_(gl2489_)(gl2487_);
               gl2490_ = obj_res_type__;
               cache_dispatch_(gl2490_,2029,gl2491_,INTVAL_(gl2492_));
               gl236_ = CFN_(gl2492_)(gl2490_);
               gl2493_ = obj_res_type__;
               cache_dispatch_(gl2493_,1886,gl2494_,INTVAL_(gl2495_));
               gl237_ = CFN_(gl2495_)(gl2493_);
               gl2496_ = obj_res_type__;
               cache_dispatch_(gl2496_,1887,gl2497_,INTVAL_(gl2498_));
               gl238_ = CFN_(gl2498_)(gl2496_);
               if (((((gl234_ | gl235_) | gl236_) | gl237_) | gl238_)) {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2030_));
               }
               else {
                  PRI187_cprint_type_access_(0,outfile__,10,self__);
               }
               break;
            default:
               if ((! EXP113_handle_spec_expr_act_code_(self__,outfile__))) {
                  (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2031_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls2028_));
               }
               else {
               }
               ;
         }
      }
      else {
         gl2499_ = PATT_(self__,36);
         switch (TYPE_(gl2499_)) {
            case (151) :
            case (152) :
               gl2502_ = PATT_(self__,8);
               cache_dispatch_(gl2502_,374,gl2503_,INTVAL_(gl2504_));
               switch (IFN_(gl2504_)(gl2502_)) {
                  case (1) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2032_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
                     (void)SAT99_c_(outfile__,')');
                     break;
                  case (3) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2033_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
                     (void)SAT99_c_(outfile__,')');
                     break;
                  case (2) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2034_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
                     (void)SAT99_c_(outfile__,')');
                     break;
                  case (4) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2035_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
                     (void)SAT99_c_(outfile__,')');
                     break;
                  case (5) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2036_));
                     GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
                     (void)SAT99_c_(outfile__,')');
                     break;
                  default:
                     ;
                     ;
               }
               break;
            case (156) :
               switch (IATT_(self__,60)) {
                  case (1) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2037_));
                     break;
                  case (2) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2038_));
                     break;
                  case (3) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2039_));
                     break;
                  case (4) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2040_));
                     break;
                  case (5) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2041_));
                     break;
                  case (6) :
                     (void)SAT99_s_(outfile__,(ptr)(&ls2042_));
                     break;
                  default:
                     ;
                     ;
               }
               GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
               (void)SAT99_s_(outfile__,(ptr)(&ls1978_));
               GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
               i__ = (int)0;
               while (1) {
                  if ((i__ >= num_args__)) {
                     goto goto_tag_2637_;
                  }
                  else {
                  }
                  (void)SAT99_c_(outfile__,',');
                  EXP113_cprint_ith_arg_act_code_(self__,i__,outfile__);
                  i__ = (int)(i__ + 1);
               }
            goto_tag_2637_: ;
               (void)SAT99_c_(outfile__,')');
               break;
            case (153) :
               gl2508_ = PATT_(self__,36);
               cache_dispatch_(gl2508_,661,gl2509_,INTVAL_(gl2510_));
               gl2505_ = PFN_(gl2510_)(gl2508_);
               cache_dispatch_(gl2505_,374,gl2506_,INTVAL_(gl2507_));
               switch (IFN_(gl2507_)(gl2505_)) {
                  case (1) :
                     PRI187_cprint_ptr_attr_access_(0,outfile__,1,self__);
                     break;
                  case (3) :
                     PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
                     break;
                  case (2) :
                     PRI187_cprint_char_attr_access_(0,outfile__,1,self__);
                     break;
                  case (4) :
                     PRI187_cprint_float_attr_access_(0,outfile__,1,self__);
                     break;
                  case (5) :
                     PRI187_cprint_double_attr_access_(0,outfile__,1,self__);
                     break;
                  default:
                     ;
                     ;
               }
               break;
            default:
               ;
               ;
         }
      }
   }
   else {
      if ((PATT_(self__,36) == 0)) {
         switch (IATT_(self__,28)) {
            case (32) :
               gl2511_ = obj_res_type__;
               cache_dispatch_(gl2511_,1547,gl2512_,INTVAL_(gl2513_));
               gl239_ = IFN_(gl2513_)(gl2511_);
               (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,'('),gl239_),')');
               goto ret0__;
               break;
            case (34) :
            case (37) :
            case (33) :
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2043_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls2028_));
               break;
            case (31) :
               gl2514_ = obj_res_type__;
               cache_dispatch_(gl2514_,1893,gl2515_,INTVAL_(gl2516_));
               if (CFN_(gl2516_)(gl2514_)) {
                  if ((num_args__ != 1)) {
                     ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2044_)));
                  }
                  else {
                  }
                  PRI187_cprint_new_(0,outfile__,1,16,self__);
               }
               else {
                  gl2517_ = obj_res_type__;
                  cache_dispatch_(gl2517_,1894,gl2518_,INTVAL_(gl2519_));
                  if (CFN_(gl2519_)(gl2517_)) {
                     if ((num_args__ != 2)) {
                        ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2045_)));
                     }
                     else {
                     }
                     PRI187_cprint_new_(0,outfile__,2,18,self__);
                  }
                  else {
                     gl2520_ = obj_res_type__;
                     cache_dispatch_(gl2520_,1895,gl2521_,INTVAL_(gl2522_));
                     if (CFN_(gl2522_)(gl2520_)) {
                        if ((num_args__ != 3)) {
                           ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2046_)));
                        }
                        else {
                        }
                        PRI187_cprint_new_(0,outfile__,3,20,self__);
                     }
                     else {
                        gl2523_ = obj_res_type__;
                        cache_dispatch_(gl2523_,1896,gl2524_,INTVAL_(gl2525_));
                        if (CFN_(gl2525_)(gl2523_)) {
                           if ((num_args__ != 4)) {
                              ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2012_)));
                           }
                           else {
                           }
                           PRI187_cprint_new_(0,outfile__,4,22,self__);
                        }
                        else {
                           PRI187_cprint_new_(0,outfile__,0,15,self__);
                        }
                     }
                  }
               }
               goto ret0__;
               break;
            case (30) :
               gl2526_ = obj_res_type__;
               cache_dispatch_(gl2526_,1893,gl2527_,INTVAL_(gl2528_));
               if (CFN_(gl2528_)(gl2526_)) {
                  if ((num_args__ != 1)) {
                     ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2047_)));
                     (void)SAT99_s_(outfile__,(ptr)(&ls1897_));
                     gl2529_ = PATT_(self__,24);
                     cache_dispatch_(gl2529_,965,gl2530_,INTVAL_(gl2531_));
                     VFN_(gl2531_)(gl2529_,outfile__);
                     gl2532_ = obj_res_type__;
                     cache_dispatch_(gl2532_,418,gl2533_,INTVAL_(gl2534_));
                     (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2000_)),IATT_(PFN_(gl2534_)(gl2532_),60)),')');
                  }
                  else {
                     (void)SAT99_s_(outfile__,(ptr)(&ls1897_));
                     gl2535_ = PATT_(self__,24);
                     cache_dispatch_(gl2535_,965,gl2536_,INTVAL_(gl2537_));
                     VFN_(gl2537_)(gl2535_,outfile__);
                     (void)SAT99_c_(outfile__,',');
                     EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                     gl2538_ = obj_res_type__;
                     cache_dispatch_(gl2538_,418,gl2539_,INTVAL_(gl2540_));
                     (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl2540_)(gl2538_),60)),')');
                  }
               }
               else {
                  gl2541_ = obj_res_type__;
                  cache_dispatch_(gl2541_,1894,gl2542_,INTVAL_(gl2543_));
                  if (CFN_(gl2543_)(gl2541_)) {
                     if ((num_args__ != 2)) {
                        ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2016_)));
                        (void)SAT99_s_(outfile__,(ptr)(&ls1899_));
                        gl2544_ = PATT_(self__,24);
                        cache_dispatch_(gl2544_,965,gl2545_,INTVAL_(gl2546_));
                        VFN_(gl2546_)(gl2544_,outfile__);
                        gl2547_ = obj_res_type__;
                        cache_dispatch_(gl2547_,418,gl2548_,INTVAL_(gl2549_));
                        (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2002_)),IATT_(PFN_(gl2549_)(gl2547_),60)),')');
                     }
                     else {
                        (void)SAT99_s_(outfile__,(ptr)(&ls1899_));
                        gl2550_ = PATT_(self__,24);
                        cache_dispatch_(gl2550_,965,gl2551_,INTVAL_(gl2552_));
                        VFN_(gl2552_)(gl2550_,outfile__);
                        (void)SAT99_c_(outfile__,',');
                        EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                        (void)SAT99_c_(outfile__,',');
                        EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
                        gl2553_ = obj_res_type__;
                        cache_dispatch_(gl2553_,418,gl2554_,INTVAL_(gl2555_));
                        (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl2555_)(gl2553_),60)),')');
                     }
                  }
                  else {
                     gl2556_ = obj_res_type__;
                     cache_dispatch_(gl2556_,1895,gl2557_,INTVAL_(gl2558_));
                     if (CFN_(gl2558_)(gl2556_)) {
                        if ((num_args__ != 3)) {
                           ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2018_)));
                           (void)SAT99_s_(outfile__,(ptr)(&ls1900_));
                           gl2559_ = PATT_(self__,24);
                           cache_dispatch_(gl2559_,965,gl2560_,INTVAL_(gl2561_));
                           VFN_(gl2561_)(gl2559_,outfile__);
                           gl2562_ = obj_res_type__;
                           cache_dispatch_(gl2562_,418,gl2563_,INTVAL_(gl2564_));
                           (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2004_)),IATT_(PFN_(gl2564_)(gl2562_),60)),')');
                        }
                        else {
                           (void)SAT99_s_(outfile__,(ptr)(&ls1900_));
                           gl2565_ = PATT_(self__,24);
                           cache_dispatch_(gl2565_,965,gl2566_,INTVAL_(gl2567_));
                           VFN_(gl2567_)(gl2565_,outfile__);
                           (void)SAT99_c_(outfile__,',');
                           EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                           (void)SAT99_c_(outfile__,',');
                           EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
                           (void)SAT99_c_(outfile__,',');
                           EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
                           gl2568_ = obj_res_type__;
                           cache_dispatch_(gl2568_,418,gl2569_,INTVAL_(gl2570_));
                           (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl2570_)(gl2568_),60)),')');
                        }
                     }
                     else {
                        gl2571_ = obj_res_type__;
                        cache_dispatch_(gl2571_,1896,gl2572_,INTVAL_(gl2573_));
                        if (CFN_(gl2573_)(gl2571_)) {
                           if ((num_args__ != 4)) {
                              ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_create_(0),(ptr)(&ls2020_)));
                              (void)SAT99_s_(outfile__,(ptr)(&ls1901_));
                              gl2574_ = PATT_(self__,24);
                              cache_dispatch_(gl2574_,965,gl2575_,INTVAL_(gl2576_));
                              VFN_(gl2576_)(gl2574_,outfile__);
                              gl2577_ = obj_res_type__;
                              cache_dispatch_(gl2577_,418,gl2578_,INTVAL_(gl2579_));
                              (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2006_)),IATT_(PFN_(gl2579_)(gl2577_),60)),')');
                           }
                           else {
                              (void)SAT99_s_(outfile__,(ptr)(&ls1901_));
                              gl2580_ = PATT_(self__,24);
                              cache_dispatch_(gl2580_,965,gl2581_,INTVAL_(gl2582_));
                              VFN_(gl2582_)(gl2580_,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,0,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,1,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,2,outfile__);
                              (void)SAT99_c_(outfile__,',');
                              EXP113_cprint_ith_arg_act_code_(self__,3,outfile__);
                              gl2583_ = obj_res_type__;
                              cache_dispatch_(gl2583_,418,gl2584_,INTVAL_(gl2585_));
                              (void)SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl2585_)(gl2583_),60)),')');
                           }
                        }
                        else {
                           (void)SAT99_s_(outfile__,(ptr)(&ls2022_));
                        }
                     }
                  }
               }
               break;
            case (28) :
               PRI187_cprint_copy_(0,outfile__,25,self__);
               goto ret0__;
               break;
            case (23) :
               gl2586_ = obj_res_type__;
               cache_dispatch_(gl2586_,1893,gl2587_,INTVAL_(gl2588_));
               if (CFN_(gl2588_)(gl2586_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,5,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2048_));
               }
               break;
            case (24) :
               gl2589_ = obj_res_type__;
               cache_dispatch_(gl2589_,1893,gl2590_,INTVAL_(gl2591_));
               gl240_ = CFN_(gl2591_)(gl2589_);
               gl2592_ = obj_res_type__;
               cache_dispatch_(gl2592_,1894,gl2593_,INTVAL_(gl2594_));
               gl241_ = CFN_(gl2594_)(gl2592_);
               gl2595_ = obj_res_type__;
               cache_dispatch_(gl2595_,1895,gl2596_,INTVAL_(gl2597_));
               gl242_ = CFN_(gl2597_)(gl2595_);
               gl2598_ = obj_res_type__;
               cache_dispatch_(gl2598_,1896,gl2599_,INTVAL_(gl2600_));
               gl243_ = CFN_(gl2600_)(gl2598_);
               if ((((gl240_ | gl241_) | gl242_) | gl243_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,5,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2049_));
               }
               break;
            case (25) :
               gl2601_ = obj_res_type__;
               cache_dispatch_(gl2601_,1894,gl2602_,INTVAL_(gl2603_));
               gl244_ = CFN_(gl2603_)(gl2601_);
               gl2604_ = obj_res_type__;
               cache_dispatch_(gl2604_,1895,gl2605_,INTVAL_(gl2606_));
               gl245_ = CFN_(gl2606_)(gl2604_);
               gl2607_ = obj_res_type__;
               cache_dispatch_(gl2607_,1896,gl2608_,INTVAL_(gl2609_));
               gl246_ = CFN_(gl2609_)(gl2607_);
               if (((gl244_ | gl245_) | gl246_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,6,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2050_));
               }
               break;
            case (26) :
               gl2610_ = obj_res_type__;
               cache_dispatch_(gl2610_,1895,gl2611_,INTVAL_(gl2612_));
               gl247_ = CFN_(gl2612_)(gl2610_);
               gl2613_ = obj_res_type__;
               cache_dispatch_(gl2613_,1896,gl2614_,INTVAL_(gl2615_));
               gl248_ = CFN_(gl2615_)(gl2613_);
               if ((gl247_ | gl248_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,7,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2051_));
               }
               break;
            case (27) :
               gl2616_ = obj_res_type__;
               cache_dispatch_(gl2616_,1896,gl2617_,INTVAL_(gl2618_));
               if (CFN_(gl2618_)(gl2616_)) {
                  PRI187_cprint_int_attr_access_(0,outfile__,8,self__);
               }
               else {
                  (void)SAT99_s_(outfile__,(ptr)(&ls2052_));
               }
               break;
            default:
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2053_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls2028_));
               ;
         }
         goto ret0__;
      }
      else {
         gl2619_ = PATT_(self__,36);
         switch (TYPE_(gl2619_)) {
            case (151) :
            case (152) :
               gl2622_ = PATT_(self__,36);
               cache_dispatch_(gl2622_,809,gl2623_,INTVAL_(gl2624_));
               VFN_(gl2624_)(gl2622_,outfile__);
               break;
            case (156) :
               gl2625_ = PATT_(self__,36);
               cache_dispatch_(gl2625_,808,gl2626_,INTVAL_(gl2627_));
               VFN_(gl2627_)(gl2625_,outfile__);
               (void)SAT99_c_(outfile__,'(');
               gl2628_ = PATT_(self__,24);
               cache_dispatch_(gl2628_,965,gl2629_,INTVAL_(gl2630_));
               VFN_(gl2630_)(gl2628_,outfile__);
               i_249_ = (int)0;
               while (1) {
                  if ((i_249_ >= num_args__)) {
                     goto goto_tag_2638_;
                  }
                  else {
                  }
                  (void)SAT99_c_(outfile__,',');
                  EXP113_cprint_ith_arg_act_code_(self__,i_249_,outfile__);
                  i_249_ = (int)(i_249_ + 1);
               }
            goto_tag_2638_: ;
               (void)SAT99_c_(outfile__,')');
               break;
            case (153) :
               gl2634_ = PATT_(self__,36);
               cache_dispatch_(gl2634_,661,gl2635_,INTVAL_(gl2636_));
               gl2631_ = PFN_(gl2636_)(gl2634_);
               cache_dispatch_(gl2631_,374,gl2632_,INTVAL_(gl2633_));
               switch (IFN_(gl2633_)(gl2631_)) {
                  case (1) :
                     PRI187_cprint_ptr_attr_access_(0,outfile__,9,self__);
                     break;
                  case (3) :
                     PRI187_cprint_int_attr_access_(0,outfile__,9,self__);
                     break;
                  case (2) :
                     PRI187_cprint_char_attr_access_(0,outfile__,9,self__);
                     break;
                  case (4) :
                     PRI187_cprint_float_attr_access_(0,outfile__,9,self__);
                     break;
                  case (5) :
                     PRI187_cprint_double_attr_access_(0,outfile__,9,self__);
                     break;
                  default:
                     ;
                     ;
               }
               break;
            default:
               (void)SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2053_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls2028_));
               ;
         }
      }
   }

   ret0__:
   return;
}

EXP113_cprint_cast_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,2,ls1493_,"(");
   SATHER_STR_(20,9,ls350_,"EXPROB_S");
   SATHER_STR_(20,20,ls1685_,"Invalid C type cast");
   SATHER_STR_(20,2,ls1278_,")");

   if ((IATT_(self__,16) != 0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls1493_));
      switch (IATT_(self__,16)) {
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

char EXP113_handle_spec_expr_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,32,ls1909_,"(EXPR_ARGS_EXPROB_S): Feature \"");
   SATHER_STR_(20,33,ls1910_,"\" applied to unknown object type");
   SATHER_STR_(20,19,ls1667_,"EXPR_ARGS_EXPROB_S");
   SATHER_STR_(20,22,ls1953_,"Invalid class number ");
   SATHER_STR_(20,34,ls1957_,"Invalid descendent index for $OB ");
   SATHER_STR_(20,28,ls1959_,"Missing symbol table class ");
   SATHER_STR_(20,30,ls1961_,"(EXPR_ARGS_EXPROB_S): Use of ");
   SATHER_STR_(20,41,ls1962_," in f.expr(f:$OB) requires a routine in ");
   SATHER_STR_(20,62,ls1965_," in f.expr(f:$OB) requires a routine with no return value in ");
   SATHER_STR_(20,59,ls1968_," in f.expr(f:$OB) requires a routine with no parameter in ");
   SATHER_STR_(20,42,ls1970_,"(EXPR_ARGS_EXPROB_S): No class contains \"");
   SATHER_STR_(20,15,ls1971_,"\" as a routine");
   ptr gl2639_;
   static int gl2640_;
   static union dtype_ gl2641_;
   ptr gl2642_;
   static int gl2643_;
   static union dtype_ gl2644_;
   int gl250_;
   ptr    obj_res_type__ = 0;
   int    obj_type_index__ = S_int_VOID_;
   int    num_args__ = S_int_VOID_;
   ptr    obj_cls__ = 0;
   ptr    descendents__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;
   int    num_found__ = S_int_VOID_;
   int    des_ind__ = S_int_VOID_;
   ptr    des_cls__ = 0;
   ptr    tmp_symtab__ = 0;
   ptr    tmp_ref__ = 0;
   ptr    tmp_rout_ref__ = 0;
   int    tmp_params_num__ = S_int_VOID_;

   res__ = (char)1;
   gl2639_ = PATT_(self__,24);
   cache_dispatch_(gl2639_,1577,gl2640_,INTVAL_(gl2641_));
   obj_res_type__ = (ptr)PATT_(gl2639_,INTVAL_(gl2641_));
   if ((obj_res_type__ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1909_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1910_)));
      res__ = (char)0;
      goto ret0__;
   }
   else {
   }
   obj_type_index__ = (int)INS150_inst_ind_(obj_res_type__);
   if ((obj_type_index__ != RES97_OB_ici_)) {
      res__ = (char)0;
      goto ret0__;
   }
   else {
   }
   num_args__ = (int)0;
   if ((PATT_(self__,32) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   if ((num_args__ != 0)) {
      res__ = (char)0;
      goto ret0__;
   }
   else {
   }
   obj_cls__ = (ptr)CLA93_at_index_(GLO68_class_inst_,obj_type_index__);
   if ((obj_cls__ == 0)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1953_)),obj_type_index__));
      res__ = (char)0;
      goto ret0__;
   }
   else {
   }
   descendents__ = (ptr)PATT_(obj_cls__,76);
   i__ = (int)0;
   sz__ = (int)IATT_(descendents__,12);
   num_found__ = S_int_VOID_;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_2645_;
      }
      else {
      }
      des_ind__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((des_ind__ > 0)) {
         des_cls__ = (ptr)CLA93_at_index_(GLO68_class_inst_,des_ind__);
         if ((des_cls__ == 0)) {
            ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1957_)),des_ind__));
            res__ = (char)0;
            goto goto_tag_2645_;
         }
         else {
            tmp_symtab__ = (ptr)PATT_(des_cls__,48);
            if ((tmp_symtab__ == 0)) {
               ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1959_)),CLA148_full_name_(des_cls__)));
               res__ = (char)0;
               goto goto_tag_2645_;
            }
            else {
            }
            tmp_ref__ = (ptr)SYM186_get_feature_(tmp_symtab__,IATT_(self__,28));
            if ((tmp_ref__ != 0)) {
               gl2642_ = tmp_ref__;
               gl250_ = TYPE_(gl2642_);
               if ((gl250_ != 156)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1961_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1962_)),CLA148_full_name_(des_cls__)));
                  res__ = (char)0;
                  goto goto_tag_2645_;
               }
               else {
               }
               tmp_rout_ref__ = (ptr)tmp_ref__;
               if ((PATT_(tmp_rout_ref__,60) != 0)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1961_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1965_)),CLA148_full_name_(des_cls__)));
                  res__ = (char)0;
                  goto goto_tag_2645_;
               }
               else {
               }
               tmp_params_num__ = (int)IATT_(PATT_(tmp_rout_ref__,56),12);
               if ((tmp_params_num__ != 0)) {
                  ERR96_format_error_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1961_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1968_)),CLA148_full_name_(des_cls__)));
                  res__ = (char)0;
                  goto goto_tag_2645_;
               }
               else {
               }
               num_found__ = (int)(num_found__ + 1);
               ROU156_update_used_in_dispatch_(tmp_rout_ref__);
               CATT_(des_cls__,9) = (char)1;
            }
            else {
            }
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_2645_: ;
   if ((num_found__ == 0)) {
      ERR96_format_warning_msg_(0,IATT_(self__,80),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1970_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1971_)));
   }
   else {
   }

   ret0__:
   return (res__);
}

char EXP113_handle_spec_expr_pre_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   char res__ = S_char_VOID_;
   ptr gl2646_;
   static int gl2647_;
   static union dtype_ gl2648_;
   ptr    obj_res_type__ = 0;
   int    obj_type_index__ = S_int_VOID_;
   int    num_args__ = S_int_VOID_;

   gl2646_ = PATT_(self__,24);
   cache_dispatch_(gl2646_,1577,gl2647_,INTVAL_(gl2648_));
   obj_res_type__ = (ptr)PATT_(gl2646_,INTVAL_(gl2648_));
   obj_type_index__ = (int)INS150_inst_ind_(obj_res_type__);
   if ((obj_type_index__ != RES97_OB_ici_)) {
      goto ret0__;
   }
   else {
   }
   num_args__ = (int)0;
   if ((PATT_(self__,32) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   if ((num_args__ != 0)) {
      goto ret0__;
   }
   else {
   }
   PRI187_cprint_ob_cache_dispatch_(0,outfile__,12,self__);
   res__ = (char)1;

   ret0__:
   return (res__);
}

char EXP113_handle_spec_expr_act_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,19,ls1667_,"EXPR_ARGS_EXPROB_S");
   SATHER_STR_(20,59,ls1975_,"(Code 4) in handling of f.expr where f:$OB.  Please report");
   SATHER_STR_(20,11,ls1976_,"((INTVAL_(");
   SATHER_STR_(20,14,ls1977_,")!=0)? (IFN_(");
   SATHER_STR_(20,3,ls1978_,")(");
   SATHER_STR_(20,6,ls1979_,")):0)");
   ptr gl2649_;
   static int gl2650_;
   static union dtype_ gl2651_;
   ptr    obj_res_type__ = 0;
   int    obj_type_index__ = S_int_VOID_;
   int    num_args__ = S_int_VOID_;

   gl2649_ = PATT_(self__,24);
   cache_dispatch_(gl2649_,1577,gl2650_,INTVAL_(gl2651_));
   obj_res_type__ = (ptr)PATT_(gl2649_,INTVAL_(gl2651_));
   obj_type_index__ = (int)INS150_inst_ind_(obj_res_type__);
   if ((obj_type_index__ != RES97_OB_ici_)) {
      goto ret0__;
   }
   else {
   }
   num_args__ = (int)0;
   if ((PATT_(self__,32) != 0)) {
      num_args__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   if ((num_args__ != 0)) {
      goto ret0__;
   }
   else {
   }
   if (((IATT_(self__,56) == 0) | (IATT_(self__,40) == 0))) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls1667_),(ptr)(&ls1975_));
      goto ret0__;
   }
   else {
   }
   (void)SAT99_s_(outfile__,(ptr)(&ls1976_));
   GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls1977_));
   GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls1978_));
   GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls1979_));
   res__ = (char)1;

   ret0__:
   return (res__);
}

EXP113_cprint_ith_arg_pre_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl2652_;
   static int gl2653_;
   static union dtype_ gl2654_;
   ptr gl2655_;
   static int gl2656_;
   static union dtype_ gl2657_;
   ptr    ith_arg__ = 0;
   int    ith_temp__ = S_int_VOID_;

   ith_arg__ = (ptr)PATT_(PATT_(self__,32), 28 + ((i__) << 2));
   ith_temp__ = (int)IATT_(PATT_(self__,72), 8 + ((i__) << 2));
   gl2652_ = ith_arg__;
   cache_dispatch_(gl2652_,1589,gl2653_,INTVAL_(gl2654_));
   VFN_(gl2654_)(gl2652_,outfile__);
   if ((ith_temp__ > 0)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl2655_ = ith_arg__;
      cache_dispatch_(gl2655_,965,gl2656_,INTVAL_(gl2657_));
      VFN_(gl2657_)(gl2655_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }

   ret0__:
   return;
}

EXP113_cprint_ith_arg_act_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   ptr gl2658_;
   static int gl2659_;
   static union dtype_ gl2660_;
   int    ith_temp__ = S_int_VOID_;

   ith_temp__ = (int)IATT_(PATT_(self__,72), 8 + ((i__) << 2));
   if ((ith_temp__ > 0)) {
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
   }
   else {
      gl2658_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
      cache_dispatch_(gl2658_,965,gl2659_,INTVAL_(gl2660_));
      VFN_(gl2660_)(gl2658_,outfile__);
   }

   ret0__:
   return;
}

char EXP113_requires_dispatched_call_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl2661_;
   static int gl2662_;
   static union dtype_ gl2663_;
   ptr gl2664_;
   static int gl2665_;
   static union dtype_ gl2666_;
   ptr gl2667_;
   static int gl2668_;
   static union dtype_ gl2669_;
   char gl251_;
   ptr gl2670_;
   static int gl2671_;
   static union dtype_ gl2672_;
   char gl252_;
   ptr gl2673_;
   static int gl2674_;
   static union dtype_ gl2675_;
   int gl253_;
   ptr    e__ = 0;

   gl2664_ = PATT_(self__,24);
   cache_dispatch_(gl2664_,1577,gl2665_,INTVAL_(gl2666_));
   gl2661_ = PATT_(gl2664_,INTVAL_(gl2666_));
   cache_dispatch_(gl2661_,1511,gl2662_,INTVAL_(gl2663_));
   if (CFN_(gl2663_)(gl2661_)) {
      res__ = (char)1;
   }
   else {
      if ((PATT_(self__,8) != 0)) {
         gl2667_ = PATT_(self__,8);
         cache_dispatch_(gl2667_,1511,gl2668_,INTVAL_(gl2669_));
         gl251_ = CFN_(gl2669_)(gl2667_);
         gl2670_ = PATT_(self__,8);
         cache_dispatch_(gl2670_,1982,gl2671_,INTVAL_(gl2672_));
         gl252_ = CATT_(gl2670_,INTVAL_(gl2672_));
         gl2673_ = PATT_(self__,24);
         gl253_ = TYPE_(gl2673_);
         if ((((! gl251_) & gl252_) & (gl253_ == 113))) {
            e__ = (ptr)PATT_(self__,24);
            if (CATT_(e__,5)) {
               res__ = (char)1;
            }
            else {
               if (EXP113_requires_dispatched_call_(e__)) {
                  res__ = (char)1;
                  CATT_(self__,5) = (char)1;
               }
               else {
               }
            }
         }
         else {
         }
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

