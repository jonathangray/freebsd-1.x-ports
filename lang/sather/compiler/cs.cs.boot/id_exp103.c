/* id_exp103.c : Sather class: ID_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr INT15_to_s_(int self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_i_(ptr self__, int in__);
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern /*constant*/ int RES71_copy_ind_;
extern /*constant*/ int RES71_self_ind_;
extern /*constant*/ int RES71_new_ind_;
extern /*constant*/ int RES71_asize_ind_;
extern /*constant*/ int RES71_asize1_ind_;
extern /*constant*/ int RES71_asize2_ind_;
extern /*constant*/ int RES71_asize3_ind_;
extern /*constant*/ int RES71_asize4_ind_;
extern /*constant*/ int RES71_extend_ind_;
extern /*constant*/ int RES71_type_ind_;
extern /*constant*/ int RES71_res_ind_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*constant*/ int RES71_void_ind_;
extern /*shared*/ ptr GLO68_voidtype_s_;
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ int GLO68_g_tag_;
extern void GLO94_check_f_ob_(ptr self__, ptr exp__, int name__, ptr symtab__);
extern char GLO94_cprint_ref_to_self_(ptr self__, ptr outfile__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*constant*/ int RES97_FOB_ici_;
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern void ERR96_format_error_exit_(ptr self__, int ln__, ptr s__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr SAT99_i_(ptr self__, int in__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern char GLO94_check_is_on_(ptr self__);
extern int GLO94_global_key_(ptr self__);
extern ptr LIS98_push_(ptr self__, int e__);
extern ptr LIS98_create_(ptr self__, int init_size__);
extern ptr INT106_create_(ptr self__, ptr v__, int ln__);
extern void GLO94_cprint_curr_exp_code_(ptr self__, ptr exp__, int temp_nm__, ptr outfile__);
extern void GLO94_cprint_ctemp_name_(ptr self__, int intval__, ptr outfile__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern void ERR96_format_warning_msg_(ptr self__, int ln__, ptr s__);
extern ptr CLA148_full_name_(ptr self__);
extern char TYP149_nonptr_p_(ptr self__);
extern ptr INS150_create_(ptr self__, int index__, int ln__);
extern ptr TYP149_inst_cls_(ptr self__);
extern ptr TYP149_paramstype_(ptr self__);
extern ptr TYP149_rettype_(ptr self__);
extern int TYP149_ctype_(ptr self__);
extern void TYP149_cprint_void_(ptr self__, ptr outfile__);
extern ptr FEA162_rettype_(ptr self__);
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
extern ptr SYM186_get_sym_(ptr self__, int nm__);
extern void PRI187_cprint_void_tst_(ptr self__, ptr outfile__, int cont__, ptr exp__);
extern ptr SEM188_typeof_(ptr self__);
extern void SEM188_cprint_init_code_(ptr self__, ptr outfile__);
extern ptr SEM188_get_constval_(ptr self__);
extern void PRI187_cprint_copy_(ptr self__, ptr outfile__, int cont__, ptr exp__);
extern void PRI187_cprint_new_(ptr self__, ptr outfile__, int dim__, int cont__, ptr exp__);
extern void PRI187_cprint_int_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
extern void SEM188_cprint_access_value_(ptr self__, ptr outfile__);
extern void SEM188_cprint_cname_(ptr self__, ptr outfile__);
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int ID_103_print_indent_ = 2;
ptr ID_103_create_(ptr self__, int nm__, int ln__);
void ID_103_out_of_line_(ptr self__, ptr fn__);
ptr ID_103_dup_(ptr self__);
void ID_103_put_kwdname_(ptr self__, int nm__);
ptr ID_103_sather_code_(ptr self__);
ptr ID_103_initialize_(ptr self__, ptr initarg__);
void ID_103_resolve_predef_types_(ptr self__, int index__);
void ID_103_semant_(ptr self__, ptr symtab__);
ptr ID_103_typeof_(ptr self__);
int ID_103_get_offset_(ptr self__);
void ID_103_cprint_offset_(ptr self__, ptr outfile__);
ptr ID_103_get_constval_(ptr self__);
void ID_103_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void ID_103_cprint_cname_(ptr self__, ptr outfile__);
void ID_103_cprint_extern_(ptr self__, ptr outfile__);
void ID_103_cprint_access_value_(ptr self__, ptr outfile__);
void ID_103_cprint_init_code_(ptr self__, ptr outfile__);
char ID_103_valid_init_expr_(ptr self__);
char ID_103_assignable_p_(ptr self__);
ptr ID_103_gen_temps_(ptr self__);
ptr ID_103_expr_eval_constant_(ptr self__);
void ID_103_get_ext_strs_(ptr self__);
char ID_103_to_be_pre_evaluated_(ptr self__);
char ID_103_access_value_only_(ptr self__);
void ID_103_fob_error_(ptr self__, ptr op_name__, ptr cls_name__);
void ID_103_cprint_pre_code_(ptr self__, ptr outfile__);
void ID_103_cprint_act_code_(ptr self__, ptr outfile__);
void ID_103_cprint_cast_code_(ptr self__, ptr outfile__);
/*constant*/ int ID_103_cont1_ = 1;
/*constant*/ int ID_103_cont2_ = 2;
/*constant*/ int ID_103_cont3_ = 3;
/*constant*/ int ID_103_cont4_ = 4;
/*constant*/ int ID_103_cont5_ = 5;
/*constant*/ int ID_103_cont6_ = 6;
/*constant*/ int ID_103_cont7_ = 7;
extern int attr_ent_ID_103[];

ptr ID_103_create_(ptr self__, int nm__, int ln__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(103,0);
   IATT_(res__,28) = (int)nm__;
   IATT_(res__,4) = (int)ln__;

   ret0__:
   return (res__);
}

void ID_103_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr ID_103_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)ID_103_create_(self__,IATT_(self__,28),IATT_(self__,4));

   ret0__:
   return (res__);
}

void ID_103_put_kwdname_(ptr self__, int nm__)
{
   ptr gl1231_;
   static int gl1232_;
   static union dtype_ gl1233_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1231_ = x__;
   cache_dispatch_(gl1231_,796,gl1232_,INTVAL_(gl1233_));
   IATT_(gl1231_,INTVAL_(gl1233_)) = (int)nm__;

   ret0__:
   return;
}

ptr ID_103_sather_code_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)STR20_s_(STR20_create_(0),STR69_at_index_(GLO68_str_table_,IATT_(self__,28)));

   ret0__:
   return (res__);
}

ptr ID_103_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void ID_103_resolve_predef_types_(ptr self__, int index__)
{


   ret0__:
   return;
}

void ID_103_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,66,ls1693_,"(ID_EXPROB_S): Applying \"new\" on an array class without arguments");
   SATHER_STR_(20,52,ls1694_,"(ID_EXPROB_S): Applying \"new\" on a non-object class");
   SATHER_STR_(20,32,ls1695_,"(ID_EXPROB_S): Abstract class \"");
   SATHER_STR_(20,21,ls1696_,"\" used in \"new\" call");
   SATHER_STR_(20,17,ls1697_,"(ID_EXPROB_S): \"");
   SATHER_STR_(20,29,ls1698_,"\" applied to non-array class");
   SATHER_STR_(20,31,ls1699_,"\" expects 2- or more dim ARRAY");
   SATHER_STR_(20,31,ls1700_,"\" expects 3- or more dim ARRAY");
   SATHER_STR_(20,31,ls1701_,"\" expects 4- or more dim ARRAY");
   SATHER_STR_(20,22,ls1702_,"\" expects ARRAY class");
   SATHER_STR_(20,47,ls1703_,"(ID_EXPROB_S): Arguments expected for \"extend\"");
   SATHER_STR_(20,49,ls1704_,"(ID_EXPROB_S): \"res\" used in non-routine feature");
   SATHER_STR_(20,58,ls1706_,"(ID_EXPROB_S): \"res\" used in routine without return type\n");
   SATHER_STR_(20,39,ls1707_,"(ID_EXPROB_S): Undeclared identifier \"");
   SATHER_STR_(20,7,ls1708_,"\" used");
   SATHER_STR_(20,30,ls1710_,"(ID_EXPROB_S): Routine call \"");
   SATHER_STR_(20,20,ls1711_,"\" expects arguments");
   ptr gl1234_;
   static int gl1235_;
   static union dtype_ gl1236_;
   ptr gl1237_;
   static int gl1238_;
   static union dtype_ gl1239_;
   ptr gl1240_;
   static int gl1241_;
   static union dtype_ gl1242_;
   ptr gl1243_;
   static int gl1244_;
   static union dtype_ gl1245_;
   int gl100_;
   ptr gl1246_;
   static int gl1247_;
   static union dtype_ gl1248_;
   ptr gl101_;
   ptr gl1249_;
   static int gl1250_;
   static union dtype_ gl1251_;
   ptr gl1252_;
   static int gl1253_;
   static union dtype_ gl1254_;
   int gl102_;
   ptr gl1255_;
   static int gl1256_;
   static union dtype_ gl1257_;
   ptr gl1258_;
   static int gl1259_;
   static union dtype_ gl1260_;
   ptr gl103_;
   ptr gl1261_;
   static int gl1262_;
   static union dtype_ gl1263_;
   ptr gl1264_;
   static int gl1265_;
   static union dtype_ gl1266_;
   ptr gl1267_;
   static int gl1268_;
   static union dtype_ gl1269_;
   ptr gl1270_;
   static int gl1271_;
   static union dtype_ gl1272_;
   ptr gl1273_;
   static int gl1274_;
   static union dtype_ gl1275_;
   ptr gl1276_;
   static int gl1277_;
   static union dtype_ gl1278_;
   int    num_params__ = S_int_VOID_;
   ptr    rout_feat__ = 0;
   ptr    const_feat__ = 0;
   ptr    attr_feat__ = 0;
   ptr    shared_feat__ = 0;

   if ((PATT_(self__,32) == 0)) {
      PATT_(self__,32) = (ptr)SYM186_get_sym_(symtab__,IATT_(self__,28));
   }
   else {
   }
   if ((IATT_(PATT_(symtab__,4),96) == RES97_FOB_ici_)) {
      GLO94_check_f_ob_(0,self__,IATT_(self__,28),symtab__);
   }
   else {
   }
   if ((PATT_(self__,32) == 0)) {
      switch (IATT_(self__,28)) {
         case (28) :
         case (34) :
            PATT_(self__,12) = (ptr)INS150_create_(0,IATT_(PATT_(symtab__,4),20),IATT_(self__,4));
            break;
         case (31) :
            PATT_(self__,12) = (ptr)INS150_create_(0,IATT_(PATT_(symtab__,4),20),IATT_(self__,4));
            if ((IATT_(PATT_(symtab__,4),100) > 0)) {
               ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1693_)));
            }
            else {
            }
            gl1234_ = PATT_(self__,12);
            cache_dispatch_(gl1234_,1549,gl1235_,INTVAL_(gl1236_));
            if (CFN_(gl1236_)(gl1234_)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1694_)));
            }
            else {
            }
            gl1237_ = PATT_(self__,12);
            cache_dispatch_(gl1237_,418,gl1238_,INTVAL_(gl1239_));
            if (CATT_(PFN_(gl1239_)(gl1237_),4)) {
               gl1240_ = PATT_(self__,12);
               cache_dispatch_(gl1240_,418,gl1241_,INTVAL_(gl1242_));
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1695_)),CLA148_full_name_(PFN_(gl1242_)(gl1240_))),(ptr)(&ls1696_)));
            }
            else {
            }
            break;
         case (23) :
         case (24) :
            if ((IATT_(PATT_(symtab__,4),100) == 0)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1697_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1698_)));
            }
            else {
            }
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            break;
         case (25) :
            if ((IATT_(PATT_(symtab__,4),100) <= 1)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1697_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1699_)));
            }
            else {
            }
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            break;
         case (26) :
            if ((IATT_(PATT_(symtab__,4),100) <= 2)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1697_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1700_)));
            }
            else {
            }
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            break;
         case (27) :
            if ((IATT_(PATT_(symtab__,4),100) <= 3)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1697_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1701_)));
            }
            else {
            }
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            break;
         case (30) :
            if ((IATT_(PATT_(symtab__,4),100) == 0)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1697_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1702_)));
            }
            else {
            }
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1703_)));
            PATT_(self__,12) = (ptr)INS150_create_(0,IATT_(PATT_(symtab__,4),20),IATT_(self__,4));
            break;
         case (32) :
            PATT_(self__,12) = (ptr)GLO68_int_typeob_s_;
            break;
         case (33) :
            gl1243_ = GLO68_curr_feature_;
            gl100_ = TYPE_(gl1243_);
            if ((gl100_ != 156)) {
               ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1704_)));
            }
            else {
            }
            gl1246_ = GLO68_curr_feature_;
            cache_dispatch_(gl1246_,1705,gl1247_,INTVAL_(gl1248_));
            gl101_ = PFN_(gl1248_)(gl1246_);
            if ((gl101_ == 0)) {
               ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1706_)));
               PATT_(self__,12) = (ptr)GLO68_ob_typeob_s_;
            }
            else {
               gl1249_ = GLO68_curr_feature_;
               cache_dispatch_(gl1249_,1705,gl1250_,INTVAL_(gl1251_));
               PATT_(self__,12) = (ptr)PFN_(gl1251_)(gl1249_);
            }
            break;
         case (37) :
            PATT_(self__,12) = (ptr)GLO68_voidtype_s_;
            break;
         default:
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1707_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1708_)));
            PATT_(self__,12) = (ptr)GLO68_ob_typeob_s_;
            ;
      }
   }
   else {
      gl1252_ = PATT_(self__,32);
      gl102_ = TYPE_(gl1252_);
      if ((gl102_ == 156)) {
         num_params__ = (int)0;
         gl1258_ = PATT_(self__,32);
         cache_dispatch_(gl1258_,661,gl1259_,INTVAL_(gl1260_));
         gl1255_ = PFN_(gl1260_)(gl1258_);
         cache_dispatch_(gl1255_,1709,gl1256_,INTVAL_(gl1257_));
         gl103_ = PFN_(gl1257_)(gl1255_);
         if ((gl103_ != 0)) {
            gl1264_ = PATT_(self__,32);
            cache_dispatch_(gl1264_,661,gl1265_,INTVAL_(gl1266_));
            gl1261_ = PFN_(gl1266_)(gl1264_);
            cache_dispatch_(gl1261_,1709,gl1262_,INTVAL_(gl1263_));
            num_params__ = (int)IATT_(PFN_(gl1263_)(gl1261_),16);
         }
         else {
         }
         if ((num_params__ > 0)) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1710_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1711_)));
         }
         else {
         }
         gl1270_ = PATT_(self__,32);
         cache_dispatch_(gl1270_,661,gl1271_,INTVAL_(gl1272_));
         gl1267_ = PFN_(gl1272_)(gl1270_);
         cache_dispatch_(gl1267_,1705,gl1268_,INTVAL_(gl1269_));
         PATT_(self__,12) = (ptr)PFN_(gl1269_)(gl1267_);
      }
      else {
         gl1273_ = PATT_(self__,32);
         cache_dispatch_(gl1273_,661,gl1274_,INTVAL_(gl1275_));
         PATT_(self__,12) = (ptr)PFN_(gl1275_)(gl1273_);
      }
      gl1276_ = PATT_(self__,32);
      switch (TYPE_(gl1276_)) {
         case (156) :
            rout_feat__ = (ptr)PATT_(self__,32);
            CATT_(rout_feat__,11) = (char)1;
            break;
         case (151) :
            const_feat__ = (ptr)PATT_(self__,32);
            CATT_(const_feat__,11) = (char)1;
            break;
         case (153) :
            attr_feat__ = (ptr)PATT_(self__,32);
            CATT_(attr_feat__,11) = (char)1;
            break;
         case (152) :
            shared_feat__ = (ptr)PATT_(self__,32);
            CATT_(shared_feat__,11) = (char)1;
            break;
         default:
            ;
            ;
      }
   }

   ret0__:
   return;
}

ptr ID_103_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int ID_103_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void ID_103_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr ID_103_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void ID_103_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{
   SATHER_STR_(20,40,ls1722_,"(ID_EXPROB_S): Attribute of void object");
   SATHER_STR_(20,26,ls1723_,"(ID_EXPROB_S): Applying \"");
   SATHER_STR_(20,17,ls1724_,"\" on void object");
   SATHER_STR_(20,27,ls1725_,"(ID_EXPROB_S): Void object");
   SATHER_STR_(20,12,ls475_,"ID_EXPROB_S");
   SATHER_STR_(20,23,ls1565_,"Unknown continuation #");

   switch (cont__) {
      case (1) :
         if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1722_)));
         }
         else {
         }
         (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(GLO68_curr_class_inst_,108));
         break;
      case (2) :
         if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1722_)));
         }
         else {
         }
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(GLO68_curr_class_inst_,108) + 4));
         break;
      case (3) :
         if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1722_)));
         }
         else {
         }
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(GLO68_curr_class_inst_,108) + 8));
         break;
      case (4) :
         if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1722_)));
         }
         else {
         }
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(GLO68_curr_class_inst_,108) + 12));
         break;
      case (5) :
         (void)SAT99_i_(SAT99_c_(SAT99_i_(outfile__,IATT_(GLO68_curr_class_inst_,20)),','),IATT_(GLO68_curr_class_inst_,60));
         break;
      case (6) :
         if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1723_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1724_)));
         }
         else {
         }
         (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(GLO68_curr_class_inst_,60));
         break;
      case (7) :
         if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1725_)));
         }
         else {
         }
         break;
      default:
         ERR96_compiler_error_msg_(0,(ptr)(&ls475_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1565_)),cont__));
         ;
   }

   ret0__:
   return;
}

void ID_103_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ID_103_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ID_103_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ID_103_cprint_init_code_(ptr self__, ptr outfile__)
{
   ptr gl1279_;
   static int gl1280_;
   static union dtype_ gl1281_;
   int gl104_;
   ptr gl1282_;
   static int gl1283_;
   static union dtype_ gl1284_;
   int gl105_;
   ptr gl1285_;
   static int gl1286_;
   static union dtype_ gl1287_;

   if ((PATT_(self__,32) != 0)) {
      gl1279_ = PATT_(self__,32);
      gl104_ = TYPE_(gl1279_);
      gl1282_ = PATT_(self__,32);
      gl105_ = TYPE_(gl1282_);
      if (((gl104_ == 151) | (gl105_ == 152))) {
         gl1285_ = PATT_(self__,32);
         cache_dispatch_(gl1285_,724,gl1286_,INTVAL_(gl1287_));
         VFN_(gl1287_)(gl1285_,outfile__);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

char ID_103_valid_init_expr_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,36,ls1690_,"(ID_EXPROB_S): Unknown identifier \"");
   SATHER_STR_(20,33,ls1691_,"\" in constant feature definition");
   ptr gl1288_;
   static int gl1289_;
   static union dtype_ gl1290_;
   int gl106_;
   ptr gl1291_;
   static int gl1292_;
   static union dtype_ gl1293_;
   int gl107_;
   ptr gl1294_;
   static int gl1295_;
   static union dtype_ gl1296_;
   int gl108_;

   if ((PATT_(self__,32) == 0)) {
      switch (IATT_(self__,28)) {
         case (32) :
         case (37) :
         case (31) :
            res__ = (char)1;
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1690_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1691_)));
            ;
      }
   }
   else {
      gl1288_ = PATT_(self__,32);
      gl106_ = TYPE_(gl1288_);
      gl1291_ = PATT_(self__,32);
      gl107_ = TYPE_(gl1291_);
      gl1294_ = PATT_(self__,32);
      gl108_ = TYPE_(gl1294_);
      res__ = (char)(((gl106_ == 151) | (gl107_ == 152)) | (gl108_ == 156));
   }

   ret0__:
   return (res__);
}

char ID_103_assignable_p_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,38,ls1713_,"(ID_EXPROB_S): Identifier referrent \"");
   SATHER_STR_(20,12,ls1714_,"\" not found");
   ptr gl1297_;
   static int gl1298_;
   static union dtype_ gl1299_;
   int gl109_;
   ptr gl1300_;
   static int gl1301_;
   static union dtype_ gl1302_;
   int gl110_;
   ptr gl1303_;
   static int gl1304_;
   static union dtype_ gl1305_;
   int gl111_;
   ptr gl1306_;
   static int gl1307_;
   static union dtype_ gl1308_;
   int gl112_;

   if ((PATT_(self__,32) == 0)) {
      switch (IATT_(self__,28)) {
         case (33) :
            res__ = (char)1;
            break;
         default:
            ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1713_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1714_)));
            res__ = (char)0;
            ;
      }
   }
   else {
      gl1297_ = PATT_(self__,32);
      gl109_ = TYPE_(gl1297_);
      gl1300_ = PATT_(self__,32);
      gl110_ = TYPE_(gl1300_);
      gl1303_ = PATT_(self__,32);
      gl111_ = TYPE_(gl1303_);
      gl1306_ = PATT_(self__,32);
      gl112_ = TYPE_(gl1306_);
      if (((((gl109_ == 154) | (gl110_ == 145)) | (gl111_ == 153)) | (gl112_ == 152))) {
         res__ = (char)1;
      }
      else {
      }
   }

   ret0__:
   return (res__);
}

ptr ID_103_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   ptr gl1309_;
   static int gl1310_;
   static union dtype_ gl1311_;

   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
         IATT_(self__,36) = (int)GLO94_global_key_(0);
         gl1309_ = PATT_(self__,12);
         cache_dispatch_(gl1309_,374,gl1310_,INTVAL_(gl1311_));
         IATT_(self__,40) = (int)IFN_(gl1311_)(gl1309_);
         if ((res__ != 0)) {
            res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(self__,36)),IATT_(self__,40));
         }
         else {
            res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,36)),IATT_(self__,40));
         }
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ID_103_expr_eval_constant_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,2,ls1692_,"0");
   SATHER_STR_(20,36,ls1690_,"(ID_EXPROB_S): Unknown identifier \"");
   SATHER_STR_(20,33,ls1691_,"\" in constant feature definition");
   ptr gl1312_;
   static int gl1313_;
   static union dtype_ gl1314_;
   int gl113_;
   ptr gl1315_;
   static int gl1316_;
   static union dtype_ gl1317_;

   if ((PATT_(self__,32) == 0)) {
      switch (IATT_(self__,28)) {
         case (32) :
            res__ = (ptr)INT106_create_(0,INT15_to_s_(IATT_(GLO68_curr_class_inst_,20)),IATT_(self__,4));
            break;
         case (37) :
            res__ = (ptr)INT106_create_(0,(ptr)(&ls1692_),IATT_(self__,4));
            break;
         case (31) :
            res__ = (ptr)0;
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1690_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1691_)));
            ;
      }
      goto ret0__;
   }
   else {
   }
   gl1312_ = PATT_(self__,32);
   gl113_ = TYPE_(gl1312_);
   if ((gl113_ == 151)) {
      gl1315_ = PATT_(self__,32);
      cache_dispatch_(gl1315_,804,gl1316_,INTVAL_(gl1317_));
      res__ = (ptr)PFN_(gl1317_)(gl1315_);
   }
   else {
   }

   ret0__:
   return (res__);
}

void ID_103_get_ext_strs_(ptr self__)
{


   ret0__:
   return;
}

char ID_103_to_be_pre_evaluated_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char ID_103_access_value_only_(ptr self__)
{
   char res__ = S_char_VOID_;
   ptr gl1318_;
   static int gl1319_;
   static union dtype_ gl1320_;
   int gl114_;

   if ((PATT_(self__,32) != 0)) {
      gl1318_ = PATT_(self__,32);
      gl114_ = TYPE_(gl1318_);
      res__ = (char)(gl114_ != 156);
   }
   else {
   }

   ret0__:
   return (res__);
}

void ID_103_fob_error_(ptr self__, ptr op_name__, ptr cls_name__)
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

void ID_103_cprint_pre_code_(ptr self__, ptr outfile__)
{
   ptr gl1321_;
   static int gl1322_;
   static union dtype_ gl1323_;
   int gl115_;

   if (GLO94_check_is_on_(0)) {
      if ((PATT_(self__,32) == 0)) {
         switch (IATT_(self__,28)) {
            case (23) :
            case (24) :
            case (25) :
            case (26) :
            case (27) :
               PRI187_cprint_void_tst_(0,outfile__,7,self__);
               break;
            default:
               ;
               ;
         }
      }
      else {
         gl1321_ = PATT_(self__,32);
         gl115_ = TYPE_(gl1321_);
         if ((gl115_ == 153)) {
            PRI187_cprint_void_tst_(0,outfile__,7,self__);
         }
         else {
         }
      }
      if ((IATT_(self__,24) != 0)) {
         GLO94_cprint_curr_exp_code_(0,self__,IATT_(self__,36),outfile__);
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

void ID_103_cprint_act_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,12,ls475_,"ID_EXPROB_S");
   SATHER_STR_(20,26,ls1727_,"Invalid printing count = ");
   SATHER_STR_(20,4,ls1728_," > ");
   SATHER_STR_(20,54,ls1730_,"(ID_EXPROB_S): \"extend\" needs at least one parameter\n");
   SATHER_STR_(20,6,ls1732_,"res__");
   SATHER_STR_(20,40,ls1733_,"(ID_EXPROB_S): Reference to void object");
   SATHER_STR_(20,28,ls1734_,"(ID_EXPROB_S): Identifier \"");
   SATHER_STR_(20,24,ls1735_,"\" has unknown referrent");
   SATHER_STR_(20,45,ls1736_,"(ID_EXPROB_S): Routine call with void object");
   ptr gl1324_;
   static int gl1325_;
   static union dtype_ gl1326_;
   ptr gl1327_;
   static int gl1328_;
   static union dtype_ gl1329_;
   ptr gl1330_;
   static int gl1331_;
   static union dtype_ gl1332_;
   ptr gl1333_;
   static int gl1334_;
   static union dtype_ gl1335_;

   if ((IATT_(self__,44) > GLO68_g_tag_)) {
      if ((IATT_(self__,36) > 0)) {
         GLO94_cprint_ctemp_name_(0,IATT_(self__,36),outfile__);
         goto ret0__;
      }
      else {
         ERR96_compiler_error_msg_(0,(ptr)(&ls475_),STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1727_)),IATT_(self__,44)),(ptr)(&ls1728_)),GLO68_g_tag_));
      }
   }
   else {
   }
   IATT_(self__,44) = (int)(IATT_(self__,44) + 1);
   if ((PATT_(self__,32) == 0)) {
      switch (IATT_(self__,28)) {
         case (28) :
            PRI187_cprint_copy_(0,outfile__,6,self__);
            break;
         case (30) :
            ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1730_)));
            break;
         case (31) :
            PRI187_cprint_new_(0,outfile__,0,5,self__);
            break;
         case (33) :
            (void)SAT99_s_(outfile__,(ptr)(&ls1732_));
            break;
         case (34) :
            if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
               ERR96_format_warning_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1733_)));
            }
            else {
            }
            break;
         case (32) :
            (void)SAT99_i_(outfile__,IATT_(GLO68_curr_class_inst_,20));
            break;
         case (37) :
            gl1324_ = PATT_(self__,12);
            cache_dispatch_(gl1324_,1591,gl1325_,INTVAL_(gl1326_));
            VFN_(gl1326_)(gl1324_,outfile__);
            break;
         case (23) :
         case (24) :
            PRI187_cprint_int_attr_access_(0,outfile__,1,self__);
            break;
         case (25) :
            PRI187_cprint_int_attr_access_(0,outfile__,2,self__);
            break;
         case (26) :
            PRI187_cprint_int_attr_access_(0,outfile__,3,self__);
            break;
         case (27) :
            PRI187_cprint_int_attr_access_(0,outfile__,4,self__);
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1734_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1735_)));
            ;
      }
   }
   else {
      ID_103_cprint_cast_code_(self__,outfile__);
      gl1327_ = PATT_(self__,32);
      switch (TYPE_(gl1327_)) {
         case (153) :
         case (151) :
         case (152) :
         case (154) :
         case (145) :
            gl1330_ = PATT_(self__,32);
            cache_dispatch_(gl1330_,809,gl1331_,INTVAL_(gl1332_));
            VFN_(gl1332_)(gl1330_,outfile__);
            break;
         case (156) :
            gl1333_ = PATT_(self__,32);
            cache_dispatch_(gl1333_,808,gl1334_,INTVAL_(gl1335_));
            VFN_(gl1335_)(gl1333_,outfile__);
            (void)SAT99_c_(outfile__,'(');
            if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
               ERR96_format_warning_msg_(0,IATT_(self__,4),STR20_s_(STR20_create_(0),(ptr)(&ls1736_)));
            }
            else {
            }
            (void)SAT99_c_(outfile__,')');
            break;
         default:
            ERR96_format_error_exit_(0,IATT_(self__,4),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1734_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,28))),(ptr)(&ls1735_)));
            ;
      }
   }

   ret0__:
   return;
}

void ID_103_cprint_cast_code_(ptr self__, ptr outfile__)
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

