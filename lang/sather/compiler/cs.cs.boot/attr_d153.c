/* attr_d153.c : Sather class: ATTR_DECL_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int INT15_rshift_(int self__, int i__);
extern int INT15_lshift_(int self__, int i__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr TYP149_dup_(ptr self__);
extern ptr STR20_c_(ptr self__, char ch__);
extern ptr TYP149_sather_code_(ptr self__);
extern void TYP149_resolve_predef_types_(ptr self__, int index__);
extern ptr TYP149_inst_cls_(ptr self__);
extern ptr STR20_i_(ptr self__, int in__);
extern int TYP149_inst_ind_(ptr self__);
extern int TYP149_ctype_(ptr self__);
extern char TYP149_is_dispatched_(ptr self__);
extern char TYP149_nonptr_p_(ptr self__);
extern char TYP149_conforms_to_(ptr self__, ptr tp__);
extern ptr CLA148_full_name_(ptr self__);
extern ptr CLA148_get_feature_(ptr self__, int index__);
extern void PRI187_cprint_ptr_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
extern void PRI187_cprint_int_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
extern void PRI187_cprint_char_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
extern void PRI187_cprint_float_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
extern void PRI187_cprint_double_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
extern /*shared*/ ptr GLO68_str_table_;
extern char STR69_reserved_name_p_(ptr self__, int nm__);
extern ptr STR69_at_index_(ptr self__, int i__);
extern /*shared*/ ptr GLO68_curr_class_inst_;
extern /*shared*/ ptr GLO68_curr_feature_;
extern /*shared*/ ptr GLO68_class_inst_;
extern /*shared*/ char COM82_verbose_code_;
extern ptr CLA93_at_index_(ptr self__, int i__);
extern char GLO94_cprint_ref_to_self_(ptr self__, ptr outfile__);
extern char GLO94_handle_class_p_(ptr self__, ptr co__);
extern void ERR96_error_msg_(ptr self__, ptr s__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr SAT99_i_(ptr self__, int in__);
extern void ERR96_format_warning_msg_(ptr self__, int ln__, ptr s__);
extern /*constant*/ int RES97_UNDEFINE_ici_;
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern ptr EXP117_dup_(ptr self__);
extern ptr EXP117_sather_code_(ptr self__);
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int ATT153_c_float_size_ = 4;
/*constant*/ int ATT153_c_double_size_ = 8;
/*constant*/ ptr ATT153_c_ptr_name_ = (ptr)(&gs1215_);
/*constant*/ ptr ATT153_c_char_name_ = (ptr)(&gs1216_);
/*constant*/ ptr ATT153_c_int_name_ = (ptr)(&gs1217_);
/*constant*/ ptr ATT153_c_float_name_ = (ptr)(&gs1218_);
/*constant*/ ptr ATT153_c_double_name_ = (ptr)(&gs1219_);
ptr ATT153_initialize_(ptr self__, ptr initarg__);
/*constant*/ int ATT153_print_indent_ = 2;
ptr ATT153_create_(ptr self__, int nm__, ptr t__, char priv__, ptr c_def__, int ln__);
void ATT153_out_of_line_(ptr self__, ptr fn__);
ptr ATT153_dup_(ptr self__);
void ATT153_put_kwdname_(ptr self__, int nm__);
ptr ATT153_sather_code_(ptr self__);
void ATT153_resolve_predef_types_(ptr self__, int index__);
void ATT153_semant_(ptr self__, ptr symtab__);
ptr ATT153_typeof_(ptr self__);
int ATT153_get_offset_(ptr self__);
void ATT153_cprint_offset_(ptr self__, ptr outfile__);
ptr ATT153_get_constval_(ptr self__);
void ATT153_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void ATT153_cprint_cname_(ptr self__, ptr outfile__);
void ATT153_cprint_extern_(ptr self__, ptr outfile__);
void ATT153_cprint_access_value_(ptr self__, ptr outfile__);
void ATT153_cprint_init_code_(ptr self__, ptr outfile__);
/*constant*/ int ATT153_c_ptr_ = 1;
/*constant*/ int ATT153_c_char_ = 2;
/*constant*/ int ATT153_c_int_ = 3;
/*constant*/ int ATT153_c_float_ = 4;
/*constant*/ int ATT153_c_double_ = 5;
/*constant*/ int ATT153_c_void_ = 6;
/*constant*/ int ATT153_c_ptr_size_ = 4;
/*constant*/ int ATT153_c_char_size_ = 1;
int ATT153_class_inst_ind_(ptr self__);
int ATT153_featob_s_name_(ptr self__);
void ATT153_mark_private_(ptr self__);
void ATT153_mark_abstract_(ptr self__);
void ATT153_mark_spec_(ptr self__);
void ATT153_mark_shared_(ptr self__);
void ATT153_mark_readonly_(ptr self__);
char ATT153_undefined_p_(ptr self__);
int ATT153_compute_own_offset_(ptr self__, int nextloc__);
void ATT153_eval_constant_(ptr self__);
ptr ATT153_rettype_(ptr self__);
void ATT153_remember_local_(ptr self__, ptr lvar__);
void ATT153_semant_prologue_(ptr self__);
void ATT153_do_gen_temps_(ptr self__);
void ATT153_gen_goto_tags_(ptr self__);
void ATT153_validate_dispatches_and_get_ext_strs_(ptr self__);
char ATT153_compatible_des_feat_(ptr self__, ptr feat__, char lval_p__);
void ATT153_update_used_in_dispatch_(ptr self__);
void ATT153_consistent_defs_(ptr self__, int nm__, char lval_p__);
void ATT153_cprint_decln_(ptr self__, ptr outfile__);
void ATT153_cprint_routine_(ptr self__, ptr outfile__);
void ATT153_cprint_store_dispval_(ptr self__, ptr outfile__);
int ATT153_declob_s_name_(ptr self__);
void ATT153_cprint_code_(ptr self__, ptr outfile__);
/*constant*/ int ATT153_c_int_size_ = 4;
/*constant*/ int ATT153_cont1_ = 1;
extern int attr_ent_ATT153[];

ptr ATT153_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr ATT153_create_(ptr self__, int nm__, ptr t__, char priv__, ptr c_def__, int ln__)
{
   ptr res__ = 0;
   SATHER_STR_(20,37,ls1557_,"(ROUT_FEATOB): Error in redefining \"");
   SATHER_STR_(20,3,ls632_,"\"\n");

   res__ = (ptr)new_(153,0);
   if (STR69_reserved_name_p_(GLO68_str_table_,nm__)) {
      ERR96_error_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1557_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls632_)));
   }
   else {
   }
   IATT_(res__,24) = (int)nm__;
   PATT_(res__,40) = (ptr)t__;
   CATT_(res__,5) = (char)priv__;
   PATT_(res__,28) = (ptr)c_def__;
   PATT_(res__,32) = (ptr)GLO68_curr_class_inst_;
   IATT_(res__,16) = (int)ln__;

   ret0__:
   return (res__);
}

void ATT153_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr ATT153_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl4259_;
   static int gl4260_;
   static union dtype_ gl4261_;
   ptr gl437_;
   ptr gl4262_;
   static int gl4263_;
   static union dtype_ gl4264_;

   gl4259_ = PATT_(self__,40);
   cache_dispatch_(gl4259_,471,gl4260_,INTVAL_(gl4261_));
   gl437_ = PFN_(gl4261_)(gl4259_);
   res__ = (ptr)ATT153_create_(self__,IATT_(self__,24),gl437_,CATT_(self__,5),PATT_(self__,28),IATT_(self__,16));
   if ((PATT_(self__,44) != 0)) {
      gl4262_ = PATT_(self__,44);
      cache_dispatch_(gl4262_,471,gl4263_,INTVAL_(gl4264_));
      PATT_(res__,44) = (ptr)PFN_(gl4264_)(gl4262_);
   }
   else {
   }

   ret0__:
   return (res__);
}

void ATT153_put_kwdname_(ptr self__, int nm__)
{
   ptr gl4265_;
   static int gl4266_;
   static union dtype_ gl4267_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl4265_ = x__;
   cache_dispatch_(gl4265_,796,gl4266_,INTVAL_(gl4267_));
   IATT_(gl4265_,INTVAL_(gl4267_)) = (int)nm__;

   ret0__:
   return;
}

ptr ATT153_sather_code_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,3,ls1558_,":=");
   ptr gl4268_;
   static int gl4269_;
   static union dtype_ gl4270_;
   ptr gl438_;
   ptr gl4271_;
   static int gl4272_;
   static union dtype_ gl4273_;
   ptr gl439_;

   gl4268_ = PATT_(self__,40);
   cache_dispatch_(gl4268_,801,gl4269_,INTVAL_(gl4270_));
   gl438_ = PFN_(gl4270_)(gl4268_);
   res__ = (ptr)STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),STR69_at_index_(GLO68_str_table_,IATT_(self__,24))),':'),gl438_);
   if ((PATT_(self__,44) == 0)) {
      res__ = (ptr)STR20_c_(res__,';');
   }
   else {
      gl4271_ = PATT_(self__,44);
      cache_dispatch_(gl4271_,801,gl4272_,INTVAL_(gl4273_));
      gl439_ = PFN_(gl4273_)(gl4271_);
      res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(res__,(ptr)(&ls1558_)),gl439_),';');
   }

   ret0__:
   return (res__);
}

void ATT153_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl4274_;
   static int gl4275_;
   static union dtype_ gl4276_;

   gl4274_ = PATT_(self__,40);
   cache_dispatch_(gl4274_,522,gl4275_,INTVAL_(gl4276_));
   VFN_(gl4276_)(gl4274_,index__);

   ret0__:
   return;
}

void ATT153_semant_(ptr self__, ptr symtab__)
{
   ptr gl4277_;
   static int gl4278_;
   static union dtype_ gl4279_;
   ptr    co__ = 0;

   gl4277_ = PATT_(self__,40);
   cache_dispatch_(gl4277_,418,gl4278_,INTVAL_(gl4279_));
   co__ = (ptr)PFN_(gl4279_)(gl4277_);
   if (CATT_(co__,14)) {
      CATT_(GLO68_curr_class_inst_,12) = (char)1;
   }
   else {
   }

   ret0__:
   return;
}

ptr ATT153_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,40);

   ret0__:
   return (res__);
}

int ATT153_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,52);

   ret0__:
   return (res__);
}

void ATT153_cprint_offset_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,5,ls1563_,"_OF_");

   if (COM82_verbose_code_) {
      (void)SAT99_c_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,PATT_(PATT_(self__,32),84)),(ptr)(&ls1563_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,24))),'_');
   }
   else {
      (void)SAT99_i_(outfile__,IATT_(self__,52));
   }

   ret0__:
   return;
}

ptr ATT153_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void ATT153_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{
   SATHER_STR_(20,19,ls448_,"ATTR_DECL_FEATOB_S");
   SATHER_STR_(20,23,ls1565_,"Unknown continuation #");
   SATHER_STR_(20,23,ls1566_," in ATTR_DECL_FEATOB_S");
   SATHER_STR_(20,34,ls1567_,"(ATTR_DECL_FEATOB_S): Void object");

   if ((cont__ != 1)) {
      ERR96_compiler_error_msg_(0,(ptr)(&ls448_),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1565_)),cont__),(ptr)(&ls1566_)));
   }
   else {
      if ((! GLO94_cprint_ref_to_self_(0,outfile__))) {
         ERR96_format_warning_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls1567_)));
      }
      else {
      }
      (void)SAT99_c_(outfile__,',');
      ATT153_cprint_offset_(self__,outfile__);
   }

   ret0__:
   return;
}

void ATT153_cprint_cname_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,3,ls1551_,"__");

   (void)SAT99_s_(SAT99_s_(outfile__,STR69_at_index_(GLO68_str_table_,IATT_(self__,24))),(ptr)(&ls1551_));

   ret0__:
   return;
}

void ATT153_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ATT153_cprint_access_value_(ptr self__, ptr outfile__)
{

   switch (IATT_(self__,48)) {
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

   ret0__:
   return;
}

void ATT153_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

int ATT153_class_inst_ind_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

int ATT153_featob_s_name_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,24);

   ret0__:
   return (res__);
}

void ATT153_mark_private_(ptr self__)
{

   CATT_(self__,5) = (char)1;

   ret0__:
   return;
}

void ATT153_mark_abstract_(ptr self__)
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

void ATT153_mark_spec_(ptr self__)
{

   CATT_(self__,6) = (char)1;

   ret0__:
   return;
}

void ATT153_mark_shared_(ptr self__)
{

   CATT_(self__,7) = (char)1;

   ret0__:
   return;
}

void ATT153_mark_readonly_(ptr self__)
{

   CATT_(self__,8) = (char)1;

   ret0__:
   return;
}

char ATT153_undefined_p_(ptr self__)
{
   char res__ = S_char_VOID_;
   ptr gl4280_;
   static int gl4281_;
   static union dtype_ gl4282_;
   int gl440_;

   gl4280_ = PATT_(self__,40);
   cache_dispatch_(gl4280_,1547,gl4281_,INTVAL_(gl4282_));
   gl440_ = IFN_(gl4282_)(gl4280_);
   res__ = (char)(gl440_ == RES97_UNDEFINE_ici_);

   ret0__:
   return (res__);
}

int ATT153_compute_own_offset_(ptr self__, int nextloc__)
{
   int res__ = S_int_VOID_;
   ptr gl4283_;
   static int gl4284_;
   static union dtype_ gl4285_;

   gl4283_ = PATT_(self__,40);
   cache_dispatch_(gl4283_,374,gl4284_,INTVAL_(gl4285_));
   IATT_(self__,48) = (int)IFN_(gl4285_)(gl4283_);
   switch (IATT_(self__,48)) {
      case (1) :
      case (3) :
      case (4) :
         IATT_(self__,52) = (int)INT15_lshift_(INT15_rshift_((nextloc__ + 3),2),2);
         res__ = (int)(4 + IATT_(self__,52));
         break;
      case (2) :
         IATT_(self__,52) = (int)nextloc__;
         res__ = (int)(1 + IATT_(self__,52));
         break;
      case (5) :
         IATT_(self__,52) = (int)INT15_lshift_(INT15_rshift_((nextloc__ + 7),3),3);
         res__ = (int)(8 + IATT_(self__,52));
         break;
      default:
         res__ = (int)nextloc__;
         ;
   }

   ret0__:
   return (res__);
}

void ATT153_eval_constant_(ptr self__)
{


   ret0__:
   return;
}

ptr ATT153_rettype_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void ATT153_remember_local_(ptr self__, ptr lvar__)
{


   ret0__:
   return;
}

void ATT153_semant_prologue_(ptr self__)
{

   GLO68_curr_feature_ = (ptr)self__;

   ret0__:
   return;
}

void ATT153_do_gen_temps_(ptr self__)
{


   ret0__:
   return;
}

void ATT153_gen_goto_tags_(ptr self__)
{


   ret0__:
   return;
}

void ATT153_validate_dispatches_and_get_ext_strs_(ptr self__)
{
   SATHER_STR_(20,35,ls1550_,"(DECLOB_S): Dispatch on basic type");
   ptr gl4286_;
   static int gl4287_;
   static union dtype_ gl4288_;
   ptr gl4289_;
   static int gl4290_;
   static union dtype_ gl4291_;

   gl4286_ = PATT_(self__,40);
   cache_dispatch_(gl4286_,1511,gl4287_,INTVAL_(gl4288_));
   if (CFN_(gl4288_)(gl4286_)) {
      gl4289_ = PATT_(self__,40);
      cache_dispatch_(gl4289_,1549,gl4290_,INTVAL_(gl4291_));
      if (CFN_(gl4291_)(gl4289_)) {
         ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls1550_)));
      }
      else {
      }
   }
   else {
   }

   ret0__:
   return;
}

char ATT153_compatible_des_feat_(ptr self__, ptr feat__, char lval_p__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,68,ls1561_,"(ATTR_DECL_FEATOB_S): Inconsistent use of \"private\" for attribute \"");
   SATHER_STR_(20,7,ls1562_,"\" in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr gl4292_;
   static int gl4293_;
   static union dtype_ gl4294_;
   ptr gl4295_;
   static int gl4296_;
   static union dtype_ gl4297_;

   if (lval_p__) {
      gl4292_ = PATT_(self__,40);
      cache_dispatch_(gl4292_,903,gl4293_,INTVAL_(gl4294_));
      res__ = (char)CFN_(gl4294_)(gl4292_,PATT_(feat__,40));
   }
   else {
      gl4295_ = PATT_(feat__,40);
      cache_dispatch_(gl4295_,903,gl4296_,INTVAL_(gl4297_));
      res__ = (char)CFN_(gl4297_)(gl4295_,PATT_(self__,40));
   }
   if ((CATT_(self__,5) != CATT_(feat__,5))) {
      ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1561_)),STR69_at_index_(GLO68_str_table_,IATT_(self__,24))),(ptr)(&ls1562_)),CLA148_full_name_(PATT_(self__,32))),(ptr)(&ls1481_)),CLA148_full_name_(PATT_(feat__,32))),(ptr)(&ls785_)));
   }
   else {
   }

   ret0__:
   return (res__);
}

void ATT153_update_used_in_dispatch_(ptr self__)
{

   CATT_(self__,10) = (char)1;
   CATT_(self__,11) = (char)1;

   ret0__:
   return;
}

void ATT153_consistent_defs_(ptr self__, int nm__, char lval_p__)
{
   SATHER_STR_(20,22,ls2124_,"(FEATOB_S): Feature \"");
   SATHER_STR_(20,49,ls2125_,"\" has incompatible definitions for dispatch in \"");
   SATHER_STR_(20,8,ls1481_,"\" and \"");
   SATHER_STR_(20,2,ls785_,"\"");
   SATHER_STR_(20,52,ls2126_,"\" are different kinds of features for dispatch in \"");
   SATHER_STR_(20,42,ls2127_,"\" is missing for dispatch in descendent \"");
   SATHER_STR_(20,13,ls2128_,"\" of class \"");
   ptr gl4298_;
   static int gl4299_;
   static union dtype_ gl4300_;
   ptr gl4301_;
   int gl441_;
   ptr    co__ = 0;
   int    i__ = S_int_VOID_;
   int    psz__ = S_int_VOID_;
   ptr    descendents__ = 0;
   int    index__ = S_int_VOID_;
   ptr    co1__ = 0;
   ptr    referrent__ = 0;
   ptr    feat__ = 0;

   if (((IATT_(self__,36) == 1) & (lval_p__ == 0))) {
      goto ret0__;
   }
   else {
      if (((IATT_(self__,36) == 2) & (lval_p__ == 1))) {
         goto ret0__;
      }
      else {
         if ((IATT_(self__,36) == 3)) {
            goto ret0__;
         }
         else {
         }
      }
   }
   if ((IATT_(self__,36) == 0)) {
      if (lval_p__) {
         IATT_(self__,36) = (int)2;
      }
      else {
         IATT_(self__,36) = (int)1;
      }
   }
   else {
      if ((IATT_(self__,36) == 1)) {
         if (lval_p__) {
            IATT_(self__,36) = (int)3;
         }
         else {
         }
      }
      else {
         if ((IATT_(self__,36) == 2)) {
            if ((! lval_p__)) {
               IATT_(self__,36) = (int)3;
            }
            else {
            }
         }
         else {
         }
      }
   }
   co__ = (ptr)PATT_(self__,32);
   i__ = (int)0;
   psz__ = S_int_VOID_;
   descendents__ = (ptr)PATT_(co__,76);
   if ((descendents__ != 0)) {
      psz__ = (int)IATT_(descendents__,12);
   }
   else {
   }
   ATT153_update_used_in_dispatch_(self__);
   while (1) {
      if ((i__ >= psz__)) {
         goto goto_tag_4302_;
      }
      else {
      }
      index__ = (int)IATT_(descendents__, 16 + ((i__) << 2));
      if ((index__ > 0)) {
         co1__ = (ptr)CLA93_at_index_(GLO68_class_inst_,index__);
         if (GLO94_handle_class_p_(0,co1__)) {
            referrent__ = (ptr)CLA148_get_feature_(co1__,nm__);
            if ((referrent__ != 0)) {
               gl4298_ = referrent__;
               gl441_ = TYPE_(gl4298_);
               gl4301_ = self__;
               if ((gl441_ == (153))) {
                  feat__ = (ptr)referrent__;
                  ATT153_update_used_in_dispatch_(feat__);
                  if ((! ATT153_compatible_des_feat_(self__,feat__,lval_p__))) {
                     ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2125_)),CLA148_full_name_(co__)),(ptr)(&ls1481_)),CLA148_full_name_(co1__)),(ptr)(&ls785_)));
                  }
                  else {
                  }
               }
               else {
                  ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2126_)),CLA148_full_name_(co__)),(ptr)(&ls1481_)),CLA148_full_name_(co1__)),(ptr)(&ls785_)));
               }
            }
            else {
               ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2124_)),STR69_at_index_(GLO68_str_table_,nm__)),(ptr)(&ls2127_)),CLA148_full_name_(co1__)),(ptr)(&ls2128_)),CLA148_full_name_(co__)),(ptr)(&ls785_)));
            }
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_4302_: ;

   ret0__:
   return;
}

void ATT153_cprint_decln_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void ATT153_cprint_routine_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,38,ls2131_,"(FEATOB_S): Error in printing routine");

   ERR96_format_error_msg_(0,IATT_(self__,16),STR20_s_(STR20_create_(0),(ptr)(&ls2131_)));

   ret0__:
   return;
}

void ATT153_cprint_store_dispval_(ptr self__, ptr outfile__)
{

   ATT153_cprint_offset_(self__,outfile__);

   ret0__:
   return;
}

int ATT153_declob_s_name_(ptr self__)
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(self__,24);

   ret0__:
   return (res__);
}

void ATT153_cprint_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

