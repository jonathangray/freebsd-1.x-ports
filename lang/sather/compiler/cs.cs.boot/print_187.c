/* print_187.c : Sather class: PRINT_C_CODE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr SAT99_indent_(ptr self__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr SAT99_i_(ptr self__, int in__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern void STR109_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
extern void EXP113_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
extern void EXP117_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
extern void SEM188_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
extern void GLO94_cprint_ctemp_name_(ptr self__, int intval__, ptr outfile__);
extern char GLO94_check_is_on_(ptr self__);
#include "macros_.h"



void PRI187_cprint_sather_str_mi_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_sather_str1_mi_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_rt_typechk_(ptr self__, ptr outfile__, int rt_type__, int temp_name__, int lineno__);
void PRI187_cprint_void_tst_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_arr_bound_tst_(ptr self__, int ith_index__, int name1__, int name2__, ptr outfile__, int lineno__);
void PRI187_cprint_char_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
void PRI187_cprint_int_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
void PRI187_cprint_float_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
void PRI187_cprint_double_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
void PRI187_cprint_ptr_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__);
void PRI187_cprint_type_access_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_cache_dispatch_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_ob_cache_dispatch_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_array_dispatch_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_atomic_check_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_copy_(ptr self__, ptr outfile__, int cont__, ptr exp__);
void PRI187_cprint_new_(ptr self__, ptr outfile__, int dim__, int cont__, ptr exp__);
void PRI187_cprint_restore_exec_info_(ptr self__, ptr outfile__);
void PRI187_cprint_update_exec_info1_(ptr self__, ptr outfile__, ptr filename__, ptr classname__, ptr routinename__);
ptr PRI187_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_PRI187[];

void PRI187_cprint_sather_str_mi_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,13,ls2735_,"SATHER_STR_(");

   (void)SAT99_indent_(outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls2735_));
   STR109_cont_cprint_code_(exp__,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_sather_str1_mi_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,14,ls2736_,"SATHER_STR1_(");

   (void)SAT99_indent_(outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls2736_));
   STR109_cont_cprint_code_(exp__,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_rt_typechk_(ptr self__, ptr outfile__, int rt_type__, int temp_name__, int lineno__)
{
   SATHER_STR_(20,11,ls2737_,"TYPECHK1_(");
   SATHER_STR_(20,11,ls2738_,"TYPECHK2_(");
   SATHER_STR_(20,3,ls1051_,")\n");

   (void)SAT99_indent_(outfile__);
   if ((rt_type__ > 0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2737_));
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls2738_));
   }
   GLO94_cprint_ctemp_name_(0,temp_name__,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_c_(SAT99_i_(SAT99_c_(outfile__,','),rt_type__),','),lineno__),(ptr)(&ls1051_)),1);

   ret0__:
   return;
}

void PRI187_cprint_void_tst_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,11,ls2739_,"VOID_TST_(");
   SATHER_STR_(20,4,ls2740_,");\n");
   ptr gl4585_;
   static int gl4586_;
   static union dtype_ gl4587_;
   ptr gl4588_;
   static int gl4589_;
   static union dtype_ gl4590_;
   int gl474_;

   (void)SAT99_indent_(outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls2739_));
   gl4585_ = exp__;
   cache_dispatch_(gl4585_,806,gl4586_,INTVAL_(gl4587_));
   VFN_(gl4587_)(gl4585_,outfile__,cont__);
   gl4588_ = exp__;
   cache_dispatch_(gl4588_,298,gl4589_,INTVAL_(gl4590_));
   gl474_ = IATT_(gl4588_,INTVAL_(gl4590_));
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_c_(outfile__,','),gl474_),(ptr)(&ls2740_)),1);

   ret0__:
   return;
}

void PRI187_cprint_arr_bound_tst_(ptr self__, int ith_index__, int name1__, int name2__, ptr outfile__, int lineno__)
{
   SATHER_STR_(20,16,ls2744_,"ARR_BOUND_TST_(");
   SATHER_STR_(20,4,ls2740_,");\n");

   (void)SAT99_indent_(outfile__);
   (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls2744_)),ith_index__),',');
   GLO94_cprint_ctemp_name_(0,name1__,outfile__);
   (void)SAT99_c_(outfile__,',');
   GLO94_cprint_ctemp_name_(0,name2__,outfile__);
   (void)SAT99_inc_ln_(SAT99_s_(SAT99_i_(SAT99_c_(outfile__,','),lineno__),(ptr)(&ls2740_)),1);

   ret0__:
   return;
}

void PRI187_cprint_char_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__)
{
   SATHER_STR_(20,7,ls2745_,"CATT_(");
   ptr gl4591_;
   static int gl4592_;
   static union dtype_ gl4593_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2745_));
   gl4591_ = obj__;
   cache_dispatch_(gl4591_,806,gl4592_,INTVAL_(gl4593_));
   VFN_(gl4593_)(gl4591_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_int_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__)
{
   SATHER_STR_(20,7,ls2746_,"IATT_(");
   ptr gl4594_;
   static int gl4595_;
   static union dtype_ gl4596_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2746_));
   gl4594_ = obj__;
   cache_dispatch_(gl4594_,806,gl4595_,INTVAL_(gl4596_));
   VFN_(gl4596_)(gl4594_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_float_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__)
{
   SATHER_STR_(20,7,ls2747_,"FATT_(");
   ptr gl4597_;
   static int gl4598_;
   static union dtype_ gl4599_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2747_));
   gl4597_ = obj__;
   cache_dispatch_(gl4597_,806,gl4598_,INTVAL_(gl4599_));
   VFN_(gl4599_)(gl4597_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_double_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__)
{
   SATHER_STR_(20,7,ls2748_,"DATT_(");
   ptr gl4600_;
   static int gl4601_;
   static union dtype_ gl4602_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2748_));
   gl4600_ = obj__;
   cache_dispatch_(gl4600_,806,gl4601_,INTVAL_(gl4602_));
   VFN_(gl4602_)(gl4600_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_ptr_attr_access_(ptr self__, ptr outfile__, int cont__, ptr obj__)
{
   SATHER_STR_(20,7,ls2749_,"PATT_(");
   ptr gl4603_;
   static int gl4604_;
   static union dtype_ gl4605_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2749_));
   gl4603_ = obj__;
   cache_dispatch_(gl4603_,806,gl4604_,INTVAL_(gl4605_));
   VFN_(gl4605_)(gl4603_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_type_access_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,7,ls2750_,"TYPE_(");
   ptr gl4606_;
   static int gl4607_;
   static union dtype_ gl4608_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2750_));
   gl4606_ = exp__;
   cache_dispatch_(gl4606_,806,gl4607_,INTVAL_(gl4608_));
   VFN_(gl4608_)(gl4606_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_cache_dispatch_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,22,ls2751_,"safe_cache_dispatch_(");
   SATHER_STR_(20,17,ls2752_,"cache_dispatch_(");
   SATHER_STR_(20,4,ls2740_,");\n");

   (void)SAT99_indent_(outfile__);
   if (GLO94_check_is_on_(0)) {
      (void)SAT99_s_(outfile__,(ptr)(&ls2751_));
      EXP113_cont_cprint_code_(exp__,outfile__,cont__);
      (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(exp__,80));
   }
   else {
      (void)SAT99_s_(outfile__,(ptr)(&ls2752_));
      EXP113_cont_cprint_code_(exp__,outfile__,cont__);
   }
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2740_)),1);

   ret0__:
   return;
}

void PRI187_cprint_ob_cache_dispatch_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,20,ls2753_,"ob_cache_dispatch_(");
   SATHER_STR_(20,4,ls2740_,");\n");

   (void)SAT99_indent_(outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls2753_));
   EXP113_cont_cprint_code_(exp__,outfile__,cont__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2740_)),1);

   ret0__:
   return;
}

void PRI187_cprint_array_dispatch_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,17,ls2754_,"array_dispatch_(");
   SATHER_STR_(20,4,ls2740_,");\n");
   ptr gl4609_;
   static int gl4610_;
   static union dtype_ gl4611_;

   (void)SAT99_indent_(outfile__);
   (void)SAT99_s_(outfile__,(ptr)(&ls2754_));
   gl4609_ = exp__;
   cache_dispatch_(gl4609_,806,gl4610_,INTVAL_(gl4611_));
   VFN_(gl4611_)(gl4609_,outfile__,cont__);
   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2740_)),1);

   ret0__:
   return;
}

void PRI187_cprint_atomic_check_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,11,ls2755_,"atomic_p_(");
   ptr gl4612_;
   static int gl4613_;
   static union dtype_ gl4614_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2755_));
   gl4612_ = exp__;
   cache_dispatch_(gl4612_,806,gl4613_,INTVAL_(gl4614_));
   VFN_(gl4614_)(gl4612_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_copy_(ptr self__, ptr outfile__, int cont__, ptr exp__)
{
   SATHER_STR_(20,7,ls2756_,"copy_(");
   ptr gl4615_;
   static int gl4616_;
   static union dtype_ gl4617_;

   (void)SAT99_s_(outfile__,(ptr)(&ls2756_));
   gl4615_ = exp__;
   cache_dispatch_(gl4615_,806,gl4616_,INTVAL_(gl4617_));
   VFN_(gl4617_)(gl4615_,outfile__,cont__);
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_new_(ptr self__, ptr outfile__, int dim__, int cont__, ptr exp__)
{
   SATHER_STR_(20,19,ls2758_,"safe_generic_new_(");
   SATHER_STR_(20,14,ls2759_,"generic_new_(");
   SATHER_STR_(20,11,ls2760_,"safe_new_(");
   SATHER_STR_(20,6,ls2761_,"new_(");
   SATHER_STR_(20,12,ls2762_,"safe_new1_(");
   SATHER_STR_(20,7,ls2763_,"new1_(");
   SATHER_STR_(20,12,ls2764_,"safe_new2_(");
   SATHER_STR_(20,7,ls2765_,"new2_(");
   SATHER_STR_(20,12,ls2766_,"safe_new3_(");
   SATHER_STR_(20,7,ls2767_,"new3_(");
   SATHER_STR_(20,12,ls2768_,"safe_new4_(");
   SATHER_STR_(20,7,ls2769_,"new4_(");
   ptr gl4618_;
   static int gl4619_;
   static union dtype_ gl4620_;
   ptr gl4621_;
   static int gl4622_;
   static union dtype_ gl4623_;
   int gl475_;
   ptr gl4624_;
   static int gl4625_;
   static union dtype_ gl4626_;
   ptr gl4627_;
   static int gl4628_;
   static union dtype_ gl4629_;
   ptr gl4630_;
   static int gl4631_;
   static union dtype_ gl4632_;
   int gl476_;
   ptr gl4633_;
   static int gl4634_;
   static union dtype_ gl4635_;
   ptr gl4636_;
   static int gl4637_;
   static union dtype_ gl4638_;
   ptr gl4639_;
   static int gl4640_;
   static union dtype_ gl4641_;
   int gl477_;
   ptr gl4642_;
   static int gl4643_;
   static union dtype_ gl4644_;
   ptr gl4645_;
   static int gl4646_;
   static union dtype_ gl4647_;
   ptr gl4648_;
   static int gl4649_;
   static union dtype_ gl4650_;
   int gl478_;
   ptr gl4651_;
   static int gl4652_;
   static union dtype_ gl4653_;
   ptr gl4654_;
   static int gl4655_;
   static union dtype_ gl4656_;
   ptr gl4657_;
   static int gl4658_;
   static union dtype_ gl4659_;
   int gl479_;
   ptr gl4660_;
   static int gl4661_;
   static union dtype_ gl4662_;
   ptr gl4663_;
   static int gl4664_;
   static union dtype_ gl4665_;
   ptr gl4666_;
   static int gl4667_;
   static union dtype_ gl4668_;
   int gl480_;
   ptr gl4669_;
   static int gl4670_;
   static union dtype_ gl4671_;

   switch (dim__) {
      case (-1) :
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2758_));
            gl4618_ = exp__;
            cache_dispatch_(gl4618_,806,gl4619_,INTVAL_(gl4620_));
            VFN_(gl4620_)(gl4618_,outfile__,cont__);
            gl4621_ = exp__;
            cache_dispatch_(gl4621_,298,gl4622_,INTVAL_(gl4623_));
            gl475_ = IATT_(gl4621_,INTVAL_(gl4623_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),gl475_);
         }
         else {
            (void)SAT99_s_(outfile__,(ptr)(&ls2759_));
            gl4624_ = exp__;
            cache_dispatch_(gl4624_,806,gl4625_,INTVAL_(gl4626_));
            VFN_(gl4626_)(gl4624_,outfile__,cont__);
         }
         break;
      case (0) :
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2760_));
            gl4627_ = exp__;
            cache_dispatch_(gl4627_,806,gl4628_,INTVAL_(gl4629_));
            VFN_(gl4629_)(gl4627_,outfile__,cont__);
            gl4630_ = exp__;
            cache_dispatch_(gl4630_,298,gl4631_,INTVAL_(gl4632_));
            gl476_ = IATT_(gl4630_,INTVAL_(gl4632_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),gl476_);
         }
         else {
            (void)SAT99_s_(outfile__,(ptr)(&ls2761_));
            gl4633_ = exp__;
            cache_dispatch_(gl4633_,806,gl4634_,INTVAL_(gl4635_));
            VFN_(gl4635_)(gl4633_,outfile__,cont__);
         }
         break;
      case (1) :
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2762_));
            gl4636_ = exp__;
            cache_dispatch_(gl4636_,806,gl4637_,INTVAL_(gl4638_));
            VFN_(gl4638_)(gl4636_,outfile__,cont__);
            gl4639_ = exp__;
            cache_dispatch_(gl4639_,298,gl4640_,INTVAL_(gl4641_));
            gl477_ = IATT_(gl4639_,INTVAL_(gl4641_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),gl477_);
         }
         else {
            (void)SAT99_s_(outfile__,(ptr)(&ls2763_));
            gl4642_ = exp__;
            cache_dispatch_(gl4642_,806,gl4643_,INTVAL_(gl4644_));
            VFN_(gl4644_)(gl4642_,outfile__,cont__);
         }
         break;
      case (2) :
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2764_));
            gl4645_ = exp__;
            cache_dispatch_(gl4645_,806,gl4646_,INTVAL_(gl4647_));
            VFN_(gl4647_)(gl4645_,outfile__,cont__);
            gl4648_ = exp__;
            cache_dispatch_(gl4648_,298,gl4649_,INTVAL_(gl4650_));
            gl478_ = IATT_(gl4648_,INTVAL_(gl4650_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),gl478_);
         }
         else {
            (void)SAT99_s_(outfile__,(ptr)(&ls2765_));
            gl4651_ = exp__;
            cache_dispatch_(gl4651_,806,gl4652_,INTVAL_(gl4653_));
            VFN_(gl4653_)(gl4651_,outfile__,cont__);
         }
         break;
      case (3) :
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2766_));
            gl4654_ = exp__;
            cache_dispatch_(gl4654_,806,gl4655_,INTVAL_(gl4656_));
            VFN_(gl4656_)(gl4654_,outfile__,cont__);
            gl4657_ = exp__;
            cache_dispatch_(gl4657_,298,gl4658_,INTVAL_(gl4659_));
            gl479_ = IATT_(gl4657_,INTVAL_(gl4659_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),gl479_);
         }
         else {
            (void)SAT99_s_(outfile__,(ptr)(&ls2767_));
            gl4660_ = exp__;
            cache_dispatch_(gl4660_,806,gl4661_,INTVAL_(gl4662_));
            VFN_(gl4662_)(gl4660_,outfile__,cont__);
         }
         break;
      case (4) :
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_s_(outfile__,(ptr)(&ls2768_));
            gl4663_ = exp__;
            cache_dispatch_(gl4663_,806,gl4664_,INTVAL_(gl4665_));
            VFN_(gl4665_)(gl4663_,outfile__,cont__);
            gl4666_ = exp__;
            cache_dispatch_(gl4666_,298,gl4667_,INTVAL_(gl4668_));
            gl480_ = IATT_(gl4666_,INTVAL_(gl4668_));
            (void)SAT99_i_(SAT99_c_(outfile__,','),gl480_);
         }
         else {
            (void)SAT99_s_(outfile__,(ptr)(&ls2769_));
            gl4669_ = exp__;
            cache_dispatch_(gl4669_,806,gl4670_,INTVAL_(gl4671_));
            VFN_(gl4671_)(gl4669_,outfile__,cont__);
         }
         break;
      default:
         ;
         ;
   }
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void PRI187_cprint_restore_exec_info_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,21,ls2770_,"RESTORE_EXEC_INFO_;\n");

   (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls2770_)),1);

   ret0__:
   return;
}

void PRI187_cprint_update_exec_info1_(ptr self__, ptr outfile__, ptr filename__, ptr classname__, ptr routinename__)
{
   SATHER_STR_(20,20,ls2772_,"UPDATE_EXEC_INFO_(\"");
   SATHER_STR_(20,5,ls2773_,"\", \"");
   SATHER_STR_(20,5,ls722_,"\");\n");

   (void)SAT99_inc_ln_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(SAT99_s_(outfile__,(ptr)(&ls2772_)),filename__),(ptr)(&ls2773_)),classname__),(ptr)(&ls2773_)),routinename__),(ptr)(&ls722_)),1);

   ret0__:
   return;
}

ptr PRI187_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

