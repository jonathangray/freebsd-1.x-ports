/* aref_e111.c : Sather class: AREF_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern /*shared*/ ptr GLO68_array_typeob_s_;
extern /*shared*/ ptr GLO68_ob_typeob_s_;
extern /*shared*/ int GLO68_g_tag_;
extern /*constant*/ int RES71_self_ind_;
extern int GLO94_global_key_();
extern GLO94_cprint_ctemp_name_();
extern int ERR96_out_of_line_err_info_();
extern ERR96_format_error_msg_();
extern /*constant*/ int RES97_FOB_ici_;
extern ptr SAT99_s_();
extern char GLO94_check_is_on_();
extern ptr SAT99_i_();
extern ptr SAT99_c_();
extern ptr ID_103_create_();
extern ERR96_compiler_error_msg_();
extern ptr LIS98_create_();
extern ptr LIS98_push_();
extern ptr LIS98_append_();
extern ptr SAT99_indent_();
extern ptr SAT99_inc_ln_();
extern GLO94_cprint_curr_exp_code_();
extern char EXP117_access_value_only_();
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
extern char EXP117_to_be_pre_evaluated_();
extern EXP117_out_of_line_();
extern ptr EXP117_dup_();
extern ptr EXP117_sather_code_();
extern LST120_out_of_line_();
extern ptr LST120_dup_();
extern ptr LST120_sather_code_();
extern EXP117_resolve_predef_types_();
extern LST120_resolve_predef_types_();
extern EXP117_semant_();
extern LST120_semant_();
extern ptr CLA148_full_name_();
extern ptr TYP149_inst_cls_();
extern int INS150_my_basic_type_();
extern char TYP149_is_dispatched_();
extern char TYP149_int_type_p_();
extern int TYP149_ctype_();
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ int C_T168_c_ptr_size_;
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ int C_T168_c_int_size_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ int C_T168_c_char_size_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ int C_T168_c_float_size_;
extern /*constant*/ int C_T168_c_double_;
extern /*constant*/ int C_T168_c_double_size_;
extern /*constant*/ ptr C_T168_c_ptr_name_;
extern /*constant*/ ptr C_T168_c_char_name_;
extern /*constant*/ ptr C_T168_c_int_name_;
extern /*constant*/ ptr C_T168_c_float_name_;
extern /*constant*/ ptr C_T168_c_double_name_;
extern PRI187_cprint_int_attr_access_();
extern PRI187_cprint_void_tst_();
extern PRI187_cprint_array_dispatch_();
extern PRI187_cprint_arr_bound_tst_();
extern PRI187_cprint_ptr_attr_access_();
extern PRI187_cprint_char_attr_access_();
extern PRI187_cprint_float_attr_access_();
extern PRI187_cprint_double_attr_access_();
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int ARE111_print_indent_ = 2;
ptr ARE111_create_();
ARE111_out_of_line_();
ptr ARE111_dup_();
ARE111_put_kwdname_();
ptr ARE111_sather_code_();
ptr ARE111_initialize_();
ARE111_resolve_predef_types_();
ARE111_semant_();
ptr ARE111_typeof_();
int ARE111_get_offset_();
ARE111_cprint_offset_();
ptr ARE111_get_constval_();
ARE111_cont_cprint_code_();
ARE111_cprint_cname_();
ARE111_cprint_extern_();
ARE111_cprint_access_value_();
ARE111_cprint_init_code_();
char ARE111_valid_init_expr_();
char ARE111_assignable_p_();
ptr ARE111_gen_temps_();
ptr ARE111_expr_eval_constant_();
ARE111_get_ext_strs_();
char ARE111_to_be_pre_evaluated_();
char ARE111_access_value_only_();
ARE111_fob_error_();
ARE111_cprint_pre_code_();
ARE111_cprint_act_code_();
ARE111_cprint_cast_code_();
ARE111_cprint_ith_index_pre_code_();
ARE111_cprint_ith_index_act_code_();
/*constant*/ int ARE111_cont1_ = 1;
/*constant*/ int ARE111_cont6_ = 6;
/*constant*/ int ARE111_cont11_ = 11;
/*constant*/ int ARE111_cont12_ = 12;
/*constant*/ int ARE111_cont17_ = 17;
/*constant*/ int ARE111_cont18_ = 18;
/*constant*/ int ARE111_cont19_ = 19;
/*constant*/ int ARE111_cont24_ = 24;
/*constant*/ int ARE111_cont25_ = 25;
/*constant*/ int ARE111_cont26_ = 26;
/*constant*/ int ARE111_cont27_ = 27;
/*constant*/ int ARE111_cont32_ = 32;
/*constant*/ int ARE111_cont37_ = 37;
/*constant*/ int ARE111_cont38_ = 38;
/*constant*/ int ARE111_cont43_ = 43;
/*constant*/ int ARE111_cont44_ = 44;
/*constant*/ int ARE111_cont45_ = 45;
/*constant*/ int ARE111_cont50_ = 50;
/*constant*/ int ARE111_cont51_ = 51;
/*constant*/ int ARE111_cont52_ = 52;
/*constant*/ int ARE111_cont53_ = 53;
/*constant*/ int ARE111_cont54_ = 54;
/*constant*/ int ARE111_cont55_ = 55;
/*constant*/ int ARE111_cont56_ = 56;
/*constant*/ int ARE111_cont57_ = 57;
/*constant*/ int ARE111_cont58_ = 58;
/*constant*/ int ARE111_cont59_ = 59;
/*constant*/ int ARE111_cont60_ = 60;
/*constant*/ int ARE111_cont61_ = 61;
/*constant*/ int ARE111_cont62_ = 62;
ARE111_cprint_index_();
extern int attr_ent_ARE111[];

ptr ARE111_create_(self__,arr__,inds__,ln__)
ptr self__;
ptr arr__;
ptr inds__;
int ln__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(111,0);
   if ((arr__ == 0)) {
      PATT_(res__,28) = (ptr)ID_103_create_(0,34,ln__);
   }
   else {
      PATT_(res__,28) = (ptr)arr__;
   }
   PATT_(res__,32) = (ptr)inds__;
   IATT_(res__,76) = (int)ln__;
   PATT_(res__,72) = (ptr)new1_(163,4,1);

   ret0__:
   return (res__);
}

ARE111_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{
   ptr gl1579_;
   static int gl1580_;
   static union dtype_ gl1581_;

   IATT_(self__,76) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,76));
   gl1579_ = PATT_(self__,28);
   cache_dispatch_(gl1579_,518,gl1580_,INTVAL_(gl1581_));
   VFN_(gl1581_)(gl1579_,fn__);
   LST120_out_of_line_(PATT_(self__,32),fn__);

   ret0__:
   return;
}

ptr ARE111_dup_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1582_;
   static int gl1583_;
   static union dtype_ gl1584_;
   ptr gl137_;

   if ((PATT_(self__,28) != 0)) {
      gl1582_ = PATT_(self__,28);
      cache_dispatch_(gl1582_,471,gl1583_,INTVAL_(gl1584_));
      gl137_ = PFN_(gl1584_)(gl1582_);
      res__ = (ptr)ARE111_create_(self__,gl137_,LST120_dup_(PATT_(self__,32)),IATT_(self__,76));
   }
   else {
      res__ = (ptr)ARE111_create_(self__,0,LST120_dup_(PATT_(self__,32)),IATT_(self__,76));
   }

   ret0__:
   return (res__);
}

ARE111_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl1585_;
   static int gl1586_;
   static union dtype_ gl1587_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1585_ = x__;
   cache_dispatch_(gl1585_,796,gl1586_,INTVAL_(gl1587_));
   IATT_(gl1585_,INTVAL_(gl1587_)) = (int)nm__;

   ret0__:
   return;
}

ptr ARE111_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1588_;
   static int gl1589_;
   static union dtype_ gl1590_;
   ptr gl138_;

   gl1588_ = PATT_(self__,28);
   cache_dispatch_(gl1588_,801,gl1589_,INTVAL_(gl1590_));
   gl138_ = PFN_(gl1590_)(gl1588_);
   res__ = (ptr)STR20_c_(STR20_s_(STR20_c_(STR20_s_(STR20_create_(0),gl138_),'['),LST120_sather_code_(PATT_(self__,32))),']');

   ret0__:
   return (res__);
}

ptr ARE111_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ARE111_resolve_predef_types_(self__,index__)
ptr self__;
int index__;
{
   ptr gl1591_;
   static int gl1592_;
   static union dtype_ gl1593_;

   if ((PATT_(self__,28) != 0)) {
      gl1591_ = PATT_(self__,28);
      cache_dispatch_(gl1591_,522,gl1592_,INTVAL_(gl1593_));
      VFN_(gl1593_)(gl1591_,index__);
   }
   else {
   }
   LST120_resolve_predef_types_(PATT_(self__,32),index__);

   ret0__:
   return;
}

ARE111_semant_(self__,symtab__)
ptr self__;
ptr symtab__;
{
   SATHER_STR_(20,48,ls1801_,"(AREF_EXPROB_S): Referencing unknown array type");
   SATHER_STR_(20,58,ls1804_,"(AREF_EXPROB_S): Invalid array access on foreign object (");
   SATHER_STR_(20,25,ls1806_,"(AREF_EXPROB_S): Index #");
   SATHER_STR_(20,20,ls1807_," has no return type");
   SATHER_STR_(20,74,ls1808_,"(AREF_EXPROB_S): Dimension of class is not the same as number of indices\n");
   SATHER_STR_(20,36,ls1809_,"(AREF_EXPROB_S): Array reference #(");
   SATHER_STR_(20,24,ls1810_,") has non-integer index");
   ptr gl1594_;
   static int gl1595_;
   static union dtype_ gl1596_;
   ptr gl1597_;
   static int gl1598_;
   static union dtype_ gl1599_;
   ptr gl1600_;
   static int gl1601_;
   static union dtype_ gl1602_;
   ptr gl1603_;
   static int gl1604_;
   static union dtype_ gl1605_;
   ptr gl1606_;
   static int gl1607_;
   static union dtype_ gl1608_;
   ptr gl139_;
   ptr gl1609_;
   static int gl1610_;
   static union dtype_ gl1611_;
   ptr gl1612_;
   static int gl1613_;
   static union dtype_ gl1614_;
   ptr gl1615_;
   static int gl1616_;
   static union dtype_ gl1617_;
   ptr gl1618_;
   static int gl1619_;
   static union dtype_ gl1620_;
   char gl141_;
   ptr gl1621_;
   static int gl1622_;
   static union dtype_ gl1623_;
   ptr gl1624_;
   static int gl1625_;
   static union dtype_ gl1626_;
   ptr gl1627_;
   static int gl1628_;
   static union dtype_ gl1629_;
   char gl144_;
   ptr gl1630_;
   static int gl1631_;
   static union dtype_ gl1632_;
   ptr    atype__ = 0;
   ptr    inst_tp__ = 0;
   int    isz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    i_140_ = S_int_VOID_;
   int    i_142_ = S_int_VOID_;
   int    i_143_ = S_int_VOID_;

   atype__ = S_ptr_VOID_;
   gl1594_ = PATT_(self__,28);
   cache_dispatch_(gl1594_,588,gl1595_,INTVAL_(gl1596_));
   VFN_(gl1596_)(gl1594_,symtab__);
   gl1597_ = PATT_(self__,28);
   cache_dispatch_(gl1597_,1577,gl1598_,INTVAL_(gl1599_));
   atype__ = (ptr)PATT_(gl1597_,INTVAL_(gl1599_));
   if ((atype__ == 0)) {
      ERR96_format_error_msg_(0,IATT_(self__,76),STR20_s_(STR20_create_(0),(ptr)(&ls1801_)));
      gl1600_ = PATT_(self__,28);
      cache_dispatch_(gl1600_,1577,gl1601_,INTVAL_(gl1602_));
      PATT_(gl1600_,INTVAL_(gl1602_)) = (ptr)GLO68_array_typeob_s_;
      atype__ = (ptr)GLO68_array_typeob_s_;
   }
   else {
   }
   inst_tp__ = (ptr)atype__;
   if ((INS150_my_basic_type_(inst_tp__) == RES97_FOB_ici_)) {
      gl1603_ = atype__;
      cache_dispatch_(gl1603_,418,gl1604_,INTVAL_(gl1605_));
      ERR96_format_error_msg_(0,IATT_(self__,76),STR20_c_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1804_)),CLA148_full_name_(PFN_(gl1605_)(gl1603_))),')'));
   }
   else {
   }
   LST120_semant_(PATT_(self__,32),symtab__);
   isz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      isz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   i__ = S_int_VOID_;
   while (1) {
      if ((i__ >= isz__)) {
         goto goto_tag_1633_;
      }
      else {
      }
      gl1606_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
      cache_dispatch_(gl1606_,1577,gl1607_,INTVAL_(gl1608_));
      gl139_ = PATT_(gl1606_,INTVAL_(gl1608_));
      if ((gl139_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,76),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1806_)),(i__ + 1)),(ptr)(&ls1807_)));
         gl1609_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
         cache_dispatch_(gl1609_,1577,gl1610_,INTVAL_(gl1611_));
         PATT_(gl1609_,INTVAL_(gl1611_)) = (ptr)GLO68_ob_typeob_s_;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1633_: ;
   gl1612_ = atype__;
   cache_dispatch_(gl1612_,418,gl1613_,INTVAL_(gl1614_));
   if ((isz__ != IATT_(PFN_(gl1614_)(gl1612_),100))) {
      ERR96_format_error_msg_(0,IATT_(self__,76),STR20_s_(STR20_create_(0),(ptr)(&ls1808_)));
   }
   else {
   }
   gl1615_ = atype__;
   cache_dispatch_(gl1615_,1511,gl1616_,INTVAL_(gl1617_));
   if (CFN_(gl1617_)(gl1615_)) {
      i_140_ = S_int_VOID_;
      while (1) {
         if ((i_140_ >= isz__)) {
            goto goto_tag_1634_;
         }
         else {
         }
         gl1618_ = PATT_(PATT_(self__,32), 28 + ((i_140_) << 2));
         cache_dispatch_(gl1618_,1679,gl1619_,INTVAL_(gl1620_));
         gl141_ = CFN_(gl1620_)(gl1618_);
         if ((! gl141_)) {
            IATT_(PATT_(self__,72), 8 + ((i_140_) << 2)) = (int)GLO94_global_key_(0);
            CATT_(self__,4) = (char)1;
         }
         else {
         }
         i_140_ = (int)(i_140_ + 1);
      }
   goto_tag_1634_: ;
   }
   else {
      i_142_ = S_int_VOID_;
      while (1) {
         if ((i_142_ >= isz__)) {
            goto goto_tag_1635_;
         }
         else {
         }
         gl1621_ = PATT_(PATT_(self__,32), 28 + ((i_142_) << 2));
         cache_dispatch_(gl1621_,1678,gl1622_,INTVAL_(gl1623_));
         if (CFN_(gl1623_)(gl1621_)) {
            IATT_(PATT_(self__,72), 8 + ((i_142_) << 2)) = (int)GLO94_global_key_(0);
            CATT_(self__,4) = (char)1;
         }
         else {
         }
         i_142_ = (int)(i_142_ + 1);
      }
   goto_tag_1635_: ;
   }
   i_143_ = (int)0;
   while (1) {
      if ((i_143_ >= isz__)) {
         goto goto_tag_1636_;
      }
      else {
      }
      gl1627_ = PATT_(PATT_(self__,32), 28 + ((i_143_) << 2));
      cache_dispatch_(gl1627_,1577,gl1628_,INTVAL_(gl1629_));
      gl1624_ = PATT_(gl1627_,INTVAL_(gl1629_));
      cache_dispatch_(gl1624_,1778,gl1625_,INTVAL_(gl1626_));
      gl144_ = CFN_(gl1626_)(gl1624_);
      if ((! gl144_)) {
         ERR96_format_error_msg_(0,IATT_(self__,76),STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1809_)),(i_143_ + 1)),(ptr)(&ls1810_)));
      }
      else {
      }
      i_143_ = (int)(i_143_ + 1);
   }
goto_tag_1636_: ;
   gl1630_ = atype__;
   cache_dispatch_(gl1630_,418,gl1631_,INTVAL_(gl1632_));
   PATT_(self__,12) = (ptr)PATT_(PFN_(gl1632_)(gl1630_),124);

   ret0__:
   return;
}

ptr ARE111_typeof_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,12);

   ret0__:
   return (res__);
}

int ARE111_get_offset_(self__)
ptr self__;
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

ARE111_cprint_offset_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ptr ARE111_get_constval_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ARE111_cont_cprint_code_(self__,outfile__,cont__)
ptr self__;
ptr outfile__;
int cont__;
{
   SATHER_STR_(20,11,ls1847_,", INTVAL_(");
   SATHER_STR_(20,19,ls1848_,") + sizeof(int) + ");
   SATHER_STR_(20,3,ls624_,", ");
   SATHER_STR_(20,4,ls1763_," + ");
   SATHER_STR_(20,9,ls1850_,") + 8 + ");
   SATHER_STR_(20,10,ls1851_,") + 12 + ");
   SATHER_STR_(20,10,ls1852_,") + 16 + ");
   SATHER_STR_(20,10,ls1853_,",INTVAL_(");
   SATHER_STR_(20,2,ls1278_,")");
   SATHER_STR_(20,4,ls1854_,")+4");
   SATHER_STR_(20,4,ls1855_,")+8");
   SATHER_STR_(20,5,ls1856_,")+12");
   SATHER_STR_(20,14,ls1658_,"AREF_EXPROB_S");
   SATHER_STR_(20,23,ls1565_,"Unknown continuation #");
   ptr gl1637_;
   static int gl1638_;
   static union dtype_ gl1639_;
   ptr gl1640_;
   static int gl1641_;
   static union dtype_ gl1642_;
   ptr gl1643_;
   static int gl1644_;
   static union dtype_ gl1645_;
   ptr gl1646_;
   static int gl1647_;
   static union dtype_ gl1648_;
   ptr gl1649_;
   static int gl1650_;
   static union dtype_ gl1651_;
   ptr gl1652_;
   static int gl1653_;
   static union dtype_ gl1654_;
   ptr gl1655_;
   static int gl1656_;
   static union dtype_ gl1657_;
   ptr gl1658_;
   static int gl1659_;
   static union dtype_ gl1660_;
   ptr gl1661_;
   static int gl1662_;
   static union dtype_ gl1663_;
   ptr gl1664_;
   static int gl1665_;
   static union dtype_ gl1666_;
   ptr gl1667_;
   static int gl1668_;
   static union dtype_ gl1669_;
   ptr gl1670_;
   static int gl1671_;
   static union dtype_ gl1672_;
   ptr gl1673_;
   static int gl1674_;
   static union dtype_ gl1675_;
   ptr gl1676_;
   static int gl1677_;
   static union dtype_ gl1678_;
   ptr gl1679_;
   static int gl1680_;
   static union dtype_ gl1681_;
   ptr gl1682_;
   static int gl1683_;
   static union dtype_ gl1684_;
   ptr gl1685_;
   static int gl1686_;
   static union dtype_ gl1687_;
   ptr gl1688_;
   static int gl1689_;
   static union dtype_ gl1690_;
   ptr gl1691_;
   static int gl1692_;
   static union dtype_ gl1693_;
   ptr gl1694_;
   static int gl1695_;
   static union dtype_ gl1696_;
   ptr gl1697_;
   static int gl1698_;
   static union dtype_ gl1699_;
   ptr gl1700_;
   static int gl1701_;
   static union dtype_ gl1702_;
   ptr gl1703_;
   static int gl1704_;
   static union dtype_ gl1705_;
   ptr gl1706_;
   static int gl1707_;
   static union dtype_ gl1708_;
   ptr gl1709_;
   static int gl1710_;
   static union dtype_ gl1711_;

   switch (cont__) {
      case (1) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1847_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1848_));
         gl1637_ = PATT_(self__,12);
         cache_dispatch_(gl1637_,374,gl1638_,INTVAL_(gl1639_));
         switch (IFN_(gl1639_)(gl1637_)) {
            case (1) :
               ARE111_cprint_index_(self__,0,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,0,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,0,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,0,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,0,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (6) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,11,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         gl1640_ = PATT_(self__,12);
         cache_dispatch_(gl1640_,374,gl1641_,INTVAL_(gl1642_));
         switch (IFN_(gl1642_)(gl1640_)) {
            case (1) :
               ARE111_cprint_index_(self__,1,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,1,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,1,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,1,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,1,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (11) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1847_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1850_));
         ARE111_cprint_index_(self__,0,4,outfile__);
         break;
      case (12) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,17,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         gl1643_ = PATT_(self__,12);
         cache_dispatch_(gl1643_,374,gl1644_,INTVAL_(gl1645_));
         switch (IFN_(gl1645_)(gl1643_)) {
            case (1) :
               ARE111_cprint_index_(self__,2,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,2,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,2,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,2,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,2,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (17) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,18,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,1,4,outfile__);
         break;
      case (18) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1847_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1851_));
         ARE111_cprint_index_(self__,0,4,outfile__);
         break;
      case (19) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,24,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         gl1646_ = PATT_(self__,12);
         cache_dispatch_(gl1646_,374,gl1647_,INTVAL_(gl1648_));
         switch (IFN_(gl1648_)(gl1646_)) {
            case (1) :
               ARE111_cprint_index_(self__,3,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,3,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,3,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,3,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,3,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (24) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,25,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,2,4,outfile__);
         break;
      case (25) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,26,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,1,4,outfile__);
         break;
      case (26) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1847_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1852_));
         ARE111_cprint_index_(self__,0,4,outfile__);
         break;
      case (27) :
         if (GLO94_check_is_on_(0)) {
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         }
         else {
            gl1649_ = PATT_(self__,28);
            cache_dispatch_(gl1649_,965,gl1650_,INTVAL_(gl1651_));
            VFN_(gl1651_)(gl1649_,outfile__);
         }
         gl1655_ = PATT_(self__,28);
         cache_dispatch_(gl1655_,1577,gl1656_,INTVAL_(gl1657_));
         gl1652_ = PATT_(gl1655_,INTVAL_(gl1657_));
         cache_dispatch_(gl1652_,418,gl1653_,INTVAL_(gl1654_));
         (void)SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls624_)),(IATT_(PFN_(gl1654_)(gl1652_),108) + 4)),(ptr)(&ls1763_));
         gl1658_ = PATT_(self__,12);
         cache_dispatch_(gl1658_,374,gl1659_,INTVAL_(gl1660_));
         switch (IFN_(gl1660_)(gl1658_)) {
            case (1) :
               ARE111_cprint_index_(self__,0,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,0,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,0,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,0,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,0,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (32) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,37,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         gl1661_ = PATT_(self__,12);
         cache_dispatch_(gl1661_,374,gl1662_,INTVAL_(gl1663_));
         switch (IFN_(gl1663_)(gl1661_)) {
            case (1) :
               ARE111_cprint_index_(self__,1,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,1,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,1,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,1,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,1,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (37) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1667_ = PATT_(self__,28);
         cache_dispatch_(gl1667_,1577,gl1668_,INTVAL_(gl1669_));
         gl1664_ = PATT_(gl1667_,INTVAL_(gl1669_));
         cache_dispatch_(gl1664_,418,gl1665_,INTVAL_(gl1666_));
         (void)SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls624_)),(IATT_(PFN_(gl1666_)(gl1664_),108) + 8)),(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,0,4,outfile__);
         break;
      case (38) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,43,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         gl1670_ = PATT_(self__,12);
         cache_dispatch_(gl1670_,374,gl1671_,INTVAL_(gl1672_));
         switch (IFN_(gl1672_)(gl1670_)) {
            case (1) :
               ARE111_cprint_index_(self__,2,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,2,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,2,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,2,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,2,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (43) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,44,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,1,4,outfile__);
         break;
      case (44) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1676_ = PATT_(self__,28);
         cache_dispatch_(gl1676_,1577,gl1677_,INTVAL_(gl1678_));
         gl1673_ = PATT_(gl1676_,INTVAL_(gl1678_));
         cache_dispatch_(gl1673_,418,gl1674_,INTVAL_(gl1675_));
         (void)SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls624_)),(IATT_(PFN_(gl1675_)(gl1673_),108) + 12)),(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,0,4,outfile__);
         break;
      case (45) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,50,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         gl1679_ = PATT_(self__,12);
         cache_dispatch_(gl1679_,374,gl1680_,INTVAL_(gl1681_));
         switch (IFN_(gl1681_)(gl1679_)) {
            case (1) :
               ARE111_cprint_index_(self__,3,4,outfile__);
               break;
            case (3) :
               ARE111_cprint_index_(self__,3,4,outfile__);
               break;
            case (2) :
               ARE111_cprint_index_(self__,3,1,outfile__);
               break;
            case (4) :
               ARE111_cprint_index_(self__,3,4,outfile__);
               break;
            case (5) :
               ARE111_cprint_index_(self__,3,8,outfile__);
               break;
            default:
               ;
               ;
         }
         break;
      case (50) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,51,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,2,4,outfile__);
         break;
      case (51) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls624_));
         PRI187_cprint_int_attr_access_(0,outfile__,52,self__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,1,4,outfile__);
         break;
      case (52) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1685_ = PATT_(self__,28);
         cache_dispatch_(gl1685_,1577,gl1686_,INTVAL_(gl1687_));
         gl1682_ = PATT_(gl1685_,INTVAL_(gl1687_));
         cache_dispatch_(gl1682_,418,gl1683_,INTVAL_(gl1684_));
         (void)SAT99_s_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls624_)),(IATT_(PFN_(gl1684_)(gl1682_),108) + 16)),(ptr)(&ls1763_));
         ARE111_cprint_index_(self__,0,4,outfile__);
         break;
      case (53) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_c_(outfile__,',');
         GLO94_cprint_ctemp_name_(0,IATT_(self__,48),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
         break;
      case (54) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         break;
      case (55) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1278_));
         break;
      case (56) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1854_));
         break;
      case (57) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1855_));
         break;
      case (58) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1853_));
         GLO94_cprint_ctemp_name_(0,IATT_(self__,56),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1856_));
         break;
      case (59) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1691_ = PATT_(self__,28);
         cache_dispatch_(gl1691_,1577,gl1692_,INTVAL_(gl1693_));
         gl1688_ = PATT_(gl1691_,INTVAL_(gl1693_));
         cache_dispatch_(gl1688_,418,gl1689_,INTVAL_(gl1690_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),IATT_(PFN_(gl1690_)(gl1688_),108));
         break;
      case (60) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1697_ = PATT_(self__,28);
         cache_dispatch_(gl1697_,1577,gl1698_,INTVAL_(gl1699_));
         gl1694_ = PATT_(gl1697_,INTVAL_(gl1699_));
         cache_dispatch_(gl1694_,418,gl1695_,INTVAL_(gl1696_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(PFN_(gl1696_)(gl1694_),108) + 4));
         break;
      case (61) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1703_ = PATT_(self__,28);
         cache_dispatch_(gl1703_,1577,gl1704_,INTVAL_(gl1705_));
         gl1700_ = PATT_(gl1703_,INTVAL_(gl1705_));
         cache_dispatch_(gl1700_,418,gl1701_,INTVAL_(gl1702_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(PFN_(gl1702_)(gl1700_),108) + 8));
         break;
      case (62) :
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         gl1709_ = PATT_(self__,28);
         cache_dispatch_(gl1709_,1577,gl1710_,INTVAL_(gl1711_));
         gl1706_ = PATT_(gl1709_,INTVAL_(gl1711_));
         cache_dispatch_(gl1706_,418,gl1707_,INTVAL_(gl1708_));
         (void)SAT99_i_(SAT99_c_(outfile__,','),(IATT_(PFN_(gl1708_)(gl1706_),108) + 12));
         break;
      default:
         ERR96_compiler_error_msg_(0,(ptr)(&ls1658_),STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1565_)),cont__));
         ;
   }

   ret0__:
   return;
}

ARE111_cprint_cname_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ARE111_cprint_extern_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ARE111_cprint_access_value_(self__,outfile__)
ptr self__;
ptr outfile__;
{


   ret0__:
   return;
}

ARE111_cprint_init_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   ptr gl1712_;
   static int gl1713_;
   static union dtype_ gl1714_;

   gl1712_ = PATT_(self__,28);
   cache_dispatch_(gl1712_,724,gl1713_,INTVAL_(gl1714_));
   VFN_(gl1714_)(gl1712_,outfile__);
   LST120_cprint_init_code_(PATT_(self__,32),outfile__);

   ret0__:
   return;
}

char ARE111_valid_init_expr_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl1715_;
   static int gl1716_;
   static union dtype_ gl1717_;
   char gl145_;

   gl1715_ = PATT_(self__,28);
   cache_dispatch_(gl1715_,1580,gl1716_,INTVAL_(gl1717_));
   gl145_ = CFN_(gl1717_)(gl1715_);
   res__ = (char)(gl145_ & LST120_valid_init_expr_(PATT_(self__,32)));

   ret0__:
   return (res__);
}

char ARE111_assignable_p_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

ptr ARE111_gen_temps_(self__)
ptr self__;
{
   ptr res__ = 0;
   ptr gl1718_;
   static int gl1719_;
   static union dtype_ gl1720_;
   ptr gl1721_;
   static int gl1722_;
   static union dtype_ gl1723_;
   ptr gl1724_;
   static int gl1725_;
   static union dtype_ gl1726_;
   ptr gl1727_;
   static int gl1728_;
   static union dtype_ gl1729_;
   ptr gl1730_;
   static int gl1731_;
   static union dtype_ gl1732_;
   ptr gl1733_;
   static int gl1734_;
   static union dtype_ gl1735_;
   ptr gl1736_;
   static int gl1737_;
   static union dtype_ gl1738_;
   ptr gl1739_;
   static int gl1740_;
   static union dtype_ gl1741_;
   ptr gl1742_;
   static int gl1743_;
   static union dtype_ gl1744_;
   ptr gl1745_;
   static int gl1746_;
   static union dtype_ gl1747_;
   ptr gl146_;
   ptr gl1748_;
   static int gl1749_;
   static union dtype_ gl1750_;
   int    num_temp_names__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   int    tmp_ind__ = S_int_VOID_;
   int    i_147_ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   if (((IATT_(self__,40) < 0) | (PATT_(self__,12) == 0))) {
   }
   else {
      if ((IATT_(self__,40) == 0)) {
         if ((PATT_(self__,28) != 0)) {
            gl1721_ = PATT_(self__,28);
            cache_dispatch_(gl1721_,1577,gl1722_,INTVAL_(gl1723_));
            gl1718_ = PATT_(gl1721_,INTVAL_(gl1723_));
            cache_dispatch_(gl1718_,1511,gl1719_,INTVAL_(gl1720_));
            if (CFN_(gl1720_)(gl1718_)) {
               IATT_(self__,40) = (int)GLO94_global_key_(0);
               gl1727_ = PATT_(self__,28);
               cache_dispatch_(gl1727_,1577,gl1728_,INTVAL_(gl1729_));
               gl1724_ = PATT_(gl1727_,INTVAL_(gl1729_));
               cache_dispatch_(gl1724_,374,gl1725_,INTVAL_(gl1726_));
               IATT_(self__,44) = (int)IFN_(gl1726_)(gl1724_);
               IATT_(self__,48) = (int)GLO94_global_key_(0);
               IATT_(self__,52) = (int)3;
               IATT_(self__,56) = (int)GLO94_global_key_(0);
               IATT_(self__,60) = (int)3;
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_push_(LIS98_push_(LIS98_push_(LIS98_push_(LIS98_create_(0,6),IATT_(self__,40)),IATT_(self__,44)),(- IATT_(self__,48))),IATT_(self__,52)),(- IATT_(self__,56))),IATT_(self__,60));
            }
            else {
               if ((IATT_(PATT_(self__,32),12) < 2)) {
                  IATT_(self__,40) = (int)(- 1);
               }
               else {
                  IATT_(self__,40) = (int)GLO94_global_key_(0);
                  gl1733_ = PATT_(self__,28);
                  cache_dispatch_(gl1733_,1577,gl1734_,INTVAL_(gl1735_));
                  gl1730_ = PATT_(gl1733_,INTVAL_(gl1735_));
                  cache_dispatch_(gl1730_,374,gl1731_,INTVAL_(gl1732_));
                  IATT_(self__,44) = (int)IFN_(gl1732_)(gl1730_);
                  res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,40)),IATT_(self__,44));
               }
            }
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
   if ((IATT_(self__,40) == (- 1))) {
      if (GLO94_check_is_on_(0)) {
         IATT_(self__,40) = (int)GLO94_global_key_(0);
         gl1739_ = PATT_(self__,28);
         cache_dispatch_(gl1739_,1577,gl1740_,INTVAL_(gl1741_));
         gl1736_ = PATT_(gl1739_,INTVAL_(gl1741_));
         cache_dispatch_(gl1736_,374,gl1737_,INTVAL_(gl1738_));
         IATT_(self__,44) = (int)IFN_(gl1738_)(gl1736_);
         res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(self__,40)),IATT_(self__,44));
      }
      else {
      }
   }
   else {
   }
   if (GLO94_check_is_on_(0)) {
      num_temp_names__ = S_int_VOID_;
      if ((PATT_(self__,32) != 0)) {
         num_temp_names__ = (int)(IATT_(PATT_(self__,32),12) * 2);
      }
      else {
      }
      PATT_(self__,36) = (ptr)new1_(163,num_temp_names__,1);
      if (((num_temp_names__ > 0) & (res__ == 0))) {
         res__ = (ptr)LIS98_create_(0,num_temp_names__);
      }
      else {
      }
      i__ = (int)0;
      while (1) {
         if ((i__ >= num_temp_names__)) {
            goto goto_tag_1751_;
         }
         else {
         }
         tmp_ind__ = (int)GLO94_global_key_(0);
         IATT_(PATT_(self__,36), 8 + ((i__) << 2)) = (int)tmp_ind__;
         res__ = (ptr)LIS98_push_(LIS98_push_(res__,tmp_ind__),3);
         i__ = (int)(i__ + 1);
      }
   goto_tag_1751_: ;
   }
   else {
   }
   if ((res__ == 0)) {
      res__ = (ptr)LST120_gen_temps_(PATT_(self__,32));
   }
   else {
      res__ = (ptr)LIS98_append_(res__,LST120_gen_temps_(PATT_(self__,32)));
   }
   if ((res__ == 0)) {
      gl1742_ = PATT_(self__,28);
      cache_dispatch_(gl1742_,1582,gl1743_,INTVAL_(gl1744_));
      res__ = (ptr)PFN_(gl1744_)(gl1742_);
   }
   else {
      gl1745_ = PATT_(self__,28);
      cache_dispatch_(gl1745_,1582,gl1746_,INTVAL_(gl1747_));
      gl146_ = PFN_(gl1747_)(gl1745_);
      res__ = (ptr)LIS98_append_(res__,gl146_);
   }
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
         IATT_(self__,64) = (int)GLO94_global_key_(0);
         gl1748_ = PATT_(self__,12);
         cache_dispatch_(gl1748_,374,gl1749_,INTVAL_(gl1750_));
         IATT_(self__,68) = (int)IFN_(gl1750_)(gl1748_);
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
      i_147_ = (int)0;
      sz__ = (int)IATT_(PATT_(self__,32),12);
      while (1) {
         if ((i_147_ >= sz__)) {
            goto goto_tag_1752_;
         }
         else {
         }
         if ((IATT_(PATT_(self__,72), 8 + ((i_147_) << 2)) > 0)) {
            if ((res__ != 0)) {
               res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(PATT_(self__,72), 8 + ((i_147_) << 2))),3);
            }
            else {
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(PATT_(self__,72), 8 + ((i_147_) << 2))),3);
            }
         }
         else {
         }
         i_147_ = (int)(i_147_ + 1);
      }
   goto_tag_1752_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr ARE111_expr_eval_constant_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ARE111_get_ext_strs_(self__)
ptr self__;
{
   ptr gl1753_;
   static int gl1754_;
   static union dtype_ gl1755_;

   gl1753_ = PATT_(self__,28);
   cache_dispatch_(gl1753_,1677,gl1754_,INTVAL_(gl1755_));
   VFN_(gl1755_)(gl1753_);
   LST120_get_ext_strs_(PATT_(self__,32));

   ret0__:
   return;
}

char ARE111_to_be_pre_evaluated_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl1756_;
   static int gl1757_;
   static union dtype_ gl1758_;
   ptr gl1759_;
   static int gl1760_;
   static union dtype_ gl1761_;

   gl1759_ = PATT_(self__,28);
   cache_dispatch_(gl1759_,1577,gl1760_,INTVAL_(gl1761_));
   gl1756_ = PATT_(gl1759_,INTVAL_(gl1761_));
   cache_dispatch_(gl1756_,1511,gl1757_,INTVAL_(gl1758_));
   res__ = (char)CFN_(gl1758_)(gl1756_);

   ret0__:
   return (res__);
}

char ARE111_access_value_only_(self__)
ptr self__;
{
   char res__ = S_char_VOID_;
   ptr gl1762_;
   static int gl1763_;
   static union dtype_ gl1764_;

   gl1762_ = PATT_(self__,28);
   cache_dispatch_(gl1762_,1679,gl1763_,INTVAL_(gl1764_));
   res__ = (char)CFN_(gl1764_)(gl1762_);

   ret0__:
   return (res__);
}

ARE111_fob_error_(self__,op_name__,cls_name__)
ptr self__;
ptr op_name__;
ptr cls_name__;
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,76),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

ARE111_cprint_pre_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   SATHER_STR_(20,14,ls1658_,"AREF_EXPROB_S");
   SATHER_STR_(20,17,ls1823_,"Too many indices");
   ptr gl1765_;
   static int gl1766_;
   static union dtype_ gl1767_;
   ptr gl1768_;
   static int gl1769_;
   static union dtype_ gl1770_;
   ptr gl1771_;
   static int gl1772_;
   static union dtype_ gl1773_;
   ptr gl1774_;
   static int gl1775_;
   static union dtype_ gl1776_;
   ptr gl1777_;
   static int gl1778_;
   static union dtype_ gl1779_;
   ptr gl1780_;
   static int gl1781_;
   static union dtype_ gl1782_;
   ptr gl1783_;
   static int gl1784_;
   static union dtype_ gl1785_;
   ptr gl1786_;
   static int gl1787_;
   static union dtype_ gl1788_;
   int    i__ = S_int_VOID_;
   int    isz__ = S_int_VOID_;
   int    i_148_ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    num_temp_names__ = S_int_VOID_;
   int    c_code__ = S_int_VOID_;

   i__ = (int)0;
   isz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      isz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   while (1) {
      if ((i__ >= isz__)) {
         goto goto_tag_1789_;
      }
      else {
      }
      ARE111_cprint_ith_index_pre_code_(self__,i__,outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_1789_: ;
   gl1765_ = PATT_(self__,28);
   cache_dispatch_(gl1765_,1589,gl1766_,INTVAL_(gl1767_));
   VFN_(gl1767_)(gl1765_,outfile__);
   gl1771_ = PATT_(self__,28);
   cache_dispatch_(gl1771_,1577,gl1772_,INTVAL_(gl1773_));
   gl1768_ = PATT_(gl1771_,INTVAL_(gl1773_));
   cache_dispatch_(gl1768_,1511,gl1769_,INTVAL_(gl1770_));
   if (CFN_(gl1770_)(gl1768_)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl1774_ = PATT_(self__,28);
      cache_dispatch_(gl1774_,965,gl1775_,INTVAL_(gl1776_));
      VFN_(gl1776_)(gl1774_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      if (GLO94_check_is_on_(0)) {
         PRI187_cprint_void_tst_(0,outfile__,54,self__);
      }
      else {
      }
      PRI187_cprint_array_dispatch_(0,outfile__,53,self__);
   }
   else {
      if ((isz__ >= 2)) {
         (void)SAT99_indent_(outfile__);
         GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls964_));
         gl1777_ = PATT_(self__,28);
         cache_dispatch_(gl1777_,965,gl1778_,INTVAL_(gl1779_));
         VFN_(gl1779_)(gl1777_,outfile__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
      }
      else {
         if (GLO94_check_is_on_(0)) {
            (void)SAT99_indent_(outfile__);
            GLO94_cprint_ctemp_name_(0,IATT_(self__,40),outfile__);
            (void)SAT99_s_(outfile__,(ptr)(&ls964_));
            gl1780_ = PATT_(self__,28);
            cache_dispatch_(gl1780_,965,gl1781_,INTVAL_(gl1782_));
            VFN_(gl1782_)(gl1780_,outfile__);
            (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
         }
         else {
         }
      }
      if (GLO94_check_is_on_(0)) {
         PRI187_cprint_void_tst_(0,outfile__,54,self__);
      }
      else {
      }
   }
   if (GLO94_check_is_on_(0)) {
      i_148_ = (int)0;
      j__ = (int)i_148_;
      num_temp_names__ = (int)(isz__ * 2);
      while (1) {
         if ((i_148_ >= num_temp_names__)) {
            goto goto_tag_1790_;
         }
         else {
         }
         (void)SAT99_indent_(outfile__);
         GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,36), 8 + ((i_148_) << 2)),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls964_));
         ARE111_cprint_ith_index_act_code_(self__,j__,outfile__);
         j__ = (int)(j__ + 1);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
         (void)SAT99_indent_(outfile__);
         GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,36), 8 + (((i_148_ + 1)) << 2)),outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls964_));
         c_code__ = S_int_VOID_;
         gl1786_ = PATT_(self__,28);
         cache_dispatch_(gl1786_,1577,gl1787_,INTVAL_(gl1788_));
         gl1783_ = PATT_(gl1786_,INTVAL_(gl1788_));
         cache_dispatch_(gl1783_,1511,gl1784_,INTVAL_(gl1785_));
         if (CFN_(gl1785_)(gl1783_)) {
            switch (i_148_) {
               case (0) :
                  c_code__ = (int)55;
                  break;
               case (2) :
                  c_code__ = (int)56;
                  break;
               case (4) :
                  c_code__ = (int)57;
                  break;
               case (6) :
                  c_code__ = (int)58;
                  break;
               default:
                  ERR96_compiler_error_msg_(0,(ptr)(&ls1658_),(ptr)(&ls1823_));
                  ;
            }
         }
         else {
            switch (i_148_) {
               case (0) :
                  c_code__ = (int)59;
                  break;
               case (2) :
                  c_code__ = (int)60;
                  break;
               case (4) :
                  c_code__ = (int)61;
                  break;
               case (6) :
                  c_code__ = (int)62;
                  break;
               default:
                  ERR96_compiler_error_msg_(0,(ptr)(&ls1658_),(ptr)(&ls1823_));
                  ;
            }
         }
         PRI187_cprint_int_attr_access_(0,outfile__,c_code__,self__);
         (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
         PRI187_cprint_arr_bound_tst_(0,i_148_,IATT_(PATT_(self__,36), 8 + ((i_148_) << 2)),IATT_(PATT_(self__,36), 8 + (((i_148_ + 1)) << 2)),outfile__,IATT_(self__,76));
         i_148_ = (int)(i_148_ + 2);
      }
   goto_tag_1790_: ;
   }
   else {
   }
   if (GLO94_check_is_on_(0)) {
      if ((IATT_(self__,24) != 0)) {
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

ARE111_cprint_act_code_(self__,outfile__)
ptr self__;
ptr outfile__;
{
   SATHER_STR_(20,14,ls1658_,"AREF_EXPROB_S");
   SATHER_STR_(20,26,ls1727_,"Invalid printing count = ");
   SATHER_STR_(20,4,ls1728_," > ");
   ptr gl1791_;
   static int gl1792_;
   static union dtype_ gl1793_;
   ptr gl1794_;
   static int gl1795_;
   static union dtype_ gl1796_;
   ptr gl1797_;
   static int gl1798_;
   static union dtype_ gl1799_;
   ptr gl1800_;
   static int gl1801_;
   static union dtype_ gl1802_;
   ptr gl1803_;
   static int gl1804_;
   static union dtype_ gl1805_;
   ptr gl1806_;
   static int gl1807_;
   static union dtype_ gl1808_;
   ptr gl1809_;
   static int gl1810_;
   static union dtype_ gl1811_;
   ptr gl1812_;
   static int gl1813_;
   static union dtype_ gl1814_;
   ptr gl1815_;
   static int gl1816_;
   static union dtype_ gl1817_;
   ptr gl1818_;
   static int gl1819_;
   static union dtype_ gl1820_;
   int    isz__ = S_int_VOID_;

   if ((IATT_(self__,80) > GLO68_g_tag_)) {
      if ((IATT_(self__,64) > 0)) {
         GLO94_cprint_ctemp_name_(0,IATT_(self__,64),outfile__);
         goto ret0__;
      }
      else {
         ERR96_compiler_error_msg_(0,(ptr)(&ls1658_),STR20_i_(STR20_s_(STR20_i_(STR20_s_(STR20_create_(0),(ptr)(&ls1727_)),IATT_(self__,80)),(ptr)(&ls1728_)),GLO68_g_tag_));
      }
   }
   else {
   }
   IATT_(self__,80) = (int)(IATT_(self__,80) + 1);
   isz__ = S_int_VOID_;
   if ((PATT_(self__,32) != 0)) {
      isz__ = (int)IATT_(PATT_(self__,32),12);
   }
   else {
   }
   ARE111_cprint_cast_code_(self__,outfile__);
   gl1794_ = PATT_(self__,28);
   cache_dispatch_(gl1794_,1577,gl1795_,INTVAL_(gl1796_));
   gl1791_ = PATT_(gl1794_,INTVAL_(gl1796_));
   cache_dispatch_(gl1791_,1511,gl1792_,INTVAL_(gl1793_));
   if (CFN_(gl1793_)(gl1791_)) {
      switch (isz__) {
         case (1) :
            gl1797_ = PATT_(self__,12);
            cache_dispatch_(gl1797_,374,gl1798_,INTVAL_(gl1799_));
            switch (IFN_(gl1799_)(gl1797_)) {
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
         case (2) :
            gl1800_ = PATT_(self__,12);
            cache_dispatch_(gl1800_,374,gl1801_,INTVAL_(gl1802_));
            switch (IFN_(gl1802_)(gl1800_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,6,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,6,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,6,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,6,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,6,self__);
                  break;
               default:
                  ;
                  ;
            }
            break;
         case (3) :
            gl1803_ = PATT_(self__,12);
            cache_dispatch_(gl1803_,374,gl1804_,INTVAL_(gl1805_));
            switch (IFN_(gl1805_)(gl1803_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,12,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,12,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,12,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,12,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,12,self__);
                  break;
               default:
                  ;
                  ;
            }
            break;
         case (4) :
            gl1806_ = PATT_(self__,12);
            cache_dispatch_(gl1806_,374,gl1807_,INTVAL_(gl1808_));
            switch (IFN_(gl1808_)(gl1806_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,19,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,19,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,19,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,19,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,19,self__);
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
   else {
      switch (isz__) {
         case (1) :
            gl1809_ = PATT_(self__,12);
            cache_dispatch_(gl1809_,374,gl1810_,INTVAL_(gl1811_));
            switch (IFN_(gl1811_)(gl1809_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,27,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,27,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,27,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,27,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,27,self__);
                  break;
               default:
                  ;
                  ;
            }
            break;
         case (2) :
            gl1812_ = PATT_(self__,12);
            cache_dispatch_(gl1812_,374,gl1813_,INTVAL_(gl1814_));
            switch (IFN_(gl1814_)(gl1812_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,32,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,32,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,32,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,32,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,32,self__);
                  break;
               default:
                  ;
                  ;
            }
            break;
         case (3) :
            gl1815_ = PATT_(self__,12);
            cache_dispatch_(gl1815_,374,gl1816_,INTVAL_(gl1817_));
            switch (IFN_(gl1817_)(gl1815_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,38,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,38,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,38,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,38,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,38,self__);
                  break;
               default:
                  ;
                  ;
            }
            break;
         case (4) :
            gl1818_ = PATT_(self__,12);
            cache_dispatch_(gl1818_,374,gl1819_,INTVAL_(gl1820_));
            switch (IFN_(gl1820_)(gl1818_)) {
               case (1) :
                  PRI187_cprint_ptr_attr_access_(0,outfile__,45,self__);
                  break;
               case (3) :
                  PRI187_cprint_int_attr_access_(0,outfile__,45,self__);
                  break;
               case (2) :
                  PRI187_cprint_char_attr_access_(0,outfile__,45,self__);
                  break;
               case (4) :
                  PRI187_cprint_float_attr_access_(0,outfile__,45,self__);
                  break;
               case (5) :
                  PRI187_cprint_double_attr_access_(0,outfile__,45,self__);
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

   ret0__:
   return;
}

ARE111_cprint_cast_code_(self__,outfile__)
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

ARE111_cprint_ith_index_pre_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl1821_;
   static int gl1822_;
   static union dtype_ gl1823_;
   ptr gl1824_;
   static int gl1825_;
   static union dtype_ gl1826_;

   gl1821_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
   cache_dispatch_(gl1821_,1589,gl1822_,INTVAL_(gl1823_));
   VFN_(gl1823_)(gl1821_,outfile__);
   if ((IATT_(PATT_(self__,72), 8 + ((i__) << 2)) > 0)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,72), 8 + ((i__) << 2)),outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl1824_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
      cache_dispatch_(gl1824_,965,gl1825_,INTVAL_(gl1826_));
      VFN_(gl1826_)(gl1824_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }

   ret0__:
   return;
}

ARE111_cprint_ith_index_act_code_(self__,i__,outfile__)
ptr self__;
int i__;
ptr outfile__;
{
   ptr gl1827_;
   static int gl1828_;
   static union dtype_ gl1829_;
   int    ith_temp__ = S_int_VOID_;

   ith_temp__ = (int)IATT_(PATT_(self__,72), 8 + ((i__) << 2));
   if ((ith_temp__ > 0)) {
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
   }
   else {
      gl1827_ = PATT_(PATT_(self__,32), 28 + ((i__) << 2));
      cache_dispatch_(gl1827_,965,gl1828_,INTVAL_(gl1829_));
      VFN_(gl1829_)(gl1827_,outfile__);
   }

   ret0__:
   return;
}

ARE111_cprint_index_(self__,i__,sz__,outfile__)
ptr self__;
int i__;
int sz__;
ptr outfile__;
{
   SATHER_STR_(20,3,ls1857_,"((");
   SATHER_STR_(20,3,ls1858_,"))");
   SATHER_STR_(20,8,ls1859_,") << 1)");
   SATHER_STR_(20,8,ls1860_,") << 2)");
   SATHER_STR_(20,8,ls1861_,") << 3)");
   SATHER_STR_(20,5,ls1862_,") * ");

   (void)SAT99_s_(outfile__,(ptr)(&ls1857_));
   if (GLO94_check_is_on_(0)) {
      GLO94_cprint_ctemp_name_(0,IATT_(PATT_(self__,36), 8 + (((i__ * 2)) << 2)),outfile__);
   }
   else {
      ARE111_cprint_ith_index_act_code_(self__,i__,outfile__);
   }
   switch (sz__) {
      case (1) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1858_));
         break;
      case (2) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1859_));
         break;
      case (4) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1860_));
         break;
      case (8) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1861_));
         break;
      default:
         (void)SAT99_c_(SAT99_i_(SAT99_s_(outfile__,(ptr)(&ls1862_)),sz__),')');
         ;
   }

   ret0__:
   return;
}

