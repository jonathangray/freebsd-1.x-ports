/* op_exp110.c : Sather class: OP_EXPROB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void EXP117_cprint_pre_code_(ptr self__, ptr outfile__);
extern void EXP117_cprint_act_code_(ptr self__, ptr outfile__);
extern int STR12_get_i_(ptr self__);
extern ptr INT15_to_s_(int self__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern ptr STR20_c_(ptr self__, char ch__);
extern int TYP149_ctype_(ptr self__);
extern char TYP149_bool_type_p_(ptr self__);
extern char TYP149_arithtype_(ptr self__);
extern char TYP149_conforms_to_(ptr self__, ptr tp__);
extern ptr TYP149_resolve_arithtype_(ptr self__, ptr tp__);
extern char TYP149_int_type_p_(ptr self__);
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
extern ptr CON194_val_(ptr self__);
extern /*shared*/ ptr GLO68_bool_typeob_s_;
extern /*shared*/ ptr GLO68_int_typeob_s_;
extern int GLO94_global_key_(ptr self__);
extern void GLO94_cprint_ctemp_name_(ptr self__, int intval__, ptr outfile__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_error_msg_(ptr self__, int ln__, ptr s__);
extern void ERR96_type_mismatch_err_(ptr self__, ptr where__, ptr comnt__, ptr t1__, ptr t2__, int ln__);
extern void ERR96_format_error_exit_(ptr self__, int ln__, ptr s__);
extern ptr LIS98_push_(ptr self__, int e__);
extern ptr LIS98_create_(ptr self__, int init_size__);
extern ptr SAT99_c_(ptr self__, char ch__);
extern ptr SAT99_s_(ptr self__, ptr st__);
extern void ERR96_compiler_error_msg_(ptr self__, ptr classname__, ptr msg__);
extern ptr SAT99_indent_(ptr self__);
extern ptr INT106_create_(ptr self__, ptr v__, int ln__);
extern ptr SAT99_inc_ln_(ptr self__, int i__);
extern ptr EXP117_sather_code_(ptr self__);
extern char EXP117_to_be_pre_evaluated_(ptr self__);
extern char EXP117_valid_init_expr_(ptr self__);
extern void LST120_out_of_line_(ptr self__, ptr fn__);
extern ptr LST120_dup_(ptr self__);
extern void LST120_resolve_predef_types_(ptr self__, int index__);
extern void LST120_semant_(ptr self__, ptr symtab__);
extern void LST120_cprint_init_code_(ptr self__, ptr outfile__);
extern ptr LST120_gen_temps_(ptr self__);
extern ptr EXP117_expr_eval_constant_(ptr self__);
extern void LST120_get_ext_strs_(ptr self__);
extern struct { int tp_; int sz_; char st_; } gs1216_;
extern struct { int tp_; int sz_; char st_; } gs1217_;
extern struct { int tp_; int sz_; char st_; } gs1218_;
extern struct { int tp_; int sz_; char st_; } gs1219_;
extern struct { int tp_; int sz_; char st_; } gs1215_;
#include "macros_.h"



/*constant*/ int OP_110_print_indent_ = 2;
ptr OP_110_create_(ptr self__, int op__, ptr children__, int ln__);
void OP_110_out_of_line_(ptr self__, ptr fn__);
ptr OP_110_dup_(ptr self__);
void OP_110_put_kwdname_(ptr self__, int nm__);
ptr OP_110_sather_code_(ptr self__);
ptr OP_110_initialize_(ptr self__, ptr initarg__);
void OP_110_resolve_predef_types_(ptr self__, int index__);
void OP_110_semant_(ptr self__, ptr symtab__);
ptr OP_110_typeof_(ptr self__);
int OP_110_get_offset_(ptr self__);
void OP_110_cprint_offset_(ptr self__, ptr outfile__);
ptr OP_110_get_constval_(ptr self__);
void OP_110_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void OP_110_cprint_cname_(ptr self__, ptr outfile__);
void OP_110_cprint_extern_(ptr self__, ptr outfile__);
void OP_110_cprint_access_value_(ptr self__, ptr outfile__);
void OP_110_cprint_init_code_(ptr self__, ptr outfile__);
char OP_110_valid_init_expr_(ptr self__);
char OP_110_assignable_p_(ptr self__);
ptr OP_110_gen_temps_(ptr self__);
ptr OP_110_expr_eval_constant_(ptr self__);
void OP_110_get_ext_strs_(ptr self__);
char OP_110_to_be_pre_evaluated_(ptr self__);
char OP_110_access_value_only_(ptr self__);
void OP_110_fob_error_(ptr self__, ptr op_name__, ptr cls_name__);
void OP_110_cprint_pre_code_(ptr self__, ptr outfile__);
void OP_110_cprint_act_code_(ptr self__, ptr outfile__);
void OP_110_cprint_cast_code_(ptr self__, ptr outfile__);
/*constant*/ int OP_110_not_op_ind_ = 1;
/*constant*/ int OP_110_lt_op_ind_ = 2;
/*constant*/ int OP_110_gt_op_ind_ = 3;
/*constant*/ int OP_110_le_op_ind_ = 4;
/*constant*/ int OP_110_ge_op_ind_ = 5;
/*constant*/ int OP_110_eq_op_ind_ = 6;
/*constant*/ int OP_110_ne_op_ind_ = 7;
/*constant*/ int OP_110_and_op_ind_ = 8;
/*constant*/ int OP_110_or_op_ind_ = 9;
/*constant*/ int OP_110_uminus_op_ind_ = 10;
/*constant*/ int OP_110_uplus_op_ind_ = 11;
/*constant*/ int OP_110_exp_op_ind_ = 12;
/*constant*/ int OP_110_plus_op_ind_ = 13;
/*constant*/ int OP_110_minus_op_ind_ = 14;
/*constant*/ int OP_110_mult_op_ind_ = 15;
/*constant*/ int OP_110_divide_op_ind_ = 16;
void OP_110_cprint_ith_child_pre_code_(ptr self__, int i__, ptr outfile__);
void OP_110_cprint_ith_child_act_code_(ptr self__, int i__, ptr outfile__);
extern int attr_ent_OP_110[];

ptr OP_110_create_(ptr self__, int op__, ptr children__, int ln__)
{
   ptr res__ = 0;
   int    s__ = S_int_VOID_;

   res__ = (ptr)new_(110,0);
   IATT_(res__,24) = (int)op__;
   PATT_(res__,28) = (ptr)children__;
   IATT_(res__,32) = (int)ln__;
   s__ = (int)IATT_(children__,12);
   PATT_(res__,44) = (ptr)new1_(163,s__,1);
   PATT_(res__,48) = (ptr)new1_(163,s__,1);

   ret0__:
   return (res__);
}

void OP_110_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,32) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,32));
   LST120_out_of_line_(PATT_(self__,28),fn__);

   ret0__:
   return;
}

ptr OP_110_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)OP_110_create_(self__,IATT_(self__,24),LST120_dup_(PATT_(self__,28)),IATT_(self__,32));

   ret0__:
   return (res__);
}

void OP_110_put_kwdname_(ptr self__, int nm__)
{
   ptr gl1361_;
   static int gl1362_;
   static union dtype_ gl1363_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl1361_ = x__;
   cache_dispatch_(gl1361_,796,gl1362_,INTVAL_(gl1363_));
   IATT_(gl1361_,INTVAL_(gl1363_)) = (int)nm__;

   ret0__:
   return;
}

ptr OP_110_sather_code_(ptr self__)
{
   ptr res__ = 0;
   SATHER_STR_(20,6,ls1756_,"(not ");
   SATHER_STR_(20,2,ls1278_,")");
   SATHER_STR_(20,4,ls1757_," < ");
   SATHER_STR_(20,4,ls1728_," > ");
   SATHER_STR_(20,5,ls1758_," <= ");
   SATHER_STR_(20,5,ls1759_," >= ");
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,5,ls1760_," /= ");
   SATHER_STR_(20,6,ls738_," and ");
   SATHER_STR_(20,5,ls1363_," or ");
   SATHER_STR_(20,4,ls1761_,"(- ");
   SATHER_STR_(20,4,ls1762_,"(+ ");
   SATHER_STR_(20,4,ls1763_," + ");
   SATHER_STR_(20,4,ls1764_," - ");
   SATHER_STR_(20,4,ls1765_," * ");
   SATHER_STR_(20,4,ls1766_," / ");
   ptr gl1364_;
   static int gl1365_;
   static union dtype_ gl1366_;
   ptr gl1367_;
   static int gl1368_;
   static union dtype_ gl1369_;
   ptr gl1370_;
   static int gl1371_;
   static union dtype_ gl1372_;
   ptr    c1__ = 0;
   ptr    c2__ = 0;

   c1__ = S_ptr_VOID_;
   c2__ = S_ptr_VOID_;
   switch (IATT_(self__,24)) {
      case (1) :
      case (10) :
      case (11) :
         gl1364_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1364_,801,gl1365_,INTVAL_(gl1366_));
         c1__ = (ptr)PFN_(gl1366_)(gl1364_);
         break;
      default:
         gl1367_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1367_,801,gl1368_,INTVAL_(gl1369_));
         c1__ = (ptr)PFN_(gl1369_)(gl1367_);
         gl1370_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1370_,801,gl1371_,INTVAL_(gl1372_));
         c2__ = (ptr)PFN_(gl1372_)(gl1370_);
         ;
   }
   switch (IATT_(self__,24)) {
      case (1) :
         res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1756_)),c2__),(ptr)(&ls1278_));
         break;
      case (2) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1757_)),c2__),')');
         break;
      case (3) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1728_)),c2__),')');
         break;
      case (4) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1758_)),c2__),')');
         break;
      case (5) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1759_)),c2__),')');
         break;
      case (6) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls964_)),c2__),')');
         break;
      case (7) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1760_)),c2__),')');
         break;
      case (8) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls738_)),c2__),')');
         break;
      case (9) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1363_)),c2__),')');
         break;
      case (10) :
         res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1761_)),c2__),(ptr)(&ls1278_));
         break;
      case (11) :
         res__ = (ptr)STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1762_)),c2__),(ptr)(&ls1278_));
         break;
      case (13) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1763_)),c2__),')');
         break;
      case (14) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1764_)),c2__),')');
         break;
      case (15) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1765_)),c2__),')');
         break;
      case (16) :
         res__ = (ptr)STR20_c_(STR20_s_(STR20_s_(STR20_s_(STR20_c_(STR20_create_(0),'('),c1__),(ptr)(&ls1766_)),c2__),')');
         break;
      default:
         ;
         ;
   }

   ret0__:
   return (res__);
}

ptr OP_110_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void OP_110_resolve_predef_types_(ptr self__, int index__)
{

   LST120_resolve_predef_types_(PATT_(self__,28),index__);

   ret0__:
   return;
}

void OP_110_semant_(ptr self__, ptr symtab__)
{
   SATHER_STR_(20,36,ls1767_,"(OP_EXPROB_S): Unknown operand type");
   SATHER_STR_(20,32,ls1769_,"(OP_EXPROB_S): Expect BOOL type");
   SATHER_STR_(20,11,ls1771_,"comparison");
   SATHER_STR_(20,15,ls1772_," or vice versa");
   SATHER_STR_(20,60,ls1773_,"(OP_EXPROB_S): Expect arithmetic expressions for comparison");
   SATHER_STR_(20,45,ls1774_,"(OP_EXPROB_S): Expect arithmetic expressions");
   SATHER_STR_(20,69,ls1775_,"(OP_EXPROB_S): Expect arithmetic expressions for arithmetic operator");
   SATHER_STR_(20,32,ls1777_,"(OP_EXPROB_S): Unknown operator");
   ptr gl1373_;
   static int gl1374_;
   static union dtype_ gl1375_;
   ptr gl117_;
   ptr gl1376_;
   static int gl1377_;
   static union dtype_ gl1378_;
   ptr gl1379_;
   static int gl1380_;
   static union dtype_ gl1381_;
   ptr gl1382_;
   static int gl1383_;
   static union dtype_ gl1384_;
   ptr gl1385_;
   static int gl1386_;
   static union dtype_ gl1387_;
   ptr gl1388_;
   static int gl1389_;
   static union dtype_ gl1390_;
   char gl118_;
   ptr gl1391_;
   static int gl1392_;
   static union dtype_ gl1393_;
   ptr gl1394_;
   static int gl1395_;
   static union dtype_ gl1396_;
   char gl119_;
   ptr gl1397_;
   static int gl1398_;
   static union dtype_ gl1399_;
   ptr gl1400_;
   static int gl1401_;
   static union dtype_ gl1402_;
   char gl120_;
   ptr gl1403_;
   static int gl1404_;
   static union dtype_ gl1405_;
   ptr gl1406_;
   static int gl1407_;
   static union dtype_ gl1408_;
   ptr gl1409_;
   static int gl1410_;
   static union dtype_ gl1411_;
   ptr gl1412_;
   static int gl1413_;
   static union dtype_ gl1414_;
   char gl121_;
   char gl122_;
   ptr gl1415_;
   static int gl1416_;
   static union dtype_ gl1417_;
   ptr gl1418_;
   static int gl1419_;
   static union dtype_ gl1420_;
   ptr gl1421_;
   static int gl1422_;
   static union dtype_ gl1423_;
   char gl123_;
   ptr gl1424_;
   static int gl1425_;
   static union dtype_ gl1426_;
   ptr gl1427_;
   static int gl1428_;
   static union dtype_ gl1429_;
   ptr gl1430_;
   static int gl1431_;
   static union dtype_ gl1432_;
   char gl124_;
   ptr gl1433_;
   static int gl1434_;
   static union dtype_ gl1435_;
   ptr gl1436_;
   static int gl1437_;
   static union dtype_ gl1438_;
   ptr gl125_;
   ptr gl126_;
   ptr gl1439_;
   static int gl1440_;
   static union dtype_ gl1441_;
   ptr gl1442_;
   static int gl1443_;
   static union dtype_ gl1444_;
   char gl127_;
   ptr gl1445_;
   static int gl1446_;
   static union dtype_ gl1447_;
   ptr gl1448_;
   static int gl1449_;
   static union dtype_ gl1450_;
   char gl128_;
   ptr gl1451_;
   static int gl1452_;
   static union dtype_ gl1453_;
   ptr gl1454_;
   static int gl1455_;
   static union dtype_ gl1456_;
   char gl129_;
   ptr gl1457_;
   static int gl1458_;
   static union dtype_ gl1459_;
   ptr gl1460_;
   static int gl1461_;
   static union dtype_ gl1462_;
   ptr gl1463_;
   static int gl1464_;
   static union dtype_ gl1465_;
   char gl130_;
   ptr gl1466_;
   static int gl1467_;
   static union dtype_ gl1468_;
   ptr gl1469_;
   static int gl1470_;
   static union dtype_ gl1471_;
   char gl131_;
   ptr gl1472_;
   static int gl1473_;
   static union dtype_ gl1474_;
   ptr gl1475_;
   static int gl1476_;
   static union dtype_ gl1477_;
   ptr gl1478_;
   static int gl1479_;
   static union dtype_ gl1480_;
   int    csz__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   csz__ = S_int_VOID_;
   if ((PATT_(self__,28) != 0)) {
      LST120_semant_(PATT_(self__,28),symtab__);
      csz__ = (int)IATT_(PATT_(self__,28),12);
   }
   else {
   }
   i__ = (int)0;
   while (1) {
      if ((i__ >= csz__)) {
         goto goto_tag_1481_;
      }
      else {
      }
      gl1373_ = PATT_(PATT_(self__,28), 28 + ((i__) << 2));
      cache_dispatch_(gl1373_,1577,gl1374_,INTVAL_(gl1375_));
      gl117_ = PATT_(gl1373_,INTVAL_(gl1375_));
      if ((gl117_ == 0)) {
         ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1767_)));
         switch (IATT_(self__,24)) {
            case (8) :
            case (9) :
            case (1) :
            case (6) :
            case (7) :
            case (2) :
            case (3) :
            case (4) :
            case (5) :
               PATT_(self__,8) = (ptr)GLO68_bool_typeob_s_;
               break;
            default:
               PATT_(self__,8) = (ptr)GLO68_int_typeob_s_;
               ;
         }
         goto ret0__;
      }
      else {
      }
      gl1376_ = PATT_(PATT_(self__,28), 28 + ((i__) << 2));
      cache_dispatch_(gl1376_,1678,gl1377_,INTVAL_(gl1378_));
      if (CFN_(gl1378_)(gl1376_)) {
         IATT_(PATT_(self__,44), 8 + ((i__) << 2)) = (int)GLO94_global_key_(0);
         gl1382_ = PATT_(PATT_(self__,28), 28 + ((i__) << 2));
         cache_dispatch_(gl1382_,1577,gl1383_,INTVAL_(gl1384_));
         gl1379_ = PATT_(gl1382_,INTVAL_(gl1384_));
         cache_dispatch_(gl1379_,374,gl1380_,INTVAL_(gl1381_));
         IATT_(PATT_(self__,48), 8 + ((i__) << 2)) = (int)IFN_(gl1381_)(gl1379_);
         CATT_(self__,5) = (char)1;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_1481_: ;
   switch (IATT_(self__,24)) {
      case (8) :
      case (9) :
         gl1388_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1388_,1577,gl1389_,INTVAL_(gl1390_));
         gl1385_ = PATT_(gl1388_,INTVAL_(gl1390_));
         cache_dispatch_(gl1385_,1768,gl1386_,INTVAL_(gl1387_));
         gl118_ = CFN_(gl1387_)(gl1385_);
         gl1394_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1394_,1577,gl1395_,INTVAL_(gl1396_));
         gl1391_ = PATT_(gl1394_,INTVAL_(gl1396_));
         cache_dispatch_(gl1391_,1768,gl1392_,INTVAL_(gl1393_));
         gl119_ = CFN_(gl1393_)(gl1391_);
         if (((! gl118_) | (! gl119_))) {
            ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1769_)));
         }
         else {
         }
         PATT_(self__,8) = (ptr)GLO68_bool_typeob_s_;
         break;
      case (1) :
         gl1400_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1400_,1577,gl1401_,INTVAL_(gl1402_));
         gl1397_ = PATT_(gl1400_,INTVAL_(gl1402_));
         cache_dispatch_(gl1397_,1768,gl1398_,INTVAL_(gl1399_));
         gl120_ = CFN_(gl1399_)(gl1397_);
         if ((! gl120_)) {
            ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1769_)));
         }
         else {
         }
         PATT_(self__,8) = (ptr)GLO68_bool_typeob_s_;
         break;
      case (6) :
      case (7) :
         gl1406_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1406_,1577,gl1407_,INTVAL_(gl1408_));
         gl1403_ = PATT_(gl1406_,INTVAL_(gl1408_));
         cache_dispatch_(gl1403_,1770,gl1404_,INTVAL_(gl1405_));
         gl121_ = CFN_(gl1405_)(gl1403_);
         gl1412_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1412_,1577,gl1413_,INTVAL_(gl1414_));
         gl1409_ = PATT_(gl1412_,INTVAL_(gl1414_));
         cache_dispatch_(gl1409_,1770,gl1410_,INTVAL_(gl1411_));
         gl122_ = CFN_(gl1411_)(gl1409_);
         if ((! (gl121_ & gl122_))) {
            gl1418_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
            cache_dispatch_(gl1418_,1577,gl1419_,INTVAL_(gl1420_));
            gl1421_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
            cache_dispatch_(gl1421_,1577,gl1422_,INTVAL_(gl1423_));
            gl1415_ = PATT_(gl1421_,INTVAL_(gl1423_));
            cache_dispatch_(gl1415_,903,gl1416_,INTVAL_(gl1417_));
            gl123_ = CFN_(gl1417_)(gl1415_,PATT_(gl1418_,INTVAL_(gl1420_)));
            gl1427_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
            cache_dispatch_(gl1427_,1577,gl1428_,INTVAL_(gl1429_));
            gl1430_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
            cache_dispatch_(gl1430_,1577,gl1431_,INTVAL_(gl1432_));
            gl1424_ = PATT_(gl1430_,INTVAL_(gl1432_));
            cache_dispatch_(gl1424_,903,gl1425_,INTVAL_(gl1426_));
            gl124_ = CFN_(gl1426_)(gl1424_,PATT_(gl1427_,INTVAL_(gl1429_)));
            if (((! gl123_) & (! gl124_))) {
               gl1433_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
               cache_dispatch_(gl1433_,1577,gl1434_,INTVAL_(gl1435_));
               gl125_ = PATT_(gl1433_,INTVAL_(gl1435_));
               gl1436_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
               cache_dispatch_(gl1436_,1577,gl1437_,INTVAL_(gl1438_));
               gl126_ = PATT_(gl1436_,INTVAL_(gl1438_));
               ERR96_type_mismatch_err_(0,(ptr)(&ls1771_),(ptr)(&ls1772_),gl125_,gl126_,IATT_(self__,32));
            }
            else {
            }
         }
         else {
         }
         PATT_(self__,8) = (ptr)GLO68_bool_typeob_s_;
         break;
      case (2) :
      case (3) :
      case (4) :
      case (5) :
         gl1442_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1442_,1577,gl1443_,INTVAL_(gl1444_));
         gl1439_ = PATT_(gl1442_,INTVAL_(gl1444_));
         cache_dispatch_(gl1439_,1770,gl1440_,INTVAL_(gl1441_));
         gl127_ = CFN_(gl1441_)(gl1439_);
         gl1448_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1448_,1577,gl1449_,INTVAL_(gl1450_));
         gl1445_ = PATT_(gl1448_,INTVAL_(gl1450_));
         cache_dispatch_(gl1445_,1770,gl1446_,INTVAL_(gl1447_));
         gl128_ = CFN_(gl1447_)(gl1445_);
         if (((! gl127_) | (! gl128_))) {
            ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1773_)));
         }
         else {
         }
         PATT_(self__,8) = (ptr)GLO68_bool_typeob_s_;
         break;
      case (10) :
      case (11) :
         gl1454_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1454_,1577,gl1455_,INTVAL_(gl1456_));
         gl1451_ = PATT_(gl1454_,INTVAL_(gl1456_));
         cache_dispatch_(gl1451_,1770,gl1452_,INTVAL_(gl1453_));
         gl129_ = CFN_(gl1453_)(gl1451_);
         if ((! gl129_)) {
            ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1774_)));
         }
         else {
         }
         gl1457_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1457_,1577,gl1458_,INTVAL_(gl1459_));
         PATT_(self__,8) = (ptr)PATT_(gl1457_,INTVAL_(gl1459_));
         break;
      case (12) :
      case (13) :
      case (14) :
      case (15) :
      case (16) :
         gl1463_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1463_,1577,gl1464_,INTVAL_(gl1465_));
         gl1460_ = PATT_(gl1463_,INTVAL_(gl1465_));
         cache_dispatch_(gl1460_,1770,gl1461_,INTVAL_(gl1462_));
         gl130_ = CFN_(gl1462_)(gl1460_);
         gl1469_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1469_,1577,gl1470_,INTVAL_(gl1471_));
         gl1466_ = PATT_(gl1469_,INTVAL_(gl1471_));
         cache_dispatch_(gl1466_,1770,gl1467_,INTVAL_(gl1468_));
         gl131_ = CFN_(gl1468_)(gl1466_);
         if (((! gl130_) | (! gl131_))) {
            ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1775_)));
         }
         else {
         }
         gl1475_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1475_,1577,gl1476_,INTVAL_(gl1477_));
         gl1478_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1478_,1577,gl1479_,INTVAL_(gl1480_));
         gl1472_ = PATT_(gl1478_,INTVAL_(gl1480_));
         cache_dispatch_(gl1472_,1776,gl1473_,INTVAL_(gl1474_));
         PATT_(self__,8) = (ptr)PFN_(gl1474_)(gl1472_,PATT_(gl1475_,INTVAL_(gl1477_)));
         break;
      default:
         ERR96_format_error_exit_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1777_)));
         ;
   }

   ret0__:
   return;
}

ptr OP_110_typeof_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)PATT_(self__,8);

   ret0__:
   return (res__);
}

int OP_110_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void OP_110_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr OP_110_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void OP_110_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void OP_110_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void OP_110_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void OP_110_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void OP_110_cprint_init_code_(ptr self__, ptr outfile__)
{

   LST120_cprint_init_code_(PATT_(self__,28),outfile__);

   ret0__:
   return;
}

char OP_110_valid_init_expr_(ptr self__)
{
   char res__ = S_char_VOID_;
   SATHER_STR_(20,32,ls1777_,"(OP_EXPROB_S): Unknown operator");
   ptr gl1482_;
   static int gl1483_;
   static union dtype_ gl1484_;
   ptr gl1485_;
   static int gl1486_;
   static union dtype_ gl1487_;
   char gl132_;
   char gl133_;
   ptr gl1488_;
   static int gl1489_;
   static union dtype_ gl1490_;

   switch (IATT_(self__,24)) {
      case (8) :
      case (9) :
      case (6) :
      case (7) :
      case (2) :
      case (3) :
      case (4) :
      case (5) :
      case (13) :
      case (14) :
      case (15) :
      case (16) :
         gl1482_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1482_,1580,gl1483_,INTVAL_(gl1484_));
         gl132_ = CFN_(gl1484_)(gl1482_);
         gl1485_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1485_,1580,gl1486_,INTVAL_(gl1487_));
         gl133_ = CFN_(gl1487_)(gl1485_);
         res__ = (char)(gl132_ & gl133_);
         break;
      case (1) :
      case (10) :
      case (11) :
         gl1488_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1488_,1580,gl1489_,INTVAL_(gl1490_));
         res__ = (char)CFN_(gl1490_)(gl1488_);
         break;
      default:
         ERR96_format_error_exit_(0,IATT_(self__,32),STR20_s_(STR20_create_(0),(ptr)(&ls1777_)));
         ;
   }

   ret0__:
   return (res__);
}

char OP_110_assignable_p_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

ptr OP_110_gen_temps_(ptr self__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    s__ = S_int_VOID_;

   res__ = (ptr)LST120_gen_temps_(PATT_(self__,28));
   if (CATT_(self__,5)) {
      i__ = S_int_VOID_;
      s__ = (int)IATT_(PATT_(self__,28),12);
      while (1) {
         if ((i__ >= s__)) {
            goto goto_tag_1491_;
         }
         else {
         }
         if ((IATT_(PATT_(self__,44), 8 + ((i__) << 2)) > 0)) {
            if ((res__ != 0)) {
               res__ = (ptr)LIS98_push_(LIS98_push_(res__,IATT_(PATT_(self__,44), 8 + ((i__) << 2))),IATT_(PATT_(self__,48), 8 + ((i__) << 2)));
            }
            else {
               res__ = (ptr)LIS98_push_(LIS98_push_(LIS98_create_(0,2),IATT_(PATT_(self__,44), 8 + ((i__) << 2))),IATT_(PATT_(self__,48), 8 + ((i__) << 2)));
            }
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_1491_: ;
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr OP_110_expr_eval_constant_(ptr self__)
{
   ptr res__ = 0;
   ptr gl1492_;
   static int gl1493_;
   static union dtype_ gl1494_;
   ptr gl1495_;
   static int gl1496_;
   static union dtype_ gl1497_;
   char gl134_;
   ptr gl1498_;
   static int gl1499_;
   static union dtype_ gl1500_;
   ptr gl1501_;
   static int gl1502_;
   static union dtype_ gl1503_;
   ptr gl1504_;
   static int gl1505_;
   static union dtype_ gl1506_;
   ptr gl1507_;
   static int gl1508_;
   static union dtype_ gl1509_;
   char gl135_;
   char gl136_;
   ptr gl1510_;
   static int gl1511_;
   static union dtype_ gl1512_;
   ptr gl1513_;
   static int gl1514_;
   static union dtype_ gl1515_;
   ptr gl1516_;
   static int gl1517_;
   static union dtype_ gl1518_;
   ptr gl1519_;
   static int gl1520_;
   static union dtype_ gl1521_;
   ptr gl1522_;
   static int gl1523_;
   static union dtype_ gl1524_;
   ptr gl1525_;
   static int gl1526_;
   static union dtype_ gl1527_;
   ptr gl1528_;
   static int gl1529_;
   static union dtype_ gl1530_;
   ptr gl1531_;
   static int gl1532_;
   static union dtype_ gl1533_;
   ptr gl1534_;
   static int gl1535_;
   static union dtype_ gl1536_;
   ptr gl1537_;
   static int gl1538_;
   static union dtype_ gl1539_;
   ptr gl1540_;
   static int gl1541_;
   static union dtype_ gl1542_;
   ptr gl1543_;
   static int gl1544_;
   static union dtype_ gl1545_;
   ptr gl1546_;
   static int gl1547_;
   static union dtype_ gl1548_;
   ptr gl1549_;
   static int gl1550_;
   static union dtype_ gl1551_;
   ptr gl1552_;
   static int gl1553_;
   static union dtype_ gl1554_;
   ptr gl1555_;
   static int gl1556_;
   static union dtype_ gl1557_;
   ptr gl1558_;
   static int gl1559_;
   static union dtype_ gl1560_;
   ptr gl1561_;
   static int gl1562_;
   static union dtype_ gl1563_;
   ptr gl1564_;
   static int gl1565_;
   static union dtype_ gl1566_;
   ptr gl1567_;
   static int gl1568_;
   static union dtype_ gl1569_;
   ptr    op1__ = 0;
   ptr    op2__ = 0;

   if (CATT_(self__,4)) {
      res__ = (ptr)PATT_(self__,36);
      goto ret0__;
   }
   else {
   }
   switch (IATT_(self__,24)) {
      case (8) :
      case (9) :
      case (1) :
      case (6) :
      case (7) :
      case (2) :
      case (3) :
      case (4) :
      case (5) :
         CATT_(self__,4) = (char)1;
         goto ret0__;
         break;
      case (10) :
      case (11) :
         gl1495_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1495_,1577,gl1496_,INTVAL_(gl1497_));
         gl1492_ = PATT_(gl1495_,INTVAL_(gl1497_));
         cache_dispatch_(gl1492_,1778,gl1493_,INTVAL_(gl1494_));
         gl134_ = CFN_(gl1494_)(gl1492_);
         if ((! gl134_)) {
            CATT_(self__,4) = (char)1;
            goto ret0__;
         }
         else {
         }
         break;
      case (12) :
      case (13) :
      case (14) :
      case (15) :
      case (16) :
         gl1501_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1501_,1577,gl1502_,INTVAL_(gl1503_));
         gl1498_ = PATT_(gl1501_,INTVAL_(gl1503_));
         cache_dispatch_(gl1498_,1778,gl1499_,INTVAL_(gl1500_));
         gl135_ = CFN_(gl1500_)(gl1498_);
         gl1507_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1507_,1577,gl1508_,INTVAL_(gl1509_));
         gl1504_ = PATT_(gl1507_,INTVAL_(gl1509_));
         cache_dispatch_(gl1504_,1778,gl1505_,INTVAL_(gl1506_));
         gl136_ = CFN_(gl1506_)(gl1504_);
         if ((! (gl135_ & gl136_))) {
            CATT_(self__,4) = (char)1;
            goto ret0__;
         }
         else {
         }
         break;
      default:
         ;
         ;
   }
   op1__ = S_ptr_VOID_;
   op2__ = S_ptr_VOID_;
   switch (IATT_(self__,24)) {
      case (10) :
         gl1510_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1510_,1596,gl1511_,INTVAL_(gl1512_));
         op1__ = (ptr)PFN_(gl1512_)(gl1510_);
         if ((op1__ != 0)) {
            gl1513_ = op1__;
            cache_dispatch_(gl1513_,1737,gl1514_,INTVAL_(gl1515_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_((- STR12_get_i_(PFN_(gl1515_)(gl1513_)))),IATT_(self__,32));
         }
         else {
         }
         break;
      case (11) :
         gl1516_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1516_,1596,gl1517_,INTVAL_(gl1518_));
         op1__ = (ptr)PFN_(gl1518_)(gl1516_);
         if ((op1__ != 0)) {
            gl1519_ = op1__;
            cache_dispatch_(gl1519_,1737,gl1520_,INTVAL_(gl1521_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_(STR12_get_i_(PFN_(gl1521_)(gl1519_))),IATT_(self__,32));
         }
         else {
         }
         break;
      case (13) :
         gl1522_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1522_,1596,gl1523_,INTVAL_(gl1524_));
         op1__ = (ptr)PFN_(gl1524_)(gl1522_);
         gl1525_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1525_,1596,gl1526_,INTVAL_(gl1527_));
         op2__ = (ptr)PFN_(gl1527_)(gl1525_);
         if (((op1__ != 0) & (op2__ != 0))) {
            gl1528_ = op1__;
            cache_dispatch_(gl1528_,1737,gl1529_,INTVAL_(gl1530_));
            gl1531_ = op2__;
            cache_dispatch_(gl1531_,1737,gl1532_,INTVAL_(gl1533_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_((STR12_get_i_(PFN_(gl1530_)(gl1528_)) + STR12_get_i_(PFN_(gl1533_)(gl1531_)))),IATT_(self__,32));
         }
         else {
         }
         break;
      case (14) :
         gl1534_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1534_,1596,gl1535_,INTVAL_(gl1536_));
         op1__ = (ptr)PFN_(gl1536_)(gl1534_);
         gl1537_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1537_,1596,gl1538_,INTVAL_(gl1539_));
         op2__ = (ptr)PFN_(gl1539_)(gl1537_);
         if (((op1__ != 0) & (op2__ != 0))) {
            gl1540_ = op1__;
            cache_dispatch_(gl1540_,1737,gl1541_,INTVAL_(gl1542_));
            gl1543_ = op2__;
            cache_dispatch_(gl1543_,1737,gl1544_,INTVAL_(gl1545_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_((STR12_get_i_(PFN_(gl1542_)(gl1540_)) - STR12_get_i_(PFN_(gl1545_)(gl1543_)))),IATT_(self__,32));
         }
         else {
         }
         break;
      case (15) :
         gl1546_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1546_,1596,gl1547_,INTVAL_(gl1548_));
         op1__ = (ptr)PFN_(gl1548_)(gl1546_);
         gl1549_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1549_,1596,gl1550_,INTVAL_(gl1551_));
         op2__ = (ptr)PFN_(gl1551_)(gl1549_);
         if (((op1__ != 0) & (op2__ != 0))) {
            gl1552_ = op1__;
            cache_dispatch_(gl1552_,1737,gl1553_,INTVAL_(gl1554_));
            gl1555_ = op2__;
            cache_dispatch_(gl1555_,1737,gl1556_,INTVAL_(gl1557_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_((STR12_get_i_(PFN_(gl1554_)(gl1552_)) * STR12_get_i_(PFN_(gl1557_)(gl1555_)))),IATT_(self__,32));
         }
         else {
         }
         break;
      case (16) :
         gl1558_ = PATT_(PATT_(self__,28), 28 + ((0) << 2));
         cache_dispatch_(gl1558_,1596,gl1559_,INTVAL_(gl1560_));
         op1__ = (ptr)PFN_(gl1560_)(gl1558_);
         gl1561_ = PATT_(PATT_(self__,28), 28 + ((1) << 2));
         cache_dispatch_(gl1561_,1596,gl1562_,INTVAL_(gl1563_));
         op2__ = (ptr)PFN_(gl1563_)(gl1561_);
         if (((op1__ != 0) & (op2__ != 0))) {
            gl1564_ = op1__;
            cache_dispatch_(gl1564_,1737,gl1565_,INTVAL_(gl1566_));
            gl1567_ = op2__;
            cache_dispatch_(gl1567_,1737,gl1568_,INTVAL_(gl1569_));
            res__ = (ptr)INT106_create_(0,INT15_to_s_((STR12_get_i_(PFN_(gl1566_)(gl1564_)) / STR12_get_i_(PFN_(gl1569_)(gl1567_)))),IATT_(self__,32));
         }
         else {
         }
         break;
      case (12) :
         break;
      default:
         ;
   }
   CATT_(self__,4) = (char)1;
   PATT_(self__,36) = (ptr)res__;

   ret0__:
   return (res__);
}

void OP_110_get_ext_strs_(ptr self__)
{

   LST120_get_ext_strs_(PATT_(self__,28));

   ret0__:
   return;
}

char OP_110_to_be_pre_evaluated_(ptr self__)
{
   char res__ = S_char_VOID_;


   ret0__:
   return (res__);
}

char OP_110_access_value_only_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)1;

   ret0__:
   return (res__);
}

void OP_110_fob_error_(ptr self__, ptr op_name__, ptr cls_name__)
{
   SATHER_STR_(20,28,ls1682_,"(EXPROB_S): Invalid use of ");
   SATHER_STR_(20,21,ls1683_," on a foreign class ");

   ERR96_format_error_msg_(0,IATT_(self__,32),STR20_s_(STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1682_)),op_name__),(ptr)(&ls1683_)),cls_name__));

   ret0__:
   return;
}

void OP_110_cprint_pre_code_(ptr self__, ptr outfile__)
{

   switch (IATT_(self__,24)) {
      case (8) :
      case (9) :
      case (6) :
      case (7) :
      case (2) :
      case (3) :
      case (4) :
      case (5) :
      case (12) :
      case (13) :
      case (14) :
      case (15) :
      case (16) :
         OP_110_cprint_ith_child_pre_code_(self__,0,outfile__);
         OP_110_cprint_ith_child_pre_code_(self__,1,outfile__);
         break;
      case (1) :
      case (10) :
      case (11) :
         OP_110_cprint_ith_child_pre_code_(self__,0,outfile__);
         break;
      default:
         ;
         ;
   }

   ret0__:
   return;
}

void OP_110_cprint_act_code_(ptr self__, ptr outfile__)
{
   SATHER_STR_(20,4,ls1785_," & ");
   SATHER_STR_(20,4,ls1786_," | ");
   SATHER_STR_(20,3,ls1787_,"! ");
   SATHER_STR_(20,5,ls1788_," == ");
   SATHER_STR_(20,5,ls1789_," != ");
   SATHER_STR_(20,4,ls1757_," < ");
   SATHER_STR_(20,4,ls1728_," > ");
   SATHER_STR_(20,5,ls1758_," <= ");
   SATHER_STR_(20,5,ls1759_," >= ");
   SATHER_STR_(20,3,ls1790_,"- ");
   SATHER_STR_(20,24,ls1791_,"/* '^' not supported */");
   SATHER_STR_(20,4,ls1763_," + ");
   SATHER_STR_(20,4,ls1764_," - ");
   SATHER_STR_(20,4,ls1765_," * ");
   SATHER_STR_(20,4,ls1766_," / ");

   OP_110_cprint_cast_code_(self__,outfile__);
   (void)SAT99_c_(outfile__,'(');
   switch (IATT_(self__,24)) {
      case (8) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1785_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (9) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1786_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (1) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1787_));
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         break;
      case (6) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1788_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (7) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1789_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (2) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1757_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (3) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1728_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (4) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1758_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (5) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1759_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (10) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1790_));
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         break;
      case (11) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         break;
      case (12) :
         (void)SAT99_s_(outfile__,(ptr)(&ls1791_));
         break;
      case (13) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1763_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (14) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1764_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (15) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1765_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      case (16) :
         OP_110_cprint_ith_child_act_code_(self__,0,outfile__);
         (void)SAT99_s_(outfile__,(ptr)(&ls1766_));
         OP_110_cprint_ith_child_act_code_(self__,1,outfile__);
         break;
      default:
         ;
         ;
   }
   (void)SAT99_c_(outfile__,')');

   ret0__:
   return;
}

void OP_110_cprint_cast_code_(ptr self__, ptr outfile__)
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

void OP_110_cprint_ith_child_pre_code_(ptr self__, int i__, ptr outfile__)
{
   SATHER_STR_(20,4,ls964_," = ");
   SATHER_STR_(20,3,ls650_,";\n");
   ptr gl1570_;
   static int gl1571_;
   static union dtype_ gl1572_;
   ptr gl1573_;
   static int gl1574_;
   static union dtype_ gl1575_;
   ptr    ith_child__ = 0;
   int    ith_temp__ = S_int_VOID_;

   ith_child__ = (ptr)PATT_(PATT_(self__,28), 28 + ((i__) << 2));
   ith_temp__ = (int)IATT_(PATT_(self__,44), 8 + ((i__) << 2));
   gl1570_ = ith_child__;
   cache_dispatch_(gl1570_,1589,gl1571_,INTVAL_(gl1572_));
   VFN_(gl1572_)(gl1570_,outfile__);
   if ((ith_temp__ > 0)) {
      (void)SAT99_indent_(outfile__);
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
      (void)SAT99_s_(outfile__,(ptr)(&ls964_));
      gl1573_ = ith_child__;
      cache_dispatch_(gl1573_,965,gl1574_,INTVAL_(gl1575_));
      VFN_(gl1575_)(gl1573_,outfile__);
      (void)SAT99_inc_ln_(SAT99_s_(outfile__,(ptr)(&ls650_)),1);
   }
   else {
   }

   ret0__:
   return;
}

void OP_110_cprint_ith_child_act_code_(ptr self__, int i__, ptr outfile__)
{
   ptr gl1576_;
   static int gl1577_;
   static union dtype_ gl1578_;
   int    ith_temp__ = S_int_VOID_;

   ith_temp__ = (int)IATT_(PATT_(self__,44), 8 + ((i__) << 2));
   if ((ith_temp__ > 0)) {
      GLO94_cprint_ctemp_name_(0,ith_temp__,outfile__);
   }
   else {
      gl1576_ = PATT_(PATT_(self__,28), 28 + ((i__) << 2));
      cache_dispatch_(gl1576_,965,gl1577_,INTVAL_(gl1578_));
      VFN_(gl1578_)(gl1576_,outfile__);
   }

   ret0__:
   return;
}

