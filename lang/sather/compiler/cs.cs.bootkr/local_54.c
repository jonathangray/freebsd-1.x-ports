/* local_54.c : Sather class: LOCAL_DECL_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_();
extern ptr ERR96_filename_();
extern ERR96_error_exit_();
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr EXP36_pcopy_();
extern ptr LST38_create_();
extern ptr LST38_push_();
extern ptr LVA145_create_();
extern ptr TYP114_pcopy_();
extern ptr LVA146_create_();
extern ptr STR20_create_();
extern ptr STR20_s_();
extern ptr STR20_c_();
extern ptr STR20_i_();
extern ptr ID_25_create_();
#include "macros_.h"



/*constant*/ int LOC54_print_indent_ = 2;
ptr LOC54_create_();
LOC54_out_of_line_();
ptr LOC54_dup_();
LOC54_put_kwdname_();
ptr LOC54_sather_code_();
ptr LOC54_initialize_();
ptr LOC54_pcopy_();
LOC54_mark_private_();
ptr LOC54_create_lst_();
int LOC54_ith_name_();
extern int attr_ent_LOC54[];

ptr LOC54_create_(self__,decl__,exp__)
ptr self__;
ptr decl__;
ptr exp__;
{
   ptr res__ = 0;

   res__ = (ptr)new_(54,0);
   PATT_(res__,20) = (ptr)PATT_(decl__,20);
   PATT_(res__,12) = (ptr)PATT_(decl__,12);
   PATT_(res__,24) = (ptr)exp__;
   IATT_(res__,16) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

LOC54_out_of_line_(self__,fn__)
ptr self__;
ptr fn__;
{

   IATT_(self__,16) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,16));

   ret0__:
   return;
}

ptr LOC54_dup_(self__)
ptr self__;
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

LOC54_put_kwdname_(self__,nm__)
ptr self__;
int nm__;
{
   ptr gl804_;
   static int gl805_;
   static union dtype_ gl806_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl804_ = x__;
   cache_dispatch_(gl804_,796,gl805_,INTVAL_(gl806_));
   IATT_(gl804_,INTVAL_(gl806_)) = (int)nm__;

   ret0__:
   return;
}

ptr LOC54_sather_code_(self__)
ptr self__;
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LOC54_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LOC54_pcopy_(self__,pl__,pi__)
ptr self__;
ptr pl__;
ptr pi__;
{
   ptr res__ = 0;
   ptr gl807_;
   static int gl808_;
   static union dtype_ gl809_;
   ptr gl810_;
   static int gl811_;
   static union dtype_ gl812_;
   ptr gl37_;
   ptr gl38_;
   ptr gl813_;
   static int gl814_;
   static union dtype_ gl815_;
   ptr gl39_;

   if ((PATT_(self__,24) != 0)) {
      gl807_ = PATT_(self__,12);
      cache_dispatch_(gl807_,407,gl808_,INTVAL_(gl809_));
      gl37_ = PFN_(gl809_)(gl807_,pl__,pi__);
      gl810_ = PATT_(self__,24);
      cache_dispatch_(gl810_,407,gl811_,INTVAL_(gl812_));
      gl38_ = PFN_(gl812_)(gl810_,pl__,pi__);
      res__ = (ptr)LVA145_create_(res__,IATT_(PATT_(self__,20), 16 + ((0) << 2)),gl37_,gl38_,IATT_(self__,16));
   }
   else {
      gl813_ = PATT_(self__,12);
      cache_dispatch_(gl813_,407,gl814_,INTVAL_(gl815_));
      gl39_ = PFN_(gl815_)(gl813_,pl__,pi__);
      res__ = (ptr)LVA145_create_(res__,IATT_(PATT_(self__,20), 16 + ((0) << 2)),gl39_,0,IATT_(self__,16));
   }

   ret0__:
   return (res__);
}

LOC54_mark_private_(self__)
ptr self__;
{

   CATT_(self__,4) = (char)1;

   ret0__:
   return;
}

ptr LOC54_create_lst_(self__,decl__)
ptr self__;
ptr decl__;
{
   ptr res__ = 0;
   SATHER_STR_(20,53,ls2540_," (LOCAL_DECL_STMTOB) : No name found in declaration\n");
   int    i__ = S_int_VOID_;
   int    num_names__ = S_int_VOID_;
   int    first__ = S_int_VOID_;
   int    nm__ = S_int_VOID_;
   ptr    tp__ = 0;
   ptr    lv_stmtob__ = 0;

   i__ = (int)0;
   num_names__ = S_int_VOID_;
   if ((PATT_(decl__,20) != 0)) {
      num_names__ = (int)IATT_(PATT_(decl__,20),4);
   }
   else {
   }
   res__ = (ptr)LST38_create_(res__,num_names__);
   first__ = S_int_VOID_;
   if ((num_names__ == 0)) {
      ERR96_error_exit_(0,STR20_s_(STR20_c_(STR20_i_(STR20_c_(STR20_s_(STR20_create_(0),ERR96_filename_(0)),'('),IATT_(decl__,16)),')'),(ptr)(&ls2540_)));
   }
   else {
      first__ = (int)LOC54_ith_name_(decl__,0);
   }
   nm__ = S_int_VOID_;
   tp__ = S_ptr_VOID_;
   lv_stmtob__ = S_ptr_VOID_;
   while (1) {
      if ((i__ >= num_names__)) {
         goto goto_tag_816_;
      }
      else {
      }
      nm__ = (int)LOC54_ith_name_(decl__,i__);
      tp__ = (ptr)PATT_(decl__,12);
      if (((i__ > 0) & (PATT_(decl__,24) != 0))) {
         lv_stmtob__ = (ptr)LVA146_create_(lv_stmtob__,nm__,tp__,ID_25_create_(0,first__),IATT_(decl__,16));
      }
      else {
         lv_stmtob__ = (ptr)LVA146_create_(lv_stmtob__,nm__,tp__,PATT_(decl__,24),IATT_(decl__,16));
      }
      res__ = (ptr)LST38_push_(res__,lv_stmtob__);
      i__ = (int)(i__ + 1);
   }
goto_tag_816_: ;

   ret0__:
   return (res__);
}

int LOC54_ith_name_(self__,i__)
ptr self__;
int i__;
{
   int res__ = S_int_VOID_;

   res__ = (int)IATT_(PATT_(self__,20), 16 + ((i__) << 2));

   ret0__:
   return (res__);
}

