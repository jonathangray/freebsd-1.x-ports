/* op_exp31.c : Sather class: OP_EXPROB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern /*shared*/ int GLO68_curr_lineno_;
extern ptr LST37_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST37_create_(ptr self__, int init_size__);
extern ptr LST37_push_(ptr self__, ptr e__);
extern ptr OP_110_create_(ptr self__, int op__, ptr children__, int ln__);
#include "macros_.h"



/*constant*/ int OP_31_not_op_ind_ = 1;
/*constant*/ int OP_31_lt_op_ind_ = 2;
/*constant*/ int OP_31_gt_op_ind_ = 3;
/*constant*/ int OP_31_le_op_ind_ = 4;
/*constant*/ int OP_31_ge_op_ind_ = 5;
/*constant*/ int OP_31_eq_op_ind_ = 6;
/*constant*/ int OP_31_ne_op_ind_ = 7;
/*constant*/ int OP_31_and_op_ind_ = 8;
/*constant*/ int OP_31_or_op_ind_ = 9;
/*constant*/ int OP_31_uminus_op_ind_ = 10;
/*constant*/ int OP_31_uplus_op_ind_ = 11;
/*constant*/ int OP_31_exp_op_ind_ = 12;
/*constant*/ int OP_31_plus_op_ind_ = 13;
/*constant*/ int OP_31_minus_op_ind_ = 14;
/*constant*/ int OP_31_mult_op_ind_ = 15;
/*constant*/ int OP_31_divide_op_ind_ = 16;
ptr OP_31_initialize_(ptr self__, ptr initarg__);
/*constant*/ int OP_31_print_indent_ = 2;
ptr OP_31_create_(ptr self__);
void OP_31_out_of_line_(ptr self__, ptr fn__);
ptr OP_31_dup_(ptr self__);
void OP_31_put_kwdname_(ptr self__, int nm__);
ptr OP_31_sather_code_(ptr self__);
ptr OP_31_pcopy_(ptr self__, ptr pl__, ptr pi__);
ptr OP_31_create_unary_(ptr self__, int op__, ptr child__);
ptr OP_31_create_binary_(ptr self__, int op__, ptr left__, ptr right__);
extern int attr_ent_OP_31[];

ptr OP_31_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr OP_31_create_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(31,0);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

void OP_31_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr OP_31_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void OP_31_put_kwdname_(ptr self__, int nm__)
{
   ptr gl567_;
   static int gl568_;
   static union dtype_ gl569_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl567_ = x__;
   cache_dispatch_(gl567_,796,gl568_,INTVAL_(gl569_));
   IATT_(gl567_,INTVAL_(gl569_)) = (int)nm__;

   ret0__:
   return;
}

ptr OP_31_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr OP_31_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;

   res__ = (ptr)OP_110_create_(res__,IATT_(self__,12),LST37_pcopy_(PATT_(self__,16),pl__,pi__),IATT_(self__,4));

   ret0__:
   return (res__);
}

ptr OP_31_create_unary_(ptr self__, int op__, ptr child__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(31,0);
   IATT_(res__,12) = (int)op__;
   PATT_(res__,16) = (ptr)LST37_push_(LST37_create_(PATT_(res__,16),1),child__);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

ptr OP_31_create_binary_(ptr self__, int op__, ptr left__, ptr right__)
{
   ptr res__ = 0;

   res__ = (ptr)new_(31,0);
   IATT_(res__,12) = (int)op__;
   PATT_(res__,16) = (ptr)LST37_push_(LST37_push_(LST37_create_(PATT_(res__,16),2),left__),right__);
   IATT_(res__,4) = (int)GLO68_curr_lineno_;

   ret0__:
   return (res__);
}

