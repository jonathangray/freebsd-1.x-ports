/* sorter236.c : Sather class: SORTER, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int SOR248_comp_();
#include "macros_.h"



/*shared*/ ptr SOR236_array_;
SOR236_qsortdoit_();
SOR236_qsort_();
ptr SOR236_initialize_();
extern int attr_ent_SOR236[];

SOR236_qsortdoit_(self__,l__,u__)
ptr self__;
int l__;
int u__;
{
   ptr gl4843_;
   static int gl4844_;
   static union dtype_ gl4845_;
   int gl487_;
   int    r__ = S_int_VOID_;
   ptr    t__ = 0;
   int    m__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    s__ = 0;

   if ((u__ > l__)) {
      r__ = (int)((l__ + u__) / 2);
      t__ = (ptr)PATT_(SOR236_array_, 8 + ((r__) << 2));
      PATT_(SOR236_array_, 8 + ((r__) << 2)) = (ptr)PATT_(SOR236_array_, 8 + ((l__) << 2));
      PATT_(SOR236_array_, 8 + ((l__) << 2)) = (ptr)t__;
      m__ = (int)l__;
      i__ = (int)(l__ + 1);
      s__ = (ptr)PATT_(SOR236_array_, 8 + ((m__) << 2));
      while (1) {
         if ((i__ > u__)) {
            goto goto_tag_4846_;
         }
         else {
         }
         gl4843_ = PATT_(SOR236_array_, 8 + ((i__) << 2));
         cache_dispatch_(gl4843_,2191,gl4844_,INTVAL_(gl4845_));
         gl487_ = IFN_(gl4845_)(gl4843_,t__);
         if ((gl487_ < 0)) {
            m__ = (int)(m__ + 1);
            s__ = (ptr)PATT_(SOR236_array_, 8 + ((m__) << 2));
            PATT_(SOR236_array_, 8 + ((m__) << 2)) = (ptr)PATT_(SOR236_array_, 8 + ((i__) << 2));
            PATT_(SOR236_array_, 8 + ((i__) << 2)) = (ptr)s__;
         }
         else {
         }
         i__ = (int)(i__ + 1);
      }
   goto_tag_4846_: ;
      s__ = (ptr)PATT_(SOR236_array_, 8 + ((l__) << 2));
      PATT_(SOR236_array_, 8 + ((l__) << 2)) = (ptr)PATT_(SOR236_array_, 8 + ((m__) << 2));
      PATT_(SOR236_array_, 8 + ((m__) << 2)) = (ptr)s__;
      SOR236_qsortdoit_(self__,l__,(m__ - 1));
      SOR236_qsortdoit_(self__,(m__ + 1),u__);
   }
   else {
   }

   ret0__:
   return;
}

SOR236_qsort_(self__,arr__,u__,l__)
ptr self__;
ptr arr__;
int u__;
int l__;
{

   SOR236_array_ = (ptr)arr__;
   SOR236_qsortdoit_(self__,u__,l__);

   ret0__:
   return;
}

ptr SOR236_initialize_(self__,initarg__)
ptr self__;
ptr initarg__;
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

