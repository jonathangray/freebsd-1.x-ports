/* dbtabl190.c : Sather class: DBTABLE, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern void FIL11_open_for_write_(ptr self__, ptr nm__);
extern int FIL11_error_(ptr self__);
extern ptr FIL11_i_(ptr self__, int in__);
extern ptr FIL11_nl_(ptr self__);
extern ptr FIL11_s_(ptr self__, ptr st__);
extern ptr FIL11_c_(ptr self__, char ch__);
extern void FIL11_close_(ptr self__);
extern /*shared*/ ptr COM82_target_dir_;
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern void SAT95_error_msg_(ptr self__, ptr s__);
extern ptr SAT232_create_(ptr self__, ptr sfn__, int sln__);
extern void ARR233_clear_(ptr self__);
extern ptr STR234_create_(ptr self__);
extern ptr STR235_create_(ptr self__);
extern int STR234_get_(ptr self__, ptr s__, int i__);
extern void STR234_insert_(ptr self__, ptr s__, int i__, int v__);
extern void SAT232_add_(ptr self__, ptr cfname__, int ln__);
extern void SOR236_qsort_(ptr self__, ptr arr__, int u__, int l__);
extern char STR235_insert_(ptr self__, ptr s__, int v__);
extern ptr STR235_gets_(ptr self__, int i__);
extern int STR235_geti_(ptr self__, ptr s__);
#include "macros_.h"



/*constant*/ int DBT190_defSize_ = 20;
/*shared*/ ptr DBT190_entries_;
/*shared*/ int DBT190_curLength_;
/*shared*/ ptr DBT190_entryMap_;
/*shared*/ ptr DBT190_satherFiles_;
/*shared*/ int DBT190_numSatherFiles_;
/*shared*/ ptr DBT190_cFiles_;
/*shared*/ int DBT190_numCFiles_;
void DBT190_init_(ptr self__);
void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__);
void DBT190_sort_(ptr self__);
void DBT190_print_(ptr self__);
ptr DBT190_initialize_(ptr self__, ptr initarg__);
extern int attr_ent_DBT190[];

void DBT190_init_(ptr self__)
{

   DBT190_entries_ = (ptr)new1_(233,20,0);
   DBT190_curLength_ = (int)0;
   DBT190_entryMap_ = (ptr)STR234_create_(0);
   DBT190_satherFiles_ = (ptr)STR235_create_(0);
   DBT190_cFiles_ = (ptr)STR235_create_(0);

   ret0__:
   return;
}

void DBT190_addCLine_(ptr self__, ptr satherFileName__, int satherLineNo__, ptr cFileName__, int cClineNo__)
{
   int    loc__ = S_int_VOID_;
   ptr    a__ = 0;
   ptr    sle__ = 0;

   loc__ = S_int_VOID_;
   loc__ = (int)STR234_get_(DBT190_entryMap_,satherFileName__,satherLineNo__);
   if ((loc__ == (- 1))) {
      STR234_insert_(DBT190_entryMap_,satherFileName__,satherLineNo__,DBT190_curLength_);
      if ((DBT190_curLength_ >= IATT_(DBT190_entries_,4))) {
         a__ = (ptr)DBT190_entries_;
         DBT190_entries_ = (ptr)extend1_(a__,(2 * IATT_(a__,4)),0);
         ARR233_clear_(a__);
      }
      else {
      }
      sle__ = (ptr)SAT232_create_(0,satherFileName__,satherLineNo__);
      SAT232_add_(sle__,cFileName__,cClineNo__);
      PATT_(DBT190_entries_, 8 + ((DBT190_curLength_) << 2)) = (ptr)sle__;
      DBT190_curLength_ = (int)(DBT190_curLength_ + 1);
   }
   else {
      SAT232_add_(PATT_(DBT190_entries_, 8 + ((loc__) << 2)),cFileName__,cClineNo__);
   }

   ret0__:
   return;
}

void DBT190_sort_(ptr self__)
{
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;

   i__ = S_int_VOID_;
   j__ = S_int_VOID_;
   SOR236_qsort_(0,DBT190_entries_,0,(DBT190_curLength_ - 1));
   i__ = (int)0;
   while (1) {
      if ((i__ >= DBT190_curLength_)) {
         goto goto_tag_4677_;
      }
      else {
      }
      if (STR235_insert_(DBT190_satherFiles_,PATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),8),DBT190_numSatherFiles_)) {
         DBT190_numSatherFiles_ = (int)(DBT190_numSatherFiles_ + 1);
      }
      else {
      }
      j__ = (int)0;
      while (1) {
         if ((j__ >= IATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),16))) {
            goto goto_tag_4678_;
         }
         else {
         }
         if (STR235_insert_(DBT190_cFiles_,PATT_(PATT_(PATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),4), 8 + ((j__) << 2)),4),DBT190_numCFiles_)) {
            DBT190_numCFiles_ = (int)(DBT190_numCFiles_ + 1);
         }
         else {
         }
         j__ = (int)(j__ + 1);
      }
   goto_tag_4678_: ;
      i__ = (int)(i__ + 1);
   }
goto_tag_4677_: ;

   ret0__:
   return;
}

void DBT190_print_(ptr self__)
{
   SATHER_STR_(20,13,ls2211_,"_dbg_output_");
   SATHER_STR_(20,19,ls1253_,"Error in opening \"");
   SATHER_STR_(20,3,ls632_,"\"\n");
   SATHER_STR_(20,2,ls2189_,":");
   SATHER_STR_(20,2,ls2190_," ");
   int    i__ = S_int_VOID_;
   int    j__ = S_int_VOID_;
   int    prev__ = S_int_VOID_;
   ptr    outfile__ = 0;
   ptr    outfname__ = 0;
   int    item__ = S_int_VOID_;

   i__ = S_int_VOID_;
   j__ = S_int_VOID_;
   prev__ = (int)(- 1);
   DBT190_sort_(self__);
   outfile__ = (ptr)new_(11,0);
   outfname__ = (ptr)STR20_s_(STR20_s_(STR20_create_(0),COM82_target_dir_),(ptr)(&ls2211_));
   FIL11_open_for_write_(outfile__,outfname__);
   if ((FIL11_error_(outfile__) != 0)) {
      SAT95_error_msg_(0,STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls1253_)),outfname__),(ptr)(&ls632_)));
      goto ret0__;
   }
   else {
   }
   (void)FIL11_nl_(FIL11_i_(outfile__,DBT190_numSatherFiles_));
   i__ = (int)0;
   while (1) {
      if ((i__ >= DBT190_numSatherFiles_)) {
         goto goto_tag_4679_;
      }
      else {
      }
      (void)FIL11_nl_(FIL11_s_(outfile__,STR235_gets_(DBT190_satherFiles_,i__)));
      i__ = (int)(i__ + 1);
   }
goto_tag_4679_: ;
   (void)FIL11_nl_(FIL11_i_(outfile__,DBT190_numCFiles_));
   i__ = (int)0;
   while (1) {
      if ((i__ >= DBT190_numCFiles_)) {
         goto goto_tag_4680_;
      }
      else {
      }
      (void)FIL11_nl_(FIL11_s_(outfile__,STR235_gets_(DBT190_cFiles_,i__)));
      i__ = (int)(i__ + 1);
   }
goto_tag_4680_: ;
   i__ = (int)0;
   while (1) {
      if ((i__ >= DBT190_curLength_)) {
         goto goto_tag_4681_;
      }
      else {
      }
      item__ = (int)STR235_geti_(DBT190_satherFiles_,PATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),8));
      if ((item__ != prev__)) {
         (void)FIL11_nl_(FIL11_s_(FIL11_i_(FIL11_c_(outfile__,'_'),item__),(ptr)(&ls2189_)));
      }
      else {
      }
      (void)FIL11_i_(outfile__,IATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),12));
      prev__ = (int)item__;
      j__ = (int)0;
      while (1) {
         if ((j__ >= IATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),16))) {
            goto goto_tag_4682_;
         }
         else {
         }
         (void)FIL11_i_(FIL11_s_(FIL11_i_(FIL11_s_(outfile__,(ptr)(&ls2190_)),STR235_geti_(DBT190_cFiles_,PATT_(PATT_(PATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),4), 8 + ((j__) << 2)),4))),(ptr)(&ls2189_)),IATT_(PATT_(PATT_(PATT_(DBT190_entries_, 8 + ((i__) << 2)),4), 8 + ((j__) << 2)),8));
         j__ = (int)(j__ + 1);
      }
   goto_tag_4682_: ;
      (void)FIL11_nl_(outfile__);
      i__ = (int)(i__ + 1);
   }
goto_tag_4681_: ;
   FIL11_close_(outfile__);

   ret0__:
   return;
}

ptr DBT190_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

