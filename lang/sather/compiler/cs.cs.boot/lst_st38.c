/* lst_st38.c : Sather class: LST_STMTOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern ptr STM121_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern ptr LST123_create_(ptr self__, int init_size__);
extern ptr LST123_push_(ptr self__, ptr e__);
#include "macros_.h"



/*constant*/ int LST38_print_indent_ = 2;
ptr LST38_create_(ptr self__, int init_size__);
void LST38_out_of_line_(ptr self__, ptr fn__);
ptr LST38_dup_(ptr self__);
void LST38_put_kwdname_(ptr self__, int nm__);
ptr LST38_sather_code_(ptr self__);
ptr LST38_initialize_(ptr self__, ptr initarg__);
ptr LST38_pcopy_(ptr self__, ptr pl__, ptr pi__);
void LST38_clear_(ptr self__);
/*constant*/ int LST38_def_init_size_ = 5;
ptr LST38_push_(ptr self__, ptr e__);
int LST38_size_(ptr self__);
char LST38_is_empty_(ptr self__);
ptr LST38_pop_(ptr self__);
ptr LST38_top_(ptr self__);
void LST38_init_iterate_(ptr self__);
ptr LST38_curr_item_(ptr self__);
ptr LST38_next_item_(ptr self__);
ptr LST38_prev_item_(ptr self__);
ptr LST38_push_unique_(ptr self__, ptr e__);
ptr LST38_append_(ptr self__, ptr list__);
ptr LST38_union_(ptr self__, ptr list__);
char LST38_not_in_(ptr self__, ptr e__);
int LST38_contains_(ptr self__, ptr e__);
extern int attr_ent_LST38[];

ptr LST38_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(38,5,0);
   }
   else {
      res__ = (ptr)new1_(38,init_size__,0);
   }

   ret0__:
   return (res__);
}

void LST38_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LST38_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void LST38_put_kwdname_(ptr self__, int nm__)
{
   ptr gl621_;
   static int gl622_;
   static union dtype_ gl623_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl621_ = x__;
   cache_dispatch_(gl621_,796,gl622_,INTVAL_(gl623_));
   IATT_(gl621_,INTVAL_(gl623_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST38_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST38_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LST38_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl624_;
   static int gl625_;
   static union dtype_ gl626_;
   ptr gl15_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST123_create_(res__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_627_;
      }
      else {
      }
      gl624_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl624_,407,gl625_,INTVAL_(gl626_));
      gl15_ = PFN_(gl626_)(gl624_,pl__,pi__);
      res__ = (ptr)LST123_push_(res__,gl15_);
      i__ = (int)(i__ + 1);
   }
goto_tag_627_: ;

   ret0__:
   return (res__);
}

void LST38_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,20))) {
         goto goto_tag_628_;
      }
      else {
      }
      PATT_(self__, 24 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_628_: ;

   ret0__:
   return;
}

ptr LST38_push_(ptr self__, ptr e__)
{
   ptr res__ = 0;

   if ((IATT_(self__,12) < IATT_(self__,20))) {
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)extend1_(self__,(2 * IATT_(self__,20)),0);
   }
   PATT_(res__, 24 + ((IATT_(self__,12)) << 2)) = (ptr)e__;
   IATT_(res__,12) = (int)(IATT_(res__,12) + 1);

   ret0__:
   return (res__);
}

int LST38_size_(ptr self__)
{
   int res__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (int)0;
   }
   else {
      res__ = (int)IATT_(self__,12);
   }

   ret0__:
   return (res__);
}

char LST38_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST38_pop_(ptr self__)
{
   ptr res__ = 0;

   if (LST38_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      IATT_(self__,12) = (int)(IATT_(self__,12) - 1);
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,12)) << 2));
      PATT_(self__, 24 + ((IATT_(self__,12)) << 2)) = (ptr)0;
   }

   ret0__:
   return (res__);
}

ptr LST38_top_(ptr self__)
{
   ptr res__ = 0;

   if (LST38_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 24 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LST38_init_iterate_(ptr self__)
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST38_curr_item_(ptr self__)
{
   ptr res__ = 0;

   if ((IATT_(self__,16) < IATT_(self__,12))) {
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST38_next_item_(ptr self__)
{
   ptr res__ = 0;

   if (((IATT_(self__,16) + 1) < IATT_(self__,12))) {
      IATT_(self__,16) = (int)(IATT_(self__,16) + 1);
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST38_prev_item_(ptr self__)
{
   ptr res__ = 0;

   if (((IATT_(self__,16) - 1) >= 0)) {
      IATT_(self__,16) = (int)(IATT_(self__,16) - 1);
      res__ = (ptr)PATT_(self__, 24 + ((IATT_(self__,16)) << 2));
   }
   else {
   }

   ret0__:
   return (res__);
}

ptr LST38_push_unique_(ptr self__, ptr e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST38_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 24 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST38_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST38_append_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   if ((list__ == 0)) {
      res__ = (ptr)self__;
      goto ret0__;
   }
   else {
   }
   res__ = (ptr)self__;
   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_629_;
      }
      else {
      }
      res__ = (ptr)LST38_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_629_: ;

   ret0__:
   return (res__);
}

ptr LST38_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_630_;
      }
      else {
      }
      if (LST38_not_in_(self__,PATT_(list__, 24 + ((i__) << 2)))) {
         res__ = (ptr)LST38_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_630_: ;

   ret0__:
   return (res__);
}

char LST38_not_in_(ptr self__, ptr e__)
{
   char res__ = S_char_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      res__ = (char)1;
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (char)1;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_631_;
      }
      else {
      }
      if ((PATT_(self__, 24 + ((i__) << 2)) == e__)) {
         res__ = (char)0;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_631_: ;

   ret0__:
   return (res__);
}

int LST38_contains_(ptr self__, ptr e__)
{
   int res__ = S_int_VOID_;
   int    i__ = S_int_VOID_;

   if ((self__ == 0)) {
      goto ret0__;
   }
   else {
   }
   i__ = (int)0;
   res__ = (int)(- 1);
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_632_;
      }
      else {
      }
      if ((PATT_(self__, 24 + ((i__) << 2)) == e__)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_632_: ;

   ret0__:
   return (res__);
}

