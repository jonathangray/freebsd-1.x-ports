/* lst_fe43.c : Sather class: LST_FEATOB, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern ptr FEA131_pcopy_(ptr self__, ptr pl__, ptr pi__);
extern void FEA131_mark_private_(ptr self__);
extern ptr LST133_create_(ptr self__, int init_size__);
extern ptr LST133_push_(ptr self__, ptr e__);
extern void FEA131_mark_spec_(ptr self__);
extern void FEA131_mark_abstract_(ptr self__);
extern void FEA131_mark_shared_(ptr self__);
extern void FEA131_mark_readonly_(ptr self__);
extern /*shared*/ ptr GLO68_curr_filename_;
extern /*shared*/ ptr GLO68_str_table_;
extern ptr STR69_at_index_(ptr self__, int i__);
extern ptr STR20_create_(ptr self__);
extern ptr STR20_s_(ptr self__, ptr st__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void ERR96_format_warning_msg_file_(ptr self__, ptr file__, int ln__, ptr cls__, ptr s__);
extern void FEA162_mark_abstract_(ptr self__);
extern void FEA162_mark_readonly_(ptr self__);
#include "macros_.h"



/*constant*/ int LST43_print_indent_ = 2;
ptr LST43_create_(ptr self__, int init_size__);
void LST43_out_of_line_(ptr self__, ptr fn__);
ptr LST43_dup_(ptr self__);
void LST43_put_kwdname_(ptr self__, int nm__);
ptr LST43_sather_code_(ptr self__);
ptr LST43_initialize_(ptr self__, ptr initarg__);
ptr LST43_pcopy_(ptr self__, ptr pl__, ptr pi__);
void LST43_clear_(ptr self__);
/*constant*/ int LST43_def_init_size_ = 5;
ptr LST43_push_(ptr self__, ptr e__);
int LST43_size_(ptr self__);
char LST43_is_empty_(ptr self__);
ptr LST43_pop_(ptr self__);
ptr LST43_top_(ptr self__);
void LST43_init_iterate_(ptr self__);
ptr LST43_curr_item_(ptr self__);
ptr LST43_next_item_(ptr self__);
ptr LST43_prev_item_(ptr self__);
ptr LST43_push_unique_(ptr self__, ptr e__);
ptr LST43_append_(ptr self__, ptr list__);
ptr LST43_union_(ptr self__, ptr list__);
char LST43_not_in_(ptr self__, ptr e__);
int LST43_contains_(ptr self__, ptr e__);
void LST43_mark_private_(ptr self__);
void LST43_mark_spec_(ptr self__);
void LST43_mark_abstract_(ptr self__);
void LST43_mark_shared_(ptr self__);
void LST43_mark_readonly_(ptr self__);
/*shared*/ char LST43_duplicate_defs_ok_;
ptr LST43_add_unique_feat_(ptr self__, ptr f__);
extern int attr_ent_LST43[];

ptr LST43_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(43,5,0);
   }
   else {
      res__ = (ptr)new1_(43,init_size__,0);
   }

   ret0__:
   return (res__);
}

void LST43_out_of_line_(ptr self__, ptr fn__)
{

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));

   ret0__:
   return;
}

ptr LST43_dup_(ptr self__)
{
   ptr res__ = 0;

   res__ = (ptr)copy_(self__,0);

   ret0__:
   return (res__);
}

void LST43_put_kwdname_(ptr self__, int nm__)
{
   ptr gl669_;
   static int gl670_;
   static union dtype_ gl671_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl669_ = x__;
   cache_dispatch_(gl669_,796,gl670_,INTVAL_(gl671_));
   IATT_(gl669_,INTVAL_(gl671_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST43_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST43_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

ptr LST43_pcopy_(ptr self__, ptr pl__, ptr pi__)
{
   ptr res__ = 0;
   ptr gl672_;
   static int gl673_;
   static union dtype_ gl674_;
   ptr gl18_;
   ptr gl675_;
   static int gl676_;
   static union dtype_ gl677_;
   ptr gl678_;
   static int gl679_;
   static union dtype_ gl680_;
   ptr gl681_;
   static int gl682_;
   static union dtype_ gl683_;
   ptr gl684_;
   static int gl685_;
   static union dtype_ gl686_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST133_create_(res__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_687_;
      }
      else {
      }
      gl672_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl672_,407,gl673_,INTVAL_(gl674_));
      gl18_ = PFN_(gl674_)(gl672_,pl__,pi__);
      res__ = (ptr)LST133_push_(res__,gl18_);
      gl675_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl675_,309,gl676_,INTVAL_(gl677_));
      if (CATT_(gl675_,INTVAL_(gl677_))) {
         gl678_ = PATT_(res__, 24 + ((i__) << 2));
         cache_dispatch_(gl678_,494,gl679_,INTVAL_(gl680_));
         VFN_(gl680_)(gl678_);
      }
      else {
      }
      gl681_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl681_,2094,gl682_,INTVAL_(gl683_));
      if (CATT_(gl681_,INTVAL_(gl683_))) {
         gl684_ = PATT_(res__, 24 + ((i__) << 2));
         cache_dispatch_(gl684_,2097,gl685_,INTVAL_(gl686_));
         VFN_(gl686_)(gl684_);
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_687_: ;

   ret0__:
   return (res__);
}

void LST43_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,20))) {
         goto goto_tag_688_;
      }
      else {
      }
      PATT_(self__, 24 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_688_: ;

   ret0__:
   return;
}

ptr LST43_push_(ptr self__, ptr e__)
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

int LST43_size_(ptr self__)
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

char LST43_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST43_pop_(ptr self__)
{
   ptr res__ = 0;

   if (LST43_is_empty_(self__)) {
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

ptr LST43_top_(ptr self__)
{
   ptr res__ = 0;

   if (LST43_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 24 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LST43_init_iterate_(ptr self__)
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST43_curr_item_(ptr self__)
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

ptr LST43_next_item_(ptr self__)
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

ptr LST43_prev_item_(ptr self__)
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

ptr LST43_push_unique_(ptr self__, ptr e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST43_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 24 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST43_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST43_append_(ptr self__, ptr list__)
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
         goto goto_tag_689_;
      }
      else {
      }
      res__ = (ptr)LST43_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_689_: ;

   ret0__:
   return (res__);
}

ptr LST43_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_690_;
      }
      else {
      }
      if (LST43_not_in_(self__,PATT_(list__, 24 + ((i__) << 2)))) {
         res__ = (ptr)LST43_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_690_: ;

   ret0__:
   return (res__);
}

char LST43_not_in_(ptr self__, ptr e__)
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
         goto goto_tag_691_;
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
goto_tag_691_: ;

   ret0__:
   return (res__);
}

int LST43_contains_(ptr self__, ptr e__)
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
         goto goto_tag_692_;
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
goto_tag_692_: ;

   ret0__:
   return (res__);
}

void LST43_mark_private_(ptr self__)
{
   ptr gl693_;
   static int gl694_;
   static union dtype_ gl695_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_696_;
      }
      else {
      }
      gl693_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl693_,493,gl694_,INTVAL_(gl695_));
      VFN_(gl695_)(gl693_);
      i__ = (int)(i__ + 1);
   }
goto_tag_696_: ;

   ret0__:
   return;
}

void LST43_mark_spec_(ptr self__)
{
   ptr gl697_;
   static int gl698_;
   static union dtype_ gl699_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_700_;
      }
      else {
      }
      gl697_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl697_,2095,gl698_,INTVAL_(gl699_));
      VFN_(gl699_)(gl697_);
      i__ = (int)(i__ + 1);
   }
goto_tag_700_: ;

   ret0__:
   return;
}

void LST43_mark_abstract_(ptr self__)
{
   ptr gl701_;
   static int gl702_;
   static union dtype_ gl703_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_704_;
      }
      else {
      }
      gl701_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl701_,494,gl702_,INTVAL_(gl703_));
      VFN_(gl703_)(gl701_);
      i__ = (int)(i__ + 1);
   }
goto_tag_704_: ;

   ret0__:
   return;
}

void LST43_mark_shared_(ptr self__)
{
   ptr gl705_;
   static int gl706_;
   static union dtype_ gl707_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_708_;
      }
      else {
      }
      gl705_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl705_,2096,gl706_,INTVAL_(gl707_));
      VFN_(gl707_)(gl705_);
      i__ = (int)(i__ + 1);
   }
goto_tag_708_: ;

   ret0__:
   return;
}

void LST43_mark_readonly_(ptr self__)
{
   ptr gl709_;
   static int gl710_;
   static union dtype_ gl711_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_712_;
      }
      else {
      }
      gl709_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl709_,2097,gl710_,INTVAL_(gl711_));
      VFN_(gl711_)(gl709_);
      i__ = (int)(i__ + 1);
   }
goto_tag_712_: ;

   ret0__:
   return;
}

ptr LST43_add_unique_feat_(ptr self__, ptr f__)
{
   ptr res__ = 0;
   SATHER_STR_(20,11,ls302_,"LST_FEATOB");
   SATHER_STR_(20,24,ls2326_,"Overriding definition \"");
   SATHER_STR_(20,2,ls785_,"\"");
   ptr gl713_;
   static int gl714_;
   static union dtype_ gl715_;
   ptr gl716_;
   static int gl717_;
   static union dtype_ gl718_;
   int gl19_;
   ptr gl719_;
   static int gl720_;
   static union dtype_ gl721_;
   int gl20_;
   ptr gl722_;
   static int gl723_;
   static union dtype_ gl724_;
   int gl21_;
   ptr gl725_;
   static int gl726_;
   static union dtype_ gl727_;
   int gl22_;
   ptr gl728_;
   static int gl729_;
   static union dtype_ gl730_;
   ptr gl731_;
   static int gl732_;
   static union dtype_ gl733_;
   int gl23_;
   int gl24_;
   ptr gl734_;
   static int gl735_;
   static union dtype_ gl736_;
   ptr gl737_;
   static int gl738_;
   static union dtype_ gl739_;
   int gl25_;
   int gl26_;
   int    i__ = S_int_VOID_;

   gl713_ = f__;
   switch (TYPE_(gl713_)) {
      case (62) :
         res__ = (ptr)LST43_push_(self__,f__);
         break;
      default:
         gl716_ = f__;
         gl19_ = TYPE_(gl716_);
         gl719_ = f__;
         gl20_ = TYPE_(gl719_);
         gl722_ = f__;
         gl21_ = TYPE_(gl722_);
         gl725_ = f__;
         gl22_ = TYPE_(gl725_);
         if (((((gl19_ == 63) | (gl20_ == 60)) | (gl21_ == 59)) | (gl22_ == 58))) {
            i__ = (int)0;
            while (1) {
               if ((i__ >= IATT_(self__,12))) {
                  goto goto_tag_740_;
               }
               else {
               }
               gl728_ = f__;
               cache_dispatch_(gl728_,476,gl729_,INTVAL_(gl730_));
               gl23_ = IATT_(gl728_,INTVAL_(gl730_));
               gl731_ = PATT_(self__, 24 + ((i__) << 2));
               cache_dispatch_(gl731_,476,gl732_,INTVAL_(gl733_));
               gl24_ = IATT_(gl731_,INTVAL_(gl733_));
               if ((gl23_ == gl24_)) {
                  PATT_(self__, 24 + ((i__) << 2)) = (ptr)f__;
                  if ((! LST43_duplicate_defs_ok_)) {
                     gl734_ = f__;
                     cache_dispatch_(gl734_,298,gl735_,INTVAL_(gl736_));
                     gl26_ = IATT_(gl734_,INTVAL_(gl736_));
                     gl737_ = f__;
                     cache_dispatch_(gl737_,476,gl738_,INTVAL_(gl739_));
                     gl25_ = IATT_(gl737_,INTVAL_(gl739_));
                     ERR96_format_warning_msg_file_(0,GLO68_curr_filename_,gl26_,(ptr)(&ls302_),STR20_s_(STR20_s_(STR20_s_(STR20_create_(0),(ptr)(&ls2326_)),STR69_at_index_(GLO68_str_table_,gl25_)),(ptr)(&ls785_)));
                  }
                  else {
                  }
                  res__ = (ptr)self__;
                  goto ret0__;
               }
               else {
               }
               i__ = (int)(i__ + 1);
            }
         goto_tag_740_: ;
         }
         else {
         }
         res__ = (ptr)LST43_push_(self__,f__);
         ;
   }

   ret0__:
   return (res__);
}

