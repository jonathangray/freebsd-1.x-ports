/* lst_fe133.c : Sather class: LST_FEATOB_S, dbg=F, gc=T, chk=F */

#include "all_.h"
#include "c_names_.h"

static char __sather_compiled__;

extern /*shared*/ ptr GLO68_curr_feature_;
extern int TYP149_ctype_(ptr self__);
extern int ERR96_out_of_line_err_info_(ptr self__, ptr f__, int ln__);
extern void FEA162_out_of_line_(ptr self__, ptr fn__);
extern ptr FEA162_dup_(ptr self__);
extern void FEA162_resolve_predef_types_(ptr self__, int index__);
extern void FEA162_semant_(ptr self__, ptr symtab__);
extern int FEA162_featob_s_name_(ptr self__);
extern /*constant*/ int C_T168_c_ptr_;
extern /*constant*/ int C_T168_c_ptr_size_;
extern /*constant*/ int C_T168_c_char_;
extern /*constant*/ int C_T168_c_char_size_;
extern /*constant*/ int C_T168_c_int_;
extern /*constant*/ int C_T168_c_int_size_;
extern /*constant*/ int C_T168_c_float_;
extern /*constant*/ int C_T168_c_float_size_;
extern /*constant*/ int C_T168_c_double_;
extern /*constant*/ int C_T168_c_double_size_;
#include "macros_.h"



/*constant*/ int LST133_print_indent_ = 2;
ptr LST133_create_(ptr self__, int init_size__);
void LST133_out_of_line_(ptr self__, ptr fn__);
ptr LST133_dup_(ptr self__);
void LST133_put_kwdname_(ptr self__, int nm__);
ptr LST133_sather_code_(ptr self__);
ptr LST133_initialize_(ptr self__, ptr initarg__);
void LST133_resolve_predef_types_(ptr self__, int index__);
void LST133_semant_(ptr self__, ptr symtab__);
ptr LST133_typeof_(ptr self__);
int LST133_get_offset_(ptr self__);
void LST133_cprint_offset_(ptr self__, ptr outfile__);
ptr LST133_get_constval_(ptr self__);
void LST133_cont_cprint_code_(ptr self__, ptr outfile__, int cont__);
void LST133_cprint_cname_(ptr self__, ptr outfile__);
void LST133_cprint_extern_(ptr self__, ptr outfile__);
void LST133_cprint_access_value_(ptr self__, ptr outfile__);
void LST133_cprint_init_code_(ptr self__, ptr outfile__);
void LST133_clear_(ptr self__);
/*constant*/ int LST133_def_init_size_ = 5;
ptr LST133_push_(ptr self__, ptr e__);
int LST133_size_(ptr self__);
char LST133_is_empty_(ptr self__);
ptr LST133_pop_(ptr self__);
ptr LST133_top_(ptr self__);
void LST133_init_iterate_(ptr self__);
ptr LST133_curr_item_(ptr self__);
ptr LST133_next_item_(ptr self__);
ptr LST133_prev_item_(ptr self__);
ptr LST133_push_unique_(ptr self__, ptr e__);
ptr LST133_append_(ptr self__, ptr list__);
ptr LST133_union_(ptr self__, ptr list__);
char LST133_not_in_(ptr self__, ptr e__);
int LST133_contains_(ptr self__, ptr fo__);
void LST133_cprint_code_(ptr self__, ptr outfile__);
ptr LST133_compact_(ptr self__);
void LST133_compactAttr_(ptr self__);
extern int attr_ent_LST133[];

ptr LST133_create_(ptr self__, int init_size__)
{
   ptr res__ = 0;

   if ((init_size__ <= 0)) {
      res__ = (ptr)new1_(133,5,0);
   }
   else {
      res__ = (ptr)new1_(133,init_size__,0);
   }

   ret0__:
   return (res__);
}

void LST133_out_of_line_(ptr self__, ptr fn__)
{
   ptr gl3160_;
   static int gl3161_;
   static union dtype_ gl3162_;
   int    i__ = S_int_VOID_;

   IATT_(self__,4) = (int)ERR96_out_of_line_err_info_(0,fn__,IATT_(self__,4));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3163_;
      }
      else {
      }
      gl3160_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl3160_,518,gl3161_,INTVAL_(gl3162_));
      VFN_(gl3162_)(gl3160_,fn__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3163_: ;

   ret0__:
   return;
}

ptr LST133_dup_(ptr self__)
{
   ptr res__ = 0;
   ptr gl3164_;
   static int gl3165_;
   static union dtype_ gl3166_;
   int gl304_;
   ptr gl3167_;
   static int gl3168_;
   static union dtype_ gl3169_;
   ptr gl305_;
   ptr gl3170_;
   static int gl3171_;
   static union dtype_ gl3172_;
   ptr gl3173_;
   static int gl3174_;
   static union dtype_ gl3175_;
   int gl3176_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (ptr)LST133_create_(self__,IATT_(self__,12));
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3177_;
      }
      else {
      }
      gl3164_ = PATT_(self__, 24 + ((i__) << 2));
      gl304_ = TYPE_(gl3164_);
      if ((gl304_ != 155)) {
         GLO68_curr_feature_ = (ptr)PATT_(self__, 24 + ((i__) << 2));
         gl3167_ = PATT_(self__, 24 + ((i__) << 2));
         cache_dispatch_(gl3167_,471,gl3168_,INTVAL_(gl3169_));
         gl305_ = PFN_(gl3169_)(gl3167_);
         res__ = (ptr)LST133_push_(res__,gl305_);
         gl3173_ = GLO68_curr_feature_;
         cache_dispatch_(gl3173_,298,gl3174_,INTVAL_(gl3175_));
gl3176_ = IATT_(gl3173_,INTVAL_(gl3175_));
         gl3170_ = PATT_(self__, 24 + ((i__) << 2));
         cache_dispatch_(gl3170_,298,gl3171_,INTVAL_(gl3172_));
         IATT_(gl3170_,INTVAL_(gl3172_)) = gl3176_;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3177_: ;

   ret0__:
   return (res__);
}

void LST133_put_kwdname_(ptr self__, int nm__)
{
   ptr gl3178_;
   static int gl3179_;
   static union dtype_ gl3180_;
   ptr    x__ = 0;

   x__ = (ptr)self__;
   gl3178_ = x__;
   cache_dispatch_(gl3178_,796,gl3179_,INTVAL_(gl3180_));
   IATT_(gl3178_,INTVAL_(gl3180_)) = (int)nm__;

   ret0__:
   return;
}

ptr LST133_sather_code_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

ptr LST133_initialize_(ptr self__, ptr initarg__)
{
   ptr res__ = 0;

   res__ = (ptr)self__;

   ret0__:
   return (res__);
}

void LST133_resolve_predef_types_(ptr self__, int index__)
{
   ptr gl3181_;
   static int gl3182_;
   static union dtype_ gl3183_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3184_;
      }
      else {
      }
      gl3181_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl3181_,522,gl3182_,INTVAL_(gl3183_));
      VFN_(gl3183_)(gl3181_,index__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3184_: ;

   ret0__:
   return;
}

void LST133_semant_(ptr self__, ptr symtab__)
{
   ptr gl3185_;
   static int gl3186_;
   static union dtype_ gl3187_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3188_;
      }
      else {
      }
      gl3185_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl3185_,588,gl3186_,INTVAL_(gl3187_));
      VFN_(gl3187_)(gl3185_,symtab__);
      i__ = (int)(i__ + 1);
   }
goto_tag_3188_: ;

   ret0__:
   return;
}

ptr LST133_typeof_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

int LST133_get_offset_(ptr self__)
{
   int res__ = S_int_VOID_;


   ret0__:
   return (res__);
}

void LST133_cprint_offset_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr LST133_get_constval_(ptr self__)
{
   ptr res__ = 0;


   ret0__:
   return (res__);
}

void LST133_cont_cprint_code_(ptr self__, ptr outfile__, int cont__)
{


   ret0__:
   return;
}

void LST133_cprint_cname_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST133_cprint_extern_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST133_cprint_access_value_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST133_cprint_init_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

void LST133_clear_(ptr self__)
{
   int    i__ = S_int_VOID_;

   i__ = S_int_VOID_;
   while (1) {
      if ((i__ == IATT_(self__,20))) {
         goto goto_tag_3189_;
      }
      else {
      }
      PATT_(self__, 24 + ((i__) << 2)) = (ptr)0;
      i__ = (int)(i__ + 1);
   }
goto_tag_3189_: ;

   ret0__:
   return;
}

ptr LST133_push_(ptr self__, ptr e__)
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

int LST133_size_(ptr self__)
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

char LST133_is_empty_(ptr self__)
{
   char res__ = S_char_VOID_;

   res__ = (char)(IATT_(self__,12) == 0);

   ret0__:
   return (res__);
}

ptr LST133_pop_(ptr self__)
{
   ptr res__ = 0;

   if (LST133_is_empty_(self__)) {
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

ptr LST133_top_(ptr self__)
{
   ptr res__ = 0;

   if (LST133_is_empty_(self__)) {
      res__ = (ptr)0;
   }
   else {
      res__ = (ptr)PATT_(self__, 24 + (((IATT_(self__,12) - 1)) << 2));
   }

   ret0__:
   return (res__);
}

void LST133_init_iterate_(ptr self__)
{

   IATT_(self__,16) = (int)0;

   ret0__:
   return;
}

ptr LST133_curr_item_(ptr self__)
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

ptr LST133_next_item_(ptr self__)
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

ptr LST133_prev_item_(ptr self__)
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

ptr LST133_push_unique_(ptr self__, ptr e__)
{
   ptr res__ = 0;
   int    k__ = S_int_VOID_;

   k__ = (int)LST133_contains_(self__,e__);
   if ((k__ >= 0)) {
      PATT_(self__, 24 + ((k__) << 2)) = (ptr)e__;
      res__ = (ptr)self__;
   }
   else {
      res__ = (ptr)LST133_push_(self__,e__);
   }

   ret0__:
   return (res__);
}

ptr LST133_append_(ptr self__, ptr list__)
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
         goto goto_tag_3190_;
      }
      else {
      }
      res__ = (ptr)LST133_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      i__ = (int)(i__ + 1);
   }
goto_tag_3190_: ;

   ret0__:
   return (res__);
}

ptr LST133_union_(ptr self__, ptr list__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;
   int    sz__ = S_int_VOID_;

   i__ = (int)0;
   sz__ = (int)IATT_(list__,12);
   res__ = (ptr)self__;
   while (1) {
      if ((i__ >= sz__)) {
         goto goto_tag_3191_;
      }
      else {
      }
      if (LST133_not_in_(self__,PATT_(list__, 24 + ((i__) << 2)))) {
         res__ = (ptr)LST133_push_(res__,PATT_(list__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3191_: ;

   ret0__:
   return (res__);
}

char LST133_not_in_(ptr self__, ptr e__)
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
         goto goto_tag_3192_;
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
goto_tag_3192_: ;

   ret0__:
   return (res__);
}

int LST133_contains_(ptr self__, ptr fo__)
{
   int res__ = S_int_VOID_;
   ptr gl3193_;
   static int gl3194_;
   static union dtype_ gl3195_;
   ptr gl3196_;
   static int gl3197_;
   static union dtype_ gl3198_;
   int gl306_;
   int gl307_;
   int    i__ = S_int_VOID_;

   i__ = (int)0;
   res__ = (int)(- 1);
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3199_;
      }
      else {
      }
      gl3193_ = fo__;
      cache_dispatch_(gl3193_,445,gl3194_,INTVAL_(gl3195_));
      gl306_ = IFN_(gl3195_)(gl3193_);
      gl3196_ = PATT_(self__, 24 + ((i__) << 2));
      cache_dispatch_(gl3196_,445,gl3197_,INTVAL_(gl3198_));
      gl307_ = IFN_(gl3198_)(gl3196_);
      if ((gl306_ == gl307_)) {
         res__ = (int)i__;
         goto ret0__;
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3199_: ;

   ret0__:
   return (res__);
}

void LST133_cprint_code_(ptr self__, ptr outfile__)
{


   ret0__:
   return;
}

ptr LST133_compact_(ptr self__)
{
   ptr res__ = 0;
   int    i__ = S_int_VOID_;

   res__ = (ptr)LST133_create_(self__,IATT_(self__,12));
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3200_;
      }
      else {
      }
      if ((PATT_(self__, 24 + ((i__) << 2)) != 0)) {
         res__ = (ptr)LST133_push_(res__,PATT_(self__, 24 + ((i__) << 2)));
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3200_: ;

   ret0__:
   return (res__);
}

void LST133_compactAttr_(ptr self__)
{
   ptr gl3201_;
   static int gl3202_;
   static union dtype_ gl3203_;
   int gl308_;
   ptr gl3204_;
   static int gl3205_;
   static union dtype_ gl3206_;
   int    curr_pos__ = S_int_VOID_;
   int    i__ = S_int_VOID_;
   ptr    af__ = 0;
   ptr    af_type__ = 0;
   int    af_ctype__ = S_int_VOID_;
   int    af_ctype_size__ = S_int_VOID_;
   ptr    tmp__ = 0;

   curr_pos__ = (int)0;
   i__ = (int)0;
   while (1) {
      if ((i__ >= IATT_(self__,12))) {
         goto goto_tag_3207_;
      }
      else {
      }
      gl3201_ = PATT_(self__, 24 + ((i__) << 2));
      gl308_ = TYPE_(gl3201_);
      if ((gl308_ == 153)) {
         af__ = (ptr)PATT_(self__, 24 + ((i__) << 2));
         af_type__ = (ptr)PATT_(af__,40);
         gl3204_ = af_type__;
         cache_dispatch_(gl3204_,374,gl3205_,INTVAL_(gl3206_));
         af_ctype__ = (int)IFN_(gl3206_)(gl3204_);
         af_ctype_size__ = S_int_VOID_;
         switch (af_ctype__) {
            case (1) :
               af_ctype_size__ = (int)4;
               break;
            case (2) :
               af_ctype_size__ = (int)1;
               break;
            case (3) :
               af_ctype_size__ = (int)4;
               break;
            case (4) :
               af_ctype_size__ = (int)4;
               break;
            case (5) :
               af_ctype_size__ = (int)8;
               break;
            default:
               ;
               ;
         }
         if ((af_ctype_size__ < 4)) {
            tmp__ = (ptr)PATT_(self__, 24 + ((i__) << 2));
            PATT_(self__, 24 + ((i__) << 2)) = (ptr)PATT_(self__, 24 + ((curr_pos__) << 2));
            PATT_(self__, 24 + ((curr_pos__) << 2)) = (ptr)tmp__;
            curr_pos__ = (int)(curr_pos__ + 1);
         }
         else {
         }
      }
      else {
      }
      i__ = (int)(i__ + 1);
   }
goto_tag_3207_: ;

   ret0__:
   return;
}

